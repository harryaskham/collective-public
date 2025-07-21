{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with collective-lib.typed;
rec {
  checkTypes = Ts: 
    assert (all (x: x == true) (
      strict (
        map 
          (T: assert that (T ? check || isbuiltinName T) ''
            Type argument does not have a check method and is not a builtin type name:
              ${_ph_ T}
          ''; 
          true)
          Ts)));
    true;

  assertIs = T: a:
    if T ? check then 
      assert assertMsg (T.check a) (_b_ ''
        check: expected value of type ${T}, got: ${getT a}
      ''); 
      true
    else if isbuiltinName T then
      assert assertMsg (typeOf a == T) (_b_ ''
        check: expected value of type ${T}, got: ${getT a}
      ''); 
      true
    else
      assert assertMsg false (_b_ ''
        check: expected type string or __type ${T}, got: ${lib.typeOf T}
      ''); 
      true;

  is = T: a: errors.tryBool (assertIs T a);

  getT = a: a.__type or (typeOf a);

  Any = {
    __toString = self: "Any";
    check = x: true;
    __functor = self: value: {
      __type = Any;
      inherit value;
    };
  };

  Either = E: A: assert checkTypes [E A]; rec {
    __toString = self: "Either ${E} ${A}";
    __functor = self: x:
      assert that (is E x || is A x) ''Either: expected type ${E} or ${A} but got ${_p_ x}'';
      if is E x then Left x else Right x;
    check = x: (isLeft x && is Left x) || (isRight x && is Right x);
    Left = __Left E A;
    Right = __Right E A;
    pure = Right;
  };

  __Left = E: A: assert checkTypes [E A]; {
    __toString = self: "(Either ${E} ${A}).Left";
    check = e: e ? __isLeft && is E e.left;
    __functor = self: e:
      assert that (is E e) ''Either.Left: expected type ${E} but got ${_p_ e}'';
      let this = {
        __type = Either E A;
        __isLeft = true; 
        __toString = self: "Left ${_p_ e}";
        left = e; 
        fmap = _: this;
      }; in this;
  };

  __Right = E: A: assert checkTypes [E A]; {
    __toString = self: "(Either ${E} ${A}).Right";
    check = a: a ? __isRight && is A a.right;
    __functor = self: a:
      assert that (is A a) ''Either.Right: expected type ${A} but got ${a}'';
      let this = { 
        __type = Either E A;
        __isRight = true; 
        __toString = self: "Right ${_p_ a}"; 
        right = a; 
        fmap = f: 
          let 
            b = f a;
            B = getT b;
          in __Right E B b;
      }; in this;
  };

  isEither = x: x ? __isLeft || x ? __isRight;
  isLeft = x: x ? __isLeft;
  isRight = x: x ? __isRight;
  case = e: cases: if isLeft e then cases.Left e.left else cases.Right e.right;

  EvalError = rec {
    __toString = self: "EvalError";
    check = x: x ? __isEvalError;
    __functor = self: name: {
      __toString = self: "EvalError.${name}";
      check = x: (x ? __isEvalError) && (x ? "__isEvalError${name}");
      __functor = self: __msg: {
        __type = EvalError;
        __isEvalError = true; 
        "__isEvalError${name}" = true; 
        __toString = self: "EvalError.${name}: ${__msg}";
        inherit __msg;
      };
    };
  };
  Abort = EvalError "Abort";
  AssertError = EvalError "AssertError";
  Throw = EvalError "Throw";
  TypeError = EvalError "TypeError";
  RuntimeError = EvalError "RuntimeError";
  MissingAttributeError = EvalError "MissingAttributeError";

  EvalState = rec {
    __toString = self: "EvalState";
    check = x: x ? __isEvalState;
    mempty = _: EvalState {};
    mconcat = ss: EvalState (mergeAttrsList (map (s: s.scope) ss));
    __functor = self: scope: {
      __type = EvalState;
      __isEvalState = true;
      __toString = self: _b_ "EvalState ${_ph_ self.scope}";
      inherit scope;
      fmap = f: EvalState (f scope);
    };
  };

  initEvalState = EvalState {
    true = true;
    false = false;
    null = null;
    import = builtins.import;
  };

  Unit = {
    __toString = self: "Unit";
    check = x: x ? __isUnit;
    __functor = self: {}: {
      __toString = self: "unit";
      __type = Unit;
      __isUnit = true;
    };
  };
  unit = Unit {};
  void = x: with x; bind (_: pure unit);

  # Monadic evaluation state.
  # Roughly simulates an ExceptT EvalError (StateT EvalState m) a monad stack.
  Eval = rec {
    __toString = self: "Eval";
    check = x: x ? __isEval;
    Error = EvalError;
    S = EvalState;
    pure = x: 
      let A = getT x;
      in Eval A id ((Either Error A).pure x);
    throws = e: (Eval.pure unit).throws e;

    mapState = f: x: Eval x.A (f x.s) x.e;

    mapEither = f: x: Eval x.A x.s (f x.e);

    __functor = self: A: assert checkTypes [A]; rec {
      __toString = self: "Eval ${A}";
      inherit A;
      E = Either Error A;
      check = x: x ? __isEval && is E x.e;

      __functor = self:
        s: assert that (lib.isFunction s) ''Eval: expected lambda state but got ${_p_ s}'';
        e: assert that (is E e) ''Eval: expected Either value ${E} but got ${_p_ e}'';

        let this = {
          __type = Eval A;
          __isEval = true;
          __toString = self: _b_ "Eval ${A} (${_ph_ self.e})";
          inherit S E A s e;

          # modify :: (EvalState -> EvalState) -> Eval A -> Eval {}
          modify = f: 
            if isLeft this.e then this else
            mapState (const (compose f s)) (Eval.pure unit);

          set = st: this.modify (const st);

          get = _: this.pure (this.s (S.mempty {}));

          pure = x: mapState (const this.s) (Eval.pure x);

          fmap = f: Eval A this.s (this.e.fmap f);

          bind = f:
            if isLeft this.e then this else 
            let 
              a = this.e.right;
              mb = f a;
            in 
              assert that (is Eval mb) ''
                Eval.bind: non-Eval value returned of type ${getT mb}:
                  ${_p_ mb}
              '';
              if isLeft mb.e then mb else
              let 
                e' = mb.e;
                s' = compose mb.s this.s;
                A' = getT e'.right;
              in Eval A' s' e';

          # Set the value to the given error.
          throws =
            e: assert that (is Error e) ''Eval.throws: expected Either value ${Error} but got ${_p_ e}'';
            mapEither (const (E.Left e)) this;

          # Catch specific error types and handle them with a recovery function
          # catch :: (EvalError -> Eval A) -> Eval A
          catch = handler:
            if isLeft this.e then 
              let recovery = handler this.e.left;
              in mapState (const this.s) recovery
            else this;

          # Returns (Either EvalError set)
          run = state: this.e.fmap (a: { s = s (S.mempty {}); inherit a; });
        };
        in this;
    };
  };

  _tests = with tests;
    let
      Int = { 
        __toString = self: "Int";
        check = x: isInt (x.x or null);
        __functor = self: x: { 
          inherit x; 
          __type = Int; 
          __toString = self: "Int ${_p_ self.x}";
          }; 
      };
    in suite {
      either =
        let
          E = Either EvalError Int;
        in with E; with EvalError; {
          left.isLeft = expect.True (isLeft (Left (Abort "test")));
          left.isRight = expect.False (isRight (Left (Abort "test")));
          left.wrongType = expect.error (Left (Int 1));
          right.isLeft = expect.False (isLeft (Right (Int 1)));
          right.isRight = expect.True (isRight (Right (Int 1)));
          right.wrongType = expect.error (Right (Abort "test"));
          left.fmap = expect.noLambdasEq ((Left (Abort "test")).fmap (x: Int (x.x + 1))) (Left (Abort "test"));
          right.fmap.sameType = expect.noLambdasEq ((Right (Int 1)).fmap (x: Int (x.x + 1))) (Right (Int 2));
          right.fmap.changeType = 
            let Right' = (Either EvalError parser.AST).Right;
            in expect.noLambdasEq ((Right (Int 1)).fmap (_: parser.N.int 42)) (Right' (parser.N.int 42));
        };

      state = {
        mk = expect.eq (EvalState {}).scope {};
        fmap =
          expect.noLambdasEq
          ((EvalState {}).fmap (scope: scope // {x = 1;}))
          (EvalState {x = 1;});
      };

      monad = 
        let
          a = rec {
            _42 = Eval.pure (Int 42);
            stateXIs2 = _42.set (EvalState { x = 2; });
            stateXTimes3 = stateXIs2.modify (s: EvalState { x = s.scope.x * 3; });
            const42 = with stateXTimes3; pure (Int 42);
            getStatePlusValue = 
              with const42; 
              bind (i: (get {}).bind (s: pure (Int (s.scope.x + i.x))));
            thenThrows = with stateXTimes3; bind (i: throws (Abort "test error"));
            bindAfterThrow = with thenThrows; bind ({}: pure "not reached");
            catchAfterThrow = with thenThrows; catch (e: pure "handled error '${e}'");
            fmapAfterCatch = with catchAfterThrow; fmap (s: s + " then ...");
          };
          expectRun = s: a: s': a': 
            with Either EvalError "set";
            expect.noLambdasEq
              (a.run (EvalState s))
              (Right { s = EvalState s'; a = a'; });
          expectRunError = s: a: e: 
            with Either EvalError "set";
            expect.noLambdasEq
              (a.run (EvalState s))
              (Left e);
        in with EvalState; {
          _0_pure = expectRun {} a._42 {} (Int 42);
          _1_set = expectRun {} a.stateXIs2 { x = 2; } unit;
          _2_modify = expectRun {} a.stateXTimes3 { x = 6; } unit;
          _4_bind.get = expectRun {} a.getStatePlusValue { x = 6; } (Int 48);
          _5_bind.thenThrows = expectRunError {} a.thenThrows (Abort "test error");
          _6_bind.bindAfterThrow = expectRunError {} a.bindAfterThrow (Abort "test error");
          _7_catch.noError = expectRun {} (a._42.catch (_: throw "no")) {} (Int 42);
          _8_catch.withError = expectRun {} a.catchAfterThrow { x = 6; } "handled error 'EvalError.Abort: test error'";
          _9_catch.thenFmap = expectRun {} a.fmapAfterCatch { x = 6; } "handled error 'EvalError.Abort: test error' then ...";
        };
    };
}