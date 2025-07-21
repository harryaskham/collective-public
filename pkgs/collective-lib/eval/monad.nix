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

  # Monadic evaluation state.
  # Roughly simulates an ExceptT EvalError (StateT EvalState m) a monad stack.
  Eval = A: assert checkTypes [A]; rec {
    __toString = self: "Eval ${A}";
    inherit A;
    Error = EvalError;
    E = Either Error A;
    S = EvalState;
    check = x: x ? __isEval && is E x.e;
    pure = with E; x: Eval (getT x) id (E.pure x);
    throws = with E; e: Eval (getT e) id (E.Left e);
    __functor = with E; self:
      s: assert that (lib.isFunction s) ''Eval: expected lambda state but got ${_p_ s}'';
      e: assert that (is E e) ''Eval: expected Either value ${E} but got ${_p_ e}'';
      let this = {
        __type = Eval A;
        __isEval = true;
        __toString = self: _b_ "Eval ${A} (${_ph_ self.e})";
        inherit s e;
        modify = f: self (compose f s) e;
        set = st: this.modify (_: st);
        get = with S; s (mempty {});
        throws = e: self s (Left e);
        fmap = f: Eval A s (e.fmap f);
        bind = f:
          if isLeft e then this else 
          let 
            a = e.right;
            mb = f a;
          in 
            if isLeft mb.e then mb else
            let 
              e' = mb.e;
              s' = compose mb.s s;
              A' = getT e'.right;
            in Eval A' s' e';
        # Returns (Either EvalError set)
        run = state: this.e.fmap (a: { s = this.get; inherit a; });
      };
      in this;
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
          a = with Eval Int; rec {
            _42 = pure (Int 42);
            stateXPlus2 = _42.modify (s: s.fmap (scope: scope // {x = (scope.x or 0) + 2;}));
            stateXPlus2Times3 = stateXPlus2.modify (s: s.fmap (scope: scope // {x = scope.x * 3; }));
            getStatePlusValue = with stateXPlus2Times3; bind (i: pure (Int (i.x + get.scope.x)));
            thenThrows = with stateXPlus2Times3; bind (i: throws (Abort "test"));
            bindAfterThrow = with thenThrows; bind (i: pure (Int i.x + 1));
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
          pure = expectRun {} a._42 {} (Int 42);
          modify.once = expectRun {} a.stateXPlus2 { x = 2; } (Int 42);
          modify.twice = expectRun {} a.stateXPlus2Times3 { x = 6; } (Int 42);
          bind.get = expectRun {} a.getStatePlusValue { x = 6; } (Int 48);
          bind.thenThrows = expectRunError {} a.thenThrows (Abort "test");
          bind.bindAfterThrow = expectRunError {} a.bindAfterThrow (Abort "test");
        };
    };
}