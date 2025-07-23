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

  # Get the type of a value.
  getT = a: a.__type or (typeOf a);

  # Get the Monad type of a value.
  getM = a: 
    assert that (isMonadValue a) ''
      getM: expected monad but got ${_p_ a}
    '';
    getT (getT a);

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
  NixPathError = EvalError "NixPathError";

  isEvalError = x: x ? __isEvalError;

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

  initEvalState = EvalState initScope;
  initScope = {
    NIX_PATH = {
      nixpkgs = <nixpkgs>;
    };

    true = true;
    false = false;
    null = null;
    builtins = builtins;
    derivation = derivation;
    import = builtins.import;
    throw = throw;
    abort = abort;
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
  void = m: 
    assert that ((m ? bind) && (m ? pure)) ''
      void: expected monad but got ${getT m}
    '';
    with m; bind (_: pure unit);
  when = cond: m: if cond then m else void m;
  unless = cond: m: if cond then void m else m;

  isDoStatement = M: xs:
    (M != null && isMonadOf M xs)
    || (M != null && isDoOf M xs)
    || (isFunction xs)
    || (isSolo xs && isDoStatement M (soloValue xs));

  assertIsDoStatement = M: xs:
    assert that (isDoStatement M xs) ''
      do: expected a statement of form

        ( ${if M == null then "<monadic value>" else M Any} )

      or

        ( { bindings, ... }: ${if M == null then "<monadic value>" else M Any} )

      or

        { name = ${if M == null then "<monadic value>" else M Any}; }

      or

        { name = ({ bindings, ... }: ${if M == null then "<monadic value>" else M Any}); }

      but got ${_p_ xs}
    '';
    true;

  # Do notation builder for a given monad.
  mkDo = M: 
    let m = M.pure unit;
        bindings = M.pure { _ = m; };
        stack = [];
        canRun = true;
    in __mkDo M bindings m stack canRun;

  isDo = x: x ? __isDo;
  isDoOf = M: xdo: isDo xdo && isMonadOf M xdo.__m;

  __mkDo = M: bindings: m: stack: canRun:
    let this = rec {
      __isDo = true;

      # Set to true if the last action was not assignment.
      inherit canRun;

      # Stores the actual bindings for error logging.
      inherit stack;

      # The built-up monadic action.
      __m = if m == null then M.pure unit else m;

      # Create bindings that include the void monadic action exposing pure, bind, etc.
      __bindings = 
        if bindings == null 
        then M.pure { _ = void __m; }
        else bindings;

      # Print an error with the do-stack trace.
      withStackError = self: msg: _b_ ''
        ${msg}

        in

        ${toString self}
      '';

      # do-level run alias.
      run = arg:
        assert that canRun (withStackError this ''
          do: final statement of a do-block cannot be an assignment.
        '');
        __m.run arg;

      # do-level aliases.
      do = this;
      pure = __m.pure;
      bind = __m.bind;
      throws = __m.throws;
      catch = __m.catch;

      __toString = self: 
        let 
          doPos = if empty stack then null else debuglib.pos.path (head stack);
          doLine = 
            if doPos == null then null
            else let p = soloValue doPos; in 
                if (p ? file) && (p ? line) then "${p.file}:${toString p.line}"
                else null;
          header = optionalString (doLine != null) doLine;
        in _b_ ''
          ${header}
          do
            ${_h_ (_ls_ (map (debuglib.printPosWith {
              emptyMsg = "<no source>";
              errorMsg = "<error>";
            }) stack))}
        '';

      __functor = self: statement:
        let
          stack' = stack ++ [statement];

          # Create new bindings (not a new 'do' structure) with the given binding.
          withBindingM = name: valueM: __m: __bindings:
            assert that (isMonadOf M valueM || isDoOf M valueM) (
              self.withStackError (self // { stack = stack'; }) (''
                do: expected binding RHS of ${M} but got
                  ${_ph_ (getM valueM)}
              ''));

            __bindings.bind (bindings:
            valueM.bind (value:
            M.pure (bindings // { 
              _ = void __m;
              ${name} = value; 
            })));

          # Rebind the context to the given monad and bindings without adding any new bindings.
          rebindContext = __m: __bindings:
            __bindings.bind (bindings:
            M.pure (bindings // { 
              _ = void __m;
            }));

        in flip dispatch statement {
          # Dependent action operation:
          # { bindings, ...}: monadic value; }
          #
          # Run the given function with the bindings.
          # Bindings are not updated.
          # If this is the final action of a monad, then the value will be returned
          # by 'run'
          lambda = f: 
            let m = self.__m.bind (_: self.__bindings.bind f);
                bindings = rebindContext m self.__bindings;
            in __mkDo M bindings m stack' true;

          # Binding operation:
          # { x = (monadic value) | ({ bindings, ... }: monadic value); }
          set = xs:
            assert assertIsDoStatement M xs;

            # Independent action operation:
            # ( monadic value )
            # Just set the monad value.
            # Bindings are not updated.
            if isDoOf M xs then
              # Propagate the previous action with a void bind
              let m = self.__m.bind (_: xs.__m);
                  bindings = rebindContext m self.__bindings;
              in __mkDo M bindings m stack' true

            else if isMonadOf M xs then
              # Propagate the previous action with a void bind
              let m = self.__m.bind (_: xs);
                  bindings = rebindContext m self.__bindings;
                in __mkDo M bindings m stack' true

            else (flip dispatch) (soloValue xs) {
              # Independent bind:
              # { x = (monadic value); }
              # Return new void do with the bindings bound.
              set = valueM:
                let 
                  m = self.__m.bind (_: valueM);
                  bindings = withBindingM (soloName xs) valueM m self.__bindings;
                in __mkDo M bindings m stack' false;

              # Dependent bind:
              # {x = { some function of state }}
              # Build the computation for the RHS and return a new do.
              lambda = f:
                let valueM = self.__bindings.bind f;
                    m = self.__m.bind (_: valueM);
                    bindings = withBindingM (soloName xs) valueM m self.__bindings;
                in __mkDo M bindings m stack' false;
            };
        };
    };
  in 
    this;

  # Generic do notation that infers its type.
  # Cannot apply lambdas without M, so can only infer from a constant binding.
  do = dispatch {
    lambda = _: throw "do: cannot infer monadic type from lambda expression";
    set = xs:
      if isDo xs then mkDo M xs.__m
      else if isMonadOf M xs then mkDo M xs
      else flip dispatch (soloValue xs) {
        lambda = _: throw "do: cannot infer monadic type from dependent binding expression";
        set = valueM:
          let M = getT (getT valueM);
          in mkDo M xs;
      };
  };

  # Check if a value is a monad.
  # i.e. isMonad (Eval.pure 1) -> true
  #      isMonad (Either.pure 1) -> true
  isMonadValue = x: (getT x) ? __isMonad;

  # Is the given x an instance of the given monad, ignoring the type parameter?
  # i.e. isMonadOf Eval (Eval.pure 1) -> true
  #      isMonadOf Eval (Either.pure 1) -> false
  isMonadOf = M: x: isMonadValue x && is (getT (M.pure unit)) (void x);

  # Monadic evaluation state.
  # Roughly simulates an ExceptT EvalError (StateT EvalState m) a monad stack.
  Eval = rec {
    __toString = self: "Eval";
    check = x: x ? __isEval;
    Error = EvalError;
    S = EvalState;
    do = mkDo Eval;
    pure = x: 
      let A = getT x;
      in Eval A id ((Either Error A).pure x);
    throws = e: (Eval.pure unit).throws e;

    __functor = self: A: assert checkTypes [A]; rec {
      __toString = self: "Eval ${A}";
      __isMonad = true;
      __type = Eval;
      inherit A;
      E = Either Error A;
      check = x: x ? __isEval && is E x.e;
      pure = x: Eval A id ((Either Error A).pure x);

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
            void (this.mapState f);

          set = st: 
            if isLeft this.e then this else
            void (this.setState st);

          # Thunked to avoid infinite nesting - (m.get {}) is an (Eval EvalState)
          get = _: this.bind (_: this.pure (this.s (S.mempty {})));

          setState = s: Eval A (const s) this.e;
          mapState = f: Eval A (compose f this.s) this.e;
          mapEither = f: Eval A this.s (f this.e);

          withScope = f: (this.get {}).bind (s: f s.scope);
          getScope = {}: this.withScope this.pure;
          setScope = scope: this.modifyScope (const scope);
          modifyScope = f: this.modify (s: s.fmap f);
          prependScope = newScope: this.modifyScope (scope: newScope // scope);
          appendScope = newScope: this.modifyScope (scope: scope // newScope);

          do = Eval.do;
          pure = x: this.bind (_: Eval.pure x);
          fmap = f: Eval A this.s (this.e.fmap f);
          when = eval.monad.when;
          unless = eval.monad.unless;

          bind = f:
            if isLeft this.e then this else 
            let 
              a = this.e.right;
              # TODO: Handle 'do' more cleanly at top level
              mb = let r = f a; in 
                if is Eval r then r
                else if isDoOf Eval r
                then r.__m
                else _throw_ ''
                  Eval.bind: non-Eval value returned of type ${getT r}:
                    ${_ph_ r}
                '';
            in
              if isLeft mb.e then mb else
              let 
                e' = mb.e;
                s' = compose mb.s this.s;
                A' = getT e'.right;
              in Eval A' s' e';

          # Set the value to the given error.
          throws =
            e: assert that (is Error e) ''Eval.throws: expected Either value ${Error} but got ${_p_ e} of type ${getT e}'';
            this.mapEither (const (E.Left e));

          # Catch specific error types and handle them with a recovery function
          # catch :: (EvalError -> Eval A) -> Eval A
          catch = handler:
            if isLeft this.e then 
              let recovery = handler this.e.left;
              in 
                assert that (isMonadOf Eval recovery) ''
                  Eval.catch: expected recovery function of type Eval A but got ${_p_ recovery}
                '';
                (this.mapEither (const (E.Right unit))).bind (_: recovery)
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

          do = {
            notation = {
              bindOne =
                let m = Eval.do {x = Eval.pure 1;} ({_, ...}: _.pure unit);
                in {
                  bindings = expectRun {} m.__bindings {} { x = 1; _ = Eval.pure unit; };
                  run = expectRun {} m {} unit;
                };
              bindOneGetOne = 
                let m = Eval.do {x = Eval.pure 1;} ({_, x}: _.pure x);
                in expectRun {} m {} 1;
              dependentBindGet = 
                let m = Eval.do
                  {x = Eval.pure 1;}
                  {y = {_, x}: _.pure (x + 1);}
                  ({_, x, y}: _.pure (x + y));
                in expectRun {} m {} 3;
              boundDo = 
                let do = Eval.do; in with Eval;
                let m = do
                  {x = pure 1;}
                  {y = pure 2;}
                  ({x, y, ...}: pure (x + y));
                in expectRun {} m {} 3;

              setGet =
                let m = Eval.do
                  ( {_}: _.set (EvalState {x = 1;}) )
                  ( {_}: _.get {} );
                in expectRun {} m {x = 1;} (EvalState {x = 1;});

              composes = 
                let a = Eval.do ( {_}: _.appendScope {x = 1;});
                    b = Eval.do ( {_}: _.appendScope {y = 2;});
                in {
                  scopeExists = expectRun {} a {x = 1;} unit;

                  do = 
                    let c = 
                      Eval.do 
                        a
                        b
                        ( {_}: _.get {} );
                    in expectRun {} c {x = 1; y = 2;} (EvalState {x = 1; y = 2;});

                  doBind =
                    let 
                      c = (a.bind (_: b.bind (_: Eval.pure unit))).get {};
                    in expectRun {} c {x = 1; y = 2;} (EvalState {x = 1; y = 2;});
                };
            };
          };
        };

    };
}