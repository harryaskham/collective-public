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

  tEq = a: b: pointerEqual a b || toString a == toString b;

  # Get the type of a value.
  getT = a: a.__type or (typeOf a);

  # Get the Monad type of a value.
  getM = a: 
    assert that (isMonadValue a) ''
      getM: expected monad but got ${_p_ a}
    '';
    if isDo a then a.M
    else getT (getT a);

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

  isDo = x: x ? __isDo;

  isDoOf = M: xdo: isDo xdo && tEq xdo.M M;

  isDoStatement = M: xs:
    (M != null && isMonadOf M xs)
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

  withStackError = self: msg: _b_ ''
    ${msg}

    in

    ${toString self}
  '';

  inferMonadFromStatement = dispatch {
    lambda = _: throw "do: cannot infer monadic type from lambda expression";
    set = xs:
      if isDo xs then xs.M
      else if isMonadValue xs then (getM xs)
      else if isSolo xs then flip dispatch (soloValue xs) {
        set = getM;
        lambda = _: throw "do: cannot infer monadic type from dependent binding expression";
      }
      else throw "do: malformed non-solo non-monad non-do set statement: ${_p_ xs}";
  };

  inferTypeFromStatement = dispatch {
    null = _: throw "do: cannot infer type from null statement";
    lambda = _: throw "do: cannot infer type from lambda expression";
    set = xs:
      if isMonadValue xs then (getT xs)
      else if isSolo xs then flip dispatch (soloValue xs) {
        set = getT;
        lambda = _: throw "do: cannot infer type from dependent binding expression";
      }
      else throw "do: malformed non-solo non-monad non-do set statement: ${_p_ xs}";
  };

  bindStatementSignature = M: dispatch {
    lambda = _: "DependentAction";
    set = statement:
      if isMonadOf M statement then "IndependentAction"
      else switch.typeOf (soloValue statement) {
        lambda = "DependentBind";
        set = "IndependentBind";
      };
  };

  printDo = self:
    let 
      doPos = 
        if empty self.__statements then null
        else debuglib.pos.path (head self.__statements);
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
        }) self.__statements))}
    '';

  # Given a monad M, a state containing an M bindings and an M m monadic action,
  # and a statement of one of these forms:
  #
  # <M value>
  # { bindings, ... }: <M value>
  # { nameToBind = <M value>; }
  # { nameToBind = { bindings, ... }: <M value>; }
  #
  # Return an updated state with the bindings updated inside the monad to any new
  # bindings, and an updated monadic action.
  handleDoStatement = 
    M: {bindings, m, canBind}: statement:
    assert (assertIsDoStatement M statement);
    switch (bindStatementSignature M statement) {
      IndependentAction = {
        inherit bindings;
        m = m.bind (_: statement);
        canBind = true;
      };
      DependentAction = {
        inherit bindings;
        m = bindings.bind (bindings: m.bind (_: statement (bindings // { _ = void m; })));
        canBind = true;
      };
      IndependentBind = 
        let
          name = soloName statement;
          m' = m.bind (_: soloValue statement);
        in {
          bindings = bindings.bind (bindings: m'.bind (v: M.pure (bindings // { ${name} = v; })));
          m = m';
          canBind = false;
        };
      DependentBind =
        let
          name = soloName statement;
          m' = bindings.bind (bindings: m.bind (_: (soloValue statement) (bindings // { _ = void m; })));
        in {
          bindings = bindings.bind (bindings: m'.bind (v: M.pure (bindings // { ${name} = v; })));
          m = m';
          canBind = false;
        };
    };

  # Do-notation functor that simply stores the statements given to it
  # When bound, it runs the statements in order to produce the monadic value
  # on which to call bind.
  do_ = M: statement: mkDo M [ statement ];
  do = statement: mkDo (inferMonadFromStatement statement) [ statement ];

  mkDo = M: __statements:
    let this = {
      __isDo = true;
      __isMonad = true;
      __type = inferTypeFromStatement (maybeLast __statements);
      inherit M __statements;
      inherit (M) pure;

      __toString = printDo;

      __functor = self: statement:
        mkDo M (self.__statements ++ [statement]);

      # If there is no previous action, we bind from scratch with unit state.
      bind = f: this.bind_ (M.pure unit) f;

      # To correctly nest do statements, we need to start inside the action we're binding to.
      bind_ = initM: f:
        let 
          initAcc = {
            bindings = M.pure {};
            m = initM;
            canBind = false;
          };
          state = fold (handleDoStatement M) initAcc this.__statements;
        in 
          assert that state.canBind (withStackError this ''
            do: final statement of a do-block cannot be an assignment.
          '');
          state.m.bind f;

      run = (this.bind M.pure).run;
      mapState = (this.bind M.pure).mapState;
    };
    in this;

  # Check if a value is a monad.
  # i.e. isMonad (Eval.pure 1) -> true
  #      isMonad (Either.pure 1) -> true
  #      isMonad (do (Eval.pure 1)) -> true
  isMonadValue = x: isDo x || (getT x) ? __isMonad;

  # Is the given x an instance of the given monad, ignoring the type parameter?
  # i.e. isMonadOf Eval (Eval.pure 1) -> true
  #      isMonadOf Eval (Either.pure 1) -> false
  isMonadOf = M: x: isMonadValue x && tEq M (getM x);

  # Monadic evaluation state.
  # Roughly simulates an ExceptT EvalError (StateT EvalState m) a monad stack.
  Eval = rec {
    __toString = self: "Eval";
    check = x: x ? __isEval;
    Error = EvalError;
    S = EvalState;
    do = mkDo Eval [];
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
            void (this.mapState (s: st: f (s st)));

          set = st: 
            if isLeft this.e then this else
            void (this.setState (const st));

          # Thunked to avoid infinite nesting - (m.get {}) is an (Eval EvalState)
          get = _: this.bind (_: this.pure (this.s (S.mempty {})));

          setState = s: Eval A s this.e;
          mapState = f: Eval A (f this.s) this.e;
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

          bind = statement:
            if isLeft this.e then this else
            switch.def
              (throw "Eval.bind: cannot bind to a statement of type ${bindStatementSignature Eval statement}")
              (bindStatementSignature Eval statement) {
              IndependentAction = this.bind ({_, ...}: statement);
              DependentAction =
                let 
                  bindings = {
                    _ = void this;
                  };
                  f = statement bindings;
                  a = this.e.right;
                  mb =
                    let r = f a;
                    in
                      if isMonadOf Eval r then r
                      else _throw_ ''
                        Eval.bind: non-Eval value returned of type ${getT r}:
                          ${_ph_ r}
                      '';
                in mb.mapState (s': st: s' (this.s st));
              };
             #   if isLeft e then e
             #   else 
             # if isLeft mb.e then mb else
             # let
             #   e' = mb.e;
             #   s' = compose mb.s this.s;
             #   A' = getT e'.right;
             # in Eval A' s' e';

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
            thenThrows = with stateXTimes3; bind (i: throws (Throw "test error"));
            bindAfterThrow = with thenThrows; bind ({}: pure "not reached");
            catchAfterThrow = thenThrows.catch (e: Eval.pure "handled error '${e}'");
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
          __smoke = {
            isMonadOf.monad = expect.True (isMonadOf Eval (Eval.pure unit));
            isMonadOf.do = expect.True (isMonadOf Eval (do (Eval.pure unit)));
            isMonadOf.false = expect.False (isMonadOf Eval ((Either EvalError Int).Right (Int 42)));

            getM.monad = expect.True (tEq Eval (getM (Eval.pure unit)));
            getM.do = expect.True (tEq Eval (getM (do (Eval.pure unit))));

            getT.monad = expect.True (tEq (Eval Unit) (getT (Eval.pure unit)));
            getT.do = expect.True (tEq (Eval Unit) (getT (do (Eval.pure unit))));
          };

          _00_pure = expectRun {} a._42 {} (Int 42);
          _01_set = expectRun {} a.stateXIs2 { x = 2; } unit;
          _02_modify = expectRun {} a.stateXTimes3 { x = 6; } unit;
          _04_bind.get = expectRun {} a.getStatePlusValue { x = 6; } (Int 48);
          _05_bind.thenThrows = expectRunError {} a.thenThrows (Throw "test error");
          _06_bind.bindAfterThrow = expectRunError {} a.bindAfterThrow (Throw "test error");
          _07_catch.noError = expectRun {} (a._42.catch (_: throw "no")) {} (Int 42);
          _08_catch.withError = expectRun {} a.catchAfterThrow { x = 6; } "handled error 'EvalError.Throw: test error'";
          _09_catch.thenFmap = expectRun {} a.fmapAfterCatch { x = 6; } "handled error 'EvalError.Throw: test error' then ...";

          _10_signatures = {
            IndependentAction.monad =
              expect.eq (bindStatementSignature Eval (Eval.pure unit)) "IndependentAction";
            IndependentAction.do =
              expect.eq (bindStatementSignature Eval (do (Eval.pure unit))) "IndependentAction";
            IndependentBind =
              expect.eq (bindStatementSignature Eval {a = Eval.pure unit;}) "IndependentBind";
            DependentAction =
              expect.eq (bindStatementSignature Eval ({_}: _.pure unit)) "DependentAction";
            DependentBind =
              expect.eq (bindStatementSignature Eval {a = {_}: _.pure unit;}) "DependentBind";
          };

          _11_inferMonad = {
            IndependentAction.monad =
              expect.True (tEq Eval (inferMonadFromStatement (Eval.pure unit)));
            IndependentAction.do =
              expect.True (tEq Eval (inferMonadFromStatement (do (Eval.pure unit))));
            IndependentBind =
              expect.True (tEq Eval (inferMonadFromStatement {a = Eval.pure unit;}));
            DependentAction =
              expect.error (inferMonadFromStatement ({_}: _.pure unit));
            DependentBind =
              expect.error (inferMonadFromStatement {a = {_}: _.pure unit;});
          };

          do.notation = {
            bindOne =
              let m = Eval.do {x = Eval.pure 1;} ({_, ...}: _.pure unit);
              in {
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

            setGetInferred =
              let m = do
                ( Eval.pure unit )
                ( {_}: _.set (EvalState {x = 1;}) )
                ( {_}: _.get {} );
              in expectRun {} m {x = 1;} (EvalState {x = 1;});

            setGet =
              let m = Eval.do
                ( {_}: _.set (EvalState {x = 1;}) )
                ( {_}: _.get {} );
              in expectRun {} m {x = 1;} (EvalState {x = 1;});

            setGetChainBlocks =
              let a = Eval.do ( {_}: _.set (EvalState {x = 1;}) );
                  b = Eval.do a ( {_}: _.get {} );
                  m = Eval.do b;
              in expectRun {} m {x = 1;} (EvalState {x = 1;});

            setGetWithoutDo =
              let a = (Eval.pure unit).bind ({_}: _.set (EvalState {x = 1;}));
                  b = (Eval.pure unit).bind ({_}: _.modify (s: EvalState {x = s.scope.x + 1;}));
                  c = (Eval.pure unit).bind ({_}: _.get {}).bind ({_}: _.pure s.scope.x);
                  m = a.bind ({_}: b.bind ({_}: c));
              in solo expectRun {} m {x = 1;} 4;

            setGetDifferentBlocksBind =
              let a = Eval.do ( {_}: _.set (EvalState {x = 1;}) );
                  b = Eval.do ( {_}: _.bind (xxx: _.get {}) );
                  m = a.bind (_: b);
              in solo expectRun {} m {x = 1;} (EvalState {x = 1;});

            setGetDifferentBlocks =
              let a = Eval.do ( {_}: _.set (EvalState {x = 1;}) );
                  b = Eval.do ( {_}: _.bind (xxx: _.get {}) );
                  m = Eval.do a b;
              in solo expectRun {} m {x = 1;} (EvalState {x = 1;});

            setGetDifferentBlocksInferred =
              let a = do (Eval.pure unit) ({_}: _.set (EvalState {x = 1;}) );
                  b = do (Eval.pure unit) ({_}: _.get {} );
                  m = do a b;
              in expectRun {} m {x = 1;} (EvalState {x = 1;});

            setGetScope =
              let m = Eval.do
                ( {_}: _.setScope ({x = 1;}) )
                ( {_}: _.getScope {} );
              in expectRun {} m {x = 1;} {x = 1;};

            setAppendGetScope =
              let m = Eval.do
                ( {_}: _.setScope ({x = 1;}) )
                ( {_}: _.appendScope ({y = 2;}) )
                ( {_}: _.getScope {} );
              in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

            setAppendGetScopeDifferentBlocks =
              let 
                a = Eval.do
                  ( {_}: _.setScope ({x = 1;}) );
                b = Eval.do
                  ( {_}: _.appendScope ({y = 2;}) );
                c = Eval.do
                  ( {_}: _.getScope {} );
                m = Eval.do a b c;
              in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

            overwriteScope =
              let m = Eval.do
                ( {_}: _.setScope ({x = 1;}) )
                ( {_}: _.appendScope ({x = 2;}) )
                ( {_}: _.getScope {} );
              in expectRun {} m {x = 2;} {x = 2;};

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
}
