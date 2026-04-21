{ lib, collective-lib, nix-reflect, ... }:

let 
  inherit (nix-reflect) parser debuglib eval;
  inherit (parser) parse isAST printASTName;
  inherit (collective-lib.typed.script-utils.ansi-utils) ansi;
in 
  with collective-lib.typed;
rec {
  # Do not move; predictable source location for reflection tests
  testData = {
    minimalDoBlock =
      (Eval.do
        {a = Eval.pure "pure string";}
        ({a, _}: _.pure a));

    doBlock =
      (Eval.do
        {a = Eval.pure "independent bind";}
        {b = pure "implicit dependent bind";}
        {c = {_}: _.pure "implicit dependent bind";}
        (Eval.pure "independent action")
        (pure "implicit dependent action")
        ({_}: _.pure "explicit dependent action")
        (Eval.do
          {a = Eval.pure "nested independent bind";}
          {b = pure "nested implicit dependent bind";}
          {c = {_}: _.pure "nested implicit dependent bind";}
          (Eval.pure "nested independent action")
          (pure "nested implicit dependent action")
          ({_}: _.pure "nested explicit dependent action")));
  };

  checkTypes = all (T: T ? check || isbuiltinName T);
    
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

  is = T: a: 
    if T ? check then T.check a
    else isbuiltinName T && typeOf a == T;

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
    inherit E A;
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
        case = case this;
        unwrap = unwrap this;
        fmap = _: this;
      }; in this;
  };

  __Right = E: A: assert checkTypes [E A]; {
    __toString = self: "(Either ${E} ${A}).Right";
    check = a: a ? __isRight && is A a.right;
    __functor = self: a:
      assert that (is A a) ''Either.Right: expected type ${A} but got ${_p_ a}'';
      let this = { 
        __type = Either E A;
        __isRight = true; 
        __toString = self: "Right ${_p_ a}"; 
        right = a; 
        case = case this;
        unwrap = unwrap this;
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
  unwrap = e: e.case {
    Left = e: e;
    Right = a: a;
  };

  EvalError = rec {
    __toString = self: "EvalError";
    check = x: x ? __isEvalError;
    __functor = self: name: {
      __toString = self: "EvalError.${name}";
      __isErrorType = true;
      check = x: (x ? __isEvalError) && (x ? "__isEvalError${name}");
      __functor = self: __msg: {
        __type = EvalError;
        __errorName = name;
        __isEvalError = true; 
        "__isEvalError${name}" = true; 
        __toString = self: _b_ ''
          EvalError.${name}:
            ${_h_ __msg}
        '';
        inherit __msg;
      };
    };
  };

  isErrorType = x: x ? __isErrorType;

  Abort = EvalError "Abort";
  AssertError = EvalError "AssertError";
  Throw = EvalError "Throw";
  TypeError = EvalError "TypeError";
  RuntimeError = EvalError "RuntimeError";
  SyntaxError = EvalError "SyntaxError";
  UnknownIdentifierError = EvalError "UnknownIdentifierError";
  MissingAttributeError = EvalError "MissingAttributeError";
  CatchableMissingAttributeError = EvalError "CatchableMissingAttributeError";
  NixPathError = EvalError "NixPathError";
  MissingThunkError = EvalError "MissingThunkError";
  InvalidThunkError = EvalError "InvalidThunkError";

  isCatchableError = x:
    isEvalError x
    && (is AssertError x || is Throw x || is CatchableMissingAttributeError x);

  isEvalError = x: x ? __isEvalError;

  # Handles forcing a thunk with current state cache and writing it back
  force = forceWith eval.ast.forceEvalNodeM;
  forceWith = f: x: {_, ...}:
    if !(isThunk x)
      then _.do
        (whileV 4 {_ = "not forcing a non-thunk of type ${lib.typeOf x}";})
        (pure x)
      else _.do
        (whileV 4 {_ = "forcing #${x.thunkId} via thunk cache";})
        # Force thunks, persisting the value in the cache upon first force.
        {thunkCache = getThunkCache;}
        ({thunkCache, _}: _.bind (thunkCache.forceThunkWith f x));

  isThunk = x: x ? __isThunk;

  mkThunk = node: {_, ...}:
    _.do
      (whileV 5 {_ = "constructing Thunk from:\n${_p_ node}";})
      (guard (isAST node) (RuntimeError ''
        mkThunk: expected an AST node but got ${_p_ node}
      ''))
      {scope = getScope;}
      ({scope, _}: _.pure (
        let 
          mk = {thunkId, before} @ args: lib.fix (self: {
            __isThunk = true;
            inherit thunkId;
            inherit (node) nodeType;
            __toString = self: "<CODE#${thunkId}|${self.nodeType}${optionalString (before != null) "|+"}>";

            # Adding before operations must write a new thunk to the cache,
            # otherwise lambda bodies would only ever evaluate to their first-evaluation value.
            forkWithBefore = self.__addBefore "forkWithBefore";
            unsafeAddBefore = self.__addBefore "unsafeAddBefore";

            # Used by the cache to set the thunkId to the next ID.
            # No code using thunks should ever make one without the cache as a factory,
            # so that the ID is always present in in-use thunks.
            setThunkId = thunkId: mk (args // { inherit thunkId; });

            # Should only be called by __addBefore
            __setBefore = before: mk (args // { inherit before; });

            # Add to the before operation. If mode == newThunk, creates a new cached thunk, otherwise
            # unsafely overwrites the thunk (only used for fine control over e.g. rec attrset creation
            # where there is no other reference to the thunk held yet)
            __addBefore = mode: newBefore: {_, ...}:
              let
                before' =
                  if before == null then newBefore
                  else {_, ...}: _.do before newBefore;
              in _.do
                {thunkCache = getThunkCache;}
                ({thunkCache, _}: _.bind (
                  let mapFn = {
                    forkWithBefore = thunkCache.mapThunkId;
                    unsafeAddBefore = thunkCache.unsafeUpdateThunkId;
                  }.${mode};
                  in mapFn (thunk: thunk.__setBefore before') self.thunkId));

            # Run the thunk with the given monadic state and extra scope.
            runThunkWith = f: {_, ...}:
              _.saveScope (_.do
                (whileV 3 {_ = _b_ ''computing thunk #${thunkId}'';})
                (setScope scope)
                (when (before != null) ({_, ...}:
                  _.do
                    (whileV 3 {_ = "running 'before' action on #${thunkId}";})
                    (before)))
                (f node));
            runThunk = {_, ...}: self.runThunkWith eval.ast.forceEvalNodeM;
          });
        in mk { thunkId = null; before = null; }));

  maybeThunk = node: if isThunk node then pure node else Thunk node;

  Thunk = node:
    # Do not allow identifier node-thunks, which can then refer to themselves in an infinite loop.
    #if node.nodeType == "identifier" then nix-reflect.eval.ast.evalIdentifier node
    #else {_}: _.do
    {_, ...}: _.do
      (while {_ = "constructing '${node.nodeType}' Thunk";})
      {thunkCache = getThunkCache;}
      ({thunkCache, _}: _.bind (thunkCache.cacheNode node));

  ThunkCache = {
    __toString = self: "ThunkCache";
    check = x: x ? __isThunkCache;
    __functor = self:
      {thunks ? {}, values ? {}, nextId ? 0, hits ? 0, misses ? 0}:
      (lib.fix (this: {
        __toString = self: self.print {};

        print = args: with script-utils.ansi-utils.ansi; box ({
          header = style [fg.magenta bold] "ThunkCache";
          body = _b_ ''
            ${_h_ (this.stats {})}

            ${_h_ (this.debug {})}
          '';
        } // args);

        __type = ThunkCache;
        __isThunkCache = true;
        inherit thunks values nextId hits misses;

        stats = {}: _b_ ''
          ${toString (size values)}/${toString (size thunks)} forced
          ${toString hits}/${toString (hits+misses)} hits
          next ID: ${toString nextId}
        '';
        debug = {}: _b_ ''
          thunks:
            ${_ph_ this.thunks}

          values:
            ${_ph_ this.values}
        '';

        # Cache and return the thunk that evaluates the node.
        cacheNode = node: {_, ...}:
          _.do
            {thunk = mkThunk node;}
            ({thunk, _}: _.bind (this.writeThunk thunk));

        # Write the next thunk to the cache, setting its ID.
        writeThunk = thunk_: {_, ...}:
          let thunkId = toString this.nextId;
              thunk = thunk_.setThunkId thunkId;
          in _.do
            (whileV 4 {_ = "writing #${thunkId} into cache";})
            (setThunkCache (ThunkCache {
              inherit (this) values hits misses;
              thunks = this.thunks // { ${thunkId} = thunk; };
              nextId = this.nextId + 1;
            }))
            (whileV 3 {_ = "returning #${thunkId} from writeThunk";})
            (pure thunk);

        # Write the thunk to the cache, assuming it already has an ID.
        unsafeWriteUpdatedThunk = thunk: {_, ...}:
          let thunkId = thunk.thunkId;
          in _.do
            (whileV 4 {_ = "unsafely writing #${thunkId} into cache:";})
            (guard (thunkId != null) (InvalidThunkError ''
              ThunkCache.unsafeWriteThunk: thunk ${thunk} has no thunkId:
                ${thunkCache}
            ''))
            (guard (this.thunks ? ${thunkId}) (MissingThunkError ''
              ThunkCache.unsafeWriteThunk: thunk ${thunk} cannot be unsafely written with the cache's next ID (${toString this.nextId})
            ''))
            (setThunkCache (ThunkCache {
              inherit (this) hits misses nextId;
              thunks = this.thunks // { ${thunkId} = thunk; };
              # Also invalidate the cache if this was already forced.
              values = removeAttrs this.values [thunkId];
            }))
            (whileV 3 {_ = "returning #${thunkId} from unsafeWriteThunk";})
            (pure thunk);

        # Apply a function Thunk -> Thunk to a thunk and store a new thunk in the cache.
        mapThunkId = f: thunkId: {_, ...}:
          _.do
            (guard (this.thunks ? ${thunkId}) (MissingThunkError ''
              ThunkCache.forceThunkId: thunkId ${thunkId} not found in cache:
                ${thunkCache}
            ''))
            (this.writeThunk (f this.thunks.${thunkId}));

        unsafeUpdateThunkId = f: thunkId: {_, ...}:
          _.do
            (guard (this.thunks ? ${thunkId}) (MissingThunkError ''
              ThunkCache.forceThunkId: thunkId ${thunkId} not found in cache:
                ${thunkCache}
            ''))
            (this.unsafeWriteUpdatedThunk (f this.thunks.${thunkId}));

        # Argument is just used as ID carrier.
        # TODO: ThunkCache could just hold nextId and values.
        # Uses the thunk from outside to respect forkBefore etc
        forceThunk = this.forceThunkWith eval.ast.forceEvalNodeM;
        forceThunkWith = f: thunk:
          let thunkId = thunk.thunkId;
              thunkCache = this;
          in {_}: _.do
            (whileV 3 {_ = "forcing thunk #${thunkId}";})
            (guard (thunkCache.thunks ? ${thunkId}) (MissingThunkError ''
              ThunkCache.forceThunkId: thunkId ${thunkId} not found in cache:
                ${thunkCache}
            ''))
            (let cachedThunk = thunkCache.thunks.${thunkId};
              in guard (thunk.nodeType == cachedThunk.nodeType) (InvalidThunkError ''
              ThunkCache.forceThunk:
                Provided thunk ${thunk} has nodeType ${thunk.nodeType}
                Cached thunk ${cachedThunk} has nodeType ${cachedThunk.nodeType}
            ''))
            ({_}:
              if thunkCache.values ? ${thunkId} then _.do
                (while {_ = "ThunkCache hit for #${thunkId}";})
                (setThunkCache (ThunkCache {
                  inherit (thunkCache) thunks values nextId misses;
                  hits = thunkCache.hits + 1;
                }))
                (pure thunkCache.values.${thunkId} )

              else _.do
                (while { _ = "ThunkCache miss for #${thunkId}"; })
                # Run the thunk and get both value and updated cache
                (whileV 5 {_ = "Calling runThunk to handle cache miss for #${thunkId}";})
                {result = thunk.runThunkWith f;}
                ({result, _}: _.do
                  # Get the updated cache from the state after evaluation since the thunk
                  # may have itself created/forced other thunks.
                  {updatedCache = getThunkCache;}
                  ({updatedCache, _}: _.do
                    (setThunkCache (ThunkCache {
                      inherit (updatedCache) thunks nextId hits;
                      misses = updatedCache.misses + 1;
                      values = updatedCache.values // { 
                        ${thunkId} = result;
                      };
                    }))
                    (pure result))));
      }));
  };

  EvalState = rec {
    __toString = self: "EvalState";
    check = x: x ? __isEvalState;
    mempty = _: initState;

    __functor = self:
      {scope ? initScope, thunkCache ? ThunkCache {}, stack ? Stack {}}: 
      lib.fix (this: {
        inherit scope thunkCache stack;
        publicScope = {}: removeAttrs scope ["__internal__"];

        __type = EvalState;
        __isEvalState = true;
        __toString = self: with script-utils.ansi-utils.ansi; box {
          header = style [fg.cyan bold] "EvalState";
          body = joinVerticalSep " " [
            (this.printScope {margin = zeros;})
            (this.thunkCache.print {margin = zeros;})
          ];
        };
        printScope = args: with script-utils.ansi-utils.ansi; box ({
          header = style [bold] "Scope";
          body = _b_ ''
            ${_ph_ (this.scope)}
          '';
        } // args);

        setScope = scope: EvalState { inherit (this) thunkCache stack; inherit scope; };
        setThunkCache = thunkCache: EvalState { inherit (this) scope stack; inherit thunkCache; };
        setStack = stack: EvalState { inherit (this) scope thunkCache; inherit stack; };

        fmap = f: this.setScope (f this.scope);
      });
    };

  resetScope = {_}: _.do
    {scope = getScope;}
    ({scope, _}: _.bind scope.__internal__.resetScopeM);

  getInitScope = {_}: _.do
    {scope = getScope;}
    ({scope, _}: _.bind (scope.__internal__.initScopeM));

  mkInitEvalState = scope: EvalState {
    scope = mkInitScope scope;
    thunkCache = ThunkCache {};
    stack = Stack {};
  };

  mkInitScope = scope: lib.fix (self: 
    scope // {
      __internal__ = {
        withScope = {};
        initScope = {}: self;
        initScopeM = {_}: _.pure self;
        resetScopeM = {_}: _.do
          (while {_ = "resetting scope";})
          (setScope self);
      };
    });

  testScope = scope: recursiveMergeAttrsList [ (mkInitScope initScope) scope ];

  initState = mkInitEvalState initScope;

  initScope = lib.fix (self: {
    NIX_PATH = {
      nixpkgs = <nixpkgs>;
    };
    PWD = /tmp/pwd;
    HOME = /tmp/home;

    true = true;
    false = false;
    null = null;

    # Override builtins with library versions
    builtins =
      with nix-reflect.parser; 
      with nix-reflect.eval.ast;
      removeAttrs builtins ["builtins"] // {
        # Move the recursive self-reference to an identifier to enable laziness.
        "builtins" = N.identifier "builtins";
        "throw" = N.throwExpr;
        "abort" = N.abortExpr;
        "assert" = N.assertExpr;
        # TODO: Native import
        #"import" = ...
        # Start shimming out native versions of key builtins
        #toString = (Eval.pure unit).bind (Thunk (parse "x: x.__toString x"));
      };

    # Expose same builtins on top-level as Nix
    inherit (self.builtins) "derivation" "import" "throw" "abort";

    # Expose minimal lib to avoid error blowup
    lib = {
      inherit (lib) isFunction attrValues;
      toString = self.builtins.toString;
    };
  });

  since = t: builtins.currentTime - t;

  Stack = 
    { maxDepth ? 10000, startTimestamp ? builtins.currentTime, stackFrames ? [], completedFrames ? [] }:
    lib.fix (self: seqId {
      inherit maxDepth startTimestamp stackFrames completedFrames;

      elapsed = {}: since self.startTimestamp;
      depth = size self.stackFrames;

      __toString = self: self.printCompleted {};
      printTrace = {}: _b_ ''
        ${self.printStack {}}
        ${self.printCompleted {}}
      '';

      printStack = {}: _b_ ''
        Stack | ${toString (size self.stackFrames)} frames
        ${_h_ (_ls_ (map (f: f.printTrace {}) (reverseList self.stackFrames)))}
      '';

      printCompleted = {}: _b_ ''
        Completed | ${toString (size self.completedFrames)} frames
        ${_h_ (_ls_ (map toString self.completedFrames))}
      '';

      push = node:
        assert that (self.depth < self.maxDepth) ''
          Stack.push: depth limit reached (${toString self.maxDepth})

          ${self}
        '';
        Stack {
          inherit (self) maxDepth startTimestamp completedFrames;
          stackFrames = [
            (StackFrame {
              inherit node;
              inherit (self) depth;
              startTimestamp = builtins.currentTime;
              endTimestamp = null;
            })
          ] ++ self.stackFrames;
       };

      pop = {}:
        if self.stackFrames == [] 
        then throw "Stack.pop: stack is empty"
        else
          let sfs = snoc self.stackFrames;
              completedHead = sfs.head.complete {};
          in {
            head = completedHead;
            tail = Stack {
              inherit (self) maxDepth startTimestamp;
              stackFrames = sfs.tail;
              completedFrames = self.completedFrames ++ [completedHead];
            };
          };
    });

  StackFrame = {startTimestamp ? builtins.currentTime, endTimestamp ? null, node, depth}:
    lib.fix (self: seqId {
      inherit node depth startTimestamp endTimestamp;

      elapsed = {}: since self.startTimestamp;
      __toString = self: with ansi; _b_ ''
        ${
          style [fg.grey] (replicate (self.depth + 1) ">")
        } ${
          style [fg.grey bold] "${self.node.nodeType} ${printASTName self.node}"
        }
      '';
      printTrace = {}: _b_ ''
        ${self}
        ${def "<no source>" (self.node.__src or null)}
      '';

      complete = {}:
        if self.endTimestamp != null then throw "StackFrame.complete: endTimestamp already set"
        else StackFrame {
          inherit (self) node depth startTimestamp;
          endTimestamp = builtins.currentTime;
        };
    });

  getStack = {_, ...}:
    _.do
      (whileV_ 5 "getting stack")
      {state = get;}
      ({_, state}: _.pure state.stack);

  pushStack = node: {_, ...}:
    _.do
      {state = get;}
      ({state, _}: _.do
        (whileV_ 5 "pushing stack frame #${toString (state.stack.depth + 1)}:\n${node}")
        (set (state.setStack (state.stack.push node))));

  popStack = {_, ...}:
    _.do
      (whileV_ 5 "popping stack frame")
      {state = get;}
      ({state, _}: 
        let popped = state.stack.pop {};
        in _.do
          (whileV_ 5 "popped stack frame #${toString popped.head.depth}:\n${popped.head}")
          (set (state.setStack popped.tail)));

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
    m.bind ({_}: _.pure unit);

  isDo = x: x ? __isDo;

  isDoOf = M: xdo: isDo xdo && tEq xdo.M M;

  isDoStatement = M: xs:
    log.while ("checking if ${_p_ xs} is a do-statement of ${M}") (
    (M != null && isMonadOf M xs)
    || (isFunction xs)
    || (isSolo xs && isDoStatement M (soloValue xs))
    );

  assertIsDoStatement = doSelf: M: xs:
    assert that (!(isSyntax xs)) ''
      do: encountered unapplied syntax element:

      ${_pv_ xs}

      (likely missing parentheses)
    '';
    assert that (isDoStatement M xs) ''
      do: expected a statement of form

        ( ${if M == null then "<monadic value>" else M Any} )

      or

        ( { bindings, ... }: ${if M == null then "<monadic value>" else M Any} )

      or

        { name = ${if M == null then "<monadic value>" else M Any}; }

      or

        { name = ({ bindings, ... }: ${if M == null then "<monadic value>" else M Any}); }

      but got ${_pv_ xs}
    '';
    true;

  withStackError = self: msg: _b_ ''
    ${msg}

    in

    ${toString self}
  '';

  unsetM = throw "do: cannot infer monadic type from lambda expression";

  unsetType = throw "do: cannot infer type from lambda expression";

  inferMonadFromStatement = dispatch {
    lambda = _: unsetM;
    set = xs:
      if isDo xs then xs.M
      else if isMonadValue xs then (getM xs)
      else if isSolo xs then flip dispatch (soloValue xs) {
        set = getM;
        lambda = _: unsetM;
      }
      else throw "do: malformed non-solo non-monad non-do set statement: ${_p_ xs}";
  };

  inferTypeFromStatement = dispatch {
    null = _: unsetType;
    lambda = _: unsetType;
    set = xs:
      if isMonadValue xs then (getT xs)
      else if isSolo xs then flip dispatch (soloValue xs) {
        set = getT;
        lambda = _: unsetType;
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

  mkNormalisedDoStatement = statement: bindName: f:
    if statement ? __isNormalisedDoStatement then statement
    else {
      __isNormalisedDoStatement = true;
      inherit bindName f;
    };

  # Idempotently convert all do statements to the same form.
  # { bindName = null | string; f = { _, _a, bindings... }: statement }
  normaliseBindStatement = M: dispatch.on (bindStatementSignature M) {
    IndependentAction = statement:
      mkNormalisedDoStatement statement null ({_ ? M.pure unit, _a ? unit, ...}: statement);
    DependentAction = statement: 
      mkNormalisedDoStatement statement null (addEllipsis statement);
    IndependentBind = statement:
      mkNormalisedDoStatement statement (soloName statement) ({_ ? M.pure unit, _a ? unit, ...}: soloValue statement);
    DependentBind = statement:
      mkNormalisedDoStatement statement (soloName statement) (addEllipsis (soloValue statement));
  };

  enableDoStatementRHSPrinting = true;
  tryPrintStatementRHS = M: f:
    let rhs = 
      if enableDoStatementRHSPrinting then try (f (nullRequiredArgs f // {_ = M.pure unit;})) (_: "<RHS eval failed>")
      else "<applicable RHS>";
    in if isString rhs then rhs else printStatement M rhs;

  printStatement = M: 
    dispatch.on (bindStatementSignature M) {
      IndependentAction = statement:
        if isDo statement then "(" + _b_ "${_h_ (printDo statement)})"
        else "(<${getT statement}>)";
      DependentAction = statement: 
        let args = builtins.functionArgs statement;
        in _b_ ''
          (${if size args > 0
              then "{${joinSep ", " (attrNames args)}, ...}"
              else "{?}"
            }: ${_h_ (tryPrintStatementRHS M statement)})
        '';
      IndependentBind = statement: _b_ ''
        ${debuglib.printPos (debuglib.pos.path statement)}
      '';
      DependentBind = statement: _b_ ''
        ${debuglib.printPos (debuglib.pos.path statement)}
      '';
    };

  printDo = self:
    let
      # Try to infer the do block position from the first self-contained statement
      # i.e. a bind statement where { k = ...; } is source-tracked
      doPos = 
        if empty self.__statements then null
        else
          let ps = filter (x: x != null) (map debuglib.pos.path self.__statements);
          in if empty ps then null else head ps;

      header =
        if doPos == null then null
        else 
          let 
            p = soloValue doPos;
          in 
            if (p ? file) && (p ? line)
            then "${builtins.baseNameOf p.file}:${toString p.line}"
            else null;

    in 
      if empty self.__statements then "<empty do-block>"
      else _b_ ''
        ${self.M}.do${optionalString (header != null) " <${header}>"}
          ${_h_ (joinLines (map (printStatement self.M) self.__statements))}
      '';

  tryApplyDoBinding = _: doSelf: statement: f: bindings:
    errors.try (let x = tryApply f bindings; in seq x x) (_e: _.throws (SyntaxError ''
      Failed to bind:
        ${printStatement doSelf.M statement}

      In do-block:
        ${doSelf}
      
      Against bindings:
        ${_ph_ bindings}
    ''));

  tryApplyBinding = _: M: statement: f: bindings:
    errors.try (let x = tryApply f bindings; in seq x x) (_e: _.throws (SyntaxError ''
      Failed to bind:
        ${printStatement M statement}
      
      Against bindings:
        ${_ph_ bindings}
    ''));

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
  handleBindStatement = doSelf: M: acc: statement:
    assert (assertIsDoStatement doSelf M statement);
    let normalised = normaliseBindStatement M statement;
    in
      (acc.m.bind ({_, _a}:
        let
          mb_ = 
            # After upgrading addEllipsis to use tryApply, this will return a
            # catchable error for some syntactic-type errors in do usage like
            # binding non-existing variables.
            let bindings = acc.bindings // { inherit _ _a; };
            in tryApplyDoBinding _ doSelf statement normalised.f bindings;
         mb = if isDo mb_ then mb_.__setInitM acc.m else mb_;
        in
          assert that (isMonadOf M mb) ''
            handleBindStatement: non-${M} value returned of type ${getT mb}:
              ${_ph_ mb}

            In do-block:
              ${_ph_ doSelf}
          '';
          mb.bind ({_, _a}: _.pure {
            bindings = acc.bindings // optionalAttrs (normalised.bindName != null) {
              ${normalised.bindName} = _a;
            };
            canBind = normalised.bindName == null;
            m = mb;
          })));

  # Do-notation functor that simply stores the statements given to it
  # When bound, it runs the statements in order to produce the monadic value
  # on which to call bind.
  mkDo = M: __initM: __statements:
    log.while ("constructing a do-block of\n${joinLines (map (printStatement M) __statements)}") (
    let this = {
      __isDo = true;
      __isMonad = true;
      __type = inferTypeFromStatement (maybeLast __statements);
      inherit M __initM __statements;

      __toString = printDo;

      __functor = self: statement:
        log.while ("lazily storing a do-statement:\n${printStatement M statement}") (
        mkDo M self.__initM (self.__statements ++ [statement])
        );

      __setInitM = initM: mkDo this.M initM this.__statements;

      # Bind with specified initial monadic value.
      # Just creates a new do with self as the initial value of the last one.
      bind = statement:
        log.while ("binding a do-statement:\n${printStatement M statement}") (
        M.do
          (this.action.bind statement)
        );

      sq = b: this.bind (_: b);

      # Actually force the block to evaluate down to a final M value.
      force = {}:
        #log.while "forcing do-block:\n${this}" (
        log.while "forcing do-block" (
        let 
          initAcc = {
            bindings = {};
            canBind = false;
            m = this.__initM;
          };
          accM = (M.pure unit).foldM (handleBindStatement this M) initAcc (this.__statements);
        in
          accM.bind ({_, _a}:
            assert that _a.canBind (withStackError this ''
              do: final statement of a do-block cannot be an assignment.
            '');
            _a.m)
        );

      action = this.force {};
      run = arg: this.action.run arg;
      run_ = arg: this.action.run_ arg;
      runM = arg: this.action.runM arg;
      runInitM = arg: this.action.runInitM arg;
      runInitM_ = arg: this.action.runInitM_ arg;
      runInit = arg: this.action.runInit arg;
      runInit_ = arg: this.action.runInit_ arg;
      getValueM = arg: this.action.getValueM arg;
      mapState = arg: this.action.mapState arg;
      setState = arg: this.action.setState arg;
      mapEither = arg: this.action.mapEither arg;
      catch = arg: this.action.catch arg;
      do = mkDo M this.action [];
    };
    in this
    );

  isSyntax = x: x ? __isSyntax;
  mkSyntax = f: {
    __isSyntax = true;
    __functor = self: f;
  };

  pure = mkSyntax (x: {_, ...}: _.pure x);
  throws = e: {_, ...}: _.throws e;
  guard = cond: e: {_, ...}: _.guard cond e;
  while = msg: {_, ...}: _.while msg;
  while_ = msg: {_, ...}: _.while_ msg;
  whileV = v: msg: {_, ...}: _.whileV v msg;
  whileV_ = v: msg: {_, ...}: _.whileV_ v msg;
  when = cond: m: {_, ...}: _.when cond m;
  unless = cond: m: {_, ...}: _.unless cond m;

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
    do = mkDo Eval (Eval.pure unit) [];
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
        e: assert that (is E e) ''
          Eval: expected Either value ${E} but got ${getT e}:
            ${_pv_ e}
        '';

        let 
          rebind = 
            this: 
            {A ? this.A, E ? Either Error A, s ? this.s, s' ? this.s', e ? this.e, __type ? Eval A} @ args: 
              fix (this_: 
                this 
                // args 
                // mapAttrs (_: f: f this_) this.__unbound
              );
          set_s = this: s: rebind this { inherit s; };
          set_s' = this: s': rebind this { s = const s'; inherit s'; };
          set_e = this: e: e.case {
            Left = e: set_e_Left this e;
            Right = a: set_e_Right this a;
          };
          set_e_Left = this: e_: rebind this (rec { A = Unit; E = Either Error Unit; e = E.Left e_;});
          set_e_Right = this: a_: rebind this (rec { A = getT a_; E = Either Error A; e = E.Right a_;});

          this = {
            __type = Eval A;
            __isEval = true;
            __toString = self: _b_ "Eval ${A} (${_ph_ self.e})";

            inherit S E A s e;
            s' = s (S.mempty {});

            __unbound = {
              # modify :: (EvalState -> EvalState) -> Eval A -> Eval {}
              modify = this: f: 
                if isLeft this.e then this else
                void (this.mapState (compose f));

              set = this: state: 
                if isLeft this.e then this else
                void (this.setState (const state));

              get = this: {_, ...}: _.pure (
                if this.strictState then this.s' else this.s (S.mempty {}));

              strictState = this: true;
              setState = this: s:
                if this.strictState 
                then let s' = s (S.mempty {}); in set_s' this s'
                else set_s this s;
              mapState = this: f: this.setState (f this.s);

              setEither = this: e: set_e this e;
              mapEither = this: f: this.setEither (f this.e);
              liftEither = this: e: if isEvalError e then this.throws e else this.pure e;

              getStack = this: this.bind getStack;
              pushStack = this: node: this.bind (pushStack node);
              popStack = this: this.bind popStack;

              getThunkCache = this: this.bind getThunkCache;
              setThunkCache = this: thunkCache: this.bind (setThunkCache thunkCache);

              getScope = this: this.bind getScope;
              getPublicScope = this: this.bind getPublicScope;
              setScope = this: newScope: this.bind (setScope newScope);
              setPublicScope = this: newScope: this.bind (setPublicScope newScope);
              modifyPublicScope = this: f: this.bind (modifyPublicScope f);
              saveScope = this: f: this.bind (saveScope f);
              modifyScope = this: f: this.bind (modifyScope f);
              appendScope = this: newScope: this.bind (appendScope newScope);

              getWithScope = this: this.bind getWithScope;
              setWithScope = this: newScope: this.bind (setWithScope newScope);
              appendWithScope = this: newScope: this.bind (appendWithScope newScope);

              do = this: statement: mkDo Eval this [] statement;
              pure = this: x: set_e_Right this x;
              fmap = this: f: set_e this (this.e.fmap f);
              liftA2 = this: f: aM: bM:
                this.do
                  {a = aM;}
                  {b = bM;}
                  ({a, b, _}: _.pure (f a b));

              when = this: cond: m: if cond then this.bind m else this.pure unit;
              unless = this: cond: m: if !cond then this.bind m else this.pure unit;

              whileV_ = this: v: s:
                # Add the stack logging to the monadic value itself
                log.while s (
                  # Add runtime tracing to the resolution of the bind only
                  this.bind ({_, ...}:
                    _.do
                      {scope = getScope;}
                      ({scope, _, ...}:
                        let d = scopeDiffSimple scope;
                            inScope = 
                              optionalString (d != {}) (with ansi; "in scope: ${style [fg.brightcyan] (_l_ d)}\n");
                        in _.bind (
                          (log.v v).show
                            # End ansi to avoid printf buffering
                            (ansi.end + "${inScope}while ${s}")
                            (this)))));

              # Supports extra source printing info via while {_ = "...";}, or just while "..."
              whileV = this: v: s_:
                with ansi;
                let
                  div = style [fg.grey] "│";
                  extra = this.e.case {
                    Left = style [fg.red];
                    Right = a: style [fg.green] (getT a);
                  };
                  s = 
                    if isAttrs s_
                    then _b_ (''
                      ${style [fg.black italic] "@${let p = (debuglib.pos.file s_)._; in "${p.file}:${toString p.line}"}"} ${div} ${extra}
                          ${_h_ ((style [fg.grey] "↳ │ ") + (_ls_ (mapTailLines (line: "  ${style [fg.grey] "│"} ${line}") (s_._))))}
                    '')
                    else s_;
                in this.whileV_ v s;

              while = this: s: this.whileV 3 s;
              while_ = this: s: this.whileV_ 3 s;

              guard = this: cond: e: 
                if cond 
                then this.bind ({_}: _.pure unit) 
                else (this.throws e);

              foldM = this: f: initAcc: xs:
                let startM = this.pure initAcc;
                in fold.left (accM: a: accM.bind ({_, _a}: _.bind (f _a a))) startM xs;

              # traverse :: (a -> Eval b) -> [a] -> Eval [b]
              traverse = this: f: xs:
                fold.left (accM: x: accM.bind ({_, _a, ...}: _.do
                  {value = f x;}
                  ({_, value}: _.pure (_a ++ [value]))))
                  (this.pure [])
                  xs;

              traverseAttrs = this: f: xs:
                this.do
                  {ss =
                    traverse
                      (x: {_, ...}: _.do
                        {value = f (soloName x) (soloValue x);}
                        ({value, _}: _.pure { ${soloName x} = value;}))
                      (solos xs);}
                  ({ss, _}: _.pure (mergeSolos ss));

              bind = this: statement: 
                this.e.case {
                  Left = _: this;
                  Right = a: 
                    let normalised = normaliseBindStatement Eval statement;
                        mb = tryApplyBinding this Eval statement normalised.f {_ = this; _a = a;};
                    in assert that (isMonadOf Eval mb) ''
                      Eval.bind: non-Eval value returned of type ${getT mb}:
                        ${_ph_ mb}
                    '';
                    mb.mapState (s: compose s this.s);
                };

              sq = this: b: this.bind (_: b);

              # Set the value to the given error.
              throws = this:
                e: 
                if is Error e
                then this.setEither (E.Left (e // { __printStackTrace = this.s'.stack.printTrace; }))
                else this.setEither (E.Left (RuntimeError ''
                  Eval.throws: expected Either value ${Error} but got ${_p_ e} of type ${getT e}
                ''));

              loggingCatchHandler = this: handler: err:
                (log.v 3).show ("\n" + (with ansi; box {
                  header = style [fg.green bold] "Caught Evaluation Error";
                  body = toString err;
                  margin = zeros;
                })) handler;

              # Catch specific error types and handle them with a recovery function
              # catch :: (EvalError -> Eval A) -> Eval A
              catch = this: handler:
                if isLeft this.e && isCatchableError this.e.left then 
                  (set_e_Right this unit)
                  .bind ((this.loggingCatchHandler handler this.e.left) this.e.left)
                else this;

              # Returns (Either EvalError { a :: A, s :: S })
              runM = this: initialState:
                Eval.do
                  (set initialState)
                  {a = this;}
                  {s = get;}
                  ({a, s, _}: _.pure {inherit a s;});
              runInitM = this: {_, ...}:
                _.do
                  ({_}: _.saveScope (
                    this.do
                      (resetScope)
                      {a = this;}
                      {s = get;}
                      ({a, s, _}: _.pure {inherit a s;})));
              runInitM_ = this: {_}: _.do
                {result = this.runInitM;}
                ({result, _}: _.pure result.a);
              run = this: initialState:
                (this.runM initialState).action.e;
              runInit = this: {}:
                (Eval.do this.runInitM).action.e;
              runInit_ = this: {}:
                (Eval.do this.runInitM_).action.e;
              run_ = this: _: this.e;
              getValueM = this: {_, ...}:
                _.bind
                  ({_}: ((this.run_ {}).case {
                    Left = _.liftEither;
                    Right = _.pure;
                  }));

            };
          };
        # Bind 'this'
        in rebind this {};
    };
  };

  liftA2 = f: aM: bM: {_, ...}: _.liftA2 f aM bM;
  foldM = f: initAcc: xs: {_, ...}: _.foldM f initAcc xs;
  traverse = f: xs: {_, ...}: _.traverse f xs;
  traverseAttrs = f: xs: {_, ...}: _.traverseAttrs f xs;

  get = {_, ...}: _.bind _.get;
  set = state: {_, ...}: _.set state;
  modify = f: {_, ...}: _.modify f;
  liftEither = e: {_, ...}: _.liftEither e;

  getThunkCache = {_, ...}:
    _.do
      (whileV 4 {_ = "getting thunk cache";})
      {state = get;}
      ({_, state}: _.pure state.thunkCache);

  setThunkCache = thunkCache: {_, ...}:
    _.do
      (whileV 4 {_ = "setting thunk cache";})
      {state = get;}
      ({_, state}: _.set (state.setThunkCache thunkCache));

  saveScope = f: {_, ...}:
    _.do
      (whileV 4 {_ = "saving scope";})
      {scope = getScope;}
      (whileV 5 {_ = "with saved scope";})
      {a = f;}
      ({_, scope, a, ...}: _.do
        (whileV 5 {_ = "restoring saved scope";})
        (setScope scope)
        (whileV 5 {_ = "after restoring saved scope";})
        (pure a));

  setScope = scope: {_, ...}:
    _.do
      (whileV 4 {_ = "setting scope";})
      {state = get;}
      ({state, _}: _.set (state.setScope scope));

  guardScopeUpdate = scope: {_, ...}: _.do
    (guard (!(scope ? __internal__)) (RuntimeError ''
      scope must not contain __internal__:
        ${_ph_ scope}
    ''));

  setPublicScope = scope: {_, ...}:
    _.do
      (whileV 5 {_ = "setting public scope";})
      (guardScopeUpdate scope)
      {state = get;}
      ({state, _}: _.set (state.setScope (scope // {inherit (state.scope) __internal__;})));
  
  modifyScope = f: {_, ...}:
    _.do
      (whileV 5 {_ = "modifying scope";})
      (modify (s: s.fmap f));

  modifyPublicScope = f: {_, ...}:
    _.do
      (whileV 5 {_ = "modifying public scope";})
      (modifyScope (scope: f scope // {inherit (scope) __internal__;}));

  getScope = {_, ...}:
    _.do
      #(whileV_ 5 {_ = "getting scope";})
      {state = get;}
      ({_, state}: _.pure state.scope);

  getPublicScope = {_, ...}:
    _.do
      (whileV 5 {_ = "getting public scope";})
      {state = get;}
      ({_, state}: _.pure (state.publicScope {}));

  appendScope = newScope: {_, ...}:
    _.do
      (whileV 4 {_ = "appending scope: ${_l_ (attrNames newScope)}";})
      (guardScopeUpdate newScope)
      (modifyScope (scope: scope // newScope));

  getWithScope = {_}: _.do
    (whileV 5 {_ = "getting 'with' scope";})
    {scope = getScope;}
    ({scope, _}: _.pure scope.__internal__.withScope);

  setWithScope = withScope: {_}: _.do
    (whileV 5 {_ = "setting 'with' scope";})
    {scope = getScope;}
    ({scope, _}: _.setScope (scope // {
      __internal__ = scope.__internal__ // { 
        inherit withScope; 
      };
    }));

  appendWithScope = withScope: {_}: _.do
    (whileV 4 {_ = "appending 'with' scope: ${_l_ (attrNames withScope)}";})
    {scope = getScope;}
    ({scope, _}: _.setScope (scope // {
      __internal__ = scope.__internal__ // { 
        withScope = scope.__internal__.withScope // withScope;
      };
    }));

  traceWithScope = {_}: _.do
    {scope = getWithScope;}
    ({scope, _}: _.whileV 1 {_ = "tracing 'with' scope:\n${_p_ scope}";});

  traceScope = {_}: _.do
    {scope = getScope;}
    ({scope, _}: _.whileV 1 {_ = "tracing scope:\n${_p_ scope}";});

  scopeDiff = scope: toReprDiff (diffShort 
    (removeAttrs initScope ["__internal__" "builtins"])
    (removeAttrs scope ["__internal__" "builtins"]));

  scopeDiffSimple = scope: 
    mapAttrs 
      (_: v: if isAST v then printASTName v else v)
      (removeAttrs scope (["__internal__"] ++ attrNames initScope));

  trackScope' = msg: scope: {_, ...}:
    _.whileV 1 {
      _ = _b_ ''
        tracking scope; ${msg}
        ${with script-utils.ansi-utils.ansi; box {
          styles = [bg.white fg.black];
          borderStyles = [fg.blue];
          header = style_ [bold fg.blue] "Scope";
          body = _p_ (scopeDiffSimple scope);
          margin = ones;
        }}
      '';
    };

  trackScope = msg: {_, ...}: _.do
    {scope = getScope;}
    ({scope, _}: _.bind (trackScope' msg scope));

  CODE = thunkId: nodeType: {
    inherit nodeType;
    thunkId = toString thunkId;
    __isThunk = true;
    __toString = collective-lib.tests.expect.anyLambda;
    __setBefore = collective-lib.tests.expect.anyLambda;
    __addBefore = collective-lib.tests.expect.anyLambda;
    forkWithBefore = collective-lib.tests.expect.anyLambda; 
    unsafeAddBefore = collective-lib.tests.expect.anyLambda;
    setThunkId = collective-lib.tests.expect.anyLambda;
    runThunk = collective-lib.tests.expect.anyLambda;
    runThunkWith = collective-lib.tests.expect.anyLambda;
  };

  expectEvalError = with tests; expectEvalErrorWith expect.noLambdasEq;

  expectEvalErrorWith = expectation: E: expr:
    let result = 
      (log.v 1).show
        (with ansi; "Expecting error: ${style [fg.red bold] (toString E)}")
        (eval.ast.runAST' expr);
    in expectation (rec {
      resultIsLeft = isLeft result;
      resultEMatches = is E (result.left or null);
      inherit E;
      resultE = result.left or result.right;
    }) {
      resultIsLeft = true;
      resultEMatches = true;
      inherit E;
      resultE = result.left or null;
    };

  removeBuiltins = s:
    if EvalState.check s then s.setScope (removeAttrs s.scope ["builtins"])
    else if isAttrs s then removeAttrs s ["builtins"]
    else s;

  expectRun = {
    actual, 
    expected,
    initialScope ? initScope,
    includeBuiltins ? false,
    expectedStack ? null,
    expectedScope ? null,
    buildExpectedScope ? mkInitScope,
    expectedThunkCache ? null
  }:
    with tests;
    let 
      maybeRemoveBuiltins = e: 
        if includeBuiltins then e else e // { 
          s = removeBuiltins e.s;
          a = removeBuiltins e.a;
        };

      initialState = mkInitEvalState initialScope;
      r = actual.run initialState;
    in 
      if isErrorType expected then
        expect.eqOn (e: e.left.__errorName or {__notLeft = e;})
          r
          {left = {__errorName = (expected "").__errorName;};}

      else 
        expect.eqOn
          (e: 
            if e ? right then Compare.NoLambdas e.right
            else Compare.NoLambdas e)
          (r.fmap maybeRemoveBuiltins)
          {
            right = maybeRemoveBuiltins {
              s = EvalState {
                scope =
                  if expectedScope == null then r.right.s.scope or r.left
                  else buildExpectedScope expectedScope;
                thunkCache =
                   def (r.right.s.thunkCache or null) expectedThunkCache;
                stack =
                  def (r.right.s.stack or null) expectedStack;
              };
              a = expected;
            };
          };

  expectRunError = s: a: e: 
    with tests;
    expect.noLambdasEq
      ((a.run (mkInitEvalState s)).left or {__notLeft = true;})
      (e // {__printStackTrace = expect.anyLambda;});

  expectRunNixError = s: a: 
    with tests;
    expect.error
      (a.run (mkInitEvalState s));

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
      _00_either =
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

      _01_state = {
        mk.public = expect.noLambdasEq ((EvalState {scope = mkInitScope {};}).publicScope {}) {};
        mk.private = expect.noLambdasEq (EvalState {scope = mkInitScope {};}).scope (mkInitScope {});
        fmap =
          expect.noLambdasEq
          ((EvalState {scope = {};}).fmap (scope: scope // {x = 1;}))
          (EvalState {scope = {x = 1;};});
      };

      _02_monad = 
        let
          a = rec {
            _42 = Eval.pure (Int 42);
            stateXIs2 = _42.bind (set (mkInitEvalState { x = 2; }));
            stateXTimes3 = stateXIs2.bind (modify (s: mkInitEvalState { x = s.scope.x * 3; }));
            const42 = stateXTimes3.pure (Int 42);
            getStatePlusValue = 
              const42.bind ({_, _a}:
                let i = _a;
                in (_.bind _.get).bind ({_, _a}: _.pure (Int (_a.scope.x + i.x))));
            thenThrows = stateXTimes3.bind ({_}: _.throws (Throw "test error"));
            bindAfterThrow = thenThrows.bind ({_}: _.pure "not reached");
            
            catchAfterThrow = thenThrows.catch (e: {_}: _.pure "handled error '${e}'");
            fmapAfterCatch = catchAfterThrow.fmap (s: s + " then ...");
          };
        in with EvalState; {
          __smoke = {
            isMonadOf.monad = expect.True (isMonadOf Eval (Eval.pure unit));
            isMonadOf.do = expect.True (isMonadOf Eval (Eval.do (Eval.pure unit)));
            isMonadOf.false = expect.False (isMonadOf Eval ((Either EvalError Int).Right (Int 42)));

            getM.monad = expect.True (tEq Eval (getM (Eval.pure unit)));
            getM.do = expect.True (tEq Eval (getM (Eval.do (Eval.pure unit))));

            getT.monad = expect.True (tEq (Eval Unit) (getT (Eval.pure unit)));
            getT.do = expect.True (tEq (Eval Unit) (getT (Eval.do (Eval.pure unit))));
          };

          _00_chain = {
            _00_pure = expectRun { actual = a._42; expected = Int 42; };
            _01_set = expectRun { actual = a.stateXIs2; expected = unit; expectedScope = { x = 2; }; };
            _02_modify = expectRun { actual = a.stateXTimes3; expected = unit; expectedScope = { x = 6; }; };
            _04_bind.get = expectRun { actual = a.getStatePlusValue; expected = Int 48; expectedScope = { x = 6; }; };
            _05_bind.thenThrows = expectRunError {} a.thenThrows (Throw "test error");
            _06_bind.bindAfterThrow = expectRunError {} a.bindAfterThrow (Throw "test error");
            _07_catch.noError = expectRun { actual = (a._42.catch (_: throw "no")); expected = Int 42; };
            _08_catch.withError = expectRun { actual = a.catchAfterThrow; expected = "handled error 'EvalError.Throw:\n  test error'"; expectedScope = { x = 6; }; };
            _09_catch.thenFmap = expectRun { actual = a.fmapAfterCatch; expected = "handled error 'EvalError.Throw:\n  test error' then ..."; expectedScope = { x = 6; }; };
          };
          
          _01_signatures = {
            IndependentAction.monad =
              expect.eq (bindStatementSignature Eval (Eval.pure unit)) "IndependentAction";
            IndependentAction.do =
              expect.eq (bindStatementSignature Eval (Eval.do (Eval.pure unit))) "IndependentAction";
            IndependentBind =
              expect.eq (bindStatementSignature Eval {a = Eval.pure unit;}) "IndependentBind";
            DependentAction =
              expect.eq (bindStatementSignature Eval ({_}: _.pure unit)) "DependentAction";
            DependentBind =
              expect.eq (bindStatementSignature Eval {a = {_}: _.pure unit;}) "DependentBind";
          };

          _02_inferMonad = {
            IndependentAction.monad =
              expect.True (tEq Eval (inferMonadFromStatement (Eval.pure unit)));
            IndependentAction.do =
              expect.True (tEq Eval (inferMonadFromStatement (Eval.do (Eval.pure unit))));
            IndependentBind =
              expect.True (tEq Eval (inferMonadFromStatement {a = Eval.pure unit;}));
            DependentAction =
              expect.error (inferMonadFromStatement ({_}: _.pure unit));
            DependentBind =
              expect.error (inferMonadFromStatement {a = {_}: _.pure unit;});
          };

          _03_doNotation = {
            _00_basic = {
              const = expectRun { actual = (Eval.do (Eval.pure 123)); expected = 123; };

              constBound = expectRun { actual = (Eval.do ({_}: _.pure 123)); expected = 123; };

              bindOne =
                let m = Eval.do {x = Eval.pure 1;} ({_, ...}: _.pure unit);
                in expectRun { actual = m; expected = unit; };

              bindOneBound =
                let m = Eval.do {x = {_}: _.pure 1;} ({_}: _.pure unit);
                in expectRun { actual = m; expected = unit; };

              bindOneGetOne =
                let m = Eval.do {x = Eval.pure 1;} ({_, x}: _.pure x);
                in expectRun { actual = m; expected = 1; };

              dependentBindGet = 
                let m = Eval.do
                  {x = {_}: _.pure 1;}
                  {y = {_, x}: _.pure (x + 1);}
                  ({_, x, y}: _.pure (x + y));
                in expectRun { actual = m; expected = 3; };

              boundDo = 
                let do = Eval.do; in with Eval;
                let m = do
                  {x = Eval.pure 1;}
                  {y = Eval.pure 2;}
                  ({x, y, ...}: Eval.pure (x + y));
                in expectRun { actual = m; expected = 3; };

              guard.pass =
                let m = Eval.do
                  ( {_}: _.guard true (TypeError "fail"))
                  ( {_}: _.pure unit );
                in expectRun { actual = m; expected = unit; };

              guard.fail =
                let m = Eval.do
                  ( {_}: _.guard false (TypeError "fail"))
                  ( {_}: _.pure unit );
                in expectRunError {} m (TypeError "fail");
            };

            _01_setGet = {
              _00_helpers = {
                _00_get = expectRun {
                  actual = Eval.do (getPublicScope);
                  expected = initScope;
                };

                _01_set = expectRun { 
                  actual = Eval.do (setPublicScope {x = 1;});
                  expected = unit;
                  expectedScope = mkInitScope {x = 1;};
                };

                _02_setSet = expectRun { 
                  actual = Eval.do
                    (setPublicScope {x = 1;})
                    (setPublicScope {y = 2;});
                  expected = unit;
                  expectedScope = {y = 2;};
                };

                _03_setModGet = expectRun { 
                  actual = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: scope // {x = scope.x + 2; y = 2;}))
                    (getPublicScope);
                  expected = {x = 3; y = 2;};
                  expectedScope = {x = 3; y = 2;};
                };

                _04_setModGetBlocks = 
                  let sets = {_, ...}: _.setPublicScope {x = 1;};
                      mods = {_, ...}: _.modifyPublicScope (scope: scope // {x = scope.x + 2; y = 2;});
                      gets = {_, ...}: _.getPublicScope;
                      m = Eval.do sets mods gets;
                  in expectRun { actual = m; expected = {x = 3; y = 2;}; expectedScope = {x = 3; y = 2;}; };

                _05_setModGetBlocksDo = 
                  let sets = Eval.do ({_, ...}: _.setPublicScope {x = 1;});
                      mods = Eval.do ({_, ...}: _.modifyPublicScope (scope: scope // {x = scope.x + 2; y = 2;}));
                      gets = Eval.do ({_, ...}: _.getPublicScope);
                      m = Eval.do sets mods gets;
                  in expectRun { actual = m; expected = {x = 3; y = 2;}; expectedScope = {x = 3; y = 2;}; };

                _06_useScope = 
                  let 
                    getClear = Eval.do
                      {scope = getPublicScope;}
                      (setPublicScope {cleared = true;})
                      ({_, scope}: _.pure scope);

                    xInc4 = Eval.do
                      {scope = getPublicScope;}
                      ({_, scope}: _.modifyPublicScope (scope': scope' // {x = scope.x + 1;}))
                      (modifyPublicScope (scope: scope // {x = scope.x + 3;}));

                    m = Eval.do
                      ({_}: _.setPublicScope {x = 1;})
                      xInc4
                      xInc4
                      getClear;

                  in expectRun { actual = m; expected = {x = 9;}; expectedScope = {cleared = true;}; };
              };

              _01_blocks = {
                _00_do =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                _01_chainBlocks =
                  let a = Eval.do (setPublicScope {x = 1;});
                      b = Eval.do a getPublicScope;
                      m = Eval.do b;
                  in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                _02_withoutDo =
                  let a = setPublicScope {x = 1;};
                      b = modifyPublicScope (scope: scope // {x = scope.x + 1;});
                      c = {_}: (_.bind _.getPublicScope).bind ({_, _a}: _.pure _a.x);
                      m = (((Eval.pure unit).bind a).bind b).bind c;
                  in expectRun { actual = m; expected = 2; expectedScope = {x = 2;}; };

                _03_getScope =
                  expectRun {
                    actual = Eval.do
                      (setPublicScope {x = 1;})
                      getPublicScope;
                    expected = {x = 1;};
                    expectedScope = {x = 1;};
                  };

                _04_appendScope =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: scope // {y = 2;}))
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                _05_overwriteScopeAppend =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: scope // {x = 2;}))
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 2;}; expectedScope = {x = 2;}; };

                _06_overwriteScopePrepend =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: {x = 2;} // scope))
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };
                
                _07_saveScopeAppendScope =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (saveScope ({_}: _.do
                      (appendScope {y = 2;})
                      getPublicScope
                    ));
                  in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1;}; };

                _08_nestedSaveScope =
                  expectRun { 
                    actual = 
                      Eval.do
                        (setPublicScope {x = 1;})
                        (saveScope ({_}: _.do
                          (appendScope {y = 2;})
                          (saveScope ({_}: _.do
                            (appendScope {z = 3;})
                            getPublicScope
                          ))
                        ));
                    expected = {x = 1; y = 2; z = 3;};
                    expectedScope = {x = 1;};
                  };

                _09_withScope = {
                  _00_empty = 
                    expectRun { 
                      actual = (Eval.do (setWithScope {})); 
                      expected = unit; 
                    };
                  _01_set = 
                    expectRun {
                      actual = (Eval.do (setWithScope {x = 1;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope.x = 1; };
                    };
                  _02_append = 
                    expectRun {
                      actual = (Eval.do (appendWithScope {x = 1;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope.x = 1; };
                    };
                  _03_overwrite = 
                    expectRun {
                      actual = (Eval.do (setWithScope {x = 2;}) (appendWithScope {x = 3;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope.x = 3; };
                    };
                  _04_appendAppend = 
                    expectRun { 
                      actual = (Eval.do (appendWithScope {x = 1;}) (appendWithScope {y = 2;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope = {x = 1; y = 2;}; };
                    };
                  _05_appendAppendAppend = 
                    expectRun { 
                      actual = (Eval.do (appendWithScope {x = 1;}) (appendWithScope {y = 2;}) (appendWithScope {z = 3;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope = {x = 1; y = 2; z = 3;}; };
                    };
                };

                _10_differentBlocks = {
                  _00_differentBlocks =
                    let a = setPublicScope {x = 1;};
                        b = getPublicScope;
                        m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _01_differentBlocksBind =
                    let a = setPublicScope {x = 1;};
                        b = getPublicScope;
                        m = ((Eval.pure unit).bind a).bind b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _02_differentDoBlocks =
                    let a = {_}: _.do (setPublicScope {x = 1;});
                        b = {_}: _.do getPublicScope;
                        m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _03_differentEvalDoBlocks =
                    let a = Eval.do (setPublicScope {x = 1;});
                        b = Eval.do getPublicScope;
                        m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _04_appendScopeDifferentEvalBlock =
                    let 
                      a = 
                        Eval.do 
                          (setPublicScope {x = 1;})
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      m = Eval.do a;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _05_appendScopeDifferentEvalBlocks =
                    let 
                      a = 
                        Eval.do 
                          (setPublicScope {x = 1;})
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      b = 
                        Eval.do 
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _06_appendScopeDifferentBlock =
                    let 
                      a = 
                        {_}: _.do 
                          (setPublicScope {x = 1;})
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      m = Eval.do a;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _07_appendScopeDifferentBlocks =
                    let 
                      a = {_}: _.do (setPublicScope {x = 1;});
                      b = {_}: _.do (modifyPublicScope (scope: scope // {y = 2;}));
                      c = {_}: _.do getPublicScope;
                      m = Eval.do a b c;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _08_appendScopeDifferentBlocksBindNoDo =
                    let 
                      a = setPublicScope {x = 1;};
                      b = modifyPublicScope (scope: scope // {y = 2;});
                      c = getPublicScope;
                      m = (((Eval.pure unit).bind a).bind b).bind c;
                    in expectRun { actual = Eval.do m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };
                };
              };

              _02_printDo = {
                minimalDoBlock = expect.eq (toString testData.minimalDoBlock) (_b_ ''
                  Eval.do <monad.nix:14>
                    {a = Eval.pure "pure string";}
                    ({_, a, ...}: (<Eval Unit>))
                '');
                doBlock = expect.eq (toString testData.doBlock) (_b_ ''
                  Eval.do <monad.nix:19>
                    {a = Eval.pure "independent bind";}
                    {b = pure "implicit dependent bind";}
                    {c = {_}: _.pure "implicit dependent bind";}
                    (<Eval string>)
                    ({_, ...}: (<Eval Unit>))
                    ({_, ...}: (<Eval Unit>))
                    (Eval.do <monad.nix:26>
                      {a = Eval.pure "nested independent bind";}
                      {b = pure "nested implicit dependent bind";}
                      {c = {_}: _.pure "nested implicit dependent bind";}
                      (<Eval string>)
                      ({_, ...}: (<Eval Unit>))
                      ({_, ...}: (<Eval Unit>)))
                '');
              };
              
              _03_composes = 
                let a = Eval.do (modifyPublicScope (scope: scope // {x = 1;}));
                    b = Eval.do (modifyPublicScope (scope: scope // {y = 2;}));
                in {
                  _00_scopeExists = expectRun { 
                    actual = a; 
                    expected = unit;
                    buildExpectedScope = testScope;
                    expectedScope = {x = 1;};
                  };

                  #_01_doBind = expectRun { 
                  #  actual = Eval.do a b;
                  #  expected = unit;
                  #  buildExpectedScope = testScope;
                  #  expectedScope = {x = 1; y = 2;};
                  #};

                  #_02_doSq = expectRun { 
                  #  actual = a.sq b;
                  #  expected = unit;
                  #  buildExpectedScope = testScope;
                  #  expectedScope = {x = 1; y = 2;};
                  #};
                };
            };

            _02_functions = {
              _00_foldM = {
                empty = expectRun { actual = (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [])); expected = 0; };
                single = expectRun { actual = (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [5])); expected = 5; };
                multiple = expectRun { actual = (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [1 2 3])); expected = 6; };
              };

              _01_traverse = {
                _00_empty = expectRun { actual = (Eval.do ({_}: _.traverse (x: Eval.pure (x + 1)) [])); expected = []; };
                _01_single = expectRun { actual = (Eval.do ({_}: _.traverse (x: Eval.pure (x + 1)) [5])); expected = [6]; };
                _02_multiple = expectRun { actual = (Eval.do ({_}: _.traverse (x: Eval.pure (x * 2)) [1 2 3])); expected = [2 4 6]; };
                
                # Simple test showing get() works correctly in do-block  
                _03_simpleGetIssue = expectRun {
                  actual = (
                    Eval.do
                      (set (mkInitEvalState {test = "value";}))
                      {stateAfterSet = get;}
                      ({_, stateAfterSet}: _.pure (stateAfterSet.publicScope {}))
                  );
                  expected = { test = "value"; };
                  expectedScope = { test = "value"; };
                };
                
                # Test that traverse properly threads state with foldM implementation
                _04_traverseWithState = expectRun {
                  actual = (
                    Eval.do
                      (set (mkInitEvalState {counter = 0; seen = [];}))
                      {result = {_, ...}: _.traverse (x: 
                        Eval.do
                          {state = get;}
                          ({_, state}: _.set (EvalState {scope = state.scope // {
                            counter = (state.scope.counter or 0) + x;
                            seen = (state.scope.seen or []) ++ [x];
                          };}))
                          ({_}: _.pure x)
                      ) [1 2 3 4 5];}
                      {finalState = get;}
                      ({_, result, finalState}: _.pure {
                        result = result;
                        finalCounter = finalState.scope.counter;
                      })
                  );
                  expected = {
                    result = [1 2 3 4 5];
                    finalCounter = 15;
                  };
                  expectedScope = { counter = 15; seen = [1 2 3 4 5]; };
                };

                _05_traverseAttrs = expectRun {
                  actual =
                    Eval.do
                      (set (mkInitEvalState {counter = 0; seen = [];}))
                      (traverseAttrs (k: v: {_}:
                        _.do
                          (modifyScope (s: {
                            sum = (s.sum or 0) + v;
                            seen = (s.seen or []) ++ [k];
                          }))
                          (pure "ok"))
                        {a = 1; b = 2; c = 3;});
                  expected = {a = "ok"; b = "ok"; c = "ok";};
                  expectedScope = { sum = 6; seen = ["a" "b" "c"]; };
                  buildExpectedScope = id;
                };
              };
            };

            _03_thunkCache = with parser; {
              _00_getEmpty =
                expectRun {
                  actual =
                    Eval.do
                      getThunkCache;
                  expected = ThunkCache {};
                };

              _01_put = 
                expectRun {
                  actual =
                    Eval.do
                      (Thunk (N.string "x"));
                  expected = CODE 0 "string";
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; };
                    nextId = 1;
                  };
                };

              _02_putGet = 
                expectRun {
                  actual =
                    Eval.do
                      {x = Thunk (N.string "x");}
                      ({x, _}: _.bind (force x));
                  expected = "x";
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; };
                    values = { "0" = "x"; };
                    misses = 1;
                    hits = 0;
                    nextId = 1;
                  };
                };

              _03_putGetGet = 
                expectRun {
                  actual =
                    Eval.do
                      {x = Thunk (N.string "x");}
                      ({x, _}: _.do
                        {x'0 = force x;} # Miss
                        {x'1 = force x;} # Hit
                        ({x'0, x'1, _}: _.pure [x'0 x'1]));
                  expected = ["x" "x"];
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; };
                    values = { "0" = "x"; };
                    misses = 1;
                    hits = 1;
                    nextId = 1;
                  };
                };

              _04_putGetMany = 
                expectRun {
                  actual =
                    Eval.do
                    {x = Thunk (N.string "x");}
                    {y = Thunk (N.int 1);}
                    ({x, y, _}: _.do
                      {x'0 = force x;} # Miss
                      {x'1 = force x;} # Hit
                      {x'2 = force x;} # Hit
                      {y'0 = force y;} # Miss
                      {x'3 = force x;} # Hit
                      {y'1 = force y;} # Hit
                      {x'4 = force x;} # Hit
                      ({x'0, x'1, x'2, x'3, x'4, y'0, y'1, _}: _.pure [x'0 x'1 x'2 x'3 x'4 y'0 y'1]));
                  expected = ["x" "x" "x" "x" "x" 1 1];
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; "1" = CODE 1 "int"; };
                    values = { "0" = "x"; "1" = 1; };
                    misses = 2;
                    hits = 5;
                    nextId = 2;
                  };
                };

              #_04_thunksCaptureScope = 
              #  expectRun {
              #    actual = 
              #      Eval.do
              #        (setScope {a = 1;})
              #        {x = Thunk (N.list (N.identifier "a"));}
              #        (setScope {a = 2;})
              #        {y = Thunk (N.list (N.identifier "a"));}
              #        ({x, y, _}: _.pure (x ++ y));
              #    expected = [1 2];
              #    expectedScope = {a = 2;};
              #    expectedThunkCache = ThunkCache {
              #      thunks = {
              #        "0" = CODE 0 "list";
              #        "1" = CODE 1 "identifier";
              #        "2" = CODE 2 "list";
              #        "3" = CODE 3 "identifier";
              #      };
              #      values = { 
              #        "0" = [ (Code 1 "identifier") ];
              #        "1" = 1;
              #        "2" = [ (Code 3 "identifier") ];
              #        "3" = 2;
              #      };
              #      misses = 0;
              #      hits = 4;
              #      nextId = 5;
              #  };
              #};
          };

          _04_syntax = {
            _00_missingBind =
              expectRun {
                actual = (Eval.do ({a, _}: _.pure a));
                expected = SyntaxError;
              };
            _01_missingBindNotCatchable =
              expectRun {
                actual = ((Eval.do ({a, _}: _.pure a)).catch (_e: pure "got ${_e}"));
                expected = SyntaxError;
              };
            _02_missingParens = 
              expectRun {
                actual = Eval.do pure 1;
                expected = SyntaxError;
              };
          };

        };
      };
    };
}
