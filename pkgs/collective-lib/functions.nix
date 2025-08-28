# Predictable non-moving source location for test data.
let 
  testData = {
    f = {
      opt = {b, a ? 1}: a + b;
      req = {b ? 2, a ? 1}: a + b;
      cb = {b ? 2, a ? 1}: callback:
        if callback != null then callback { inherit a b; } else a + b;
    };
  };
in

{ 
  pkgs ? import <nixpkgs>,
  lib ? pkgs.lib,
  collective-lib ? import ./. { inherit lib; },
  ... 
}:

with collective-lib.nix-reflect.debuglib;

with lib;
with collective-lib.attrsets;
with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.errors;
with collective-lib.lists;
with collective-lib.strings;
with collective-lib.syntax;
with collective-lib.tests;

# Misc functional utilities
let
  log = collective-lib.log;
  errors = collective-lib.errors;
  typelib = collective-lib.typelib;
  typed = collective-lib.typed;
in rec {
  pointerEqual = a: b: [ a ] == [ b ];

  # Compose two functions left-to-right
  compose = dispatch.on (x: boolToString (isFunction x)) {
    true = f: g: a: f (g a);
    false = dispatch {
      list = composeMany;
    };
  };

  # Compose two functions left-to-right
  precompose = dispatch.on (x: boolToString (isFunction x)) {
    true = f: g: a: g (f a);
    false = dispatch {
      list = precomposeMany;
    };
  };

  composeMany = typed.foldl' compose id;
  precomposeMany = typed.foldl' precompose id;

  # Apply a function to a value
  ap = {
    __functor = self: f: a: f a;
    list = f: xs: typed.fold.list.left (f: x: f x) f xs;
  };

  # Apply a function to a value with the arguments flipped.
  flap = a: f: ap f a;

  # Flipped maps.
  for = flip map;
  ifor = flip imap0;
  forAttrs = flip mapAttrs;
  forAttrsToList = flip mapAttrsToList;
  concatForAttrs = flip concatMapAttrs;

  # Return a value iff a condition is met, otherwise return null
  when = cond: a: if cond then a else null;

  # Return a value iff a condition is not met, otherwise return null
  unless = cond: when (!cond);

  # Wrap a possibly null value with a default.
  # Can use as e.g. person.name = def "Unknown" (person.name or null);
  #                 xs = maybeTail ys |> def [];
  def = d: a: if a == null then d else a;

  # Evaluate 'a' strictly, forcing all of its components, and return the final value.
  strict = a: deepSeq a a;

  # Functional 'not' for composition.
  not = x: !x;

  # Create a composition pipe
  # pipe x doA doB ___;
  pipe = x:
    Variadic.mkListThen
      (fs: let f = typed.foldl1 compose (reverseList fs); in f x);

  # Add an implied ellipsis to the given function of a arg-set without one.
  # f = {a, b}: a + b;
  # g = {a, b, ...}: a + b;
  # f {a = 1; b = 2;} = 3
  # f {a = 1; b = 2; c = 3;} = error
  # g {a = 1; b = 2; c = 3;} = 3
  # addEllipsis f {a = 1; b = 2;} = 3
  # addEllipsis f == g;
  # addEllipsis f {a = 1; b = 2; c = 3;} = 3
  addEllipsis = f:
    if f ? __isAddedEllipsis then f else
    let fArgs = lib.functionArgs f;
    in if (empty fArgs) then {...}@args: f args
    else {
      __isAddedEllipsis = true;
      __functor = self: args: f (intersectAttrs fArgs args); 
    };

  nullArgs = f:
    mapAttrs (_: _: null) (lib.functionArgs f);

  unitArgs = f:
    mapAttrs (_: _: {}) (lib.functionArgs f);

  argNames = f: attrNames (lib.functionArgs f);
  requiredArgs = f: filterAttrs (_: v: !v) (lib.functionArgs f);
  defaultArgs = f: filterAttrs (_: v: v) (lib.functionArgs f);
  missingRequiredArgNames = f: args: attrNames (removeAttrs (requiredArgs f) (attrNames args));

  # Get the names of arguments that were supplied but not expected by the function.
  # Can't know about ellipses, so this isn't an error by itself.
  suppliedUnknownArgs = f: args: attrNames (removeAttrs args (argNames f));

  nullRequiredArgs = f: mapAttrs (_: _: null) (requiredArgs f);
  unitRequiredArgs = f: mapAttrs (_: _: {}) (requiredArgs f);

  hasFunctionArgs = f: lib.functionArgs f != {};

  # Function application that throws a catchable error if arguments are missing,
  # rather than the default behavior of:
  # Missing arguments: ({a}: a) {} -> error: function 'anonymous lambda' called without required argument 'a'
  # Non-attrset arguments: ({a}: a) 1 -> error: expected a set but found an integer: 1
  # Can't distinguish between a zero-arg function that only accepts {} and a regular function,
  # so can't catch (({}: 1) 2)
  tryApply = f: args:
    if typed.isFunctor f then tryApply (f.__functor f) args
    else if builtins.isFunction f then
      if hasFunctionArgs f then
        assert that (isAttrs args) "tryApply: expected attrset, got ${lib.typeOf args}";
        let missingArgs = missingRequiredArgNames f args; in
        assert that (missingArgs == []) "tryApply: expected attrset to contain all required arguments; missing: ${joinSep ", " missingArgs}";
        f args
      else f args
    else _throw_ "tryApply: expected function, got ${lib.typeOf f}";

  # Make a thunk out of a value
  thunk = x: _: x;

  # Resolve a thunk.
  resolve = x:
    if (x ? __resolve) then
      x.__resolve x
    else if typelib.isFunction x then
      x (throw ''Resolved lambda-thunk made use of its thunk-argument.'')
    else if isThunkSet x then
      x.__get {}
    else throw (indent.block ''
      resolve: Invalid argument type: ${typeOf x}
        ${indent.here (log.print x)}
    '');

  # Resolve a thunk, throwing an error if the resolving value is used.
  # Any catchable error thrown by the thunk will be propagated with extra logging.
  tryResolve = x:
    let propagateResolutionError = e:
      throw (indent.block ''
        resolve: thunk resolution error:
          Thunk: ${indent.here (log.print x)}
          Error: ${indent.here (log.print e)}
        '');
    in errors.try (resolve x) propagateResolutionError;

  # True iff x is a thunk that resolves to x via regular equality.
  resolvesTo = th: to: resolvable th && resolve th == to;

  # True iff x resolves to a value where f value == true
  resolvesWith = f: th:
    let f_th = thunkDo th f;
    in
      assert assertMsg (isBool f_th) (indent.block ''
        resolvesToWith: f provided did not return a bool:
          to = ${indent.here (log.print to)}
          th = ${indent.here (log.print th)}
          f = ${indent.here (log.print f)}
          thunkDo th f (expected bool) = ${indent.here (log.print f_th)}
      '');
      resolvable th && f_th;

  # Thunkify x if it is not already a thunk.
  maybeThunk = x: if isThunkSet x then x else Thunk;

  # Resolve a thunk if x is one, otherwise return x.
  maybeResolve = x: try (resolve x) (_: x);

  # Resolve x until it is no longer a thunk.
  resolveDeep = x:
    try (
      if resolvable x
      then let x_ = strict (try (resolve x) (_: throw ''resolveDeep: failed to resolve''));
           in resolveDeep x_
      else x
    ) (_: x);

  # Object wrapping a thunk with metadata.
  Thunk = x:
    let
      # Make a new TT with the given depth and Type T.
      mkThunk = x: rec {
        # Marker for identifying TTs, since they live outside the type system.
        __isThunkSet = true;

        # Store x as a lambda thunk for returning by __resolve.
        # To change the value of the thunk, one can just re-bind __x.
        __x = _: x;

        # Get the resolved Type. Must be a regular thunk itself to avoid recursion.
        # This is picked up by 'resolve (mkThunk x)' to give x
        __resolve = self: self.__x {};

        # Also implement functor s.t. thunks can be called via 'someThunk {}'
        __functor = self: _: self.__x {};

        # Display thunks
        __toString = self:
          let arrow = if (self ? __ThunkName) then ">-[${self.__ThunkName}]->" else ">->";
          in thunkDo self (x: indent.lines (
            ["${self.__ThunkType} ${arrow} ${self.__showValue self}"]
            ++ (let extra = self.__showExtra self;
                    extraStr = log.show extra;
                in optionals (extra != null && size extraStr != 0) ["  ${extraStr}"])));
        __show = __toString;

        # Override in other thunk types to affect showa showable
        __ThunkType = "Thunk";
        __showValue = self: if x ? __TypeId then x.__TypeId {} else typeOf x;
        __showExtra = self: "";
      };
    in
      mkThunk x;

  # True iff the value is a Thunk set
  isThunkSet = x: isAttrs x && (x.__isThunkSet or false);

  # True iff the value is a resolvable
  resolvable = x: isFunction x || x ? __resolve;

  # Run a function over the resolved Type, returning its unthunked value.
  thunkDo = th: f: f (resolve th);

  # Run a function over the resolved Type, retaining the thunk structure around the value.
  thunkFmap = th: f:
    if isThunkSet th then th // {__x = _: thunkDo th f;}
    else if isFunction th then _: thunkDo th f
    else throw (indent.block ''
      thunkFmap: Invalid type of thunk (not resolvable):
        ${indent.here (log.print th)}
      '');

  setThunkName = name: x:
    assert isThunkSet x;
    x // { __ThunkName = name; };
  NamedThunk = name: x: setThunkName name (Thunk x) // { __isNamedThunk = true; };
  isNamedThunk = x: isThunkSet x && (x.__isNamedThunk or false);


  # Compose two functions left-to-right and merge their outputs.
  # For example:
  # f = sequentialWith mergeAttrs (b: c: {inherit b c;}) (a: b: {inherit a b;});
  # f "a" "b" "c" "d" = { a = "a"; b = "b"; c = "c"; d = "d"; }
  fjoin = mergeFn: g: f:
    let g_ = fx: Variadic.compose (gx: mergeFn gx fx) g;
    in Variadic.compose g_ f;


  # Variadic end-marker for otherwise-ambiguous termination
  ___ = { __end = true; };

  # Variadic function builder.
  Variadic = rec {
    # Default settings for taking arguments of e.g. f { a = 1; } { b = 2; } -> { a = 1; b = 2; }
    defaults = {
      # Start with empty accrued args.
      initialState = {};
      # Merge any incoming arg attrsets.
      handle = state: arg: state // arg;
      # Return the plain state
      terminate = state: _: state;
      # Simple bool checks on {state, arg}
      check = _: _: true;
      # Function from nextState, arg to true iff arg is the final arg.
      isTerminal = throw "Variadic.mk: isTerminal must be set";
    };

    # Construct a variadic function from the given spec.
    # Variadic.mk or just Variadic { ...}
    __functor = self: spec_: self.mk spec_;
    mk = spec_:
      let 
        # Pull in defaults.
        # isTerminal will always be required as a custom setting.
        spec = defaults // spec_;

        # The inner function carries a set of parameters built up from the
        # non-terminal arguments.
        f = prevState: arg:
          let 
            nextState = spec.handle prevState arg;
            errors =
              nonEmpties [
                (optionalString (!(spec.check nextState arg)) "Basic 'check' failed.")
                (checkPredMsgs { inherit prevState nextState arg; } (spec.predMsgs or []))
              ];
          in
            if errors != []
            then throw (indent.block ''
              Variadic.mk: check failed:

              Errors:
                ${indent.here (joinLines (map (msg: "- ${msg}") errors))}

              Previous State:
                ${indent.here (log.print prevState)}

              Last argument:
                ${indent.here (log.print arg)}

              Next State:
                ${indent.here (log.print nextState)}
            '')
            else if (spec.isTerminal nextState arg)
              then spec.terminate nextState arg
            else f nextState;

      in 
        if spec.isTerminal spec.initialState null
        then spec.terminate spec.initialState null
        else arg: f spec.initialState arg;

    mkThen = f: spec:
      let g = Variadic spec;
      in Variadic.compose f g;

    # Build a variadic function that merges its attrset arguments.
    set_ = isTerminal: {
      inherit isTerminal;
      initialState = { xs = {}; };
      handle = state: x:
        if isTerminal state x
        then state
        else { xs = state.xs // x; };
      terminate = state: _: state.xs;
    };
    mkSet_ = isTerminal: Variadic (set_ isTerminal);
    mkSet = mkSet_ (_: x: x == ___);
    mkSetThen = f: Variadic.compose f mkSet;
    mkSetFrom = init: mkSetThen (xs: init // xs);
    mkSetFromThen = init: f: Variadic.compose f (mkSetFrom init);
    # Accrue arguments into a set until the given size is met
    mkSetOfMinSize = l: mkSet_ (state: _: (size state.xs) >= l);

    # Build a variadic function that accepts partial attrsets until
    # exactly the given names are present.
    argNames = names: {
      isTerminal = state: _: attrNames state == names;
      check = _: x: isAttrs x && all (name: elem name names) (attrNames x);
    };
    mkArgNames = names: Variadic (argNames names);

    # Expect arguments to be passed as single values in the order provided
    mkOrderedThen_ = f: allowPartial: fieldOrder:
      Variadic.compose
        (xs:
          let xs' =
            mergeAttrsList
              (zipListsWith
                (name: value: { ${name} = value; })
                fieldOrder
                xs);
          in f xs')
        (if allowPartial then mkListOfMaxLength (length fieldOrder)
         else mkListOfLength (length fieldOrder));

    # Variadic function that accepts exactly the given field names in the order provided.
    mkOrdered = mkOrderedThen_ id false;
    mkOrderedThen = f: mkOrderedThen_ f false;

    # Variadic function that accepts the given field names in the order provided,
    # but allows for early termination via ___ in which case the remaining fields
    # are not set in the resulting attrset.
    mkOrderedPartial = mkOrderedThen_ id true;
    mkOrderedPartialThen = f: mkOrderedThen_ f true;

    # Expect a single argument to be embedded within an attrset with the given name.
    mkUnary = fieldName: mkOrdered [fieldName];

    # Accrue arguments into a list until one satisfies isTerminal.
    # includeFinal is a function of (partial list) -> (most recent arg) -> bool
    list = includeFinal: isTerminal: {
      inherit isTerminal;
      initialState = { xs = []; };
      handle = state: x: 
        if !(isTerminal state x) || includeFinal state x
          then { xs = [x] ++ state.xs; }
          else state;
      terminate = state: x: reverseList state.xs;
    };
    isTerminator = _: x: x == ___;
    # mkListThen_ discards the final arg; f is now only a function of the final list.
    mkList_ = includeFinal: isTerminal: Variadic (list includeFinal isTerminal);

    # Accrue arguments into a list until the end-marker is encountered.
    mkListExclusive = mkList_ (_: _: false);
    mkListInclusive = mkList_ (_: _: true);
    mkList = mkListExclusive isTerminator;

    # Accrue until the terminator and then apply f to the list.
    mkListThen_ = f: isTerminal: Variadic.compose f (mkListExclusive isTerminal);
    mkListThen = f: mkListThen_ f isTerminator;

    # Accrue arguments into a list until the given size is met.
    mkListOfLength = n: 
      let spec = list (_: _: true) (state: _: (length state.xs) == n);
      in Variadic (spec // {
        handle = state: x:
          assert assertMsg (x != ___) "mkListOfLength: got ___";
          spec.handle state x;
      });

    # Accrue arguments into a list until the given size is met or the end marker is returned
    mkListOfMaxLength = n:
      let includeFinal = state: x: x != ___;
          isTerminal = state: x: length state.xs == n || x == ___;
      in mkList_ includeFinal isTerminal;

    # Accrue a list of function arguments then compose them
    mkListCompose_ = isTerminal: mkListThen_ (typed.foldl' compose id) isTerminal;
    mkListCompose = mkListCompose_ (_: x: x == ___);

    # Compose a variadic function f with a function g.
    # Takes a predicate to check if the partially applied value f is a function - if so, continues
    # to apply it, otherwise terminates the variadic call.
    # If g is n-ary or variadic, then the final result of f is passed as the first argument of g,
    # so that the composition continues to be variadic.
    #j
    # Intended termination of f can't return a function in general, or this will not be able to detect termination
    # since the Variadic is elided and we don't have access to f.isTerminal
    # Does not handle f returning a functor as its final object for the same reason.
    # TODO: Can handle this by making Variadic return a functor carrying isTerminal?
    # 
    # Two variants are provided - one that terminates only when f emits a non-function non-functor,
    # and one that terminates when f emits precisely a function.
    compose_ = isTerminal: g: fOrX:
      assert assertMsg (isFunction g) "Cannot precompose a non-function (${typeOf g}) in Variadic.compose";
      if isTerminal fOrX then let x = fOrX; in g x
      else let f = fOrX; in a: compose_ isTerminal g (f a);

    # Compose, passing to g whenever a non-partial function is reached. If f
    # returns a functor, then the functor continues being applied until a pure non-functor
    # value is reached.
    compose = compose_ (x: !(builtins.isFunction x || typelib.isFunctor x));

    # Like compose, but if f returns a functor, it is treated as a value.
    composeFunctorsAreAttrs = compose_ (x: !(builtins.isFunction x));
  };

  # Exhaust a function and do something with its first non-function curried output.
  # See compose_ for similar details about handling f returning a functor.
  exhaust_ = fIsFunction: fThen: f: Variadic {
    initialState = { inherit f; };
    handle = state: x: { f = state.f x; };
    isTerminal = nextState: _: !(fIsFunction nextState.f);
    terminate = nextState: _: fThen nextState.f;
  };

  exhaust = exhaust_ isFunction;
  exhaustFunctor = exhaust_ typelib.isFunctionNotFunctor;

  # Convert a list of length n[ x ... y ] to a list
  # [ {index = 0; value = x;} ... {index = n - 1; value = y;} ]
  enumerate = xs: 
    zipListsWith 
      (index: value: { inherit index value; })
      (map toString (range 0 (length xs - 1)))
      xs;

  ### Solo lambdas

  # Convert function args to list of of 
  # i.e. {f} -> [{name = "f"; hasDefault = false;}]
  #      {a ? Int} -> [{name = "a"; hasDefault = true;}]
  #      {a ? Int, b ? String} -> [{name = "a"; hasDefault = true;} {name = "b"; hasDefault = true;}]
  # Does not yet enforce solo-currying.
  getArgs = f: 
    let args_ = builtins.functionArgs f;                                                                                            
        sortedArgs = sortedPos args_;
        args =
          map 
            (pos: 
              assert assertMsg (pos != null) "getArgs: got null arg position (arg must be defined literally in-file)";
              rec {
                name = pos.name;
                hasDefault = args_.${name};
                inherit pos;
              }) 
            sortedArgs;
    in args;

  getPartitionedArgs = f:
    let args = partition (arg: arg.hasDefault) (getArgs f);
    in {
      optional = args.right;
      required = args.wrong;
    };

  # i.e. {f}: ... -> true
  #      {a ? Int} -> false
  #      (x: ...) -> false
  isNamedLambda = f:
    let args = getPartitionedArgs f;
    in size args.optional == 0 && size args.required == 1;

  # i.e. {f}: ... -> false
  #      {a ? Int} -> true
  #      (x: ...) -> false
  isTypedLambda = f:
    let args = getPartitionedArgs f;
    in size args.optional >= 1 && size args.required == 0;

  # i.e. {f}: ... -> false
  #      {a ? Int}: ... -> false
  #      (x: x + 1) -> true
  isRegularLambda = f:
    let args = getPartitionedArgs f;
    in size args.optional == 0 && size args.required == 0;

  # Get default args from a solo lambda with a callback argument
  getOptionalArgDefaultValues = f:
    log.describe "while getting optional arg default values from solo lambda" (
    let args = getPartitionedArgs f;
        requiredArgValues = mergeAttrsList (map (arg: { ${arg.name} = null; }) args.required);
        optionalArgs = keyByName args.optional;
        callbackToRetval = f requiredArgValues;
        allArgValues = callbackToRetval id;
        defaultArgValues = intersectAttrs optionalArgs allArgValues;
    in defaultArgValues);

  # nix eval --impure --expr '(import collective-public/pkgs/collective-utils/functions.nix {})._tests.run'
  _tests = with collective-lib.tests; suite {
    functionArgs = {
      getArgs.opt = expect.noLambdasEq (getArgs testData.f.opt) [
        {name = "b"; hasDefault = false; pos = {__isPos = true; name = "b"; file = "functions.nix"; line = 5; column = 14; __toString = expect.anyLambda;};}
        {name = "a"; hasDefault = true; pos = {__isPos = true; name = "a"; file = "functions.nix"; line = 5; column = 17; __toString = expect.anyLambda;};}
        ];
      getArgs.req = expect.noLambdasEq (getArgs testData.f.req) [
        {name = "b"; hasDefault = true; pos = {__isPos = true; name = "b"; file = "functions.nix"; line = 6; column = 14; __toString = expect.anyLambda;};}
        {name = "a"; hasDefault = true; pos = {__isPos = true; name = "a"; file = "functions.nix"; line = 6; column = 21; __toString = expect.anyLambda;};}
        ];
      getArgs.cb = expect.noLambdasEq (getArgs testData.f.cb) [
        {name = "b"; hasDefault = true; pos = {__isPos = true; name = "b"; file = "functions.nix"; line = 7; column = 13; __toString = expect.anyLambda;};}
        {name = "a"; hasDefault = true; pos = {__isPos = true; name = "a"; file = "functions.nix"; line = 7; column = 20; __toString = expect.anyLambda;};}
        ];
      getOptionalArgDefaultValues =
        expect.eq
          (getOptionalArgDefaultValues testData.f.cb)
          {a = 1; b = 2;};
    };

    ap = {
      functor.partial = expect.isLambda (ap (a: b: a + b) 1);
      functor.full = expect.eq (ap (a: a + 1) 1) 2;
      list.partial = expect.isLambda (ap.list (a: b: c: a + 2 * b + 3 * c) [1 2]);
      list.full = expect.eq (ap.list (a: b: c: a + 2 * b + 3 * c) [1 2 3]) 14;
      list.orderDependent.math = expect.eq (ap.list (a: b: c: a + b * c) [1 2 3]) 7;
    };

    compose =
      let f = a: a + 1;
          g = a: a * 3;
      in {
        fns =
          let gf = compose g f;
          in expect.eq (map gf [0 1 2 3]) [3 6 9 12];
        list =
          let gf = compose [g f];
          in expect.eq (map gf [0 1 2 3]) [3 6 9 12];
      };

    precompose =
      let f = a: a + 1;
          g = a: a * 3;
      in {
        fns =
          let gf = precompose f g;
          in expect.eq (map gf [0 1 2 3]) [3 6 9 12];
        list =
          let gf = precompose [f g];
          in expect.eq (map gf [0 1 2 3]) [3 6 9 12];
      };

    Variadic = {
      default = {
        expr =
          let f = Variadic { isTerminal = _: arg: isAttrs arg && attrValues arg == [123]; };
          in f {x = "y";} {abc = 123;};
        expected = { x = "y"; abc = 123; };
      };

      ordered = {
        noPartial = {
          _0 = expect.eq (Variadic.mkOrdered []) {};
          _1 = expect.eq ((Variadic.mkOrdered ["a"]) 1) {a = 1;};
          _2.full = expect.eq ((Variadic.mkOrdered ["a" "b"]) 1 2) {a = 1; b = 2;};
          _2.partial = expect.isLambda ((Variadic.mkOrdered ["a" "b"]) 1);
          _2.early = expect.error ((Variadic.mkOrdered ["a" "b"]) 1 ___);
        };
        noPartialThen = {
          _0 = expect.eq (Variadic.mkOrderedThen attrValues []) [];
          _1 = expect.eq ((Variadic.mkOrderedThen attrValues ["a"]) 1) [1];
          _2.full = expect.eq ((Variadic.mkOrderedThen attrValues ["a" "b"]) 1 2) [1 2];
          _2.partial = expect.isLambda ((Variadic.mkOrderedThen attrValues ["a" "b"]) 1);
          _2.early = expect.error ((Variadic.mkOrderedThen attrValues ["a" "b"]) 1 ___);
        };
        partial = {
          _0 = expect.eq (Variadic.mkOrderedPartial []) {};
          _1 = expect.eq ((Variadic.mkOrderedPartial ["a"]) 1) {a = 1;};
          _2.full = expect.eq ((Variadic.mkOrderedPartial ["a" "b"]) 1 2) {a = 1; b = 2;};
          _2.partial = expect.isLambda ((Variadic.mkOrderedPartial ["a" "b"]) 1);
          _2.early = expect.eq ((Variadic.mkOrderedPartial ["a" "b"]) 1 ___) {a = 1;};
          _2.veryEarly = expect.eq ((Variadic.mkOrderedPartial ["a" "b"]) ___) {};
        };
        partialThen = {
          _0 = expect.eq (Variadic.mkOrderedPartialThen attrValues []) [];
          _1 = expect.eq ((Variadic.mkOrderedPartialThen attrValues ["a"]) 1) [1];
          _2.full = expect.eq ((Variadic.mkOrderedPartialThen attrValues ["a" "b"]) 1 2) [1 2];
          _2.partial = expect.isLambda ((Variadic.mkOrderedPartialThen attrValues ["a" "b"]) 1);
          _2.early = expect.eq ((Variadic.mkOrderedPartialThen attrValues ["a" "b"]) 1 ___) [1];
          _2.veryEarly = expect.eq ((Variadic.mkOrderedPartialThen attrValues ["a" "b"]) ___) [];
        };
      };

      unary = {
        expr = (Variadic.mkUnary "xxx") "abc";
        expected = { xxx = "abc"; };
      };

      set = {
        default = {
          noArgs = {
            expr = Variadic.mkSet ___;
            expected = {};
          };
          empty = {
            expr = Variadic.mkSet {} ___;
            expected = {};
          };
          merged = {
            expr = Variadic.mkSet {a = 1;} {b = 2;} ___;
            expected = {a = 1; b = 2;};
          };
          preferLater = {
            expr = Variadic.mkSet {a = 1;} {a = 2;} ___;
            expected = {a = 2;};
          };
          from = {
            noArgs = {
              expr = Variadic.mkSetFrom {x = 9;} ___;
              expected = {x = 9;};
            };
            args = {
              expr = Variadic.mkSetFrom {x = 9;} {a = 1;} ___;
              expected = {a = 1; x = 9;};
            };
            overwriteInit = {
              expr = Variadic.mkSetFrom {x = 9;} {x = 1;} ___;
              expected = {x = 1;};
            };
          };
          setThen = {
            expr = sortOn id (Variadic.mkSetThen attrNames {a = 1;} {b = 3;} ___);
            expected = ["a" "b"];
          };
          fromThen = {
            expr = sortOn id (Variadic.mkSetFromThen {x = 9;} attrNames {a = 1;} ___);
            expected = ["a" "x"];
          };
          setOfMinSize = {
            partial = {
              expr = typeOf ((Variadic.mkSetOfMinSize 3) {a = 1;} {b = 2;});
              expected = "lambda";
            };
            full = {
              expr = (Variadic.mkSetOfMinSize 3) {a = 1;} {b = 2;} {c = 3;};
              expected = {a = 1; b = 2; c = 3;};
            };
          };
        };
      };

      list = {
        default = {
          empty = expect.eq (Variadic.mkList ___) [];
          partial = expect.isLambda ((Variadic.mkList) 1 2);
          full = expect.eq ((Variadic.mkList) 1 2 3 ___) [1 2 3];
        };
        ofLength = {
          empty = expect.eq (Variadic.mkListOfLength 0) [];
          partial = expect.isLambda ((Variadic.mkListOfLength 3) 1 2);
          early = expect.error ((Variadic.mkListOfLength 3) 1 2 ___);
          full = expect.eq ((Variadic.mkListOfLength 3) 1 2 3) [1 2 3];
        };
        ofMaxLength = {
          partial = expect.isLambda ((Variadic.mkListOfMaxLength 3) 1 2);
          early = expect.eq ((Variadic.mkListOfMaxLength 3) 1 2 ___) [1 2];
          full = expect.eq ((Variadic.mkListOfMaxLength 3) 1 2 3) [1 2 3];
        };
      };

      listThen = {
        expr = Variadic.mkListThen size 1 1 1 ___;
        expected = 3;
      };

      listCompose = {
        expr =
          let f = Variadic.mkListCompose (x: x + 1) (x: x * 2) ___;
          in f 2;
        expected = 5;
      };

      compose = {
        unaryWithVariadic = {
          expr =
            let f = Variadic.mkOrdered ["a" "b"];
                g = x: x // { c = 123; };
                gf = Variadic.compose g f;
            in gf 1 2;

          expected = {
            a = 1;
            b = 2;
            c = 123;
          };
        };

        variadicWithVariadic = {
          expr =
            let f = Variadic.mkOrdered ["a" "b"];
                g = Variadic { isTerminal = state: _: 3 <= size state; };
                gf = Variadic.compose g f;
            in gf 1 2 {c = 123;};

          expected = {
            a = 1;
            b = 2;
            c = 123;
          };
        };

        orderedWithOrdered = {
          expr =
            let f = Variadic.mkOrdered ["a" "b"];
                g = Variadic.mkOrdered ["c" "d"];
                gf = Variadic.compose g f;
            in gf 1 2 3;
          expected = {
            c = {
              a = 1;
              b = 2;
            };
            d = 3;
          };
        };

        functionWithFunctor = {
          compose = {
            expr =
              let f = a: { __functor = self: b: c: a + b * c; };
                  g = x: 1 + x;
                  gf = Variadic.compose g f;
              in gf 2 3 4;
            expected = 15;
          };

          composeFunctorsAreAttrs = {
            expr =
              let f = a: { __functor = self: b: c: a + b * c; };
                  g = f: 1 + f 3 4;
                  gf = Variadic.composeFunctorsAreAttrs g f;
              in gf 2;
            expected = 15;
          };
        };
      };

      exhaust = {
        value = expect.eq (exhaust (x: x + 1) 6) 7;
        unary = expect.eq (exhaust (x: x + 1) (a: a + 3) 2) 6;
        binary = expect.eq (exhaust (x: x + 1) (a: b: a + b) 1 2) 4;
        trinary = expect.eq (exhaust (x: x + 1) (a: b: c: a + b + c) 1 2 3) 7;
        unexhausted = expect.True (isFunction (exhaust (x: x + 1) (a: b: c: a + b + c) 1 2));
        functor = {
          exhaust = 
            expect.eq
              (exhaust (x: x + 1) { __functor = self: a: b: c: a + b + c; } 1 2 3)
              7;
          exhaustFailsFunctorTerminal = 
            expect.eqOn lib.typeOf
              (exhaust (f: f 1) (a: b: c: { __functor = self: arg: a + b + c + arg; }) 1 2 3)
              expect.anyLambda;
          exhaustFunctorInitial = 
            expect.eq 
              (exhaustFunctor (f: f 1 2 3) { __functor = self: a: b: c: a + b + c; })
              6;
          exhaustFunctorTerminal = 
            expect.eq 
              (exhaustFunctor (f: f 1) (a: b: c: { __functor = self: arg: a + b + c + arg; }) 1 2 3)
              7;
        };
      };
    };

    pipe = {
      simple = expect.eq
        (pipe 123
          (x: x + 1)
          toString
          (s: "${s} is 124")
          ___)
        "124 is 124";
    };

    addEllipsis = 
      let f = {a, b}: a + b;
      in {
        simple = expect.eq (addEllipsis f {a = 1; b = 2; c = 5;}) 3;
      };

    requiredArgs = {
      attrs = expect.eq (requiredArgs ({a, b, c ? 3}: a + b + c)) {a = false; b = false;};
      lambda = expect.eq (requiredArgs (a: a)) {};
    };

    defaultArgs = {
      attrs = expect.eq (defaultArgs ({a, b, c ? 3}: a + b + c)) {c = true;};
      lambda = expect.eq (defaultArgs (a: a)) {};
    };

    missingRequiredArgs = {
      all = expect.eq (missingRequiredArgNames ({a, b, c}: a + b + c) {}) ["a" "b" "c"];
      total = expect.eq (missingRequiredArgNames ({a, b, c ? 3}: a + b + c) {a = 0; b = 1; c = 2;}) [];
      partial = expect.eq (missingRequiredArgNames ({a, b, c ? 3}: a + b + c) {b = 1;}) ["a"];
      functor = expect.eq (missingRequiredArgNames ({__functor = self:
        {a, b, c ? 3}: a + b + c;}) {b = 1;}) ["a"];
      lambda = expect.eq (missingRequiredArgNames (a: a) {a = 1;}) [];
    };

    unitArgs = expect.eq (unitArgs ({a, b, c ? 3}: a + b + c)) {a = {}; b = {}; c = {};};
    nullArgs = expect.eq (nullArgs ({a, b, c ? 3}: a + b + c)) {a = null; b = null; c = null;};
    nullRequiredArgs = expect.eq (nullRequiredArgs ({a, b, c ? 3}: a + b + c)) {a = null; b = null;};
    unitRequiredArgs = expect.eq (unitRequiredArgs ({a, b, c ? 3}: a + b + c)) {a = {}; b = {};};

    tryApply = skip {
      simple = {
        lambda = expect.eq (tryApply (a: a) 1) 1;
        functor.lambda = expect.eq (tryApply {__functor = self: a: a;} 1) 1;
        functor.attrs = expect.eq (tryApply {
          __functor = self: 
          {a}: a;
        } {a = 1;}) 1;
        attrs = expect.eq (tryApply ({a}: a) {a = 1;}) 1;
        defaultAttrs = expect.eq (tryApply ({a ? 1}: a) {}) 1;
        providedDefaultAttrs = expect.eq (tryApply ({a ? 1}: a) {a = 2;}) 2;
      };
      missing = {
        empty = expect.error (tryApply ({a}: a) {});
        unused = expect.error (tryApply ({a, b}: a) {a = 1;});
        used = expect.error (tryApply ({a, b}: a) {b = 1;});
        mixed = expect.error (tryApply ({a, b ? 2}: a) {b = 1;});
        functor = expect.error (tryApply {__functor = self: {a}: a;} {});
      };
      nonAttrset = expect.error (tryApply ({a}: a) 1);
    };

    thunk = {
      manual = expect.eq ((thunk 123) {}) 123;
      resolve = expect.eq (resolve (thunk 123)) 123;
      recursive =
        let mkX = i: { inherit i; next = thunk (mkX (i + 1)); };
        in {
          mkXPrints = expect.printEq (mkX 0) { i = 0; next = expect.anyLambda; };
          mkX_0 = expect.eq (mkX 0).i 0;
          mkX_0_next = expect.eq (resolve (mkX 0).next).i 1;
          mkX_0_next_next = expect.eq (resolve (resolve (mkX 0).next).next).i 2;
        };
    };
    Thunk = {
      isThunkLambda = expect.eq (isThunkSet (_: 123)) false;
      isThunkSet = expect.eq (isThunkSet {}) false;
      isThunkThunk = expect.eq (isThunkSet (Thunk 123)) true;
      mk = expect.eq (resolve (Thunk 123)) 123;
      mk2 = expect.eq (resolve (resolve (Thunk (Thunk 123)))) 123;
      mk5 = expect.eq
        (resolve (resolve (resolve (resolve (resolve
          (Thunk (Thunk (Thunk (Thunk (Thunk 123)))))
        )))))
        123;
      print1 = expect.eq (log.print (Thunk 123)) "Thunk >-> int";
      printNamed1 = expect.eq (log.print (NamedThunk "name" 123)) "Thunk >-[name]-> int";
      print5 = expect.eq
        (log.print
          (Thunk (Thunk (Thunk (Thunk (Thunk 123)))))
        )
        "Thunk >-> set";
      show5 = expect.eq
        (log.show
          (Thunk (Thunk (Thunk (Thunk (Thunk 123)))))
        )
        "Thunk >-> set";
      do = expect.eq (thunkDo (Thunk 123) (x: x+1)) 124;
      fmap = expect.eq (resolve (thunkFmap (Thunk 123) (x: x+1))) 124;
      fmapRetainsThunk = expect.True (isThunkSet (thunkFmap (Thunk 123) (x: x+1)));
    };

    fjoin = {
      fUnary =
        let f = a: a * 3;
        in {
          gUnary =
            let g = a: a + 2;
                gf = fjoin (gr: fr: {inherit gr fr;}) g f;
            in {
              expr = gf 2 10;
              expected = { fr = 6; gr = 12;};
            };
          gBinary =
            let g = a: b: a + b;
                gf = fjoin (gr: fr: {inherit gr fr;}) g f;
            in {
              expr = gf 2 10 1;
              expected = { fr = 6; gr = 11;};
            };
        };

      fBinary =
        let f = a: b: a * b;
        in {
          gUnary =
            let g = a: a + 2;
                gf = fjoin (gr: fr: {inherit gr fr;}) g f;
            in {
              expr = gf 3 2 10;
              expected = { fr = 6; gr = 12;};
            };
          gBinary =
            let g = a: b: a + b;
                gf = fjoin (gr: fr: {inherit gr fr;}) g f;
            in {
              expr = gf 3 2 10 1;
              expected = { fr = 6; gr = 11;};
            };
        };

      orderedWithOrderedMerge = {
        distinct = {
          expr =
            let f = Variadic.mkOrdered ["a" "b"];
                g = Variadic.mkOrdered ["c" "d"];
                gf = fjoin mergeAttrs g f;
            in gf 1 2 3 4;
          expected = { a = 1; b = 2; c = 3; d = 4; };
        };
        overlapping = {
          expr =
            let f = Variadic.mkOrdered ["a" "b"];
                g = Variadic.mkOrdered ["b" "c"];
                gf = fjoin mergeAttrs g f;
            in gf 1 2 3 4;
          expected = { a = 1; b = 2; c = 4; };
        };
        overlappingFlip = {
          expr =
            let f = Variadic.mkOrdered ["a" "b"];
                g = Variadic.mkOrdered ["b" "c"];
                gf = fjoin (flip mergeAttrs) g f;
            in gf 1 2 3 4;
          expected = { a = 1; b = 3; c = 4; };
        };
      };

      pointerEqual = {
        simple = expect.eq (pointerEqual 123 123) true;
        lambda = let f = x: x; in expect.eq (pointerEqual f f) true;
        lambdaNonEqual = let f = x: x; g = x: x + 1; in expect.eq (pointerEqual f g) false;
        set = expect.eq (pointerEqual {a = 1; b = 2;} {a = 1; b = 2;}) true;
        list = expect.eq (pointerEqual [1 2 3] [1 2 3]) true;
        setWithLambda = expect.eq (pointerEqual {a = 1; b = 2;} {a = 1; b = (x: x);}) false;
        listWithLambda = expect.eq (pointerEqual [1 2 3] [1 2 (x: x)]) false;
      };

    };
  };

}
