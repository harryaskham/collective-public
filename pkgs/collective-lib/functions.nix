{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.dispatch;
with cutils.errors;
with cutils.lists;
with cutils.strings;
with cutils.tests;

# Misc functional utilities
let
  log = cutils.log;
  errors = cutils.errors;
  typelib = cutils.typelib;
in rec {

  # Compose two functions left-to-right
  compose = g: f: a: g (f a);
  composeMany = foldl' compose id;

  # Apply a function to a value
  ap = f: a: f a;
  apply = f: a: f a;

  # Apply a function to a value with the arguments flipped.
  flap = a: f: ap f a;

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

  # Create a composition pipe
  # pipe x doA doB ___;
  pipe = x:
    Variadic.mkListThen
      (fs: let f = foldl1 compose (reverseList fs); in f x);

  # Make a thunk out of a value
  thunk = x: _: x;

  # Resolve a thunk.
  resolve = x:
    if (x ? __resolve) then
      x.__resolve x
    else if isFunction x then
      x (throw ''Resolved lambda-thunk made use of its thunk-argument.'')
    else if isThunkSet x then
      x.__get {}
    else throw ''resolve: Invalid argument type: ${typeOf x}: ${log.print x}'';

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
      # Function from {prevState, nextState, arg} to list of { cond, msg }
      checks = _: null;
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
    ordered = fieldOrder: {
      initialState = {
        inherit fieldOrder;
        args = {};
      };
      isTerminal = state: _: state.fieldOrder == [];
      handle = state: x:
        let ht = maybeSnoc state.fieldOrder;
         in if ht == null then state
         else state // {
           fieldOrder = ht.tail;
           args =
             assert assertMsg (isString ht.head) ''
               Variadic.mkOrdered: expected a string field name, got ${typeOf ht.head}: ${log.print ht.head}
               fieldOrder: ${log.print state.fieldOrder}
             '';
             state.args // { ${ht.head} = x; };
         };
      terminate = state: _: state.args;
    };
    mkOrdered = fieldOrder: if fieldOrder == [] then {} else Variadic (ordered fieldOrder);

    # Expect a single argument to be embedded within an attrset with the given name.
    unary = fieldName: ordered [fieldName];
    mkUnary = fieldName: Variadic (unary fieldName);

    # Accrue arguments into a list until one satisfies isTerminal.
    # Run f on the final list.
    listThen_ = f: isTerminal: {
      inherit isTerminal;
      initialState = { xs = []; };
      handle = state: x: 
        if isTerminal state x
        then state
        else { xs = [x] ++ state.xs; };
      terminate = state: x: f (reverseList state.xs) x;
    };
    mkListThenWith_ = f: isTerminal: Variadic (listThen_ f isTerminal);
    mkListThen_ = f: isTerminal: Variadic (listThen_ (xs: _: f xs) isTerminal);
    mkListThen = f: mkListThen_ f (_: x: x == ___);

    # Accrue arguments into a list until the end-marker is encountered.
    mkList = mkListThen_ id (_: x: x == ___);

    # Accrue arguments into a list until the given size is met
    mkListOfLength = l: mkListThen_ id (state: _: (length state.xs) == l);

    # Accrue a list of function arguments then compose them
    mkListCompose_ = isTerminal: mkListThen_ (foldl' compose id) isTerminal;
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
    compose_ = fIsFunction: g: f:
      if (!isFunction g) then throw "Cannot precompose a non-function (${typeOf g}) in Variadic.compose"
      else if fIsFunction f then a: Variadic.compose_ fIsFunction g (f a)
      else g f;

    compose = compose_ isFunction;
    composeFunctor = compose_ typelib.isFunctionNotFunctor;
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

  # nix eval --impure --expr '(import collective-public/pkgs/collective-utils/functions.nix {})._tests.run'
  _tests = with cutils.tests; suite {
    compose = {
      expr =
        let f = a: a + 1;
            g = a: a * 3;
            gf = compose g f;
        in map gf [0 1 2 3];
      expected = [3 6 9 12];
    };

    Variadic = {
      default = {
        expr =
          let f = Variadic { isTerminal = _: arg: isAttrs arg && attrValues arg == [123]; };
          in f {x = "y";} {abc = 123;};
        expected = { x = "y"; abc = 123; };
      };

      ordered = {
        _0 = {
          expr = Variadic.mkOrdered [];
          expected = {};
        };
        _1 = {
          expr = (Variadic.mkOrdered ["a"] ) 1;
          expected = { a = 1; };
        };
        _2 = {
          expr = (Variadic.mkOrdered ["a" "b"]) 1 2;
          expected = { a = 1; b = 2; };
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
        expr = Variadic.mkList 1 2 3 ___;
        expected = [ 1 2 3 ];
      };

      listOfLength = {
        partial = {
          expr = typeOf ((Variadic.mkListOfLength 3) 1 2);
          expected = "lambda";
        };
        full = {
          expr = (Variadic.mkListOfLength 3) 1 2 3;
          expected = [1 2 3];
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
                g = Variadic { isTerminal = state: _: size state > 2; };
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

          composeFunctor = {
            expr =
              let f = a: { __functor = self: b: c: a + b * c; };
                  g = f: 1 + f 3 4;
                  gf = Variadic.composeFunctor g f;
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

      pipe = {
        simple = expect.eq
          (pipe 123
            (x: x + 1)
            toString
            (s: "${s} is 124")
            ___)
          "124 is 124";
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

      };

    };
  };

}
