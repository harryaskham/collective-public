{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.errors;
with cutils.lists;
with cutils.tests;

# Misc functional utilities
let
  log = cutils.log;
in rec {

  # Compose two functions left-to-right
  compose = g: f: a: g (f a);

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

  # Compose two functions left-to-right and merge their outputs.
  # For example:
  # f = sequentialWith mergeAttrs (b: c: {inherit b c;}) (a: b: {inherit a b;});
  # f "a" "b" "c" "d" = { a = "a"; b = "b"; c = "c"; d = "d"; }
  fjoin = mergeFn: g: f:
    let g_ = fx: Variadic.compose (gx: mergeFn gx fx) g;
    in Variadic.compose g_ f;

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
                (checkPredMsgs (spec.predMsgs or []) { inherit prevState nextState arg; })
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

      in f spec.initialState;

    # Build a variadic function that accepts partial attrsets until
    # exactly the given names are present.
    argNames = names: {
      isTerminal = state: _: attrNames state == names;
      check = _: x: isAttrs x && all (name: elem name names) (attrNames x);
    };
    mkArgNames = names: mk (argNames names);

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
           args = state.args // { ${ht.head} = x; };
         };
      terminate = state: _: state.args;
    };
    mkOrdered = fieldOrder: if fieldOrder == [] then {} else mk (ordered fieldOrder);

    # Expect a single argument to be embedded within an attrset with the given name.
    unary = fieldName: ordered [fieldName];
    mkUnary = fieldName: mk (unary fieldName);

    # Variadic end-marker for otherwise-ambiguous termination
    end = { __end = true; };

    # Accrue arguments into a list until one satisfies isTerminal.
    list_ = isTerminal: {
      inherit isTerminal;
      initialState = { xs = []; };
      handle = state: x: 
        if isTerminal state x
        then state
        else { xs = [x] ++ state.xs; };
      terminate = state: _: reverseList state.xs;
    };
    mkList_ = isTerminal: mk (list_ isTerminal);

    # Accrue arguments into a list until the end-marker is encountered.
    mkList = mkList_ (_: x: x == end);

    # Accrue arguments into a list until the given size is met
    mkListOfLength = l: mkList_ (state: _: (length state.xs) == l);

    # Compose a variadic function f with a function g.
    # The variadic can't return terminate with a function or this will not be able to detect termination
    # since the Variadic is elided and we don't have g.isTerminal
    # If g is n-ary or variadic, then the final result of f is passed as the first argument of g.
    compose = g: f:
      if (!isFunction g) then throw "Cannot precompose a non-function (${typeOf g}) in Variadic.compose"
      else if isFunction f then a: Variadic.compose g (f a)
      else g f;
  };

  # Shorthand for variadic end marker
  ___ = Variadic.end;

  # Make a polymorphic function from the given type-to-value attrs.
  # Accepts a given default function to apply to any unspecified types.
  # Also dispatches a function polymorphically if values are functions from the type of the argument.
  dispatchDef = defaultF: dict: x:
    let f = dict.${typeOf x} or defaultF; in f x;

  # Make a polymorphic function from the given type-to-value attrs
  # Also dispatches a function polymorphically if values are functions from the type of the argument.
  dispatch = dict: x:
    let defaultF = throw ''
      Unsupported type ${typeOf x} in polymorphic dispatch.
      Expected: ${joinSep ", " (attrNames dict)}
    '';
    in dispatchDef defaultF dict x;

  # Map a function over the leaves of an arbitrary value, applying it recursively to all set and list values.
  deepMap = f: dispatchDef f {
    list = map (deepMap f);
    set = mapAttrs (_: (deepMap f));
  };

  # Map a function over the depth and leaves of an arbitrary value, applying it recursively to all set and list values.
  deepMapWith = f:
    let
      go = depth: dispatchDef (f depth) {
        list = map (go (depth + 1));
        set = mapAttrs (_: go (depth + 1));
      };
    in
      go 0;

  # Polymorphic object size
  size = dispatch {
    list = length;
    set = compose length attrValues;
    string = stringLength;
    path = compose stringLength toString;
  };

  # Convert a list of length n[ x ... y ] to a list
  # [ {index = 0; value = x;} ... {index = n - 1; value = y;} ]
  enumerate = xs: 
    zipListsWith 
      (index: value: { inherit index value; })
      (map toString (range 0 (length xs - 1)))
      xs;

  # nix eval --impure --expr '(import collective-public/pkgs/collective-utils/functions.nix {})._tests.run'
  _tests =
    cutils.tests.suite {
      functions = {
        compose = {
          expr =
            let f = a: a + 1;
                g = a: a * 3;
                gf = compose g f;
            in map gf [0 1 2 3];
          expected = [3 6 9 12];
        };

        size = {
          list_0 = { expr = size []; expected = 0; };
          list_1 = { expr = size [1]; expected = 1; };
          list_2 = { expr = size [1 2]; expected = 2; };
          set_0 = { expr = size {}; expected = 0; };
          set_1 = { expr = size { a = 1; }; expected = 1; };
          set_2 = { expr = size { a = 1; b = 2; }; expected = 2; };
          string_0 = { expr = size ""; expected = 0; };
          string_1 = { expr = size "a"; expected = 1; };
          string_2 = { expr = size "ab"; expected = 2; };
          null_0 = { expr = size null; expected = expect.error; };
        };

        dispatch = {
          deepMap = {
            expr = deepMap (x: x + 1) { a = 1; b = [2 3]; c = { d = 4; }; };
            expected = { a = 2; b = [3 4]; c = { d = 5; }; };
          };
          deepMapDef = {
            expr = deepMap (dispatchDef id {
              string = x: "hello ${x}";
              int = x: x + 1;
              float = x: x + 10.0;
              lambda = f: f 3;
            }) { a = 1; b = [2.0 "world"]; c = { d = a: a * -1; }; };
            expected = { a = 2; b = [12.0 "hello world"]; c = { d = -3; }; };
          };
          deepMapWith = {
            shallow = {
              expr = deepMapWith (depth: x: "${x} at ${toString depth}") "value";
              expected = "value at 0";
            };
            deep = {
              expr = deepMapWith (depth: x: "${toString x} at ${toString depth}") {
                a = 1;
                b = [2 3];
                c = { d = { e = [ "c" "d" "e" ];
                            f = "f";
                          };
                    };
                g = { h = "g.h"; };
              };
              expected = {
                a = "1 at 1";
                b = ["2 at 2" "3 at 2"];
                c = { d = { e = [ "c at 4" "d at 4" "e at 4" ];
                            f = "f at 3";
                          };
                    };
                g = { h = "g.h at 2"; };
              };
            };
          };
        };

        Variadic = {
          default = {
            expr =
              let f = Variadic.mk { isTerminal = _: arg: attrValues arg == [123]; };
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

          list = {
            expr = Variadic.mkList 1 2 3 Variadic.end;
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
                    g = Variadic.mk { isTerminal = state: _: size state > 2; };
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
    };

}
