{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.errors;

# Misc functional utilities
let
  log = cutils.log;
in rec {

  # Compose two functions left-to-right
  compose = g: f: a: g (f a);

  # Apply a function to a value
  apply = f: a: f a;

  # Return a value iff a condition is met, otherwise return null
  when = cond: a: if cond then a else null;

  # Return a value iff a condition is not met, otherwise return null
  unless = cond: when (!cond);

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
      handle = state: x: {
        fieldOrder = tail state.fieldOrder;
        args = state.args // { ${head state.fieldOrder} = x; };
      };
      terminate = state: _: state.args;
    };
    mkOrdered = fieldOrder: mk (ordered fieldOrder);

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

    # Compose a variadic function with a function that accepts a single argument.
    # The variadic can't return a function or it will not be able to detect termination.
    compose = g: f:
      if isFunction f then a: Variadic.compose g (f a)
      else g f;

  };

  # Make a polymorphic function from the given type-to-value attrs
  # Also dispatches a function polymorphically if values are functions from the type of the argument.
  dispatch = dict: x:
    let
      f = dict.${typeOf x}
        or (throw ''
              Unsupported type ${typeOf x} in polymorphic dispatch.
              Expected: ${joinSep ", " (attrNames dict)}
            '');
    in f x;

  # Polymorphic object size
  size = dispatch {
    list = length;
    set = compose length attrValues;
    string = stringLength;
    null = const 0;
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
          null_0 = { expr = size null; expected = 0; };
        };

        Variadic = {
          default = {
            expr =
              let f = Variadic.mk { isTerminal = _: arg: attrValues arg == [123]; };
              in f {x = "y";} {abc = 123;};
            expected = { x = "y"; abc = 123; };
          };

          ordered = {
            expr = (Variadic.mkOrdered ["a" "b"]) 1 2;
            expected = { a = 1; b = 2; };
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
          };

        };
      };
    };

}
