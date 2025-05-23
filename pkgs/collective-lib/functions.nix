{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;

# Misc functional utilities
rec {

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
    defaults = {
      initialState = {};
      handle = mergeAttrs;
      terminate = _: id;
      check = _: _: true;
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
        f = prevState: x:
          let 
            nextState = spec.handle prevState x;
          in 
            if (!spec.check nextState x) 
              then throw "Invalid argument in Variadic.mk: ${log.print x}"
            else if (spec.isTerminal nextState)
              then spec.terminate nextState
            else f nextState;

      in f spec.initialState;

    # Build a variadic function that accepts partial attrsets until
    # exactly the given names are present.
    Tags = names: mk {
      isTerminal = state: _: attrNames state == names;
      check = _: x: isAttrs x && all (name: elem name names) x;
    };
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
      };
    };

}
