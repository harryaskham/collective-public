{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.errors;
with cutils.functions;
with cutils.lists;
with cutils.strings;
with cutils.tests;

rec {
  # Make a polymorphic function from the given type-to-value attrs.
  # Accepts a given default function to apply to any unspecified types.
  # Also dispatches a function polymorphically if values are functions from the type of the argument.
  dispatchDefOn = getType: defaultF: dict: x:
    let f = dict.${getType x} or defaultF; in f x;
  dispatchDef = dispatchDefOn typeOf;

  # Make a polymorphic function from the given type-to-value attrs
  # Also dispatches a function polymorphically if values are functions from the type of the argument.
  dispatchOn = getType: dict: x:
    let defaultF = throw ''
      Unsupported type ${getType x} in polymorphic dispatch.
      Expected: ${joinSep ", " (attrNames dict)}
    '';
    in dispatchDefOn getType defaultF dict x;
  dispatch = dispatchOn typeOf;

  # Make a polymorphic function from the given type-to-value attrs
  # Gets the type of the argument via a function f e.g.
  dispatchElem = dict: xs:
    if size xs == 0 then throw "Cannot dispatch on an empty ${typeOf xs}"
    else dispatch {
      list = dispatchOn (xs: typeOf (head xs)) dict;
      set = dispatchOn (xs: typeOf (head (attrValues xs))) dict;
    } xs;

  # Polymorphic object size
  size = dispatch {
    list = length;
    set = compose length attrValues;
    string = stringLength;
    path = compose stringLength toString;
  };

  # Polymorphic emptiness check
  empty = dispatch {
    list = l: l == [];
    set = s: s == {};
    string = s: s == "";
  };
  nonEmpty = x: !(empty x);

  # Polymorphic map.
  fmap = f: dispatch {
    list = map f;
    set = mapAttrs (_: f);
    lambda = compose f;
  };

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

  # nix eval --impure --expr '(import collective-public/pkgs/collective-utils/functions.nix {})._tests.run'
  _tests =
    with cutils.tests;
    suite {
      dispatch = {
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
          null_0 = expect.error (size null);
        };

        deepMap = {
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

      };
    };

}
