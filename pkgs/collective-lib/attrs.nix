{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;

rec {
  # Convert a list of attrs single attrset using a function of the attrs to compute the key
  keyByF = f: xs: mergeAttrsList (map (x: { ${f x} = x; }) xs);

  # Convert a list of attrs with "name" attribute to a single attrset using this as a key
  keyByName = keyByF (x: x.name);

  # Convert a list of strings to an attrset from self to self.
  selfAttrs = keyByF id;

  # Flatten an attribute with params
  # - f: a function from path and value to key.
  # - stop (optional): a predicate from path and value to true iff we should not traverse in.
  flattenWith = params:
    let go = path:
          concatMapAttrs
            (k: v:
              let path' = path ++ [k];
              in if isAttrs v && !(params ? stop && params.stop path' k v)
                 then go path' v
                 else {${params.f path' k v} = v;});
    in go [];

  # Flatten an attribute set separating keys in the path with the given separator.
  flattenSep = sep: flattenWith {
    f = path: _: _: joinSep sep path;
  };

  # Flatten tests with __ separator and avoiding expr/expected.
  flattenTests = flattenWith {
    f = path: _: _: "test-${joinSep "__" path}";
    stop = _: _: v: isAttrs v && v ? expr && v ? expected;
  };

  # Swap an attrset keys and values.
  swap = concatMapAttrs (k: v: { ${v} = k; });

  _tests =
    cutils.tests.suite {
      attrs = {
        flatten = {
          sep = {
            expr = flattenSep "-" {
              a = {
                b = {
                  c = 123;
                  d = {
                    e = 456;
                  };
                };
              };
            };
            expected = {
              a-b-c = 123;
              a-b-d-e = 456;
            };
          };
        };
      };
    };
}
