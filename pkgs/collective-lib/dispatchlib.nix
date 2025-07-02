{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.collections;
with collective-lib.errors;
with collective-lib.functions;
with collective-lib.lists;
with collective-lib.strings;
with collective-lib.tests;

let log = collective-lib.log;
in rec {
  dispatch = rec {
    # Default behaviour is to dispatch on the simple Nix builtin type of the argument.
    # Throws an error if the type of x is not in the dict.
    # See dispatch.on for further details.
    #
    # e.g. map (dispatch { int = x: x + 1; float = x: x - 1.0; } [3 5.0]) == [4 4.0]
    #      dispatch { int = x: x + 1; } "not int" => throws error
    __functor = self: self.on lib.typeOf;

    # Make a polymorphic function from the given type-to-value attrs
    # Takes a function getType that returns the dispatch type of the argument,
    # a function to call on a value if it does not match any of the types in the dict,
    # a dict from the return of getType to the function to call,
    # and an argument x to dispatch on.
    #
    # Examples:
    # 
    # let 
    #   f = dispatch.def (x: { unknown = x; }) {
    #     string = x: x + " world";
    #     int = x: x + 1;
    #   };
    # in 
    #   map f [ "hello" 1 2.0 ];
    # => [ "hello world" 2 { unknown = 2.0; } ]
    #
    # let 
    #   f = dispatch.def.on (x: x.name) (x: { unknown = x; }) {
    #     a = attrNames;
    #     b = attrValues;
    #   };
    # in 
    #   map f [ { name = "a"; value = 1; } 
    #           { name = "b"; value = 2; } ];
    #           { name = "c"; value = 3; } ];
    # => [ [ "name" "value" ]
    #      [ "b" 2 ]
    #      { unknown = { name = "c"; value = 3; } } ]
    def = {
      __functor = self: self.on lib.typeOf;
      on = getType: defaultF: dict: x:
        let f = dict.${getType x} or defaultF; in f x;
    };

    # Make a polymorphic function from the given type-to-value attrs
    # Takes a function getType that returns the dispatch type of the argument,
    # a dict from the return of getType to the function to call,
    # and an argument x to dispatch on.
    #
    # Throws an error if the type of x is not in the dict.
    #
    # Examples:
    # 
    # let f = dispatch.on (x: x.name or "") { a = x: x.value; };
    # in f { name = "a"; value = 1; } == 1;
    #
    # let f = dispatch.on (x: x.name or "") { a = x: x.value; };
    # in f { name = "b"; value = 1; } => throws error
    on = getType: dict: x:
      let defaultF = throw ''
        Unsupported type ${getType x} in polymorphic dispatch.
        Expected: ${joinSep ", " (attrNames dict)}
        Got ${getType x} of value: ${log.print x}
      '';
      in dispatch.def.on getType defaultF dict x;

    # Make a polymorphic function from the given type-to-value attrs
    # Assumes the collection provided is homogeneous in its value types.
    # The given dispatch dict contains functions that operate over the whole
    # collection, not the individual elements.
    #
    # Examples:
    #
    # let f = dispatch.elem {
    #   int = fmap (x: x + 1);
    #   float = fmap (x: x - 1.0);
    # };
    # in f [1 2 3] == [2 3 4];
    # in f [1.0 2.0 3.0] == [0.0 1.0 2.0];
    # in f { a = 1; b = 2; c = 3; } == { a = 2; b = 3; c = 4; };
    # in f { a = 1.0; b = 2.0; c = 3.0; } == { a = 0.0; b = 1.0; c = 2.0; };
    # in f { a = "no"; } => throws error
    # in f [ "no" ] => throws error
    # in f "no" => throws error
    elem = {
      __functor = self: self.on lib.typeOf;
      on = getElemType: dict: 
        dispatch.on (compose getElemType elems.head) dict;
    };
  };

  ### Polymorphic functions

  # Polymorphic map.
  fmap = f: dispatch {
    list = map f;
    set = mapAttrs (_: f);
    lambda = compose f;
  };

  # Map a function over the leaves of an arbitrary value, applying it recursively to all set and list values.
  deepMap = f: dispatch.def f {
    list = map (deepMap f);
    set = mapAttrs (_: (deepMap f));
  };

  # recursiveMapAttrs that also allows for mapping over lists.
  # As with concatMapAttrs, f must return a set.
  # f is applied to all leaves, and recursively to all containers after application at the leaves.
  deepConcatMap = f: dispatch.def id {
    list = map (deepConcatMap f);
    set = concatMapAttrs (k: v: f k (deepConcatMap f v));
  };

  # As deepConcatMap but only recurses into values that pass cond.
  # f is only applied to leaves that pass cond.
  deepConcatMapCond = cond: f:
    dispatch.def id {
      list = map (deepConcatMapCond cond f);
      set = concatMapAttrs (k: v: if cond k v then { ${k} = deepConcatMapCond cond f v; } else f k v);
    };

  # As deepConcatMap but only recurses into values that pass cond.
  # f is applied anywhere cond matchesto all leaves and also to all containers after application at the leaves
  deepConcatMapCondAll = cond: f:
    dispatch.def id {
      list = imap0 (i: v: if cond i v then f i (deepConcatMapCondAll cond f v) else f i v);
      set = concatMapAttrs (k: v: if cond k v then f k (deepConcatMapCondAll cond f v) else f k v);
    };

  deepMapNames = f: dispatch.def id {
    list = map (deepMapNames f);
    set = deepConcatMap (k: v: { ${f k} = v; });
  };

  deepMapNamesCond = cond: f: dispatch.def id {
    list = map (deepMapNamesCond cond f);
    set = concatMapAttrs (k: v:
      if cond k 
      then { ${f k} = deepMapNamesCond cond f v; } 
      else { ${k} = v;});
  };

  deepFilter = f: dispatch.def id {
    list = xs: filter f (map (deepFilter f) xs);
    set = xs: filterAttrs (_: f) (mapAttrs (_: deepFilter f) xs);
  };

  # Map a function over the depth and leaves of an arbitrary value, applying it recursively to all set and list values.
  deepMapWith = f:
    let
      go = depth: dispatch.def (f depth) {
        list = map (go (depth + 1));
        set = mapAttrs (_: go (depth + 1));
      };
    in
      go 0;

  # nix eval --impure --expr '(import collective-public/pkgs/collective-utils/functions.nix {})._tests.run'
  _tests =
    with collective-lib.tests;
    suite {
      deepMap = {
        deepMap = {
          expr = deepMap (x: x + 1) { a = 1; b = [2 3]; c = { d = 4; }; };
          expected = { a = 2; b = [3 4]; c = { d = 5; }; };
        };
        deepMapDef = {
          expr = deepMap (dispatch.def id {
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

        deepConcatMap.sets = {
          expr = 
            deepConcatMap 
            (k: v:
              if k == "no" then { yes = v; } 
              else if lib.typeOf v == "int" then { ${k} = v + 1; } 
              else if lib.typeOf v == "string" then { ${k} = v; }
              else { ${k} = v; })
            { a = 1; no = 2; b = { no = { c = 3; no = 4; noChange = "not int"; }; }; };
          expected = 
            { a = 2; yes = 2; b = { yes = { c = 4; yes = 4; noChange = "not int"; }; }; };
        };

        deepConcatMapCond.sets = {
          expr = 
            deepConcatMapCond 
            (k: v: collection v && !(v ? "noChange") && k != "no")
            (k: v:
              if k == "no" then { yes = v; } 
              else if lib.typeOf v == "int" then { ${k} = v + 1; } 
              else if lib.typeOf v == "string" then { ${k} = v; }
              else { ${k} = v; })
            { 
              a = 1;
              no = 2;
              b = { 
                no = { 
                  c = 3;
                  no = 4;
                  noChange = true;
                };
              };
            };
          expected = 
            { 
              a = 2; 
              yes = 2; 
              b = { 
                yes = { 
                  c = 3; 
                  no = 4; 
                  noChange = true;
                }; 
              };
            };
        };

        deepConcatMapCond.setsLists = {
          expr = 
            deepConcatMapCond 
            (k: _: dispatch { int = i: i < 2; string = k: !(elem k ["c" "no"]); } k)
            (k: v: { "_${toString k}" = v; })
            { a = [0 1 2 3];
              b = [{no = "no";} {no = "no";} {no = "no";}];
              c = [0 1 2 3];
            };
          expected = 
            { a = [ 0 1 2 3]; 
              b = [{_no = "no";} {_no = "no";} {_no = "no";} ]; 
              _c = [0 1 2 3];
            };
        };

        deepConcatMapCondAll.setsLists = {
          expr = 
            deepConcatMapCondAll
            (k: _: dispatch { int = i: i < 2; string = k: !(elem k ["d" "no"]); } k)
            (k: v: { "_${toString k}" = v; })
            { a = [0 1 2 3];
              b = [{no = "no";} {no = "no";} {no = "no";}];
              c = "c";
              d = [0 1 2];
            };
          expected = 
            { _a = [{_0 = 0;} {_1 = 1;} {_2 = 2;} {_3 = 3;}];
              _b = [{_0 = {_no = "no";};} {_1 = {_no = "no";};} {_2 = {no = "no";};}];
              _c = "c";
              _d = [0 1 2];
            };
        };
      };

      deepMapNames =
        expect.eq
          (deepMapNames (k: "_${k}") {a = { b = [ { c = 1; } { d = 2; } ]; }; })
          { _a = { _b = [ { _c = 1; } { _d = 2; } ]; }; };

      deepMapNamesCond =
        expect.eq
          (deepMapNamesCond (k: k != "c") (k: "_${k}") {a = { b = [ { c = {d = 1;}; } { e = 2; } ]; }; })
          { _a = { _b = [ { c = {d = 1;}; } { _e = 2; } ]; }; };

    };

}
