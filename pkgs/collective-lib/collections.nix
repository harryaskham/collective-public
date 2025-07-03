{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

# TODO: Move other polymorphic collection functions here e.g. size.

with lib;
with collective-lib.attrsets;
with collective-lib.dispatchlib;
with collective-lib.errors;
with collective-lib.functions;
with collective-lib.lists;
with collective-lib.strings;
with collective-lib.tests;

let log = collective-lib.log;
in rec {
  # Identify values that have elements.
  # Paths are collections, treated as normalised lists of components.
  collection = {
    __functor = self: dispatch.def (const false) {
      list = const true;
      set = const true;
      string = const true;
      path = const true;
    };
    check = x: assert assertMsg (collection x) "collection.check: Got non-collection ${typeOf x}"; x;
  };

  # Get the elements of a collection.
  elems = {
    __functor = self: dispatch {
      list = id;
      set = attrValues;
      string = stringToCharacters;
      path = p: 
        let go = pSplit: [pSplit.root] ++ (lib.path.splitComponents pSplit.rest);
        in go (lib.path.splitRoot p);
    };
    head = xs: head (elems (nonEmpty.check xs));
    tail = xs: tail (elems (nonEmpty.check xs));
  };

  # Polymorphic collection size
  size = dispatch {
    list = length;
    set = compose length attrValues;
    string = stringLength;
    path = compose length lib.path.subpath.components;
  };

  # Polymorphic collection emptiness check
  empty = {
    __functor = self: dispatch {
      list = l: l == [];
      set = s: s == {};
      string = s: s == "";
      path = s: s == [];
    };
    check = x_: 
      let x = collection.check x; in
      assert assertMsg (empty x) "empty.check: Got non-empty ${typeOf x}"; 
      x;
  };

  # Polymorphic collection non-emptiness check
  nonEmpty = {
    __functor = self: x: !(empty x);
    check = x_: 
      let x = collection.check x_; in
      assert assertMsg (nonEmpty x) "nonEmpty.check: Got empty ${typeOf x}"; 
      x;
  };

  # Prepend a collection to another collection.
  # The type of the second argument dictates the type of the result.
  # For 'prepend list list':
  #   the first list is prepended to the front of the second.
  #   does not check for solos, which are handled correctly by regular prepend.
  # For 'prepend set set':
  #   the sets are merged, preferring items in the second collection.
  # For 'prepend list set':
  #   list is treated as solos, merged with set, to return a set.
  #   fails if list is not a list of solos.
  # For 'prepend set list':
  #   set is converted to solos, prepended to list, to return a list of solos.
  #   fails if list is not a list of solos.
  prepend = dispatch {
    list =
      xs: dispatch {
        list = ys: xs ++ ys;
        set = ys: (mergeSolos xs) // ys;
      };
    set = xs: dispatch {
      list = ys: concatSolos (solos xs) (solos ys);
      set = ys: xs // ys;
    };
  };

  # Centraliser for 'all'
  # Exposes lib.lists.all by default, otherwise uses fold.*._1 structure.
  # Uses structure of global merged fold._1.left
  all = 
    (concatMapAttrs 
      (k: foldType: 
      if elem k ["__functor" "_1" "left" "right"] then {}
      else { 
        ${k} = p: xs: 
          foldType.left
            (acc: x: 
              let px = p x;
              in if isBool px then acc && px
              else y: let pxy = px y; in acc && pxy)
            true
            xs;
      })
      collective-lib.fold
      ) // {
        __functor = self: self.list;
      };

  _tests = with collective-lib.tests; suite {
    collection = {
      list.empty = expect.True (collection []);
      list.full = expect.True (collection [1 2 3]);
      set.empty = expect.True (collection {});
      set.full = expect.True (collection {a = 1; b = 2; c = 3;});
      string.empty = expect.True (collection "");
      string.full = expect.True (collection "abc");
      path = expect.True (collection ./abc);
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
      null_0 = expect.error (size null);
    };

    prepend = {
      listToList = {
        simple = expect.eq (prepend [1 2] [3 4]) [1 2 3 4];
        emptyToList = expect.eq (prepend [] [3 4]) [3 4];
        listToEmpty = expect.eq (prepend [1 2] []) [1 2];
        emptyToEmpty = expect.eq (prepend [] []) [];
      };

      setToSet = {
        simple = expect.eq (prepend {a = 1; b = 2;} {c = 3; d = 4;}) {a = 1; b = 2; c = 3; d = 4;};
        overlapping = expect.eq (prepend {a = 1; b = 2;} {b = 3; c = 4;}) {a = 1; b = 3; c = 4;};
        emptyToSet = expect.eq (prepend {} {c = 3; d = 4;}) {c = 3; d = 4;};
        setToEmpty = expect.eq (prepend {a = 1; b = 2;} {}) {a = 1; b = 2;};
        emptyToEmpty = expect.eq (prepend {} {}) {};
      };

      listToSet = {
        nonSolos = expect.error (prepend [1 2] {c = 3; d = 4;});
        simple = expect.eq (prepend [ {a = 1;} {b = 2;} ] {b = 3; c = 4;}) {a = 1; b = 3; c = 4;};
        overlapping = expect.eq (prepend [ {a = 1;} {b = 2;} ] {b = 3; c = 4;}) {a = 1; b = 3; c = 4;};
        emptyToSet = expect.eq (prepend [] {c = 3; d = 4;}) {c = 3; d = 4;};
        listToEmpty = expect.eq (prepend [ {a = 1;} {b = 2;} ] {}) {a = 1; b = 2;};
        emptyToEmpty = expect.eq (prepend [] {}) {};
      };

      setToList = {
        nonSolos = expect.error (prepend {a = 1; b = 2;} [3 4]);
        simple = expect.eq (prepend {a = 1; b = 2;} [ {c = 3;} {d = 4;} ]) [ {a = 1;} {b = 2;} {c = 3;} {d = 4;} ];
        overlapping = expect.eq (prepend {a = 1; b = 2;} [ {b = 3;} {c = 4;} ]) [ {a = 1;} {b = 3;} {c = 4;} ];
        emptyToList = expect.eq (prepend {} [ {c = 3;} {d = 4;} ]) [ {c = 3;} {d = 4;} ];
        setToEmpty = expect.eq (prepend {a = 1; b = 2;} []) [ {a = 1;} {b = 2;} ];
        emptyToEmpty = expect.eq (prepend {} []) [];
      };
    };

    all = {
      default.true = expect.True (all (x: x > 0) [1 2 3]);
      default.false = expect.False (all (x: x > 1) [1 2 3]);

      attrs.true.value = expect.True (all.attrs (k: v: v > 0) {a=1;b=2;c=3;});
      attrs.false.value = expect.False (all.attrs (k: v: v > 1) {a=1;b=2;c=3;});
      attrs.true.key = expect.True (all.attrs (k: v: size k == 1) {a=1;b=2;c=3;});
      attrs.false.key = expect.False (all.attrs (k: v: size k == 1) {a=1;b=2;ccc=3;});

      solos.true.value = expect.True (all.solos (k: v: v > 0) [{a=1;}{b=2;}{c=3;}]);
      solos.false.value = expect.False (all.solos (k: v: v > 1) [{a=1;}{b=2;}{c=3;}]);
      solos.true.key = expect.True (all.solos (k: v: size k == 1) [{a=1;}{b=2;}{c=3;}]);
      solos.false.key = expect.False (all.solos (k: v: size k == 1) [{a=1;}{b=2;}{ccc=3;}]);
      solos.nonSolos.list = expect.error (all.solos (k: v: size k == 1) [{a=1;b=2;c=3;}]);
      solos.nonSolos.set = expect.error (all.solos (k: v: size k == 1) {a=1;b=2;c=3;});
    };
  };

}
