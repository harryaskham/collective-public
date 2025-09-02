{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

# TODO: Move other polymorphic collection functions here e.g. size.

with collective-lib.attrsets;
with collective-lib.dispatchlib;
with collective-lib.errors;
with collective-lib.functions;
with collective-lib.lists;
with collective-lib.strings;
with collective-lib.tests;
with lib;

let 
  log = collective-lib.log;
  typed = collective-lib.typed;
  errors = collective-lib.errors;
  lists = collective-lib.lists;
  strings = collective-lib.strings;
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
  isCollection = x: errors.tryBool (collection.check x);

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

  # Get the elements of a collection with an int index
  # Returns a list of {i, value} pairs.
  iter = dispatch {
    list = imap0 (i: value: {inherit i value;});
    set = compose iter elems;
    string = compose iter elems;
    path = compsoe iter elems;
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
      assert assertMsg (isCollection x) "empty.check: Got non-collection ${typeOf x}"; 
      assert assertMsg (empty x) "empty.check: Got non-empty ${typeOf x}"; 
      x;
  };

  # Polymorphic collection non-emptiness check
  nonEmpty = {
    __functor = self: x: !(empty x);
    check = x_: 
      assert assertMsg (isCollection x) "nonEmpty.check: Got non-collection ${typeOf x}"; 
      assert assertMsg (nonEmpty x) "nonEmpty.check: Got empty ${typeOf x}"; 
      x;
  };

  # Any non-collection is considered non-empty
  safeEmpty = x: if isCollection x then empty x else false;

  # Any non-collection is considered non-empty
  safeNonEmpty = x: !(safeEmpty x);

  # Handle string replication for StringW, Strings
  replicate = n: dispatch {
    list = xs: typed.concat.lists (lib.lists.replicate n xs);
    string = s: lib.strings.replicate n s;
    set = s:
      if isStrings s then s.replicate n
      else throw "Invalid set argument to replicate: ${_ph_ s} (expected Strings)";
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

  # Centraliser for 'fold' that dispatches dynamically on the final arg.
  fold = {
    __functor = self: self.left;
    left = f: init: dispatch {
      set = typed.fold.attrs.left f init;
      list = typed.fold.list.left f init;
    };
    right = f: init: dispatch {
      set = typed.fold.attrs.right f init;
      list = typed.fold.list.right f init;
    };
    _1 = {
      __functor = self: self.left;
      left = f: dispatch {
        set = typed.fold._1.attrs.left f;
        list = typed.fold._1.list.left f;
      };
      right = f: dispatch {
        set = typed.fold._1.attrs.right f;
        list = typed.fold._1.list.right f;
      };
    };
  };

  # Centraliser for 'all'
  # Exposes lib.lists.all by default, otherwise uses fold.*._1 structure.
  # Uses structure of global merged fold._1.left
  all = {
    __functor = self: self.list;
    list = p: xs: fold.left (acc: x: acc && p x) true xs;
    solos = p: xs: all.list (x: p (soloName x) (soloValue x)) xs;
    attrs = p: xs: all.solos p (solos xs);
  };

  # Expose pad polymorphically via dispatch functor trick.
  pad = args: {
    __functor = dispatch;
    list = padList args;
    string = padString args;
    set = xs: 
      if isStrings xs then padString args xs
      else throw "Invalid set argument to pad: ${_ph_ xs} (expected Strings)";
  };

  padLongest = padLongest_ {};
  padLongest_ = args: dispatch {
    list = xs: 
      let width = maximum (map size xs);
      in map (pad (args // {to = width;})) xs;
    set = xs:
      let width = maximum (mapAttrsToList (_: size) xs);
      in mapAttrs (_: pad (args // {to = width;})) xs;
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

    replicate = {
      list = {
        simple = expect.eq (replicate 3 [1 2]) [1 2 1 2 1 2];
        empty = expect.eq (replicate 3 []) [];
      };
      string = {
        simple = expect.eq (replicate 3 "a") "aaa";
        empty = expect.eq (replicate 3 "") "";
        StringW = expect.eq 
          (toString (replicate 3 (StringW 1 "a"))) 
          "aaa";
        Strings = expect.eq 
          (toString (replicate 3 (Strings ["a" "b"]))) 
          "ababab";
        Char = expect.eq 
          (toString (replicate 3 (Char "a"))) 
          "aaa";
      };
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

    pad = {
      string = expect.eq (pad {to = 5;} "abc") "abc  ";
      list = expect.eq (pad {to = 5;} [1 2 3]) [1 2 3 null null];
      poly = 
        let ansi = collective-lib.script-utils.ansi-utils.ansi;
            redHello = toString (with ansi; style [underline fg.red] "hello");
        in expect.eq 
          (map
            (pad {to = 10; display = true; emptyChar = "x"; emptyElem = 7;})
            ["abcde" [1 2 3] redHello])
          ["abcdexxxxx" [1 2 3 7 7 7 7 7 7 7] "${redHello}xxxxx"];
      padLongest.list = expect.eq (padLongest ["hi" "hello" [0 1 2 3]]) ["hi   " "hello" [0 1 2 3 null]];
      padLongest.set = expect.eq (padLongest {a = "hi"; b = "hello"; c = [0 1 2 3];}) {a = "hi   "; b = "hello"; c = [0 1 2 3 null];};
    };
  };

}
