{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;

with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.errors;
with collective-lib.functions;
with collective-lib.strings;

let
  log = collective-lib.log;
  lists = collective-lib.lists;
  attrsets = rec {
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

    # Get only attribute in a solo attrset.
    # Returns {name, value}.
    getSolo =
      dispatch {
        set = xs:
          if size xs == 1
          then {
            name = head (attrNames xs);
            value = head (attrValues xs);
          }
          else throw "solo(set): Expected exactly one attribute, got ${log.print xs}";
    };

    # Make a solo.
    mkSolo = name: value: { ${name} = value; };

    # Get only attribute in an attrset. Null for a list.
    soloName = xs: (getSolo xs).name;

    # Get the ordered list of solo names.
    soloNames = map soloName;

    # Get only value in an attrset or item in list.
    soloValue = xs: (getSolo xs).value;

    # Get the ordered list of solo values.
    soloValues = map soloValue;

    # Look up a solo value by name in a solo list.
    lookupSolos = name: xs: (mergeSolos xs).${name};

    # Look up a solo value by name in a solo list.
    lookupSolosDef = name: def: xs: (mergeSolos xs).${name} or def;

    # Check if an attribute is solo.
    checkSolo = x:
      assert assertMsg (isAttrs x) "checkSolo: Not an attrset: ${log.print x}";
      assert assertMsg (size x == 1) "checkSolo: Expected exactly one attribute, got ${log.print x}";
      x;

    isSolo = xs: tryBool (checkSolo xs);

    # true iff xs is a valid solo list.
    isSolos = xs: tryBool (checkSolos xs);

    # Throw if xs is not a valid solo list.
    # Cannot use mapSolo/filterSolo as they use this for typechecking.
    checkSolos = xs:
      let
        nonSoloXs = filter (x: !isSolo x) xs;
        nonSoloXSizes = map size nonSoloXs;
        names = soloNames xs;
        nameToCountSolos = map (name: { ${name} = count (name_: name == name_) names; }) names;
        duplicateNameCountSolos = filter (nameCount: (soloValue nameCount) > 1) nameToCountSolos;
        duplicateNameBlock =
          indent.lines
            (mapAttrsToList
              (name: count:
                ''
                  ${name} (${toString count} ${pluralises count "occurence"})
                '')
              (mergeAttrsList duplicateNameCountSolos));
      in
        assert assertMsg (isList xs) (indent.block ''
          checkSolos: Not a list
            xs = ${indent.here (log.print xs)}
        '');
        assert assertMsg (nonSoloXs == []) (indent.block ''
          checkSolos: List contains non-solo items
              xs sizes = ${indent.here (log.print nonSoloXSizes)}
            non-solo xs = ${indent.here (log.vprint nonSoloXs)}
                    xs = ${indent.here (log.print xs)}
        '');
        assert assertMsg (duplicateNameCountSolos == []) (indent.block ''
          checkSolos: List contains duplicate solo names
                    xs = ${indent.here (log.print xs)}
            duplicates = ${indent.here duplicateNameBlock}
        '');
        xs;

    # Convert:
    # - an attribute set to a list of solo attributes.
    # - a list of attributes to itself, with assertions that they are solo.
    solos = dispatch {
      list = checkSolos;
      set = mapAttrsToList (k: v: { ${k} = v; });
    };

    # Map an (f :: k -> v -> a) over a list of solos [{k -> v}], returning a list of solos [{k -> a}].
    # If xs is a set, it is first converted to solos.
    mapSolos = {
      __functor = self: f: xs: map (mapAttrs f) (solos xs);
      to = {
        # Map an (f :: k -> v -> a) over a list of solos [{k -> v}], returning a list of values [a].
        # If xs is a set, it is first converted to solos.
        list = f: xs: soloValues (mapSolos f xs);
      };
    };

    # Map an (f :: k -> v -> {k' = v'}) over a list of solos [{k -> v}], returning another list of solos [{k' -> v'}].
    # If xs is a set, it is first converted to solos.
    # f must return a solo; this preserves the solos form for lists.
    concatMapSolos = f: dispatch {
      list = map (x: checkSolo (f (soloName x) (soloValue x)));
      set = xs: mergeSolos (concatMapSolos f (solos xs));
    };

    # Fold over attrs and solos with either acc -> k -> v -> acc or k -> v -> acc -> acc.
    # Centralised by merge with lists.fold etc to make collective-lib.fold.{left, right, _1, .{attrs,solos}.{left,right,_1}}
    fold = {
      # Note - different to foldAttrs - the fold.attrs f init xs has an xs of attrs type.
      # lib.foldAttrs folds over a list of attrs.
      # This essentially treats the attrs as a list of solos.
      # left/right is a dangerous distinction, depends on nix's set iteration order.
      attrs = {
        __functor = self: self.left;
        left = attrsets.fold.solos.left // { __mkList = solos; };
        right = attrsets.fold.solos.right // { __mkList = solos; };
      };

      solos = {
        __functor = self: self.left;
        left = lists.mkFoldSpec {
          __mkF = f: acc: x: f acc (soloName x) (soloValue x);
          __mkList = checkSolos;
          __functor = self: f: init: xs: lists.fold.left (self.__mkF f) init (self.__mkList xs);
        };
        right = lists.mkFoldSpec {
          __mkF = f: x: acc: f (soloName x) (soloValue x) acc;
          __mkList = checkSolos;
          __functor = self: f: init: xs: lists.fold.right (self.__mkF f) init (self.__mkList xs);
        };
      };

      _1 = {
        solos = {
          __functor = self: self.left;
          left = lists.mkFoldSpec {
            inherit (attrsets.fold.solos.left) __mkF __mkList;
            __functor = self: f: xs: lists.fold._1.left (self.__mkF f) (self.__mkList xs);
          };
          right = lists.mkFoldSpec {
            inherit (attrsets.fold.solos.right) __mkF __mkList;
            __functor = self: f: xs: lists.fold._1.right (self.__mkF f) (self.__mkList xs);
          };
        };
      };
    };

    # Merge a list of solos [{k -> a}] into a single attrset {k -> a}.
    # Fails if any element is not a solo.
    # Duplicates are merged preferring later entries.
    mergeSolos = xs: mergeAttrsList (checkSolos xs);

    # Concatenates two lists of solos [{k -> a}] into a single list of solos [{k -> a}].
    # If a solo appears in both lists, the second list is preferred, but the position of the
    # solo is maintained in the first list.
    concatSolos = xs: ys:
      let xsMerged = mergeSolos xs;
          ysMerged = mergeSolos ys;
          xsUpdated = mapSolos (xName: x: ysMerged.${xName} or x) xs;
          ysFiltered = filterSolos (yName: y: !(xsMerged ? ${yName})) ys;
      in xsUpdated ++ ysFiltered;

    # Adds a solo to the front of a list of solos, or update it in place if it already exists.
    consSolo = x: xs:
      if (mergeSolos xs) ? ${soloName x}
        then concatSolos xs [x]
        else concatSolos [x] xs;

    # Splits solos into its head and tail. Converts to solos first if needed.
    # Returns {head, tail} or null if empty.
    # Throws error if empty.
    snocSolos = xs:
      assert assertMsg (nonEmpty xs) "snocSolos: Empty input provided: ${log.print xs}";
      lists.maybeSnoc (solos xs);

    # Partition a list of solos [{k -> a}] based on a predicate.
    # Follows lib.partition interface for lists.
    partitionSolos = pred: xs:
      { right = filter (x: pred (soloName x) (soloValue x)) xs;
        wrong = filter (x: !(pred (soloName x) (soloValue x))) xs;
      };

    # Map an (f :: int -> k -> a -> b) over a list of solos [{k -> a}], returning another list of solos [{k -> b}].
    # f has access to the index in the list.
    # If xs is a set, it is first converted to solos.
    imapSolos = f: xs: imap0 (i: mapAttrs (f i)) (solos xs);

    # Map an (f :: int -> k -> a -> b) over a collection of solos {k -> a}.
    # f has access to the index in the list.
    # Preserve the type s.t. a set is converted to solos, has f applied, and is then merged back.
    ifmapSolos = f: dispatch {
      list = imapSolos f;
      set = xs: mergeAttrsList (imapSolos f xs);
    };

    # Filter a list of solos by a predicate of name and value.
    # f :: name -> value -> bool
    filterSolos = f: xs: filter (x: f (soloName x) (soloValue x)) (checkSolos xs);

    # Add an attribute name to each attrset in the list containing its index.
    # Defaults to "__index"
    indexed = xs: ifmapSolos (index: _: value: { inherit index value; }) xs;
    indices = concatMapSolos (k: x: mkSolo k x.index);
    unindexed = concatMapSolos (k: x: mkSolo k x.value);

    # Diff two attrsets, returning any divergent keys and their values.
    diff = a: b:
      if isList a && isList b
        then
          (zipListsWith
            (a: b: diff a b)
            a
            b)
          ++ (if length a < length b
              then map (x: { missing_in_a = x; }) (drop (length a) b)
              else if length b > length a
              then map (x: { missing_in_b = x; }) (drop (length b) a)
              else [])
      else if isAttrs a && isAttrs b
        then
          (zipAttrsWith
            (name: values:
              if length values == 1
              then {
                missing = {
                  value = elemAt values 0;
                };
              }
              else
                diff
                  (elemAt values 0)
                  (elemAt values 1))
            [a b])
      else if isFunction a && isFunction b then { __unequal = "<lambda>"; }
      else if a == b then { __equal = a; }
      else { __unequal = { inherit a b; }; };

    diffShort = a: b: deepFilter (x: !(x ? __equal)) (diff a b);

    TerseAttrs = xs: {
      __functor = self: _: xs;
      __toString = self: "{${joinSep ", " (attrNames xs)}}";
    };

    # Create an attrset that must be resolved via 'resolve'
    # but that still has attrNames capability.
    LazyAttrs_ = mkThunk: xs:
      assert assertMsg (isAttrs xs) "LazyAttrs: Not an attrset: ${log.print xs}";
      mkThunk xs
      // rec {
        __ThunkType = "LazyAttrs";
        __isLazyAttrs = true;
        __attrNames = _: attrNames xs;
        __showValue = self: "${toString (size xs)} attrs";
        __showExtra = self:
          let names = self.__attrNames {};
          in optionalString (size names > 0) "(${joinSep ", " (self.__attrNames {})})";
      };
    isLazyAttrs = x: isThunkSet x && (x.__isLazyAttrs or false);
    LazyAttrs = LazyAttrs_ (NamedThunk "LazyAttrs");
    maybeLazyAttrs = x: if isLazyAttrs x then x else LazyAttrs x;
    maybeNamedLazyAttrs = name: x: if isLazyAttrs x then setThunkName name x else NamedLazyAttrs name x;
    NamedLazyAttrs = name: LazyAttrs_ (NamedThunk name);

    _tests = with collective-lib.tests; suite {
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

      solos =
      let
        s0 = mkSolo "abc" 123;
        s1 = mkSolo "def" 456;
        notSolo = { a = 1; b = 2; };
        soloSet = { abc = 123; def = 456; };
        soloList = [ { abc = 123; } { def = 456; } ];
        notSoloList = [ s0 notSolo s1 ];
      in {
        mkSolo = expect.eq s0 { abc = 123; };
        getSolo = {
          s0 = expect.eq (getSolo s0) { name = "abc"; value = 123; };
          notSolo = expect.error (getSolo notSolo);
        };
        soloName = {
          s0 = expect.eq (soloName s0) "abc";
          notSolo = expect.error (soloName notSolo);
        };
        soloValue = {
          s0 = expect.eq (soloValue s0) 123;
          notSolo = expect.error (soloValue notSolo);
        };
        solos = {
          singleSoloToList = expect.eq (solos {a = 1;}) [ {a = 1;} ];
          setToList = expect.eq (sortOn soloName (solos soloSet)) soloList;
          listToList = expect.eq (solos soloList) soloList;
          string = expect.error (solos "notSolo");
        };
        isSolo = {
          s0 = expect.True (isSolo s0);
          s1 = expect.True (isSolo s1);
          notSolo = expect.False (isSolo notSolo);
          emptySet = expect.False (isSolo {});
          emptyList = expect.False (isSolo []);
          int = expect.False (isSolo 123);
          intList = expect.False (isSolo [123]);
          string = expect.False (isSolo "notSolo");
          soloList = expect.False (isSolo [s0]);
          setOfSolos = expect.False (isSolo {inherit s0 s1;});
        };
        isSolos = {
          validSolos = {
            emptyList = expect.True (isSolos []);
            soloList0 = expect.True (isSolos [s0]);
            soloList1 = expect.True (isSolos [s1]);
            soloList01 = expect.True (isSolos [s0 s1]);
          };
          nonList = {
            emptySet = expect.False (isSolos {});
            setOfSolos = expect.False (isSolos {inherit s0 s1;});
            int = expect.False (isSolos 123);
            string = expect.False (isSolos "notSolo");
          };
          nonSoloEntries = {
            notSolo = expect.False (isSolos [notSolo]);
            soloList0N = expect.False (isSolos [s0 notSolo]);
            soloListN0 = expect.False (isSolos [notSolo s0]);
            intList = expect.False (isSolos [123]);
            soloList01I = expect.False (isSolos [s0 s1 123]);
          };
          duplicates = {
            soloList00 = expect.False (isSolos [s0 s0]);
            soloList11 = expect.False (isSolos [s1 s1]);
            soloList101 = expect.False (isSolos [s1 s0 s1]);
            soloList010 = expect.False (isSolos [s0 s1 s0]);
            soloList000111 = expect.False (isSolos [s0 s0 s0 s1 s1 s1]);
          };
        };
        mapSolos = {
          setToList = expect.eq (sortOn soloName (mapSolos (_: x: x+1) soloSet)) [ {abc = 124;} {def = 457;} ];
          listToList = expect.eq (mapSolos (_: x: x+1) soloList) [ {abc = 124;} {def = 457;} ];
          notSoloList = expect.error (mapSolos (_: x: x+1) notSoloList);
          withName = expect.eq (mapSolos (n: x: "${n}${toString x}") soloList) [ {abc = "abc123";} {def = "def456";} ];
          to.list = {
            listToList = expect.eq (mapSolos.to.list (_: x: x+1) soloList) [ 124 457 ];
            setToList = expect.eq (mapSolos.to.list (_: x: x+1) soloSet) [ 124 457 ];
            notSoloList = expect.error (mapSolos.to.list (_: x: x+1) notSoloList);
            withName = expect.eq (mapSolos.to.list (n: x: "${n}${toString x}") soloList) [ "abc123" "def456" ];
            valuesOnly = expect.eq (mapSolos.to.list (_: x: mod x 2 == 0) soloList) [ false true ];
          };
        };
        fold.solos = 
          let fold = attrsets.fold;
          in {
            left = 
              expect.eq 
                (fold.solos.left (acc: k: v: rec {x = acc.x + v; s = "${k} ${toString x};${acc.s}";}) {x=0; s="";} [{a=1;} {b=2;} {c=3;}])
                {x = 6; s = "c 6;b 3;a 1;";};
            right = 
              expect.eq 
                (fold.solos.right (k: v: acc: rec {x = acc.x + v; s = "${k} ${toString x};${acc.s}";}) {x=0; s="";} [{a=1;} {b=2;} {c=3;}])
                {x = 6; s = "a 6;b 5;c 3;";};
            _1 = {
              left = 
                expect.eq 
                  (fold._1.solos.left (acc: k: v: {"${soloName acc}${k}" = soloValue acc + v;}) [{a=1;} {b=2;} {c=3;}])
                  {abc = 6;};
              right = 
                expect.eq 
                  (fold._1.solos.right (k: v: acc: {"${soloName acc}${k}" = soloValue acc + v;}) [{a=1;} {b=2;} {c=3;}])
                  {acb = 6;};
            };
          };
        mergeSolos = {
          soloList = expect.eq (mergeSolos [s0 s1]) {abc = 123; def = 456;};
          notSoloList = expect.error (mergeSolos notSoloList);
        };
        concatSolos = {
          concatSolos01 = expect.eq (concatSolos [s0] [s1]) [{abc = 123;} {def = 456;}];
          concatSolos10 = expect.eq (concatSolos [s1] [s0]) [{def = 456;} {abc = 123;}];
          concatSolos00 = expect.eq (concatSolos [s0] [s0]) [{abc = 123;}];
          concatSolos11 = expect.eq (concatSolos [s1] [s1]) [{def = 456;}];
          preserveOrder = expect.eq (concatSolos [{a = 1;} {b = null;} {c = 3;}] [{b = 2;} {d = 4;}]) [{a = 1;} {b = 2;} {c = 3;} {d = 4;}];
          concatSolos011 = expect.error (concatSolos [s0] [s1 s1]);
          nonSoloListLHS = expect.error (concatSolos notSoloList soloList);
          nonSoloListRHS = expect.error (concatSolos soloList notSoloList);
          nonSoloListBoth = expect.error (concatSolos notSoloList notSoloList);
        };
        consSolo = {
          consSolo01 = expect.eq (consSolo s0 [s1]) [{abc = 123;} {def = 456;}];
          consSolo10 = expect.eq (consSolo s1 [s0]) [{def = 456;} {abc = 123;}];
          consSolo00 = expect.eq (consSolo s0 [s0]) [{abc = 123;}];
          consSolo11 = expect.eq (consSolo s1 [s1]) [{def = 456;}];
          preserveOrder =
            expect.eq
              (consSolo {b = 2;} [{a = 1;} {b = null;} {c = 3;}])
              [{a = 1;} {b = 2;} {c = 3;}];
          consSolo011 = expect.error (consSolo s0 [s1 s1]);
          nonSoloListLHS = expect.error (consSolo notSolo soloList);
          nonSoloListRHS = expect.error (consSolo s0 notSoloList);
          nonSoloListBoth = expect.error (consSolo notSolo notSoloList);
        };
        snocSolos = {
          snocSolo01 = expect.eq (snocSolos [s0 s1]) { head = {abc = 123;}; tail = [{def = 456;}]; };
          snocSolo10 = expect.eq (snocSolos [s1 s0]) { head = {def = 456;}; tail = [{abc = 123;}]; };
          snocSoloSet = expect.eq (snocSolos (s0 // s1)) { head = {abc = 123;}; tail = [{def = 456;}]; };
          snocSolo00 = expect.error (snocSolos [s0 s0]);
          snocSolo11 = expect.error (snocSolos [s1 s1]);
        };
        concatMapSolos = {
          setToList = expect.eq (concatMapSolos (k: x: mkSolo k (x+1)) soloSet) {abc = 124; def = 457;};
          listToList = expect.eq (concatMapSolos (k: x: mkSolo k (x+1)) soloList) [ {abc = 124;} {def = 457;} ];
          notSoloList = expect.error (concatMapSolos (k: x: mkSolo k (x+1)) notSoloList);
          withName = expect.eq (concatMapSolos (n: x: mkSolo n "${n}${toString x}") soloList) [ {abc = "abc123";} {def = "def456";} ];
        };
        ifmapSolos = {
          setToSet = expect.eq (ifmapSolos (i: _: x: x+(1000*(i+1))) soloSet) {abc = 1123; def = 2456;};
          listToList = expect.eq (ifmapSolos (i: _: x: x+(1000*(i+1))) soloList) [ {abc = 1123;} {def = 2456;} ];
          notSolo = expect.eq (ifmapSolos (i: _: x: x+(10*(i+1))) notSolo) { a = 11; b = 22; };
          notSoloList = expect.error (ifmapSolos (_: _: x: x) notSoloList);
          withName =
            expect.eq
              (ifmapSolos (i: n: x: "${toString i}, ${n}, ${toString x}") soloSet)
              {abc = "0, abc, 123"; def = "1, def, 456";};
        };
        filterSolos = {
          evens = expect.eq (filterSolos (_: v: mod v 2 == 0) soloList) [ {def = 456;} ];
          odds = expect.eq (filterSolos (_: v: mod v 2 == 1) soloList) [ {abc = 123;} ];
          byName = expect.eq (filterSolos (n: _: n == "abc") soloList) [ {abc = 123;} ];
        };
      };

      index = {
        addToList = expect.eq (indexed [ {a = 1;} {b = 2;} ]) [{ a = { index = 0; value = 1; }; } { b = { index = 1; value = 2; }; }];
        addToSet = expect.eq (indexed {a = 1; b = 2;}) { a = {index = 0; value = 1;}; b = { index = 1; value = 2; }; };
        unindexedList = expect.eq (unindexed (indexed [ {a = 1;} {b = 2;} ])) [ {a = 1;} {b = 2;} ];
        unindexedSet = expect.eq (unindexed (indexed {a = 1; b = 2;})) {a = 1; b = 2;};
        indicesList = expect.eq (indices (indexed [ {a = 1;} {b = 2;} ])) [ {a = 0;} {b = 1;} ];
        indicesSet = expect.eq (indices (indexed {a = 1; b = 2;})) {a = 0; b = 1;};
      };

      LazyAttrs = {
        is = {
          LazyAttrs.empty = expect.True (isLazyAttrs (LazyAttrs {}));
          LazyAttrs.value = expect.True (isLazyAttrs (LazyAttrs { a = 123; }));
          LazyAttrs.throw = expect.True (isLazyAttrs (LazyAttrs { a = throw "no"; }));
          NamedLazyAttrs.empty = expect.True (isLazyAttrs (NamedLazyAttrs "name" {}));
          NamedLazyAttrs.value = expect.True (isLazyAttrs (NamedLazyAttrs "name" { a = 123; }));
          NamedLazyAttrs.throw = expect.True (isLazyAttrs (NamedLazyAttrs "name" { a = throw "no"; }));
          set = expect.False (isLazyAttrs {});
          Thunk.int = expect.False (isLazyAttrs (Thunk 123));
          Thunk.set = expect.False (isLazyAttrs (Thunk {}));
          Thunk.LazyAttrs = expect.False (isLazyAttrs (Thunk (LazyAttrs {})));
          NamedThunk.int = expect.False (isLazyAttrs (NamedThunk "name" 123));
          NamedThunk.set = expect.False (isLazyAttrs (NamedThunk "name" {}));
          NamedThunk.LazyAttrs = expect.False (isLazyAttrs (NamedThunk "name" (LazyAttrs {})));
        };
        names.empty = expect.eq ((LazyAttrs {}).__attrNames {}) [];
        names.full.values =
          expect.eq
            ((LazyAttrs {a = "a"; b = 123;}).__attrNames {})
            ["a" "b"];
        names.full.throws =
          expect.eq
            ((LazyAttrs {a = throw "no"; b = 123;}).__attrNames {})
            ["a" "b"];
        resolves.value = expect.eq (resolve (LazyAttrs {a = 123;})) {a = 123;};
        resolves.throw = expect.error (resolve (LazyAttrs {a = throw "no";}));
      };
    };

  };
in 
  attrsets
