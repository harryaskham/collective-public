{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with cutils.dispatch;
with cutils.functions;
with cutils.attrsets;

# List utils and aliases.
let 
  log = cutils.log;
  inherit 
    (lib)
    length
    head tail
    take drop
    foldl' foldr
    concatLists
    mergeAttrsList
    min max
    assertMsg
    isFunction
    isList
    ;
in rec {
  # Append element to end of list
  append = x: xs: xs ++ [x];

  # Prepend element to start of list
  cons = x: xs: [x] ++ xs;

  # Insert element at position pos in list xs.
  insertAt = pos: x: xs:
    take pos xs ++ [x] ++ drop pos xs;

  # Polymorphic concat for [[a]] and [{_=a}]
  concat = dispatchElem {
    list = concatLists;
    set = mergeAttrsList;
  };

  # Left-fold f over x with initial value (head xs).
  # Throws if the list is empty.
  foldl1 = f: xs:
    if (length xs == 0) then throw "foldl1: empty list"
    else foldl' f (head xs) (tail xs);

  # Right-fold f over x with initial value (head xs).
  # Throws if the list is empty.
  foldr1 = f: xs:
    if (length xs == 0) then throw "foldr1: empty list"
    else foldr f (head xs) (tail xs);

  # Get the minimum of a list of numbers.
  minimum = foldr1 min;

  # Get the maximum of a list of numbers.
  maximum = foldr1 max;

  # Get the tail of a list or null if empty.
  maybeHead = xs: if xs == [] then null else head xs;

  # Get the tail of a list or null if empty.
  maybeTail = xs: if xs == [] then null else tail xs;

  # Get the head/tail of a list as {head, tail}, { a, [] } for singleton and null;
  maybeSnoc = xs:
    let h = head xs;
        t = tail xs;
    in if xs == [] then null else {head = h; tail = t;};

  mapSnoc = fHead: fTail: xs:
    let snoc = maybeSnoc xs;
    in if snoc == null then xs
       else if snoc.tail == null then [(fHead snoc.head)]
       else
         let tail' = fTail snoc.tail;
             tail'' = if isString tail' then [tail'] else tail';
         in [(fHead snoc.head)] ++ tail'';

  mapHead = fHead: mapSnoc fHead id;

  mapTail = fTail: mapSnoc id fTail;

  # Create a list that must be resolved via 'resolve'
  # but that still has a length.
  LazyList_ = mkThunk: xs:
    assert assertMsg (isList xs) "LazyList: Not a list: ${log.print xs}";
    mkThunk xs
    // rec {
      __ThunkType = "LazyList";
      __isLazyList = true;
      __length = _: length xs;
      __showValue = self: "[${toString (self.__length {})} items]";
    };
  isLazyList = x: isThunkSet x && (x.__isLazyList or false);
  LazyList = LazyList_ (NamedThunk "LazyList");
  maybeLazyList = x: if isLazyList x then x else LazyList x;
  maybeNamedLazyList = name: x: if isLazyList x then setThunkName name x else NamedLazyList name x;
  NamedLazyList = name: LazyList_ (NamedThunk name);

  _tests = with cutils.tests; suite {
    foldl' = {
      sum = {
        expr = foldl' (a: b: a + b) 4 [1 2 3];
        expected = 10;
      };
      reverse = {
        expr = foldl' (xs: x: [x] ++ xs) [] [1 2 3];
        expected = [3 2 1];
      };
    };
    foldr = {
      sum = {
        expr = foldr (a: b: a + b) 4 [1 2 3];
        expected = 10;
      };
      reverse = {
        expr = foldr (x: xs: xs ++ [x]) [] [1 2 3];
        expected = [3 2 1];
      };
    };
    append = {
      expr = append 5 [1 2 3];
      expected = [1 2 3 5];
    };
    cons = {
      expr = cons 0 [1 2 3];
      expected = [0 1 2 3];
    };
    insertAt = {
      start = {
        expr = insertAt 0 99 [1 2 3 4];
        expected = [99 1 2 3 4];
      };
      middle = {
        expr = insertAt 2 99 [1 2 3 4];
        expected = [1 2 99 3 4];
      };
      end = {
        expr = insertAt 4 99 [1 2 3 4];
        expected = [1 2 3 4 99];
      };
      beyondEnd = {
        expr = insertAt 10 99 [1 2 3 4];
        expected = [1 2 3 4 99];
      };
    };
    concat = {
      listOfLists = {
        expr = concat [[1 2] [3 4] [5]];
        expected = [1 2 3 4 5];
      };
      listOfSets = {
        expr = concat [{a=1;}{b=2;c=3;}{d=4;}];
        expected = { a=1; b=2; c=3; d=4; };
      };
    };
    LazyList = {
      is = {
        LazyList.empty = expect.True (isLazyList (LazyList []));
        LazyList.value = expect.True (isLazyList (LazyList [1 2 3]));
        LazyList.throw = expect.True (isLazyList (LazyList [1 2 3 { a = throw "no"; }]));
        NamedLazyList.empty = expect.True (isLazyList (NamedLazyList "name" []));
        NamedLazyList.value = expect.True (isLazyList (NamedLazyList "name" [1 2 3]));
        NamedLazyList.throw = expect.True (isLazyList (NamedLazyList "name" [1 2 3 { a = throw "no"; }]));
        set = expect.False (isLazyList {});
        LazyAttrs = expect.False (isLazyList (LazyAttrs {}));
        Thunk.int = expect.False (isLazyList (Thunk 123));
        Thunk.set = expect.False (isLazyList (Thunk {}));
        Thunk.list = expect.False (isLazyList (Thunk []));
        Thunk.LazyList = expect.False (isLazyList (Thunk (LazyList [])));
        NamedThunk.int = expect.False (isLazyList (NamedThunk "name" 123));
        NamedThunk.set = expect.False (isLazyList (NamedThunk "name" {}));
        NamedThunk.LazyList = expect.False (isLazyList (NamedThunk "name" (LazyList [])));
      };
      length.empty = expect.eq ((LazyList []).__length {}) 0;
      length.full =
        expect.eq
          ((LazyList [1 2 3]).__length {})
          3;
      length.throws =
        expect.eq
          ((LazyList [1 2 3 { a = throw "no"; }]).__length {})
          4;
      resolves.value = expect.eq (resolve (LazyList [1 2 3])) [1 2 3];
      resolves.throw = expect.error (resolve (LazyList [1 2 3 { a = throw "no"; }]));
    };
  };
}
