{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with collective-lib.clib;
with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.functions;
with collective-lib.attrsets;
with collective-lib.rebinds;
with collective-lib.strings;
with {
  inherit (lib)
  const
  length
  head tail
  take drop
  concatLists
  mergeAttrsList
  min max
  assertMsg
  isFunction
  isList
  ;
  inherit (lib.trivial)
  id
  ;
};

# List utils and aliases.
let 
  log = collective-lib.log;
  typed = collective-lib.typed;
in rec {
  # Append element to end of list
  append = x: xs: xs ++ [x];

  # Prepend element to start of list
  cons = x: xs: [x] ++ xs;

  # Insert element at position pos in list xs.
  insertAt = pos: x: xs:
    take pos xs ++ [x] ++ drop pos xs;

  # Update element at position pos in list xs.
  updateAt = pos: f: xs:
    let init = take pos xs;
        rest = drop pos xs;
    in init ++ mapSnoc f id rest;

  # Set element at position pos in list xs.
  setAt = pos: x: updateAt pos (const x);

  # Delete element at position pos in list xs.
  deleteAt = pos: xs:
    take pos xs ++ drop (pos + 1) xs;

  # Polymorphic concat for [""], [[a]] and [{_=a}]
  concat = lib.fix (this: {
    def = ifEmpty: xs:
      if safeEmpty xs then ifEmpty
      else dispatch.elem {
        list = concatLists;
        set = mergeAttrsList;
        string = join;
      } xs;

    lists = xs: this.def [] xs;
    sets = xs: this.def {} xs;
    strings = xs: this.def "" xs;

    # Default to returning self if empty.
    __functor = self: xs: this.def xs xs;
  });

  dropWhile = pred: xs:
    if empty xs then xs
    else let s = snoc xs; in if pred s.head then dropWhile pred s.tail
    else xs;

  takeWhile = pred: xs:
    if empty xs then xs
    else let s = snoc xs; in if pred s.head then [s.head] ++ takeWhile pred s.tail
    else [];

  # Fold centraliser.
  # Merges with other modules that define fold functions.
  # A fold variant spec must have __mkF to produce a binary foldl or foldr f function
  # and a __mkList to produce a list to fold over.
  # Defaults to a regular left list fold.
  mkFoldSpec = xs: 
    {
      __isFoldSpec = true;
      __1 = false;
      __assertion = xs: true;
      __mkF = id;
      __mkList = id;
      __mkInit = f: init: xs: init;
      __foldFn = lib.foldl';
      __functor = self:
        if self.__1 then 
          f: xs:
            assert self.__assertion xs;
            self.__foldFn (self.__mkF f) (self.__mkInit f null xs) (self.__mkList xs)
        else 
          f: init: xs:
            assert self.__assertion xs;
            self.__foldFn (self.__mkF f) (self.__mkInit f init xs) (self.__mkList xs);
    } // xs;

  isFoldSpec = xs: xs.__isFoldSpec or false;

  fold = rec {
    list = {
      __functor = self: self.left;

      left = mkFoldSpec {};

      right = mkFoldSpec {
        __foldFn = lib.foldr;
      };
    };

    _1 = rec {
      list = {
        __functor = self: self.left;

        # Left-fold f over x with initial value (head xs).
        # Throws if the list is empty.
        left = mkFoldSpec rec {
          __assertion = xs:
            assertMsg (typed.nonEmpty xs) "fold._1.left: empty list";
          __1 = true;
          __mkInit = f: _: xs: head xs;
          __mkList = tail;
        };

        # Right-fold f over x with initial value (head xs).
        # Throws if the list is empty.
        right = mkFoldSpec rec {
          __assertion = xs:
            assertMsg (typed.nonEmpty xs) "fold._1.right: empty list";
          __1 = true;
          __mkInit = f: _: xs: head xs;
          __mkList = tail;
          __foldFn = lib.foldr;
        };
      };
    };
  };

  # Legacy aliases.
  foldl1 = typed.fold._1.left;
  foldr1 = typed.fold._1.right;

  # Get the minimum of a list of numbers.
  minimum = typed.fold._1.right min;

  # Get the maximum of a list of numbers.
  maximum = typed.fold._1.right max;

  # Get the sum of a list of numbers.
  sum = typed.fold._1.right (a: b: a + b);

  # Get the tail of a list or null if empty.
  maybeHead = xs: if xs == [] then null else head xs;

  # Get the tail of a list or null if empty.
  maybeTail = xs: if xs == [] then null else tail xs;

  # Get the last of a list or null if empty.
  maybeLast = xs: if xs == [] then null else lib.last xs;

  # As maybeSnoc, but throws if the list is empty.
  snoc = xs:
    assert assertMsg (nonEmpty xs) "snoc: Empty list";
    maybeSnoc xs;

  # Get the head/tail of a list as {head, tail}, { a, [] } for singleton and null;
  maybeSnoc = xs:
    let h = head xs;
        t = tail xs;
    in if xs == [] then null else {head = h; tail = t;};

  # Map over the head and tail of a list.
  mapSnoc = fHead: fTail: xs:
    let snoc = maybeSnoc xs;
    in if snoc == null then xs
       else if snoc.tail == null then [(fHead snoc.head)]
       else
         let tail' = fTail snoc.tail;
         in [(fHead snoc.head)] ++ tail';

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

  padList = { to, emptyElem ? null, align ? "left", ... }: xs:
    let len = size xs;
        paddingSize = max 0 (to - len);
        padding = typed.replicate paddingSize [emptyElem];
        halfPaddingL = typed.replicate (builtins.floor (paddingSize / 2.0)) [emptyElem];
        halfPaddingR = typed.replicate (builtins.ceil (paddingSize / 2.0)) [emptyElem];
    in switch align {
      left = xs ++ padding;
      right = padding ++ xs;
      center = halfPaddingL ++ xs ++ halfPaddingR;
    };

  # The default 'lib.sort' always requires e.g. (a: b: a < b) as a key function.
  LT = a: b: a < b;
  LTE = a: b: a <= b;
  GT = a: b: a > b;
  GTE = a: b: a >= b;
  EQ = a: b: a == b;

  sorted = lib.fix (self: {
    # default to LT for 'sorted [3 2 1]' etc.
    __functor = self: self.up;
    up = self.by LT;
    down = self.by GT;
    by = lib.sort;
    on = lib.sortOn;
  });

  _tests = with collective-lib.tests; suite {
    fold.list.default = {
      sum = expect.eq 10 (fold.list (a: b: a + b) 4 [1 2 3]);
      reverse = expect.eq [3 2 1] (fold.list (xs: x: [x] ++ xs) [] [1 2 3]);
    };
    fold.list.left = {
      sum = expect.eq 10 (fold.list.left (a: b: a + b) 4 [1 2 3]);
      reverse = expect.eq [3 2 1] (fold.list.left (xs: x: [x] ++ xs) [] [1 2 3]);
    };
    fold.list.right = {
      sum = expect.eq 10 (fold.list.right (a: b: a + b) 4 [1 2 3]);
      reverse = expect.eq [3 2 1] (fold.list.right (x: xs: xs ++ [x]) [] [1 2 3]);
    };
    fold._1.list.default = {
      sum = expect.eq 6 (fold._1.list (a: b: a + b) [1 2 3]);
    };
    fold._1.list.left = {
      sum = expect.eq 6 (fold._1.list.left (a: b: a + b) [1 2 3]);
    };
    fold._1.list.right = {
      sum = expect.eq 6 (fold._1.list.right (a: b: a + b) [1 2 3]);
    };

    append = expect.eq (append 5 [1 2 3]) [1 2 3 5];

    cons = expect.eq (cons 0 [1 2 3]) [0 1 2 3];

    insertAt = {
      start = expect.eq (insertAt 0 99 [1 2 3 4]) [99 1 2 3 4];
      middle = expect.eq (insertAt 2 99 [1 2 3 4]) [1 2 99 3 4];
      end = expect.eq (insertAt 4 99 [1 2 3 4]) [1 2 3 4 99];
      beyondEnd = expect.eq (insertAt 10 99 [1 2 3 4]) [1 2 3 4 99];
    };

    updateAt = {
      start = expect.eq (updateAt 0 (a: a + 1) [1 2 3 4]) [2 2 3 4];
      middle = expect.eq (updateAt 2 (a: a + 1) [1 2 3 4]) [1 2 4 4];
      end = expect.eq (updateAt 3 (a: a + 1) [1 2 3 4]) [1 2 3 5];
    };

    setAt = {
      start = expect.eq (setAt 0 99 [1 2 3 4]) [99 2 3 4];
      middle = expect.eq (setAt 2 99 [1 2 3 4]) [1 2 99 4];
      end = expect.eq (setAt 3 99 [1 2 3 4]) [1 2 3 99];
    };

    deleteAt = {
      start = expect.eq (deleteAt 0 [1 2 3 4]) [2 3 4];
      middle = expect.eq (deleteAt 2 [1 2 3 4]) [1 2 4];
      end = expect.eq (deleteAt 3 [1 2 3 4]) [1 2 3];
    };

    concat = {
      listOfLists = expect.eq (concat [[1 2] [3 4] [5]]) [1 2 3 4 5];
      listOfSets = expect.eq (concat [{a=1;}{b=2;c=3;}{d=4;}]) { a=1; b=2; c=3; d=4; };
      listOfStrings = expect.eq (concat ["a" "b" "c"]) "abc";
      empty = {
        lists = expect.eq (concat.lists []) [];
        sets = expect.eq (concat.sets []) {};
        strings = expect.eq (concat.strings []) "";
        functor = expect.eq (concat []) [];
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
    };

    dropWhile = {
      empty = expect.eq (dropWhile (a: a > 0) []) [];
      single = expect.eq (dropWhile (a: a > 0) [1]) [];
      multiple = expect.eq (dropWhile (a: a > 1) [1 2 3]) [1 2 3];
      multiple2 = expect.eq (dropWhile (a: a < 3) [1 2 3]) [3];
    };

    takeWhile = {
      empty = expect.eq (takeWhile (a: a > 0) []) [];
      single = expect.eq (takeWhile (a: a > 0) [1]) [1];
      multiple = expect.eq (takeWhile (a: a > 0) [1 2 3]) [1 2 3];
      multiple2 = expect.eq (takeWhile (a: a < 3) [1 2 3]) [1 2];
    };

    padList = {
      left = expect.eq (padList {to = 10; align = "left";} [0 1 2 3 4]) [0 1 2 3 4 null null null null null];
      right = expect.eq (padList {to = 10; align = "right";} [0 1 2 3 4]) [null null null null null 0 1 2 3 4];
      center.exact = expect.eq (padList {to = 9; align = "center";} [0 1 2 3 4]) [null null 0 1 2 3 4 null null];
      center.roundUp = expect.eq (padList {to = 10; align = "center";} [0 1 2 3 4]) [null null 0 1 2 3 4 null null null];
      center.roundDown = expect.eq (padList {to = 8; align = "center";} [0 1 2 3 4]) [null 0 1 2 3 4 null null];
      withValue = expect.eq (padList {to = 8; emptyElem = "ok";} [0 1 2 3 4]) [0 1 2 3 4 "ok" "ok" "ok"];
    };

    sorted = {
      default = expect.eq (sorted [3 2 1]) [1 2 3];
      up = expect.eq (sorted.up [3 2 1]) [1 2 3];
      down.id = expect.eq (sorted.down [3 2 1]) [3 2 1];
      down.rev = expect.eq (sorted.down [1 2 3]) [3 2 1];
      by = expect.eq (sorted.by (a: b: if abs (a - b) == 1 then true else false) [1 4 5]) [1 5 4];
      on = expect.eq (sorted.on (a: a.x) [{x=2;} {x=1;}]) [{x=1;} {x=2;}];
    };
  };

}
