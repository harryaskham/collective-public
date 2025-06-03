{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lists;
with cutils.functions;

# List utils and aliases.
rec {
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

  _tests =
    cutils.tests.suite {
      lists = {
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
      };
    };
}
