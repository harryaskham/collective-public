{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lists;

# List utils and aliases.
rec {
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
}
