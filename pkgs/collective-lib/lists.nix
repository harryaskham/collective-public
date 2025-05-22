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
}
