{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

rec {
  # Partition attrs based on a predicate.
  # Follows lib.partition interface for lists.
  partitionAttrs = pred: xs:
    { right = lib.filterAttrs pred xs;
      wrong = lib.filterAttrs (k: v: !(pred k v)) xs;
    };

  # Merge attrs in a list deeply, allowing for modules to combine named dicts implicitly.
  recursiveMergeAttrsList = lib.foldl' lib.recursiveUpdate {};
}
