{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

rec {
  # Partition attrs based on a predicate.
  # Follows lib.partition interface for lists.
  partitionAttrs = pred: xs:
    { right = lib.filterAttrs pred xs;
      wrong = lib.filterAttrs (k: v: !(pred k v)) xs;
    };

  # Merge attrs in a list deeply, allowing for modules to combine named dicts implicitly.
  # Left-fold means that attrs coming later in the list take precedence over earlier ones.
  # This means lib.fold is overwritten by collective-lib.fold from a function to the fold
  # centraliser.
  recursiveMergeAttrsList = lib.foldl' lib.recursiveUpdate {};
}
