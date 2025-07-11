{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, ... }:
# DO NOT IMPORT COLLECTIVE-LIB HERE.

# Top-level lib rebinds.
# 'with' statements do not shadow earlier ones, so these will not fire off if
# 'with lib' is at the top of the file.
let
  abortTyped = fname: x:
    builtins.addErrorContext "|-> while aborting about use of unsafe function '${fname}'" (
    if x ? __special__isTypedAttrs
    then abort ''
      UNSAFE: ${fname} over typed value.
      
      (use unsafe* method variant instead if this is intentional)
    ''
    else x
    );

  abortTyped1 = fname: f: a: f (abortTyped fname a);
  abortTyped2 = fname: f: a: b: f a (abortTyped fname b);
  abortTyped3 = fname: f: a: b: c: f a b (abortTyped fname c);

  wrappedLibFns = lib.mapAttrs (name: abortFn: abortFn name lib.${name}) {
    attrNames = abortTyped1;
    attrValues = abortTyped1;
    mapAttrs = abortTyped2;
    concatMapAttrs = abortTyped2;
    mapAttrsToList = abortTyped2;
  };
in 
  # Add the rebinds at the top level
  wrappedLibFns // {
    # Expose the originals under Unsafe.
    Unsafe = lib.mapAttrs (name: _: lib.${name}) wrappedLibFns;
  }