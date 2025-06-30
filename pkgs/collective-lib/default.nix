{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
  traceLevel ? 0,
  enablePartialTrace ? false,
  enableVerboseTrace ? false,
  ... }:

let
  # Merge all modules in base into a single module.
  #
  # Includes the original base so that we have everything at top level, and can
  # still also access / override e.g. lib.strings via e.g. collective-lib.strings.
  #
  # Some modules are not intended to be used individually i.e. log
  # which is used as log.* always.
  #
  # This will also combine e.g. base.dispatch with base.typelib.dispatch
  # and expose the result as mergedBase.dispatch.
  # We cannot then in general have a top-level value in a module with the same
  # name as a module, unless we are happy to have it merged.

  # Modules and top-level attributes in modules that should not be merged.
  # These are only exposed either as collective-lib.log or e.g. collective-lib.base.typelib._tests.
  # In the case of _tests, a new suite is created at collective-lib._tests.
  mergeBase = base:
    let
      unmergeableAttrNames = [ "_tests" ];
      splitModule = base.attrsets.partitionAttrs (k: _: lib.elem k unmergeableAttrNames);

      unmergeableModuleNames = [ "log" ];
      splitModules = base.attrsets.partitionAttrs (k: _: lib.elem k unmergeableModuleNames);

      moduleMergeable = _: module: (splitModule module).wrong;

      baseSplit = splitModules base;
      baseMergeable = lib.mapAttrs moduleMergeable baseSplit.wrong;
    in
      base 
      // (base.attrsets.recursiveMergeAttrsList (lib.attrValues baseMergeable));

  mkCollectiveLib = base:
    let baseMerged = mergeBase base; in

    # Merge base in at the top level s.t. collective-lib.attrsets etc is available.
    baseMerged   # e.g. Overwrites dispatch with the merged version, adds toplevel functions like Int.
    // rec {

      # Merge with another downstream collective-lib extension.
      # Merges deeply s.t. modules that share names are themselves merged.
      extend = otherBase: mkCollectiveLib (lib.recursiveUpdate base otherBase);

      # Keep a reference to the original base so that we can still access it unmerged.
      inherit base;

      # Keep a reference to the merged base so that we can inspect only the merged state.
      inherit baseMerged;

      # Produce a new version of the collective-lib with 'lib' merged in.
      # Can be used as a drop-in replacement for 'lib' in modules that do not rely on the type system.
      # Does not include the type system and its overrides of functions like isType, isNull, etc
      # at the top level.
      #
      # Merges with 'lib' to expose additional library functions on:
      # 
      # - lib.lists
      # - lib.attrsets
      # - lib.strings
      #
      # Does not include _tests; intended only for use as a library.
      untyped = lib.recursiveUpdate lib baseMerged;

      # Produce a new version of the collective-lib with 'lib' merged in and the type system enabled and exposed.
      # Can be used as a drop-in replacement for 'lib' in modules that make use of the type system.
      #
      # Merges with 'lib' to expose additional library functions on:
      # 
      # - lib.lists
      # - lib.attrsets
      # - lib.strings
      #
      # Does not include _tests; intended only for use as a library.
      #
      # lib.types is not affected; the type system is implemented as lib.typelib.
      # However functions like isType, isNull are overridden by typelib.lib and exposed
      # at the top level. If needed, the originals can be accessed via builtins.isNull, lib.isType, etc
      typed = lib.recursiveUpdate untyped base.typelib.library;

      # Merged tests from all modules.
      _tests = base.tests.mergeSuites base;

      # Merged tests from all modules, excluding typelib.
      _testsUntyped = base.tests.mergeSuites (removeAttrs base [ "typelib" ]);
    };

  base = 
    # TODO: Remove legacy alias.
    let cutils = base; in {
      attrsets = import ./attrsets.nix { inherit lib collective-lib cutils; };
      binding = import ./binding.nix { inherit lib collective-lib cutils; };
      clib = import ./clib.nix { inherit lib collective-lib cutils; };
      collections = import ./collections.nix { inherit lib collective-lib cutils; };
      colors = import ./colors.nix { inherit lib collective-lib cutils; };
      disk = import ./disk.nix { inherit lib collective-lib cutils; };
      dispatchlib = import ./dispatchlib.nix { inherit lib collective-lib cutils; };
      display = import ./display.nix { inherit lib collective-lib cutils; };
      errors = import ./errors.nix { inherit lib collective-lib cutils; };
      fan = import ./fan.nix { inherit lib collective-lib cutils; };
      font = import ./font.nix { inherit lib collective-lib cutils; };
      functions = import ./functions.nix { inherit lib collective-lib cutils; };
      lists = import ./lists.nix { inherit lib collective-lib cutils; };
      log = import ./log.nix { inherit lib collective-lib cutils traceLevel enablePartialTrace enableVerboseTrace; };
      strings = import ./strings.nix { inherit lib collective-lib cutils; };
      tests = import ./tests.nix { inherit lib collective-lib cutils; };
      typelib = import ./typelib.nix { inherit lib collective-lib cutils; };
      wm = import ./wm.nix { inherit lib collective-lib cutils; };
    };

  collective-lib = mkCollectiveLib base;

  __libCollisions =
    (# <nix>
    with collective-lib;
    {
      base = lib.attrNames (lib.intersectAttrs baseMerged lib);
      lists = lib.attrNames (lib.intersectAttrs lists lib.lists);
      attrsets = lib.attrNames (lib.intersectAttrs attrsets lib.attrsets);
      strings = lib.attrNames (lib.intersectAttrs attrsets lib.strings);
    })
    # </nix>
    ;

  __baseMergedDiffs =
    (# <nix>
    with collective-lib;
    lib.mapAttrs (name: module: attrsets.diffShort module baseMerged.${name}) base
    )# </nix>
    ;

  in collective-lib
