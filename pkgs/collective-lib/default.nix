{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
  withTests ? true,
  traceOpts ? null,
  nix-parsec ? (
    let
      version = "v0.1.0";
      sha256 = "sha256-HOsko3wE4wt6+TdfPdaQj3A3UJ8AB9vHmHuDfyBlosY=";
    in
      import (pkgs.fetchFromGitHub {
        owner = "nprindle";
        repo = "nix-parsec";
        rev = version;
        sha256 = "sha256-JVgNZSj3ViroWymo0ydkIEVnlRJdXRV+D0ig0OiuZ7o=";
      })
  ),
  ... 
}:

let
  # Functions required for building the collective-lib.
  # Can't put this inside e.g. lists/attrsets otherwise it gets merged into itself here.
  modulelib = import ./modulelib.nix { inherit lib; };

  # Merge all modules in base into a single module.
  #
  # Individual modules should not use the merged version, instead explicitly calling into
  # the specific module's partial set as needed.
  # e.g. attrsets.fold.solos rather than fold.solos, lists.fold.left rather than fold.left
  # When used merged, fold.solos and fold.left are both exposed.
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
  #
  # Modules and top-level attributes in modules that should not be merged.
  # These are only exposed either as collective-lib.log or e.g. collective-lib.base.typelib._tests.
  # In the case of _tests, a new suite is created at collective-lib._tests.
  mergeBase = baseModules:
    let
      unmergeableAttrNames = [ "_tests" ];
      splitModule = modulelib.partitionAttrs (k: _: lib.elem k unmergeableAttrNames);

      # Modules intended to be merged together deeply.
      mergeableModuleNames = [
        "attrsets"
        "clib"
        "collections"
        "debuglib"
        "dispatchlib"
        "functions"
        "lists"
        "modulelib"
        "rebinds"
        "strings"
        "syntax"
        "typelib"
      ];

      splitModules = modulelib.partitionAttrs (k: _: lib.elem k mergeableModuleNames);

      moduleMergeable = _: module: (splitModule module).wrong;

      baseModulesSplit = splitModules baseModules;
      baseModulesMergeable = lib.mapAttrs moduleMergeable baseModulesSplit.right;
    in
      modulelib.recursiveMergeAttrsList 
        [ baseModules 
          (modulelib.recursiveMergeAttrsList (lib.attrValues baseModulesMergeable))];

  mkCollectiveLib = withTests: baseModules:
    let baseMerged = mergeBase baseModules;
    in baseMerged // rec {
      # Merge with another downstream collective-lib extension.
      # Merges deeply s.t. modules that share names are themselves merged.
      # i.e. merge log.* with log.shell.*
      extend = otherBaseModules: mkCollectiveLib withTests (
        lib.recursiveUpdate baseModules otherBaseModules);

      # Keep a reference to the original base so that we can still access it unmerged.
      base = baseModules;

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
      typed = lib.recursiveUpdate untyped baseModules.typelib.library;
    } // lib.optionalAttrs withTests {
      # Merged tests from all modules.
      _tests = baseModules.tests.mergeSuites baseModules;

      # Merged tests from all modules, excluding typelib.
      _testsUntyped = baseModules.tests.mergeSuites (removeAttrs baseModules [ "typelib" ]);
    };

  baseModules = 
    let 
      # Break circular dependency by using lazy evaluation
      argsWithoutCollectiveLib = { inherit pkgs lib; };
      args = argsWithoutCollectiveLib // { collective-lib = collective-lib; };
      # Import modules that parser depends on first
      eval = import ./eval argsWithoutCollectiveLib;
      typelib = import ./typelib.nix argsWithoutCollectiveLib;
    in
    {
      attrsets = import ./attrsets.nix argsWithoutCollectiveLib;
      binding = import ./binding.nix argsWithoutCollectiveLib;
      clib = import ./clib.nix argsWithoutCollectiveLib;
      collections = import ./collections.nix argsWithoutCollectiveLib;
      colors = import ./colors.nix argsWithoutCollectiveLib;
      data = import ./data.nix argsWithoutCollectiveLib;
      debuglib = import ./debuglib.nix argsWithoutCollectiveLib;
      disk = import ./disk.nix argsWithoutCollectiveLib;
      dispatchlib = import ./dispatchlib.nix argsWithoutCollectiveLib;
      display = import ./display.nix argsWithoutCollectiveLib;
      errors = import ./errors.nix argsWithoutCollectiveLib;
      inherit eval;
      fan = import ./fan.nix argsWithoutCollectiveLib;
      font = import ./font.nix argsWithoutCollectiveLib;
      functions = import ./functions.nix argsWithoutCollectiveLib;
      lists = import ./lists.nix argsWithoutCollectiveLib;
      log = import ./log.nix (argsWithoutCollectiveLib // { inherit traceOpts; });
      inherit modulelib;
      parser = import ./parser (argsWithoutCollectiveLib // { inherit nix-parsec eval; typed = typelib.library; });
      rebinds = import ./rebinds.nix argsWithoutCollectiveLib;
      script-utils = import ./script-utils argsWithoutCollectiveLib;
      strings = import ./strings.nix argsWithoutCollectiveLib;
      syntax = import ./syntax.nix argsWithoutCollectiveLib;
      tests = import ./tests.nix argsWithoutCollectiveLib;
      inherit typelib;
      wm = import ./wm.nix argsWithoutCollectiveLib;
    };

  __libCollisions =
    (# <nix>
    with collective-lib;
    {
      base = lib.attrNames (lib.intersectAttrs baseMerged lib);
      lists = lib.attrNames (lib.intersectAttrs base.lists lib.lists);
      attrsets = lib.attrNames (lib.intersectAttrs base.attrsets lib.attrsets);
      strings = lib.attrNames (lib.intersectAttrs base.strings lib.strings);
    })
    # </nix>
    ;

  __baseMergedDiffs =
    (# <nix>
    with collective-lib;
    lib.mapAttrs (name: module: attrsets.diffShort module baseMerged.${name}) base
    )# </nix>
    ;

  collective-lib = mkCollectiveLib withTests baseModules;

  #collective-lib-from-drv = import "${collective-lib-drv}" { inherit pkgs lib; };

in

  #collective-lib-from-drv
  collective-lib
