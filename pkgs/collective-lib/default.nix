{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
  traceLevel ? 0,
  enablePartialTrace ? false,
  enableVerboseTrace ? false,
  ... }:

let 
  mkCollectiveLib = base: 
    # Merge base in at the top level s.t. collective-lib.attrsets etc is available.
    base // rec {

      # Merge with another downstream collective-lib extension.
      extend = otherBase: mkCollectiveLib (lib.recursiveUpdate base otherBase);

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
      untyped = lib.recursiveUpdate lib base;

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
      attrsets = import ./attrsets.nix { inherit lib cutils; };
      binding = import ./binding.nix { inherit lib cutils; };
      clib = import ./clib.nix { inherit lib cutils; };
      collections = import ./collections.nix { inherit lib cutils; };
      colors = import ./colors.nix { inherit lib cutils; };
      disk = import ./disk.nix { inherit lib cutils; };
      dispatch = import ./dispatch.nix { inherit lib cutils; };
      display = import ./display.nix { inherit lib cutils; };
      errors = import ./errors.nix { inherit lib cutils; };
      fan = import ./fan.nix { inherit lib cutils; };
      font = import ./font.nix { inherit lib cutils; };
      functions = import ./functions.nix { inherit lib cutils; };
      lists = import ./lists.nix { inherit lib cutils; };
      log = import ./log.nix { inherit lib cutils traceLevel enablePartialTrace enableVerboseTrace; };
      strings = import ./strings.nix { inherit lib cutils; };
      tests = import ./tests.nix { inherit lib cutils; };
      typelib = import ./typelib.nix { inherit lib cutils; };
      wm = import ./wm.nix { inherit lib cutils; };
    };

in 
  mkCollectiveLib base