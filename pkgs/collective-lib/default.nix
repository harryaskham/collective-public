{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
  traceLevel ? 0,
  enablePartialTrace ? false,
  enableVerboseTrace ? false,
  ... }:

let 
  self = {
    # Produce a version of the collective-lib without 'lib' merged in.
    # Can be used when only individual library functions are needed.
    base = 
      let cutils = self.base;
      in {
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
    untyped = lib.recursiveUpdate lib self.base;

    # Produce a new version of the collective-lib with 'lib' merged in and the type system enabled and exposed.
    # Can be used as a drop-in replacement for 'lib' in modules that make use of the type system.
    #
    # Merges with 'lib' to expose additional library functions on:
    # 
    # - lib.lists
    # - lib.attrsets
    # - lib.strings
    #
    # lib.types is not affected; the type system is implemented as lib.typelib.
    # However functions like isType, isNull are overridden by typelib.lib and exposed
    # at the top level. If needed, the originals can be accessed via builtins.isNull, lib.isType, etc
    typed = lib.recursiveUpdate self.untyped self.base.typelib.library;

    # Legacy base alias used throughout
    cutils = self.base;

    # Merged tests from all modules.
    _tests = self.base.tests.mergeSuites self.base;

    # Merged tests from all modules, excluding typelib.
    _testsUntyped = self.base.tests.mergeSuites (removeAttrs self.base [ "typelib" ]);
  };
in 
  self // self.base