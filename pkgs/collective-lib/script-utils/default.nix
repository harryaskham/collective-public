{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ../. { inherit lib; }, ... }:

let
  # TODO: Remove override once this is no longer very slow.
  overrideToShellValue = collective-lib.typelib.toShellValueUnsafe;

  modules = rec {
    ansi-utils = pkgs.callPackage ./ansi-utils.nix {
      inherit collective-lib;
      inherit overrideToShellValue;
    };
    log-utils = pkgs.callPackage ./log-utils.nix {
      inherit collective-lib ansi-utils;
      inherit overrideToShellValue;
      enableTypedTests = false;
    };
    main-utils = pkgs.callPackage ./main-utils.nix { inherit collective-lib ansi-utils log-utils; };
    options-utils = pkgs.callPackage ./options-utils.nix { inherit collective-lib ansi-utils log-utils; };
    usage-utils = pkgs.callPackage ./usage-utils.nix {
      inherit collective-lib ansi-utils log-utils;
      inherit overrideToShellValue;
    };
    script-types = pkgs.callPackage ./script-types.nix { inherit collective-lib ansi-utils log-utils usage-utils options-utils main-utils; };
    command-utils = pkgs.callPackage ./command-utils.nix { inherit collective-lib ansi-utils log-utils usage-utils script-types; };
  };
in
  modules // {
    _tests = collective-lib.tests.mergeSuites modules;
  }
