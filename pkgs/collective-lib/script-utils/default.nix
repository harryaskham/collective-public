{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ../. { inherit lib; }, ... }:

let
  args = {
    inherit pkgs lib collective-lib script-utils;
    # TODO: Remove override once this is no longer very slow.
    overrideToShellValue = collective-lib.typelib.toShellValueUnsafe;
  };

  script-utils = rec {
    ansi-utils = import ./ansi-utils.nix args;
    log-utils = import ./log-utils.nix args // { enableTypedTests = false; };
    main-utils = import ./main-utils.nix args;
    options-utils = import ./options-utils.nix args;
    usage-utils = import ./usage-utils.nix args;
    script-types = import ./script-types.nix args;
    command-utils = import ./command-utils.nix args;
  };
in
  script-utils // {
    _tests = collective-lib.tests.mergeSuites script-utils;
  }
