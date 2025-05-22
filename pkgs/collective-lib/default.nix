{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, ... }:

let
  collective-lib = rec {
    tests = import ./tests.nix { inherit lib cutils; };
    functions = import ./functions.nix { inherit lib cutils; };
    log = import ./log.nix { inherit lib cutils; };
    lists = import ./lists.nix { inherit lib cutils; };
    attrs = import ./attrs.nix { inherit lib cutils; };
    strings = import ./strings.nix { inherit lib cutils; };
    errors = import ./errors.nix { inherit lib cutils; };
    types = import ./types.nix { inherit lib cutils; };
    clib = import ./clib.nix { inherit lib cutils; };
    colors = import ./colors.nix { inherit lib cutils; };
    disk = import ./disk.nix { inherit lib cutils; };
    display = import ./display.nix { inherit lib cutils; };
    fan = import ./fan.nix { inherit lib cutils; };
    font = import ./font.nix { inherit lib cutils; };
    binding = import ./binding.nix { inherit lib cutils; };
    wm = import ./wm.nix { inherit lib cutils; };
  };
  # TODO: Remove legacy alias.
  cutils = collective-lib;
in
  collective-lib // {
    # nix eval --impure --expr '(import ./pkgs/collective-lib {})._tests'
    _tests =
      collective-lib.tests.suite
        (lib.concatMapAttrs
          (name: module: if (module ? _tests) then module._tests.nestedTests else {})
          collective-lib);
 }
