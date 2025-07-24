{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

let
  strings = import ./strings.nix { inherit pkgs lib collective-lib; };
in strings // {
  _tests = import ./strings_test.nix { inherit pkgs lib collective-lib; };
}
