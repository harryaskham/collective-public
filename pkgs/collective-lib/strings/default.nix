{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

let
  strings = import ./strings.nix { inherit lib collective-lib; };
in strings // {
  _tests = import ./strings_test.nix { inherit lib collective-lib; };
}
