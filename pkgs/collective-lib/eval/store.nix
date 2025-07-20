{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with collective-lib.typed;
rec {
  # Write an expression to a file and return the path.
  evalStoreFile = exprStr: builtins.toFile "expr.nix" exprStr;

  # Exposed as eval.store in default.nix
  evalStore = exprStr: import (evalStoreFile exprStr);

  # Tested more thoroughly in default.nix
  _tests = with tests; suite {
    smoke = expect.eq (evalStore "1") 1;
  };
}
