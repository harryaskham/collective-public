{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with collective-lib.typed;
rec {
  # Exposed as eval.store in default.nix
  evalStore = exprStr: rec {
    exprNixFile = builtins.toFile "expr.nix" exprStr;
    value = import "${exprNixFile}";
  }.value;

  # Tested more thoroughly in default.nix
  _tests = with tests; suite {
    smoke = expect.eq (evalStore "1") 1;
  };
}
