{ lib, collective-lib, ... }:

with collective-lib.typed;
rec {
  # Write an expression to a file and return the path.
  file = exprStr: builtins.toFile "expr.nix" exprStr;

  # Exposed as eval.store in default.nix
  evalStore = exprStr: import (file exprStr);

  # Tested more thoroughly in default.nix
  _tests = with tests; suite {
    # smoke = expect.eq (evalStore "1") 1;
  };
}