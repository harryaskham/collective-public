{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

# TODO:
# - Dynamic derivations should let eval-in-eval occur without requiring nested nix build:
#   https://fzakaria.com/2025/03/11/nix-dynamic-derivations-a-practical-application

# Evaluate a Nix expression contained within a string.
# Writes the string out to a file in the store by a derivation, and then
# imports that file.
let
  evalBuilder = pkgs.writeTextFile {
    name = "eval_builder.sh";
    executable = true;
    text = ''
      set -e
      unset PATH
      for p in $buildInputs; do
          export PATH=$p/bin''${PATH:+:}$PATH
      done

      mkdir -p $out
      echo "$exprStr" > $out/expr.nix
    '';
  };
in
with collective-lib.typed;
rec {
  # Default to eval
  __functor = self: evalStore;

  # Exposed as eval.store in default.nix
  evalStore = exprStr: rec {
    exprDrvName = "eval-${builtins.hashString "sha256" exprStr}";
    # Wrap as a zero-arg module so that __functor can just return 'import' and we can
    # use the eval result as a value.
    exprModuleStr = "{}: ${exprStr}";
    exprNixFile = exprStr: builtins.toFile "expr.nix" exprStr;
    exprDrv = 
      let nixFile = exprNixFile exprStr;
      in derivation {
        name = exprDrvName;
        system = builtins.currentSystem;
        builder = "${pkgs.bash}/bin/bash";
        args = [ evalBuilder ];
        inherit exprStr;
        buildInputs = with pkgs; [ coreutils ];
        outputs = [ "out" ];
      };
    exprFile = "${exprDrv}/expr.nix";
    __functor = self: import exprFile;
  };

  # Tested more thoroughly in default.nix
  _tests = with tests; suite {
    #smoke = expect.eq (evalStore "1") 1;
  };
}
