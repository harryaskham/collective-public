{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, stdenv ? pkgs.stdenv, ... }:

# TODO:
# - Dynamic derivations should let eval-in-eval occur without requiring nested nix build:
#   https://fzakaria.com/2025/03/11/nix-dynamic-derivations-a-practical-application

# Evaluate a Nix expression contained within a string.
# Writes the string out to a file in the store by a derivation, and then
# imports that file.
let
  tests = collective-lib.tests;
  typed = collective-lib.typed;
in
rec {
  evalDrvName = exprStr: "eval-${builtins.hashString "sha256" exprStr}";

  exprNixFile = exprStr: builtins.toFile "expr.nix" exprStr;

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

  evalDrv = exprStr: 
    let nixFile = exprNixFile exprStr;
    in derivation {
      name = evalDrvName exprStr;
      system = builtins.currentSystem;
      builder = "${pkgs.bash}/bin/bash";
      args = [ evalBuilder ];
      inherit exprStr;
      buildInputs = with pkgs; [ coreutils ];
      outputs = [ "out" ];
    };

  # Create a structured eval object.
  eval_ = exprStr: rec {
    inherit exprStr;
    drv = evalDrv exprStr;
    exprFile = "${drv}/expr.nix";
    __import = {}: import exprFile;
  };

  eval = exprStr: (eval_ exprStr).__import {};

  # Use eval to create a callable lambda function from a string.
  txtfn = functionText: rec {
    inherit functionText;
    __fn = eval functionText;
    __functor = self: arg: self.__fn arg;
    # Create a new function with a text transformation f applied.
    mapText = f: txtfn (f functionText);
  };

  # For just 'collective-lib.eval "1 + 1"'
  __functor = self: eval;

  # <nix>eval._tests.run {}</nix>
  _tests = with tests; suite {
    eval = {
      const = expect.eq (eval "1") 1;
      add = expect.eq (eval "1 + 1") 2;
    };
    txtfn =
      let fTxt = "a: b: 3 * a + b";
          f = txtfn fTxt;
          g = f.mapText (t: "z: ${t} + z");
      in {
        exprStr = expect.eq f.functionText fTxt;
        call = expect.eq (f 3 1) 10;
        fmap = expect.eq (g 5 3 1) 15;
      };
  };
}
