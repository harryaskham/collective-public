{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.attrs;

# Nicer interface to runtests
rec {
  expect = {
    failure = {
      success = false;
      value = false;
    };
  };

  log = rec {
    pretty = {
      results = tests: results_: 
      let
        nTests = length (attrNames tests);
        nFail = length results_;
        nPass = nTests - nFail;
      in ''
        Running ${toString nTests} tests

        ${toString nPass} of ${toString nTests} tests passed
        
        ${toString nFail} failed:
        ${concatStringsSep "\n" (map (t: "${t.name}: ${t.result}") results_)}
      '';
    };
  };

  suite = nestedTests: rec {
    inherit nestedTests;
    tests = flattenTests nestedTests;
    run =
      let t = log.pretty.results tests (runTests tests);
      in deepSeq t t;
  };
}
