{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.attrs;
with cutils.strings;
with cutils.types;

# Nicer interface to runtests
let
  log = cutils.log;
in rec {
  expect = {
    failure = {
      success = false;
      value = false;
    };
  };

  formatTestResults = tests: results_:
    let
      nTests = length (attrNames tests);
      nFail = length results_;
      nPass = nTests - nFail;
    in ''
      Running ${toString nTests} tests

      ${toString nPass} of ${toString nTests} tests passed

      ${toString nFail} failed:
      ${joinLines
          (map
            (t: joinLines [
              t.name
              "Expected: ${log.print tests.${t.name}.expected}"
              "Actual: ${log.print t.result}"
            ])
            results_
          )}
    '';

  suite = nestedTests: rec {
    inherit nestedTests;
    tests = flattenTests nestedTests;
    run =
      let t = formatTestResults tests (runTests tests);
      in deepSeq t t;
  };
}
