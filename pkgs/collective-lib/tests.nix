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

  formatTestResults = test: tryEvalResults:
    let
      evalSuccess = tryEvalResults.success or false;
      results = tryEvalResults.value;
      passed = results == [];
    in
      if passed then {
        passed = true;
        msg = "PASS: ${test.name}";
      }
      else {
        passed = false;
        msg = indent.block ''
          FAIL: ${test.name}

          Expected: ${log.print test.expected}

          Actual: ${if evalSuccess then log.print (head results).result else "<tryEval error>"}
        '';
      };

  runFormatted = test:
    let t = formatTestResults test (builtins.tryEval (runTests { ${test.name} = test; }));
    in deepSeq t t;

  toTest = testName: test:
    let
      test_ = test // {
        name = testName;
      };
    in test_ // {
      run = runFormatted test_;
    };

  suite = nestedTests: rec {
    inherit nestedTests;
    tests = mapAttrs toTest (flattenTests nestedTests);
    run =
      let
        results_ = mapAttrsToList (_: test: test.run) tests;
        results = deepSeq results_ results_;
        passed = filter (r: r.passed) results;
        failed = filter (r: !r.passed) results;
        nTests = length results;
        nPassed = length passed;
        nFailed = length failed;
        testBlocksSep = indent.blocksSep "\n\n==========\n\n";
        headerBlock = ''
          Running ${toString nTests} tests
        '';
        passedHeader = ''
          ${toString nPassed} of ${toString nTests} tests passed
        '';
        passedBlock =
          optionalString (nPassed > 0)
          (indent.lines
            (map (result: result.msg) passed));
        failedBlock =
          optionalString (nFailed > 0)
          (testBlocksSep
            (map (result: result.msg) failed));

      in testBlocksSep [
        headerBlock
        passedHeader
        passedBlock
        failedBlock
      ];
  };
}
