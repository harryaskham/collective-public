{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.attrs;
with cutils.strings;
with cutils.types;

# Nicer interface to runtests
let
  log = cutils.log;
in rec {
  TODO = "TODO: Enable this test";
  DISABLED = "Test is disabled";

  isTest = test: test ? expr && test ? expected;
  disable_ = expr: expected: tests:
    let doDisable = test: test // {
        inherit expr expected;
      };
    in
      if isTest tests then doDisable tests
      else mapAttrsRecursiveCond (xs: !(isTest xs)) (_: doDisable) tests;
  disable = disable_ TODO DISABLED;
  disablePass = disable_ TODO TODO;

  expect = {
    failure = {
      success = false;
      value = false;
    };
  };

  formatTestResults = test: tryEvalResults:
    let
      evalSuccess = tryEvalResults.success or false;
      results = if tryEvalResults ? success && tryEvalResults ? value then tryEvalResults.value else tryEvalResults;
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

          Expected:
            ${indent.here (log.print test.expected)}

          Actual:
            ${indent.here (if evalSuccess then log.print (head results).result else "<tryEval error>")}
        '';
      };

  runFormatted = tryEvalFn: test:
    let t = formatTestResults test (tryEvalFn (runTests { ${test.name} = test; }));
    in deepSeq t t;

  toTest = testName: test:
    let
      test_ = test // {
        name = testName;
      };
    in test_ // {
      run = runFormatted builtins.tryEval test_;
      debug = runFormatted id test_;
    };

  suite = nestedTests: rec {
    inherit nestedTests;
    tests = mapAttrs toTest (flattenTests nestedTests);
    overOne = f: mapAttrs (testName: test: f { ${testName} = test; }) tests;
    runOne = overOne (run_ (test: test.run));
    debugOne = overOne (run_ (test: test.debug));
    run = run_ (test: test.run) tests;
    debug = run_ (test: test.debug) tests;
    run_ = runner: tests:
      let
        results_ = mapAttrsToList (_: runner) tests;
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
