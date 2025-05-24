{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.attrs;
with cutils.functions;
with cutils.lists;
with cutils.strings;
with cutils.types;

# Nicer interface to runtests
let
  log = cutils.log;
in rec {

  Status = {
    Passed = "Status.Passed";
    Failed = "Status.Failed";
    Skipped = "Status.Skipped";
  };

  EvalStatus = {
    OK = "EvalStatus.OK";
    Error = "EvalStatus.Error";
    Skipped = "EvalStatus.Skipped";
  };

  Compare = rec {
    Fields = this:
      if typeOf this == "set"
      then mapAttrs (_: Fields) (this.get or this)
      else this;
    # Produce a version of the this-set with replaced lambdas, enabling deep comparison.
    NoLambdas = this:
      if typeOf this == "lambda" then { __lambda = true; }
      else if typeOf this == "set" then mapAttrs (_: NoLambdas) this
      else this;
    Print = this: log.print this;
  };

  # Detect a raw test object
  isTest = test: test ? expr && test ? expected;

  # Detect a wrapped test object
  isWrappedTest = test: test ? __mkTest;

  skip = tests:
    let setSkip = test: test // {skip = true;};
    in if isTest tests
       then setSkip tests
       else mapAttrsRecursiveCond (xs: !(isTest xs)) (_: setSkip) tests;

  expect = {
    failure = {
      success = false;
      value = false;
    };

    True = expr: {
      inherit expr;
      expected = true;
    };

    False = expr: {
      inherit expr;
      expected = false;
    };
  };

  # Is x the result of calling builtins.tryEval
  isTryEvalResult = x: isAttrs x && x ? success && x ? value;

  runOneTest = test: results_: rec {
    evalStatus =
      if test.skip then EvalStatus.Skipped
      else if isTryEvalResult results_ && !results_.success then EvalStatus.Error
      else EvalStatus.OK;
    results =
      if test.skip then null
      else if evalStatus == EvalStatus.Error then null
      else if isTryEvalResult results_ then results_.value
      else results_;
    status =
      if test.skip then Status.Skipped
      else if evalStatus == EvalStatus.Error then Status.Failed
      else if results == [] then Status.Passed
      else Status.Failed;
    actual =
      if test.skip then null
      else if evalStatus == EvalStatus.Error then "<evaluation error>"
      else if status == Status.Failed
      then
        let failedResult = assert (size results) <= 1; maybeHead results;
        in if failedResult == null then null else failedResult.result
      else null;
    msg = {
      ${Status.Skipped} = "SKIP: ${test.name}";
      ${Status.Passed} = "PASS: ${test.name}";
      ${Status.Failed} = joinLines [
        (indent.block ''
          FAIL: ${test.name}

          Expected:
            ${indent.here (indent.blocks [
                (log.print test.rawExpected)
                (optionalString (test.compare != null) (indent.lines [
                  "Comparing on:"
                  (log.print test.expected)
                ]))
            ])}

          Actual:
            ${indent.here (log.print actual)}
        '')
        ""
      ];
    }.${status};
  };

  # Run the given test as a singleton test suite, formatting its results.
  evalOneTest = evalFn: test:
    let tests = { ${test.name} = test; };
        result = evalFn (runTests tests);
    in strict (runOneTest test result);

  # Create a test attribute set adding extra functionality to a runTests-style
  # test of format { expr = ...; expected = ...; }
  # Optionally { skip = bool; } can be set too on the raw test.
  mkTest = testName: test:
    let test_ = rec {
      # Marker for detecting an augmented test.
      __mkTest = true;

      # The flattened test name.
      name = testName;

      # A custom comparator to use to evaluate equality, or null if none provided.
      compare = test.compare or null;

      # The raw expression to evaluate as defined in the test.
      rawExpr = test.expr;

      # The expression to evaluate, optionally under a comparison function.
      expr = if compare == null then rawExpr else compare rawExpr;

      # The raw expected expression to compare with as defined in the test.
      rawExpected = test.expected;

      # The expected expression to compare with, optionally under a comparison function.
      expected = if compare == null then rawExpected else compare rawExpected;

      # Iff true, skip this test and do not treat as failure.
      skip = test.skip or false;

      # Run the test under tryEval, treating eval failure as test failure
      run = evalOneTest builtins.tryEval test_;

      # Run the test propagating eval errors that mask real failures
      debug = evalOneTest id test_;
    };
    in test_;

  suite = nestedTests: rec {
    inherit nestedTests;
    tests = mapAttrs mkTest (flattenTests nestedTests);
    overOne = f: mapAttrs (testName: test: f { ${testName} = test; }) tests;
    runOne = overOne (run_ (test: test.run));
    debugOne = overOne (run_ (test: test.debug));
    run = run_ false (test: test.run) tests;
    runThenDebug = run_ true (test: test.run) tests;
    debug = run_ false (test: test.debug) tests;
    run_ = debugOnFailure: runner: tests:
      let
        results = strict (mapAttrsToList (_: runner) tests);
        byStatus = mapAttrs (statusK: statusV: filter (r: r.status == statusV) results) Status;
        counts =
          (mapAttrs (status: results: length results) byStatus)
          // { all = size tests;
               run = counts.all - counts.Skipped;
             };
        header = ''
          Running ${toString counts.all} tests
        '';
        verbs = mapAttrs (statusName: _: toLower statusName) Status;
        allCounts = {
          Skipped = counts.all;
          Passed = counts.run;
          Failed = counts.run;
        };
        headers = mapAttrs (statusName: _:
          optionalString (counts.${statusName} > 0) ''
            ${toString (counts.${statusName})} of ${toString allCounts.${statusName}} tests ${verbs.${statusName}}
          '') Status;
        msgs =
          mapAttrs
            (statusName: _: joinLines (map (result: result.msg) byStatus.${statusName}))
            Status;
        maybeDebugAfterRun =
          optionalString (debugOnFailure && counts.Failed > 0) debug;

      in indent.blocksSep "\n\n==========\n\n" [
        header
        headers.Skipped
        msgs.Skipped
        headers.Passed
        msgs.Passed
        headers.Failed
        msgs.Failed
        maybeDebugAfterRun
      ];
  };
}
