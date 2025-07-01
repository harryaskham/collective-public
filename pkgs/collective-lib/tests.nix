{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.attrsets;
with cutils.collections;
with cutils.dispatchlib;
with cutils.errors;
with cutils.functions;
with cutils.lists;
with cutils.strings;

# Nicer interface to runtests
let
  log = cutils.log;
  inherit (cutils.typelib) cast isCastError hasToString isFunctionNotFunctor;
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
    # Compare test outputs only on recursively extracted field values.
    Fields = this:
      let
        this_ =
          if this ? get
          then mapAttrs (_: Fields) (mapAttrs (_: maybeResolve) (removeAttrs this.get ["__Type"]))
          else this;
        this__ = deepConcatMap (k: v:
          if elem k ["__toString" "__show" "__functor"]
          then { "elided-${k}" = k; }
          else if isFunctionNotFunctor v then { ${k} = "<lambda>"; }
          else { ${k} = v; })
          this_;
      in
        this__;

    String = this:
      if hasToString this then toString this
      else throw (indent.block ''
        Error occurred treating this as string:
          ${indent.here (log.print this)}
      '');

    # Resolve thunks in the expr and expected.
    Resolve = this: tryStrict (resolveDeep this) (e: { Compare.Resolve = "Thunk resolution evaluation error"; }) ;

    # Produce a version of the this-set with replaced lambdas, enabling deep comparison.
    NoLambdas = this:
      let
        maxD = 10;
        go = d: this:
          if d >= maxD then { __NoLambdas_maxDepth = true; }
          else
            if typeOf this == "lambda" then
              { __lambda = true; }

            else if typeOf this == "set" then
              concatMapAttrs
                (k: v:
                  if k == "__toString"
                  then { __toString__NoLambdas = "<__toString>"; }
                  else if k == "__show"
                  then { __show__NoLambdas = "<__show>"; }
                  else { ${k} = go (d + 1) v; })
                this

            else if typeOf this == "list" then
              map (go (d + 1)) this

            else this;

      in go 0 this;

    # Compare test outputs only on their canonical stringified form.
    Print = this: log.print this;
  };

  # Detect a raw test object
  isTest = test: test ? expr && test ? expected;

  # Detect a wrapped test object
  isWrappedTest = test: test.__wrappedTest or false;

  skip = tests:
    let setSkip = test: test // {skip = true;};
    in if isTest tests
       then setSkip tests
       else mapAttrsRecursiveCond (xs: !(isTest xs)) (_: setSkip) tests;

  solo = tests:
    let setSolo = test: test // {solo = true;};
    in if isTest tests
       then setSolo tests
       else mapAttrsRecursiveCond (xs: !(isTest xs)) (_: setSolo) tests;

  expect = rec {
    failure = {
      success = false;
      value = false;
    };

    error = expr: { inherit expr; expected = failure; };
    lazyError = expr: {
      expr = NamedThunk "lazyError.expr" expr;
      expected = NamedThunk "lazyError.expected" failure;
      compare = Compare.Resolve;
    };

    lazyEq = expr: expected: {
      expr = NamedThunk "expr" expr;
      expected = NamedThunk "expected" expected;
      compare = Compare.Resolve;
    };

    lazyEqOn = compare_: expr: expected: lazyEq expr expected // {
      compare = compose compare_ Compare.Resolve;
    };

    eq = expr: expected: { inherit expr expected; };

    eqOn = compare: expr: expected: { inherit expr expected compare; };

    eqWith = compareWith: expr: expected:
      True (compareWith expr expected);

    printEq = eqOn Compare.Print;

    stringEq = eqOn Compare.String;

    fieldsEq = eqOn Compare.Fields;

    noLambdasEq = eqOn Compare.NoLambdas;

    valueEq = eqOn (this: 
      assert assertMsg (this ? getValue) (indent.block ''
        expect.valueEq: No getValue on this
          ${indent.here (log.print this)}
        '');
      this.getValue {}
    );

    lazyFieldsEq = lazyEqOn Compare.Fields;

    anyLambda = _: throw ''expect.anyLambda was called; should only be used in placeholder for Print/NoLambdas expectations.'';

    asserts = {
      ok = expr_: {
        expr = assert expr_; { asserts = "ok"; };
        expected = { asserts = "ok"; };
      };
      fail = expr_: {
        expr = assert expr_; { asserts = "ok"; };
        expected = failure;
      };
    };


    True = expr: eq expr true;
    lazyTrue = expr: lazyEq expr true;
    False = expr: eq expr false;
    lazyFalse = expr: lazyEq expr false;
  };

  runOneTest = test: results_:
    with (log.v 1).test test.name results_ ___;
    return rec {
      evalStatus =
        if test.skip then EvalStatus.Skipped
        else if isTryEvalFailure results_ then EvalStatus.Error
        else EvalStatus.OK;
      results =
        if test.skip then null
        else if evalStatus == EvalStatus.Error then
          # Surface the raw results of an eval failure so we can expect against them
          results_
        else if isTryEvalSuccess results_ then
          # Otherwise unwrap the successful result for comparison
          results_.value
        else if test.mode == "debug" then
          # If debug mode, we do not tryEval so the result is already unwrapped.
          results_
        else throw (indent.block ''
          Invalid results from non-debug test (neither of isTryEval{Success,Failure} matched):
            ${indent.here (log.print results_)}
          '');
      status =
        if test.skip then Status.Skipped
        else if evalStatus == EvalStatus.Error then
          if test.expected == expect.failure then Status.Passed
          else Status.Failed  # Failure due to tryEval
        else if results == [] then Status.Passed
        else Status.Failed;  # Failure due to mismatch
      actual =
        let mkActual = msg: result: {
              inherit status evalStatus result;
              __toString = _:
                if result == null then msg
                else indent.block ''
                  ${msg}: ${indent.here (log.print result)}
                  ${optionalString (status == Status.Failed) ''
                  Diff:
                    ${indent.here (log.vprintD 9 (diffShort test.expected result))}
                  ''}
                '';
            };
       in
        if test.skip
          then mkActual "SKIP" null

        else if evalStatus == EvalStatus.Error
          then
            let errorResult =
                  # TODO: Redundant check
                  assert assertMsg (isTryEvalFailure results)
                    "Eval error handled without being a tryEval failure: ${log.print results}";
                  results;
            in mkActual "ERROR" errorResult

        else if status == Status.Failed
          then
            let failedResult = assert (size results) == 1; head results;

            in mkActual "FAIL" failedResult.result

        else
          mkActual "PASS" null;

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
            ${indent.here (try (toString actual) (_: "<error>"))}
        '')
        ""
      ];
    }.${status};
  };

  # Run the given test as a singleton test suite, formatting its results.
  evalOneTest = evalFn: test:
    let tests = { ${test.name} = test; };
        result = evalFn (runTests tests);
    in runOneTest test result;

  # Evaluating tests at their callsite makes for better errors.
  # Instead of e.g.
  # test = suite { a = { expr = ...; expected = ...; }; }
  # This lets us write e.g.
  # test = suite { a = _t { expr = ...; expected = ...; }; }
  # and get better errors.
  _t = x:
    if isAttrs x then mkTest "_t" x
    else if isFunction x then exhaust (mkTest "_t") x
    else throw "Invalid test argument (expected test attrset or function): ${log.print x}";

  # Create a test attribute set adding extra functionality to a runTests-style
  # test of format { expr = ...; expected = ...; }
  # Optionally { skip = bool; } can be set too on the raw test.
  mkTest = testName: test:
    if isWrappedTest test then 
      if test.name == "_t" then test // { name = testName; }
      else test
    else
      let test_ = rec {
        # Marker for detecting an augmented test.
        __wrappedTest = true;

        # The flattened test name.
        name = testName;

        # A custom comparator to use to evaluate equality, or null if none provided.
        # Evaluated as a unary function of each side.
        compare = test.compare or null;

        # The raw expression to evaluate as defined in the test.
        rawExpr = test.expr;

        # The expression to evaluate, optionally under a comparison function.
        # Can't thunkify here - this is the "expr" literally passed to runTests.
        expr = if compare == null then rawExpr else compare rawExpr;

        # The raw expected expression to compare with as defined in the test.
        rawExpected = test.expected;

        # The expected expression to compare with, optionally under a comparison function.
        # Can't thunkify here - this is the "expected" literally passed to runTests.
        expected = if compare == null then rawExpected else compare rawExpected;

        # Iff true, skip this test and do not treat as failure.
        skip = test.skip or false;

        # Iff any test has solo == true, run only tests with solo == true.
        solo = test.solo or false;

        # Run the test under tryEval, treating eval failure as test failure
        run = evalOneTest (expr: builtins.tryEval (strict expr)) (test_ // { mode = "run"; });

        # Run the test propagating eval errors that mask real failures
        debug = evalOneTest (expr: strict expr) (test_ // { mode = "debug"; });
      };
    in test_;

  # Collect the tests from a given set of modules / sets containining a _tests attribute into
  # a single test suite.
  mergeSuites = modules:
    suite
      (lib.concatMapAttrs
        (name: module: {
          ${name} = 
            if (module ? _tests) 
            then module._tests.nestedTests
            else { emptyTestSuite = expect.True true; };
        })
        modules);

  # Create a test suite from a nested set of tests.
  # e.g.
  # {
  #   ...
  #   _tests = suite {
  #     topLevelTest = { expr = ...; expected = ...; };
  #     section1 = {
  #       testA = { expr = ...; expected = ...; };
  #       testB = { expr = ...; expected = ...; };
  #       ...
  #       subSection = {
  #         testE = { expr = ...; expected = ...; };
  #         ...
  #       };
  #     };
  #     section2 = { ... };
  #   };
  # }
  suite = nestedTests: rec {
    inherit nestedTests;
    tests =
      let flatTests = mapAttrs mkTest (flattenTests nestedTests);
          soloTests = filterAttrs (_: t: t.solo) flatTests;
          nonSoloTests = filterAttrs (_: t: !t.solo) flatTests;
      in if soloTests == {}
         then flatTests
         else soloTests;
    overOne = f: mapAttrs (testName: test: f { ${testName} = test; }) tests;
    runOne = overOne (run_ false (test: test.run));
    debugOne = overOne (run_ true (test: test.debug));
    run = run_ false (test: test.run) tests;
    runThenDebug = run_ true (test: test.run) tests;
    debug = run_ false (test: test.debug) tests;
    run_ = debugOnFailure: runner: tests:
      let
        results = mapAttrsToList (_: runner) tests;
        byStatus =
          mapAttrs (statusK: statusV: filter (r: r.status == statusV) results) Status;
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
