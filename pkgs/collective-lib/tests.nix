{ lib ? import <nixpkgs/lib>,
  collective-lib ? import ./. { inherit lib; },
  # Set to 0/false to disable diffing features.
  cltvPretty ? (!(builtins.elem (builtins.getEnv "CLTV_PRETTY") ["0" "false"])),
  enableStringDiff ? cltvPretty && (!(builtins.elem (builtins.getEnv "CLTV_ENABLE_STRING_DIFF") ["0" "false"])),
  prettyStringDiff ? cltvPretty && (!(builtins.elem (builtins.getEnv "CLTV_PRETTY_STRING_DIFF") ["0" "false"])),
  linewiseStringDiff ? cltvPretty && (!(builtins.elem (builtins.getEnv "CLTV_LINEWISE_STRING_DIFF") ["0" "false"])),
  elementwiseSetDiff ? cltvPretty && (!(builtins.elem (builtins.getEnv "CLTV_ELEMENTWISE_SET_DIFF") ["0" "false"])),
  ... }:

with collective-lib.attrsets;
with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.errors;
with collective-lib.functions;
with collective-lib.lists;
with collective-lib.log;
with collective-lib.script-utils;
with collective-lib.strings;
with collective-lib.syntax;
with lib;

# Nicer interface to runtests
let
  log = collective-lib.log;
  ansi = ansi-utils.ansi;
  atom = ansi.atom;

  msgSKIP = with ansi; style [fg.grey bold] "SKIP";
  msgPASS = with ansi; style [fg.green bold] "PASS";
  msgFAIL = with ansi; style [fg.red bold] "FAIL";
  msgERROR = with ansi; style [fg.magenta bold] "ERROR";

  inherit (collective-lib.typelib) cast isCastError hasToString isFunctionNotFunctor;
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
      else _throw_ ''
        Error occurred treating this as string:
          ${_pvh_ this}
      '';

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
                  if k == "__functor"
                  then { __functor__NoLambdas = "<__functor>"; }
                  else if k == "__toString"
                  then { __toString__NoLambdas = "<__toString>"; }
                  else if k == "__show"
                  then { __show__NoLambdas = "<__show>"; }
                  else { ${k} = go (d + 1) v; })
                this

            else if typeOf this == "list" then
              map (go (d + 1)) this

            else this;

      in go 0 this;

    PointerLambdas = this:
      let
        mkEq = this: {
          __this = this;
          __eq = pointerEqual this;
        };
        maxD = 10;
        go = d: this:
          if d >= maxD then { __PointerLambdas_maxDepth = true; }
          else
            if typeOf this == "lambda" then mkEq this

            else if typeOf this == "set" then
              concatMapAttrs
                (k: v:
                  if k == "__toString" && isFunction v
                  then { __toString__NoLambdas = mkEq v; }
                  else if k == "__show" && isFunction v
                  then { __show__NoLambdas = mkEq v; }
                  else { ${k} = go (d + 1) v; })
                this

            else if typeOf this == "list" then
              map (go (d + 1)) this

            else this;

      in go 0 this;

    # Compare test outputs only on their canonical stringified form.
    Print = this: log.vprint (log.pruneCycles this);
  };

  # Add a lambda to a value to make it uncomparable.
  uncomparable = a: {
    inherit a;
    __toString = self: "<uncomparable: ${_p_ self.a}>";
    __functor = self: {}: self.a;
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

  setSolo = dispatch {
    set = test: test // {solo = true;};
    lambda = expectFn: exhaust (test: test // {solo = true;}) expectFn;
  };

  solo = dispatch {
    set = mapAttrsRecursiveCond (x: !(isTest x)) (_: setSolo);
    lambda = setSolo;
  };

  expect = rec {
    failure = {
      success = false;
      value = false;
    };

    fail = msg: {
      expr = msg;
      expected = "<expect.fail>";
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

    pointerEq = eqWith pointerEqual;

    eqOnWith = compare: compareWith: expr: expected:
      with log.trace.msg (_b_ "eqOnWith: raw expr: ${_pvh_ expr}");
      with log.trace.msg (_b_ "eqOnWith: raw expected: ${_pvh_ expected}");
      with log.trace.msg (_b_ "eqOnWith: compare: ${_pvh_ (compare expr)}");
      with log.trace.msg (_b_ "eqOnWith: compare expected: ${_pvh_ (compare expected)}");
      True (compareWith (compare expr) (compare expected));

    printEq = eqOn Compare.Print;

    stringEq = eqOn Compare.String;

    isLambda = x: eqOn typeOf x anyLambda;

    fieldsEq = eqOn Compare.Fields;

    noLambdasEq = eqOn Compare.NoLambdas;

    pointerLambdasEq = eqOnWith Compare.PointerLambdas (a: b: emptyDiff (diffShortWithEq a b));

    valueEq = eqOn (this: 
      assert assertMsg (this ? value) (indent.block ''
        expect.valueEq: No value on this
          ${_pvh_ this}
        '');
      this.value
    );

    lazyFieldsEq = lazyEqOn Compare.Fields;

    anyLambda = arg:
      lib.traceSeq
        ''expect.anyLambda was called; should only be used in placeholder for Print/NoLambdas expectations.
          arg: ${_ph_ arg}
        ''
        {};

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

  traceTestSummary = testResult:
    with log.trace;
    let test = testResult.test;
        # TODO: unicode syms
        status = with Status; switch testResult.status {
          ${Skipped} = msgSKIP;
          ${Passed} = msgPASS;
          ${Failed} = msgFAIL;
        };
    in
    assert over (_b_ ''
      ${testResult.msg}
    '');
    testResult;


  runOneTest = test: results_:
    # disabled verbose per-test tracing during suite runs
    let testResult = rec {
      inherit test;
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
            else toString (with ansi; box { 
              header = atom.h1 "Actual";
              showBorder = false;
              body = [
                (if isTryEvalFailure result 
                 then style [fg.red bold] "<tryEval failure>"
                 else _p_ result)
              ] ++ (
                optionals ((status == Status.Failed) && !(isTryEvalFailure result)) [
                  (box {
                    header = style_ [fg.yellow bold] "Diff";
                    styles = [bg.black];
                    showBorder = false;
                    showDivider = false;
                    body = 
                      if cltvPretty then 
                        reprDiff_ {
                          # From env / module args
                          inherit enableStringDiff prettyStringDiff linewiseStringDiff elementwiseSetDiff;
                          maxDepth = 10;
                          aLabel = "expected";
                          bLabel = "actual";
                        }
                        test.expected
                        result
                      else
                        _p_ (diffShortWithEq test.expected result);
                    })

                ]);
              });
        };
       in
        if test.skip
          then mkActual msgSKIP null

        else if evalStatus == EvalStatus.Error
          then
            let errorResult =
                  # TODO: Redundant check
                  assert assertMsg (isTryEvalFailure results)
                    "Eval error handled without being a tryEval failure: ${log.vprintDUnsafe 3 results}";
                  results;
            in mkActual msgERROR errorResult

        else if status == Status.Failed
          then
            let failedResult = assert (size results) == 1; head results;
            in mkActual msgFAIL failedResult.result

        else
          mkActual msgPASS null;

    msg = {
      ${Status.Skipped} = "${msgSKIP}: ${atom.testName test.name}";
      ${Status.Passed} = "${msgPASS}: ${atom.testName test.name}";
      ${Status.Failed} = _b_ ''
        ${msgFAIL}: ${atom.testName test.name}

        ${_h_ (with ansi; box { 
          header = atom.h1 "Expected";
          borderStyles = [fg.brightblack];
          body = [
            (_b_ ''
              ${_pv_ test.rawExpected}
            '')
          ] ++ (
            optionals
              (test.compare != null) 
              [(with ansi; box { 
                header = style_ [fg.yellow bold] "Comparing on";
                styles = [bg.black];
                showBorder = false;
                showDivider = false;
                #styles = [fg.brightblack bg.black];
                body = _p_ test.expected;
              })]);
        })}
        ${_h_ actual}
      '';
    }.${status};
  };
  in 
    traceTestSummary testResult;

  # Run the given test as a singleton test suite, formatting its results.
  evalOneTest = evalFn: test:
    log.describe "while evaluating a test '${test.name}'" (
    let tests = { ${test.name} = test; };
        result = evalFn (runTests tests);
    in runOneTest test result
    );

  # Evaluating tests at their callsite makes for better errors.
  # Instead of e.g.
  # test = suite { a = { expr = ...; expected = ...; }; }
  # This lets us write e.g.
  # test = suite { a = _t { expr = ...; expected = ...; }; }
  # and get better errors.
  _t = x:
    if isAttrs x then mkTest "_t" x
    else if isFunction x then exhaust (mkTest "_t") x
    else throw "Invalid test argument (expected test attrset or function): ${log.vprintUnsafe x}";

  # Create a test attribute set adding extra functionality to a runTests-style
  # test of format { expr = ...; expected = ...; }
  # Optionally { skip = bool; } can be set too on the raw test.
  mkTest = testName: test:
    if isWrappedTest test then 
      if test.name == "_t" then test // { name = testName; }
      else test
    else
      assert assertMsg (isTest test) "mkTest: Invalid test ${testName}";
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
        expr = 
          if compare != null then compare rawExpr
          else rawExpr;

        # The raw expected expression to compare with as defined in the test.
        rawExpected = test.expected;

        # The expected expression to compare with, optionally under a comparison function.
        # Can't thunkify here - this is the "expected" literally passed to runTests.
        expected = 
          if compare != null then compare rawExpected
          else rawExpected;

        # Iff true, skip this test and do not treat as failure.
        skip = test.skip or false;

        # Iff any test has solo == true, run only tests with solo == true.
        solo = test.solo or false;

        # Run the test under tryEval, treating eval failure as test failure
        # Strict needed in order to catch eval errors
        run = evalOneTest (expr: builtins.tryEval expr) (test_ // { mode = "run"; });

        # Run the test propagating eval errors that mask real failures
        # Strict needed in order to catch eval errors
        debug = 
          # Still tryEval if the test expects error, otherwise we false-positive flag
          # these errors.
          let maybeTry = if isTryEvalFailure rawExpected then builtins.tryEval else id;
          in evalOneTest (expr: maybeTry expr) (test_ // { mode = "debug"; });
      };
    in test_;

  emptyTests = { emptyTestSuite = expect.True true; };
  emptySuite = suite emptyTests;

  # Collect the tests from a given set of modules / sets containining a _tests attribute into
  # a single test suite.
  mergeSuites = modules:
    suite
      (lib.concatMapAttrs
        (name: module: {
          ${name} = 
            if (module ? _tests) 
            then module._tests.nestedTests {}
            else emptyTests;
        })
        modules);

  # Extend a test suite with a new set of tests.
  # e.g.
  #   ...
  #   _tests = extendSuite (mergeSuites { inherit module1 module2; } {
  #     localTests = { ... };
  #   };
  # }
  extendSuite = prevSuite: newSuite:
    suite (lib.recursiveUpdate (prevSuite.nestedTests {}) (newSuite.nestedTests {}));

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
  suite = nestedTests_: rec {
    nestedTests = {}: nestedTests_;
    tests = {}:
      let flatTests = mapAttrs mkTest (flattenTests nestedTests_);
          soloTests = filterAttrs (_: t: t.solo) flatTests;
          nonSoloTests = filterAttrs (_: t: !t.solo) flatTests;
      in if soloTests == {}
         then flatTests
         else soloTests;
    # Run a thunk'd total f over one test.
    overOne = f: 
      mapAttrs
        (testName: test: 
          {}: f { ${testName} = test; })
        (tests {});
    runOne = overOne (run_ (test: test.run));
    debugOne = overOne (run_ (test: test.debug));
    run = run_ (test: test.run) (tests {});
    debug = run_ (test: test.debug) (tests {});
    runResults =
      {}:  # Thunk the tests to avoid strict execution.
      mapAttrsToList (_: test: test.run.test) (tests {});
    run_ = runner: tests:
      {}:  # Thunk the tests to avoid strict execution.
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
        failedTestNamesBlock = joinLines (map (result: "FAIL: ${result.test.name}") byStatus.Failed);

      in indent.blocksSep "\n\n${with ansi; style [bold fg.brightblack] "========================"}\n\n" [
        header
        headers.Skipped
        msgs.Skipped
        (indent.blocks [headers.Passed msgs.Passed])
        (indent.blocks [headers.Failed failedTestNamesBlock])
        msgs.Failed
      ];

  };

  removeTests = xs: removeAttrs xs ["_tests"];

  isTestableModule = module:
    lib.isFunction module
    && !(module ? __functor)
    && ((builtins.functionArgs module) ? testableModule);

  testModule = module: 
    (getTestsFromTestableModule module)._tests or emptySuite;

  getModuleFromTestableModule = module:
    if (isString module || isPath module) 
    then (getModuleFromTestableModule (import module))
    else if isTestableModule module
    then module { testableModule = lib.const; inherit lib collective-lib; }
    else module;

  getTestsFromTestableModule = module:
    if (isString module || isPath module)
    then (getTestsFromTestableModule (import module))
    else if isTestableModule module
    then module { testableModule = flip lib.const; inherit lib collective-lib; }
    else module;

  collectTestableModules = maybeTestableModules:
    (mapAttrs (_: getModuleFromTestableModule) maybeTestableModules) // {
      _tests = mergeModuleSuites (mapAttrs (_: getTestsFromTestableModule) maybeTestableModules);
    };

  evalTestableModule = module:
    (getTestsFromTestableModule module)._evalModule;

  mergeModuleSuites = modules:
    mergeSuites (mapAttrs (_: testModule) modules);

  # Expose module-specific merger separately.
  modules = {
    withMergedSuites = modules:
      modules // { _tests = mergeModuleSuites modules; };
  };

  withMergedSuites = modules:
    modules // { _tests = mergeSuites modules; };

  # Test the test lib
  __testf = x: x;
  _tests = suite {
    lambdaEquality =
      let
        a = { c = 3; d = { f = __testf; }; };
        b = { c = 3; d = { f = __testf; }; }; # Pointer-equal lambda
      in {
        regularEq = expect.eq a b;
        noLambdasEq = expect.noLambdasEq a b;
        #pointerEqDeep = expect.pointerEq a.d.f b.d.f;
        pointerLambdaTransform = 
          expect.noLambdasEq
            (Compare.PointerLambdas a)
            { c = 3; d = { f = { __this = a.d.f; __eq = x: x; }; }; };
        #pointerLambdaTransformEq = 
        #  expect.eq
        #    (diffShortWithEq
        #      (Compare.PointerLambdas a)
        #      (Compare.PointerLambdas b))
        #    {};
        #pointerLambdasEq = expect.pointerLambdasEq a b;
      };
  };
}
