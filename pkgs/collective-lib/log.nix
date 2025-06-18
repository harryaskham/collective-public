{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib, cutils ? import ./. { inherit lib; },
  # Default trace level, 0 = all (log.trace), 1 = vtrace, 2+ = custom
  # Null to disable.
  traceLevel ? 0,
  # If true, trace not only on start and return, but also on accruing all intermediate values.
  # Duplicative but enables tracing of calls that never reach their return value.
  enablePartialTrace ? false,
  # If true, traces via vprint instead of print.
  enableVerboseTrace ? false,
  ...
}:

with lib;
with cutils.attrs;
with cutils.dispatch;
with cutils.lists;
with cutils.functions;
with cutils.strings;

# Printing/logging utilities
let
  log = rec {

    mkPrintArgs = {
      printStrictly = false;
      # If true, do not respect __toString
      ignoreToString = false;
      # If true, do not respect __show
      ignoreShow = false;
      # Default here to leave indentation markers in until the top level call
      formatBlock = trimNewlines;
      formatLines = indent.linesSep "\n";
      # If true, parens and braces on same line as first item.
      compact = true;
      # Tracks the current depth of printing, used to limit recursion depth.
      depth = 0;
      # Maximum depth to print before truncating.
      maxDepth = 20;
      # A set of attribute names to replace, with functions from the value
      # to replace to the string value to display instead.
      replaceAttrs = {};
    };
    descend = args: args // { depth = args.depth + 1; };

    compactBlock = args: braceL: braceR: px:
      with args;
      formatBlock (
        let pxLines = splitLines (formatBlock (joinLines px));
        in ''
          ${braceL} ${indent.here (formatLines pxLines)} ${braceR}
        '');

    printAttrs_ = args: x:
      with args;
      if depth >= maxDepth then "..."
      else if x == {} then "{}"
      else
        let maybePrintValue = k: v:
              if replaceAttrs ? ${k}
              then let f = replaceAttrs.${k}; in f v
              else print_ (descend args) v;
        in formatBlock (
          let px = mapAttrsToList (k: v: "${k} = ${maybePrintValue k v};") x;
              pxLine = "{ ${head px} }";
          in
            if length px == 1 && lineCount pxLine == 1 then pxLine
            else if compact then (compactBlock args "{" "}" px)
            else formatLines [
              "{"
              "  ${indent.here (formatLines px)}"
              "}"
            ]);

    printAttrs = xs: indent.block (printAttrs_ mkPrintArgs xs);

    printList_ = args: x:
      with args;
      if depth >= maxDepth then "..."
      else if x == [] then "[]"
      else
        let px = map (print_ (descend args)) x;
            pxLine = formatBlock "[ ${formatBlock (joinLines px)} ]";
        in
          if lineCount pxLine <= 1 then pxLine
          else if compact then formatBlock (compactBlock args "[" "]" px)
          else formatBlock (formatLines [
            "["
            "  ${indent.here (formatLines px)}"
            "]"
          ]);

    printList = xs: indent.block (printList_ mkPrintArgs xs);

    # Add parens around a string only if it contains whitespace.
    maybeParen = x: if wordCount x <= 1 then x else "(${x})";

    # Convert a value of any type to a string, supporting the types module's Type values.
    print_ = args: x_:
      with args;
      let
        x = if printStrictly then strict x_ else x_;
        block =
          if depth >= maxDepth then "..."
          else if hasShow x && !ignoreShow then
            show x
          else if (x ? __toString) && !ignoreToString then
            toString x
          else {
            null = "null";
            path = toString x;
            string = ''"${x}"'';
            int = ''${builtins.toJSON x}'';
            float = ''${builtins.toJSON x}'';
            lambda = "<lambda>";
            list = formatBlock (printList_ args x);
            set = formatBlock (printAttrs_ args x);
            bool = boolToString x;
          }.${typeOf x};
      in
        block;

    # print x using a function of the default print options
    printWith = f: x: indent.block (print_ (f mkPrintArgs) x);
    print = printWith id;
    vprintD = n: printWith (args: args // prints.using.raw // prints.using.depth n);
    prints = rec {
      ___ = cutils.functions.___;
      put = x: Variadic.mkSetFromThen mkPrintArgs (args: print_ args x);
      block = x: Variadic.compose indent.block (put x);
      here = x: Variadic.compose indent.here (put x);
      putD = n: x: put x (using.depth n);
      using = {
        raw = { ignoreToString = true; ignoreShow = true; };
        strict = { printStrictly = true; };
        lazy = { printStrictly = false; };
        line = { formatLines = indent.linesSep " "; };
        depth = n: { maxDepth = n; };
        mask = names: {
          replaceAttrs =
            mergeAttrsList (map (name: v: "<masked: ${name}>") names);
        };
      };
      _raw = using.raw;
      _strict = using.strict;
      _lazy = using.lazy;
      _line = using.line;
      _depth = using.depth;
      _mask = using.mask;
    };
    vprint = x: with prints; put x using.raw ___;

    # Either print to string using __show if it exists, or return an already-string
    hasShow = x: (x ? __show) && (isFunction x.__show);
    show = x:
      let showX =
            if hasShow x
              then x.__show x
              else x;
          showXHandlingPartial =
            if isAttrs x && isFunction showX
              then log.printAttrs x
              else showX;
      in
        dispatchDef print {
          string = id;
        } showXHandlingPartial;

    mkTrace = level:
      let
        enableTrace = traceLevel != null && level <= traceLevel;
        traceFn = if enableTrace then builtins.trace else (_: id);
        printFn =
          if enableVerboseTrace
          then printWith (args: args // prints.using.depth (3 + 3 * traceLevel))
          else log.show;
        self = rec {
        # log.trace.show [ 456 { a = 2; }] 123
        # -> trace: [ 456 { a = 2; }]
        # 123
        # show = x: a: traceFn "\n\n[log.trace.show]\n${log.vprintD 6 x}\n" a;
        show = x: a: traceFn "\n\n[log.trace(${toString level}).show]\n${printFn x}\n" a;

        # log.trace.showId 123
        # -> trace: 123
        # 123
        showId = x: self.show x x;
        msg = x: over (showId x);

        toTraceF = fs: {
          list =
            let fs_ = map (f: if isFunction f then f else show f) fs;
                f = foldl1 compose fs_;
            in f;
        }.${typeOf fs} or (show fs);

        # For use with assert for low-paren tracing
        # e.g.
        # with log.trace; assert over ["msg" {a = 123;}]; expr
        # or
        # with log.trace; assert over {a = 123;}; expr
        #
        # Can't access expr for e.g. log.trace.id
        # Can mix partially applied traces with raw values, which will be printed.
        over = fs:
          # Short-circuit if trace disabled to save traceF construction.
          if enableTrace
            then (toTraceF fs) true
            else true;

        # Trace over the given fs only if intermediate tracing is enabled.
        overPartial = fs:
          if enablePartialTrace then over fs else true;

        # e.g. with log.trace; assert over (tagged "START_CALL" xs);
        tagged = tag: dispatch {
          set = xs: xs // { _ = tag; };
          list = xs: { _ = tag; __ = xs; };
        };

        mkSurround = tag: fs: [ "\n\n" "START:${tag}" (toTraceF fs) "END:${tag}" "\n\n" ];
        surround = tag: fs: over mkSurround tag fs;

        # e.g.
        # with log.trace; assert call "callName" arg1 arg2
        buildInitialLogState = initEvent: {
          events = [initEvent];
          safe = false;
        };

        buildCall = callName:
          Variadic.mkListThen
            (l: buildInitialLogState {call = [{name = callName;} {args = l;}]; });

        buildMethodCall = this: methodName:
          Variadic.mkListThen
            (l: buildInitialLogState {method = [{name = methodName;} { this = "unsafe:this"; } {args = l;}]; });

        buildAttrs = attrsName:
          Variadic.mkListThen
            (l: buildInitialLogState {attrs = [{name = attrsName;}] ++ (optionals (size l > 0) [{extra = l;}]); });

        buildTest = testName:
          Variadic.mkListThen
            (l: buildInitialLogState {test = [{name = testName;}] ++ (optionals (size l > 0) [{args = l;}]); });

        traceCall = callName:
          Variadic.compose
            (xs: over xs)
            (buildCall callName);

        traceMethodCall = this: methodName:
          Variadic.compose
            (xs: over xs)
            (buildMethodCall this methodName);

        # e.g.
        # with log.vtrace.returning "newSubType" This name args ___;
        # ...
        # return expr;
        #
        # or
        #
        # let
        #   return = (log.vtrace.returning "newSubType" This name args ___).return;
        # in ...
        #   return x;
        #   ...
        #   return y;
        #
        # Note even if tracing is disabled, these are still constructed, because
        # the function logic then depends upon the constructs here i.e. with lets, return.
        # We just don't do any print-tracing.
        mkEventGroup = groupType: buildGroup_: groupName:
          let
            groupBuilder = buildGroup_ groupName;
            mkGroupClosure = logState: logState // rec {
              # Store an arbitrary list of events and rebuild the closure.
              # Also perform an assertion over the updated state, which can perform logging.
              __withEventsAssert = events: assertion:
                let
                  logState' = mkGroupClosure (logState // {
                    events = logState.events ++ events;
                  });
                in
                  assert assertion logState';
                  logState';

              # Store an arbitrary list of events and rebuild the closure.
              # Also perform an assertion over the updated state, which can perform logging.
              # Returns a modified state, under application of f (i.e. for storing any extra 'with' context)
              # which has its bindings rebuilt.
              __withEventsAssertWith = events: assertion: f:
                let logState = __withEventsAssert events assertion;
                in mkGroupClosure (f logState);

              # Store an arbitrary list of events and rebuild the closure.
              # Also perform an assertion over the updated state, which can perform logging.
              # Discard the state in favor of returning x
              __withEventsAssertReturning = events: assertion: x:
                seq (__withEventsAssert events assertion) x;

              # Store an arbitrary list of events and rebuild the closure.
              __withEvents = events: __withEventsAssert events true;

              # Set safe mode.
              # i.e.
              # with log....
              # with safety true;
              # return x;
              safety = safe_: mkGroupClosure (logState // { safe = safe_; });
              safely = x: if logState.safe then LazyAttrs x else x;

              # Return accumulated log state.
              # Thunked due to self-reference.
              __logState = NamedThunk "__logState" logState;

              # Return accumulated log events.
              __logEvents = NamedThunk "__logEvents" logState.events;

              # Trace an arbitrary key/value event
              event = name: value: events [{ ${name} = value; }];

              # Trace a message by showing the given value.
              # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
              msg = value:
                __withEventsAssert
                  [{ msg = log.show value; }]
                  (logState: overPartial (tagged "MSG:${groupType}:${groupName}" logState.events));

              # Check a cond is true and throw if it isn't.
              # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
              # Will always perform the check, even if tracing is disabled - only the result
              # logging is controlled by the traceLevel.
              check = name: cond: msg: checks [{inherit name cond msg;}];

              # Check a list of {name cond msg} is true and throw if it isn't.
              # Run the given errors.checks style checks.
              # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
              # Will always perform the checks, even if tracing is disabled - only the result
              # logging is controlled by the traceLevel.
              checks = cs:
                __withEventsAssertReturning
                  [{ checks = map (c: c.name or "unnamed") cs; }]
                  (logState: overPartial (tagged "CHECKS:${groupType}:${groupName}" logState.events))
                  (assert cutils.errors.checks cs; true);

              # Print a warning message
              # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
              warning = msg: Variadic.mkListThen (extra:
                __withEventsAssert
                  [{ warning = [{inherit msg;}] ++ optionals (size extra > 0) [{inherit extra;}]; }]
                  (logState: overPartial (tagged "WARNING:${groupType}:${groupName}" logState.events)));

              # Trace an assignment inline.
              # Cannot be used with 'with' to accrue to the group; returns the assignment.
              # Use 'lets' to accrue.
              assign = name: value:
                __withEventsAssertReturning
                  [{ assign = [{ inherit name; } {inherit value;}];}]
                  (logState: over (tagged "ASSIGN:${groupType}:${groupName}" logState.events))
                  value;

              # Trace a group of assignments and provide access to the results.
              # Accrues logs e.g.
              # with (lets { x = ... }); ... <use x>
              lets = vars:
                assert assertMsg (isAttrs vars) ''Non-attrset vars in 'lets': ${log.print mkVars}'';
                __withEventsAssertWith
                  [{lets = vars;}]
                  (logState: overPartial (tagged "LETS:${groupType}:${groupName}" logState.events))
                  (logState: logState // vars);

              # Return a value from the group, tracing the value.
              #
              # Does not accrue; instead intended to terminate the group by emitting accrued
              # values.
              return = x:
                __withEventsAssertReturning
                  [{return = safely x;}]
                  (logState: over (tagged "RETURN:${groupType}:${groupName}" logState.events))
                  (if isFunction x then returnEta x else x);

              # Return a (possibly variadic) function from the group, tracing the function's
              # return value when it is fully invoked.
              #
              # Does not accrue; instead intended to terminate the group by emitting accrued
              # values.
              #
              # Should be used only when the return value's application is eta-reduced and intended
              # to be the output of the containing function; i.e.
              #
              # Correct usage:
              #
              # getNThings = n:
              #   with log.trace.call "getNThings" n ___;
              #   returnEta (Variadic.mkListOfLength n) # Or just 'return (Variadic.mkListOfLength n)'
              #
              # getNThings 3 "a" "b" -> partial result, no logs
              # getNThings 3 "a" "b" "c" -> fully applied down to a value, now log ["a" "b" "c"]
              #
              # Incorrect usage:
              # (returns a function by design, which can't be distinguished in general
              # from a partially applied Variadic)
              # getGreeter = person:
              #   with log.trace.call "getGreeter" person ___;
              #   returnEta person.greet
              # greet = getGreeter alice;  # No logs
              # greet "hello"; # Logs now that fully applied
              # greet "there"; # Logs again
              #
              # Incorrect usage:
              # (returns a partially applied (filter composedPreds) which will not emit logs
              # until it is applied to a list, and will log every time it is applied to a list)
              #
              # composeNFilters = n:
              #   with log.trace.call "composeNFilters" n ___
              #   returnEta Variadic.mkListThen (fs: filter (x: foldl' (pred: b: b && pred x) true fs))
              returnEta = f:
                let traceOut = out:
                      __withEventsAssertReturning
                        [{ returnEta = out; }]
                        (logState: over (tagged "RETURN_ETA:${groupType}:${groupName}" logState.events))
                        out;
                in Variadic.compose traceOut f;

              # Log a variadic composite function from the group, tracing the function's return value when it is fully invoked.
              # Logs the intermediate value of the fully applied f before passing to g.
              # Can't accrue due to the event state being delegated until application, but the composed
              # result can itself be passed to returnEta/return.
              traceComposeVariadic = name: gName: fName: g: f:
                let gTraceVarargs = varargs:
                      __withEventsAssertReturning
                        [{ vcompose = [ { inherit name; } {g = gName;} {f = fName;} { inherit varargs; } ]; }]
                        (logState: overPartial (tagged "VARIADIC_COMPOSE:${groupType}:${groupName}" logState.events))
                        (g varargs);
                in Variadic.compose gTraceVarargs f;
            };
          in
            Variadic.compose
              (initialState:
                let logState = mkGroupClosure initialState; in
                with logState;
                assert (over (tagged "START:${groupType}:${groupName}" initialState.events));
                logState)
              groupBuilder;

        call = mkEventGroup "call" buildCall;
        methodCall = this: mkEventGroup "methodCall" (buildMethodCall this);
        attrs = mkEventGroup "attrs" buildAttrs;
        test = mkEventGroup "test" buildTest;

      }; in self;

    # Tracing interface using print.
    # with log.trace; assert traces [
    #   (msg "newSubType")
    #   ((withMsg "newSubType").show {inherit This name args;})
    # ];
    trace = mkTrace 0;

    # Tracing interface using print.
    # Enabled when module arg traceLevel is >= 1
    # Does not use --trace-verbose as it is not available in tvix.
    # with log.vtrace; assert over [
    #   (msg "newSubType")
    #   ((withMsg "newSubType").show {inherit This name args;})
    # ];
    vtrace = mkTrace 1;

    # Tracing interface using print at a custom level.
    # Enabled when module arg traceLevel is >= level
    # with log.v 1; assert over ...
    v = level: mkTrace level;
  };

in log // {

  _tests = with cutils.tests; suite {
    log = {

      print = {
        string = expect.eq (log.print "abc") ''"abc"'';
        int = expect.eq (log.print 123) ''123'';
      };

      show = {
        stringToString = expect.eq (log.show "abc") "abc";
        intToString = expect.eq (log.show 123) "123";
      };
      
      trace = {
        call = {
          combined = {
            expr =
              with log.vtrace.call "callName" "arg0" "arg1" ___;
              with msg "test msg";
              with lets {
                a = 1;
                b = 2;
              };
              with lets rec {
                c = 3;
                d = c + 1;
              };
              {
                out = {inherit a b c d;};
                events = resolve __logEvents;
              };
            expected = {
              # Assignments exposed raw for use with 'with'
              __out = {
                a = 1;
                b = 2;
                c = 3;
                d = 4;
              };
              __logEvents = [
                {
                  call = [
                    { name = "callName"; }
                    { args = [ "arg0" "arg1" ]; }
                  ];
                }
                { msg = "test msg"; }
                { lets = { a = 1;
                           b = 2; }; }
                { lets = { c = 3;
                           d = 4; }; }
              ];
            };
          };
        };
      };

    };
  };
}
