{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; },
  # Collate overrides into a single optional attrset, or allow per-option overrides.
  traceOpts ? null,
  # Default trace level, 0 = all (log.trace), 1 = vtrace, 2+ = custom
  # Null to disable.
  traceLevel ? traceOpts.traceLevel or 0,
  # If true, trace not only on start and return, but also on accruing all intermediate values.
  # Duplicative but enables tracing of calls that never reach their return value.
  enablePartialTrace ? traceOpts.enablePartialTrace or false,
  # If true, traces via vprint instead of print.
  enableVerboseTrace ? traceOpts.enableVerboseTrace or false,
  # If true, trace short values at levels 0+.
  enableShortTrace ? traceOpts.enableShortTrace or false,
  ...
}:

with lib;
with collective-lib.attrsets;
with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.lists;
with collective-lib.functions;
with collective-lib.strings;
with collective-lib.syntax;

# Printing/logging utilities
let
  typelib = collective-lib.typelib;
  typed = collective-lib.typed;
  errors = collective-lib.errors;
  syntax = collective-lib.syntax;
  log = rec {

    # Shorthand for adding context for better traces when the expr fails to eval.
    describe = msg: expr:
      builtins.addErrorContext (Safe "|-> ${msg}") expr;
    while = msg: expr: describe "while ${msg}" expr;

    defPrintArgs = {
      # If true, all conversions happen under tryEval
      printSafely = true;
      # If true, print under deepseq
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
      # cycle detection
      cycles = rec {
        minLength = 5;
        maxLength = 10;
        revPath = [];
        cycle = null;
      };
    };

    descend = key: args: args // rec {
      depth = args.depth + 1;
      cycles =
        let
          revPath' = [key] ++ args.cycles.revPath;
          # e.g. { "a.b.c" = {
          #   # Index in the non-reversed path first seen at
          #   index = 7;
          #   # The segment of the path
          #   segment = ["a" "b" "c"];}
          # }
          go = n:
            if n > args.cycles.maxLength then null
            else let segment = take n revPath';
                     lastSegment = take n (drop n revPath');
                 in if segment == lastSegment then segment
                    else go (n + 1);
          detectedCycle =
            if length revPath' < (2 * args.cycles.minLength)
            then null
            else go args.cycles.minLength;
          cycle' =
            if args.cycles.cycle == null && detectedCycle != null
            then { path = reverseList revPath';
                   segment = detectedCycle; }
            else null;
        in
          args.cycles // rec {
            revPath = revPath';
            cycle = cycle';
          };
    };

    compactBlock = args: braceL: braceR: px:
      with args;
      let pxLines = splitLines (formatBlock (joinLines px));
      in formatBlock ''
        ${braceL} ${indent.here (formatLines pxLines)} ${braceR}
      '';

    printAttrs_ = args: x:
      with args;
      if depth >= maxDepth then "... ${toString (size x)} attrs"
      else if x == {} then "{}"
      else
        let maybePrintValue = k: v:
              if replaceAttrs ? ${k}
              then let f = replaceAttrs.${k}; in f v
              else print_ (descend k args) v;
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

    printAttrs = xs: indent.block (printAttrs_ defPrintArgs xs);

    printList_ = args: x:
      with args;
      if depth >= maxDepth then "..."
      else if x == [] then "[]"
      else
        let px = imap0 (i: print_ (descend i args)) x;
            pxLine = formatBlock "[ ${formatBlock (joinLines px)} ]";
        in
          if lineCount pxLine <= 1 then pxLine
          else if compact then formatBlock (compactBlock args "[" "]" px)
          else formatBlock (formatLines [
            "["
            "  ${indent.here (formatLines px)}"
            "]"
          ]);

    printList = xs: indent.block (printList_ defPrintArgs xs);

    # Add parens around a string only if it contains whitespace.
    maybeParen = x: if wordCount x <= 1 then x else "(${x})";

    # Convert a value of any type to a string, supporting the types module's Type values.
    print_ = args: x:
      with args;
      let
        printBlock = value:
          if depth >= maxDepth then "..."
          else if hasShow value && !ignoreShow then
            show value
          else if (value ? __toString) && !ignoreToString then
            toString value
          else {
            null = "null";
            path = toString value;
            string = ''"${value}"'';
            int = ''${builtins.toJSON value}'';
            float = ''${builtins.toJSON value}'';
            lambda = "<lambda>";
            list = formatBlock (printList_ args value);
            set = formatBlock (printAttrs_ args value);
            bool = boolToString value;
          }.${typeOf value};
        maybeStrict = if printStrictly then strict else id;
      in
        if args.cycles.cycle != null then
          "<LOOP: ${joinSep "." (map toString args.cycles.cycle.segment)}>"
        else if printSafely then
          maybeStrict (errors.try (printBlock (maybeStrict x)) (_: "<eval error>"))
        else
          maybeStrict (printBlock x);

    # print x using a function of the default print options
    printWith = f: x: indent.block (print_ (f defPrintArgs) x);
    print = printSafe;
    printSafe = printWith (args: args // prints.using.safe);
    printUnsafe = printWith (args: args // prints.using.unsafe);
    vprintD = n: printWith (args: args // prints.using.raw // prints.using.depth n // prints.using.safe);
    vprintUnsafe = printWith (args: args // prints.using.raw // prints.using.unsafe);
    vprintDUnsafe = n: printWith (args: args // prints.using.raw // prints.using.depth n // prints.using.unsafe);
    prints = rec {
      ___ = collective-lib.functions.___;
      put = x: Variadic.mkSetFromThen defPrintArgs (args: print_ args x);
      block = x: Variadic.composeFunctorsAreAttrs indent.block (put x);
      here = x: Variadic.composeFunctorsAreAttrs indent.here (put x);
      putD = n: x: put x (using.depth n);
      using = {
        safe = { printSafely = true; };
        unsafe = { printSafely = false; };
        raw = { ignoreToString = true; ignoreShow = true; };
        strict = { printStrictly = true; };
        lazy = { printStrictly = false; };
        line = {
          formatBlock = indent.block;
          formatLines = indent.linesSep " ";
        };
        depth = n: { maxDepth = n; };
        mask = names: {
          replaceAttrs =
            mergeAttrsList (map (name: v: "<masked: ${name}>") names);
        };
      };
      _safe = using.safe;
      _raw = using.raw;
      _strict = using.strict;
      _lazy = using.lazy;
      _line = using.line;
      _depth = using.depth;
      _mask = using.mask;
    };
    vprint = x: with prints; put x using.raw using.safe ___;

    # Either print to string using __show if it exists, or return an already-string
    hasShow = x: (x ? __show) && (isFunction x.__show);
    show = x:
      let showX =
            if hasShow x
              then x.__show x
              else x;
          # Handle an unbound 'show' of (this: self: x: x)
          showXHandlingPartial =
            if isAttrs x && typelib.isFunctionNotFunctor showX
              then log.printAttrs x
              else showX;
      in
        dispatch.def print {
          string = id;
        } showXHandlingPartial;

    # Wrapper indicating the value should be traced at the given level
    # using the given trace functions.
    TraceAt = atLevel: x: {
      __isTraceAt = true;
      __x = x;

      # Configurable
      __enableTrace = self: traceLevel != null && self.__atLevel <= traceLevel;
      __atLevel = atLevel;
      __traceFn = self: builtins.trace;

      __trace = self: a:
          if self.__enableTrace self
          then (self.__traceFn self) (self.__showTrace self) a
          else a;

      __showTrace = self:
        let printFn =
          if enableVerboseTrace
          then printWith (args: args // prints.using.depth (3 + 3 * traceLevel))
          else log.print;
        in "\n\n[log.trace(${toString self.__atLevel}).show]\n${printFn self.__x}\n";

      # TraceAt 3 { some = "value"; } outputValue
      __functor = self: a:
        self.__trace self a;
    };
    isTraceAt = x: x.__isTraceAt or false;

    TraceSeqAt = atLevel: x: (TraceAt atLevel x) // {
      __depth = 5;
      __showTrace = self: {
        level = self.__atLevel;
        value = self.__x;
      };
      __traceFn = self:
        builtins.traceSeqN self.__depth;
    };

    TraceShort = x:
      let T = TraceAt 0 x;
      in T // {
      __depth = 3;
      __enableTrace = self: (T.__enableTrace T) && enableShortTrace;
      __showTrace = self:
        if enableVerboseTrace
          then "> ${with log.prints; putD self.__depth self.__x _line _raw ___}"
          else "> ${with log.prints; putD self.__depth self.__x _line ___}";
    };
    short = TraceShort;

    mkTrace = level:
      let
        self = rec {
          # Options that can be set on a mkTrace object that influence any created
          # trace groups. Injected into the initial log state.
          # Can be updated in a rebound way using setOpts i.e.
          # with log.trace.setOpts {safe = true;};
          opts = {
            safe = false;
          };

          # Short-circuit if tracing is disabled.
          enableTrace = traceLevel != null && (level <= traceLevel) || enableShortTrace;

          # log.trace.show [ 456 { a = 2; }] 123
          # -> trace: [ 456 { a = 2; }]
          # 123
          show = xOrTraceAtF: a:
            if !enableTrace then a else

            if isTraceAt xOrTraceAtF
              then let f = xOrTraceAtF; in f a
            else if typelib.isFunctionNotFunctor xOrTraceAtF
              then let f = xOrTraceAtF; in f a
            else
              let x = xOrTraceAtF; in TraceAt level x a;

          # log.trace.over (showId 123)
          # -> trace: 123
          # 123
          showId = x: self.show x x;

          # log.trace.over (msg "123")
          # -> trace: "123"
          # 123
          msg = showId;

          maybeShow = dispatch.def self.show {
            lambda = id;
          };

          # For use with assert for low-paren tracing
          # e.g.
          # with log.trace; assert over ["msg" {a = 123;}]; expr
          # or
          # with log.trace; assert over {a = 123;}; expr
          overNoAssert =
            dispatch.def
              self.show
              {
                lambda = f: f;
                list = fs: composeMany (map maybeShow fs);
              };

          over =
            # Short-circuit if tracing is disabled.
            if enableTrace then (fs: assert overNoAssert fs true; true)
            else const true;

          # Trace over the given fs only if intermediate tracing is enabled.
          overPartial =
            if enablePartialTrace then over else const true;

          # e.g. with log.trace; assert over (tagged "START_CALL" xs);
          tagged = tag: dispatch {
            set = xs: xs // { _ = tag; };
            list = xs: { _ = tag; __ = xs; };
          };

          mkSurround = tag: fs: [ "\n\n" "START:${tag}" (toTraceF fs) "END:${tag}" "\n\n" ];
          surround = tag: fs: over mkSurround tag fs;

          buildInitialLogState = buildInitialLogStateWith {};

          # Builds the initial log state for a call, methodCall, etc.
          # Anything provided in 'extra' is merged into the state (i.e. __functor,
          # or lets/arg assignments)
          buildInitialLogStateWith = extra: self: initEvent: {
            events = [initEvent];
            safe = self.opts.safe;
          } // extra;

          # Builds the initial log state for a typed function call.
          # The state includes a __functor member that consumes as many arguments as are
          # specified in the signature, checks their types, casts if necessary, and invokes the
          # function body against the checked arguments.
          anonymousName = "<anonymous>";
          buildTypedCall = self: includeStateArg: callName: Variadic {
            initialState = {
              ArgTypeSolos = [];
            };

            isTerminal = _: x: lib.isFunction x;

            check = _: x:
              log.while "checking ${callName} argument" (
              (isSolo x && (typed.isTypeLike (soloValue x) 
                            || lib.isString (soloValue x)
                            || builtins.isNull (soloValue x)))
              || lib.isFunction x
              );

            handle = state: x:
              if isSolo x then state // {
                ArgTypeSolos = 
                  log.while "adding ${callName} argument" (
                  concatSolos state.ArgTypeSolos [x]
                  );
              }
              else state;

            # When we reach a function body, wrap it with an arg-acceptor that
            # typechecks the arguments and passes them to the function body.
            # The first argument is the log state so that we can propagate
            terminate = state: fnBody:
              let
                ArgTypeSolos = state.ArgTypeSolos;
                ArgTypes = mergeSolos ArgTypeSolos;
                argNames = soloNames ArgTypeSolos;
                # (typedFnBody callName)should have the same signature as fnBody after this wrapping.
                typedFnBody = callName:
                  log.while "building typed function body for ${callName}" (
                  (flip Variadic.mkOrderedThen) argNames (uncheckedArgs:
                    let 
                      argCastResultSolos =
                        log.while "casting arguments for ${callName}" (
                        mapSolos
                          (argName: ArgType:
                            log.while "casting an argument for ${callName}" (
                            let uncheckedArg = uncheckedArgs.${argName}; in
                            if (builtins.isNull ArgType) then uncheckedArg
                            else if (typelib.isTypeLike ArgType)
                              then typed.unwrapCastResult (typed.cast ArgType uncheckedArg)
                            else if lib.isString ArgType
                              then if (typed.typeIdOf uncheckedArg) == ArgType
                                   then uncheckedArg
                                   else typed.mkCastError "TypeId argument check failed: got ${typed.typeOf uncheckedArg}"
                            else typed.mkCastError "Invalid argument type in function definition: ${ArgType}"
                            )
                          )
                          ArgTypeSolos
                        );

                      partitionedArgSolos = partitionSolos (_: typed.isCastError) argCastResultSolos;
                      argErrorMsgSolos = mapSolos (_: e: e.castError) partitionedArgSolos.right;
                      argSolos = partitionedArgSolos.wrong;
                      args = mergeSolos argSolos;
                      argList = soloValues argSolos;
                      mkSignature = rec {
                        arg = name: "${name} :: ${log.print ArgTypes.${name}}";
                        argValue = name: _l_ (args.${name});
                        argWithMsg = msg: name:
                          if nonEmpty msg then "${arg name} <- ${_l_ msg}"
                          else arg name;
                        argWithMsgOrValue = msg: name:
                          if nonEmpty msg then argWithMsg name
                          else "${arg name} <- ${argValue name}";
                        call = msgs: _b_ ''
                          ${callName} (
                            ${_h_ (_ls_ (map (name: argWithMsg (msgs.${name} or "") name) argNames))}
                          )
                        '';
                      };
                      signature =
                        log.while "building signature for ${callName}" (
                        mkSignature.call (mergeSolos argErrorMsgSolos)
                        );
                      typecheckPassed = empty argErrorMsgSolos;

                      typedLogState = 
                        log.while "building typed log state for ${callName}" (
                        let st = ap.list (self.call callName) argList ___;
                        in st // {
                          call = st.call ++ [
                            {typedCall = true;}
                            {inherit ArgTypes;}
                            {inherit signature;}
                          ];
                          __isAnonymousFunction = callName == anonymousName;
                        } // (optionalAttrs includeStateArg args) # Add top-level args for 'with'
                        );
                    in
                      log.while ''
                        typechecking invocation of typed function:
                        ${signature}
                      '' (
                      assert assertMsg typecheckPassed (_b_ ''
                        Type check failed:
                          ${signature}
                      '');
                      let 
                        argList_ = if includeStateArg then [typedLogState] else argList;
                      in 
                        log.while "invoking ${callName} after successful typecheck" (
                        with typedLogState;
                        assert over [argList_];
                        let retVal = ap.list fnBody argList_; in
                        assert over [(short signature)
                                     (shortReturn typedLogState retVal)];
                        retVal
                        )
                      )
                  )
                  );
              in
                # Expose both functor and fn, allowing:
                # typedCall "f" { a = "int"; } (a: a + 1)
                # with typedCall "f"; fn { a = "int"; } (a: return (a + 1))
                let mkLogState = callName: buildInitialLogStateWith
                  { 
                    __functor = functorSelf: arg: functorSelf.${callName} arg;
                    # Expose the function by name
                    ${callName} = typedFnBody callName;

                    # Add bound way to rebuild with a new name.
                    # Lets anonymous functions be named by their attr key.
                    inherit callName;
                    __isAnonymousFunction = callName == anonymousName;
                    setCallName = mkLogState;
                  }
                  self
                  {
                    call = [
                      {name = callName;}
                      {typedCall = true;}
                    ];
                  };
              in mkLogState callName;
          };

          buildCall = self: callName:
            Variadic.mkListThen (l: 
              buildInitialLogState self {
                call = [
                  {name = callName;}
                  {args = l;}
                ];
              });

          buildMethodCall = self: this: methodName:
            Variadic.mkListThen (l: 
              buildInitialLogState self {
                method = [
                  { name = methodName; } 
                  { inherit this; }
                  { args = l; }
                ]; 
              });

          buildAttrs = self: attrsName:
            Variadic.mkListThen
              (l: buildInitialLogState self {
                attrs = (
                  [ {name = attrsName;} ]
                  ++ (optionals (nonEmpty l) [{extra = l;}]));
              });

          buildTest = self: testName:
            Variadic.mkListThen
              (l: buildInitialLogState self {
                test = (
                  [{name = testName;}]
                  ++ (optionals (nonEmpty l) [{args = l;}]));
              });

          traceCall = self: callName:
            Variadic.composeFunctorsAreAttrs
              (xs: over xs)
              (buildCall self callName);

          traceMethodCall = self: this: methodName:
            Variadic.composeFunctorsAreAttrs
              (xs: over xs)
              (buildMethodCall self this methodName);

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
                # The assertion is always run even if tracing is disabled to enable checks.
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
                # The assertion is always run even if tracing is disabled to enable checks.
                __withEventsAssertWith = events: assertion: f:
                  let logState = __withEventsAssert events assertion;
                  in mkGroupClosure (f logState);

                # Store an arbitrary list of events and rebuild the closure.
                # Also perform an assertion over the updated state, which can perform logging.
                # Discard the state in favor of returning x
                # The assertion is always run even if tracing is disabled to enable checks.
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
                safely = x: 
                  if logState.safe then LazyAttrs x else x;

                # Return accumulated log state.
                # Thunked due to self-reference.
                __logState = NamedThunk "__logState" logState;

                # Return accumulated log events.
                __logEvents = NamedThunk "__logEvents" logState.events;

                # Trace an arbitrary key/value event
                event = name: value: events [safely { ${name} = value; }];

                # Trace a message by showing the given value.
                # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
                msg = value:
                  let event = { msg = log.show value; };
                  in __withEventsAssert
                    [event]
                    (logState:
                      overPartial [
                        (tagged "MSG:${groupType}:${groupName}" logState.events)
                        (short event)
                      ]);

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
                    (assert collective-lib.errors.checks cs; true);

                # Print a warning message
                # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
                warning = msg: Variadic.mkListThen (extra:
                  __withEventsAssert
                    [{ warning = (
                      [{inherit msg;}] 
                      ++ (optionals (nonEmpty extra) [{inherit extra;}])
                    ); 
                    }]
                    (logState: overPartial [
                      (tagged "WARNING:${groupType}:${groupName}" logState.events)
                      (short {warning = msg;})
                    ]));

                # Trace an assignment inline.
                # Cannot be used with 'with' to accrue to the group; returns the assignment.
                # Use 'lets' to accrue.
                assign = name: value:
                  __withEventsAssertReturning
                    [{ assign = [
                      { inherit name; }
                      (safely { inherit value; })
                    ];}]
                    (logState: over [
                      (tagged "ASSIGN:${groupType}:${groupName}" logState.events)
                      #(short {assign = name;})
                    ])
                    value;

                # Override syntax.lets with additional logging.
                # Trace a group of assignments and provide access to the results.
                # Accrues logs e.g.
                # with (lets { x = ... }); ... <use x>
                # Handles the case where the RHS is an anonmymous function, in which case this
                # is renamed.
                lets = vars_:
                  let
                    vars = syntax.lets vars_;
                  in 
                    __withEventsAssertWith
                      [{lets = safely vars;}]
                      (logState: overPartial [
                        (tagged "LETS:${groupType}:${groupName}" logState.events)
                        #(short {lets = attrNames vars;})
                      ])
                      (logState: logState // vars);

                # Create a one-line log for tersely tracing function calls.
                shortReturn = logState: x:
                  let
                    event = (head logState.events);

                    fnStr =
                      if event ? call then
                        (head event.call).name
                      else if event ? method then
                        let this = lookupSolos "this" event.method;
                            methodName = lookupSolos "name" event.method;
                            thisName =
                              if isString this then this
                              else if typelib.isTypeSet this && this ? getName then this.getName {}
                              else if this ? __Type && (resolve this.__Type) ? getName
                              then (resolve this.__Type).getName {}
                              else "<no name event>";
                        in "${thisName}.${methodName}"
                      else
                        "<no call event>";

                    valueStr = x:
                      if isSolo x then with log.prints; put x (_depth 2) _line ___
                      else if typelib.isTypeSet x && x ? getName then "${x.getName {}} <TypeAttrs: ${toString (TerseAttrs x)}>"
                      else if x ? __Type && (resolve x.__Type) ? getName then "Instance<${(resolve x.__Type).getName {}}> ${toString (TerseAttrs x)}"
                      else if lib.isAttrs x then "set(${toString (TerseAttrs x)})"
                      else with log.prints; "${typelib.typeOf x}(${put x (_depth 2) _line ___})";

                    args = lookupSolosDef "args" ["<no event.call or event.method>"] (event.call or event.method or []);
                    argsLines = map (arg: ellipsis 100 (valueStr arg)) args;

                    xStr = valueStr x;

                  in short ("\n" + indent.block ''
                    ${fnStr}(${indent.here (indent.lines argsLines)})
                      -> ${indent.here xStr}
                  '');

                # Return a value from the group, tracing the value.
                # Importantly, if the value is a functor, it will not be traced.
                #
                # Does not accrue; instead intended to terminate the group by emitting accrued
                # values.
                return = x:
                  __withEventsAssertReturning
                    [{return = safely x;}]
                    (logState: over [
                      (tagged "RETURN:${groupType}:${groupName}" logState.events)
                      (shortReturn logState x)
                    ])
                    (if typelib.isFunctionNotFunctor x then returnEta x else x);

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
                          [{ returnEta = safely out; }]
                          (logState: over [
                            (tagged "RETURN_ETA:${groupType}:${groupName}" logState.events)
                            (shortReturn logState out)
                          ])
                          out;
                  in Variadic.composeFunctorsAreAttrs traceOut f;

                # Log a variadic composite function from the group, tracing the function's return value when it is fully invoked.
                # Logs the intermediate value of the fully applied f before passing to g.
                # Can't accrue due to the event state being delegated until application, but the composed
                # result can itself be passed to returnEta/return.
                traceComposeVariadic = name: gName: fName: g: f:
                  let gTraceVarargs = varargs:
                        __withEventsAssertReturning
                          [{ vcompose = [ { inherit name; } {g = gName;} {f = fName;} { varargs = safely varargs; } ]; }]
                          (logState: overPartial [
                            (tagged "VARIADIC_COMPOSE:${groupType}:${groupName}" logState.events)
                            (short {vcompose = name;})
                          ])
                          (g varargs);
                  in Variadic.composeFunctorsAreAttrs gTraceVarargs f;

              };
            in
              # Output of composition is exposed as call, typedCall, etc.
              Variadic.composeFunctorsAreAttrs
                (initialState:
                  let logState = mkGroupClosure initialState; in
                  with logState;
                  assert (over [
                    (tagged "START:${groupType}:${groupName}" initialState.events)
                  ]);
                  logState)
                groupBuilder;

          unbound = self: rec {
            call = mkEventGroup "call" (buildCall self);
            methodCall = this: mkEventGroup "methodCall" (buildMethodCall self this);
            attrs = mkEventGroup "attrs" (buildAttrs self);
            test = mkEventGroup "test" (buildTest self);

            fn_ = includeStateArg: mkEventGroup "typedCall" (buildTypedCall self includeStateArg);
            fn = {
              __functor = self: dispatch {
                string = self.named;
                set = self.anonymous;
              };
              named = fn_ false;
              anonymous = fn_ false anonymousName;

              # Include a first-arg of the function / functor log state itself.
              _ = {
                __functor = self: dispatch {
                  string = self.named;
                  set = self.anonymous;
                };
                named = fn_ true;
                anonymous = fn_ true anonymousName;
              };
            };
            typedCall = fn.named;
            fns = mapAttrs (name: fn: fn.setCallName name);

            FnTestFn = 
              with typed;
              {f}: {a ? Int, b ? String}: callback:
                if callback != null then callback {inherit a b;}
                else a: b: "${toString (a.value + 1)} ${b}";

            # Enable typed syntax using builtin functions.
            # i.e.
            # f = {f}: {a ? Int}: a.value + 1;
            Fn = 
              let
                # i.e. {f}: {a ? Int}: ... -> { name = "f"; bound = ({a ? Int}: ... with f bound to "f") }
                bindFnName = f:
                  log.describe "while binding typed function name by solo arg" (
                  assert isNamedLambda f;
                  let arg = head (getArgs f);
                  in { 
                    name = arg.name; 
                    ArgTypeSolos = [];
                    f__bound = f {${arg.name} = arg.name;};
                  });
                # Exhaust the inner function down by applying defaults,
                # eventually reaching a function body after binding each
                # arg name to its default value containing its typedef.
                bindArgs = f__attrs:
                  log.while "getting arg types for typed function '${f__attrs.name}'" (
                  assert isTypedLambda f__attrs.f__bound;
                  let 
                    # Get the arg type from default value by application of the callback.
                    # Args are returned in the order they appear in the source file.
                    sortedArgTypes = getArgs f__attrs.f__bound;
                    # Pass empty args to get to the callback-to-body lambda.
                    f__doCallback = f__attrs.f__bound {};
                  in f__attrs // rec {
                    # Extract the arg types from the args inside the call.
                    ArgTypes = f__doCallback id;
                    # Pull out the types in the order of arg definition.
                    ArgTypeSolos =
                      checkSolos 
                        (map 
                          (arg: mkSolo arg.name ArgTypes.${arg.name})
                          sortedArgTypes);
                    # Pass a null callback to skip past it to the function body.
                    f__body = f__doCallback null;
                    # Construct the final typed function
                    f__typed = 
                      # Equivalent to:
                      # fn <name> <arg0> <arg1> ... <argN> <body>
                      let argTypesToFnBodyToTypedFn = 
                            log.while "binding typed function name from structured lambda" (
                            fn f__attrs.name
                            );
                          bodyToTypedFn = 
                            log.while "binding argument type solos from structured lambda" (
                            assert log.trace.over [ArgTypeSolos];
                            ap.list argTypesToFnBodyToTypedFn ArgTypeSolos
                            );
                          typedFn = bodyToTypedFn f__body;
                      in typedFn;
                  });
              in f: 
                log.while "constructing typed function from structured lambda" (
                let f__nameBound = bindFnName f;
                    f__argsBound = bindArgs f__nameBound;
                in f__argsBound.f__typed
                );

            # Set options on the bound self and rebind such that they are
            # available on the unbound methods.
            setOpts = opts: bindSelf (self // { inherit opts; });
            modifyOpts = f: setOpts (f self.opts);
            # Set safe mode.
            # i.e. (log.trace.safe true).call "callName" "arg0" "arg1" ___;
            safe = safe_: modifyOpts (opts: opts // { safe = safe_; });
          };
        };
        # Bind the self reference to the unbound methods.
        bindSelf = self: 
          let self_ = self // self.unbound self_;
          in self_;
      in 
        bindSelf self;

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

    # Light interface to interject safety mode without parens
    # with log.safe; assert over ...
    safe = {
      v = level: (v level).safe true;
      trace = trace.safe true;
      vtrace = vtrace.safe true;
    };
  };

in log // {
  _tests = with collective-lib.tests; suite {
    print = {
      string = expect.eq (log.print "abc") ''"abc"'';
      int = expect.eq (log.print 123) ''123'';
      cycles.none = expect.eq (with log.prints; put { a = 123; } _line ___) "{ a = 123; }";
      cycles.tooShort =
        expect.eq
          (with log.prints;
            put { a = { a = { a = { a = { a = { a = { a = { a = { a = 123; }; }; }; }; }; }; }; }; } _line ___)
          "{ a = { a = { a = { a = { a = { a = { a = { a = { a = 123; }; }; }; }; }; }; }; }; }";
      cycles.falsePositive =
        expect.eq
          (with log.prints;
            put { a = { a = { a = { a = { a = { a = { a = { a = { a = {a = 123; }; }; }; }; }; }; }; }; }; } _line ___)
          "{ a = { a = { a = { a = { a = { a = { a = { a = { a = { a = <LOOP: a.a.a.a.a>; }; }; }; }; }; }; }; }; }; }";
      cycles.trueCycle.aaa =
        expect.eq
          (with log.prints; put (let a = { a = { a = a; }; }; in a) _line ___)
          "{ a = { a = { a = { a = { a = { a = { a = { a = { a = { a = <LOOP: a.a.a.a.a>; }; }; }; }; }; }; }; }; }; }";
      cycles.trueCycle.aba =
        expect.eq
          (with log.prints; put rec { a = { b = { a = a; }; }; } _line ___)
          "{ a = { b = { a = { b = { a = { b = { a = { b = { a = { b = { a = { b = <LOOP: b.a.b.a.b.a>; }; }; }; }; }; }; }; }; }; }; }; }";
      cycles.trueCycle._000 =
        expect.eq
          (with log.prints; put (let a = [a]; in a) _line ___)
          "[ [ [ [ [ [ [ [ [ [ <LOOP: 0.0.0.0.0> ] ] ] ] ] ] ] ] ] ]";
      cycles.trueCycle.a1b2 =
        expect.eq
          (with log.prints; put rec { a = [ 0 { b = [ 0 1 a ]; } ]; } _line ___)
          "{ a = [ 0 { b = [ 0 1 [ 0 { b = [ 0 1 [ 0 { b = [ 0 1 [ 0 { b = [ 0 1 <LOOP: 2.b.1.2.b.1> ]; } ] ]; } ] ]; } ] ]; } ]; }";
      cycles.trueCycle.max =
        expect.eq
          (with log.prints;
            put (let a = { a = { b = { c = {d = {e = {f = {g = {h = {i = {j =  a;};};};};};};};};};}; in a)  _line ___)
          "{ a = { b = { c = { d = { e = { f = { g = { h = { i = { j = { a = { b = { c = { d = { e = { f = { g = { h = { i = { j = <LOOP: j.i.h.g.f.e.d.c.b.a>; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }";
      cycles.trueCycle.tooLong =
        expect.eq
          (with log.prints;
            put (let a = { a = { b = { c = {d = {e = {f = {g = {h = {i = {j = {k = a;};};};};};};};};};};}; in a)  _line ___)
          # Undetected; falls back to ellipsis-at-20
          "{ a = { b = { c = { d = { e = { f = { g = { h = { i = { j = { k = { a = { b = { c = { d = { e = { f = { g = { h = { i = ...; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }; }";
    };

    show = {
      stringToString = expect.eq (log.show "abc") "abc";
      intToString = expect.eq (log.show 123) "123";
    };

    # <nix>log._tests.run</nix>
    # <nix>log._tests.debug</nix>
    trace = {
      call = {
        typed = with typed; with log.trace; {
          # Not permitted due to inability to detect termination when we get a function.
          nullary = expect.error (log.trace.typedCall "nullary" 1);
          unary.Any = expect.eq
            (let f = typedCall "f" { a = Any; } (a: (a + 1));
             in [(f 3) (f 3.5)])
            [4 4.5];
          unary.Int.Int = expect.eq
            (let f = typedCall "f" { a = Int; } (a: int a + 1);
             in f (Int 1))
             2;
          unary.Int.int = expect.eq
            (let f = typedCall "f" { abc = Int; } (abc: int abc + 1);
             in f 1)
             2;
          unary.int.Int = expect.eq
            (let f = typedCall "f" { abc = "int"; } (abc: abc + 1);
             in f (Int 1))
             2;
          unary.Lambda.lambda = expect.eq
            (let f = typedCall "f" { g = Lambda; } (g: g 3);
             in f (x: x + 4))
             7;
          unary.returnLambda = expect.eq
            (let f = typedCall "f" { abc = Any; } (abc: x: 123);
             in f 1 null)
             123;
          binary.intInt.partial = expect.isLambda
            (let f = typedCall "f" { a = Int; } { b = Int; } (a: b: int a + int b);
             in f 1);
          binary.intInt.full = expect.eq
            (let f = typedCall "f" { a = Int; } { b = Int; } (a: b: int a + int b);
             in f 1 2)
             3;
          binary.usingWith = expect.eq
            (with typedCall "f" { a = Int; } { b = Int; }
                  (a: b: int a + int b);
             f 1 2)
             3;
          binary.fn = 
            expect.eq
            (let f = fn "f" { a = Int; } { b = Int; } (a: b: int a + int b);
             in f 1 2)
             3;
          binary.usingState = expect.eq
            (let f = fn "f" { a = Int; } { b = Int; }
                     (a: b: with f; with lets {c = int a + int b;}; return c);
             in f 1 2)
             3;
          binary.usingStateArg = expect.eq
            (let f = fn._ "f" { a = Int; } { b = Int; }
                     (_: with _; with lets {c = int a + int b;}; return c);
             in f 1 2)
             3;
          anonymous.noState = expect.eq
            (let f = fn { a = Int; } (a: int a + 1);
             in f 1)
             2;
          anonymous.usingState = expect.eq
            (let f = fn { a = Int; } (a: with f; return (int a + 1));
             in f 1)
             2;
          anonymous.usingStateArg = expect.eq
            (let f = fn._ { a = Int; } (_: with _; return (int a + 1));
             in f 1)
             2;
          anonymous.usingStateArgDirectly = expect.eq
            (let f = fn._ { a = Int; } (_: _.return (int _.a + 1));
             in f 1)
             2;
          fns.withFns = expect.eq
            (with fns {
               f = fn { a = Int; } (a: int a + 1);
               g = fn { b = Int; } (b: int b * 2);
             };
             f 3 + g 5)
             14;
          fns.withLets = expect.eq
            (with lets {
               x = 3;
               f = fn { a = Int; } (a: int a + 1);
               g = fn { b = Int; } (b: int b * 2);
             };
             f x + g x)
             10;
        };
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
            out = {
              a = 1;
              b = 2;
              c = 3;
              d = 4;
            };
            events = [
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
}
