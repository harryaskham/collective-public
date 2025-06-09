{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.attrs;
with cutils.lists;
with cutils.functions;
with cutils.strings;

# Printing/logging utilities
let
  log = rec {

    mkPrintArgs = {
      ignoreToString = false;
      # Default here to leave indentation markers in until the top level call
      formatBlock = trimNewlines;
      formatLines = indent.linesSep "\n";
      compact = true;
      depth = 0;
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
    print_ = args: x:
      with args;
      let
        block =
          if depth >= maxDepth then "..."
          else if (x ? Type) && (x ? __toString) && !ignoreToString then
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
        raw = { ignoreToString = true; };
        line = { formatLines = indent.linesSep " "; };
        depth = n: { maxDepth = n; };
        mask = names: {
          replaceAttrs =
            mergeAttrsList (map (name: v: "<masked: ${name}>") names);
        };
      };
      _raw = using.raw;
      _line = using.line;
      _depth = using.depth;
      _mask = using.mask;
    };
    vprint = x: with prints; put x using.raw ___;

    # Either print to string or return an already-string
    show = dispatchDef print {
      string = id;
    };

    mkTrace = traceFn:
      let self = rec {
        # log.trace.show [ 456 { a = 2; }] 123
        # -> trace: [ 456 { a = 2; }]
        # 123
        show = x: a: traceFn "\n\n[log.trace.show]\n${log.show x}\n" a;

        # log.trace.showId 123
        # -> trace: 123
        # 123
        showId = x: show x x;
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
        over = fs: (toTraceF fs) true;

        # e.g. with log.trace; assert over (tagged "START_CALL" xs);
        tagged = tag: xs: xs // { _ = tag; };

        mkSurround = tag: fs: [ "\n\n" "START:${tag}" (toTraceF fs) "END:${tag}" "\n\n" ];
        surround = tag: fs: over mkSurround tag fs;

        # e.g.
        # with log.trace; assert call "callName" arg1 arg2
        buildCall = callName:
          Variadic.compose
            (l: { events = [ {call = [{name = callName;} {args = l;}]; }]; })
            Variadic.mkList;

        buildMethodCall = this: methodName:
          Variadic.compose
            (l: { events = [ {method = [{name = methodName;} { This = this.name; } {args = l;}]; }]; })
            Variadic.mkList;

        buildAttrs = attrsName:
          Variadic.compose
            (l: { events = [ {attrs = [{name = attrsName;}] ++ (optionals (size l > 0) [{extra = l;}]); }]; })
            Variadic.mkList;

        buildTest = testName:
          Variadic.compose
            (l: { events = [ {test = [{name = testName;}] ++ (optionals (size l > 0) [{args = l;}]); }]; })
            Variadic.mkList;

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
        mkEventGroup = groupType: buildGroup_: groupName:
          let
            groupBuilder = buildGroup_ groupName;
            mkGroupClosure = first: xs:
              assert (if first
                      then vtrace.over (tagged "START:${groupType}:${groupName}" xs)
                      else true);
              let
                withEvents = xs: events: xs // {
                  events = xs.events ++ events;
                };
              in rec {
                # Return accumulated log state.
                __logState = xs;

                # Return accumulated log events.
                __logEvents = xs.events;

                # Trace an intermediate value
                intermediate = name: value: event "intermediate" { ${name} = value; };

                # Trace an arbitrary key/value event
                event = name: value: events [{ ${name} = value; }];

                # Trace an arbitrary set of key/value events
                events = events_:
                  let xs' = withEvents xs (solos events_);
                  in assert over (tagged "EVENTS:${groupType}:${groupName}" xs');
                     mkGroupClosure false xs';

                # Trace a message by showing the given value.
                # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
                msg = value:
                  let xs' = withEvents xs [{ msg = log.show value; }];
                  in assert over (tagged "MSG:${groupType}:${groupName}" xs');
                     mkGroupClosure false xs';

                # Trace a message by printing the given value.
                # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
                check = name: p: msg:
                  if p then
                    let xs' = withEvents xs [{ check = { ${name} = "OK"; }; }];
                    in assert over (tagged "CHECK:${groupType}:${groupName}" xs');
                       xs'
                  else
                    let xs' = withEvents xs [{ check = { ${name} = "FAIL: ${msg}"; }; }];
                    in throw (log.print (tagged "CHECK:${groupType}:${groupName}" xs'));

                # Print a warning message
                # When used with 'with', accrues logs by modifying the mkGroupClosure in scope.
                warning = msg: Variadic.mkListThen (extra:
                  let
                    xs' = withEvents xs [
                      { warning = [{inherit msg;}] ++ optionals (size extra > 0) [{inherit extra;}]; }
                    ];
                  in assert over (tagged "WARNING:${groupType}:${groupName}" xs');
                     mkGroupClosure false xs'
                );

                # Trace an assignment inline.
                # Does not accrue logs, and returns the assignment.
                assign = name: value:
                  let xs' = withEvents xs [{ assign = [{ inherit name; } {inherit value;}];}];
                  in assert over (tagged "ASSIGN:${groupType}:${groupName}" xs');
                     value;

                # Trace a group of assignments and provide access to the results.
                # Accrues logs for e.g.
                # with (letrec (_: { ... })); ...
                lets = vars: letrec_ "LETS" (_: vars);

                # Trace a group of assignments and provide access to the results.
                # Can recursively refer to the finally-assigned attributes via
                # the function self argument.
                # Accrues logs for e.g.
                # with (letrec (_: { ... })); ...
                letrec = letrec_ "LETREC";
                letrec_ = tag: mkVars:
                  let vars = mkVars vars;
                      xs' = withEvents xs [{assigns = vars;}];
                  in
                    assert over (tagged "${tag}:${groupType}:${groupName}" xs');
                    # Return the combined log closure and vars for with to provide access
                    # Name collisions prefer the vars
                    (mkGroupClosure false xs') // vars;

                # Return a value from the group, tracing the value.
                return = x:
                  if isFunction x then traceVariadic x
                  else
                    let xs' = withEvents xs [{return = x;}];
                    in assert over (tagged "RETURN:${groupType}:${groupName}" xs');
                       x;
                                   

                # Return a variadic function from the group, tracing the function's return value when it is fully invoked.
                traceVariadic = f:
                  let traceOut = out:
                        let xs' = withEvents xs [{ variadicReturn = out; }];
                        in assert over (tagged "VARIADIC_RETURN:${groupType}:${groupName}" xs');
                           out;
                  in Variadic.compose traceOut f;

                # Return a variadic composite function from the group, tracing the function's return value when it is fully invoked.
                traceComposeVariadic = name: gName: fName: g: f:
                  let gTraceVarargs = varargs:
                        let xs' = withEvents xs [
                          { vcompose = [ { inherit name; } {g = gName;} {f = fName;} { inherit varargs; } ]; }
                        ];
                        in assert over (tagged "VARIADIC_COMPOSE:${groupType}:${groupName}" xs');
                        g varargs;
                  in Variadic.compose gTraceVarargs f;

            };
          in
            Variadic.compose (mkGroupClosure true) groupBuilder;

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
    trace = mkTrace builtins.trace;

    # Tracing interface using print.
    # Only log when --trace-verbose is set
    # with log.vtrace; assert over [
    #   (msg "newSubType")
    #   ((withMsg "newSubType").show {inherit This name args;})
    # ];
    vtrace = mkTrace builtins.traceVerbose;

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
              with letrec (_: {
                c = 3;
                d = _.c + 1;
              });
              let __out = {inherit a b c d;};
              in { inherit __out __logEvents; };
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
                { assigns = { a = 1;
                              b = 2; }; }
                { assigns = { c = 3;
                              d = 4; }; }
              ];
            };
          };
        };
      };

    };
  };
}
