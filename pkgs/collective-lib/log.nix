{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.types;
with cutils.lists;
with cutils.functions;

# Printing/logging utilities
let
  log = rec {

    mkPrintArgs = {
      ignoreToString = false;
      # Default here to leave indentation markers in until the top level call
      formatBlock = trimNewlines;
      compact = true;
      depth = 0;
      maxDepth = 10;
    };
    descend = args: args // { depth = args.depth + 1; };

    compactBlock = args: braceL: braceR: px:
      with args;
      formatBlock (
        let pxLines = splitLines (formatBlock (joinLines px));
        in ''
          ${braceL} ${indent.here (indent.lines pxLines)} ${braceR}
        '');

    printAttrs_ = args: x:
      with args;
      if depth >= maxDepth then "..."
      else if x == {} then "{}"
      else
        formatBlock (
          let px = mapAttrsToList (k: v: "${k} = ${(print_ (descend args) v)};") x;
              pxLine = "{ ${head px} }";
          in
            if length px == 1 && lineCount pxLine == 1 then pxLine
            else if compact then (compactBlock args "{" "}" px)
            else ''
              {
                ${indent.here (indent.lines px)}
              }
            '');

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
          else formatBlock ''
            [
              ${indent.here (indent.lines px)}
            ]
          '';

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
    prints = rec {
      ___ = cutils.functions.___;
      put = x: Variadic.mkSetFromThen mkPrintArgs (args: print_ args x);
      block = x: Variadic.compose indent.block (put x);
      here = x: Variadic.compose indent.here (put x);
      using = {
        raw = { ignoreToString = true; };
        depth = n: { depth = n; };
        maxDepth = n: { maxDepth = n; };
      };
    };
    vprint = x: with prints; put x using.raw ___;

    mkTrace = traceFn:
      let self = rec {
        # log.trace.show [ 456 { a = 2; }] 123
        # -> trace: [ 456 { a = 2; }]
        # 123
        show = x: a: traceFn "\n\n[log.trace.show]\n${print x}\n" a;

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
            (l: { group = [ {call = callName;} {args = l;} ]; })
            Variadic.mkList;

        buildMethodCall = this: methodName:
          Variadic.compose
            (l: { group = [ {method = methodName;} { T = this.name; } {args = l;} ]; })
            Variadic.mkList;

        buildAttrs = attrsName:
          Variadic.compose
            (l: { group = [ {attrs = attrsName;} ] ++ (optionals (size l > 0) [{extra = l;}]); })
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
        group_ = groupType: buildGroup_: groupName:
          let
            groupBuilder = buildGroup_ groupName;
            groupUtils = xs:
              assert vtrace.over (tagged "START:${groupType}:${groupName}" xs);

              rec {
              # Trace an intermediate value
              intermediate = name: value:
                let xs' = xs // {
                  group = xs.group ++ [
                    { intermediate = name; }
                    { inherit value; }
                  ];
                }; in assert over (tagged "TRACE:${groupType}:${groupName}" xs'); xs';

              # Trace a message by printing the given value.
              # When used with 'with', accrues logs by modifying the groupUtils in scope.
              msg = value:
                let xs' = xs // {
                  group = xs.group ++ [
                    { msg = print value; }
                  ];
                }; in assert over (tagged "MSG:${groupType}:${groupName}" xs'); xs;

              # Trace an assignment inline.
              # Does not accrue logs, and returns the assignment.
              assign = name: value:
                assert over (tagged "ASSIGN:${groupType}:${groupName}" (xs // {
                  group = xs.group ++ [{
                    assign = [{ inherit name; } {inherit value;}];
                  }];
                }));
                value;

              # Return a value from the group, tracing the value.
              return = x:
                if isFunction x then traceVariadic x
                else assert over (tagged "RETURN:${groupType}:${groupName}" (xs // {
                  group = xs.group ++ [{
                    return = x;
                  }];
                })); x;

              # Return a variadic function from the group, tracing the function's return value when it is fully invoked.
              traceVariadic = f:
                let traceOut = out:
                      assert over (tagged "VARIADIC_RETURN:${groupType}:${groupName}" (xs // {
                        group = xs.group ++ [{
                          variadicReturn = out;
                        }];
                      }));
                      out;
                in Variadic.compose traceOut f;

              # Return a variadic composite function from the group, tracing the function's return value when it is fully invoked.
              traceComposeVariadic = name: gName: fName: g: f:
                let gTraceVarargs = varargs:
                      assert over (tagged "VARIADIC_COMPOSE:${groupType}:${groupName}" (xs // {
                        group = xs.group ++ [{
                          vcompose = [ { inherit name; } {g = gName;} {f = fName;} { inherit varargs; } ];
                        }];
                      }));
                      g varargs;
                in Variadic.compose gTraceVarargs f;

            };
          in
            Variadic.compose groupUtils groupBuilder;

        call = group_ "call" buildCall;
        methodCall = this: group_ "methodCall" (buildMethodCall this);
        attrs = group_ "attrs" buildAttrs;


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

in log
