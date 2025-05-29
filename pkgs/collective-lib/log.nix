{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.types;
with cutils.lists;
with cutils.functions;

# Printing/logging utilities
let
  log = rec {
    compactBlock = formatBlock: braceL: braceR: px:
      formatBlock (
        let pxLines = splitLines (formatBlock (joinLines px));
        in ''
          ${braceL} ${indent.here (indent.lines pxLines)} ${braceR}
        '');

    printAttrs_ = formatBlock: compact: x:
      if x == {} then "{}"
      else
        let px = mapAttrsToList (k: v: "${k} = ${formatBlock (print_ formatBlock compact v)};") x;
            pxLine = formatBlock "{ ${head px} }";
        in
          if length px == 1 && lineCount pxLine == 1 then pxLine
          else if compact then formatBlock (compactBlock formatBlock "{" "}" px)
          else formatBlock ''
            {
              ${indent.here (indent.lines px)}
            }
          '';

    printAttrs = printAttrs_ codeBlock true;

    printList_ = formatBlock: compact: x:
      if x == [] then "[]"
      else
        let px = map (print_ formatBlock compact) x;
            pxLine = formatBlock "[ ${formatBlock (joinLines px)} ]";
        in
          if lineCount pxLine <= 1 then pxLine
          else if compact then formatBlock (compactBlock formatBlock "[" "]" px)
          else formatBlock ''
            [
              ${indent.here (indent.lines px)}
            ]
          '';

    printList = printList_ codeBlock true;

    # Add parens around a string only if it contains whitespace.
    maybeParen = x: if wordCount x <= 1 then x else "(${x})";

    # Convert a value of any type to a string, supporting the types module's Type values.
    print_ = formatBlock: compact: x:
      if Types.isTyped x && (x ? __toString)
      then maybeParen (toString x)
      else {
        null = "null";
        path = toString x;
        string = ''"${x}"'';
        int = ''"${builtins.toJSON x}"'';
        float = ''"${builtins.toJSON x}"'';
        lambda = "<lambda>";
        list = formatBlock (printList_ formatBlock compact x);
        set = formatBlock (printAttrs_ formatBlock compact x);
        bool = if x then "true" else "false";
      }.${typeOf x};

    print = x: codeBlock (print_ trimNewlines true x);

    mkTrace = traceFn:
      let self = rec {
        # log.trace.show [ 456 { a = 2; }] 123
        # -> trace: [ 456 { a = 2; }]
        # 123
        show = x: a: traceFn "\n\n[log.trace.show]\n${print x}\n" a;

        # log.trace.id 123
        # -> trace: 123
        # 123
        id = x: show x x;

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

        mkTagged = tag: fs: [ "\n\n" "START:${tag}" (toTraceF fs) "END:${tag}" "\n\n" ];
        tagged = tag: fs: over mkTagged tag fs;

        # e.g.
        # with log.trace; assert call "callName" arg1 arg2
        buildCall = callName:
          Variadic.compose
            (l: { call = [ {name = callName;} {args = l;} ]; })
            Variadic.mkList;

        buildMethodCall = this: methodName:
          Variadic.compose
            (l: { call = [ {method = methodName;} { inherit this; } {args = l;} ]; })
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
        call_ = buildCall_: callName:
          Variadic.compose
            (xs: rec {
              # Return a value from the call, tracing the value.
              return = x: assert over (xs // { call = xs.call ++ [{
                return = x;
              }]; }); x;

              # Return a variadic function from the call, tracing the function's return value when it is fully invoked.
              traceVariadic = f:
                assert assertMsg (isFunction f) "returnVariadic: f must be a function (got ${log.print f})";
                let traceOut =
                      out:
                      assert over (xs // {
                        call = xs.call ++ [{
                          variadicReturn = out;
                        }];
                      });
                      out;
                in Variadic.compose traceOut f;

              # Return a variadic composite function from the call, tracing the function's return value when it is fully invoked.
              traceComposeVariadic = name: gName: fName: g: f:
                let gTraceVarargs =
                      varargs:
                        assert over (xs // {
                          call = xs.call ++ [{
                            vcompose = [ { inherit name; } {g = gName;} {f = fName;} { inherit varargs; } ];
                          }];
                        });
                        g varargs;
                in traceVariadic (Variadic.compose gTraceVarargs f);

            })
            (buildCall_ callName);

        call = call_ buildCall;
        methodCall = this: call_ (buildMethodCall this);


      }; in self;

    # Tracing interface using print.
    # with log.trace; assert traces [
    #   (msg "newSubType")
    #   ((withMsg "newSubType").show {inherit This name args;})
    # ];
    trace = mkTrace builtins.trace;

    # Tracing interface using print.
    # Only log when --trace-verbose is set
    # with log.vtrace; assert traces [
    #   (msg "newSubType")
    #   ((withMsg "newSubType").show {inherit This name args;})
    # ];
    vtrace = mkTrace builtins.traceVerbose;



  };

in log
