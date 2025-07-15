{ lib, collective-lib, ansi-utils, ... }:

with collective-lib.typed;  # Replace 'lib' entirely
with lists;
with functions;
with ansi-utils;
with ansi;

let typed = collective-lib.typed;
    tests = collective-lib.tests;
    expect = collective-lib.tests.expect;
in rec {
  # Expose an extended log containing log.shell.*
  log = collective-lib.log // {
    shell = rec {
      levels = {
        debug = {
          name = "debug";
          style = {
            tag = [bold fg.brightcyan];
            lineText = [];
          };
        };
        info = {
          name = "info";
          style = {
            tag = [bold fg.green];
            lineText = [];
          };
        };
        warn = {
          name = "warn";
          style = {
            tag = [bold fg.magenta];
            lineText = [];
          };
        };
        error = {
          name = "error";
          style = {
            tag = [bold fg.red];
            lineText = [];
          };
        };
        fatal = {
          name = "fatal";
          style = {
            tag = [bold bg.red fg.brightwhite];
            lineText = [bold];
          };
        };
        success = {
          name = "success";
          style = {
            tag = [bold fg.green];
            lineText = [];
          };
        };
      };

      DEBUG = levels.debug;
      INFO = levels.info;
      WARN = levels.warn;
      ERROR = levels.error;
      FATAL = levels.fatal;
      SUCCESS = levels.success;

      # Construct a prefix with the binary name and the level tag
      prefix =
        let mkTag = _: level:
            ''${style [fg.grey] "$(basename $0)"} ${style level.style.tag "[${level.name}]"}'';
        in mapAttrs mkTag levels;

      # Construct a set of log line builder functions where each of line.level
      # are functions that take a string and return a string.
      # e.g. line.info true "Hello world" is "appname [info] Hello world" (with styles)
      #      line.info false "Hello world" is "Hello world" (with styles)
      line =
        let mkLineBuilder = levelName: level:
              withPrefix: text:
                ''${optionalString withPrefix "${prefix.${levelName}} "}${style level.style.lineText text}'';
        in mapAttrs mkLineBuilder levels;

      # A set from level to a builder for a formatted line without prefix.
      text = mapAttrs (_: lineFn: lineFn false) line;

      # Get the indent level to line a newline up with the logtag RHS
      indent = level:
        let getIndent = levelName: level: prefix.${levelName};
        in 1 + mapAttrs getIndent levels;

      # An attrset from level to an echo-command builder that takes text and returns an echo command string.
      # Not usually used directly, but rather via e.g. log.shell.info which calls shell
      # defined log() generated via this echo.${level} function.
      # e.g. echo.info true "Hello world" is "echo -e \"appname [info] Hello world\"" (with styles)
      # All levels log to stderr only.
      echoLine = withPrefix:
        let mkEchoCommand = levelName: level:
              text: ''${ansi.echo (line.${levelName} withPrefix text)} >&2'';
        in mapAttrs mkEchoCommand levels;

      # Echo a line text in the style of the level without the prefix.
      echoLineText = echoLine false;

      # Convert a value to a string for output to the shell.
      # Calls getValue to handle either e.g. Int 123, 123, String "a string", "a string", etc.
      ShellValue = Type.template "ShellValue" [{ T = Type; }] (_: {
        fields = [{ value = _.T; }];

        # ToShellValue implemented on the Class.
        methods.__implements__toString = this: self: toShellValue this;
      });

      # A shell value plus a code.
      ShellReturnValue = Type.template "ShellReturnValue" [{ T = Type; }] (_: {
        fields = [
          { returnValue = _.T; }
          { code = ShellValue.bind { T = Int; }; }
        ];

        # ToShellValue implemented on the Class.
        methods.__implements__toString = this: self: toShellValue this;
      });

      # Store the exit/return action, code and optional value for the return/exit log functions.
      LogReturnAction = Type.template "LogReturnAction" [{ T = Type; }] (_: {
        fields = [
          # The optional value to pass to toShellValue for conversion to a string for
          # logging in functions that return a value.
          # Only applicable if returnCode is set for logging in a function; in this
          # case, equivalent to:
          # echo "${toShellValue returnValue}"
          # return "${toShellValue returnCode}"
          # If instead exitCode is set, the 'return' value is considered to be
          # the log text, which is output in place of any custom return.
          { returnValue = _.T; }

          # The log-* suffix to determine which log function to call.
          { suffix = String; }

          # print usage message after logging
          { withUsage = Bool; }
        ];

        methods = {
          # Generate e.g. -with-usage, -return-with-usage, etc.
          getSuffix = this: _:
            let withUsageSuffix = if bool this.withUsage then "-with-usage" else "";
            in "${this.suffix}${withUsageSuffix}";

          # Generate e.g. log-exit-with-usage
          getLogFn = this: _: "log${this.getSuffix {}} ${this.returnValue}";

          # ToShellValue implemented on the Class.
          __implements__toString = this: self: toShellValue this;
        };
      });

      LogReturnValueCode = value: code: LogReturnAction (ShellReturnValue value code) "-return" false;
      LogReturnCode = code: LogReturnAction (ShellReturnValue null code) "-return" false;
      LogExitCode = code: LogReturnAction (ShellReturnValue null code) "-exit" false;
      LogExitCodeWithUsage = code: LogReturnAction (ShellReturnValue null code) "-exit" true;

      # Makes a log message attribute set.
      # LogMessage INFO "text"
      # LogMessage.WithUsage FATAL "error"
      # LogMessage.WithUsage FATAL (LogReturnAction.Exit 1)
      # LogMessage SUCCESS (LogReturnAction.ReturnValue 0 123)
      # Ultimately exposed via e.g. log.shell.info <args> "text"
      LogMessage = Type.template "LogMessage" [{ T = Type; }] (_: {

        fields = [
          # The action to take if any.
          { action = NullOr _.T; }
          # The log level to use.
          # TODO: Move to a log level type
          { level = Set; }
          # The text to log if any.
          { text = NullOr (ShellValue.bind { T = String; }); }
        ];

        methods = {
          getLogText = this: _:
            if typed.isNull this.text then ''""'' else
            toString this.text;

          getLevelName = this: _:
            ((set this.level).name);

          getLogFn = this: _:
            if typed.isNull this.action then "log" else
            toString this.action;

          # Generate the bash 'log level "msg"' function call for invoking the log helpers to print
          # this message.
          getLogCall = this: _: 
            "${this.getLogFn {}} ${this.getLevelName {}} ${this.getLogText {}}";

          # Finally override s.t. toString allows interpolation like:
          # ${log.shell.info "test"}
          # ToShellValue implemented on the Class.
          __implements__toString = this: self: toShellValue this;
        };
      });

      # Intended actual logging interface, using log() function defined in logBlock.
      # Avoids directly inlining the debug checks and escape codes on each line.
      # log.shell.{debug,info,warn,error} output to STDERR
      # log.shell.{fatal,fatalCode,fatalWithUsage} output to STDERR and exit with code
      # log.shell.return.* output an optional value to STDOUT and return with code
      # log.shell.exit.* output an optional value to STDOUT and return with code
      # Fast untyped versions for better performance
      debug = msg: ''log debug "${msg}"'';
      info = msg: ''log info "${msg}"'';
      warn = msg: ''log warn "${msg}"'';
      error = msg: ''log error "${msg}"'';
      
      # Typed versions (slower but more features)
      debugTyped = LogMessage Nil DEBUG;
      infoTyped = LogMessage Nil INFO;
      warnTyped = LogMessage Nil WARN;
      errorTyped = LogMessage Nil ERROR;
      exit = {
        success = msg: ''log-exit 0 "${msg}"'';
        fatalCode = exitCode: msg: ''log-exit ${toString exitCode} "${msg}"'';
        fatal = msg: ''log-exit 1 "${msg}"'';
        fatalWithUsage = msg: ''log-exit-with-usage 1 "${msg}"'';
        
        # Typed versions (slower but more features)
        successTyped = LogMessage (LogExitCode 0) SUCCESS;
        fatalCodeTyped = exitCode: LogMessage (LogExitCode exitCode) FATAL;
        fatalTyped = exit.fatalCodeTyped 1;
        fatalWithUsageTyped = LogMessage (LogExitCodeWithUsage 1) FATAL;
      };
      return = {
        success = msg: ''log-return 0 "" success "${msg}"'';
        value = v: ''log-return 0 ${toString v} "" success ""'';
        errorCode = returnCode: msg: ''log-return ${toString returnCode} "${msg}" error ""'';
        error = msg: ''log-return 1 "${msg}" error ""'';
        
        # Typed versions (slower but more features)
        successTyped = LogMessage (LogReturnCode 0) SUCCESS;
        valueTyped = v: LogMessage (LogReturnValueCode v 0) SUCCESS;
        errorCodeTyped = returnCode: LogMessage (LogReturnCode returnCode) ERROR;
        errorTyped = return.errorCodeTyped 1;
      };

      # Shorthand log.shell.fatal* for log.shell.exit.fatal*, since fatal always exits
      inherit (exit) fatal fatalCode fatalWithUsage;
    };
  };

  logBlock = codeBlockHeader "### Logging utilities" ''
    function __collective_log_debug_check() {
      if [[ "$__COLLECTIVE_DEBUG" == true ]]; then
        echo true
        return 0
      fi

      if [[ "$__COLLECTIVE_DEBUG_CHECKED" == true ]]; then
        if [[ "$__COLLECTIVE_DEBUG_ENABLED_BY_KV" == true ]]; then
          echo true
          return 0
        else
          echo false
          return 1
        fi
      fi

      # No re-entry
      export __COLLECTIVE_DEBUG_CHECKED=true

      THIS=$(basename $0)
      if [[ "$THIS" == cltv-kv-check || "$THIS" == cltv-gv-get ]]; then
        echo false
        return 1
      fi

      if [[ cltv-kv-check && "$(cltv-kv-get --key __COLLECTIVE_DEBUG)" == true ]]; then
        export __COLLECTIVE_DEBUG_ENABLED_BY_KV=true
        echo true
        return 0
      fi

      echo false
      return 1
    }

    function print-log-text() {
      LEVEL="$1"
      shift
      MSG="$@"
      DO_PRINT_LOG=true
      case "$LEVEL" in
        debug)
          if [[ "$(__collective_log_debug_check)" != true ]]; then
            return 1
          fi
          PREFIX=${toShellValue log.shell.prefix.debug}
          LOGINE=${toShellValue (log.shell.text.debug "$MSG")}
        ;;
        info)
          PREFIX=${toShellValue log.shell.prefix.info}
          LOGINE=${toShellValue (log.shell.text.info "$MSG")}
        ;;
        warn)
          PREFIX=${toShellValue log.shell.prefix.warn}
          LOGINE=${toShellValue (log.shell.text.warn "$MSG")}
        ;;
        error)
          PREFIX=${toShellValue log.shell.prefix.error}
          LOGINE=${toShellValue (log.shell.text.error "$MSG")}
        ;;
        fatal)
          PREFIX=${toShellValue log.shell.prefix.fatal}
          LOGINE=${toShellValue (log.shell.text.fatal "$MSG")}
        ;;
        success)
          PREFIX=${toShellValue log.shell.prefix.success}
          LOGINE=${toShellValue (log.shell.text.success "$MSG")}
        ;;
        *)
          print-log-text fatal "Unknown log level: $LEVEL (message: $MSG)"
          exit 1
        ;;
      esac

      INDENT_PAST_PREFIX=$((1+''${#PREFIX}))
      SPACES=$(for i in $(seq 1 $INDENT_PAST_PREFIX); do echo -n " "; done)
      ${echo-n "$PREFIX "}
      ${echo "$LOGINE"} | head -n1
      for l in $(echo $LOGLINE | tail -n+2); do
        ${echo "$SPACES$l"}
      done
    }

    function log() {
      print-log-text "$@" >&2
    }

    function log-with-usage() {
      if (log "$@"); then
        echo "" >&2
        usage
      fi
    }

    function __log-return() {
      CODE="$1"
      VALUE="$2"
      shift 2
      if (log "$@"); then
        if [[ -n "$VALUE" ]]; then
          echo "$VALUE"
        fi
      fi
    }

    function log-return() {
      __log-return "$@"
    }

    function log-return-with-usage() {
      if (__log-return "$@"); then
        echo "" >&2
        usage
      fi
    }

    function log-exit() {
      CODE="$1"
      log-return "$@"
      exit "$CODE"
    }

    function log-exit-with-usage() {
      CODE="$1"
      log-return-with-usage "$@"
      exit "$CODE"
    }

  '';

  _tests = with tests; suite {
    log.shell = with log.shell; {
      # Simplified, fast tests focusing on the core functionality
      levels = {
        info.text = expect.eq (info "its ok") ''log info "its ok"'';
        debug.text = expect.eq (debug "its ok") ''log debug "its ok"'';
        warn.text = expect.eq (warn "its bad") ''log warn "its bad"'';
        error.text = expect.eq (error "its bad") ''log error "its bad"'';
      };

      # Test a few key functions without heavy type operations  
      exit = {
        success = expect.eq (exit.success "its ok") ''log-exit 0 "its ok"'';
        fatal = expect.eq (exit.fatal "its bad") ''log-exit 1 "its bad"'';
      };
    };
  };
}
