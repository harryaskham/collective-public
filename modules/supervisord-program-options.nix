{ lib, typed }:

with typed;
with lib;

{ ... }: {
  options = {
    command = mkOption {
      type = types.str;
      description = "The command to execute.";
    };
    directory = mkOption {
      type = types.str;
      default = "/";
      description = "Working directory for the program.";
    };
    environment = mkOption {
      type = types.either (types.attrsOf types.str) types.str;
      default = {};
      description = "Environment variables for the program.";
    };
    path = mkOption {
      type = types.listOf types.package;
      default = [];
      description = "Packages to add to PATH for the program.";
    };
    autostart = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to start this program when supervisord starts.";
    };
    autorestart = mkOption {
      type = types.either types.bool (types.enum ["true" "false" "unexpected"]);
      default = true;
      description = "Whether to restart the program when it exits. Can be true, false, or \"unexpected\".";
    };
    startsecs = mkOption {
      type = types.int;
      default = 1;
      description = "Seconds the program must stay running to consider the start successful. Use 0 for oneshot.";
    };
    startretries = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = ''
        Failed starts supervisord allows before it gives up and marks the
        program FATAL, where it stays until someone starts it by hand. Each
        retry waits one second longer than the last. When null, a program that
        restarts (autorestart not false) gets 1000: about 5.8 days of retries,
        a few minutes apart at most, so a daemon that starts before what it
        needs, such as a listener on the tailnet address while the phone has
        none, comes up by itself once it can. A program that does not restart
        keeps supervisord's default of 3.
      '';
    };
    stopsignal = mkOption {
      type = types.str;
      default = "TERM";
      description = "Signal to send when stopping the program.";
    };
    stopwaitsecs = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = ''
        Optional seconds to wait for the process to stop after sending the stop
        signal. When null, omit the setting and preserve Supervisor's default.
      '';
    };
    stopasgroup = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to send the stop signal to the whole process group.";
    };
    killasgroup = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to send SIGKILL to the whole process group when stopping.";
    };
    redirect_stderr = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to redirect stderr to stdout.";
    };
    stdout_logfile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Optional stdout log path for the program.";
    };
    stderr_logfile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Optional stderr log path for the program.";
    };
    requiresPath = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Absolute path that must exist for the program to run, such as state a
        bootstrap step creates. While it is missing, supervisord-start leaves
        the program out and prints requiresHint instead of failing: supervisord
        refuses to start at all when a program's log file has no directory.
        Run supervisord-start again once the path exists to add the program.
      '';
    };
    requiresHint = mkOption {
      type = types.str;
      default = "";
      description = "Guidance printed while requiresPath is missing.";
    };
  };
}
