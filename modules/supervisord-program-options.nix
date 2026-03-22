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
    stopsignal = mkOption {
      type = types.str;
      default = "TERM";
      description = "Signal to send when stopping the program.";
    };
    stopasgroup = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to send the stop signal to the whole process group.";
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
  };
}
