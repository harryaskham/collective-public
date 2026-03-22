{
  lib,
  typed,
  ...
}:
# Home-manager facing supervisord declarations for nix-on-droid.
# These declarations are rolled up by the nix-on-droid system module into the
# single system-level supervisord instance so user-scoped modules can declare
# long-running services without depending on a separate home-level process
# manager.
with typed;
with lib; let
  programOpts = {...}: {
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
        type = types.attrsOf types.str;
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
        type = types.either types.bool (types.enum ["unexpected"]);
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
    };
  };
in {
  options.supervisord = {
    programs = mkOption {
      type = types.attrsOf (types.submodule programOpts);
      default = {};
      description = ''
        User-declared supervisord programs. On nix-on-droid these are merged
        into the single system-level supervisord instance so home-manager
        modules can contribute managed services alongside system modules.
      '';
    };
  };
}
