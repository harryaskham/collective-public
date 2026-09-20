{ config, lib, pkgs, typed, ... }:

# pulseserver: supervises the PulseServer Android app.
#
# Runs am-supervisor under supervisord to health-check the PulseAudio
# TCP server on 127.0.0.1:4713 and restart the Android app via
# `am start` when it goes down. Uses exponential backoff.

with typed;
with lib;

let
  cfg = config.pulseserver;
  amSupervisor = pkgs.am-supervisor;

  checkCmd = pkgs.writeShellScript "pulseserver-check" ''
    export PULSE_SERVER=${escapeShellArg "tcp:${cfg.host}:${toString cfg.port}"}
    export PULSE_PACTL=${pkgs.pulseaudio}/bin/pactl
    export PULSE_TIMEOUT=${pkgs.coreutils}/bin/timeout
    set -- ${escapeShellArgs cfg.virtualSinks}
    ${builtins.readFile ./pulseserver-check.sh}
  '';
  actionCmd = "am start -n ${cfg.package}";
in {
  options.pulseserver = {
    enable = mkEnableOption "PulseServer Android app supervisor";

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Client address used to check the app-owned server, not its bind address. The Android app controls its own wildcard listener.";
    };

    port = mkOption {
      type = types.port;
      default = 4713;
      description = "PulseServer TCP port.";
    };

    virtualSinks = mkOption {
      type = types.listOf (types.strMatching "[A-Za-z0-9_.-]+");
      default = [];
      description = ''
        Null sinks to ensure on every successful health check. The Android app
        generates its own private default.pa; reconciliation recreates these
        sinks after app/daemon restarts without changing default devices or routes.
      '';
    };

    package = mkOption {
      type = types.str;
      default = "com.harryaskham.pulse/.MainActivity";
      description = "Android component name for am start.";
    };

    checkInterval = mkOption {
      type = types.int;
      default = 10;
      description = "Seconds between health checks when healthy.";
    };

    backoffBase = mkOption {
      type = types.int;
      default = 5;
      description = "Initial backoff in seconds after a failed check.";
    };

    backoffMax = mkOption {
      type = types.int;
      default = 300;
      description = "Maximum backoff ceiling in seconds.";
    };
  };

  config = mkIf cfg.enable {
    environment.packages = [
      pkgs.pulseaudio  # provides pactl
      amSupervisor
    ];

    supervisord.programs.pulseserver-supervisor = {
      command = concatStringsSep " " [
        "${amSupervisor}/bin/am-supervisor"
        "--name PulseServer"
        "--check '${checkCmd}'"
        "--action '${actionCmd}'"
        "--interval ${toString cfg.checkInterval}"
        "--backoff-base ${toString cfg.backoffBase}"
        "--backoff-max ${toString cfg.backoffMax}"
      ];
      autostart = true;
      autorestart = true;
      startsecs = 0;
    };
  };
}
