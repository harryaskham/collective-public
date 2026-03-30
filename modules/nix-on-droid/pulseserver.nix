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

  checkCmd = "PULSE_SERVER=${cfg.host}:${toString cfg.port} ${pkgs.pulseaudio}/bin/pactl info";
  actionCmd = "am start -n ${cfg.package}";
in {
  options.pulseserver = {
    enable = mkEnableOption "PulseServer Android app supervisor";

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "PulseServer listen address.";
    };

    port = mkOption {
      type = types.port;
      default = 4713;
      description = "PulseServer TCP port.";
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
