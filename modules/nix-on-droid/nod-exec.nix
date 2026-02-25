{ config, lib, pkgs, typed, ... }:

# nod-exec: TCP command server for Nix-on-Droid.
#
# Exposes a lightweight TCP endpoint (default :18357) inside proot
# that external Android apps can connect to for running commands
# in the full Nix environment.
#
# Use cases:
#   - Tasker actions: Run Shell → nc 127.0.0.1 18357 with command
#   - KWGT formulas: $sh("echo 'uptime' | nc 127.0.0.1 18357")$
#   - Custom floating terminal APK (nod-float)
#   - Any app that can open a TCP socket to localhost
#
# Managed by supervisord — auto-restarts on crash/app restart.

with typed;
with lib;

let
  cfg = config.nod-exec;

  nodExecPkgs = pkgs.nod-exec.override {
    port = toString cfg.port;
    host = cfg.host;
  };
in {
  options.nod-exec = {
    enable = mkEnableOption "nod-exec TCP command server";

    port = mkOption {
      type = types.port;
      default = 18357;
      description = "TCP port for the nod-exec server (localhost only).";
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Bind address for the nod-exec server.";
    };
  };

  config = mkIf cfg.enable {
    environment.packages = [
      nodExecPkgs.server
      nodExecPkgs.client
      nodExecPkgs.nc
    ];

    # Copy the lightweight Android client to shared storage
    # so Tasker/KWGT can use it without needing nix paths
    termux.sharedDir.copy."nod-exec-android.sh" =
      "${nodExecPkgs.android}";

    # Run under supervisord — auto-starts, auto-restarts
    supervisord.programs.nod-exec = {
      command = "${nodExecPkgs.server}/bin/nod-exec-server";
      autostart = true;
      autorestart = true;
      startsecs = 1;
      environment = {
        NOD_EXEC_PORT = toString cfg.port;
        NOD_EXEC_HOST = cfg.host;
      };
    };
  };
}
