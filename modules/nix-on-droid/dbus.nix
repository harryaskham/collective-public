{ config, lib, pkgs, outputs, untyped, ...}:

# D-Bus configuration and system bus daemon.
# Forked from https://raw.githubusercontent.com/NixOS/nixpkgs/refs/heads/master/nixos/modules/services/system/dbus.nix

with untyped.clib;

let
  cfg = config.dbus;
  dbus-start-bin = "dbus-start";
  # Pinned session bus socket: stable path so shell.init can export
  # DBUS_SESSION_BUS_ADDRESS, preventing glib/gtk apps from autolaunching
  # their own dbus-daemon (which leaks one process + tmpfs inodes per call,
  # eventually filling /tmp and breaking nix-on-droid switch).
  dbus-socket-path = "/tmp/run/dbus-session.socket";
  dbus-bus-address = "unix:path=${dbus-socket-path}";
  # Foreground dbus-daemon for supervisord management.
  dbus-start = pkgs.writeScriptBin dbus-start-bin ''
    #!${pkgs.runtimeShell}
    mkdir -p ''${XDG_RUNTIME_DIR:-/tmp/run}
    rm -f ${dbus-socket-path}
    exec ${pkgs.dbus}/bin/dbus-daemon \
      --session \
      --nofork \
      --address=${dbus-bus-address}
  '';
  inherit (untyped) mkOption mkEnableOption mkIf mkMerge types;
in
{
  options.dbus = {
    enable = mkEnable
      ''
        Whether to start the D-Bus message bus daemon, which is
        required by many other system services and applications.
      '';

    packages = mkOption {
      type = types.listOf types.path;
      default = [ ];
      description = ''
        Packages whose D-Bus configuration files should be included in
        the configuration of the D-Bus system-wide or session-wide
        message bus.
      '';
    };

    suidHelper = mkNestedEnable
      ''
        Whether to enable setting the suidHelper to
        `dbus-daemon-launch-helper`. On nix-on-droid we can't use
        security.wrappers to create the root-owned wrapper that
        apparmor wants, but this may still work.
      '';

    apparmor =
      mkDefaultOption
      (types.enum ["enabled" "disabled" "required"])
      "disabled"
      ''Whether to enable apparmor.'';

  };

  config = mkIf cfg.enable (mkMerge [
    {
      environment.etc."dbus-1".source = pkgs.makeDBusConf.override {
        inherit (cfg) apparmor;
        suidHelper =
          if cfg.suidHelper.enable
            then "${pkgs.dbus}/libexec/dbus-daemon-launch-helper"
            else "/bin/false";
        serviceDirectories = [ pkgs.dbus ] ++ cfg.packages;
      };
      environment.packages = [
        dbus-start
        pkgs.dbus
      ];

      # Managed by supervisord — auto-restarts on app restart
      supervisord.programs.dbus = {
        command = "${dbus-start}/bin/${dbus-start-bin}";
        autostart = true;
        autorestart = true;
        startsecs = 1;
      };

      # Export the pinned bus address so glib/gtk/qt apps connect to the
      # supervisord-managed daemon instead of autolaunching their own.
      shell.env.DBUS_SESSION_BUS_ADDRESS = dbus-bus-address;
    }

  ]);
}
