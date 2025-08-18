{ config, lib, pkgs, outputs, untyped, ...}:

# D-Bus configuration and system bus daemon.
# Forked from https://raw.githubusercontent.com/NixOS/nixpkgs/refs/heads/master/nixos/modules/services/system/dbus.nix

with untyped.clib;

let
  cfg = config.dbus;
  dbus-start-bin = "dbus-start";
  dbus-start = pkgs.writeScriptBin dbus-start-bin ''
    #!${pkgs.runtimeShell}
    ${pkgs.dbus}/bin/dbus-daemon --session &
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

      build.activationAfter.dbus = ''
        mkdir -p $XDG_RUNTIME_DIR
        DBUS_PID=$(${pkgs.procps}/bin/ps -a | ${pkgs.toybox}/bin/grep dbus || true)
        if [ -z "$DBUS_PID" ]; then
          $DRY_RUN_CMD ${dbus-start}/bin/${dbus-start-bin}
        fi
      '';
    }

  ]);
}
