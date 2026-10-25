{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.handheld-daemon;
  ppdPackage = config.services.power-profiles-daemon.package;

  # Adjustor implements the PPD API itself. Keep the CLI and bus permissions,
  # but not PPD's D-Bus activation files: systemPackages also feed the system
  # bus configuration, so installing the full package could start PPD again.
  ppdCompat = pkgs.runCommand "handheld-daemon-power-profiles-compat" {} ''
    mkdir -p "$out/bin" "$out/share/dbus-1/system.d"
    ln -s ${lib.getExe' ppdPackage "powerprofilesctl"} "$out/bin/powerprofilesctl"
    cp ${ppdPackage}/share/dbus-1/system.d/net.hadess.PowerProfiles.conf \
       ${ppdPackage}/share/dbus-1/system.d/org.freedesktop.UPower.PowerProfiles.conf \
       "$out/share/dbus-1/system.d/"
  '';

  # Adjustor launches its PPD server as `sys.executable -m adjustor...`. On Nix
  # the bare interpreter lacks the wrapper's site-packages, so the child fails
  # with ModuleNotFoundError and no power-profile bus service appears. Pass the
  # parent's import path; --replace-fail flags any upstream launcher change.
  ppdLauncherFix = package:
    package.overrideAttrs (old: {
      postPatch =
        (old.postPatch or "")
        + ''
          substituteInPlace src/adjustor/drivers/gpu/__init__.py \
            --replace-fail '[sys.executable, "-m", "adjustor.drivers.gpu.ppd"],' \
              '[sys.executable, "-m", "adjustor.drivers.gpu.ppd"], env={**os.environ, "PYTHONPATH": os.pathsep.join(sys.path)},'
        '';
    });
in {
  imports = [
    # The upstream NixOS module now owns Adjustor packaging and acpi_call.
    (lib.mkRenamedOptionModule
      ["services" "handheld-daemon" "adjustor" "acpiCall" "enable"]
      ["services" "handheld-daemon" "adjustor" "loadAcpiCallModule"])
  ];

  config = lib.mkIf (cfg.enable && cfg.adjustor.enable) {
    services.handheld-daemon.package = lib.mkDefault (ppdLauncherFix pkgs.handheld-daemon);

    # Desktop environments may enable PPD by default. There must be only one
    # owner: an active PPD or TuneD makes Adjustor disable energy management.
    services.power-profiles-daemon.enable = lib.mkForce false;
    services.tuned.enable = lib.mkForce false;
    # nixos-hardware's laptop profile enables TLP when PPD is off. TLP sets the
    # same platform profile, governor, EPP and boost knobs as Adjustor.
    services.tlp.enable = lib.mkForce false;

    environment.systemPackages = [ppdCompat];
    services.dbus = {
      enable = true;
      packages = [ppdCompat];
    };
    systemd.services.handheld-daemon = {
      requires = ["dbus.service"];
      after = ["dbus.service"];
      restartTriggers = [ppdCompat];
    };
  };
}
