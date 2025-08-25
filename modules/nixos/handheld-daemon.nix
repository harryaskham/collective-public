{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.handheld-daemon;
in {
  options.services.handheld-daemon.adjustor = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable the HHD Adjustor for TDP control.";
    };
    acpiCall = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable acpi_call at boot, required for Adjustor TDP control on most devices.";
      };
    };
  };

  config = mkIf (cfg.enable && cfg.adjustor.enable) (mkMerge [

    (let
      hhdPython = pkgs.python3.withPackages (ps: [ ps.handheld-daemon-adjustor ] );

      handheld-daemon-with-adjustor = pkgs.handheld-daemon.overrideAttrs (attrs: {
        # Ensure the Python adjustor can be seen by HHD:
        nativeBuildInputs = (attrs.nativeBuildInputs or []) ++ [ hhdPython.pkgs.wrapPython ];
        propagatedBuildInputs =
          (attrs.propagatedBuildInputs or [])
          ++ (with pkgs.python3Packages; [handheld-daemon-adjustor])
          ++ (with pkgs; [busybox]);
        postFixup = ''
          wrapProgram "$out/bin/hhd" \
            --prefix PYTHONPATH : "$PYTHONPATH" \
            --prefix PATH : "${hhdPython}/bin"
        '';

        # Substitutions don't handle the udev rules at least in nixos-unstable 2025-08-25
        postInstall = ''
          ${attrs.postInstall or ""}
          substituteInPlace $src/usr/lib/udev/rules.d/83-hhd.rules \
            --replace-fail "/bin/chmod" "${lib.getExe' pkgs.coreutils "chmod"}"
        '';
      });

    in rec {
      services.handheld-daemon.package = handheld-daemon-with-adjustor;
      # Adjustor assumes it can talk PPD protocol over dbus
      services.power-profiles-daemon.enable = true;
    })

    (mkIf cfg.adjustor.acpiCall.enable {
      boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
      boot.kernelModules = [ "acpi_call" ];
    })

  ]);

}
