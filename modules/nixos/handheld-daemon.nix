{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.handheld-daemon;
in {
  options.services.handheld-daemon.adjustor = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Install the HHD Adjustor for TDP control.";
    };
    acpiCall = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable acpi_call at boot, required for Adjustor TDP control.";
      };
    };
  };

  config = mkIf (cfg.enable && cfg.adjustor.enable) (mkMerge [

    {
      environment.systemPackages = with pkgs; [
        handheld-daemon-adjustor
        python3Packages.handheld-daemon-adjustor
      ];
      services.handheld-daemon.package = pkgs.handheld-daemon.overrideAttrs (attrs: {
        nativeBuildInputs = [
          (pkgs.python3.withPackages (ps: [ ps.handheld-daemon-adjustor ]))
        ];
        dependencies = (attrs.dependencies or []) ++ (with pkgs; [
          python3Packages.handheld-daemon-adjustor
        ]);
      });
    }

    (mkIf cfg.adjustor.acpiCall.enable {
      boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
      boot.kernelModules = [ "acpi_call" ];
    })

  ]);

}
