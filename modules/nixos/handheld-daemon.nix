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

    (let adjustorPkgs = with pkgs; [
      handheld-daemon-adjustor
      python3Packages.handheld-daemon-adjustor
    ];
    in {
      # environment.systemPackages = adjustorPkgs;
      services.handheld-daemon.package = pkgs.handheld-daemon.overrideAttrs (attrs: {
        propagatedBuildInputs = attrs.propagatedBuildInputs ++ adjustorPkgs;
      });
    })

    (mkIf cfg.adjustor.acpiCall.enable {
      boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
      boot.kernelModules = [ "acpi_call" ];
    })

  ]);

}
