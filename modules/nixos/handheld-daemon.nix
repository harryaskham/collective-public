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
      # environment.systemPackages = with pkgs.python3Packages; [ handheld-daemon-adjustor ];
      services.handheld-daemon.package = pkgs.handheld-daemon.overrideAttrs (attrs:
        let python =
              pkgs.python3.override {
                packageOverrides = pyfinal: pyprev: {
                  inherit (pkgs.python3Packages) handheld-daemon-adjustor;
                };
              };
            pythonWithAdjustor = python.withPackages (ps: [ ps.handheld-daemon-adjustor ]);
        in {
          buildInputs = (attrs.buildInputs or []) ++ [ pythonWithAdjustor ];
          nativeBuildInputs = (attrs.nativeBuildInputs or []) ++ [ pythonWithAdjustor ];
          propagatedBuildInputs = (attrs.propagatedBuildInputs or []) ++ [ pythonWithAdjustor ];
        });
    }

    (mkIf cfg.adjustor.acpiCall.enable {
      boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
      boot.kernelModules = [ "acpi_call" ];
    })

  ]);

}
