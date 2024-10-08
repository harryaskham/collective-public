{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.handheld-daemon;
in {
  options = {
    handheld-daemon = {
      enable = mkEnableOption "Enable the handheld-daemon service.";
      user = mkOption {
        type = types.str;
        description = "The user to run the daemon with.";
      };
      ui = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Enable the HHD UI overlay.";
        };
      };
      adjustor = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Install the HHD Adjustor for TDP control. Note that this is bundled with HHD as a dependency so is always enabled with HHD; this just exposes the CLI tool and python library to the system.";
        };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [

    {
      services.handheld-daemon = {
        enable = true;
        user = cfg.user;
      };
      environment.systemPackages = with pkgs; [
        handheld-daemon
        python3Packages.handheld-daemon-hhd
      ];
    }

    (mkIf cfg.ui.enable ({
      environment.systemPackages = with pkgs; [
        handheld-daemon-ui
      ];
    }))

    (mkIf cfg.adjustor.enable ({
      environment.systemPackages = with pkgs; [
        handheld-daemon-adjustor
        python3Packages.handheld-daemon-adjustor
      ];
    }))

  ]);
}
