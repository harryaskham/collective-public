{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.handheld-daemon;
in {
  options.handheld-daemon.adjustor = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Install the HHD Adjustor for TDP control.";
    };
  };

  config = mkIf cfg.adjustor.enable {
    environment.systemPackages = with pkgs; [
      handheld-daemon-adjustor
      python3Packages.handheld-daemon-adjustor
    ];
  };

}
