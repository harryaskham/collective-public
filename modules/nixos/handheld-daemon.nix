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
  };

  config = mkIf (cfg.enable && cfg.adjustor.enable) (
    let adjustorPkgs = with pkgs; [
      # Makes `adjustor` available in the environment for forked execution
      handheld-daemon-adjustor
      # Makes hhd.adjustor Python library available to HHD for direct imported use
      python3Packages.handheld-daemon-adjustor
    ];
    in {
      environment.systemPackages = adjustorPkgs;
      services.handheld-daemon.package = pkgs.handheld-daemon.overrideAttrs (attrs: {
        propagatedBuildInputs = attrs.propagatedBuildInputs ++ adjustorPkgs;
      });
    });

}
