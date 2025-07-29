{ config, lib, pkgs, inputs, outputs, systemConfig, systemType, getHomeConfig, collectiveEnvironment, untyped, configType, ... }:

# Collects environment settings, enabling home-manager to set /etc if needed.

with untyped;
with untyped.data;

let
  cfg = config.agnostic.environment;
in {
  options.agnostic.environment = {
    enable = Opt.enable.default "Enable agnostic environment settings";
    etc = mkOption {
      type = types.attrs;
      default = {};
      description = "etc files to write";
    };
  };

  config = mkIf cfg.enable (
  with ConfigType;
    let
      etc = cfg.etc // configType.switch {
        System = (getHomeConfig config).agnostic.environment.etc;
        Home = systemConfig.agnostic.environment.etc;
      };
    in configType.switch {
      Home = {};
      System = { environment.etc = etc; };
    });
}
