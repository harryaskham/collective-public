{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;

# Misc library fns
rec {
  # Options factories
  Opt = {
    def = mkDefaultOption;

    enum = {
      __functor = self: mkEnum;
      def = mkEnumDef;
    };

    enable = {
      __functor = self: mkEnable;
      default = mkDefaultEnable;
      nested = {
        __functor = self: mkNestedEnable;
        default = mkNestedDefaultEnable;
      };
    };
  };

  # Legacy options factories
  mkEnum = (values: desc: mkOption {
    type = types.enum values;
    description = desc;
  });

  mkEnumDef = (def: values: desc: mkEnum values desc // { default = def; });

  mkEnable = mkEnableOption;

  mkDefaultEnable = (desc: mkOption {
    type = types.bool;
    default = true;
    description = desc;
  });

  mkNestedEnable = (desc: {
    enable = mkEnableOption desc;
  });

  mkNestedDefaultEnable = (desc: {
    enable = mkDefaultEnable desc;
  });

  mkDefaultOption = (type: default: description: mkOption {
    inherit type default description;
  });

  mkDefaultNull = type: mkDefaultOption (types.nullOr type) null;
}
