{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;

# Misc library fns
rec {
  # Options factories
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

  # Tracers
  traceId = a: trace a a;
  traceShow = a: b: trace (toString a) b;
  traceShowId = a: traceShow a a;

  # Logic
  ifDef = def: cond: _then: if cond then _then else def;
}
