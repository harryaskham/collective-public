{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

# Build monitor configs agnostically to the display manager / window manager
# Resolution and position are modelled as a list of [ width height ]
rec {
  # Resolution and refresh rate to string
  mkRes = res: rr: "${strings.concatStringsSep "x" (map toString res)}@${toString rr}Hz";
  mkPos = pos: "${strings.concatStringsSep " " (map toString pos)}";
  mkPosComma = pos: "${strings.concatStringsSep "," (map toString pos)}";
  mkPosX = pos: "${strings.concatStringsSep "x" (map toString pos)}";
  toTransform = t:
    if t == 1 then "90"
    else if t == 2 then "180"
    else if t == 3 then "270"
    else if t == 4 then "flipped"
    else if t == 5 then "flipped-90"
    else if t == 6 then "flipped-180"
    else if t == 7 then "flipped-270"
    else "normal";

  toSway = display: {
    ${display.defaultOutput} = {
      mode = mkRes display.resolution display.refreshRate;
      position = mkPos display.position;
      scale = toString display.scale;
      transform = toTransform display.transform;
      bg = "${display.backgroundImage} stretch";
      adaptive_sync = if display.vrr then "on" else "off";
    } // (optionalAttrs (display.bitDepth != null) {
      render_bit_depth = toString display.bitDepth;
    });
  };

  toKanshi = display: {
    criteria = display.name;
    status = if display.enable then "enable" else "disable";
    mode = mkRes display.resolution display.refreshRate;
    position = mkPosComma display.position;
    scale = display.scale;
    transform = toTransform display.transform;
    adaptiveSync = display.vrr;
    # Bitrate not supported in kanshi profile
    # https://github.com/nix-community/home-manager/blob/master/modules/services/kanshi.nix
  };

  toHyprSettings = display:
    if display.enable
    then (''${mkRes display.resolution display.refreshRate}, ${mkPosX display.position}, ${builtins.toJSON display.scale}''
    + (optionalString display.vrr ", vrr, 1")
    + (optionalString (display.bitDepth != null) '', bitdepth, ${toString display.bitDepth}'')
    + (optionalString (display.transform != null) '', transform, ${toString display.transform}''))
    else "disable";
  hyprName = display: if display.name != "" then ''desc:${display.name}'' else display.defaultOutput;
  toHypr = display: ''${hyprName display}, ${toHyprSettings display}'';

  # Display positioning and scaling + library
  _1080p = [1920 1080];
  _1200p = [1920 1200];
  _1600p = [2560 1600];
  _4k = [3840 2160];
  swap = dims: [ (elemAt dims 1) (elemAt dims 0) ];
  scaleFn = scale: x: (100 * x) / (builtins.floor (scale * 100.0));
  scaledRes = display:
    let width = scaleFn (display.scale) (elemAt display.resolution 0);
        height = scaleFn (display.scale) (elemAt display.resolution 1);
    in if ((display ? transform) && ((display.transform == 1) || (display.transform == 3)))
       then { width = height; height = width; }
       else { inherit width height; };
  pos = display:
    {
      left = elemAt display.position 0;
      top = elemAt display.position 1;
    };
  shiftBy = left: top: display:
    let p = pos display;
    in display // {
         position = [ (if (left == null) then p.left else p.left + left)
                      (if (top == null) then p.top else p.top + top)
                    ];
       };
  moveTo = left: top: display:
    let p = pos display;
    in display // {
         position = [ (if (left == null) then p.left else left)
                      (if (top == null) then p.top else top)
                    ];
       };

  # Builder functions for constructing semantic declarative monitor layouts
  underneath = display: moveTo null ((pos display).top + (scaledRes display).height);
  above = display: this: moveTo null ((pos display).top - (scaledRes this).height) this;
  rightOf = display: moveTo ((pos display).left + (scaledRes display).width) null;
  leftOf = display: this: moveTo ((pos display).left - (scaledRes this).width) null this;
  leftAlign = display: shiftBy (-1 * ((scaledRes display).width)) 0 display;
  rightAlign = display: shiftBy ((scaledRes display).width) 0 display;
  topAlign = display: shiftBy 0 (-1 * ((scaledRes display).height)) display;
  bottomAlign = display: shiftBy 0 ((scaledRes display).height) display;
  centerOfH = display: this: moveTo ((pos display).left + ((scaledRes display).width / 2) - ((scaledRes this).width / 2)) null this;
  centerOfV = display: this: moveTo null ((pos display).top + ((scaledRes display).height / 2) - ((scaledRes this).height / 2)) this;
  onPort = port: display: display // { defaultOutput = port; };
  onSpace = space: display: display // { defaultWorkspace = space; };

  mkKanshi = internal: primary: rest: {
    enable = true;
    profiles = {
      undocked = {
        displays = [ internal ];
      };
      docked = {
        displays = [ primary internal ];
      };
    } // (mergeAttrsList (map (other: {
        "docked-${other.nickname}" = {
          displays = [ internal primary other ];
        };
        "docked-clamshell-${other.nickname}" = {
          displays = [ (internal // { enable = false; }) primary other ];
        };
    }) rest));
  };
}
