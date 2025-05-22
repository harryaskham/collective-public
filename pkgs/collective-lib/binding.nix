{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.clib;

# Generate keybindings for i3, sway, and hyprland
rec {
  bind = types.submodule {
    options = {
      mods = mkOption {
        type = types.nullOr (types.listOf types.str);
        description = "The mod keys of the binding";
      };
      keys = mkOption {
        type = types.nullOr (types.listOf types.str);
        description = "The keys of the binding";
      };
    };
  };
  oneKey = k: {
    mods = [];
    keys = [k];
  };
  modAnd = k: {
    mods = ["$mod"];
    keys = [k];
  };
  superAnd = k: {
    mods = ["super"];
    keys = [k];
  };
  superShiftAnd = k: {
    mods = ["super" "shift"];
    keys = [k];
  };
  modSuperAnd = k: {
    mods = ["$mod" "super"];
    keys = [k];
  };
  modShiftAnd = k: {
    mods = ["$mod" "shift"];
    keys = [k];
  };
  modCtrlAnd = k: {
    mods = ["$mod" "ctrl"];
    keys = [k];
  };
  modCtrlShiftAnd = k: {
    mods = ["$mod" "ctrl" "shift"];
    keys = [k];
  };
  swayMod = m: if (m == "super") then "Mod4" else m;
  swayMods = ms: map swayMod ms;
  modPrefix = rec {
    # sway has "mod+mod+mod+key", empty mods are "key"
    sway = b: optionalString (b.mods != null && length b.mods > 0) "${strings.concatStringsSep "+" (swayMods b.mods)}+";
    i3 = sway;
    # Hyprland has "mod mod mod, keys", empty mods are ", keys"
    hypr = b: "${optionalString (b.mods != null && length b.mods > 0) (strings.concatStringsSep " " b.mods)}, ";
  };
  keySuffix = rec {
    sway = b: strings.concatStringsSep "+" b.keys;
    i3 = sway;
    hypr = b: strings.concatStringsSep " " b.keys;
  };
  mkBind = rec {
    sway = b: "${modPrefix.sway b}${keySuffix.sway b}";
    i3 = b: "${modPrefix.i3 b}${keySuffix.i3 b}";
    hypr = b: "${modPrefix.hypr b}${keySuffix.hypr b}";
  };
  mkExec = rec {
    sway = cmd: "exec '${cmd}'";
    i3 = sway;
    hypr = cmd: "exec, ${cmd}";
  };
  mkBindExec = rec {
    sway = (a: { ${mkBind.sway a.keybind} = mkExec.sway a.exec; });
    i3 = (a: { ${mkBind.i3 a.keybind} = mkExec.i3 a.exec; });
    hypr = (a: "${mkBind.hypr a.keybind}, ${mkExec.hypr a.exec}");
  };
  mkOptionalBindExec = {
    hypr = (a: optionals a.enable [(mkBindExec.hypr a)]);
  };
}
