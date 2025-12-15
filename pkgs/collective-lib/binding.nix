{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

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
  fnAnd = k: {
    mods = ["fn"];
    keys = [k];
  };
  modFnAnd = k: {
    mods = ["$mod" "fn"];
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
  skhdMod = m: if (m == "super") then "cmd" else if (m == "$mod") then "alt" else m;
  skhdMods = b:
    (map skhdMod b.mods)
    ++ (optionals (any (k: elem k [ "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12" ]) (skhdKeys b.keys)) [ "fn" ]);

  swayKeys = map (k: {
    "=" = "equal";
  }.${k} or k);
  hyprKeys = map (k: {
    "=" = "equal";
  }.${k} or k);
  skhdKeys = map (k: {
    "=" = "0x18";
    F1 = "f1";
    F2 = "f2";
    F3 = "f3";
    F4 = "f4";
    F5 = "f5";
    F6 = "f6";
    F7 = "f7";
    F8 = "f8";
    F9 = "f9";
    F10 = "f10";
    F11 = "f11";
    F12 = "f12";
    Return = "return";
  }.${k} or k);
  modPrefix = rec {
    # sway has "mod+mod+mod+key", empty mods are "key"
    sway = b: optionalString (b.mods != null && length b.mods > 0) "${strings.concatStringsSep "+" (swayMods b.mods)}+";
    i3 = sway;
    # Hyprland has "mod mod mod, keys", empty mods are ", keys"
    hypr = b: "${optionalString (b.mods != null && length b.mods > 0) (strings.concatStringsSep " " b.mods)}, ";
    skhd = b: 
      let mods = skhdMods b;
      in optionalString (mods != null && length mods > 0) "${strings.concatStringsSep " + " mods} - "; };
  keySuffix = rec {
    sway = b: strings.concatStringsSep "+" (swayKeys b.keys);
    i3 = sway;
    hypr = b: strings.concatStringsSep " " (hyprKeys b.keys);
    skhd = b: strings.concatStringsSep " " (skhdKeys b.keys);
  };
  mkBind = rec {
    sway = b: "${modPrefix.sway b}${keySuffix.sway b}";
    i3 = b: "${modPrefix.i3 b}${keySuffix.i3 b}";
    hypr = b: "${modPrefix.hypr b}${keySuffix.hypr b}";
    skhd = b: "${modPrefix.skhd b}${keySuffix.skhd b}";
  };
  mkExec = rec {
    sway = cmd: "exec '${cmd}'";
    i3 = sway;
    hypr = cmd: "exec, ${cmd}";
    skhd = cmd: cmd;
  };
  mkBindExec = rec {
    sway = (a: { ${mkBind.sway a.keybind} = mkExec.sway a.exec; });
    i3 = (a: { ${mkBind.i3 a.keybind} = mkExec.i3 a.exec; });
    hypr = (a: "${mkBind.hypr a.keybind}, ${mkExec.hypr a.exec}");
    skhd = (a: "${mkBind.skhd a.keybind} : ${mkExec.skhd a.exec}");
  };
}
