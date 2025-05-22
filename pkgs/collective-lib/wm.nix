{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with cutils;
with cutils.clib;

# Utilities for configuring window management intents and bindings independently from the WM.
# i.e. maps desktop manipulation in a way that can be shared across sway/i3/hyprland/etc
let
  types = lib.types;
  mkOption = lib.mkOption;
  mkOptionDefault = lib.mkOptionDefault;
in rec {
  typeWMOption = boundCommand types.anything;
  typeWMOptions = types.listOf typeWMOption;
  commandWithArgs = argType: types.submodule {
    options = {
      tag = mkOption {
        description = "The type of command";
        type = types.enum [
          "goToWorkspace"
          "currentToWorkspace"
          "killCurrent"
          "fullscreen"
          "exitSession"
          "focusUp"
          "focusDown"
          "focusLeft"
          "focusRight"
          "moveWindowUp"
          "moveWindowDown"
          "moveWindowLeft"
          "moveWindowRight"
          "openTerminal"
          "openLauncher"
          "exec"
          "reloadConfig"
          "floatCurrent"
          "pseudofloatCurrent"
          "pinCurrent"
          "growUp"
          "growDown"
          "growLeft"
          "growRight"
          "screenshot"
          "volumeUp"
          "volumeDown"
          "volumeMute"
          "brightnessUp"
          "brightnessDown"
          "toggleOutput"
        ];
      };
      args = mkOption {
        type = argType;
        description = "The arguments for the command";
      };
    };
  };
  boundCommand = argType: types.submodule {
    options = {
      bind = mkOption {
        type = types.nullOr binding.bind;
        description = "The binding for the command";
      };
      cmd = mkOption {
        type = commandWithArgs types.int;
        description = "The command to bind";
      };
    };
  };
  mkB = b: c: { bind = b; cmd = c; };
  mkB1 = b: c: { bind = (binding.oneKey b); cmd = c; };
  mkBMod = b: c: { bind = (binding.modAnd b); cmd = c; };
  mkBSuper = b: c: { bind = (binding.superAnd b); cmd = c; };
  mkBSuperShift = b: c: { bind = (binding.superShiftAnd b); cmd = c; };
  mkBModShift = b: c: { bind = (binding.modShiftAnd b); cmd = c; };
  mkBModCtrlShift = b: c: { bind = (binding.modCtrlShiftAnd b); cmd = c; };
  mkKeybindCmd = wm: bind: cmd: {
    sway = {
      keybindings = {
        "${binding.mkBind.sway bind}" = cmd;
      };
    };
    i3 = {
      keybindings = {
        "${binding.mkBind.i3 bind}" = cmd;
      };
    };
    hyprland = {
      bind = [
        "${binding.mkBind.hypr bind}, ${cmd}"
      ];
    };
  }.${wm};
  actions = {
    goToWorkspace = workspaceIndex: {
      tag = "goToWorkspace";
      args = { n = workspaceIndex; };
    };
    exec = execCmd: {
      tag = "exec";
      args = { execCmd = execCmd; };
    };
    toggleOutput = disp: {
      tag = "toggleOutput";
      args = { disp = disp; };
    };
    currentToWorkspace = workspaceIndex: {
      tag = "currentToWorkspace";
      args = { n = workspaceIndex; };
    };
    killCurrent = { tag = "killCurrent"; };
    fullscreen = { tag = "fullscreen"; };
    exitSession = { tag = "exitSession"; };
    focusUp = { tag = "focusUp"; };
    focusDown = { tag = "focusDown"; };
    focusLeft = { tag = "focusLeft"; };
    focusRight = { tag = "focusRight"; };
    moveWindowUp = { tag = "moveWindowUp"; };
    moveWindowDown = { tag = "moveWindowDown"; };
    moveWindowLeft = { tag = "moveWindowLeft"; };
    moveWindowRight = { tag = "moveWindowRight"; };
    openTerminal = { tag = "openTerminal"; };
    openLauncher = { tag = "openLauncher"; };
    reloadConfig = { tag = "reloadConfig"; };
    floatCurrent = { tag = "floatCurrent"; };
    pseudofloatCurrent = { tag = "pseudofloatCurrent"; };
    pinCurrent = { tag = "pinCurrent"; };
    toggleSplit = { tag = "toggleSplit"; };
    growLeft = { tag = "growLeft"; };
    growRight = { tag = "growRight"; };
    growUp = { tag = "growUp"; };
    growDown = { tag = "growDown"; };
    screenshot = { tag = "screenshot"; };
    volumeUp = { tag = "volumeUp"; };
    volumeDown = { tag = "volumeDown"; };
    volumeMute = { tag = "volumeMute"; };
    brightnessUp = { tag = "brightnessUp"; };
    brightnessDown = { tag = "brightnessDown"; };
    nextWorkspace = { tag = "nextWorkspace"; };
    previousWorkspace = { tag = "previousWorkspace"; };
  };
  impl = wm: bc:
    (let
      bind = bc.bind;
      args = bc.cmd.args;
      runs = mkKeybindCmd wm bind;
    in rec {
      i3 = sway // {
        goToWorkspace = runs "workspace number ${toString args.n}";
        exec = runs ''exec "${toString args.execCmd}"'';
        toggleOutput = runs ''output "${args.disp.defaultOutput}" toggle'';
        currentToWorkspace = runs ''move container to workspace ${toString args.n}'';
        killCurrent = runs "kill";
        fullscreen = runs "fullscreen toggle";
        exitSession = runs ''exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit; xfce4-session-logout'"'';
        focusUp = runs "focus up";
        focusDown = runs "focus down";
        focusLeft = runs "focus left";
        focusRight = runs "focus right";
        moveWindowUp = runs "move up";
        moveWindowDown = runs "move down";
        moveWindowLeft = runs "move left";
        moveWindowRight = runs "move right";
        openTerminal = runs ''exec "alacritty"'';
        openLauncher = runs ''exec "rofi -show run -modi run"'';
        reloadConfig = runs "reload";
        floatCurrent = runs "floating toggle";
        pseudofloatCurrent = runs "floating toggle";
        pinCurrent = runs "sticky toggle";
        toggleSplit = runs "layout toggle split";
        growLeft = runs "resize shrink width 5 px or 5 ppt";
        growRight = runs "resize grow width 5 px or 5 ppt";
        growUp = runs "resize grow height 5 px or 5 ppt";
        growDown = runs "resize shrink height 5 px or 5 ppt";
        screenshot = runs ''exec "xfce4-screenshooter -c -r"'';
        volumeUp = runs ''exec "pactl set-sink-volume @DEFAULT_SINK@ +5%"'';
        volumeDown = runs ''exec "pactl set-sink-volume @DEFAULT_SINK@ -5%"'';
        volumeMute = runs ''exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"'';
        brightnessUp = runs ''exec "light -A 10"'';
        brightnessDown = runs ''exec "light -U 10"'';
        nextWorkspace = runs "workspace next";
        previousWorkspace = runs "workspace previous";
      };
      sway = {
        goToWorkspace = runs "workspace number ${toString args.n}";
        exec = runs ''exec "${toString args.execCmd}"'';
        toggleOutput = runs ''output "${args.disp.defaultOutput}" toggle'';
        currentToWorkspace = runs ''move container to workspace number ${toString args.n}'';
        killCurrent = runs "kill";
        fullscreen = runs "fullscreen toggle";
        exitSession = runs ''exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit' '';
        focusUp = runs "focus up";
        focusDown = runs "focus down";
        focusLeft = runs "focus left";
        focusRight = runs "focus right";
        moveWindowUp = runs "move up";
        moveWindowDown = runs "move down";
        moveWindowLeft = runs "move left";
        moveWindowRight = runs "move right";
        openTerminal = runs ''exec "kitty || xterm"'';
        openLauncher = runs ''exec "rofi -show run -modi run"'';
        reloadConfig = runs "reload";
        floatCurrent = runs "floating toggle";
        pseudofloatCurrent = runs "floating toggle";
        pinCurrent = runs "sticky toggle";
        toggleSplit = runs "layout toggle split";
        growLeft = runs "resize shrink width 5 px or 5 ppt";
        growRight = runs "resize grow height 5 px or 5 ppt";
        growUp = runs "resize grow width 5 px or 5 ppt";
        growDown = runs "resize shrink height 5 px or 5 ppt";
        screenshot = runs ''exec "grim -g \"$(slurp)\" ~/Pictures/$(date +\"screenshot-%Y-%m-%d-%H%M%S\").png"'';
        volumeUp = runs ''exec "pactl set-sink-volume @DEFAULT_SINK@ +5%"'';
        volumeDown = runs ''exec "pactl set-sink-volume @DEFAULT_SINK@ -5%"'';
        volumeMute = runs ''exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"'';
        brightnessUp = runs ''exec "light -A 10"'';
        brightnessDown = runs ''exec "light -U 10"'';
        nextWorkspace = runs "workspace next";
        previousWorkspace = runs "workspace previous";
      };
      hyprland = {
        goToWorkspace = runs "workspace, ${toString args.n}";
        exec = runs "exec, ${toString args.execCmd}";
        toggleOutput = runs ''exec, if [[ $(hyprctl -j monitors | jq ".[]" | jq "if .description == \"${args.disp.name}\" then .disabled else false end" | jq "if . == true then \"1\" else \"0\" end") ]]; then hyprctl keyword monitor 'desc:${args.disp.name}', disable; else hyprctl keyword monitor 'desc:${args.disp.name}', ${display.toHyprSettings (args.disp // { enable = true;  })}; fi 2>/dev/null'';
        currentToWorkspace = runs ''movetoworkspacesilent, ${toString args.n}'';
        killCurrent = runs "killactive";
        fullscreen = runs "fullscreen, 0";
        exitSession = runs ''exit'';
        focusUp = runs "movefocus, u";
        focusDown = runs "movefocus, d";
        focusLeft = runs "movefocus, l";
        focusRight = runs "movefocus, r";
        moveWindowUp = runs "movewindow, u";
        moveWindowDown = runs "movewindow, d";
        moveWindowLeft = runs "movewindow, l";
        moveWindowRight = runs "movewindow, r";
        openTerminal = runs ''exec, kitty || xterm'';
        openLauncher = runs ''exec , rofi -show run -modi run'';
        reloadConfig = runs "exec, hyprctl reload";
        floatCurrent = runs "togglefloating";
        pseudofloatCurrent = runs "pseudo";
        pinCurrent = runs "pin";
        toggleSplit = runs "swapsplit";
        growLeft = runs "resizeactive, -100 0";
        growRight = runs "resizeactive, 100 0";
        growUp = runs "resizeactive, 0 -100";
        growDown = runs "resizeactive, 0 100";
        screenshot = runs ''exec, grim -g "$(slurp)" ~/Pictures/$(date +'screenshot-%Y-%m-%d-%H%M%S').png'';
        volumeUp = runs "exec, pactl set-sink-volume @DEFAULT_SINK@ +5%";
        volumeDown = runs "exec, pactl set-sink-volume @DEFAULT_SINK@ -5%";
        volumeMute = runs "exec, pactl set-sink-mute @DEFAULT_SINK@ toggle";
        brightnessUp = runs "exec, light -A 10";
        brightnessDown = runs "exec, light -U 10";
        nextWorkspace = runs "workspace, +1";
        previousWorkspace = runs "workspace, -1";
      };
    }.${wm}.${bc.cmd.tag});
  toConfig = impl;
  toi3Config = toConfig "i3";
  toSwayConfig = toConfig "sway";
  toHyprlandSetting = toConfig "hyprland";
  toConfigs = wm: map (toConfig wm);
  toi3Configs = toConfigs "i3";
  toSwayConfigs = toConfigs "sway";
  toHyprlandSettings = toConfigs "hyprland";
}
