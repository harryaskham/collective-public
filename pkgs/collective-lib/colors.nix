{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

rec {
  schemeType = types.submodule {
    options = {
      color0 = mkOption { type = types.str; };
      color1 = mkOption { type = types.str; };
      color2 = mkOption { type = types.str; };
      color3 = mkOption { type = types.str; };
      color4 = mkOption { type = types.str; };
      color5 = mkOption { type = types.str; };
      color6 = mkOption { type = types.str; };
      color7 = mkOption { type = types.str; };
      color8 = mkOption { type = types.str; };
      color9 = mkOption { type = types.str; };
      color10 = mkOption { type = types.str; };
      color11 = mkOption { type = types.str; };
      color12 = mkOption { type = types.str; };
      color13 = mkOption { type = types.str; };
      color14 = mkOption { type = types.str; };
      color15 = mkOption { type = types.str; };
    };
  };
  mkSchemeOption = mkOption { type = schemeType; };

  # Formatting for configs
  toList = scheme: (genList (i: scheme."color${toString i}") 15);
  withHash = c: "#${c}";
  with0x = c: "0x${c}";
  asRGB = c: "rgb(${c})";

  # Indexing into themes with formatting
  icol = scheme: i: scheme."color${toString i}";
  i0x = scheme: i: with0x (icol scheme i);
  ihex = scheme: i: withHash (icol scheme i);
  irgb = scheme: i: asRGB (icol scheme i);
  igrad = scheme: i: j: angle: "${irgb scheme i} ${irgb scheme j} ${toString angle}deg";

  # Scheme conversion for various systems

  fori3 = forSway;

  forSway = lighten: scheme: strings.concatLines(
    imap0 (i: c: ''set $color${toString i} ${c}'') (map withHash (toList scheme)));

  forHyprland = (lighten: scheme: {
    misc = {
      background_color = irgb scheme (if lighten then 5 else 1);
    };
    decoration = {
      shadow = {
        color = irgb scheme 0;
        color_inactive = irgb scheme 0;
      };
    };
    general = {
      "col.inactive_border" = igrad scheme 3 0 45;
      "col.active_border" = igrad scheme 6 8 45;
      "col.nogroup_border" = igrad scheme 3 0 45;
      "col.nogroup_border_active" = igrad scheme 5 15 45;
    };
    group = {
      "col.border_inactive" = igrad scheme 3 0 45;
      "col.border_active" = igrad scheme 6 8 45;
      "col.border_locked_inactive" = igrad scheme 3 0 45;
      "col.border_locked_active" = igrad scheme 5 15 45;
      groupbar = {
        text_color = irgb scheme 4;
        "col.inactive" = igrad scheme 3 0 45;
        "col.active" = igrad scheme 6 8 45;
        "col.locked_inactive" = igrad scheme 3 0 45;
        "col.locked_active" = igrad scheme 5 15 45;
      };
    };
  });

  # At least for https://github.com/connorholyday/nord-kitty/blob/master/nord.conf
  # some colors are reused and bg/fg are not reused in the scheme
  # I then alter the cursor/url/selections to be from the scheme too
  forKitty = scheme: (
      let
        ix = i: withHash (icol scheme i);
        order = [1 11 14 13 9 15 8 5 3 11 14 13 9 15 7 6];
      in (
        ''
          foreground ${ix 4}
          background ${ix 0}
          selection_foreground ${ix 0}
          selection_background ${ix 6}
          url_color ${ix 15}
          cursor ${ix 4}
        ''
        + strings.concatLines(
          imap0 (i: cix: 
          ''color${toString i} ${ix cix}'')
          order)));

  forAlacritty = scheme: {
    primary = {
      background = i0x scheme 0;
      foreground = i0x scheme 4;
      dim_foreground = "0xa5abb6";  # TODO: in theme?
    };
    cursor = {
      text = i0x scheme 0;
      cursor = i0x scheme 4;
    };
    vi_mode_cursor = {
      text = i0x scheme 0;
      cursor = i0x scheme 4;
    };
    selection = {
      text = i0x scheme 0;
      background = i0x scheme 3;
    };
    search = {
      matches = {
        foreground = i0x scheme 0;
        background = i0x scheme 0;
      };
    };
    normal = {
      black = i0x scheme 1;
      red = i0x scheme 11;
      green = i0x scheme 14;
      yellow = i0x scheme 13;
      blue = i0x scheme 9;
      magenta = i0x scheme 15;
      cyan = i0x scheme 8;
      white = i0x scheme 5;
    };
    bright = {
      black = i0x scheme 3;
      red = i0x scheme 11;
      green = i0x scheme 14;
      yellow = i0x scheme 13;
      blue = i0x scheme 9;
      magenta = i0x scheme 15;
      cyan = i0x scheme 7;
      white = i0x scheme 6;
    };
    dim = {  # TODO: Not standard but from nord.yaml
      black = "0x373e4d";
      red = "0x94545d";
      green = "0x809575";
      yellow = "0xb29e75";
      blue = "0x68809a";
      magenta = "0x8c738c";
      cyan = "0x6d96a5";
      white = "0xaeb3bb";
    };
  };

  forMako = scheme: {
    services.mako.settings = {
      background-color = ihex scheme 6;
      text-color = ihex scheme 1;
      border-color = ihex scheme 9;
    };
  };

  forNixOnDroid = scheme: {
     background = ihex scheme 0;
     foreground = ihex scheme 4;
     cursor = ihex scheme 4;
  } // (let
          ix = i: ihex scheme i;
          order = [1 11 14 13 9 15 8 5 3 11 14 13 9 15 7 6];
         in listToAttrs (imap0 (i: cix: {
           name = "color${toString i}";
           value = ix cix;
         }) order));

  schemes = {
    nord = {
      color0 = "2E3440";
      color1 = "3B4252";
      color2 = "434C5E";
      color3 = "4C566A";
      color4 = "D8DEE9";
      color5 = "E5E9F0";
      color6 = "ECEFF4";
      color7 = "8FBCBB";
      color8 = "88C0D0";
      color9 = "81A1C1";
      color10 = "5E81AC";
      color11 = "BF616A";
      color12 = "D08770";
      color13 = "EBCB8B";
      color14 = "A3BE8C";
      color15 = "B48EAD";
    };
  };
}
