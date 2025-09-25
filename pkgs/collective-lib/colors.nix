{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.typed;

rec {
  schemeType = types.submodule {
    options = {
      foreground = mkOption { type = types.nullOr types.str; default = null; };
      background = mkOption { type = types.nullOr types.str; default = null; };
      selection_foreground = mkOption { type = types.nullOr types.str; default = null; };
      selection_background = mkOption { type = types.nullOr types.str; default = null; };
      url_color = mkOption { type = types.nullOr types.str; default = null; };
      cursor = mkOption { type = types.nullOr types.str; default = null; };
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
  rawHex = c:
    if c == null then null
    else if substring 0 1 c == "#" then substring 1 (size c) c
    else c;
  withHash = c:
    if c == null then null
    else if substring 0 1 c == "#" then c 
    else "#${c}";
  with0x = c: "0x${rawHex c}";
  asRGB = c: "rgb(${rawHex c})";

  # Indexing into themes with formatting
  icol = scheme: i: scheme."color${toString i}";
  i0x = scheme: i: with0x (icol scheme i);
  ihex = scheme: i: withHash (icol scheme i);
  irgb = scheme: i: asRGB (icol scheme i);
  igrad = scheme: i: j: angle: "${irgb scheme i} ${irgb scheme j} ${toString angle}deg";

  mapSchemeReordered = colorF: ordering: scheme:
    assert that (length ordering == 16) "Invalid ordering length: ${toString (length ordering)} (${_l_ ordering})";
    mergeAttrsList (imap0 (i: j: { "color${toString i}" = colorF scheme j; }) ordering);

  toOrderedHashedColorList = scheme: (genList (ihex scheme) 16);

  # Scheme conversion for various systems

  fori3 = forSway;

  forSway = lighten: scheme: strings.concatLines(
    imap0 (i: c: ''set $color${toString i} ${c}'') (toOrderedHashedColorList scheme));

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
  forKitty = scheme: ''
    foreground ${def (ihex scheme 4) (withHex scheme.foreground)}
    background ${def (ihex scheme 0) (withHex scheme.background)}
    selection_foreground ${def (ihex scheme 0) (withHex scheme.selection_foreground)}
    selection_background ${def (ihex scheme 6) (withHex scheme.selection_background)}
    url_color ${def (ihex scheme 15) (withHex scheme.url_color)}
    cursor ${def (ihex scheme 4) (withHex scheme.cursor)}
    ${ # Include all other colors in expected order, not nord order
      # This loses a lot of color-space 
      let ordering = [1 11 14 13 9 15 8 5 3 11 14 13 9 15 7 6];
      in _ls_ (mapAttrsToList (k: v: "${k} ${v}") (mapSchemeReordered ihex ordering scheme))}
  '';

  forAlacritty = scheme: {
    primary = {
      background = def (i0x scheme 0) (with0x scheme.background);
      foreground = def (i0x scheme 4) (with0x scheme.foreground);
      dim_foreground = "0xa5abb6";  # TODO: in theme?
    };
    cursor = {
      text = i0x scheme 0;
      cursor = def (i0x scheme 4) (with0x scheme.cursor);
    };
    vi_mode_cursor = {
      text = i0x scheme 0;
      cursor = def (i0x scheme 4) (with0x scheme.cursor);
    };
    selection = {
      text = def (i0x scheme 0) (with0x scheme.selection_foreground);
      background = def (i0x scheme 3) (with0x scheme.selection_background);
    };
    search = {
      matches = {
        foreground = i0x scheme 0;
        background = i0x scheme 7;
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

  #services.mako.settings.*
  forMako = scheme: {
    background-color = ihex scheme 6;
    text-color = ihex scheme 1;
    border-color = ihex scheme 9;
  };

  forNixOnDroid = scheme: {
     background = def (ihex scheme 0) (withHex scheme.background);
     foreground = def (ihex scheme 4) (withHex scheme.foreground);
     cursor = def (ihex scheme 4) (withHex scheme.cursor);
  } // (mapSchemeReordered ihex [1 11 14 13 9 15 8 5 3 11 14 13 9 15 7 6] scheme);

  schemes = {
    nord = rec {
      color0 = "#2E3440";
      color1 = "#3B4252";
      color2 = "#434C5E";
      color3 = "#4C566A";
      color4 = "#D8DEE9";
      color5 = "#E5E9F0";
      color6 = "#ECEFF4";
      color7 = "#8FBCBB";
      color8 = "#88C0D0";
      color9 = "#81A1C1";
      color10 = "#5E81AC";
      color11 = "#BF616A";
      color12 = "#D08770";
      color13 = "#EBCB8B";
      color14 = "#A3BE8C";
      color15 = "#B48EAD";
      foreground = color4;
      background = color0;
      url_color = color10;
      selection_foreground = color0;
      selection_background = color6;
      cursor = color4;
    };

    # Just reverses colors 0-6
    nord-light = {
      color0 = "#ECEFF4";
      color1 = "#E5E9F0";
      color2 = "#D8DEE9";
      color3 = "#4C566A";
      color4 = "#434C5E";
      color5 = "#3B4252";
      color6 = "#2E3440";
      color7 = "#8FBCBB";
      color8 = "#88C0D0";
      color9 = "#81A1C1";
      color10 = "#5E81AC";
      color11 = "#BF616A";
      color12 = "#D08770";
      color13 = "#EBCB8B";
      color14 = "#A3BE8C";
      color15 = "#B48EAD";
      foreground = color4;
      background = color0;
      url_color = color10;
      selection_foreground = color0;
      selection_background = color6;
      cursor = color4;
    };
  };

  _tests = with tests; suite {
    toOrderedHashedColorList = 
      expect.eq
        (toOrderedHashedColorList schemes.nord) 
        [ "#2E3440" "#3B4252" "#434C5E" "#4C566A" "#D8DEE9" "#E5E9F0" "#ECEFF4" "#8FBCBB"
          "#88C0D0" "#81A1C1" "#5E81AC" "#BF616A" "#D08770" "#EBCB8B" "#A3BE8C" "#B48EAD" ];

    mapSchemeReordered =
      expect.eq
        (mapSchemeReordered i0x [1 11 14 13 9 15 8 5 3 11 14 13 9 15 7 6] schemes.nord)
        { 
           color0 = "0x3B4252";
           color1 = "0xBF616A";
           color2 = "0xA3BE8C";
           color3 = "0xEBCB8B";
           color4 = "0x81A1C1";
           color5 = "0xB48EAD";
           color6 = "0x88C0D0";
           color7 = "0xE5E9F0";
           color8 = "0x4C566A";
           color9 = "0xBF616A";
           color10 = "0xA3BE8C";
           color11 = "0xEBCB8B";
           color12 = "0x81A1C1";
           color13 = "0xB48EAD";
           color14 = "0x8FBCBB";
           color15 = "0xECEFF4";
        };

    forAlacritty =
      expect.eq
        (forAlacritty schemes.nord)
        { bright = { black = "0x4C566A";
                    blue = "0x81A1C1";
                    cyan = "0x8FBCBB";
                    green = "0xA3BE8C";
                    magenta = "0xB48EAD";
                    red = "0xBF616A";
                    white = "0xECEFF4";
                    yellow = "0xEBCB8B"; };
          cursor = { cursor = "0xD8DEE9";
                    text = "0x2E3440"; };
          dim = { black = "0x373e4d";
                  blue = "0x68809a";
                  cyan = "0x6d96a5";
                  green = "0x809575";
                  magenta = "0x8c738c";
                  red = "0x94545d";
                  white = "0xaeb3bb";
                  yellow = "0xb29e75"; };
          normal = { black = "0x3B4252";
                    blue = "0x81A1C1";
                    cyan = "0x88C0D0";
                    green = "0xA3BE8C";
                    magenta = "0xB48EAD";
                    red = "0xBF616A";
                    white = "0xE5E9F0";
                    yellow = "0xEBCB8B"; };
          primary = { background = "0x2E3440";
                      dim_foreground = "0xa5abb6";
                      foreground = "0xD8DEE9"; };
          search = { matches = { background = "0x8FBCBB";
                                foreground = "0x2E3440"; }; };
          selection = { background = "0xECEFF4";
                        text = "0x2E3440"; };
          vi_mode_cursor = { cursor = "0xD8DEE9";
                            text = "0x2E3440"; }; };
  };
}
