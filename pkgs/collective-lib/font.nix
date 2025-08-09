{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

# Nicer whole-system font configuration
rec {
  style = types.submodule {
    options = {
      family = mkOption {
        description = "Font family i.e. FiraCode Nerd Font";
        type = types.str;
      };
      style = mkOption {
        description = "Font style i.e. Regular";
        type = types.str;
      };
    };
  };

  # Font configuration shared across terminal emulators
  termSpec = types.submodule {
    options = {
      regular = mkOption {type = style;};
      bold = mkOption {type = style;};
      italic = mkOption {type = style;};
      boldItalic = mkOption {type = style;};
      size = mkOption {type = types.float;};
    };
  };

  defaultTermSpec = {
    size = 12.0;
    regular = {
      family = "FiraCode Nerd Font";
      style = "Regular";
    };
    bold = {
      family = "FiraCode Nerd Font";
      style = "Bold";
    };
    italic = {
      family = "JetBrains Mono";
      style = "Italic";
    };
    boldItalic = {
      family = "JetBrains Mono";
      style = "Bold Italic";
    };
  };
  fullName = f: "${f.family} ${f.style}";
}
