{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.types;

# Printing/logging utilities
rec {

  # Convert a value of any type to a string, supporting the types module's Type values.
  print = x: {
    null = "null";
    path = x;
    string = ''"${x}"'';
    int = ''"${toJSON x}"'';
    float = ''"${toJSON x}"'';
    lambda = "<lambda>";
    list = ''[ ${joinSep " " (map print x) } ]'';
    set = codeBlock ''
      {
        ${indent.here (codeBlockLines (mapAttrs (k: v: "${k} = ${print v};") x))};
      }
    '';
    bool = if x then "true" else "false";
  }.${Types.typeName x} or (toString x);

}
