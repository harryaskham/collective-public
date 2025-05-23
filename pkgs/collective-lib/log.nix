{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.types;

# Printing/logging utilities
rec {
  printAttrs = x: codeBlock ''
    {
      ${indent.here (codeBlockLines (mapAttrsToList (k: v: "${k} = ${print v};") x))};
    }
  '';

  # Convert a value of any type to a string, supporting the types module's Type values.
  print = x: 
    if Types.isTyped x
      then toString x
      else {
        null = "null";
        path = x;
        string = ''"${x}"'';
        int = ''"${builtins.toJSON x}"'';
        float = ''"${builtins.toJSON x}"'';
        lambda = "<lambda>";
        list = ''[ ${joinSep " " (map print x) } ]'';
        set = printAttrs x;
        bool = if x then "true" else "false";
      }.${typeOf x};

}
