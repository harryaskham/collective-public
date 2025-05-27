{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;
with cutils.types;

# Printing/logging utilities
rec {
  printAttrs = x:
    if x == {} then "{}"
    else codeBlock ''
    {
      ${indent.here (codeBlockLines (mapAttrsToList (k: v: "${k} = ${print v};") x))};
    }
  '';

  printList = x:
    if x == [] then "[]"
    else "[ ${joinSep " " (map print x) } ]";

  # Convert a value of any type to a string, supporting the types module's Type values.
  print = x: 
    if Types.isTyped x && (x ? __toString)
      then toString x
      else {
        null = "null";
        path = x;
        string = ''"${x}"'';
        int = ''"${builtins.toJSON x}"'';
        float = ''"${builtins.toJSON x}"'';
        lambda = "<lambda>";
        list = printList x;
        set = printAttrs x;
        bool = if x then "true" else "false";
      }.${typeOf x};

}
