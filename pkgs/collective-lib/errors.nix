{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;

rec {
  # Take a list of [{cond = bool, msg = string}] and return
  # the combined msgs if any cond is true.
  doChecks = condMsgs:
    let errors = nonEmpties (map (e: optionalString (!e.cond) e.msg) condMsgs);
    in if errors == [] then true
       else assertMsg false (joinOptionalLines errors);
}
