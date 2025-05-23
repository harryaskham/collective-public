{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;

rec {
  # Take a list of [{cond = bool, msg = string}] and return
  # the first error if any cond is true.
  # Only returns the first to avoid defensive checks for cascading conditions.
  doChecks = condMsgs:
    let errors = filter (x: x != "") (map (e: optionalString (!e.cond) e.msg) condMsgs);
    in if errors == [] then true
       else assertMsg false (head errors);

  doChecksNoAssert = condMsgs:
    let errors = filter (x: x != "") (map (e: optionalString (!e.cond) e.msg) condMsgs);
    in errors == [];

  predChecks = predMsgs: x:
    let go = predMsgs:
          if predMsgs == [] then true
          else 
            let predMsg = head predMsgs;
            in if predMsg.pred x then go (tail predMsgs)
               else assertMsg false predMsg.msg;
    in go predMsgs;
}
