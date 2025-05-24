{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;

rec {
  # Take a list of [{cond = bool, msg = string}] and return the errors for false conditions.
  checkCondMsgs = condMsgs: map (x: x.msg) (filter (x: !x.cond) condMsgs);

  # Take a list of [{cond = bool, msg = string}] and assert the first error if any cond is true.
  # Only returns the first to avoid defensive checks for cascading conditions.
  doChecks = condMsgs:
    let errors = checkCondMsgs condMsgs;
    in if errors == [] then true
       else assertMsg false (head errors);

  doChecksNoAssert = condMsgs: checkCondMsgs condMsgs == [];

  # Take a list of [{pred = x: bool, msg = string}] and return the first errors for false preds on x.
  checkPredMsgs = predMsgs: x:
    let go = predMsgs:
          if predMsgs == [] then null
          else
            let predMsg = head predMsgs;
            in if predMsg.pred x then go (tail predMsgs)
               else predMsg.msg;
    in go predMsgs;

  # Take a list of [{pred = x: bool, msg = string}] and assert the first errors for false preds on x.
  predChecks = predMsgs: x:
    let msg = checkPredMsgs predMsgs x;
    in if msg == null then true else assertMsg false msg;
}
