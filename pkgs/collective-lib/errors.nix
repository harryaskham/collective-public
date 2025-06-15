{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with cutils.strings;

rec {
  # Is x the result of calling builtins.tryEval
  isTryEvalResult = x: isAttrs x && x ? success && x ? value;

  # Is x the successful result of calling builtins.tryEval
  isTryEvalSuccess = x: isTryEvalResult x && x.success;

  # Is x the failed result of calling builtins.tryEval
  isTryEvalFailure = x: isTryEvalResult x && !x.success;

  # Evaluate the given expression strictly, catching any catchable errors and handling with catch.
  # Cannot catch language failures, only assertions.
  # catch is a function (error: continuation). For most assertions, the argument is just error == false.
  try = expr: catch:
    let res = builtins.tryEval expr;
    in if isTryEvalSuccess res then res.value
       else if isTryEvalFailure res then catch res.value
       else throw (indent.block ''
         Unexpected result from builtins.tryEval:
           ${indent.here (log.print res)}
       '');

  # Try to evaluate expr and return true iff evaluation succeeds.
  tryBool = expr:
    let res = builtins.tryEval expr;
    in if isTryEvalSuccess res then true
       else if isTryEvalFailure res then false
       else throw (indent.block ''
         Unexpected result from builtins.tryEval:
           ${indent.here (log.print res)}
       '');

  # Try to evaluate expr and return either the result, or the defaultValue if evaluation fails.
  tryOr = defaultValue: expr: try expr (_: defaultValue);

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
