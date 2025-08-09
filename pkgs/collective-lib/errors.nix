{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.strings;
with collective-lib.functions;

let typed = collective-lib.typed;
in rec {
  # Is x the result of calling builtins.tryEval
  isTryEvalResult = x: isAttrs x && x ? success && x ? value;

  # Is x the successful result of calling builtins.tryEval
  isTryEvalSuccess = x: isTryEvalResult x && x.success;

  # Is x the failed result of calling builtins.tryEval
  isTryEvalFailure = x: isTryEvalResult x && !x.success;

  # Evaluate the given expression, catching any catchable errors and handling with catch.
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

  # Evaluate the given expression strictly, catching any catchable errors and handling with catch.
  tryStrict = expr: catch: try (strict expr) catch;

  # Try to evaluate expr and return true iff evaluation succeeds.
  tryBool = expr:
    isTryEvalSuccess (builtins.tryEval (strict expr));

  # Try to evaluate expr and return either the result, or the defaultValue if evaluation fails.
  tryOr = defaultValue: expr: try (strict expr) (_: defaultValue);

  # Take a list of [{cond = bool, msg = string}] and assert the first error if any cond is true.
  # Returns all success messages if all conds are true.
  # tryBool to avoid throwing.
  checks = cs:
    let
      prefix = c:
        if c ? name then "CHECK(${c.name})"
        else "CHECK";
      mkFailedMsg = c: "${prefix c} failed: ${c.msg}";
      go = cs:
          if cs == [] then true
          else
            let c = head cs;
            in
              assert assertMsg c.cond (mkFailedMsg c);
              go (tail cs);
    in go cs;

  checksBool = cs: tryBool (checks cs);

  # Take a list of [{pred = x: bool, msg = string}] and return the first errors for false preds on x.
  checkPredMsgs = x: predMsgs:
    let go = predMsgs:
          if predMsgs == [] then null
          else
            let predMsg = head predMsgs;
            in if predMsg.pred x then go (tail predMsgs)
               else predMsg.msg;
    in go predMsgs;

  # Take a list of [{pred = x: bool, msg = string}] and assert the first errors for false preds on x.
  predChecks = x: predMsgs:
    let msg = checkPredMsgs x predMsgs;
    in if msg == null then true else assertMsg false msg;

  _tests = with typed.tests; suite {
    try = {
      int = expect.eq (try 1 (_: 0)) 1;
      throw = expect.eq (try (throw "error") (_: 0)) 0;
      deep = expect.eqOn attrNames (try ({a = throw "error";}) (_: 0)) {a=null;};
    };
    tryStrict = {
      int = expect.eq (tryStrict 1 (_: 0)) 1;
      throw = expect.eq (tryStrict (throw "error") (_: 0)) 0;
      deep = expect.eq (tryStrict ({a = throw "error";}) (_: 0)) 0;
    };
    tryOr = {
      int = expect.eq (tryOr 0 1) 1;
      throw = expect.eq (tryOr 0 (throw "error")) 0;
      deep = expect.eq (tryOr 0 ({a = throw "error";})) 0;
    };
    tryBool = {
      int = expect.eq (tryBool 1) true;
      throw = expect.eq (tryBool (throw "error")) false;
      deep = expect.eq (tryBool ({a = throw "error";})) false;
    };
  };
}
