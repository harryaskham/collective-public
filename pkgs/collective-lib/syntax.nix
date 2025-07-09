{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.errors;
with collective-lib.lists;
with collective-lib.strings;
with collective-lib.tests;

let
  log = collective-lib.log;
  errors = collective-lib.errors;
  typelib = collective-lib.typelib;
  typed = collective-lib.typed;
in rec {
  # For use as:
  # with lets { x = 123; }; x + 1
  # or
  # with lets rec { x = 1; y = x + 1; }; y
  # 
  # Anonymous functions are renamed to their attr key:
  # with lets { f = fn {a = Int;} (a: int a + 1); }; f 123 <- logs "f(...)"
  lets = vars_:
    assert assertMsg (isAttrs vars_) ''Non-attrset vars in 'lets': ${log.print vars_}'';
    let 
      maybeSetCallName = name: v:
        if v.__isAnonymousFunction or false then v.setCallName name else v;
      vars = mapAttrs maybeSetCallName vars_;
    in 
      vars;

  # Shorthand print syntax.
  _b_ = indent.block;
  _bs_ = indent.blocks;
  _l_ = x: _P_ x _line ___;
  _ls_ = indent.lines;
  _p_ = indent.print;
  _ph_ = x: _h_ (_p_ x);
  _P_ = log.prints;
  _pv_ = indent.vprint;
  _pd_ = indent.vprintD;
  _h_ = indent.here;
  _throw_ = x: throw (_b_ x);
  that = cond: x: assertMsg cond (_b_ x);

  # <nix>syntax._tests.run</nix>
  _tests = with collective-lib.tests; suite {
    lets = with typed; {
      const = expect.eq (with lets { a = 1; }; a) 1;
      constRec = expect.eq (with lets rec { a = 1; b = a + 1;}; b) 2;
      fns = expect.eq (with lets { f = fn {a = Int;} (a: int a + 1); }; f 1) 2;
      fnsRec = expect.eq (with lets rec { x = 1; f = fn {a = Int;} (a: int a + x); }; f 1) 2;
    };
  };

}
