{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib;
with typed; 
let 
  inherit (Types.TS) int;
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
  _l_ = x: with _P_; put x _line ___;
  _ls_ = indent.lines;
  _h_ = indent.here;
  _p_ = indent.print;
  _ph_ = x: _h_ (_p_ x);
  _P_ = log.prints;
  _pv_ = indent.vprint;
  _pvh_ = x: _h_ (_pv_ x);
  _pd_ = d: x: with _P_; putD d x ___;
  _pdh_ = d: x: _h_ (_pd_ d x);
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
