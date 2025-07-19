{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

# Create a callable lambda function from a string.
# Exposed as eval.fn in default.nix

with collective-lib.typed;
rec {
  # Default to AST-based txtfns.
  __functor = self: self.ast;

  # Use eval to create a callable lambda function from a string.
  fn = {
    __functor = self: self.ast;

    store = functionExpr: {
      inherit functionExpr;
      __fn = eval.eval.store functionExpr;
      __functor = self: arg: self.__fn arg;
      # Create a new function of the same type with a text transformation f applied.
      mapText = f: fn.store (f functionExpr);
    };

    ast = functionExpr: {
      inherit functionExpr;
      __fn = 
        with eval.monad;
        let fE = eval.eval.ast functionExpr;
        in arg: 
          case fE {
            Left = e: toString e;
            Right = f: f arg;
          };
      __functor = self: arg: self.__fn arg;
      # Create a new function of the same type with a text transformation f applied.
      mapText = f: fn.ast (f functionExpr);
      # Create a new function of the same type with an AST transformation f applied.
      mapAST = f: fn.ast (f (parser.parse functionExpr));
    };
  };

  # Test both AST and Store based txtfns.
  _tests = with tests; suite {
    # Store disabled due to drv errors
    each = flip mapAttrs { inherit (fn) ast store; } (_: fn: 
      let fTxt = "a: b: 3 * a + b";
          f = fn fTxt;
          g = f.mapText (t: "z: ${t} + z");
      in {
        exprStr = expect.eq f.functionExpr fTxt;
        call = expect.eq (f 3 1) 10;
        fmap = expect.eq (g 5 3 1) 15;
      }
    );

    fn.astOnly.mapAST =
      let 
        trans = l: l // { param = l.param // { name = l.param.name // { name = "${l.param.name.name}XXX"; }; }; };
        fnExpr = "arg: argXXX + 1";
      in {
        withoutTransformation = 
          with eval.monad;
          with Either EvalError "set";
          let f = fn.ast fnExpr;
          in expect.noLambdasEq (f 123) (RuntimeError (_b_ ''
            Undefined identifier 'argXXX' in current scope:
              { arg = 123;
                false = false;
                import = <lambda>;
                null = null;
                true = true; }
          ''));
        withTransformation = 
          let f = fn.ast fnExpr;
          in expect.eq ((f.mapAST trans) 123) 124;
      };


  };
}
