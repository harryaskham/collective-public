{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

# TODO:
# - Dynamic derivations should let eval-in-eval occur without requiring nested nix build:
#   https://fzakaria.com/2025/03/11/nix-dynamic-derivations-a-practical-application

# Evaluate a Nix expression contained within a string.
# Writes the string out to a file in the store by a derivation, and then
# imports that file.
let
  args = { inherit pkgs lib collective-lib; };
  modules = {
    ast = import ./ast.nix args;
    monad = import ./monad.nix args;
    store = import ./store.nix args;
    fn = import ./fn.nix args;
  };
in

# Expose the modules as eval.monad, eval.store, eval.ast
# Plus the centralising eval function.
with collective-lib.typed;
rec {

  # Expose all but store/ast to avoid clashes with eval.ast.
  inherit (modules) monad fn;

  /* Default to dispatching based on the type of the argument,
  eval :: (string | AST) -> Either EvalError a */
  __functor = self: self.ast;

  /* Eval a string or AST.
  eval :: (string | AST) -> Either EvalError a */
  ast = compose modules.ast.evalAST parser.parse;

  /* Eval a string by persisting to the store.
  Disabled, something wrong with store eval
  store :: string -> a */
  store = const "disabled"; # modules.store.evalStore;

  # Test both AST and Store eval.
  _tests = with tests; extendSuite (mergeSuites modules) (suite {
    # TODO: Move into eval.by when AST eval supports import
    # eval.storeOnly = flip mapAttrs { inherit store; } (_: evalFn: {
    #   meta = expect.eq (eval ''
    #     { evalPath }:
    #     let eval = import evalPath {};
    #     in eval "1 + 1"
    #   '' { evalPath = ./.; }) 2;
    #   nest =
    #     let nestEval = n: exprStr:
    #           if n == 0 then exprStr
    #           else nestEval (n - 1) (_b_ ''
    #             let eval = import ${./.} {}; in
    #             eval "(${replaceStrings [''"'' ''\''] [''\"'' ''\\''] exprStr}) + 1"
    #           '');
    #     in expect.eq (eval (nestEval 5 "0")) 5;
    # });

    # TODO: Re-enable store eval
    #eval.by = flip mapAttrs { inherit (eval) ast store; } (_: evalFn: {
    eval = {
      ast = {
        const = expect.eq (eval.ast "1").right 1;
        add = expect.eq (eval.ast "1 + 1").right 2;
      };
    };
  });
}