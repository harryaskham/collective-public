{ lib, collective-lib, nix-reflect, ... }:

# TODO:
# - Dynamic derivations should let eval-in-eval occur without requiring nested nix build:
#   https://fzakaria.com/2025/03/11/nix-dynamic-derivations-a-practical-application

# Evaluate a Nix expression contained within a string.
# Writes the string out to a file in the store by a derivation, and then
# imports that file.
let
  args = { inherit lib collective-lib nix-reflect; };
  modules = {
    monad = import ./monad.nix args;
    store = import ./store.nix args;
    fn = import ./fn.nix args;
    ast = import ./ast.nix args;
  };

  unsafely = f: expr: (f expr).case {
    Left = e: throw "Eval error: ${e}";
    Right = v: v;
  };
in
# Expose the modules as eval.monad, eval.store, eval.ast
# Plus the centralising eval function.
collective-lib.tests.withMergedSuites modules // (lib.fix (self: {
  /* Default to dispatching based on the type of the argument,
  eval :: (string | AST) -> Either EvalError a */
  __functor = self: self.lazy;
  lazy = unsafely self.either.lazy;
  strict = unsafely self.either.strict;

  /* Evaluate safely to a result or an error.
  either :: (string | AST) -> Either EvalError a */
  either = {
    __functor = self: self.lazy;
    lazy = modules.ast.evalAST;
    strict = modules.ast.evalAST';
  };

  # Expose under 'eval.eval' and 'eval'
  eval = {
    __functor = self: self.ast;

    /* Eval a string or AST.
    eval :: (string | AST) -> Either EvalError a */
    ast = self.lazy;

    /* Eval a string by persisting to the store.
    store :: string -> a */
    store = modules.store.evalStore;
  };

}))
