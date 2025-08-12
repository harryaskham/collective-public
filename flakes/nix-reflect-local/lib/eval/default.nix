{ lib, collective-lib, parser, ... }:

# TODO:
# - Dynamic derivations should let eval-in-eval occur without requiring nested nix build:
#   https://fzakaria.com/2025/03/11/nix-dynamic-derivations-a-practical-application

# Evaluate a Nix expression contained within a string.
# Writes the string out to a file in the store by a derivation, and then
# imports that file.
let
  argsBase = { inherit lib collective-lib; };
  modules = let self = {
    monad = import ./monad.nix argsBase;
  }; in self // {
    store = import ./store.nix argsBase;
    fn = import ./fn.nix (argsBase // { inherit parser; eval = { monad = self.monad; }; });
    ast = import ./ast.nix (argsBase // { inherit parser; eval = { monad = self.monad; }; });
  };
in

# Expose the modules as eval.monad, eval.store, eval.ast
# Plus the centralising eval function.
with collective-lib.typed;
rec {

  inherit (modules) monad fn ast store;

  /* Default to dispatching based on the type of the argument,
  eval :: (string | AST) -> Either EvalError a */
  __functor = self: self.eval;

  # Expose under 'eval.eval' and 'eval'
  eval = {
    __functor = self: self.ast;

    /* Eval a string or AST.
    eval :: (string | AST) -> Either EvalError a */
    ast = modules.ast.evalAST;

    /* Eval a string by persisting to the store.
    store :: string -> a */
    store = modules.store.evalStore;
  };

  # Test both AST and Store eval.
  _tests = with tests; extendSuite (mergeSuites modules) (suite {
    eval = {
      ast = {
        const = expect.eq (eval.ast "1").right 1;
        add = expect.eq (eval.ast "1 + 1").right 2;
      };
      store = {
        const = expect.eq (eval.store "1") 1;
        add = expect.eq (eval.store "1 + 1") 2;
      };
    };
  });
}