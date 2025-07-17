let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  collective-lib = import ./pkgs/collective-lib {
    inherit pkgs lib;
    traceOpts = {
      traceLevel = 0;
      enablePartialTrace = false;
      enableVerboseTrace = false;
      enableShortTrace = false;
    };
  };
in
with collective-lib;
let
  # Test cases
  expr = "{ a = 42; }.b or 0";
  
  # Step by step debugging
  parsed = parser.parseAST expr;
  evaluated = parser.evalAST parsed;
  
  # Additional debugging
  rootNode = parsed.root;
  nodeType = rootNode.nodeType;
  
in {
  inherit expr parsed evaluated rootNode nodeType;
  
  # Additional details
  hasDefault = builtins.hasAttr "default" rootNode;
  defaultValue = if hasDefault then rootNode.default else "NO_DEFAULT";
  
  # Test the individual components
  exprNode = rootNode.expr;
  pathNode = rootNode.path;
}