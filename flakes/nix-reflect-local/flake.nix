{
  description = "Nix library for parsing and evaluating Nix code (standalone subflake).";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Upstream collective-public to depend on and use as a library
    collective-public.url = "github:harryaskham/collective-public";

    # Parsec library used by the parser module
    nix-parsec.url = "github:nprindle/nix-parsec";
  };

  outputs = { self, nixpkgs, flake-utils, collective-public, nix-parsec, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;

        # Upstream base library from collective-public for this system
        baseLib = collective-public.packages.${system}.collective-lib;

        # A small, standalone library that depends on upstream baseLib but
        # only exposes the local parser, eval and debuglib.
        reflectLib = rec {
          parser = import ./lib/parser/default.nix {
            inherit lib;
            collective-lib = baseLib;
            inherit nix-parsec;
          };

          eval = import ./lib/eval/default.nix {
            inherit lib;
            collective-lib = baseLib;
            parser = parser;
          };

          debuglib = import ./lib/debuglib.nix {
            inherit lib;
            collective-lib = baseLib;
          };
        };
      in {
        lib = reflectLib;
        devShells.default = pkgs.mkShell { };  
      }
    ) // {
      inherit inputs;
    };
}