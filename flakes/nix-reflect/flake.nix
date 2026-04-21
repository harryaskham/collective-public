{
  description = "Nix library for parsing, evaluating and reflecting on Nix code, written in pure Nix";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    collective-public = {
      url = "github:harryaskham/collective-public";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nix-parsec.url = "github:nprindle/nix-parsec";
  };

  outputs = { nixpkgs, flake-utils, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        reflectLib = import ./lib { inherit inputs pkgs; };
      in {
        lib = reflectLib;
        devShells = {
          default = pkgs.mkShell (rec {
            buildInputs = with pkgs; [ direnv ];
          });
        };
      }
    ) // {
      inherit inputs;
    };
}
