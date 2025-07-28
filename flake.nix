{
  description = "Public subset of Nix configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nix-parsec = {
      url = "github:nprindle/nix-parsec";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    let
      inherit (self) outputs;
      collectiveLibForArchitecture = architectureStr:
        outputs.packages.${architectureStr}.collective-lib;
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          packages = import ./pkgs { inherit pkgs inputs; };
          devShells = { default = pkgs.mkShell {}; };
          lib = collectiveLibForArchitecture system;
        }
      ) 
      // {
        overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; };
        agnosticModules = import ./modules/agnostic;
        nixosModules = import ./modules/nixos;
        nixOnDroidModules = import ./modules/nix-on-droid;
        nixDarwinModules = import ./modules/nix-darwin;
        homeManagerModules = import ./modules/home-manager;
      };
}
