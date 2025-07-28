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
      ) // (
        let
          # TODO: Decouple reliance on system.
          inherit (self.lib.x86_64-linux) tests;
          moduleArgs = { collective-lib = self.lib; };
          importModules = path: tests.removeTests (import path moduleArgs);
        in {
          overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; };
          agnosticModules = importModules ./modules/agnostic;
          nixosModules = importModules ./modules/nixos;
          nixOnDroidModules = importModules ./modules/nix-on-droid;
          nixDarwinModules = importModules ./modules/nix-darwin;
          homeManagerModules = importModules ./modules/home-manager;
        });
}
