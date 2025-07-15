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
    nix-parsec,
    ...
  } @ inputs:
    let
      inherit (self) outputs;
    in
      flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in {
          packages = import ./pkgs { inherit nix-parsec pkgs; };
          devShells = { default = pkgs.mkShell {}; };
        }) // rec {
          overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; inherit nix-parsec; };
          nixosModules = import ./modules/nixos;
        };
}
