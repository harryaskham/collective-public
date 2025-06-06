{
  description = "Public subset of Nix configurations.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    let
      inherit (self) outputs;
    in
      flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in {
          packages = import ./pkgs { inherit pkgs; };
          devShells = { default = pkgs.mkShell {}; };
        }) // rec {
          overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; };
          nixosModules = import ./modules/nixos;
        };
}
