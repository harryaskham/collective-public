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
      # TODO: Move whole library away from pkgs.
      collective-lib = self.packages.x86_64-linux.collective-lib;
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          packages = import ./pkgs { inherit pkgs inputs; };
          devShells = { default = pkgs.mkShell {}; };
        }
      ) 
      // {
        lib = collective-lib;
        overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; };
        nixosModules = import ./modules/nixos;
      };
}
