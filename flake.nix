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
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          collective-lib = outputs.packages.${system}.collective-lib;
          pkgs = nixpkgs.legacyPackages.${system};
          testedModules = import ./modules { inherit collective-lib; };
        in {
          lib = collective-lib;
          packages = import ./pkgs { inherit pkgs inputs; };
          devShells = { default = pkgs.mkShell {}; };
          # Modules still have _tests here; removed before applying them to
          # systems.
          inherit testedModules;
          modules = 
            with collective-lib.tests;
            removeTests (mapAttrs (_: removeTests) testedModules);
        }
      ) // {
        overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; };
      };
}
