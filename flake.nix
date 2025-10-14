
{
  description = "Public subset of Nix configurations.";

  inputs = {
    self.submodules = true;
    nix-reflect = {
      url = ./flakes/nix-reflect;
      inputs.collective-public.follows = "";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    #nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    #nixpkgs-stable.url = "github:nixos/nixpkgs/release-25.05";
    #nix-parsec.url = "github:nprindle/nix-parsec";
    nixpkgs.url = "git+https://git@github.com/nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs-stable.url = "git+https://git@github.com/nixos/nixpkgs?ref=release-25.05";
    nix-parsec.url = "git+https://git@github.com/nprindle/nix-parsec";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-stable,
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
          devShells = {
            default = pkgs.mkShell {};
          };
          inherit testedModules;
          modules = 
            with collective-lib.typed;
            tests.removeTests (mapAttrs (_: tests.removeTests) testedModules);
        }
      ) // {
        inherit inputs;
        overlays = import ./overlays { inherit inputs; inherit (nixpkgs) lib; inherit nixpkgs-stable; };
      };
}
