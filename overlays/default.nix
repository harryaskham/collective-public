{
  inputs,
  lib, 
  # Downgrades
  nixpkgs-stable,
  ...
}:

let
  overlays = {
    python3Overlay = final: prev: 
      let
        stable = import nixpkgs-stable {
          inherit (prev) system;
          config.allowUnfree = true;
        };
      in {
        python3 = prev.python3.override {
          packageOverrides = self: super: {
            image-go-nord = stable.python3Packages.image-go-nord;
          };
        };
        python3Packages = prev.python3Packages.override {
          overrides = self: super: {
            image-go-nord = stable.python3Packages.image-go-nord;
          };
        };
    };
    packagesOverlay = final: prev: import ../pkgs { pkgs = prev; inherit inputs; };
  };
in
overlays
