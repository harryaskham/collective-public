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
      let stable = import nixpkgs-stable { inherit (prev) system; };
      in rec {
      python3 = prev.python3.override {
        packageOverrides = (self: super:
          (import ../pkgs/pythonPackages { pkgs = prev; })
          // {
            image-go-nord = stable.python3Packages.image-go-nord;
          }
        );
      };
      python3Packages = prev.python3Packages // final.python3.pkgs;
    };
    packagesOverlay = final: prev: import ../pkgs { pkgs = prev; inherit inputs; };
  };
in
overlays
