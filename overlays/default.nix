{inputs, lib, ...}:

let
  imageGoNordOverlay = self: super: {
    image-go-nord = super.image-go-nord.overrideAttrs (_: {
      doCheck = false;
      propagatedBuildInputs =
        let missingBuildInputs = with super; [numpy ffmpeg-python requests];
        in super.image-go-nord.propagatedBuildInputs ++ missingBuildInputs;
    });
  };
  overlays = {
    python3Overlay = final: prev: rec {
      python3 = prev.python3.override {
        packageOverrides = (self: super:
          (import ../pkgs/pythonPackages { pkgs = prev; })
          // (imageGoNordOverlay self super)
        );
      };
      python3Packages = prev.python3Packages // final.python3.pkgs;
    };
    packagesOverlay = final: prev: import ../pkgs { pkgs = prev; };
  };
in
overlays // {
  all = lib.composeManyExtensions (lib.attrValues overlays);
}
