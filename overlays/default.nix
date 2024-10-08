{inputs, ...}:

let
  image-go-nord-overlay = self: super: {
    image-go-nord = super.image-go-nord.overrideAttrs (_: {
      doCheck = false;
      propagatedBuildInputs =
        let missingBuildInputs = with super; [numpy ffmpeg-python requests];
        in super.image-go-nord.propagatedBuildInputs ++ missingBuildInputs;
    });
  };
in final: prev:
  let myPkgs = import ../pkgs final.pkgs;
   in rec {
    python3 = prev.python3.override {
      packageOverrides = self: super:
        myPkgs.python3PackageOverrides
        // (image-go-nord-overlay self super);
    };
    python3Packages = final.python3.pkgs;
  } // myPkgs.replacement // myPkgs.new;
