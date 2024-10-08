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
  (import ../pkgs final.pkgs)
  // rec {
    python3 = prev.python3.override {
      packageOverrides = self: super:
        super
        // final.toOverlay.pythonPackages
        // (image-go-nord-overlay self super);
    };
    python3Packages = final.python3.pkgs;

    handheld-daemon = final.toOverlay.handheld-daemon;
  }
