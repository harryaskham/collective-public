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
  handheld-daemon-overlay = final: prev: {
    handheld-daemon = prev.handheld-daemon.overrideAttrs (attrs: {
      dependencies = (attrs.dependencies or []) ++ [
        final.pythonPackages.handheld-daemon-adjustor
      ];
    });
  };
in rec {
  python3Overlay = final: prev: rec {
    python3 = prev.python3.override {
      packageOverrides = (self: super:
        (import ../pkgs/pythonPackages { pkgs = prev; })
        // (image-go-nord-overlay self super)
      );
    };
    python3Packages = prev.python3Packages // final.python3.pkgs;
  };
  packagesOverlay = final: prev: import ../pkgs { pkgs = prev; };
  handheldDaemonOverlay = inputs.nixpkgs.lib.fixedPoints.composeManyExtensions [ python3Overlay handheld-daemon-overlay ];
}
