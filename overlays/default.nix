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
in
{
  "python" = final: prev: rec {
    python3 = prev.python3.override {
      packageOverrides = (self: super:
        (import ../pkgs/pythonPackages { pkgs = prev; handheld-daemon-ui = final.handheld-daemon-ui; })
        // (image-go-nord-overlay self super)
      );
    };
    python3Packages = prev.python3Packages // final.python3.pkgs;
  };
  "packages" = final: prev: prev // (import ../pkgs { pkgs = prev; });
}
