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
        buildInputs = (attrs.buildInputs or []) ++ [
          (final.python3.withPackages (ps: [ ps.handheld-daemon-adjustor ]))
        ];
        dependencies = (attrs.dependencies or []) ++ [
          final.handheld-daemon-adjustor
          (final.python3.withPackages (ps: [ ps.handheld-daemon-adjustor ]))
        ];
        propagatedBuildInputs = (attrs.propagatedBuildInputs or []) ++ [
          final.python3Packages.handheld-daemon-adjustor
        ];
      });
  };
in
{
  "python" = final: prev: rec {
    python3 = prev.python3.override {
      packageOverrides = (self: super:
        (import ../pkgs/pythonPackages { pkgs = prev; })
        // (image-go-nord-overlay self super)
      );
    };
    python3Packages = prev.python3Packages // final.python3.pkgs;
  };
  "packages" = final: prev: import ../pkgs { pkgs = prev; };
  "handheld-daemon" = handheld-daemon-overlay;
}
