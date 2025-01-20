{ pkgs, ...}:

let
  pythonPackages = import ./pythonPackages { inherit pkgs; };
in {
  handheld-daemon-adjustor =
    (pkgs.python3Packages.toPythonApplication
      pythonPackages.handheld-daemon-adjustor).overrideAttrs (oldAttrs: {
        # Include its own Python library in its runtime dependencies as it forks Python processes that import `hhd.adjustor`
        propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [ pythonPackages.handheld-daemon-adjustor ];
      });
}
