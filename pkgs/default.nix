{ pkgs, ...}:

let
  # handheld-daemon-ui-pkg = pkgs.callPackage ./handheld-daemon-ui.nix { };
  pythonPackages = import ./pythonPackages { inherit pkgs; };
in {
  # handheld-daemon =
  #   pkgs.python3Packages.toPythonApplication
  #     pythonPackages.handheld-daemon-hhd;
  handheld-daemon-adjustor =
      pkgs.python3Packages.toPythonApplication
        pythonPackages.handheld-daemon-adjustor;
  # handheld-daemon-ui = handheld-daemon-ui-pkg;
}
