{ pkgs, ...}:

let
  handheld-daemon-ui = pkgs.callPackage ./handheld-daemon-ui.nix { };
  pythonPackages = import ./pythonPackages { inherit pkgs handheld-daemon-ui; };
in {
  handheld-daemon =
    pkgs.python3Packages.toPythonApplication
      pythonPackages.handheld-daemon-hhd;
  handheld-daemon-adjustor =
      pkgs.python3Packages.toPythonApplication
        pythonPackages.handheld-daemon-adjustor;
  handheld-daemon-ui = handheld-daemon-ui;
}
