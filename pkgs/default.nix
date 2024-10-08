pkgs: rec {
  python3PackageOverrides = {
    handheld-daemon = rec {
      adjustor = pkgs.callPackage ./pythonPackages/handheld-daemon/adjustor.nix {  };
      hhd = pkgs.callPackage ./pythonPackages/handheld-daemon/hhd.nix { inherit adjustor; };
    };
  };
  replacements = {
    handheld-daemon =
      pkgs.python3Packages.toPythonApplication
        python3PackageOverrides.handheld-daemon.hhd;
  };
  new = {
    handheld-daemon-adjustor =
      pkgs.python3Packages.toPythonApplication
        python3PackageOverrides.handheld-daemon.adjustor;
    handheld-daemon-ui = pkgs.callPackage ./handheld-daemon-ui.nix { };
  };
}
