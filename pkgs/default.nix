pkgs: rec {
  new = {  # Place name clashes here and resolve them in overlays.nix
    pythonPackages = {
      handheld-daemon = rec {
        adjustor = pkgs.callPackage ./pythonPackages/handheld-daemon/adjustor.nix {  };
        hhd = pkgs.callPackage ./pythonPackages/handheld-daemon/hhd.nix { inherit adjustor; };
      };
    };
    handheld-daemon = pkgs.pythonPackages.toPythonApplication new.pythonPackages.handheld-daemon.hhd;
  };
  handheld-daemon-adjustor = pkgs.pythonPackages.toPythonApplication new.pythonPackages.handheld-daemon.adjustor;
  handheld-daemon-ui = pkgs.callPackage ./handheld-daemon-ui.nix { };
}
