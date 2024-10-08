{ pkgs, handheld-daemon-ui, ... }:

rec {
  handheld-daemon-adjustor =
    pkgs.callPackage ./handheld-daemon/adjustor.nix {  };
  handheld-daemon-hhd =
    pkgs.callPackage ./handheld-daemon/hhd.nix {
      adjustor = handheld-daemon-adjustor;
      handheld-daemon-ui = handheld-daemon-ui;
    };
}
