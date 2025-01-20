{ pkgs, ... }:

{
  handheld-daemon-adjustor = pkgs.callPackage ./handheld-daemon/adjustor.nix {  };
}
