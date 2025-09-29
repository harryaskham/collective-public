{ pkgs, ...}:

{
  default = pkgs.callPackage ./vastai-cli.nix { };
}
