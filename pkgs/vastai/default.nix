{ pkgs, ...}:

{
  vastai-cli = pkgs.callPackage ./vastai-cli.nix { };
}
