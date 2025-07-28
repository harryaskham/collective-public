{ collective-lib, ... } @ args:

collective-lib.tests.withMergedSuites {
  agnostic = import ./agnostic args;
  home-manager = import ./home-manager args;
  nixos = import ./nixos args;
  nix-on-droid = import ./nix-on-droid args;
  nix-darwin = import ./nix-darwin args;
};