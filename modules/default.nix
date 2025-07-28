{ collective-lib, ... } @ args:

let 
  modules = {
    agnostic = import ./agnostic args;
    home-manager = import ./home-manager args;
    nixos = import ./nixos args;
    nix-on-droid = import ./nix-on-droid args;
    nix-darwin = import ./nix-darwin args;
  };
in
  modules // {
    _tests = collective-lib.tests.mergeSuites modules;
  }


