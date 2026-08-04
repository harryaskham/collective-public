{ collective-lib, pkgs, ... }:

let
  modules = collective-lib.tests.collectTestableModules {
    supervisord = import ./supervisord.nix;
  };
  supervisordTests = (import ../tests/supervisord.nix { inherit pkgs collective-lib; }).home-manager;
in
modules // {
  _tests = collective-lib.tests.extendSuite modules._tests supervisordTests;
}
