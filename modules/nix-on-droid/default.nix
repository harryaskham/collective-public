{ collective-lib, pkgs, ... }:

let
  modules = collective-lib.tests.collectTestableModules {
    dbus = import ./dbus.nix;
    fonts-fontconfig = import ./fonts/fontconfig.nix;
    fonts-packages = import ./fonts/packages.nix;
    fonts-fontdir = import ./fonts/fontdir.nix;
    nod-exec = import ./nod-exec.nix;
    pulseserver = import ./pulseserver.nix;
    session = import ./session.nix;
    sshd = import ./sshd.nix;
    supervisord = import ./supervisord.nix;
    termux = import ./termux.nix;
  };
  supervisordTests = (import ../tests/supervisord.nix { inherit pkgs collective-lib; }).nix-on-droid;
in
modules // {
  _tests = collective-lib.tests.extendSuite modules._tests supervisordTests;
}
