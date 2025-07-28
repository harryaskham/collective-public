{ collective-lib, ... }:

let
  modules = {
    dbus = import ./dbus.nix;
    fonts-fontconfig = import ./fonts/fontconfig.nix;
    fonts-packages = import ./fonts/packages.nix;
    fonts-fontdir = import ./fonts/fontdir.nix;
    unexpected-keyboard = import ./unexpected-keyboard;
    sshd = import ./sshd.nix;
    termux = import ./termux.nix;
  };
in
  modules // {
    _tests = collective-lib.tests.mergeModuleSuites modules;
  }
