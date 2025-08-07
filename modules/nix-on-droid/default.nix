{ collective-lib, ... }:

collective-lib.tests.collectTestableModules {
  dbus = import ./dbus.nix;
  fonts-fontconfig = import ./fonts/fontconfig.nix;
  fonts-packages = import ./fonts/packages.nix;
  fonts-fontdir = import ./fonts/fontdir.nix;
  session = import ./session.nix;
  sshd = import ./sshd.nix;
  termux = import ./termux.nix;
}
