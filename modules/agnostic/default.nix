{ collective-lib, ... }:

collective-lib.tests.collectTestableModules {
  etc = import ./etc.nix;
  unexpected-keyboard = import ./unexpected-keyboard;
}
