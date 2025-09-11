{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.typed;

{
  runBash = lib.fix (self: {
    __functor = self: self.cached;
    cached = command: self.__run { inherit command; cache = true; };
    uncached = command: self.__run { inherit command; cache = false; };
    __run = {
      command,
      cache,
      bashCommand ? toShellValueUnsafe command,
      name ? "runBash-${bashCommand}",
    }:
      let drv =
        pkgs.runCommandLocal
          name 
          (optionalAttrs (!cache) { env.when = builtins.currentTime; })
          "${pkgs.bash}/bin/bash -c ${bashCommand} > $out";
      in lib.readFile "${drv}";
  });
}