# Utils for importing the lib in external contexts.
{
  # Assemble sufficient arguments to import collective-lib in a WASM context
  # where NIX_PATH is set up with dependencies hard-coded into the WASM blob.
  wasm = rec {
    lib = import <nixpkgs/lib>;
    collective-lib = import <collective/pkgs/collective-lib> args;
    collective-lib-public = import <collective/collective-public/pkgs/collective-lib> args;
    args = {
      pkgs.lib = lib;
      pkgs.system = "x86_64-linux";
      inputs.nix-parsec = import <nix-parsec>;
      inputs.collective-public.lib.${args.pkgs.system} = collective-lib-public;
      inputs.nix-reflect.lib.${args.pkgs.system} = import <nix-reflect/lib> args;
    };
    inherit (collective-lib) typed;
  };
}
