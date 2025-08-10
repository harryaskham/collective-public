# Utils for importing the lib in external contexts.
{
  # Assemble sufficient arguments to import collective-lib in a WASM context
  # where NIX_PATH is set up with dependencies hard-coded into the WASM blob.
  wasm = rec {
    lib = import <nixpkgs/lib>;
    args = {
      pkgs.lib = lib;
      pkgs.system = "x86_64-linux";
      inputs.nix-parsec = import <nix-parsec>;
      inputs.collective-public.lib.${args.pkgs.system} =
        import <collective/collective-public/pkgs/collective-lib> args;
    };
    collective-lib = import <collective/pkgs/collective-lib> args;
    inherit (collective-lib) typed;
  };
}
