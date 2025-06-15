{ pkgs ? import <nixpkgs> {},
  lib ? pkgs.lib,
  trace-verbose ? false,
  ... }:

let
  collective-lib = rec {
    attrs = import ./attrs.nix { inherit lib cutils; };
    binding = import ./binding.nix { inherit lib cutils; };
    clib = import ./clib.nix { inherit lib cutils; };
    collections = import ./collections.nix { inherit lib cutils; };
    colors = import ./colors.nix { inherit lib cutils; };
    disk = import ./disk.nix { inherit lib cutils; };
    dispatch = import ./dispatch.nix { inherit lib cutils; };
    display = import ./display.nix { inherit lib cutils; };
    errors = import ./errors.nix { inherit lib cutils; };
    fan = import ./fan.nix { inherit lib cutils; };
    font = import ./font.nix { inherit lib cutils; };
    functions = import ./functions.nix { inherit lib cutils; };
    lists = import ./lists.nix { inherit lib cutils; };
    log = import ./log.nix { inherit lib cutils trace-verbose; };
    strings = import ./strings.nix { inherit lib cutils; };
    tests = import ./tests.nix { inherit lib cutils; };
    types = import ./types.nix { inherit lib cutils; };
    wm = import ./wm.nix { inherit lib cutils; };
  };
  # TODO: Remove legacy alias.
  cutils = collective-lib;
in
  collective-lib // {
    # Run all:
    # nix eval --impure --expr '(import collective-public/pkgs/collective-lib {})._tests.run' --show-trace | xargs -0 -I{} python -c 'print('''{}''')' | less
    # Run one:
    # nix eval --impure --expr '(import collective-public/pkgs/collective-lib {})._tests.tests.test-types__MyType2_mk_missingDefault'
    #
    # Or:
    # function evaldbg() {
    #   PRINT="log.vprint"
    #   FLAGS="--trace-verbose"
    #   while [[ -n "$1" ]]; do
    #     case "$1" in
    #       terse)
    #         PRINT="log.print"
    #         shift
    #         ;;
    #       notrace)
    #         FLAGS=
    #         shift
    #         ;;
    #       *)
    #         EXPR="$1"
    #         break
    #         ;;
    #     esac
    #   done
    #
    #   FULL_EXPR="let pkgs = import <nixpkgs> {}; in with pkgs.lib; with (import ~/collective/collective-public/pkgs/collective-lib {}); ${PRINT} (${EXPR})"
    #
    #   CMD="nix eval --impure --expr \"$FULL_EXPR\" --raw --show-trace $FLAGS"
    #
    #   echo "Running: ${CMD}" >&2
    #   bash -c "$CMD"
    # }
    #
    # evaldbg terse notrace types._tests.run
    _tests =
      collective-lib.tests.suite
        (lib.concatMapAttrs
          (name: module: if (module ? _tests) then module._tests.nestedTests else {})
          collective-lib);
 }
