# AGENT.md (collective-public)

This document is a detailed guide to the public flake "collective-public". It explains the libraries, modules, overlays, and flake outputs intended for public consumption. A private flake named `collective` exists and may consume this project, but details of that private flake are intentionally omitted here.

## Overview

- Purpose: Share reusable Nix modules, a comprehensive functional library (`collective-lib`), and overlays that can be consumed in any flake.
- Packaging: Exposed via a Nix flake with minimal assumptions and few dependencies beyond `nixpkgs`, `nix-parsec`, and `nix-reflect`.
- Design goals: Composability, type-driven utilities, script-building ergonomics, and high signal-to-noise logging.

## Flake structure and outputs

- `collective-public/flake.nix`
  - Inputs: `nixpkgs`, `nixpkgs-stable`, `nix-parsec` (for parser utilities), `nix-reflect` (for Nix parsing/evaluation)
  - Outputs (per supported system):
    - `lib`: The instantiated `collective-lib`
    - `packages`:
      - `collective-lib`: library exported as a derivation for programmatic import
      - `collective-pythonPackages`: namespaced Python packages
      - `handheld-daemon-adjustor`: Python application wrapper around the adjustor CLI
    - `devShells`: `default` minimal shell
    - `testedModules`: module trees arranged for testing
    - `modules`: `testedModules` with tests removed (suitable for consumption)
  - Top-level:
    - `inputs`: re-exposed for consumers
    - `overlays`: see Overlays section

### Consuming this flake

- As a flake input:
```nix
{
  inputs.collective-public.url = "github:harryaskham/collective-public";
}
```

- Access the library and modules in outputs:
```nix
# Example in your flake outputs
{
  outputs = { self, nixpkgs, collective-public, ... }:
  let
    system = "x86_64-linux"; # or aarch64-linux, aarch64-darwin, etc
    pkgs = nixpkgs.legacyPackages.${system};
    libPublic = collective-public.outputs.lib;               # instantiated collective-lib
    publicModules = collective-public.outputs.modules.${system};
    publicPkgs = collective-public.outputs.packages.${system};
  in {
    # use libPublic, publicModules, publicPkgs
  }; 
}
```

## Overlays

Defined in `overlays/default.nix`:
- `python3Overlay`
  - Overrides `python3` to include `collective-public/pkgs/pythonPackages` and extends `python3Packages`.
  - Also injects an `image-go-nord` overlay to include missing Python dependencies (`numpy`, `ffmpeg-python`, `requests`) and disable checks.
- `packagesOverlay`
  - Imports `collective-public/pkgs` into your package set.

Usage example:
```nix
{ inputs, ... }:
{
  nixpkgs.overlays = [
    inputs.collective-public.overlays.python3Overlay
    inputs.collective-public.overlays.packagesOverlay
  ];
}
```

## Packages

- `packages.collective-lib`
  - A derivation exporting the library from `pkgs/collective-lib`. You can also import it directly from source.
- `packages.collective-pythonPackages`
  - Namespaced Python packages for use via the `python3Overlay` or directly.
- `packages.handheld-daemon-adjustor`
  - A Python application wrapper for the handheld-daemon adjustor CLI.

Access example:
```nix
let
  publicPkgs = inputs.collective-public.outputs.packages.${system};
  collectiveLib = publicPkgs.collective-lib;  # library derivation
in
  collectiveLib
```

## Modules

Modules are organized per target and exposed both under `testedModules` (with tests) and `modules` (with tests removed) for consumption. Use entries from `modules` in your system or home-manager configurations.

Structure (`modules/`):
- `agnostic/`
  - `etc.nix`: cross-platform `/etc`-like declarations helpers
  - `unexpected-keyboard/`: helpers for Unexpected Keyboard configs
    - `default.nix`: module entry
    - `keyboards.nix`: keyboard layout utilities
    - `wasm/default.nix`: WASM-related helpers
- `home-manager/`
  - `default.nix`: home-manager module group (thin wrapper)
- `nix-darwin/`
  - `default.nix`: nix-darwin module group (thin wrapper)
- `nix-on-droid/`
  - `dbus.nix`: DBus configuration helpers
  - `fonts/`: font configuration (fontconfig, packages, fontdir)
  - `session.nix`: session setup utilities
  - `sshd.nix`: sshd configuration on Android/Termux context
  - `termux.nix`: Termux-related helpers
- `nixos/`
  - `default.nix`: NixOS module group
  - `handheld-daemon.nix`: NixOS module for handheld-daemon integration

Consumption example (NixOS):
```nix
{ inputs, ... }:
{
  imports = [
    inputs.collective-public.outputs.modules.${pkgs.system}.nixos.handheld-daemon
    # Or import subtrees as needed
  ];
}
```

Consumption example (home-manager):
```nix
{ inputs, ... }:
{
  imports = [
    inputs.collective-public.outputs.modules.${pkgs.system}.home-manager
  ];
}
```

## collective-lib (library)

The library lives at `pkgs/collective-lib/`. It is built by merging a set of modules into a cohesive, typed library. Key characteristics:

- Multiple views:
  - `base`: original modules without merged conveniences
  - `baseMerged`: all mergeable modules combined for top-level access (e.g., `lists`, `attrsets`, `strings`)
  - `untyped`: `baseMerged` merged into `nixpkgs.lib` (drop-in replacement for `lib`, no type system)
  - `typed`: `untyped` + type system enabled and exposed via `typelib.library`
  - `noTests`: library without `_tests`
  - `_tests`/`_testsUntyped`: aggregated test suites
- Extensibility:
  - `extend`: deep-merge another downstream `collective-lib`-shaped set of modules

### Module index (high level)

- `attrsets.nix`: attribute-set utilities (merging, diffs, traversal)
- `lists.nix`: list operations, folds, zips, partitions
- `strings/`: string utilities and tests
- `functions.nix`: function composition, partial application, higher-order utils
- `collections.nix`: generic collection operations across structures
- `data.nix`: data manipulation helpers
- `dispatchlib.nix`: predicate-based dispatch helpers
- `errors.nix`: structured error helpers and propagation
- `typelib.nix`: runtime type system; `typed` view exposes its overrides (e.g., `isType`, `isNull`)
- `binding.nix`: binding utilities
- `clib.nix`: core library utilities
- `shell.nix`: shell-related helpers
- `script-utils/`: CLI-building framework
  - `script-types.nix`: script type model (command trees, scripts)
  - `options-utils.nix`: declarative option schemas (required/defaults/switches)
  - `command-utils.nix`: command wiring, helpers, aliases
  - `log-utils.nix`: structured logging, debug switches, fatal/return helpers
  - `ansi-utils.nix`: ANSI styling, atom helpers (pretty values, cmd rendering)
  - `usage-utils.nix`: usage/help text generation
  - `main-utils.nix`: script entry helpers
- `log.nix`: logging configuration and sinks
- `syntax.nix`: mini-DSL helpers
- `rebinds.nix`: rebinding utilities
- `display.nix`, `font.nix`, `colors.nix`, `wm.nix`: UI-related primitives
- `disk.nix`, `fan.nix`: system helpers
- `ext.nix`: external integration helpers
- `modulelib.nix`: deep merge utilities, test suite merging, and module composition
- `tests.nix`: testing combinators and suite assembly

### Using collective-lib

Import directly (non-flake context):
```nix
let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  collective-lib = import ./pkgs/collective-lib { inherit pkgs lib; };
  typed = collective-lib.typed;
  untyped = collective-lib.untyped;
  # Or extend with downstream modules
  # collective-lib-extended = collective-lib.extend { mymod = { ... }; };
in
  typed.lists.length [1 2 3]
```

From flake outputs:
```nix
let
  libPublic = inputs.collective-public.outputs.lib.${system};
  typed = libPublic.typed;
  strings = typed.strings;
in
  strings.concat ["hello" " " "world"]
```

Using the script framework to define a CLI command tree (sketch):
```nix
with collective-lib.script-utils;
mkDefaultScriptPackage {
  name = "my-cli";
  type = script-types.collection;
  scripts = {
    hello = {
      type = script-types.bashScript;
      summary = "Say hello";
      main = ''
        echo "hello"
      '';
    };
  };
}
```

### Type system notes

- The `typed` view exposes runtime-checked versions of certain functions via `typelib`.
- If you want speed or direct drop-in replacement for `nixpkgs.lib`, use the `untyped` view.
- If you require strict validation and richer invariants, use the `typed` view.

## Testing

- Quick run of library tests (from this directory, with direnv picking up the correct PATH):
```bash
# Run all tests
run-tests
```

```bash
# Run one library's tests
run-tests functions
```
- There is also `debug-tests` which will allow Nix errors to propagate and kill the test, allowing for deeper insight into failure but aborting the other tests.
- The library also exposes test suites under `collective-lib._tests` and `collective-lib._testsUntyped`.

## Notes on visibility

- This document is public. A private flake named `collective` may layer private modules and configurations over `collective-public`. Details of private usage are intentionally omitted.
