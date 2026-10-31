# Shared closures

Some machines configured from the private collective repository need packages
that no public binary cache serves. That happens most on macOS, where Hydra's
cache for a nixpkgs revision is often incomplete, and for packages built
against collective's pins. Building them on the machine itself takes hours.

Each `<machine>.json` here lists the derivations from one machine's system
that can be built from public sources alone. Its `.drv` closure is attached to
the release `shared-closure-<machine>`. The
[shared-closures workflow](../.github/workflows/shared-closures.yml) does the rest:

1. It runs on the free runner for the machine's system: `macos-latest` for
   `aarch64-darwin`, `ubuntu-latest` for `x86_64-linux`, `ubuntu-24.04-arm`
   for `aarch64-linux`.
2. It checks the bundle against the manifest's SHA-256 and imports the
   derivations.
3. It skips anything FlakeHub Cache already serves, builds the rest and
   pushes the results to FlakeHub Cache.

The machine then downloads those paths instead of building them. The
derivations are exact, so the outputs match without this repository
reproducing the private pins.

A bundle contains only derivations whose whole derivation closure has:

- **public sources only:** no private flake inputs, nothing from the private
  repository, and no files copied out of either;
- **no private inline text:** no SSH keys, age recipients, tailnet addresses
  or names, or personal identifiers;
- **no unfree packages.**

Trivial local builders, such as generated configuration files, are never
build roots.

The private repository's `scripts/public-prebuild-plan` makes these checks,
and its `scripts/shared-closure-publish` publishes the result.
