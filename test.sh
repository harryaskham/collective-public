#!/usr/bin/env bash
nix eval --impure --expr "(import ./pkgs/collective-lib {})._tests.run {}"
