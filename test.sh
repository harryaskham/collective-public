#!/usr/bin/env bash

function maybe-install-nix() {
  if ! which nix; then
    sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
    . /home/ubuntu/.nix-profile/etc/profile.d/nix.sh
  fi
}

function get-raw-nix-expr() {
  if [[ -z "$@" ]]; then
    echo -n "collective-lib._tests.run {}"
  else
    echo -n "$@"
  fi
}

function wrap-nix-expr() {
  cat << EOF
let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  collective-lib = import ./pkgs/collective-lib {
    inherit pkgs lib;
    traceOpts = {
      traceLevel = 0;
      enablePartialTrace = false;
      enableVerboseTrace = false;
      enableShortTrace = false;
    };
  };
in
  with collective-lib;
  lib.traceSeq ($@) {}
EOF
}

function run-in-nix-eval() {
  nix --extra-experimental-features nix-command eval --impure --show-trace --expr "$@"
}

function run-in-nix-repl() {
  EXPECT_SCRIPT=$(cat << EOF
spawn nix repl --show-trace
expect "nix-repl> "
send "$@\r" 
expect "nix-repl> "
send ""
interact
EOF
)
  expect -c "$EXPECT_SCRIPT"
}

function run-expr() {
  RAW_EXPR=$(get-raw-nix-expr $@)
  EXPR=$(wrap-nix-expr "$RAW_EXPR")
  echo "Running in nix repl:" >&2
  echo "$EXPR" >&2
  run-in-nix-eval "$EXPR" 
}

if [[ "$(hostname)" == "cursor" ]]; then
  maybe-install-nix
fi
run-expr "$@"
