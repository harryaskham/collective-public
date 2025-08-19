#!/usr/bin/env bash

source ./maybe_bootstrap_cursor_agent.sh

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
  collective-lib = import ${CLTV_ROOT:-.}/pkgs/collective-lib {
    inherit pkgs lib;
    traceOpts = {
      traceLevel = ${CLTV_TRACE_LEVEL:-0};
      enablePartialTrace = ${CLTV_PARTIAL_TRACE:-false};
      enableVerboseTrace = ${CLTV_VERBOSE_TRACE:-false};
      enableShortTrace = ${CLTV_SHORT_TRACE:-false};
    };
  };
in
  with collective-lib;
  ($@)
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
  maybe-bootstrap-cursor-agent

  RAW_EXPR=$(get-raw-nix-expr $@)
  EXPR=$(wrap-nix-expr "$RAW_EXPR")
  echo "Running in nix repl:" >&2
  echo "$EXPR" >&2
  run-in-nix-eval "$EXPR" 
}