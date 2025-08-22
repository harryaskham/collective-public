#!/usr/bin/env bash

function maybe-install-nix() {
  if ! which nix; then
    sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
    . /home/ubuntu/.nix-profile/etc/profile.d/nix.sh
  fi
}

function maybe-bootstrap-cursor-agent() {
  if [[ "$(hostname)" == "cursor" ]]; then
    maybe-install-nix
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
  EXPR=$1
  shift
  nix --extra-experimental-features nix-command eval --impure --show-trace --expr "$EXPR" $@
}

function run-in-nix-eval-flake() {
  EXPR=$1
  shift
  nix eval --impure --show-trace --apply "collective-lib: $EXPR" $@ .#lib.x86_64-linux
}

function run-in-nix-repl() {
  EXPR=$1
  shift
  EXPECT_SCRIPT=$(cat << EOF
spawn nix repl --show-trace $@
expect "nix-repl> "
send "$EXPR\r" 
expect "nix-repl> "
send ""
interact
EOF
)
  expect -c "$EXPECT_SCRIPT"
}

function eval-expr() {
  RAW_EXPR=$(echo -n "$1")
  shift
  EXPR=$(wrap-nix-expr "$RAW_EXPR")
  echo "Running Nix expression via eval strategy '$CLTV_EVAL_STRATEGY'" >&2
  echo "$EXPR" >&2
  case "${CLTV_EVAL_STRATEGY:-eval}" in
    repl)
      run-in-nix-repl "$EXPR" $@
      ;;
    eval)
      run-in-nix-eval "$EXPR" $@
      ;;
    flake)
      run-in-nix-eval-flake "$EXPR" $@
      ;;
  esac
}

function run-tests() {
  if [[ -z "$1" ]]; then
    printf "$(eval-expr "collective-lib._tests.run {}" --raw 2>&1)"
  else
    printf "$(eval-expr "collective-lib.$1._tests.run {}" --raw 2>&1)"
  fi
}

function debug-tests() {
  if [[ -z "$1" ]]; then
    printf "$(eval-expr "collective-lib._tests.debug {}" --raw 2>&1)"
  else
    printf "$(eval-expr "collective-lib.$1._tests.debug {}" --raw 2>&1)"
  fi
}
