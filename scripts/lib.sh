#!/usr/bin/env bash

#function wrap-nix-expr() {
#  cat << EOF
#let
#  pkgs = import <nixpkgs> {};
#  lib = pkgs.lib;
#  collective-lib = import ${CLTV_ROOT:-.}/pkgs/collective-lib {
#    inherit pkgs lib;
#    traceOpts = {
#      traceLevel = ${CLTV_TRACE_LEVEL:-0};
#      enablePartialTrace = ${CLTV_PARTIAL_TRACE:-false};
#      enableVerboseTrace = ${CLTV_VERBOSE_TRACE:-false};
#      enableShortTrace = ${CLTV_SHORT_TRACE:-false};
#    };
#  };
#in
#  with collective-lib;
#  ($@)
#EOF
#}

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

function color() {
  IFS=''
  while read -r line ; do
    (printf "$line\n" 1>&2) 2>&1
  done
}

function with-installable() {
  installable="$1"
  arg_name="$2"
  expr="$3"
  shift 3
  flags=()
  # bd-00e92f: the collective-lib suite exercises nix-reflect's AST evaluator,
  # whose deep recursive round-trips (e.g. the lazy factorial `f 2`+) exceed
  # Nix's default max-call-depth and overflow. nix-reflect's own test runner
  # raises it to 1000000 for exactly this reason; match that here so running
  # those tests through collective-public does not spuriously stack-overflow.
  flags+=(--option max-call-depth 1000000)
  if [[ "$CLTV_TRACE_LEVEL" != "0" ]]; then
    flags+=(--show-trace)
  fi
  nix eval --impure ${flags[@]} --apply "$arg_name: $expr" ${@} $installable
}

function with-lib() {
  expr="$1"
  shift 
  with-installable ".#lib.x86_64-linux" "lib" "with lib; $expr" ${@}
}

function eval-expr() {
  expr="$1"
  shift 1
  with-lib "$expr" --raw 2>&1 \
    | sed -u "s/trace: start_trace(\(.\+\)): /\\\\e[90m[\\1] \\\\e[0m/" \
    | sed -u '/warning.*dirty/d' \
    | grep --line-buffered -v "^trace: end_trace$" \
    | color
}

function run-module-tests() {
  eval-expr "(lib.tests.testModule (import modules/agnostic/unexpected-keyboard)).run {}"
}

function debug-module-tests() {
  eval-expr "(lib.tests.testModule (import modules/agnostic/unexpected-keyboard)).debug {}"
}

function run-tests() {
  if [[ -z "$1" ]]; then
    eval-expr "lib._tests.run {}"
  else
    eval-expr "lib.$1._tests.run {}"
  fi
}

function debug-tests() {
  if [[ -z "$1" ]]; then
    eval-expr "lib._tests.debug {}"
  else
    eval-expr "lib.$1._tests.debug {}"
  fi
}

function run-test() {
  if [[ -z "$2" ]]; then
    eval-expr "with (import <nixpkgs/lib>); concatStringsSep \"\\n\" (attrNames (lib.$1._tests.runOne))"
  else
    eval-expr "lib.$1._tests.runOne.$2 {} {}"
  fi
}

function debug-test() {
  if [[ -z "$2" ]]; then
    eval-expr "with (import <nixpkgs/lib>); concatStringsSep \"\\n\" (attrNames (lib.$1._tests.debugOne))"
  else
    eval-expr "lib.$1._tests.debugOne.$2 {} {}"
  fi
}