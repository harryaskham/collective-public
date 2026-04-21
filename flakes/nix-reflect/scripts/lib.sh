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

function color() {
  IFS=''
  while read -r line ; do
    printf "$line\n"
  done
}

function with-installable() {
  installable="$1"
  arg_name="$2"
  expr="$3"
  shift 3
  flags=()
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

function fast-mode () {
  export CLTV_TRACE_LEVEL=0
  export CLTV_PRETTY=0
  export CLTV_ENABLE_STRING_DIFF=0
  exec "$@"
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

function compare-lazy-strict() {
  base_expr="$1"
  shift
  echo "Expression:"
  echo "$base_expr"
  for eval_fn in "lib.eval.lazy" "lib.eval.strict"; do
    echo ""
    echo "Evaluated via $eval_fn:"
    timeout 5 eval-expr "$eval_fn ''${base_expr}'' " || echo "Timeout"
  done
}