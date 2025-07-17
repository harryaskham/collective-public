#!/usr/bin/env bash
RAW_EXPR="$@"
if [[ -z "$RAW_EXPR" ]]; then
  RAW_EXPR="collective-lib._tests.run {}"
fi

EXPR=$(cat << EOF
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
  lib.traceSeq ($RAW_EXPR) {}
EOF
)

NIX_DAEMON=$(which nix-daemon)
NIX=$(which nix)

if ! pgrep nix-daemon; then
  source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  sudo $NIX_DAEMON &
fi

echo "Running:"
echo "$EXPR"

LOAD_REPL_EXP=$(cat << EOF                                                                                                    
spawn nix repl --show-trace
expect "nix-repl> "
send ":lf .\r" 
expect "nix-repl> "
send "${EXPR}\r" 
expect "nix-repl> "
send ""
interact
EOF
)                                                                                                                             
expect -c "$LOAD_REPL_EXP" 
