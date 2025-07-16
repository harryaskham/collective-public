#!/usr/bin/env bash
RAW_EXPR="$@"
if [[ -z "$RAW_EXPR" ]]; then
  RAW_EXPR="collective-lib._tests.run {}"
fi

EXPR=$(cat << EOF
let
  collective-lib = import ./pkgs/collective-lib {
    traceOpts = {
      traceLevel = 0;
      enablePartialTrace = false;
      enableVerboseTrace = false;
      enableShortTrace = false;
    };
  };
in 
  $RAW_EXPR
EOF
)

NIX_DAEMON=$(which nix-daemon)
NIX=$(which nix)

source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
if ! pgrep nix-daemon; then
  sudo $NIX_DAEMON &
fi

echo "Running:\n$EXPR"
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
