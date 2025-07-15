#!/usr/bin/env bash

RAW_EXPR="$@"
if [[ -z "$RAW_EXPR" ]]; then
  RAW_EXPR="collective-lib._tests.run {}"
fi

EXPR=$(cat << EOF
let
  collective-lib = import ./pkgs/collective-lib {
    traceOpts = {
      traceLevel = 1;
      enablePartialTrace = false;
      enableVerboseTrace = false;
      enableShortTrace = false;
    };
  };
in 
  $RAW_EXPR
EOF
)

echo "Running:\n$EXPR"

nix eval --impure --expr "$EXPR"
