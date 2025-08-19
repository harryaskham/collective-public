#!/usr/bin/env bash
source ./scripts/cursor_agent_bootstrap.sh
maybe-bootstrap-cursor-agent
nix develop .#test
source ./scripts/run_expr.sh
run-expr "$@"
