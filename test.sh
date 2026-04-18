#!/usr/bin/env bash
# Convenience entry point for running the collective-lib test suite.
# Documented in CLAUDE.md / AGENTS.md as `./collective-public/test.sh`.
#
# Delegates to scripts/run-tests, exporting COLLECTIVE_PUBLIC_DIR so the
# helper scripts can locate themselves regardless of the caller's CWD.
#
# Usage:
#   ./collective-public/test.sh              # run all suites
#   ./collective-public/test.sh functions    # run a single library suite
set -euo pipefail

# Resolve this script's directory regardless of how it was invoked.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export COLLECTIVE_PUBLIC_DIR="$SCRIPT_DIR"

exec "$SCRIPT_DIR/scripts/run-tests" "$@"
