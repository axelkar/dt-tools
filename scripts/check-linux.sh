#!/usr/bin/env bash
# Checks all devicetree files in the Linux kernel tree in one pass.

set -euo pipefail

linux="${LINUX:?set LINUX to the Linux kernel tree}"
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cargo run --manifest-path "$repo/Cargo.toml" -q -p dt-tools-cli --release -- \
  --workspace-start-dir $linux check-many --only-fail --print-diagnostics \
  $(fd -edts -edtso --exclude=/scripts/dtc/dt-style-selftest . $linux)
