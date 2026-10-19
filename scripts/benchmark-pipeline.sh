#!/usr/bin/env bash
# Runs one two-stage pipeline for benchmark-preprocessing.sh.

set -euo pipefail

linux="${LINUX:?set LINUX to the Linux kernel tree}"
preprocess="${PREPROCESS:?set PREPROCESS to the preprocessor example}"
cli="${DT_TOOLS_CLI:?set DT_TOOLS_CLI to dt-tools-cli}"

mode="$1"
file="$2"
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

"$preprocess" \
  -I "$linux/include" \
  -I "$linux/scripts/dtc/include-prefixes" \
  -D __DTS__ \
  "$file" >"$tmp" || true

case "$mode" in
  dtc) dtc -I dts -O dtb -o /dev/null "$tmp" || true ;;
  mir) "$cli" mir "$tmp" || true ;;
  *)
    echo "unknown mode: $mode" >&2
    exit 2
    ;;
esac
