#!/usr/bin/env bash
# Benchmarks preprocessing and pipelines against GCC and Clang.
#
# Requires hyperfine, fd, gcc, and clang on PATH. The dev shell provides them.
# Set LINUX to the Linux kernel tree.

set -euo pipefail

linux="${LINUX:?set LINUX to the Linux kernel tree}"
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
preprocess="$repo/target/release/examples/preprocess"
cli="$repo/target/release/dt-tools-cli"
pipeline="$repo/scripts/benchmark-pipeline.sh"
dts_root="$linux/arch/arm64/boot/dts"
board="$dts_root/qcom/sm7225-fairphone-fp4.dts"
corpus="$dts_root/qcom"

cargo build --release --manifest-path "$repo/Cargo.toml" -p dt-tools-cli
cargo build --release --manifest-path "$repo/Cargo.toml" \
  -p dt-tools-preprocessor --example preprocess

echo "== corpus is $corpus"

# for $pipeline
export LINUX PREPROCESS="$preprocess" DT_TOOLS_CLI="$cli"

# Quotes arguments for hyperfine's shell-word parser.
quote() {
  printf '"%s" ' "$@"
}

include=(-I "$linux/include" -I "$linux/scripts/dtc/include-prefixes")
cpp_flags=(-E -nostdinc -undef -D__DTS__ -x assembler-with-cpp -P)

our_cmd="$preprocess $(quote "${include[@]}") -D __DTS__"
expand_cmd="$cli $(quote "${include[@]}") -D __DTS__ expand"
mir_cmd="$cli $(quote "${include[@]}") -D __DTS__ mir"
gcc_cmd="gcc $(quote "${cpp_flags[@]}") $(quote "${include[@]}")"
clang_cmd="clang $(quote "${cpp_flags[@]}") $(quote "${include[@]}")"

hyperfine=(hyperfine --warmup 3 --shell=none --export-markdown -)

echo "== single file preprocessing: ${board#"$linux/"}"
"${hyperfine[@]}" \
  -n dt-tools-preprocessor "$our_cmd $(quote "$board")" \
  -n dt-tools-expand "$expand_cmd $(quote "$board")" \
  -n gcc "$gcc_cmd $(quote "$board")" \
  -n clang "$clang_cmd $(quote "$board")"

echo
echo "== single file: pipelines"
"${hyperfine[@]}" \
  -n "preprocess + dtc" "$(quote "$pipeline") dtc $(quote "$board")" \
  -n "preprocess + mir" "$(quote "$pipeline") mir $(quote "$board")" \
  -n "mir" "$mir_cmd $(quote "$board")"

echo
echo "== corpus: preprocessing"
fd_cmd="fd -e dts . $(quote "$corpus") -x"
"${hyperfine[@]}" \
  -n dt-tools-preprocessor "$fd_cmd $our_cmd" \
  -n dt-tools-expand "$fd_cmd $expand_cmd" \
  -n gcc "$fd_cmd $gcc_cmd" \
  -n clang "$fd_cmd $clang_cmd"

fd_cmd_batch="fd -e dts . $(quote "$corpus") -X"
check_many_cmd="$fd_cmd_batch $(quote "$cli") $(quote "${include[@]}") -D __DTS__ check-many --only-summary"

echo
echo "== corpus: pipelines"
"${hyperfine[@]}" \
  -n "preprocess + dtc" "$fd_cmd $(quote "$pipeline") dtc" \
  -n "preprocess + mir" "$fd_cmd $(quote "$pipeline") mir" \
  -n "mir" "$fd_cmd $mir_cmd" \
  -n "check-many" "$check_many_cmd"
