#!/usr/bin/env bash
set -eux

fd -eprofraw --no-ignore -xrm
RUSTFLAGS='-Cinstrument-coverage' LLVM_PROFILE_FILE='cargo-test-%p-%m.profraw' cargo test
grcov . -s . --binary-path ./target/debug/ -t html --branch --ignore-not-existing -o ./target/debug/coverage/ --ignore "/*"

echo 'Open ./target/debug/coverate/html/index.html to view the results.'
echo 'Note that there is no rule about code coverage.'
