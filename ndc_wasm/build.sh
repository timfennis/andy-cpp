#!/usr/bin/env bash
# Builds the interpreter for the browser into the manual's playground.
#
# Needs: rustup target add wasm32-unknown-unknown
#        cargo install wasm-bindgen-cli --version <the wasm-bindgen version in Cargo.lock>
# Then build or serve the manual: mdbook serve manual
set -euo pipefail
cd "$(dirname "$0")/.."

cargo build -p ndc_wasm --target wasm32-unknown-unknown --profile wasm-release
# The name section only serves stack traces; it is a fifth of the module.
wasm-bindgen --target web --remove-name-section --out-dir manual/src/playground/pkg \
    target/wasm32-unknown-unknown/wasm-release/ndc_wasm.wasm
