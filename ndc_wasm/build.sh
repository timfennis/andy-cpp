#!/usr/bin/env bash
# Builds the browser playground into ndc_wasm/web.
#
# Needs: rustup target add wasm32-unknown-unknown
#        cargo install wasm-bindgen-cli --version <the wasm-bindgen version in Cargo.lock>
# Then serve the folder, for example: python3 -m http.server -d ndc_wasm/web
set -euo pipefail
cd "$(dirname "$0")/.."

cargo build -p ndc_wasm --target wasm32-unknown-unknown --profile wasm-release
# The name section only serves stack traces; it is a fifth of the module.
wasm-bindgen --target web --remove-name-section --out-dir ndc_wasm/web/pkg \
    target/wasm32-unknown-unknown/wasm-release/ndc_wasm.wasm
