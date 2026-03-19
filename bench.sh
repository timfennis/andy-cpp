#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <file.ndc>" >&2
    exit 1
fi

FILE="$1"

if [[ ! -f "$FILE" ]]; then
    echo "Error: file not found: $FILE" >&2
    exit 1
fi

if ! command -v ndc &>/dev/null; then
    echo "Error: 'ndc' not found in PATH" >&2
    exit 1
fi

if ! command -v hyperfine &>/dev/null; then
    echo "Error: 'hyperfine' not found in PATH" >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOCAL="$SCRIPT_DIR/target/release/ndc"

echo "Building release binary..."
cargo build --release --manifest-path "$SCRIPT_DIR/Cargo.toml" -q

echo ""
echo "Validating the VM version can run '$FILE' without crashing..."

validate() {
    local label="$1"
    local cmd=("${@:2}")
    if ! "${cmd[@]}" &>/dev/null; then
        echo "  FAIL: $label crashed or returned an error" >&2
        exit 1
    fi
    echo "  OK:   $label"
}

validate "ndc (PATH)"        ndc       run "$FILE"
validate "local release (VM)" "$LOCAL" run "$FILE"

echo ""
echo "Running benchmark..."
echo ""

hyperfine \
    --warmup 3 \
    --command-name "ndc (installed)" "ndc run '$FILE'" \
    --command-name "local release (VM)" "'$LOCAL' run '$FILE'"
