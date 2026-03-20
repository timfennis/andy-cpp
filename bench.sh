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
echo "Validating versions can run '$FILE' without crashing..."

validate() {
    local label="$1"
    local cmd=("${@:2}")
    if ! "${cmd[@]}" &>/dev/null; then
        echo "  FAIL: $label crashed or returned an error" >&2
        exit 1
    fi
    echo "  OK:   $label"
}

validate "ndc (PATH)"         ndc      run "$FILE"
validate "local release (VM)" "$LOCAL" run "$FILE"

# ndc2 and ndc1 are optional: older versions may not support all language features
# ndc1 also has a different CLI (no 'run' subcommand)
USE_NDC2=false
if command -v ndc2 &>/dev/null; then
    if ndc2 run "$FILE" &>/dev/null; then
        echo "  OK:   ndc2 (v0.2.1)"
        USE_NDC2=true
    else
        echo "  SKIP: ndc2 (v0.2.1) failed to run '$FILE', skipping"
    fi
else
    echo "  SKIP: ndc2 not found, skipping"
fi

USE_NDC1=false
if command -v ndc1 &>/dev/null; then
    if ndc1 "$FILE" &>/dev/null; then
        echo "  OK:   ndc1 (v0.1.0)"
        USE_NDC1=true
    else
        echo "  SKIP: ndc1 (v0.1.0) failed to run '$FILE', skipping"
    fi
else
    echo "  SKIP: ndc1 not found, skipping"
fi

echo ""
echo "Running benchmark..."
echo ""

NDC2_ARGS=()
if [[ "$USE_NDC2" == true ]]; then
    NDC2_ARGS=(--command-name "ndc2 (v0.2.1)" "ndc2 run '$FILE'")
fi

NDC1_ARGS=()
if [[ "$USE_NDC1" == true ]]; then
    NDC1_ARGS=(--command-name "ndc1 (v0.1.0)" "ndc1 '$FILE'")
fi

hyperfine \
    --warmup 3 \
    --command-name "ndc (installed)" "ndc run '$FILE'" \
    --command-name "local release (VM)" "'$LOCAL' run '$FILE'" \
    "${NDC2_ARGS[@]}" \
    "${NDC1_ARGS[@]}"
