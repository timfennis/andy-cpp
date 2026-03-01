#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 3 ]; then
    echo "Usage: $0 <branch1> <branch2> <script.ndc>" >&2
    exit 1
fi

BRANCH1="$1"
BRANCH2="$2"
SCRIPT="$(realpath "$3")"
REPO_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
WORKDIR="$(mktemp -d)"

cleanup() {
    git -C "$REPO_ROOT" worktree remove --force "$WORKDIR/b1" 2>/dev/null || true
    git -C "$REPO_ROOT" worktree remove --force "$WORKDIR/b2" 2>/dev/null || true
    rm -rf "$WORKDIR"
}
trap cleanup EXIT

build_branch() {
    local branch="$1"
    local worktree="$2"
    local out="$3"

    echo "==> Building $branch..."
    git -C "$REPO_ROOT" worktree add --quiet --detach "$worktree" "$branch"
    cargo build --release --quiet --manifest-path "$worktree/Cargo.toml" -p ndc_bin 2>&1
    cp "$worktree/target/release/ndc" "$out"
    echo "    Built $branch -> $out"
}

BIN1="$WORKDIR/ndc-$(echo "$BRANCH1" | tr '/' '-')"
BIN2="$WORKDIR/ndc-$(echo "$BRANCH2" | tr '/' '-')"

build_branch "$BRANCH1" "$WORKDIR/b1" "$BIN1"
build_branch "$BRANCH2" "$WORKDIR/b2" "$BIN2"

echo ""
hyperfine \
    --warmup 3 \
    --shell none \
    -n "$BRANCH1" "$BIN1 $SCRIPT" \
    -n "$BRANCH2" "$BIN2 $SCRIPT"
