#!/bin/sh

set -eu

usage() {
    echo "Usage: $0 <neovim|helix>" >&2
}

editor=${1:-}
case "$editor" in
    neovim|nvim)
        editor=neovim
        ;;
    helix)
        ;;
    -h|--help)
        usage
        exit 0
        ;;
    *)
        usage
        exit 2
        ;;
esac

source_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
config_home=${XDG_CONFIG_HOME:-${HOME:?HOME must be set when XDG_CONFIG_HOME is unset}/.config}
compiler=${CC:-cc}

if ! command -v "$compiler" >/dev/null 2>&1; then
    echo "C compiler not found: $compiler" >&2
    echo "Install a C compiler or set CC to its executable path." >&2
    exit 1
fi

case $(uname -s) in
    Darwin)
        shared_flag=-dynamiclib
        ;;
    Linux|FreeBSD|OpenBSD|NetBSD)
        shared_flag=-shared
        ;;
    *)
        echo "Unsupported operating system: $(uname -s)" >&2
        exit 1
        ;;
esac

case "$editor" in
    neovim)
        parser_dir=$config_home/nvim/parser
        query_dir=$config_home/nvim/queries/andy_cpp
        parser_name=andy_cpp.so
        ;;
    helix)
        parser_dir=$config_home/helix/runtime/grammars
        query_dir=$config_home/helix/runtime/queries/andy-cpp
        parser_name=andy-cpp.so
        ;;
esac

build_dir=$(mktemp -d "${TMPDIR:-/tmp}/tree-sitter-andy-cpp.XXXXXX")
cleanup() {
    rm -rf "$build_dir"
}
trap cleanup 0 1 2 15

"$compiler" -O2 -fPIC "$shared_flag" -I"$source_dir/src" \
    "$source_dir/src/parser.c" "$source_dir/src/scanner.c" \
    -o "$build_dir/$parser_name"

mkdir -p "$parser_dir" "$query_dir"
cp "$build_dir/$parser_name" "$parser_dir/$parser_name"
cp "$source_dir"/queries/*.scm "$query_dir/"

echo "Installed the Andy C++ Tree-sitter parser for $editor:"
echo "  parser: $parser_dir/$parser_name"
echo "  queries: $query_dir"
