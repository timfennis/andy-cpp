#!/usr/bin/env bash
# Bundles the playground's editor (CodeMirror) into manual/src/playground/editor.js.
#
#   ./manual/editor/build.sh                 build the bundle
#   ./manual/editor/build.sh add <pkg>...    add dependencies (or upgrade them)
#
# npm only ever runs inside a Docker container, pinned by digest, as the user
# running this script. To build, the sources are mounted read-only and copied
# into the container; `npm ci` installs exactly what package-lock.json lists,
# checking every package against its integrity hash, with install scripts off
# (see .npmrc); only the bundle is written back. Adding dependencies resolves
# versions published at least $MIN_AGE_DAYS days ago, as malicious releases
# tend to be caught and pulled within days, and writes back only package.json
# and package-lock.json. No node_modules ever lands on the host.
set -euo pipefail
cd "$(dirname "$0")"

IMAGE="node:22.23.5-alpine@sha256:8af05ed9012b24a9205075889b231b65b47f8cc299f3034de2b8a864780ee07f"
MIN_AGE_DAYS=30
OUT="$(cd ../src/playground && pwd)"

run() {
    docker run --rm --user "$(id -u):$(id -g)" --env HOME=/tmp \
        --volume "$PWD:/src:ro" "$@"
}

case "${1:-build}" in
build)
    run --volume "$OUT:/out" "$IMAGE" sh -euc '
        mkdir /tmp/editor && cd /tmp/editor
        cp -r /src/.npmrc /src/package.json /src/package-lock.json /src/src .
        npm ci --no-audit --no-fund
        node_modules/.bin/esbuild src/editor.js --bundle --format=esm --minify \
            --target=es2020 --legal-comments=none --outfile=/out/editor.js
        # The licences of everything bundled, which they ask to ship with it.
        for dir in $(npm ls --omit=dev --all --parseable | tail -n +2); do
            node -e "const p = require(\"$dir/package.json\"); console.log(\"==\", p.name, p.version)"
            cat "$dir"/LICEN[CS]E* 2>/dev/null || echo "(no licence file)"
            echo
        done > /out/editor.licenses.txt'
    ;;
add)
    shift
    before="$(date -u -d "$MIN_AGE_DAYS days ago" +%Y-%m-%d)"
    run --volume "$PWD:/work" "$IMAGE" sh -euc '
        mkdir /tmp/editor && cd /tmp/editor
        cp /src/.npmrc /src/package.json .
        [ -f /src/package-lock.json ] && cp /src/package-lock.json .
        npm install --before="$0" "$@"
        cp package.json package-lock.json /work/' "$before" "$@"
    ;;
*)
    echo "usage: $0 [build | add <package>...]" >&2
    exit 2
    ;;
esac
