#!/bin/sh
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
for dependency in xxhash blake3; do
    feature=$(printf '%s' "$dependency" | tr '[:lower:]' '[:upper:]')
    if grep -q "^#define CSPP_WITH_${feature} 1$" "$project_root/src/cspp/c/build-config.h"; then
        expected=success
    else
        expected='disabled at build time'
        if readelf -d "$project_root/libcspp.so" | grep -Eq "NEEDED.*lib${dependency}"; then
            printf 'disabled %s remains a linker dependency\n' "$dependency" >&2
            exit 1
        fi
    fi
    "$project_root/cspp" --script="$project_root/tests/optional-hash-libs.ss" \
        "$dependency" "$expected"
done
