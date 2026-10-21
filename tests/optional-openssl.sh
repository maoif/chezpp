#!/bin/sh
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if grep -q '^#define CSPP_WITH_OPENSSL 0$' "$project_root/src/cspp/c/build-config.h"; then
    if readelf -d "$project_root/libcspp.so" | grep -Eq 'NEEDED.*lib(ssl|crypto)'; then
        printf '%s\n' 'disabled OpenSSL remains a linker dependency' >&2
        exit 1
    fi
fi
exec "$project_root/cspp" --script="$project_root/tests/optional-library-runtime.ss"
