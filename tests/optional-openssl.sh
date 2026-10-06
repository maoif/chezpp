#!/bin/sh
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if grep -q '^#define CHEZPP_WITH_OPENSSL 0$' "$project_root/chezpp/c/build-config.h"; then
    if readelf -d "$project_root/libchezpp.so" | grep -Eq 'NEEDED.*lib(ssl|crypto)'; then
        printf '%s\n' 'disabled OpenSSL remains a linker dependency' >&2
        exit 1
    fi
fi
exec "$project_root/chez++" --script "$project_root/tests/optional-library-runtime.ss"
