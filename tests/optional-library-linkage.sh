#!/bin/sh
# Validate the current binary against the generated feature configuration.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
for feature in CARES CURL GRPC IDN2 LIBSSH WEBSOCKETS ZLIB OPENSSL UUID XXHASH BLAKE3; do
    case "$feature" in
        CARES) library=cares ;;
        CURL) library=curl ;;
        GRPC) library='(grpc|gpr)' ;;
        IDN2) library=idn2 ;;
        LIBSSH) library=ssh ;;
        WEBSOCKETS) library=websockets ;;
        ZLIB) library='z\.' ;;
        OPENSSL) library='(ssl|crypto)' ;;
        UUID) library=uuid ;;
        XXHASH) library=xxhash ;;
        BLAKE3) library=blake3 ;;
    esac
    if grep -q "^#define CHEZPP_WITH_${feature} 0$" "$project_root/chezpp/c/build-config.h" && \
       readelf -d "$project_root/libchezpp.so" | grep -Eq "NEEDED.*lib${library}"; then
        printf 'disabled %s remains a linker dependency\n' "$feature" >&2
        exit 1
    fi
done
# Enabled integrations must reference real dependency APIs; disabled imports must stay safe.
exec "$project_root/chez++" --script "$project_root/tests/optional-library-runtime.ss"
