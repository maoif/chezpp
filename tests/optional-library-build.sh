#!/bin/sh
# Verify complete builds using absent dependencies and an isolated direct-link fixture.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture_directory=$(mktemp -d)
trap 'rm -rf -- "$fixture_directory"' EXIT HUP INT TERM
fixture_project="$fixture_directory/project"
mkdir -p "$fixture_project" "$fixture_directory/include"

# Keep every build and generated file outside the user's checkout.
tar -C "$project_root" --exclude=.git --exclude=.worktrees --exclude=.superpowers \
    --exclude='*.so' --exclude='*.wpo' --exclude='*.lib' --exclude='*.covin' \
    --exclude='*.covout' --exclude='*.stdout' --exclude='*.stderr' \
    --exclude=chez++ --exclude=chez++.ss --exclude=.chezpp-build-options \
    --exclude=.chezpp-native-options.mk --exclude=build-config.h -cf - . | \
  tar -C "$fixture_project" -xf -

build_fixture_project() {
    build_label=$1
    shift
    if ! (cd "$fixture_project" && make clean && make "$@") \
        > "$fixture_directory/$build_label.log" 2>&1; then
        cat "$fixture_directory/$build_label.log" >&2
        exit 1
    fi
}

check_no_optional_linkage() {
    if readelf -d "$fixture_project/libchezpp.so" | \
        grep -E 'NEEDED.*lib(cares|curl|grpc|gpr|idn2|ssh|websockets|z\.|ssl|crypto|uuid|xxhash|blake3|chezpp-idn2-fixture)' \
        > "$fixture_directory/unexpected-needed"; then
        cat "$fixture_directory/unexpected-needed" >&2
        exit 1
    fi
}

run_probe() {
    probe_mode=$1
    shift
    if ! "$fixture_project/.chezscheme-install/bin/scheme" --script \
        "$fixture_project/tests/optional-linkage-probe.ss" \
        "$fixture_project/libchezpp.so" "$fixture_project/chezpp.lib" "$probe_mode" \
        "$fixture_project/tests" \
        > "$fixture_directory/probe.stdout" 2> "$fixture_directory/probe.stderr"; then
        cat "$fixture_directory/probe.stdout" >&2
        cat "$fixture_directory/probe.stderr" >&2
        exit 1
    fi
    if [ -s "$fixture_directory/probe.stdout" ] || [ -s "$fixture_directory/probe.stderr" ]; then
        cat "$fixture_directory/probe.stdout" >&2
        cat "$fixture_directory/probe.stderr" >&2
        exit 1
    fi
}

# No pkg-config or development metadata: auto must still produce an importable Chezpp.
build_fixture_project auto PKG_CONFIG="$fixture_directory/missing-pkg-config"
check_no_optional_linkage
run_probe disabled

dependency_names='CARES CURL GRPC IDN2 LIBSSH WEBSOCKETS ZLIB OPENSSL UUID XXHASH BLAKE3'
set --
for dependency_name in $dependency_names; do
    set -- "$@" "WITH_${dependency_name}=0"
done

# Force-disable every integration without consulting even a usable pkg-config executable.
cat > "$fixture_directory/forbidden-pkg-config" <<'SH'
#!/bin/sh
printf '%s\n' called >> "$CHEZPP_TEST_PKG_CONFIG_LOG"
echo 'pkg-config must not be called for a disabled dependency' >&2
exit 1
SH
chmod +x "$fixture_directory/forbidden-pkg-config"
CHEZPP_TEST_PKG_CONFIG_LOG="$fixture_directory/pkg-config-calls"
export CHEZPP_TEST_PKG_CONFIG_LOG
build_fixture_project disabled "$@" PKG_CONFIG="$fixture_directory/forbidden-pkg-config"
[ ! -e "$CHEZPP_TEST_PKG_CONFIG_LOG" ] || {
    printf '%s\n' 'disabled build consulted pkg-config' >&2
    exit 1
}
check_no_optional_linkage
run_probe disabled

# Complete manual header/library overrides must work without installing anything.
cat > "$fixture_directory/include/idn2.h" <<'C'
#ifndef CHEZPP_TEST_IDN2_H
#define CHEZPP_TEST_IDN2_H
#include <stdint.h>
#define IDN2_OK 0
#define IDN2_NFC_INPUT 1
#define IDN2_NONTRANSITIONAL 8
#define IDN2_USE_STD3_ASCII_RULES 32
const char *idn2_check_version(const char *required);
int idn2_lookup_u8(const uint8_t *input, uint8_t **output, int flags);
int idn2_to_unicode_8z8z(const char *input, char **output, int flags);
const char *idn2_strerror(int status);
void idn2_free(void *data);
#endif
C
cat > "$fixture_directory/idn2.c" <<'C'
#include <stdlib.h>
#include <string.h>
#include "idn2.h"
const char *idn2_check_version(const char *required) {
    (void)required;
    return "2.3.9";
}
int idn2_lookup_u8(const uint8_t *input, uint8_t **output, int flags) {
    if (strcmp((const char *)input, "error.test") == 0 || flags != 41) return -1;
    *output = (uint8_t *)strdup("fixture.example");
    return *output == NULL ? -1 : IDN2_OK;
}
int idn2_to_unicode_8z8z(const char *input, char **output, int flags) {
    (void)input;
    if (flags != 9) return -1;
    *output = strdup("fixture-\303\274.test");
    return *output == NULL ? -1 : IDN2_OK;
}
const char *idn2_strerror(int status) {
    (void)status;
    return "fixture conversion error";
}
void idn2_free(void *data) { free(data); }
C
${CC:-cc} -Wall -Wextra -fPIC -shared -I"$fixture_directory/include" \
    -Wl,-soname,libchezpp-idn2-fixture.so "$fixture_directory/idn2.c" \
    -o "$fixture_directory/libchezpp-idn2-fixture.so"
build_fixture_project manual "$@" WITH_IDN2=1 \
    PKG_CONFIG="$fixture_directory/missing-pkg-config" \
    "IDN2_CFLAGS=-I$fixture_directory/include" \
    "IDN2_LIBS=-L$fixture_directory -Wl,-rpath,$fixture_directory -lchezpp-idn2-fixture"
readelf -d "$fixture_project/libchezpp.so" | \
    grep -F 'Shared library: [libchezpp-idn2-fixture.so]' >/dev/null
nm -D "$fixture_project/libchezpp.so" | grep -E ' U idn2_lookup_u8$' >/dev/null
run_probe idn2

# Running tests must retain the manual feature flags, including spaces and empty values.
manual_signature=$(cat "$fixture_project/.chezpp-build-options")
if ! make -C "$fixture_project/tests" test optional-library.ss \
    > "$fixture_directory/manual-tests.log" 2>&1; then
    cat "$fixture_directory/manual-tests.log" >&2
    exit 1
fi
[ "$manual_signature" = "$(cat "$fixture_project/.chezpp-build-options")" ] || {
    printf '%s\n' 'test runner changed the native build settings' >&2
    exit 1
}
run_probe idn2

# Reconfiguring an enabled integration to disabled removes the linker dependency.
if ! (cd "$fixture_project" && make "$@" \
    PKG_CONFIG="$fixture_directory/missing-pkg-config") \
    > "$fixture_directory/reconfigured.log" 2>&1; then
    cat "$fixture_directory/reconfigured.log" >&2
    exit 1
fi
check_no_optional_linkage
run_probe disabled
