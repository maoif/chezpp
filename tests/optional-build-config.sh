#!/bin/sh
# Exercise configuration with real compile/link probes and isolated dependency fixtures.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
names='CARES CURL GRPC IDN2 LIBSSH WEBSOCKETS ZLIB OPENSSL UUID XXHASH BLAKE3'
disabled=''
for name in $names; do disabled="$disabled WITH_${name}=0"; done
mkdir -p "$tmp/include/curl"
printf '%s\n' '#define CURLVERSION_NOW 0' 'void *curl_version_info(int);' > "$tmp/include/curl/curl.h"
printf '%s\n' 'void *curl_version_info(int v) { (void)v; return 0; }' > "$tmp/curl.c"
${CC:-cc} -c "$tmp/curl.c" -o "$tmp/curl.o"
ar rcs "$tmp/libfixture.a" "$tmp/curl.o"
cat > "$tmp/show.mk" <<'MAKE'
.PHONY: show-optional-config
show-optional-config:
	@printf '%s\n' $(call build-shell-quote,CURL=$(RESOLVED_WITH_CURL)) $(call build-shell-quote,CFLAGS=$(OPTIONAL_CFLAGS)) $(call build-shell-quote,LIBS=$(OPTIONAL_LIBS)) $(call build-shell-quote,SIGNATURE=$(BUILD_OPTIONS_SIGNATURE)) $(call build-shell-quote,SOURCES=$(SRCS_C)) $(call build-shell-quote,RESOLVED=$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(name)=$(RESOLVED_WITH_$(name))))
MAKE
run_make() {
    make --no-print-directory -s -C "$root" -f Makefile -f "$tmp/show.mk" \
        $disabled PKG_CONFIG="$tmp/no-pkg-config" "$@"
}
assert_contains() {
    printf '%s\n' "$1" | grep -F -- "$2" >/dev/null || {
        printf 'missing expected configuration output: %s\n%s\n' "$2" "$1" >&2
        exit 1
    }
}
# Invalid switches must be rejected instead of silently disabling a dependency.
for name in $names; do
    if result=$(run_make show-optional-config "WITH_${name}=yes" 2>&1); then
        printf 'invalid WITH_%s=yes was accepted\n' "$name" >&2
        exit 1
    fi
    assert_contains "$result" "WITH_${name}"
done

result=$(run_make show-optional-config)
assert_contains "$result" 'CURL=0'
assert_contains "$result" 'CFLAGS='
assert_contains "$result" 'LIBS='
assert_contains "$result" 'chezpp/c/net/ftp_unavailable.c'
assert_contains "$result" 'chezpp/c/uuid_unavailable.c'
assert_contains "$result" 'chezpp/c/hash_unavailable.c'
assert_contains "$result" 'chezpp/c/zlib_unavailable.c'
assert_contains "$result" 'chezpp/c/crypto_unavailable.c'
assert_contains "$result" 'chezpp/c/net/tls_unavailable.c'

# Supplied flags must enable a dependency without pkg-config, using an actual linker.
result=$(run_make show-optional-config WITH_CURL=1 \
    "CURL_CFLAGS=-nostdinc -I$tmp/include" "CURL_LIBS=$tmp/libfixture.a")
assert_contains "$result" 'CURL=1'
assert_contains "$result" "CFLAGS=-nostdinc -I$tmp/include"
assert_contains "$result" "LIBS=$tmp/libfixture.a"
assert_contains "$result" 'chezpp/c/net/ftp.c'

# Accepted padded switches must retain force-disable and required-enable semantics.
padding_failures=0
result=$(run_make show-optional-config 'WITH_CURL= 0 ' \
    "CURL_CFLAGS=-nostdinc -I$tmp/include" "CURL_LIBS=$tmp/libfixture.a")
if ! printf '%s\n' "$result" | grep -Fx 'CURL=0' >/dev/null; then
    printf '%s\n' 'padded WITH_CURL=0 enabled a usable dependency' >&2
    padding_failures=$((padding_failures + 1))
fi

# A padded required-enable switch must fail when the required header is unavailable.
if result=$(run_make show-optional-config 'WITH_CURL= 1 ' \
    'CURL_CFLAGS=-nostdinc' "CURL_LIBS=$tmp/libfixture.a" 2>&1); then
    printf '%s\n' 'padded WITH_CURL=1 accepted an unavailable header' >&2
    padding_failures=$((padding_failures + 1))
else
    assert_contains "$result" 'curl/curl.h'
fi
[ "$padding_failures" = 0 ] || exit 1

# Missing headers must also retain the compiler diagnostic on explicit enablement.
if result=$(run_make show-optional-config WITH_CURL=1 \
    'CURL_CFLAGS=-nostdinc' "CURL_LIBS=$tmp/libfixture.a" 2>&1); then
    printf '%s\n' 'missing CURL header was accepted' >&2
    exit 1
fi
assert_contains "$result" 'curl/curl.h'

# A missing required library must retain the linker diagnostic on explicit enablement.
if result=$(run_make show-optional-config WITH_CURL=1 \
    "CURL_CFLAGS=-nostdinc -I$tmp/include" "CURL_LIBS=$tmp/missing.a" 2>&1); then
    printf '%s\n' 'missing CURL library was accepted' >&2
    exit 1
fi
assert_contains "$result" 'CURL'
assert_contains "$result" 'missing.a'

# Auto must quietly disable a dependency when the same compile/link probe fails.
result=$(run_make show-optional-config WITH_CURL=auto \
    'CURL_CFLAGS=-nostdinc' "CURL_LIBS=$tmp/libfixture.a")
assert_contains "$result" 'CURL=0'
assert_contains "$result" 'LIBS='

# Without pkg-config, a single override cannot implicitly supply the other side.
if result=$(run_make show-optional-config WITH_CURL=1 \
    "CURL_CFLAGS=-nostdinc -I$tmp/include" 2>&1); then
    printf '%s\n' 'incomplete CURL override was accepted' >&2
    exit 1
fi
assert_contains "$result" 'CURL_LIBS'
assert_contains "$result" 'pkg-config'

# Explicitly empty variables count as supplied, even when the dependency has no symbols.
mkdir -p "$tmp/inline/curl"
printf '%s\n' '#define CURLVERSION_NOW 0' \
    'static inline void *curl_version_info(int v) { (void)v; return 0; }' > "$tmp/inline/curl/curl.h"
result=$(run_make show-optional-config WITH_CURL=1 \
    "CURL_CFLAGS=-nostdinc -I$tmp/inline" CURL_LIBS=)
assert_contains "$result" 'CURL=1'

# Controlled pkg-config metadata fills only the override side that was not supplied.
cat > "$tmp/pkg-config" <<'PKG'
#!/bin/sh
[ "$2" = libcurl ] || exit 1
case "$1" in
    --exists) exit 0 ;;
    --cflags) printf '%s\n' "$FIXTURE_CFLAGS" ;;
    --libs) printf '%s\n' "$FIXTURE_LIBS" ;;
    *) exit 1 ;;
esac
PKG
chmod +x "$tmp/pkg-config"
FIXTURE_CFLAGS="-nostdinc -I$tmp/include"
FIXTURE_LIBS="$tmp/libfixture.a"
export FIXTURE_CFLAGS FIXTURE_LIBS
result=$(run_make show-optional-config WITH_CURL=auto PKG_CONFIG="$tmp/pkg-config")
assert_contains "$result" 'CURL=1'
assert_contains "$result" "LIBS=$tmp/libfixture.a"
result=$(run_make show-optional-config WITH_CURL=1 PKG_CONFIG="$tmp/pkg-config" \
    "CURL_CFLAGS=-nostdinc -I$tmp/include -DMANUAL=1")
assert_contains "$result" 'CURL=1'
assert_contains "$result" '-DMANUAL=1'
# Empty manual libraries override pkg-config instead of taking its nonempty archive.
result=$(run_make show-optional-config WITH_CURL=1 PKG_CONFIG="$tmp/pkg-config" \
    "CURL_CFLAGS=-nostdinc -I$tmp/inline" CURL_LIBS=)
assert_contains "$result" 'CURL=1'
assert_contains "$result" 'LIBS='

# Manual flags must be in the build signature even when the feature is disabled.
first=$(run_make show-optional-config CURL_CFLAGS=)
second=$(run_make show-optional-config CURL_CFLAGS=-DCHANGED=1)
[ "$first" != "$second" ] || {
    printf '%s\n' 'manual flag change did not affect the build signature' >&2
    exit 1
}
assert_contains "$second" 'CURL_CFLAGS=-DCHANGED=1'

# Every declared dependency must compile/link its required header and symbol.
mkdir -p "$tmp/include/grpc" "$tmp/include/libssh" "$tmp/include/openssl" "$tmp/include/uuid"
printf '%s\n' 'int ares_library_init(int);' > "$tmp/include/ares.h"
printf '%s\n' 'void grpc_init(void);' > "$tmp/include/grpc/grpc.h"
printf '%s\n' 'const char *idn2_check_version(const char *);' > "$tmp/include/idn2.h"
printf '%s\n' 'const char *ssh_version(int);' > "$tmp/include/libssh/libssh.h"
printf '%s\n' 'const char *lws_get_library_version(void);' > "$tmp/include/libwebsockets.h"
printf '%s\n' 'const char *zlibVersion(void);' > "$tmp/include/zlib.h"
printf '%s\n' 'int OPENSSL_init_ssl(unsigned long, const void *);' > "$tmp/include/openssl/ssl.h"
printf '%s\n' 'typedef unsigned char uuid_t[16];' 'void uuid_generate(uuid_t);' > "$tmp/include/uuid/uuid.h"
printf '%s\n' 'unsigned XXH32(const void *, unsigned long, unsigned) __attribute__((pure));' > "$tmp/include/xxhash.h"
printf '%s\n' 'typedef struct { int value; } blake3_hasher;' \
    'void blake3_hasher_init(blake3_hasher *);' > "$tmp/include/blake3.h"
cat > "$tmp/other.c" <<'C'
int ares_library_init(int flags) { return flags; }
void grpc_init(void) {}
const char *idn2_check_version(const char *version) { return version; }
const char *ssh_version(int value) { (void)value; return 0; }
const char *lws_get_library_version(void) { return 0; }
const char *zlibVersion(void) { return 0; }
int OPENSSL_init_ssl(unsigned long opts, const void *settings) { (void)opts; (void)settings; return 1; }
void uuid_generate(unsigned char *uuid) { uuid[0] = 0; }
unsigned XXH32(const void *data, unsigned long length, unsigned seed) { (void)data; (void)length; return seed; }
void blake3_hasher_init(void *hasher) { (void)hasher; }
C
${CC:-cc} -c "$tmp/other.c" -o "$tmp/other.o"
ar rcs "$tmp/libfixture.a" "$tmp/other.o"
set --
for name in $names; do
    set -- "$@" "WITH_${name}=1" "${name}_CFLAGS=-nostdinc -I$tmp/include" \
        "${name}_LIBS=$tmp/libfixture.a"
done
result=$(run_make show-optional-config "$@")
assert_contains "$result" 'RESOLVED=CARES=1 CURL=1 GRPC=1 IDN2=1 LIBSSH=1 WEBSOCKETS=1 ZLIB=1 OPENSSL=1 UUID=1 XXHASH=1 BLAKE3=1'
run_make chezpp/c/build-config.h "$@" > "$tmp/header.log" 2>&1
for name in $names; do
    grep -F "#define CHEZPP_WITH_${name} 1" "$root/chezpp/c/build-config.h" >/dev/null
done
result=$(run_make -n libchezpp.so "$@")
assert_contains "$result" "-I$tmp/include"
assert_contains "$result" "$tmp/libfixture.a"

# Optimizing away a pure call must not let a missing library pass the link probe.
if result=$(run_make show-optional-config WITH_XXHASH=1 \
    "XXHASH_CFLAGS=-nostdinc -I$tmp/include" XXHASH_LIBS= 2>&1); then
    printf '%s\n' 'pure XXH32 symbol was optimized out of the link probe' >&2
    exit 1
fi
assert_contains "$result" 'XXH32'

# Dependency additions must preserve the existing named compiler profiles.
for variant in release debug coverage; do
    result=$(run_make print-build-options "VARIANT=$variant")
    assert_contains "$result" "variant=$variant"
done

# Quoted manual flags must survive shell transport and signature output unchanged.
result=$(run_make show-optional-config "CURL_CFLAGS=-DNAME='flag value'")
assert_contains "$result" "CURL_CFLAGS=-DNAME='flag value'"

# Generate a complete, parseable header for the resolved all-disabled feature set.
run_make chezpp/c/build-config.h > "$tmp/header.log" 2>&1
for name in $names; do
    grep -F "#define CHEZPP_WITH_${name} 0" "$root/chezpp/c/build-config.h" >/dev/null
done
before=$(stat -c '%y' "$root/chezpp/c/build-config.h")
run_make chezpp/c/build-config.h > "$tmp/header.log" 2>&1
after=$(stat -c '%y' "$root/chezpp/c/build-config.h")
[ "$before" = "$after" ] || {
    printf '%s\n' 'unchanged generated header was rewritten' >&2
    exit 1
}
${CC:-cc} -x c -fsyntax-only -include "$root/chezpp/c/build-config.h" /dev/null

# Disabled dependencies must not appear on the actual native linker command.
# The native implementation tasks provide these fallbacks after configuration is ready.
result=$(run_make -n -o chezpp/c/zlib_unavailable.c -o chezpp/c/uuid_unavailable.c \
    -o chezpp/c/hash_unavailable.c -o chezpp/c/crypto_unavailable.c \
    -o chezpp/c/net/tls_unavailable.c libchezpp.so)
if printf '%s\n' "$result" | grep -E -- '-l(uuid|curl|cares|grpc|idn2|ssh|websockets|z|ssl|crypto|xxhash|blake3)( |$)' >/dev/null; then
    printf '%s\n' 'disabled dependency appeared on native link command' >&2
    exit 1
fi
