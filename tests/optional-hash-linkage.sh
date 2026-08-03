#!/bin/sh
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT HUP INT TERM

if readelf -d "$project_root/libchezpp.so" | grep -Eq 'lib(xxhash|blake3)'; then
  printf '%s\n' 'libchezpp.so still links directly to xxhash or blake3' >&2
  exit 1
fi

build_stub() {
  library=$1
  source=$2
  cc -shared -fPIC -Wl,-soname,"$library" "$source" -o "$temporary_directory/$library"
}

run_fixture() {
  dependency=$1
  diagnostic=$2
  LD_LIBRARY_PATH=$temporary_directory \
    "$project_root/chez++" --script "$project_root/tests/optional-hash-libs.ss" \
    "$dependency" "$diagnostic"
}

LD_LIBRARY_PATH= \
  "$project_root/chez++" --script "$project_root/tests/optional-hash-libs.ss" \
  xxhash success
LD_LIBRARY_PATH= \
  "$project_root/chez++" --script "$project_root/tests/optional-hash-libs.ss" \
  blake3 success

printf '%s\n' \
  '#define _GNU_SOURCE' \
  '#include <dlfcn.h>' \
  '#include <string.h>' \
  'void *dlopen(const char *name, int flags) {' \
  '  typedef void *(*dlopen_fn)(const char *, int);' \
  '  static dlopen_fn next_dlopen;' \
  '  if (name != 0 && (strstr(name, "libxxhash") != 0 ||' \
  '                    strstr(name, "libblake3") != 0)) return 0;' \
  '  if (next_dlopen == 0) next_dlopen = (dlopen_fn)dlsym(RTLD_NEXT, "dlopen");' \
  '  return next_dlopen(name, flags);' \
  '}' >"$temporary_directory/block-optional-hash.c"
cc -shared -fPIC "$temporary_directory/block-optional-hash.c" \
  -o "$temporary_directory/block-optional-hash.so" -ldl
LD_PRELOAD=$temporary_directory/block-optional-hash.so LD_LIBRARY_PATH= \
  "$project_root/chez++" --script "$project_root/tests/optional-hash-libs.ss" \
  xxhash 'unable to load'
LD_PRELOAD=$temporary_directory/block-optional-hash.so LD_LIBRARY_PATH= \
  "$project_root/chez++" --script "$project_root/tests/optional-hash-libs.ss" \
  blake3 'unable to load'

printf '%s\n' 'unsigned XXH_versionNumber(void) { return 900; }' \
  >"$temporary_directory/xxhash.c"
build_stub libxxhash.so.0 "$temporary_directory/xxhash.c"
run_fixture xxhash 'requires version 0.8.x'

printf '%s\n' 'unsigned XXH_versionNumber(void) { return 899; }' \
  >"$temporary_directory/xxhash.c"
build_stub libxxhash.so.0 "$temporary_directory/xxhash.c"
run_fixture xxhash 'missing symbol XXH32'

printf '%s\n' 'const char *blake3_version(void) { return "1.9.0"; }' \
  >"$temporary_directory/blake3.c"
build_stub libblake3.so.0 "$temporary_directory/blake3.c"
run_fixture blake3 'requires version 1.8.x'

printf '%s\n' 'const char *blake3_version(void) { return "1.8.99"; }' \
  >"$temporary_directory/blake3.c"
build_stub libblake3.so.0 "$temporary_directory/blake3.c"
run_fixture blake3 'missing symbol blake3_hasher_init'
