#!/bin/sh
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT HUP INT TERM

cat >"$temporary_directory/crypto.c" <<'EOF'
unsigned long OpenSSL_version_num(void) { return 0x20000000UL; }
const char *OpenSSL_version(int kind) {
  (void)kind;
  return "OpenSSL 2.0 fixture";
}
EOF
cc -shared -fPIC -Wl,-soname,libcrypto.so.3 \
  "$temporary_directory/crypto.c" -o "$temporary_directory/libcrypto.so.3"

printf '%s\n' '(import (chezpp)) (display "ok")' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/import.out" 2>"$temporary_directory/import.err"
test "$(cat "$temporary_directory/import.out")" = ok
test ! -s "$temporary_directory/import.err"

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)]) (random-bytes 8))' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/use.out" 2>"$temporary_directory/use.err"
grep -F 'OpenSSL runtime ABI major 2; requires major 3' \
  "$temporary_directory/use.out" >/dev/null
test ! -s "$temporary_directory/use.err"

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)]) (scrypt "password" "salt" 2 1 1 8))' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/scrypt.out" 2>"$temporary_directory/scrypt.err"
grep -F 'OpenSSL runtime ABI major 2; requires major 3' \
  "$temporary_directory/scrypt.out" >/dev/null
test ! -s "$temporary_directory/scrypt.err"

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)]) (make-tls-context '\''client))' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/tls.out" 2>"$temporary_directory/tls.err"
grep -F 'OpenSSL runtime ABI major 2; requires major 3' \
  "$temporary_directory/tls.out" >/dev/null
test ! -s "$temporary_directory/tls.err"

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)])' \
  '  (constant-time-bytevector=? (bytevector 1) (bytevector 1)))' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/constant-time.out" 2>"$temporary_directory/constant-time.err"
grep -F 'OpenSSL runtime ABI major 2; requires major 3' \
  "$temporary_directory/constant-time.out" >/dev/null
test ! -s "$temporary_directory/constant-time.err"

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)]) (sha256-bytevector (bytevector 1)))' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/digest.out" 2>"$temporary_directory/digest.err"
grep -F 'OpenSSL runtime ABI major 2; requires major 3' \
  "$temporary_directory/digest.out" >/dev/null
test ! -s "$temporary_directory/digest.err"

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)])' \
  '  (aead-decrypt '\''aes-128-gcm (make-bytevector 16) (make-bytevector 12)' \
  '                (make-bytevector 1) (make-bytevector 16)))' | \
  env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
  >"$temporary_directory/aead.out" 2>"$temporary_directory/aead.err"
grep -F 'OpenSSL runtime ABI major 2; requires major 3' \
  "$temporary_directory/aead.out" >/dev/null
test ! -s "$temporary_directory/aead.err"
