#!/bin/sh
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT HUP INT TERM

good_directory="$temporary_directory/good"
bad_directory="$temporary_directory/bad"
mkdir -p "$good_directory" "$bad_directory"

printf '%s\n' 'int fixture_value(void) { return 42; }' \
  >"$temporary_directory/good.c"
printf '%s\n' 'int fixture_other(void) { return 7; }' \
  >"$temporary_directory/bad.c"
cc -shared -fPIC "$temporary_directory/good.c" \
  -o "$good_directory/libchezpp-fixture.so"
cc -shared -fPIC "$temporary_directory/bad.c" \
  -o "$bad_directory/libchezpp-fixture.so"

cc -std=c11 -Wall -Wextra -pthread \
  "$project_root/tests/optional-library-loader.c" \
  "$project_root/chezpp/c/optional_library.c" \
  -ldl -o "$temporary_directory/optional-library-loader"

set +e
"$temporary_directory/optional-library-loader" \
  /missing/libchezpp-fixture.so >"$temporary_directory/missing.out"
status=$?
set -e
test "$status" -eq 2
grep -F 'fixture: unable to load' "$temporary_directory/missing.out" >/dev/null

LD_LIBRARY_PATH="$good_directory" \
  "$temporary_directory/optional-library-loader" libchezpp-fixture.so

set +e
LD_LIBRARY_PATH="$bad_directory" \
  "$temporary_directory/optional-library-loader" libchezpp-fixture.so \
  >"$temporary_directory/symbol.out"
status=$?
set -e
test "$status" -eq 3
grep -F 'fixture: missing symbol fixture_value' \
  "$temporary_directory/symbol.out" >/dev/null
