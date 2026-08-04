#!/bin/sh
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT HUP INT TERM

build_fixture() {
  name=$1
  version=$2
  mode=$3
  omitted=${4-}
  case "$name" in
    curl)
      cat >"$temporary_directory/fixture.c" <<EOF
#include <curl/curl.h>
static curl_version_info_data info = {
  CURLVERSION_NOW, "${version}", ${version%%.*} == 8 ? 0x080000 : 0
};
curl_version_info_data *curl_version_info(CURLversion age) { (void)age; return &info; }
EOF
      soname=libcurl.so.4
      ;;
    ssh)
      cat >"$temporary_directory/fixture.c" <<EOF
const char *ssh_version(int required) {
  (void)required;
  return "${version}";
}
EOF
      soname=libssh.so.4
      ;;
    websockets)
      cat >"$temporary_directory/fixture.c" <<EOF
const char *lws_get_library_version(void) { return "${version}"; }
EOF
      soname=libwebsockets.so.21
      ;;
    grpc)
      cat >"$temporary_directory/fixture.c" <<EOF
const char *grpc_version_string(void) { return "${version}"; }
EOF
      soname=libgrpc.so.54
      ;;
    *) return 1 ;;
  esac

  if test "$mode" != old; then
    case "$name" in
      curl) prefixes=curl; version_symbol=curl_version_info; source=ftp.c ;;
      ssh) prefixes='ssh|sftp'; version_symbol=ssh_version; source=ssh.c ;;
      websockets) prefixes=lws; version_symbol=lws_get_library_version; source=websocket.c ;;
      grpc) prefixes='grpc|gpr'; version_symbol=grpc_version_string; source=grpc.c ;;
    esac
    : >"$temporary_directory/stubs.c"
    rg -o '"('"$prefixes"')_[A-Za-z0-9_]+"' \
      "$project_root/chezpp/c/net/$source" | tr -d '"' | sort -u | \
      while IFS= read -r symbol; do
        if test "$symbol" != "$version_symbol" && test "$symbol" != "$omitted"; then
          printf 'long %s(void) { return 0; }\n' "$symbol"
        fi
      done >"$temporary_directory/stubs.c"
  else
    : >"$temporary_directory/stubs.c"
  fi

  cc -shared -fPIC -Wl,-soname,"$soname" \
    "$temporary_directory/fixture.c" "$temporary_directory/stubs.c" \
    -o "$temporary_directory/$soname"
  if test "$name" = grpc && test "$mode" != old; then
    cc -shared -fPIC -Wl,-soname,libgpr.so.54 "$temporary_directory/stubs.c" \
      -o "$temporary_directory/libgpr.so.54"
  fi
}

check_unavailable() {
  name=$1
  expected=$2
  output=$3
  printf '%s\n' \
    '(import (chezpp))' \
    "(let ([info (optional-library-info '$name)])" \
    '  (display (and (not (optional-library-available? info))' \
    "               (string-contains? (optional-library-error info) \"$expected\"))))" | \
    env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
    >"$output" 2>"$output.err"
  test "$(cat "$output")" = '#t' || {
    printf 'fixture %s returned: ' "$name" >&2
    printf '%s\n' \
      '(import (chezpp))' \
      "(let ([info (optional-library-info '$name)])" \
      '  (display (list (optional-library-available? info)' \
      '                 (optional-library-version info)' \
      '                 (optional-library-capabilities info)' \
      '                 (optional-library-error info))))' | \
      env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q >&2
    exit 1
  }
  test ! -s "$output.err"
}

check_available() {
  name=$1
  expected_version=$2
  expected_capabilities=$3
  output=$4
  printf '%s\n' \
    '(import (chezpp))' \
    "(let ([info (optional-library-info '$name)])" \
    '  (display (and (optional-library-available? info)' \
    "               (string=? (optional-library-version info) \"$expected_version\")" \
    "               (equal? (optional-library-capabilities info) '$expected_capabilities)" \
    '               (not (optional-library-error info)))))' | \
    env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q \
    >"$output" 2>"$output.err"
  test "$(cat "$output")" = '#t' || {
    printf 'fixture %s returned: ' "$name" >&2
    printf '%s\n' \
      '(import (chezpp))' \
      "(let ([info (optional-library-info '$name)])" \
      '  (display (list (optional-library-available? info)' \
      '                 (optional-library-version info)' \
      '                 (optional-library-capabilities info)' \
      '                 (optional-library-error info))))' | \
      env LD_LIBRARY_PATH="$temporary_directory" "$project_root/chez++" -q >&2
    exit 1
  }
  test ! -s "$output.err"
}

for name in curl ssh websockets grpc; do
  build_fixture "$name" "0.0.0" old
  case "$name" in
    curl) soname=libcurl.so.4; missing=curl_global_init ;;
    ssh) soname=libssh.so.4; missing=ssh_new ;;
    websockets) soname=libwebsockets.so.21; missing=lws_create_context ;;
    grpc) soname=libgrpc.so.54; missing=grpc_init ;;
  esac
  check_unavailable "$name" "requires" "$temporary_directory/$name-old.out"
  case "$name" in
    curl) compatible_version=8.0.0 ;;
    ssh) compatible_version=0.12.0 ;;
    websockets) compatible_version=4.5.8 ;;
    grpc) compatible_version=54.0.0 ;;
  esac
  build_fixture "$name" "$compatible_version" missing "$missing"
  check_unavailable "$name" "missing symbol $missing" "$temporary_directory/$name-missing.out"
  if test "$name" = grpc; then
    build_fixture grpc "$compatible_version" missing gpr_free
    check_unavailable grpc "gpr: missing symbol gpr_free" \
      "$temporary_directory/grpc-gpr-missing.out"
  fi
  build_fixture "$name" "$compatible_version" compatible
  case "$name" in
    curl) expected_capabilities='()' ;;
    ssh) expected_capabilities='(sftp-aio)' ;;
    websockets|grpc) expected_capabilities='(compression tls)' ;;
  esac
  check_available "$name" "$compatible_version" "$expected_capabilities" \
    "$temporary_directory/$name-compatible.out"
done
