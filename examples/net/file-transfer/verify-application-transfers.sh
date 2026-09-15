#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
state_dir=$(mktemp -d /tmp/chezpp-application-transfer.XXXXXX)
server_pids=""
run_count=0
cleanup() {
  for pid in $server_pids; do
    kill "$pid" 2>/dev/null || true
  done
  if [ "${CHEZPP_TRANSFER_KEEP_STATE:-0}" != 1 ]; then
    rm -rf "$state_dir"
  else
    printf 'transfer verification state preserved at %s\n' "$state_dir" >&2
  fi
}
trap cleanup EXIT HUP INT TERM

source_file=$state_dir/source.bin
transfer_mib=${CHEZPP_TRANSFER_MIB:-64}
dd if=/dev/zero of="$source_file" bs=1048576 count="$transfer_mib" status=none
expected=$(sha256sum "$source_file" | awk '{print $1}')

cert=$state_dir/cert.pem
key=$state_dir/key.pem
openssl req -x509 -newkey rsa:2048 -nodes -days 1 \
  -subj '/CN=localhost' \
  -addext 'subjectAltName=DNS:localhost,IP:127.0.0.1' \
  -keyout "$key" -out "$cert" >/dev/null 2>&1

wait_port() {
  port=$1
  attempt=0
  while ! ss -ltn "sport = :$port" | tail -n +2 | grep -q LISTEN; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 100 ]; then
      printf 'timed out waiting for transfer server on port %s\n' "$port" >&2
      return 1
    fi
    sleep 0.1
  done
  # Let the listener finish its protocol setup before the first client handshake.
  sleep 0.5
}

run_variant() {
  name=$1
  server_script=$2
  client_script=$3
  port=$4
  secure=${5:-0}
  if [ -n "${CHEZPP_TRANSFER_ONLY:-}" ] && [ "$CHEZPP_TRANSFER_ONLY" != "$name" ]; then
    return 0
  fi
  run_count=$((run_count + 1))
  destination=$state_dir/$name
  download=$state_dir/$name.download
  mkdir -p "$destination"
  export CHEZPP_TRANSFER_DOWNLOAD=$download
  export CHEZPP_TRANSFER_REQUESTS=2
  export CHEZPP_TRANSFER_RSS_FILE=$state_dir/$name.client.rss
  if [ "$secure" -eq 1 ]; then
    export CHEZPP_TRANSFER_CERT=$cert
    export CHEZPP_TRANSFER_KEY=$key
    export CHEZPP_TRANSFER_ROLE=server
  fi
  (cd "$project_root" && ./chez++ --script "$server_script" "$destination") \
    >"$state_dir/$name.server.out" 2>"$state_dir/$name.server.err" &
  server_pid=$!
  server_pids="$server_pids $server_pid"
  wait_port "$port"
  if [ "$secure" -eq 1 ]; then
    export CHEZPP_TRANSFER_ROLE=client
  fi
  if ! (cd "$project_root" && ./chez++ --script "$client_script" "$source_file") \
      >"$state_dir/$name.client.out" 2>"$state_dir/$name.client.err"; then
    cat "$state_dir/$name.client.out"
    cat "$state_dir/$name.client.err" >&2
    cat "$state_dir/$name.server.out"
    cat "$state_dir/$name.server.err" >&2
    kill "$server_pid" 2>/dev/null || true
    return 1
  fi
  if ! wait "$server_pid"; then
    cat "$state_dir/$name.server.out"
    cat "$state_dir/$name.server.err" >&2
    return 1
  fi
  # Allow libwebsockets and the kernel to finish releasing the listener before the next variant.
  sleep 0.5
  server_pids=$(printf '%s' "$server_pids" | sed "s/ $server_pid//")
  actual_file=$destination/$(basename "$source_file")
  test -f "$actual_file"
  actual=$(sha256sum "$actual_file" | awk '{print $1}')
  test "$actual" = "$expected"
  test -f "$download"
  downloaded=$(sha256sum "$download" | awk '{print $1}')
  test "$downloaded" = "$expected"
  test ! -s "$state_dir/$name.server.out"
  test ! -s "$state_dir/$name.server.err"
  test ! -s "$state_dir/$name.client.out"
  test ! -s "$state_dir/$name.client.err"
  client_rss=$(cat "$state_dir/$name.client.rss")
  rss_growth=$client_rss
  if [ "$rss_growth" -gt 16384 ]; then
    printf '%s RSS grew by %s KiB, limit is 16384 KiB\n' "$name" "$rss_growth" >&2
    return 1
  fi
  printf '%s upload=%s download=%s rss-growth=%s-KiB\n' \
    "$name" "$actual" "$downloaded" "$rss_growth"
}

run_variant grpc-tls \
  examples/net/file-transfer/file-transfer-grpc-tls.ss \
  examples/net/file-transfer/file-transfer-grpc-tls.ss 41007 1
run_variant http \
  examples/net/file-transfer/file-transfer-http-server.ss \
  examples/net/file-transfer/file-transfer-http-client.ss 41002
run_variant websocket \
  examples/net/file-transfer/file-transfer-websocket-server.ss \
  examples/net/file-transfer/file-transfer-websocket-client.ss 41005
run_variant grpc \
  examples/net/file-transfer/file-transfer-grpc-server.ss \
  examples/net/file-transfer/file-transfer-grpc-client.ss 41007
run_variant https \
  examples/net/file-transfer/file-transfer-https.ss \
  examples/net/file-transfer/file-transfer-https.ss 41008 1
run_variant wss \
  examples/net/file-transfer/file-transfer-wss.ss \
  examples/net/file-transfer/file-transfer-wss.ss 41009 1

printf 'application transfer verification passed for %s variant(s); SHA-256 %s\n' \
  "$run_count" "$expected"
