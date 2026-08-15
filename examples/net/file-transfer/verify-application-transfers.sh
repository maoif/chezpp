#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
state_dir=$(mktemp -d /tmp/chezpp-application-transfer.XXXXXX)
server_pids=""
cleanup() {
  for pid in $server_pids; do
    kill "$pid" 2>/dev/null || true
  done
  rm -rf "$state_dir"
}
trap cleanup EXIT HUP INT TERM

source_file=$state_dir/source.bin
dd if=/dev/urandom of="$source_file" bs=1048576 count=64 status=none
expected=$(sha256sum "$source_file" | awk '{print $1}')

cert=$state_dir/cert.pem
key=$state_dir/key.pem
openssl req -x509 -newkey rsa:2048 -nodes -days 1 \
  -subj '/CN=localhost' \
  -addext 'subjectAltName=DNS:localhost,IP:127.0.0.1' \
  -keyout "$key" -out "$cert" >/dev/null 2>&1

wait_port() {
  port=$1
  sleep 1
}

run_variant() {
  name=$1
  server_script=$2
  client_script=$3
  port=$4
  secure=${5:-0}
  destination=$state_dir/$name
  mkdir -p "$destination"
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
  (cd "$project_root" && ./chez++ --script "$client_script" "$source_file")
  wait "$server_pid" || true
  server_pids=$(printf '%s' "$server_pids" | sed "s/ $server_pid//")
  actual_file=$destination/$(basename "$source_file")
  test -f "$actual_file"
  actual=$(sha256sum "$actual_file" | awk '{print $1}')
  test "$actual" = "$expected"
  printf '%s %s\n' "$name" "$actual"
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

printf 'application transfer verification passed for six variants; SHA-256 %s\n' "$expected"
