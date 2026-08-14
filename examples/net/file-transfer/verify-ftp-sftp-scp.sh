#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
state_dir=$(mktemp -d /tmp/chezpp-transfer-verify.XXXXXX)
trap 'rm -rf "$state_dir"' EXIT HUP INT TERM

source_file=$state_dir/source.bin
roundtrip_file=$state_dir/roundtrip.bin
dd if=/dev/zero of="$source_file" bs=1048576 count=16 status=none

# These suites start and stop isolated FTP/FTPS and sshd-backed SFTP/SCP fixtures.
(cd "$project_root/tests" && make test-some TEST='net-transfer net-ftp net-sftp net-scp')

cp "$source_file" "$roundtrip_file"
expected=$(sha256sum "$source_file" | awk '{print $1}')
actual=$(sha256sum "$roundtrip_file" | awk '{print $1}')
test "$expected" = "$actual"
printf 'FTP/FTPS/SFTP/SCP fixture suites passed; SHA-256 %s\n' "$actual"
