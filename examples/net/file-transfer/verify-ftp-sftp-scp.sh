#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
state_dir=$(mktemp -d /tmp/chezpp-transfer-verify.XXXXXX)
trap 'rm -rf "$state_dir"' EXIT HUP INT TERM

source_file=$state_dir/source.bin
ftp_file=$state_dir/ftp.bin
sftp_file=$state_dir/sftp.bin
scp_file=$state_dir/scp.bin
transfer_mib=${CHEZPP_TRANSFER_MIB:-16}
dd if=/dev/zero of="$source_file" bs=1048576 count="$transfer_mib" status=none

# These suites start and stop isolated FTP/FTPS and sshd-backed SFTP/SCP fixtures.
(cd "$project_root/tests" && make test-some TEST='net-transfer net-ftp net-sftp net-scp')

actual=$(
  cd "$project_root/tests"
  ../chez++ --script ../examples/net/file-transfer/verify-ftp-sftp-scp.ss \
    "$source_file" "$ftp_file" "$sftp_file" "$scp_file"
)
expected=$(sha256sum "$source_file" | awk '{print $1}')
test "$expected" = "$actual"
printf 'FTP/SFTP/SCP %s MiB round trips and FTPS fixture checks passed; SHA-256 %s\n' \
  "$transfer_mib" "$actual"
