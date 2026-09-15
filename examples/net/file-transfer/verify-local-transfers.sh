#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
state_dir=$(mktemp -d /tmp/chezpp-local-transfer.XXXXXX)
trap 'rm -rf "$state_dir"' EXIT HUP INT TERM

transfer_mib=${CHEZPP_TRANSFER_MIB:-16}

# The application driver covers HTTP, HTTPS, WebSocket, WSS, gRPC, and TLS gRPC.
CHEZPP_TRANSFER_MIB="$transfer_mib" \
  "$project_root/examples/net/file-transfer/verify-application-transfers.sh"

# This driver covers FTP, FTPS, SFTP, and SCP, including interactive transcripts.
CHEZPP_TRANSFER_MIB="$transfer_mib" \
  "$project_root/examples/net/file-transfer/verify-ftp-sftp-scp.sh"

printf 'local transfer verification passed for ten protocol variants (%s MiB)\n' "$transfer_mib"
