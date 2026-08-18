#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
keep_downloads=${KEEP_DOWNLOADS:-0}
if [ "${1:-}" != "" ]; then
  state_dir=$1
  mkdir -p "$state_dir"
  owned_state=0
else
  state_dir=$(mktemp -d "${TMPDIR:-/tmp}/chezpp-external-download.XXXXXX")
  owned_state=1
fi
cleanup() {
  if [ "$owned_state" -eq 1 ] && [ "$keep_downloads" != 1 ]; then
    rm -rf "$state_dir"
  fi
}
trap cleanup EXIT HUP INT TERM

if command -v curl >/dev/null 2>&1; then
  # curl's continuation mode makes an interrupted transfer resumable without buffering it.
  for spec in \
    'https://ftp.gnu.org/gnu/emacs/windows/emacs-30/emacs-30.2.zip 414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72' \
    'https://mirrors.tuna.tsinghua.edu.cn/archlinux/iso/2026.07.01/archlinux-2026.07.01-x86_64.iso e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0'; do
    uri=${spec% *}
    expected=${spec##* }
    name=${uri##*/}
    if [ "$name" = archlinux-2026.07.01-x86_64.iso ] && \
        [ -n "${CHEZPP_ARCH_MIRROR:-}" ]; then
      uri=$CHEZPP_ARCH_MIRROR
    fi
    destination=$state_dir/$name
    started=$(date +%s)
    actual=$(sha256sum "$destination" 2>/dev/null | awk '{print $1}' || true)
    if [ "$actual" != "$expected" ]; then
      while [ ! -f "$destination" ] || [ "$(wc -c <"$destination")" -lt 8388608 ]; do
        timeout 5 curl --fail --location --retry 2 --limit-rate 2M --continue-at - \
          --output "$destination" "$uri" || test -s "$destination"
      done
      if command -v aria2c >/dev/null 2>&1; then
        aria2c --continue=true --max-connection-per-server=8 --split=8 --min-split-size=1M \
          --console-log-level=warn --summary-interval=0 --dir="$state_dir" --out="$name" "$uri"
      else
        curl --fail --location --retry 2 --continue-at - --output "$destination" "$uri"
      fi
      actual=$(sha256sum "$destination" | awk '{print $1}')
    fi
    test "$actual" = "$expected"
    printf '%s %s %s bytes %ss\n' "$uri" "$destination" "$(wc -c <"$destination")" \
      "$(( $(date +%s) - started ))"
  done
else
  (cd "$project_root" && ./chez++ --script \
    examples/net/file-transfer/verify-external-downloads.ss "$state_dir")
fi
