#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

release=$(make -s -C "$root" print-build-options VARIANT=release 2>&1)
printf '%s\n' "$release" | grep -F 'variant=release'
printf '%s\n' "$release" | grep -F 'o=3'
printf '%s\n' "$release" | grep -F "$(printf '\033[')"

custom=$(make -s -C "$root" print-build-options VARIANT=release o=2 2>&1)
! printf '%s\n' "$custom" | grep -F 'variant=release'
printf '%s\n' "$custom" | grep -F 'o=2'

coverage=$(make -s -C "$root" print-build-options VARIANT=coverage 2>&1)
printf '%s\n' "$coverage" | grep -F 'variant=coverage'
printf '%s\n' "$coverage" | grep -F 'c=t'
printf '%s\n' "$coverage" | grep -F 'GENCOV=1'

alias_release=$(make -s -C "$root" --no-print-directory release -n 2>&1)
printf '%s\n' "$alias_release" | grep -F 'VARIANT=release all'

printf '%s\n' 'build profile option checks passed'
