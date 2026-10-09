#!/bin/sh
# Verify the bundled ChezScheme build, launcher, symlink, and cleanup lifecycle.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

make clean-all
make
test -x ./chez++
test -L ./scheme
test -x "$(readlink -f ./scheme)"

make clean
test -x ./scheme

make clean-all
test ! -e .chezscheme-build
test ! -e .chezscheme-install
test ! -e ./scheme
test -e vendor/ChezScheme/configure
