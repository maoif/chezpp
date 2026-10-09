#!/bin/sh
# Check fixed toolchain paths and incomplete nested checkouts in an isolated fixture.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM
cp "$project_root/Makefile" "$project_root/build-options.mk" \
   "$project_root/optional-libraries.mk" "$fixture/"
mkdir -p "$fixture/tools" "$fixture/chezpp/c" "$fixture/tests"
cp "$project_root/tools/probe-optional-libraries.sh" "$fixture/tools/"
cd "$fixture"

# Command-line paths must never replace the bundled compiler or its paired header.
printf '%s\n' 'policy-paths:; @printf "%s\n" "$(CHEZ_SOURCE_DIR)" "$(CHEZ_BUILD_DIR)" "$(CHEZ_INSTALL_DIR)" "$(CHEZ_SCHEME)" "$(SCHEME_SCRIPT)" "$(CHEZ_INCLUDE_DIR)"' > policy.mk
make --no-print-directory -s -f Makefile -f policy.mk policy-paths \
    PKG_CONFIG=/missing CHEZ_SOURCE_DIR=/forbidden CHEZ_BUILD_DIR=/forbidden \
    CHEZ_INSTALL_DIR=/forbidden CHEZ_SCHEME=/forbidden SCHEME_SCRIPT=/forbidden \
    CHEZ_INCLUDE_DIR=/forbidden > paths
if grep -q forbidden paths; then
    printf '%s\n' 'toolchain path override bypassed bundled paths' >&2
    exit 1
fi

# Use real Git submodules, including a nested dependency with no Chez-specific marker files.
export GIT_ALLOW_PROTOCOL=file
export GIT_AUTHOR_NAME=Chezpp-test GIT_COMMITTER_NAME=Chezpp-test
export GIT_AUTHOR_EMAIL=test@example.invalid GIT_COMMITTER_EMAIL=test@example.invalid
git init -q dependency
printf '%s\n' dependency > dependency/source
git -C dependency add source
git -C dependency commit -qm dependency

git init -q upstream
printf '%s\n' '#!/bin/sh' 'touch configure-called; exit 42' > upstream/configure
chmod +x upstream/configure
git -C upstream add .
git -C upstream commit -qm configure
git -C upstream submodule add -q "$fixture/dependency" dependency
git -C upstream commit -qam 'add nested dependency'

git init -q .
git submodule add -q "$fixture/upstream" vendor/ChezScheme
git commit -qm 'add ChezScheme'
mkdir -p .chezscheme-build .chezscheme-install/bin
printf '%s\n' '#!/bin/sh' 'exit 0' > .chezscheme-install/bin/scheme
chmod +x .chezscheme-install/bin/scheme
touch .chezscheme-build/Makefile
commit=$(git -C vendor/ChezScheme rev-parse HEAD)
printf 'commit=%s configure=%s/vendor/ChezScheme/configure --installprefix=%s/.chezscheme-install\n' \
    "$commit" "$fixture" "$fixture" > .chezscheme-build/.chezscheme-signature

# A checked-out parent with an uninitialized nested dependency must be repaired by Git.
git submodule status --recursive | grep -q '^-'
if ! make --no-print-directory bundled-chez PKG_CONFIG=/missing > initialized.log 2>&1; then
    cat initialized.log >&2
    exit 1
fi
test -f vendor/ChezScheme/dependency/source
if git submodule status --recursive | grep -Eq '^[-+U]'; then
    printf '%s\n' 'nested submodule remains uninitialized or mismatched' >&2
    exit 1
fi
test ! -e .chezscheme-build/configure-called

# A fully initialized checkout reuses the completed compiler without configuring again.
make --no-print-directory bundled-chez PKG_CONFIG=/missing > reused.log 2>&1
test ! -e .chezscheme-build/configure-called

# A nested checkout at a different commit must return to the recorded commit.
git -C vendor/ChezScheme/dependency commit -qm 'different checkout' --allow-empty
git submodule status --recursive | grep -q '^+'
make --no-print-directory bundled-chez PKG_CONFIG=/missing > reset.log 2>&1
if git submodule status --recursive | grep -Eq '^[-+U]'; then
    printf '%s\n' 'nested submodule remains at a different commit' >&2
    exit 1
fi

# An absent top-level checkout must initialize its nested dependencies too.
git submodule deinit -q --force vendor/ChezScheme
make --no-print-directory bundled-chez PKG_CONFIG=/missing > checkout.log 2>&1
test -f vendor/ChezScheme/configure
test -f vendor/ChezScheme/dependency/source

# Populated archives must reuse contents without requiring Git metadata.
rm vendor/ChezScheme/.git
mv .git fixture-git
printf 'commit=unversioned configure=%s/vendor/ChezScheme/configure --installprefix=%s/.chezscheme-install\n' \
    "$fixture" "$fixture" > .chezscheme-build/.chezscheme-signature
make --no-print-directory bundled-chez PKG_CONFIG=/missing > archive.log 2>&1
test ! -e .chezscheme-build/configure-called

# A missing generated Makefile must invalidate otherwise complete compiler artifacts.
rm .chezscheme-build/Makefile
touch libchezpp.so chezpp.lib chezpp/stale.so chezpp/stale.wpo
if make --no-print-directory bundled-chez PKG_CONFIG=/missing \
    > invalid.stdout 2> invalid.stderr; then
    printf '%s\n' 'missing ChezScheme Makefile did not trigger reconfiguration' >&2
    exit 1
fi
test -e .chezscheme-build/configure-called
test ! -e libchezpp.so
test ! -e chezpp.lib
test ! -e chezpp/stale.so
test ! -e chezpp/stale.wpo
