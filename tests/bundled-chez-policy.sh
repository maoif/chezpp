#!/bin/sh
# Check fixed toolchain paths and incomplete nested checkouts in an isolated fixture.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM
cp "$project_root/Makefile" "$project_root/build-options.mk" \
   "$project_root/optional-libraries.mk" "$fixture/"
mkdir -p "$fixture/tools" "$fixture/chezpp/c" "$fixture/tests" "$fixture/bin"
cp "$project_root/tools/probe-optional-libraries.sh" "$fixture/tools/"
cd "$fixture"

# Command-line paths must never replace the bundled compiler or its paired header.
printf '%s\n' 'policy-paths:; @printf "%s\n" "$(CHEZ_SOURCE_DIR)" "$(CHEZ_BUILD_DIR)" "$(CHEZ_INSTALL_DIR)" "$(CHEZ_SCHEME)" "$(SCHEME_SCRIPT)" "$(CHEZ_HEADER)" "$(CHEZ_INCLUDE_DIR)"' > policy.mk
make --no-print-directory -s -f Makefile -f policy.mk policy-paths \
    PKG_CONFIG=/missing CHEZ_SOURCE_DIR=/forbidden CHEZ_BUILD_DIR=/forbidden \
    CHEZ_INSTALL_DIR=/forbidden CHEZ_SCHEME=/forbidden SCHEME_SCRIPT=/forbidden \
    CHEZ_HEADER=/forbidden CHEZ_INCLUDE_DIR=/forbidden > paths
if grep -q forbidden paths; then
    printf '%s\n' 'toolchain path override bypassed bundled paths' >&2
    exit 1
fi

mkdir -p vendor/ChezScheme .chezscheme-build/native/bin/native \
    .chezscheme-install/bin .chezscheme-install/lib/csv10.4.1/native
printf '%s\n' '#!/bin/sh' 'touch configure-called; exit 42' > vendor/ChezScheme/configure
chmod +x vendor/ChezScheme/configure
touch vendor/ChezScheme/.git
touch .chezscheme-build/Makefile .chezscheme-build/native/bin/native/scheme \
      .chezscheme-install/bin/scheme .chezscheme-install/lib/csv10.4.1/native/scheme.h
chmod +x .chezscheme-build/native/bin/native/scheme .chezscheme-install/bin/scheme
printf 'commit=unversioned configure=%s/vendor/ChezScheme/configure --installprefix=%s/.chezscheme-install\n' \
    "$fixture" "$fixture" > .chezscheme-build/.chezscheme-signature

cat > bin/git <<'SH'
#!/bin/sh
case "$*" in
    *'rev-parse HEAD'*) printf '%s\n' unversioned ;;
    *'submodule update --init --depth 1 --filter=blob:none --recursive')
        printf '%s\n' "$*" >> git-updates
        mkdir -p vendor/ChezScheme/zuo vendor/ChezScheme/nanopass \
                 vendor/ChezScheme/stex vendor/ChezScheme/zlib vendor/ChezScheme/lz4/lib
        touch vendor/ChezScheme/zuo/configure vendor/ChezScheme/nanopass/nanopass.ss \
              vendor/ChezScheme/stex/Mf-stex vendor/ChezScheme/zlib/configure \
              vendor/ChezScheme/lz4/lib/Makefile ;;
    *) exit 1 ;;
esac
SH
chmod +x bin/git
PATH="$fixture/bin:$PATH" make --no-print-directory bundled-chez PKG_CONFIG=/missing
test -s git-updates
test ! -e .chezscheme-build/configure-called

# Populated archives must reuse contents without trying to initialize submodules.
rm git-updates
rm vendor/ChezScheme/.git
PATH="$fixture/bin:$PATH" make --no-print-directory bundled-chez PKG_CONFIG=/missing
test ! -e git-updates

# A missing generated Makefile must invalidate otherwise complete compiler artifacts.
rm .chezscheme-build/Makefile
touch libchezpp.so chezpp.lib chezpp/stale.so chezpp/stale.wpo
if PATH="$fixture/bin:$PATH" make --no-print-directory bundled-chez PKG_CONFIG=/missing \
    > invalid.stdout 2> invalid.stderr; then
    printf '%s\n' 'missing ChezScheme Makefile did not trigger reconfiguration' >&2
    exit 1
fi
test -e .chezscheme-build/configure-called
test ! -e libchezpp.so
test ! -e chezpp.lib
test ! -e chezpp/stale.so
test ! -e chezpp/stale.wpo
