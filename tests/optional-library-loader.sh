#!/bin/sh
# Preserve the old test target while validating the build-time dependency resolver.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
exec "$project_root/tests/optional-build-config.sh"
