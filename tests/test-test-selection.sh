#!/bin/sh
# Verify positional test-file selection accepts tests and rejects support files.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
output=$(mktemp)
error=$(mktemp)
trap 'rm -f "$output" "$error"' EXIT HUP INT TERM

for directory in "$project_root/tests" "$project_root"; do
    # Positional files select exactly these tests, preserving the requested order.
    if ! make --no-print-directory -C "$directory" test vector.ss list.ss \
        >"$output" 2>"$error"; then
        cat "$output" >&2
        cat "$error" >&2
        exit 1
    fi
    actual=$(sed -n '/^== running .* ==$/p' "$output")
    expected=$(printf '%s\n' '== running vector.so ==' '== running list.so ==')
    if [ "$actual" != "$expected" ]; then
        printf '%s\n' 'selected test invocation ran an unexpected test set' "$actual" >&2
        exit 1
    fi

    # Helper sources, fixtures, missing files, names without .ss, and .so are invalid.
    for invalid in net-common.ss mat-requires-fixture.ss coverage-init.ss \
        missing.ss vector vector.so; do
        if make --no-print-directory -C "$directory" test "$invalid" \
            >"$output" 2>"$error"; then
            printf '%s\n' "invalid test argument was accepted: $invalid" >&2
            exit 1
        fi
        grep -Fq "unsupported test file(s): $invalid" "$error"
        # Argument rejection must happen before any build or test recipes run.
        test ! -s "$output"
    done
done
