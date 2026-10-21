"""Smoke CLI help for every explicitly inventoried network command without network access."""

import sys, os
sys.path = [path for path in sys.path if path != os.path.dirname(__file__)]
import pathlib
import re
import subprocess

root = pathlib.Path(__file__).resolve().parent.parent
inventory = (
    (root / "tests/net-examples.ss")
    .read_text()
    .split("(define entry-point-load-path*")[0]
)
entries = re.findall(r'"(examples/net/[^\"]+\.ss)"', inventory)
assert entries
for entry in entries:
    for flag in ("-h", "--help"):
        result = subprocess.run(
            [str(root / "cspp"), f"--script={entry}", "--", flag],
            cwd=root,
            capture_output=True,
            timeout=10,
        )
        assert result.returncode == 0, (entry, flag, result.stderr)
        assert result.stdout and not result.stderr, (
            entry,
            flag,
            result.stdout,
            result.stderr,
        )
        # Help must come from the example, rather than the outer launcher.
        assert b"CSPP REPL and script launcher" not in result.stdout, (entry, flag, result.stdout)
