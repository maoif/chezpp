#!/usr/bin/env python3
"""Verify descriptor adaptation preserves an enabled helper's initialization diagnostic."""
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
DEPENDENCIES = 'CARES CURL GRPC IDN2 LIBSSH WEBSOCKETS ZLIB OPENSSL UUID XXHASH BLAKE3'.split()


def main():
    with tempfile.TemporaryDirectory(prefix='chezpp-native-metadata-') as scratch:
        directory = Path(scratch)
        for filename in ['common.h', 'optional_library.h', 'optional_library_info.c']:
            shutil.copy2(ROOT / 'chezpp/c' / filename, directory / filename)
        (directory / 'build-config.h').write_text('\n'.join(
            f'#define CHEZPP_WITH_{name} {int(name == "OPENSSL")}' for name in DEPENDENCIES) + '\n')
        (directory / 'failed-helper.c').write_text('''#include "optional_library.h"
static chezpp_optional_library failed = CHEZPP_OPTIONAL_LIBRARY_INIT("openssl");
const chezpp_optional_library *chezpp_openssl_library(void) {
  failed.state = -1;
  failed.error[0] = '\\0';
  /* A real native helper can fail initialization after its library has linked. */
  __builtin_strcpy(failed.error, "OpenSSL runtime initialization failed");
  return &failed;
}
''')
        scheme_directory = Path(shutil.which('scheme')).resolve().parent
        artifact = directory / 'metadata.so'
        compiled = subprocess.run(['cc', '-shared', '-fPIC', '-Wall', '-Wextra', '-Werror',
                                   f'-I{scheme_directory}', '-pthread',
                                   str(directory / 'optional_library_info.c'),
                                   str(directory / 'failed-helper.c'), '-o', str(artifact)],
                                  capture_output=True, text=True)
        assert compiled.returncode == 0 and not compiled.stdout and not compiled.stderr, compiled.stdout + compiled.stderr
        script = f'''(import (chezscheme))
(load-shared-object "{artifact}")
(define info (foreign-procedure "chezpp_optional_library_info" (string) scheme-object))
;; Initialization failure must keep its real reason, rather than become a version error.
(let ([result (info "openssl")])
  (unless (and (not (vector-ref result 1))
               (not (vector-ref result 2))
               (null? (vector-ref result 3))
               (string=? (vector-ref result 4) "OpenSSL runtime initialization failed"))
    (error 'test-native-metadata-error "lost enabled initialization diagnostic" result)))
'''
        result = subprocess.run(['scheme', '--script', '/dev/stdin'], input=script,
                                cwd=ROOT, text=True, capture_output=True)
        assert result.returncode == 0 and not result.stdout and not result.stderr, result.stdout + result.stderr


if __name__ == '__main__':
    main()
