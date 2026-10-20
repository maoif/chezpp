#!/bin/sh
# Verify the umbrella source carries the version and compilation preserves it.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

make --no-print-directory -W cspp.ss cspp.lib >/dev/null
test -s cspp.lib
test "$(./.chezscheme-install/bin/scheme --version 2>&1)" = '10.4.1'

./.chezscheme-install/bin/scheme --script /dev/stdin <<'SCHEME'
(import (chezscheme))

(define expected-version '(0 0 0 10 4 1))
(call-with-input-file "cspp.ss"
  (lambda (port)
    (let ([declaration (read port)])
      (unless (and (eq? (car declaration) 'library)
                   (equal? (cadr declaration) (list 'cspp expected-version))
                   (eof-object? (read port)))
        (error 'umbrella-version "unexpected umbrella declaration"))
      (for-each
       (lambda (library)
         (unless (and (list? library) (for-all symbol? library))
           (error 'umbrella-version "constituent import has a version" library)))
       (append (cdr (list-ref declaration 3))
               (cdr (cadr (list-ref declaration 4))))))))

(putenv "LIBCSPP" (string-append (current-directory) "/libcspp.so"))
(load "cspp.lib")
(environment '(cspp))
(environment '(cspp (0 0 0 10 4 1)))
(unless (equal? (library-version '(cspp)) expected-version)
  (error 'umbrella-version "compiled umbrella has an unexpected version"))
SCHEME
