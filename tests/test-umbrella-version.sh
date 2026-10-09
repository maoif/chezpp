#!/bin/sh
# Verify the umbrella library version and compile it with bundled ChezScheme.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

make --no-print-directory -W chezpp.ss chezpp.lib >/dev/null
test -s chezpp.lib
test "$(./.chezscheme-install/bin/scheme --version 2>&1)" = '10.4.1'

./.chezscheme-install/bin/scheme --script /dev/stdin <<'SCHEME'
(import (chezscheme))

(define expected-version '(0 0 0 10 4 1))
(call-with-input-file "chezpp.ss"
  (lambda (port)
    (let ([declaration (read port)])
      (unless (and (eq? (car declaration) 'library)
                   (equal? (cadr declaration) (list 'chezpp expected-version))
                   (eof-object? (read port)))
        (error 'umbrella-version "unexpected umbrella declaration"))
      (for-each
       (lambda (library)
         (unless (and (list? library) (for-all symbol? library))
           (error 'umbrella-version "constituent import has a version" library)))
       (append (cdr (list-ref declaration 3))
               (cdr (cadr (list-ref declaration 4))))))))

(putenv "LIBCHEZPP" (string-append (current-directory) "/libchezpp.so"))
(load "chezpp.lib")
(environment '(chezpp))
(environment '(chezpp (0 0 0 10 4 1)))
(unless (equal? (library-version '(chezpp)) expected-version)
  (error 'umbrella-version "compiled umbrella has an unexpected version"))
SCHEME
