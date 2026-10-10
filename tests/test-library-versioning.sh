#!/bin/sh
# Verify that the build versions every Chezpp library in memory before compilation.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

source_before=$(cksum chezpp.ss; find chezpp -name '*.ss' -type f -exec cksum {} +)
make --no-print-directory -W Makefile -W chezpp.ss chezpp.lib >/dev/null
test "$(cksum chezpp.ss; find chezpp -name '*.ss' -type f -exec cksum {} +)" = "$source_before"
CHEZPP_EXPECTED_VERSION=$(make --no-print-directory -s print-library-version)
export CHEZPP_EXPECTED_VERSION

./.chezscheme-install/bin/scheme --script /dev/stdin <<'SCHEME'
(import (chezscheme))

(define expected-version
  (call-with-port (open-string-input-port (getenv "CHEZPP_EXPECTED_VERSION")) read))
(load "chezpp.lib")
(environment (list 'chezpp expected-version))
(environment (list 'chezpp 'string expected-version))
(unless (member '(chezpp string) (library-list))
  (error 'library-versioning "constituent library not loaded"))
(for-each
 (lambda (library)
   (unless (equal? (library-version library) expected-version)
     (error 'library-versioning "unexpected version" library (library-version library))))
 (filter
  (lambda (library)
    (and (pair? library) (eq? (car library) 'chezpp)))
  (library-list)))

;; Check the source inventory too, including libraries outside the original umbrella imports.
(define check-source
  (lambda (path)
    (call-with-input-file path
      (lambda (port)
        (let loop ()
          (let ([form (read port)])
            (unless (eof-object? form)
              (when (and (pair? form) (eq? (car form) 'library))
                (unless (equal? (library-version (cadr form)) expected-version)
                  (error 'library-versioning "source library missing or unversioned" path)))
              (loop))))))))
(define check-directory
  (lambda (directory)
    (for-each
     (lambda (entry)
       (let ([path (string-append directory "/" entry)])
         (cond
          [(file-directory? path) (check-directory path)]
          [(string=? (path-extension path) "ss") (check-source path)])))
     (directory-list directory))))
(check-directory "chezpp")
SCHEME

probe_dir=$(mktemp -d /tmp/chezpp-library-versioning.XXXXXX)
trap 'rm -f "$probe_dir/string.so" "$probe_dir/string.wpo" "$probe_dir/string.covin"; rmdir "$probe_dir"' EXIT HUP INT TERM
export CHEZPP_VERSION_PROBE_DIR="$probe_dir"

./.chezscheme-install/bin/scheme --script /dev/stdin <<'SCHEME'
(import (chezscheme))
(load "tools/compile-library-handler.ss")
(define output (string-append (getenv "CHEZPP_VERSION_PROBE_DIR") "/string.so"))
(define handler (make-versioned-compile-library-handler '(9 8 7 10 4 1)
                  (compile-library-handler)))
(parameterize ([generate-wpo-files #t] [generate-covin-files #t] [compile-profile #t])
  (handler "chezpp/string.ss" output))
(load output)
(unless (equal? (library-version '(chezpp string)) '(9 8 7 10 4 1))
  (error 'library-versioning "handler did not apply supplied version"))
(unless (file-exists? (string-append (path-root output) ".wpo"))
  (error 'library-versioning "missing WPO output"))
(define table (make-source-table))
(call-with-port
  (open-file-input-port (string-append (path-root output) ".covin")
                        (file-options compressed) (buffer-mode block) (current-transcoder))
  (lambda (port) (get-source-table! port table)))
(unless (> (source-table-size table) 0)
  (error 'library-versioning "missing annotated coverage information"))
SCHEME
test "$(cksum chezpp.ss; find chezpp -name '*.ss' -type f -exec cksum {} +)" = "$source_before"
