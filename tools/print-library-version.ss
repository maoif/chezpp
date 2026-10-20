(import (chezscheme))

;; Load a compiled library and print the version declared by its CSPP umbrella.
(define arguments (command-line-arguments))
(unless (= (length arguments) 1)
  (error 'print-library-version "expected a compiled CSPP library path"))

(load (car arguments))
(define version (library-version '(cspp)))
(unless (and (list? version) (= (length version) 6))
  (error 'print-library-version "compiled CSPP umbrella has no valid version" version))

(printf "CSPP version: ~a.~a.~a@~a-~a-~a\n"
        (car version) (cadr version) (caddr version)
        (cadddr version) (car (cddddr version)) (cadr (cddddr version)))
