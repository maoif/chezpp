(import (chezscheme))

;; Load a compiled library and print the version declared by its Chezpp umbrella.
(define arguments (command-line-arguments))
(unless (= (length arguments) 1)
  (error 'print-library-version "expected a compiled Chezpp library path"))

(load (car arguments))
(define version (library-version '(chezpp)))
(unless (and (list? version) (= (length version) 6))
  (error 'print-library-version "compiled Chezpp umbrella has no valid version" version))

(printf "ChezPP version: ~a.~a.~a@~a-~a-~a\n"
        (car version) (cadr version) (caddr version)
        (cadddr version) (car (cddddr version)) (cadr (cddddr version)))
