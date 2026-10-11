(import (chezscheme))

;; Run in the tests directory: key=value options, --, source, optional support object.
(load (string-append (path-parent (car (command-line))) "/compiler-options.ss"))

(let-values ([(options files) (read-build-arguments (command-line-arguments))])
  (unless (<= 1 (length files) 2)
    (error 'compile-test "expected source and optional support object"))
  (call-with-compiler-options
   (append '((i . "t") (psi . "t") (c . "f") (wpo . "f")) options)
   (lambda ()
     (parameterize ([compile-file-message #t])
       (when (pair? (cdr files)) (load (cadr files)))
       (compile-file (car files))))))
