(import (chezscheme))

;; Run in the tests directory with one compiled test object.
(define arguments (command-line-arguments))
(unless (= (length arguments) 1)
  (error 'run-test "expected one test object"))

(load "mat.so")
(load (car arguments))
