(import (chezscheme))

(define check-file
  (lambda (path)
    (call-with-input-file path
      (lambda (input)
        (let loop ()
          (unless (eof-object? (read input))
            (loop)))))))

(for-each check-file (command-line-arguments))
