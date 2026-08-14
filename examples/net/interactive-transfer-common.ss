(import (chezpp))

#|proc:interactive-tokenize
The `interactive-tokenize` procedure parses a command line into words, honoring single or double
quotes and backslash escapes without invoking a shell. It returns a list of strings or raises an
error for unterminated quotes or escapes.
|#
(define interactive-tokenize
  (lambda (line)
    (let ([n (string-length line)])
      (let loop ([i 0] [quote #f] [escaped? #f] [word ""] [out '()])
        (cond
         [(fx= i n)
          (when (or quote escaped?)
            (errorf 'interactive-tokenize "unterminated quote or escape"))
          (reverse (if (string=? word "") out (cons word out)))]
         [escaped? (loop (fx1+ i) quote #f
                         (string-append word (string (string-ref line i))) out)]
         [(char=? (string-ref line i) #\\)
          (loop (fx1+ i) quote #t word out)]
         [quote
          (if (char=? (string-ref line i) quote)
              (loop (fx1+ i) #f #f word out)
              (loop (fx1+ i) quote #f
                    (string-append word (string (string-ref line i))) out))]
         [(or (char=? (string-ref line i) #\") (char=? (string-ref line i) #\'))
          (loop (fx1+ i) (string-ref line i) #f word out)]
         [(char-whitespace? (string-ref line i))
          (if (string=? word "")
              (loop (fx1+ i) #f #f word out)
              (loop (fx1+ i) #f #f "" (cons word out)))]
         [else
          (loop (fx1+ i) #f #f
                (string-append word (string (string-ref line i))) out)])))))

#|proc:interactive-command
The `interactive-command` procedure parses one line and validates that its command is in
`allowed*`. It returns `(command . arguments)` using lowercase command names.
|#
(define interactive-command
  (lambda (line allowed*)
    (let ([part* (interactive-tokenize line)])
      (if (null? part*)
          #f
          (let ([command (string-downcase (car part*))]
                [arguments (cdr part*)])
            (unless (member command allowed*)
              (errorf 'interactive-command "unknown command ~a" command))
            (cons command arguments))))))

(define interactive-arity!
  (lambda (command arguments minimum maximum)
    (when (or (< (length arguments) minimum)
              (and maximum (> (length arguments) maximum)))
      (errorf 'interactive-command "wrong number of arguments for ~a" command))))
