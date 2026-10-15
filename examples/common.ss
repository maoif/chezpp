(import (chezpp cli))

(define example-make-option
  (lambda (name)
    (let ([option (make-option name)])
      (option-value-number-set! option 1)
      (option-value-parser-set! option parser-string)
      (option-no-short?-set! option #t)
      option)))

(define example-command
  (lambda (name overview option-name* exec)
    (let ([command (make-command name)])
      (command-overview-set! command overview)
      (command-help-set! command overview)
      (command-exec-set! command exec)
      (for-each
       (lambda (name)
         (command-options-add! command (example-make-option name)))
       option-name*)
      command)))

(define example-run!
  (lambda (name overview option-name* exec argument*)
    (run-cli-command!
     (example-command name overview option-name* exec)
     argument*)))

(define example-print
  (lambda (value)
    (write value)
    (newline)))

(define example-required
  (lambda (who ref name)
    (let ([value (ref name)])
      (unless value
        (errorf who "missing required --~a=value" name))
      value)))

(define example-option-natural
  (lambda (who ref name default)
    (let ([value (ref name)])
      (if (not value)
          default
          (let ([number (string->number value)])
            (unless (and number (integer? number) (exact? number) (>= number 0))
              (errorf who "--~a must be a nonnegative integer, given ~s" name value))
            number)))))

(define example-option-integer-list
  (lambda (who ref name default)
    (let ([text (ref name)])
      (if (not text)
          default
          (map (lambda (part)
                 (let ([number (string->number part)])
                   (unless (and number (integer? number) (exact? number))
                     (errorf who "--~a expects comma-separated integers" name))
                   number))
               (string-split text #\,))))))
