(import (chezscheme))

;; Shared by build-only scripts, before the Chezpp libraries are available.
;; Options are key=value arguments; filenames follow a -- separator.
(define read-build-arguments
  (lambda (arguments)
    (let loop ([arguments arguments] [options '()])
      (cond
       [(null? arguments) (error 'compiler-options "missing -- before filenames")]
       [(string=? (car arguments) "--") (values (reverse options) (cdr arguments))]
       [else
        (let* ([argument (car arguments)] [size (string-length argument)]
               [separator
                (let find ([index 0])
                  (cond
                   [(fx= index size) (error 'compiler-options "expected key=value" argument)]
                   [(char=? (string-ref argument index) #\=) index]
                   [else (find (fx1+ index))]))])
          (loop (cdr arguments)
                (cons (cons (string->symbol (substring argument 0 separator))
                            (substring argument (fx1+ separator) size))
                      options)))]))))

(define compiler-option
  (lambda (options name)
    (let ([entry (assq name options)])
      (and entry (cdr entry)))))

(define compiler-boolean
  (lambda (value)
    (and (member value '("t" "true" "yes" "1")) #t)))

(define compiler-option-setting
  (lambda (options name parameter boolean?)
    (let ([value (compiler-option options name)])
      (cond
       [(or (not value)
            (and (string=? value "") (not (memq name '(i psi wpo)))))
        (parameter)]
       [boolean? (compiler-boolean value)]
       [else
        (or (string->number value)
            (error 'compiler-options "expected numeric compiler option" name value))]))))

(define call-with-compiler-options
  (lambda (options thunk)
    (parameterize
        ([optimize-level (compiler-option-setting options 'o optimize-level #f)]
         [debug-level (compiler-option-setting options 'd debug-level #f)]
         [generate-inspector-information
          (compiler-option-setting options 'i generate-inspector-information #t)]
         [generate-procedure-source-information
          (compiler-option-setting options 'psi generate-procedure-source-information #t)]
         [commonization-level (compiler-option-setting options 'cl commonization-level #f)]
         [cp0-effort-limit (compiler-option-setting options 'cp0 cp0-effort-limit #f)]
         [fasl-compressed (compiler-option-setting options 'fc fasl-compressed #t)]
         [generate-covin-files (compiler-option-setting options 'c generate-covin-files #t)]
         [compile-profile (compiler-option-setting options 'p compile-profile #t)]
         [generate-profile-forms (compiler-option-setting options 'xp generate-profile-forms #t)]
         [generate-wpo-files (compiler-option-setting options 'wpo generate-wpo-files #t)])
      (thunk))))
