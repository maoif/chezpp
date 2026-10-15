(import (chezpp benchmark) (chezpp cli))

(define benchmark-option
  (lambda (name parser)
    (let ([option (make-option name)])
      (option-value-number-set! option 1)
      (option-value-parser-set! option parser)
      (option-no-short?-set! option #t)
      option)))

(define benchmark-cli
  (lambda (program overview body argument*)
    (let ([command (make-command program)])
      (command-overview-set! command overview)
      (command-help-set! command overview)
      (command-exec-set! command body)
      (command-options-add!
       command
       (benchmark-option 'size parser-natural)
       (benchmark-option 'samples parser-natural)
       (benchmark-option 'warmup parser-natural)
       (benchmark-option 'min-time parser-flonum)
       (benchmark-option 'max-iterations parser-natural)
       (benchmark-option 'format parser-string)
       (benchmark-option 'output parser-string)
       (let ([option (make-option 'cost-center)])
         (option-value-number-set! option 0)
         (option-no-short?-set! option #t)
         option))
      (run-cli-command! command argument*))))

(define benchmark-config-from-cli
  (lambda (ref)
    (let* ([base (default-benchmark-config)]
           [format-name (or (ref 'format) "text")]
           [reporter (case (string->symbol format-name)
                       [(text) (benchmark-text-reporter)]
                       [(csv) (benchmark-csv-reporter)]
                       [(json) (benchmark-json-reporter)]
                       [(datum) (benchmark-datum-reporter)]
                       [else (errorf 'benchmark "unknown report format ~s" format-name)])]
           [config (benchmark-config-with
                    base
                    (list (cons 'warmup (or (ref 'warmup) 1))
                          (cons 'samples (or (ref 'samples) 5))
                          (cons 'min-time (or (ref 'min-time) 0.01))
                          (cons 'max-iterations (or (ref 'max-iterations) 10000))
                          (cons 'reporter reporter)
                          (cons 'cost-center? (and (ref 'cost-center) #t))))]
           [output (ref 'output)])
      (if output
          (let ([port (open-file-output-port output (file-options no-fail replace)
                                             (buffer-mode block) (native-transcoder))])
            (values (benchmark-config-with config (list (cons 'output port))) port))
          (values config #f)))))

(define benchmark-size
  (lambda (ref)
    (or (ref 'size) 1000)))

(define benchmark-run-cli!
  (lambda (program overview benchmarks ref)
    (let-values ([(config output-port) (benchmark-config-from-cli ref)])
      (dynamic-wind
        void
        (lambda () (benchmark-run benchmarks config))
        (lambda () (when output-port (close-port output-port))))
      0)))

(define benchmark-consume
  (lambda (value)
    (benchmark-do-not-optimize value)))
