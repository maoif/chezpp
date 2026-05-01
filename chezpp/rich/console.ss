#!chezscheme
(library (chezpp rich console)
  (export make-rich-console
          rich-console
          rich-console?
          rich-print
          rich-println
          rich-fprint
          rich-fprintln
          rich-render
          rich-export-text
          rich-export-ansi)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp rich private common)
          (chezpp rich style)
          (chezpp rich segment)
          (chezpp rich renderable))

  (define-record-type rich-console-record
    (fields (mutable output-port rich-console-output-port rich-console-output-port-set!)
            (mutable error-port rich-console-error-port rich-console-error-port-set!)
            (mutable input-port rich-console-input-port rich-console-input-port-set!)
            (mutable width rich-console-width rich-console-width-set!)
            (mutable color-system rich-console-color-system rich-console-color-system-set!)
            (mutable force-terminal? rich-console-force-terminal? rich-console-force-terminal?-set!)
            (mutable soft-wrap? rich-console-soft-wrap? rich-console-soft-wrap?-set!)
            (mutable theme rich-console-theme rich-console-theme-set!)
            (mutable ascii-only? rich-console-ascii-only? rich-console-ascii-only?-set!)))

  #|proc:rich-console?
  The `rich-console?` procedure returns `#t` when its argument is a rich console
  object, and `#f` otherwise.
  |#
  (define rich-console? rich-console-record?)

  #|proc:make-rich-console
  The `make-rich-console` procedure constructs a console using current ports and
  conservative terminal defaults.
  |#
  (define make-rich-console
    (case-lambda
      [()
       (make-rich-console-record (current-output-port)
                                 (current-error-port)
                                 (current-input-port)
                                 80
                                 'standard
                                 #f
                                 #f
                                 #f
                                 #f)]))

  #|macro:rich-console
  The `rich-console` macro constructs a console and binds it to an identifier.
  |#
  (define-syntax rich-console
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         #'(define name (make-rich-console))]
        [_ (syntax-error stx "invalid rich-console form")])))

  (define $target+values
    (lambda (args)
      (cond [(null? args) (values (current-output-port) '())]
            [(or (output-port? (car args)) (rich-console? (car args)))
             (values (car args) (cdr args))]
            [else (values (current-output-port) args)])))

  (define $target-port
    (lambda (target)
      (if (rich-console? target)
          (rich-console-output-port target)
          target)))

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $rich-segment-line-list?
    (lambda (x)
      (and (list? x) (rich-list-every? $rich-segment-list? x))))

  (define $value->segment
    (lambda (value)
      (rich-segment
       (if (string? value)
           value
           (rich-string-output
            (lambda (port)
              (if (char? value)
                  (write-char value port)
                  (display value port))))))))

  (define $write-segment-line
    (lambda (port line)
      (display (rich-segments->plain line) port)))

  (define $write-segment-lines
    (lambda (port lines)
      (let loop ([lines lines] [first? #t])
        (unless (null? lines)
          (unless first? (newline port))
          ($write-segment-line port (car lines))
          (loop (cdr lines) #f)))))

  (define $valid-rendered-value?
    (lambda (value)
      (or (string? value) ($rich-segment-line-list? value))))

  (define $check-rendered-value
    (lambda (who value)
      (if ($valid-rendered-value? value)
          value
          (errorf who "renderer returned invalid value: ~a" value))))

  (define $write-rendered-value
    (lambda (port value)
      (let ([value ($check-rendered-value 'rich-print value)])
        (cond [(string? value) (display value port)]
              [else ($write-segment-lines port value)]))))

  (define $write-rich-value
    (lambda (port value)
      (cond [(rich-style? value) (void)]
            [(rich-reset? value) (void)]
            [else
             (let ([renderer (rich-renderer-for value)])
               (if renderer
                   ($write-rendered-value port (renderer value))
                   ($write-segment-line port (list ($value->segment value)))))])))

  #|proc:rich-print
  The `rich-print` procedure prints values to a rich console, an output port, or
  the current output port.
  |#
  (define rich-print
    (lambda args
      (let-values ([(target values) ($target+values args)])
        (let ([port ($target-port target)])
          (pcheck ([output-port? port])
                  (for-each (lambda (value) ($write-rich-value port value)) values))))))

  #|proc:rich-println
  The `rich-println` procedure prints values and then writes a newline.
  |#
  (define rich-println
    (lambda args
      (let-values ([(target values) ($target+values args)])
        (let ([port ($target-port target)])
          (pcheck ([output-port? port])
                  (for-each (lambda (value) ($write-rich-value port value)) values)
                  (newline port))))))

  #|proc:rich-fprint
  The `rich-fprint` procedure prints values to the given output port.
  |#
  (define rich-fprint
    (lambda (port . values)
      (pcheck ([output-port? port])
              (apply rich-print port values))))

  #|proc:rich-fprintln
  The `rich-fprintln` procedure prints values to the given output port and then
  writes a newline.
  |#
  (define rich-fprintln
    (lambda (port . values)
      (pcheck ([output-port? port])
              (apply rich-println port values))))

  #|proc:rich-render
  The `rich-render` procedure renders a value for a rich console.
  |#
  (define rich-render
    (lambda (console value)
      (pcheck ([rich-console? console])
              (let ([renderer (rich-renderer-for value)])
                (if renderer
                    ($check-rendered-value 'rich-render (renderer value))
                    value)))))

  #|proc:rich-export-text
  The `rich-export-text` procedure renders a value to plain text and returns the
  result as a string.
  |#
  (define rich-export-text
    (lambda (value)
      (rich-string-output
       (lambda (port)
         (rich-print port value)))))

  #|proc:rich-export-ansi
  The `rich-export-ansi` procedure renders a value to ANSI text and returns the
  result as a string.
  |#
  (define rich-export-ansi rich-export-text))
