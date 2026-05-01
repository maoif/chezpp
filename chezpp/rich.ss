#!chezscheme
(library (chezpp rich)
  (export rich-style
          rich-style?
          reset-style
          rich-reset?

          make-rich-console
          rich-console
          rich-console?
          rich-print
          rich-println
          rich-fprint
          rich-fprintln
          rich-render
          rich-export-text
          rich-export-ansi
          rich-register-renderer!)
  (import (chezpp chez)
          (chezpp utils))

  (define-record-type rich-style-record
    (fields (immutable attrs rich-style-attrs)
            (immutable fg rich-style-fg)
            (immutable bg rich-style-bg)))

  (define-record-type rich-reset-record
    (fields))

  #|proc:rich-style?
  The `rich-style?` procedure returns `#t` when its argument is a rich style
  object, and `#f` otherwise.
  |#
  (define rich-style? rich-style-record?)

  #|proc:rich-reset?
  The `rich-reset?` procedure returns `#t` when its argument is a rich style
  reset object, and `#f` otherwise.
  |#
  (define rich-reset? rich-reset-record?)

  #|proc:reset-style
  The `reset-style` procedure returns an opaque object that resets the active
  rich style when printed by rich output procedures.
  |#
  (define reset-style
    (lambda ()
      (make-rich-reset-record)))

  #|proc:rich-style
  The `rich-style` procedure constructs an opaque rich style object.
  |#
  (define rich-style
    (lambda args
      (make-rich-style-record args #f #f)))

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

  (define $write-rich-value
    (lambda (port value)
      (cond [(rich-style? value) (void)]
            [(rich-reset? value) (void)]
            [(string? value) (display value port)]
            [(char? value) (write-char value port)]
            [else (display value port)])))

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
      (apply rich-print port values)))

  #|proc:rich-fprintln
  The `rich-fprintln` procedure prints values to the given output port and then
  writes a newline.
  |#
  (define rich-fprintln
    (lambda (port . values)
      (apply rich-println port values)))

  #|proc:rich-render
  The `rich-render` procedure renders a value for a rich console.
  |#
  (define rich-render
    (lambda (console value)
      (pcheck ([rich-console? console])
              value)))

  #|proc:rich-export-text
  The `rich-export-text` procedure renders a value to plain text and returns the
  result as a string.
  |#
  (define rich-export-text
    (lambda (value)
      (call-with-string-output-port
        (lambda (port)
          (rich-print port value)))))

  #|proc:rich-export-ansi
  The `rich-export-ansi` procedure renders a value to ANSI text and returns the
  result as a string.
  |#
  (define rich-export-ansi rich-export-text)

  #|proc:rich-register-renderer!
  The `rich-register-renderer!` procedure registers a renderer predicate and
  rendering procedure for future rich output.
  |#
  (define rich-register-renderer!
    (lambda (pred renderer)
      (pcheck ([procedure? pred renderer])
              (void)))))
