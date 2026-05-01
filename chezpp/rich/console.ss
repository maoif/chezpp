#!chezscheme
(library (chezpp rich console)
  (export make-rich-console
          rich-console
          rich-console?
          rich-console-output-port
          rich-console-output-port-set!
          rich-console-error-port
          rich-console-error-port-set!
          rich-console-input-port
          rich-console-input-port-set!
          rich-console-width
          rich-console-width-set!
          rich-console-color-system
          rich-console-color-system-set!
          rich-console-force-terminal?
          rich-console-force-terminal?-set!
          rich-console-soft-wrap?
          rich-console-soft-wrap?-set!
          rich-console-theme
          rich-console-theme-set!
          rich-console-ascii-only?
          rich-console-ascii-only?-set!
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
    (fields (mutable output-port $rich-console-output-port $rich-console-output-port-set!)
            (mutable error-port $rich-console-error-port $rich-console-error-port-set!)
            (mutable input-port $rich-console-input-port $rich-console-input-port-set!)
            (mutable width $rich-console-width $rich-console-width-set!)
            (mutable color-system $rich-console-color-system $rich-console-color-system-set!)
            (mutable force-terminal? $rich-console-force-terminal? $rich-console-force-terminal?-set!)
            (mutable soft-wrap? $rich-console-soft-wrap? $rich-console-soft-wrap?-set!)
            (mutable theme $rich-console-theme $rich-console-theme-set!)
            (mutable ascii-only? $rich-console-ascii-only? $rich-console-ascii-only?-set!)))

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

  (define $rich-console-theme?
    (lambda (x)
      (or (not x) (rich-theme? x))))

  #|proc:rich-console-output-port
  The `rich-console-output-port` procedure returns the output port used by
  `console`.
  |#
  (define rich-console-output-port
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-output-port console))))

  #|proc:rich-console-output-port-set!
  The `rich-console-output-port-set!` procedure sets the output port used by
  `console`.
  |#
  (define rich-console-output-port-set!
    (lambda (console port)
      (pcheck ([rich-console? console] [output-port? port])
              ($rich-console-output-port-set! console port))))

  #|proc:rich-console-error-port
  The `rich-console-error-port` procedure returns the error port used by
  `console`.
  |#
  (define rich-console-error-port
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-error-port console))))

  #|proc:rich-console-error-port-set!
  The `rich-console-error-port-set!` procedure sets the error port used by
  `console`.
  |#
  (define rich-console-error-port-set!
    (lambda (console port)
      (pcheck ([rich-console? console] [output-port? port])
              ($rich-console-error-port-set! console port))))

  #|proc:rich-console-input-port
  The `rich-console-input-port` procedure returns the input port used by
  `console`.
  |#
  (define rich-console-input-port
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-input-port console))))

  #|proc:rich-console-input-port-set!
  The `rich-console-input-port-set!` procedure sets the input port used by
  `console`.
  |#
  (define rich-console-input-port-set!
    (lambda (console port)
      (pcheck ([rich-console? console] [input-port? port])
              ($rich-console-input-port-set! console port))))

  #|proc:rich-console-width
  The `rich-console-width` procedure returns the render width for `console`.
  |#
  (define rich-console-width
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-width console))))

  #|proc:rich-console-width-set!
  The `rich-console-width-set!` procedure sets the render width for `console`.
  |#
  (define rich-console-width-set!
    (lambda (console width)
      (pcheck ([rich-console? console] [rich-positive-integer? width])
              ($rich-console-width-set! console width))))

  #|proc:rich-console-color-system
  The `rich-console-color-system` procedure returns the ANSI color system for
  `console`.
  |#
  (define rich-console-color-system
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-color-system console))))

  #|proc:rich-console-color-system-set!
  The `rich-console-color-system-set!` procedure sets the ANSI color system for
  `console`.
  |#
  (define rich-console-color-system-set!
    (lambda (console color-system)
      (pcheck ([rich-console? console] [rich-color-system? color-system])
              ($rich-console-color-system-set! console color-system))))

  #|proc:rich-console-force-terminal?
  The `rich-console-force-terminal?` procedure returns whether `console`
  forces terminal output behavior.
  |#
  (define rich-console-force-terminal?
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-force-terminal? console))))

  #|proc:rich-console-force-terminal?-set!
  The `rich-console-force-terminal?-set!` procedure sets whether `console`
  forces terminal output behavior.
  |#
  (define rich-console-force-terminal?-set!
    (lambda (console force-terminal?)
      (pcheck ([rich-console? console] [boolean? force-terminal?])
              ($rich-console-force-terminal?-set! console force-terminal?))))

  #|proc:rich-console-soft-wrap?
  The `rich-console-soft-wrap?` procedure returns whether `console` soft-wraps
  output.
  |#
  (define rich-console-soft-wrap?
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-soft-wrap? console))))

  #|proc:rich-console-soft-wrap?-set!
  The `rich-console-soft-wrap?-set!` procedure sets whether `console`
  soft-wraps output.
  |#
  (define rich-console-soft-wrap?-set!
    (lambda (console soft-wrap?)
      (pcheck ([rich-console? console] [boolean? soft-wrap?])
              ($rich-console-soft-wrap?-set! console soft-wrap?))))

  #|proc:rich-console-theme
  The `rich-console-theme` procedure returns the theme for `console`, or `#f`
  when no theme is configured.
  |#
  (define rich-console-theme
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-theme console))))

  #|proc:rich-console-theme-set!
  The `rich-console-theme-set!` procedure sets the theme for `console`.
  |#
  (define rich-console-theme-set!
    (lambda (console theme)
      (pcheck ([rich-console? console] [$rich-console-theme? theme])
              ($rich-console-theme-set! console theme))))

  #|proc:rich-console-ascii-only?
  The `rich-console-ascii-only?` procedure returns whether `console` restricts
  output to ASCII.
  |#
  (define rich-console-ascii-only?
    (lambda (console)
      (pcheck ([rich-console? console])
              ($rich-console-ascii-only? console))))

  #|proc:rich-console-ascii-only?-set!
  The `rich-console-ascii-only?-set!` procedure sets whether `console`
  restricts output to ASCII.
  |#
  (define rich-console-ascii-only?-set!
    (lambda (console ascii-only?)
      (pcheck ([rich-console? console] [boolean? ascii-only?])
              ($rich-console-ascii-only?-set! console ascii-only?))))

  #|macro:rich-console
  The `rich-console` macro constructs a console and binds it to an identifier.
  |#
  (define-syntax rich-console
    (lambda (stx)
      (define field->setter
        (lambda (field)
          (let ([datum (syntax->datum field)])
            (case datum
              [(:output-port) #'rich-console-output-port-set!]
              [(:error-port) #'rich-console-error-port-set!]
              [(:input-port) #'rich-console-input-port-set!]
              [(:width) #'rich-console-width-set!]
              [(:color-system) #'rich-console-color-system-set!]
              [(:force-terminal?) #'rich-console-force-terminal?-set!]
              [(:soft-wrap?) #'rich-console-soft-wrap?-set!]
              [(:theme) #'rich-console-theme-set!]
              [(:ascii-only?) #'rich-console-ascii-only?-set!]
              [else (syntax-error field "invalid rich-console field")]))))
      (define build-setters
        (lambda (name clause*)
          (let loop ([clause* clause*] [setter* '()])
            (cond [(null? clause*) (reverse setter*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-console form")]
                  [else
                   (let ([setter (field->setter (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([console-name name]
                                   [set-console-field! setter]
                                   [field-value value])
                       (loop (cddr clause*)
                             (cons #'(set-console-field! console-name field-value)
                                   setter*))))]))))
      (syntax-case stx ()
        [(_ name clause ...)
         (identifier? #'name)
         (with-syntax ([(setter ...) (build-setters #'name #'(clause ...))])
           #'(begin
               (define name (make-rich-console))
               setter ...))]
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

  (define $write-rich-value/plain
    (lambda (port value)
      (cond [(rich-style? value) (void)]
            [(rich-reset? value) (void)]
            [else
             (let ([renderer (rich-renderer-for value)])
               (if renderer
                   ($write-rendered-value port (renderer value))
                   ($write-segment-line port (list ($value->segment value)))))])))

  (define $write-rich-values/plain
    (lambda (port values)
      (for-each (lambda (value) ($write-rich-value/plain port value)) values)))

  (define $write-rich-values/console
    (lambda (console values)
      (let ([port (rich-console-output-port console)]
            [color-system (rich-console-color-system console)])
        (let loop ([values values] [style-emitted? #f])
          (unless (null? values)
            (let ([value (car values)])
              (cond [(rich-style? value)
                     (if (eq? color-system 'none)
                         (loop (cdr values) style-emitted?)
                         (begin
                           (display (rich-style->ansi color-system value) port)
                           (loop (cdr values) #t)))]
                    [(rich-reset? value)
                     (when (and style-emitted? (not (eq? color-system 'none)))
                       (display rich-ansi-reset port))
                     (loop (cdr values) #f)]
                    [else
                     ($write-rich-value/plain port value)
                     (loop (cdr values) style-emitted?)])))))))

  (define $write-rich-values
    (lambda (target port values)
      (if (rich-console? target)
          ($write-rich-values/console target values)
          ($write-rich-values/plain port values))))

  #|proc:rich-print
  The `rich-print` procedure prints values to a rich console, an output port, or
  the current output port.
  |#
  (define rich-print
    (lambda args
      (let-values ([(target values) ($target+values args)])
        (let ([port ($target-port target)])
          (pcheck ([output-port? port])
                  ($write-rich-values target port values))))))

  #|proc:rich-println
  The `rich-println` procedure prints values and then writes a newline.
  |#
  (define rich-println
    (lambda args
      (let-values ([(target values) ($target+values args)])
        (let ([port ($target-port target)])
          (pcheck ([output-port? port])
                  ($write-rich-values target port values)
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
