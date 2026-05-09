#!chezscheme
(library (chezpp rich style)
  (export rich-style
          rich-style?
          rich-reset?
          reset-style
          rich-style-fg
          rich-style-bg
          rich-style-bold?
          rich-style-dim?
          rich-style-italic?
          rich-style-underline?
          rich-style-blink?
          rich-style-reverse?
          rich-style-hidden?
          rich-style-strike?
          rich-color-system?
          rich-style->ansi
          rich-ansi-reset
          rich-theme
          make-rich-theme
          rich-theme?
          rich-theme-ref
          rich-theme-set!)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp rich private common))

  (define rich-ansi-reset "\033[0m")

  (define-record-type (rich-style-record make-rich-style-record $rich-style?)
    (opaque #t)
    (fields (immutable fg $rich-style-fg)
            (immutable bg $rich-style-bg)
            (immutable bold? $rich-style-bold?)
            (immutable dim? $rich-style-dim?)
            (immutable italic? $rich-style-italic?)
            (immutable underline? $rich-style-underline?)
            (immutable blink? $rich-style-blink?)
            (immutable reverse? $rich-style-reverse?)
            (immutable hidden? $rich-style-hidden?)
            (immutable strike? $rich-style-strike?)))

  (define-record-type (rich-reset-record make-rich-reset-record $rich-reset?)
    (opaque #t)
    (fields))

  (define $named-fg
    '((black . 30) (red . 31) (green . 32) (yellow . 33)
      (blue . 34) (magenta . 35) (cyan . 36) (white . 37)
      (default . 39)
      (bright-black . 90) (bright-red . 91) (bright-green . 92)
      (bright-yellow . 93) (bright-blue . 94) (bright-magenta . 95)
      (bright-cyan . 96) (bright-white . 97)))

  (define $named-bg
    '((on-black . 40) (on-red . 41) (on-green . 42) (on-yellow . 43)
      (on-blue . 44) (on-magenta . 45) (on-cyan . 46) (on-white . 47)
      (on-default . 49)
      (on-bright-black . 100) (on-bright-red . 101)
      (on-bright-green . 102) (on-bright-yellow . 103)
      (on-bright-blue . 104) (on-bright-magenta . 105)
      (on-bright-cyan . 106) (on-bright-white . 107)))

  (define $color-ref
    (lambda (who table name)
      (let ([a (assq name table)])
        (and a (cdr a)))))

  (define $rgb-red
    (lambda (rgb)
      (fxsrl rgb 16)))

  (define $rgb-green
    (lambda (rgb)
      (fxlogand (fxsrl rgb 8) #xff)))

  (define $rgb-blue
    (lambda (rgb)
      (fxlogand rgb #xff)))

  (define $sgr
    (lambda (code)
      (string-append "\033[" (number->string code) "m")))

  (define $rgb-sgr
    (lambda (prefix rgb)
      (string-append "\033["
                     prefix
                     ";2;"
                     (number->string ($rgb-red rgb))
                     ";"
                     (number->string ($rgb-green rgb))
                     ";"
                     (number->string ($rgb-blue rgb))
                     "m")))

  (define $apply-symbol-style
    (lambda (opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
      (case opt
        [(bold) (values fg bg #t dim? italic? underline? blink? reverse? hidden? strike?)]
        [(no-bold) (values fg bg #f dim? italic? underline? blink? reverse? hidden? strike?)]
        [(dim) (values fg bg bold? #t italic? underline? blink? reverse? hidden? strike?)]
        [(no-dim) (values fg bg bold? #f italic? underline? blink? reverse? hidden? strike?)]
        [(italic) (values fg bg bold? dim? #t underline? blink? reverse? hidden? strike?)]
        [(no-italic) (values fg bg bold? dim? #f underline? blink? reverse? hidden? strike?)]
        [(underline) (values fg bg bold? dim? italic? #t blink? reverse? hidden? strike?)]
        [(no-underline) (values fg bg bold? dim? italic? #f blink? reverse? hidden? strike?)]
        [(blink) (values fg bg bold? dim? italic? underline? #t reverse? hidden? strike?)]
        [(no-blink) (values fg bg bold? dim? italic? underline? #f reverse? hidden? strike?)]
        [(reverse) (values fg bg bold? dim? italic? underline? blink? #t hidden? strike?)]
        [(no-reverse) (values fg bg bold? dim? italic? underline? blink? #f hidden? strike?)]
        [(hidden) (values fg bg bold? dim? italic? underline? blink? reverse? #t strike?)]
        [(no-hidden) (values fg bg bold? dim? italic? underline? blink? reverse? #f strike?)]
        [(strike) (values fg bg bold? dim? italic? underline? blink? reverse? hidden? #t)]
        [(no-strike) (values fg bg bold? dim? italic? underline? blink? reverse? hidden? #f)]
        [else
         (let ([fg-code ($color-ref 'rich-style $named-fg opt)]
               [bg-code ($color-ref 'rich-style $named-bg opt)])
           (cond [fg-code (values opt bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
                 [bg-code (values fg opt bold? dim? italic? underline? blink? reverse? hidden? strike?)]
                 [else (errorf 'rich-style "unknown style option: ~a" opt)]))])))

  (define $apply-color-form
    (lambda (opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
      (if (and (pair? opt) (pair? (cdr opt)) (null? (cddr opt)))
          (let ([kind (car opt)] [rgb (cadr opt)])
            (unless (rich-rgb-integer? rgb)
              (errorf 'rich-style "invalid RGB color: ~a" rgb))
            (case kind
              [(fg) (values rgb bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
              [(bg) (values fg rgb bold? dim? italic? underline? blink? reverse? hidden? strike?)]
              [else (errorf 'rich-style "invalid color form: ~a" opt)]))
          (errorf 'rich-style "invalid color form: ~a" opt))))

  (define $apply-style-option
    (lambda (opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
      (cond [(symbol? opt)
             ($apply-symbol-style opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
            [(integer? opt)
             (if (rich-rgb-integer? opt)
                 (values opt bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
                 (errorf 'rich-style "invalid RGB color: ~a" opt))]
            [(pair? opt)
             ($apply-color-form opt fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)]
            [else (errorf 'rich-style "invalid style option: ~a" opt)])))

  (define $resolve-style
    (lambda (opts)
      (let loop ([opts opts]
                 [fg #f] [bg #f]
                 [bold? #f] [dim? #f] [italic? #f] [underline? #f]
                 [blink? #f] [reverse? #f] [hidden? #f] [strike? #f])
        (if (null? opts)
            (make-rich-style-record fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
            (let-values ([(fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)
                          ($apply-style-option (car opts) fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?)])
              (loop (cdr opts) fg bg bold? dim? italic? underline? blink? reverse? hidden? strike?))))))

  #|proc:rich-style?
  The `rich-style?` procedure returns `#t` when its argument is a rich style
  object, and `#f` otherwise.
  |#
  (define rich-style?
    (lambda (x)
      ($rich-style? x)))

  #|proc:rich-reset?
  The `rich-reset?` procedure returns `#t` when its argument is a rich style
  reset object, and `#f` otherwise.
  |#
  (define rich-reset?
    (lambda (x)
      ($rich-reset? x)))

  #|proc:rich-style-fg
  The `rich-style-fg` procedure returns the resolved foreground color of
  `style`, or `#f` when none is set.
  |#
  (define rich-style-fg
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-fg style))))

  #|proc:rich-style-bg
  The `rich-style-bg` procedure returns the resolved background color of
  `style`, or `#f` when none is set.
  |#
  (define rich-style-bg
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-bg style))))

  #|proc:rich-style-bold?
  The `rich-style-bold?` procedure returns whether `style` enables bold text.
  |#
  (define rich-style-bold?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-bold? style))))

  #|proc:rich-style-dim?
  The `rich-style-dim?` procedure returns whether `style` enables dim text.
  |#
  (define rich-style-dim?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-dim? style))))

  #|proc:rich-style-italic?
  The `rich-style-italic?` procedure returns whether `style` enables italic
  text.
  |#
  (define rich-style-italic?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-italic? style))))

  #|proc:rich-style-underline?
  The `rich-style-underline?` procedure returns whether `style` enables
  underlined text.
  |#
  (define rich-style-underline?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-underline? style))))

  #|proc:rich-style-blink?
  The `rich-style-blink?` procedure returns whether `style` enables blinking
  text.
  |#
  (define rich-style-blink?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-blink? style))))

  #|proc:rich-style-reverse?
  The `rich-style-reverse?` procedure returns whether `style` enables reversed
  text.
  |#
  (define rich-style-reverse?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-reverse? style))))

  #|proc:rich-style-hidden?
  The `rich-style-hidden?` procedure returns whether `style` enables hidden
  text.
  |#
  (define rich-style-hidden?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-hidden? style))))

  #|proc:rich-style-strike?
  The `rich-style-strike?` procedure returns whether `style` enables
  strikethrough text.
  |#
  (define rich-style-strike?
    (lambda (style)
      (pcheck ([rich-style? style])
              ($rich-style-strike? style))))

  #|proc:reset-style
  The `reset-style` procedure returns an opaque object that resets the active
  rich style when printed by rich output procedures.
  |#
  (define reset-style
    (lambda ()
      (make-rich-reset-record)))

  #|proc:rich-style
  The `rich-style` procedure constructs an opaque rich style object from style
  symbols and RGB color forms.
  |#
  (define rich-style
    (lambda opts
      (pcheck ([list? opts])
              ($resolve-style opts))))

  #|proc:rich-color-system?
  The `rich-color-system?` procedure returns `#t` for supported rich color
  system names, and `#f` otherwise.
  |#
  (define rich-color-system?
    (lambda (x)
      (and (memq x '(auto none standard 256 truecolor)) #t)))

  (define $rich-color-system-option?
    (lambda (x)
      (or (not x) (rich-color-system? x))))

  (define $color->ansi
    (lambda (fg? color)
      (cond [(integer? color)
             ($rgb-sgr (if fg? "38" "48") color)]
            [(symbol? color)
             (let ([code ($color-ref 'rich-style (if fg? $named-fg $named-bg) color)])
               (if code ($sgr code) ""))]
            [else ""])))

  #|proc:rich-style->ansi
  The `rich-style->ansi` procedure renders a rich style object as ANSI SGR
  escape sequences for `color-system`. The `'none` color system suppresses ANSI
  output; other accepted systems currently emit the available SGR and truecolor
  sequences without RGB color downgrade.
  |#
  (define rich-style->ansi
    (lambda (color-system style)
      (pcheck ([$rich-color-system-option? color-system] [rich-style? style])
              (if (eq? color-system 'none)
                  ""
                  (string-append
                   (if (rich-style-bold? style) ($sgr 1) "")
                   (if (rich-style-dim? style) ($sgr 2) "")
                   (if (rich-style-italic? style) ($sgr 3) "")
                   (if (rich-style-underline? style) ($sgr 4) "")
                   (if (rich-style-blink? style) ($sgr 5) "")
                   (if (rich-style-reverse? style) ($sgr 7) "")
                   (if (rich-style-hidden? style) ($sgr 8) "")
                   (if (rich-style-strike? style) ($sgr 9) "")
                   (let ([fg (rich-style-fg style)])
                     (if fg ($color->ansi #t fg) ""))
                   (let ([bg (rich-style-bg style)])
                     (if bg ($color->ansi #f bg) "")))))))

  (define-record-type (rich-theme-record make-rich-theme-record $rich-theme?)
    (opaque #t)
    (fields (immutable table rich-theme-table)))

  (define $make-theme-table
    (lambda ()
      (make-eq-hashtable)))

  #|proc:make-rich-theme
  The `make-rich-theme` procedure constructs a mutable rich theme.
  |#
  (define make-rich-theme
    (case-lambda
      [() (make-rich-theme-record ($make-theme-table))]
      [(entries)
       (pcheck ([list? entries])
               (let ([theme (make-rich-theme)])
                 (for-each
                  (lambda (entry)
                    (if (and (pair? entry) (symbol? (car entry)))
                        (rich-theme-set! theme (car entry) (cdr entry))
                        (errorf 'make-rich-theme "invalid theme entry: ~a" entry)))
                  entries)
                 theme))]))

  #|macro:rich-theme
  The `rich-theme` macro constructs and returns a mutable rich theme. Theme
  entries are written as `:key value` clauses.
  |#
  (define-syntax rich-theme
    (lambda (stx)
      (define keyword->name
        (lambda (key)
          (let* ([sym (syntax->datum key)]
                 [str (and (symbol? sym) (symbol->string sym))]
                 [len (if str (string-length str) 0)])
            (if (and str (positive? len) (char=? (string-ref str 0) #\:))
                (string->symbol (substring str 1 len))
                (syntax-error key "invalid rich theme key")))))
      (define build-entries
        (lambda (clause*)
          (let loop ([clause* clause*] [entry* '()])
            (cond [(null? clause*) (reverse entry*)]
                  [(null? (cdr clause*))
                   (syntax-error stx "invalid rich-theme form")]
                  [else
                   (let ([name (keyword->name (car clause*))]
                         [value (cadr clause*)])
                     (with-syntax ([key-name (datum->syntax (car clause*) name)]
                                   [key-value value])
                       (loop (cddr clause*)
                             (cons #'(cons 'key-name key-value) entry*))))]))))
      (syntax-case stx ()
        [(_ clause ...)
         (with-syntax ([(entry ...) (build-entries #'(clause ...))])
           #'(make-rich-theme (list entry ...)))]
        [_ (syntax-error stx "invalid rich-theme form")])))

  #|proc:rich-theme?
  The `rich-theme?` procedure returns `#t` when its argument is a rich theme,
  and `#f` otherwise.
  |#
  (define rich-theme?
    (lambda (x)
      ($rich-theme? x)))

  #|proc:rich-theme-ref
  The `rich-theme-ref` procedure returns the value bound to `name` in `theme`.
  |#
  (define rich-theme-ref
    (case-lambda
      [(theme name)
       (pcheck ([rich-theme? theme] [symbol? name])
               (hashtable-ref (rich-theme-table theme) name #f))]
      [(theme name default)
       (pcheck ([rich-theme? theme] [symbol? name])
               (hashtable-ref (rich-theme-table theme) name default))]))

  #|proc:rich-theme-set!
  The `rich-theme-set!` procedure binds `name` to `value` in `theme`.
  |#
  (define rich-theme-set!
    (lambda (theme name value)
      (pcheck ([rich-theme? theme] [symbol? name] [rich-style? value])
              (hashtable-set! (rich-theme-table theme) name value)))))
