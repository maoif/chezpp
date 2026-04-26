#!chezscheme
(library (chezpp rich)
  (export rich-format
          rich-print
          rich-println
          rich-fprint
          rich-fprintln
          rich-style
          rich-style?
          rich-reset
          rich-enable-color?)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal))

  (define-record-type ($rich-style mk-rich-style $rich-style?)
    (fields (immutable ansi rich-style-ansi)))

  (define $style-code
    (lambda (who name)
      (case name
        [(reset) "\033[0m"]
        [(bold) "\033[1m"]
        [(dim) "\033[2m"]
        [(italic) "\033[3m"]
        [(underline) "\033[4m"]
        [(blink) "\033[5m"]
        [(reverse) "\033[6m"]
        [(hidden) "\033[7m"]
        [(strike) "\033[8m"]

        [(black) "\033[30m"]
        [(red) "\033[31m"]
        [(green) "\033[32m"]
        [(yellow) "\033[33m"]
        [(blue) "\033[34m"]
        [(magenta) "\033[35m"]
        [(cyan) "\033[36m"]
        [(white) "\033[37m"]
        [(default) "\033[39m"]

        [(bright-black) "\033[90m"]
        [(bright-red) "\033[91m"]
        [(bright-green) "\033[92m"]
        [(bright-yellow) "\033[93m"]
        [(bright-blue) "\033[94m"]
        [(bright-magenta) "\033[95m"]
        [(bright-cyan) "\033[96m"]
        [(bright-white) "\033[97m"]

        [(bg-black) "\033[40m"]
        [(bg-red) "\033[41m"]
        [(bg-green) "\033[42m"]
        [(bg-yellow) "\033[43m"]
        [(bg-blue) "\033[44m"]
        [(bg-magenta) "\033[45m"]
        [(bg-cyan) "\033[46m"]
        [(bg-white) "\033[47m"]
        [(bg-default) "\033[49m"]

        [(bg-bright-black) "\033[100m"]
        [(bg-bright-red) "\033[101m"]
        [(bg-bright-green) "\033[102m"]
        [(bg-bright-yellow) "\033[103m"]
        [(bg-bright-blue) "\033[104m"]
        [(bg-bright-magenta) "\033[105m"]
        [(bg-bright-cyan) "\033[106m"]
        [(bg-bright-white) "\033[107m"]
        [else (errorf who "unknown rich style: ~a" name)])))

  (define $rich-render-arg
    (lambda (arg)
      (if ($rich-style? arg)
          (if (rich-enable-color?)
              (rich-style-ansi arg)
              "")
          arg)))

  (define $rich-render-args
    (lambda (args)
      (map $rich-render-arg args)))

  #|proc:rich-enable-color?
The `rich-enable-color?` parameter controls whether rich style values render
ANSI escape codes. When set to `#f`, style values render as empty strings.
|#
  (define rich-enable-color?
    (make-parameter
     #t
     (lambda (v)
       (if (boolean? v)
           v
           (errorf 'rich-enable-color? "expected boolean: ~a" v)))))

  #|proc:rich-style?
The `rich-style?` procedure checks whether `obj` is a rich style value.
|#
  (define rich-style?
    (lambda (obj)
      ($rich-style? obj)))

  #|proc:rich-style
The `rich-style` procedure composes one or more style symbols into a style
value that can be inserted into `rich-format` and rich print format strings.
|#
  (define-who rich-style
    (lambda names
      (pcheck ([list? names])
              (when (null? names)
                (errorf who "expected at least one style name"))
              (for-each
               (lambda (name)
                 (unless (symbol? name)
                   (errorf who "expected style symbol: ~a" name)))
               names)
              (mk-rich-style
               (apply string-append
                      (map (lambda (name) ($style-code who name)) names))))))

  #|proc:rich-format
The `rich-format` procedure formats `fmt` with `args` like Chez `format`,
converting rich style values to ANSI escape strings before formatting.
|#
  (define rich-format
    (lambda (fmt . args)
      (pcheck ([string? fmt])
              (apply format #f fmt ($rich-render-args args)))))

  #|proc:rich-print
The `rich-print` procedure writes formatted rich output to the current output
port without appending a newline.
|#
  (define rich-print
    (lambda (fmt . args)
      (pcheck ([string? fmt])
              (apply format (current-output-port) fmt ($rich-render-args args)))))

  #|proc:rich-println
The `rich-println` procedure writes formatted rich output to the current output
port and appends a newline.
|#
  (define rich-println
    (lambda (fmt . args)
      (pcheck ([string? fmt])
              (apply format (current-output-port) fmt ($rich-render-args args))
              (newline))))

  #|proc:rich-fprint
The `rich-fprint` procedure writes formatted rich output to `port` without
appending a newline.
|#
  (define rich-fprint
    (lambda (port fmt . args)
      (pcheck ([output-port? port] [string? fmt])
              (apply format port fmt ($rich-render-args args)))))

  #|proc:rich-fprintln
The `rich-fprintln` procedure writes formatted rich output to `port` and
appends a newline.
|#
  (define rich-fprintln
    (lambda (port fmt . args)
      (pcheck ([output-port? port] [string? fmt])
              (apply format port fmt ($rich-render-args args))
              (newline port))))

  #|proc:rich-reset
The `rich-reset` value resets terminal styles when inserted into a rich format
string.
|#
  (define rich-reset (mk-rich-style "\033[0m"))

  )
