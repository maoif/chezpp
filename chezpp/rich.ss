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
          rich-enable-color?

          make-rich-table
          rich-table?
          rich-table-add-column!
          rich-table-add-row!
          rich-table-render
          rich-table-print
          rich-table-println
          rich-table-fprint
          rich-table-fprintln

          rich-panel
          rich-panel?
          rich-panel-border-style?
          rich-panel-render
          rich-panel-print
          rich-panel-println
          rich-panel-fprint
          rich-panel-fprintln)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal))

  (define-record-type ($rich-style mk-rich-style $rich-style?)
    (fields (immutable ansi rich-style-ansi)))

  (define-record-type ($rich-table mk-rich-table $rich-table?)
    (fields (mutable columns rich-table-columns rich-table-columns-set!)
            (mutable rows rich-table-rows rich-table-rows-set!)))

  (define-record-type ($rich-panel mk-rich-panel $rich-panel?)
    (fields (immutable body rich-panel-body)
            (immutable title rich-panel-title)
            (immutable border-style rich-panel-border-style)))

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

  (define $string-join
    (lambda (sep strs)
      (cond [(null? strs) ""]
            [(null? (cdr strs)) (car strs)]
            [else (let loop ([strs (cdr strs)] [res (car strs)])
                    (if (null? strs)
                        res
                        (loop (cdr strs) (string-append res sep (car strs)))))])))

  (define $rich-cell->string
    (lambda (cell)
      (if (string? cell)
          cell
          (format #f "~a" ($rich-render-arg cell)))))

  (define $string-split-lines
    (lambda (str)
      (let ([len (string-length str)])
        (let loop ([i 0] [start 0] [res '()])
          (cond [(fx= i len)
                 (reverse (cons (substring str start len) res))]
                [(char=? (string-ref str i) #\newline)
                 (loop (fx+ i 1) (fx+ i 1) (cons (substring str start i) res))]
                [else (loop (fx+ i 1) start res)])))))

  (define $rich-table-widths
    (lambda (columns rows)
      (map (lambda (column cells)
             (apply max (map string-length (cons column cells))))
           columns
           (let loop ([i 0] [columns columns])
             (if (null? columns)
                 '()
                 (cons (map (lambda (row) (list-ref row i)) rows)
                       (loop (fx+ i 1) (cdr columns))))))))

  (define $pad-right
    (lambda (str width)
      (let ([padding (fx- width (string-length str))])
        (if (fx> padding 0)
            (string-append str (make-string padding #\space))
            str))))

  (define $rich-table-border
    (lambda (widths)
      (string-append
       "+"
       ($string-join
        "+"
        (map (lambda (width) (make-string (fx+ width 2) #\-)) widths))
       "+")))

  (define $rich-table-row
    (lambda (cells widths)
      (string-append
       "| "
       ($string-join
        " | "
        (map (lambda (cell width) ($pad-right cell width)) cells widths))
       " |")))

  (define $rich-table-lines
    (lambda (table)
      (let ([columns (rich-table-columns table)] [rows (rich-table-rows table)])
        (if (null? columns)
            '()
            (let* ([widths ($rich-table-widths columns rows)]
                   [border ($rich-table-border widths)]
                   [header ($rich-table-row columns widths)])
              (if (null? rows)
                  (list border header border)
                  (append (list border header border)
                          (map (lambda (row) ($rich-table-row row widths)) rows)
                          (list border))))))))

  (define $rich-panel-border
    (lambda (who style)
      (case style
        [(ascii) '#("+" "+" "+" "+" "-" "|" #\-)]
        [(unicode) '#("┌" "┐" "└" "┘" "─" "│" #\─)]
        [else (errorf who "unknown panel border style: ~a" style)])))

  (define $rich-panel-top-border
    (lambda (width title border)
      (let ([tl (vector-ref border 0)]
            [tr (vector-ref border 1)]
            [h (vector-ref border 4)]
            [hchar (vector-ref border 6)]
            [inner-width (fx+ width 2)])
        (if title
            (let* ([prefix (string-append h " " title " " h)]
                   [fill (fx- inner-width (string-length prefix))])
              (string-append tl
                             prefix
                             (make-string (max 0 fill) hchar)
                             tr))
            (string-append tl (make-string inner-width hchar) tr)))))

  (define $rich-panel-bottom-border
    (lambda (width border)
      (let ([bl (vector-ref border 2)]
            [br (vector-ref border 3)]
            [hchar (vector-ref border 6)])
        (string-append bl (make-string (fx+ width 2) hchar) br))))

  (define $rich-panel-row
    (lambda (line width border)
      (let ([v (vector-ref border 5)])
        (string-append v " " ($pad-right line width) " " v))))

  (define $rich-panel-lines
    (lambda (panel)
      (let* ([body-lines ($string-split-lines (rich-panel-body panel))]
             [title (rich-panel-title panel)]
             [style (rich-panel-border-style panel)]
             [border ($rich-panel-border 'rich-panel-render style)]
             [width (apply max
                           (append (map string-length body-lines)
                                   (if title
                                       (list (fx+ (string-length title) 2))
                                       '())))])
        (append (list ($rich-panel-top-border width title border))
                (map (lambda (line) ($rich-panel-row line width border)) body-lines)
                (list ($rich-panel-bottom-border width border))))))

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

  #|proc:make-rich-table
The `make-rich-table` procedure constructs an empty rich table.
|#
  (define make-rich-table
    (lambda ()
      (mk-rich-table '() '())))

  #|proc:rich-table?
The `rich-table?` procedure checks whether `obj` is a rich table.
|#
  (define rich-table?
    (lambda (obj)
      ($rich-table? obj)))

  #|proc:rich-table-add-column!
The `rich-table-add-column!` procedure appends a column with the string
`title` to `table`.
|#
  (define rich-table-add-column!
    (lambda (table title)
      (pcheck ([$rich-table? table] [string? title])
              (unless (null? (rich-table-rows table))
                (errorf 'rich-table-add-column! "cannot add columns after rows"))
              (rich-table-columns-set!
               table
               (append (rich-table-columns table) (list title))))))

  #|proc:rich-table-add-row!
The `rich-table-add-row!` procedure appends a row of `cells` to `table`.
The number of cells must match the number of table columns.
|#
  (define-who rich-table-add-row!
    (lambda (table . cells)
      (pcheck ([$rich-table? table])
              (let ([columns (rich-table-columns table)])
                (unless (= (length cells) (length columns))
                  (errorf who "row has ~a cells, expected ~a"
                          (length cells)
                          (length columns)))
                (rich-table-rows-set!
                 table
                 (append (rich-table-rows table)
                         (list (map $rich-cell->string cells))))))))

  #|proc:rich-table-render
The `rich-table-render` procedure renders `table` as an ASCII table string.
|#
  (define rich-table-render
    (lambda (table)
      (pcheck ([$rich-table? table])
              ($string-join "\n" ($rich-table-lines table)))))

  #|proc:rich-table-print
The `rich-table-print` procedure writes the rendered table to the current
output port without appending a newline.
|#
  (define rich-table-print
    (lambda (table)
      (pcheck ([$rich-table? table])
              (display (rich-table-render table)))))

  #|proc:rich-table-println
The `rich-table-println` procedure writes the rendered table to the current
output port and appends a newline.
|#
  (define rich-table-println
    (lambda (table)
      (pcheck ([$rich-table? table])
              (display (rich-table-render table))
              (newline))))

  #|proc:rich-table-fprint
The `rich-table-fprint` procedure writes the rendered table to `port` without
appending a newline.
|#
  (define rich-table-fprint
    (lambda (port table)
      (pcheck ([output-port? port] [$rich-table? table])
              (display (rich-table-render table) port))))

  #|proc:rich-table-fprintln
The `rich-table-fprintln` procedure writes the rendered table to `port` and
appends a newline.
|#
  (define rich-table-fprintln
    (lambda (port table)
      (pcheck ([output-port? port] [$rich-table? table])
              (display (rich-table-render table) port)
              (newline port))))

  #|proc:rich-panel-border-style?
The `rich-panel-border-style?` procedure checks whether `obj` is a supported
panel border style symbol.
|#
  (define rich-panel-border-style?
    (lambda (obj)
      (and (memq obj '(ascii unicode)) #t)))

  #|proc:rich-panel
The `rich-panel` procedure constructs a panel around `body`. The optional
`title` is shown in the top border, and the optional `border-style` can be
`ascii` or `unicode`.
|#
  (define-who rich-panel
    (case-lambda
      [(body) (rich-panel body #f 'ascii)]
      [(body title) (rich-panel body title 'ascii)]
      [(body title border-style)
       (pcheck ([string? body] [symbol? border-style])
               (when (and title (not (string? title)))
                 (errorf who "expected title string or #f: ~a" title))
               (unless (rich-panel-border-style? border-style)
                 (errorf who "unknown panel border style: ~a" border-style))
               (mk-rich-panel body title border-style))]))

  #|proc:rich-panel?
The `rich-panel?` procedure checks whether `obj` is a rich panel.
|#
  (define rich-panel?
    (lambda (obj)
      ($rich-panel? obj)))

  #|proc:rich-panel-render
The `rich-panel-render` procedure renders `panel` as a bordered string.
|#
  (define rich-panel-render
    (lambda (panel)
      (pcheck ([$rich-panel? panel])
              ($string-join "\n" ($rich-panel-lines panel)))))

  #|proc:rich-panel-print
The `rich-panel-print` procedure writes the rendered panel to the current
output port without appending a newline.
|#
  (define rich-panel-print
    (lambda (panel)
      (pcheck ([$rich-panel? panel])
              (display (rich-panel-render panel)))))

  #|proc:rich-panel-println
The `rich-panel-println` procedure writes the rendered panel to the current
output port and appends a newline.
|#
  (define rich-panel-println
    (lambda (panel)
      (pcheck ([$rich-panel? panel])
              (display (rich-panel-render panel))
              (newline))))

  #|proc:rich-panel-fprint
The `rich-panel-fprint` procedure writes the rendered panel to `port` without
appending a newline.
|#
  (define rich-panel-fprint
    (lambda (port panel)
      (pcheck ([output-port? port] [$rich-panel? panel])
              (display (rich-panel-render panel) port))))

  #|proc:rich-panel-fprintln
The `rich-panel-fprintln` procedure writes the rendered panel to `port` and
appends a newline.
|#
  (define rich-panel-fprintln
    (lambda (port panel)
      (pcheck ([output-port? port] [$rich-panel? panel])
              (display (rich-panel-render panel) port)
              (newline port))))

  )
