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
          rich-panel-fprintln

          make-rich-progress
          rich-progress?
          rich-progress-task?
          rich-progress-add-task!
          rich-progress-remove-task!
          rich-progress-update!
          rich-progress-advance!
          rich-progress-complete!
          rich-progress-task-description-set!
          rich-progress-task-total-set!
          rich-progress-start-task!
          rich-progress-stop-task!
          rich-progress-task-visible?-set!
          rich-progress-render
          rich-progress-print
          rich-progress-println
          rich-progress-fprint
          rich-progress-fprintln
          rich-progress-refresh!
          rich-progress-frefresh!
          rich-progress-finish!
          rich-progress-ffinish!
          rich-progress-live?
          rich-progress-start!
          rich-progress-stop!
          rich-progress-column?
          rich-progress-default-columns
          rich-progress-text-column
          rich-progress-bar-column
          rich-progress-percent-column
          rich-progress-complete-column
          rich-progress-spinner-column
          rich-progress-elapsed-column
          rich-progress-remaining-column
          rich-progress-transfer-speed-column
          rich-progress-columns
          rich-progress-columns-set!
          rich-progress-current-time)
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

  (define-record-type ($rich-progress mk-rich-progress $rich-progress?)
    (fields (mutable next-id rich-progress-next-id rich-progress-next-id-set!)
            (mutable bar-width rich-progress-bar-width rich-progress-bar-width-set!)
            (mutable tasks rich-progress-tasks rich-progress-tasks-set!)
            (mutable columns rich-progress-columns rich-progress-columns-raw-set!)
            (mutable live? $rich-progress-live? $rich-progress-live?-set!)
            (mutable live-thread $rich-progress-live-thread $rich-progress-live-thread-set!)
            (mutable refresh-line-count
                     $rich-progress-refresh-line-count
                     $rich-progress-refresh-line-count-set!)))

  (define-record-type ($rich-progress-task mk-rich-progress-task $rich-progress-task?)
    (fields (immutable id rich-progress-task-id)
            (mutable description
                     rich-progress-task-description
                     rich-progress-task-description-raw-set!)
            (mutable total rich-progress-task-total rich-progress-task-total-raw-set!)
            (mutable completed rich-progress-task-completed rich-progress-task-completed-set!)
            (mutable visible? rich-progress-task-visible? rich-progress-task-visible?-raw-set!)
            (mutable start-time rich-progress-task-start-time rich-progress-task-start-time-set!)
            (mutable stop-time rich-progress-task-stop-time rich-progress-task-stop-time-set!)))

  (define-record-type ($rich-progress-column mk-rich-progress-column $rich-progress-column?)
    (fields (immutable kind rich-progress-column-kind)
            (immutable value rich-progress-column-value)))

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

  (define $current-time-seconds
    (lambda ()
      (let ([t (current-time)])
        (+ (time-second t) (/ (time-nanosecond t) 1000000000)))))

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

  (define $list-remove
    (lambda (pred ls)
      (let loop ([ls ls] [res '()])
        (cond [(null? ls) (reverse res)]
              [(pred (car ls)) (loop (cdr ls) res)]
              [else (loop (cdr ls) (cons (car ls) res))]))))

  (define $rich-progress-find-task
    (lambda (who progress task-id)
      (let loop ([tasks (rich-progress-tasks progress)])
        (cond [(null? tasks) (errorf who "unknown progress task id: ~a" task-id)]
              [(= task-id (rich-progress-task-id (car tasks))) (car tasks)]
              [else (loop (cdr tasks))]))))

  (define $rich-progress-check-total
    (lambda (who total)
      (unless (or (not total) (positive-natural? total))
        (errorf who "expected positive natural total or #f: ~a" total))))

  (define $rich-progress-check-current
    (lambda (who current total)
      (unless (natural? current)
        (errorf who "expected natural current: ~a" current))
      (when (and total (> current total))
        (errorf who "current exceeds total: ~a > ~a" current total))))

  (define $rich-progress-percent-string
    (lambda (completed total)
      (if total
          (format #f "~a%" (quotient (* completed 100) total))
          "--%")))

  (define $rich-progress-count-string
    (lambda (completed total)
      (if total
          (format #f "~a/~a" completed total)
          (format #f "~a/?" completed))))

  (define $rich-progress-total-string
    (lambda (total)
      (if total
          (format #f "~a" total)
          "?")))

  (define $rich-progress-default-spinner-frames
    '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧"))

  (define $positive-real?
    (lambda (obj)
      (and (real? obj) (finite? obj) (> obj 0))))

  (define $rich-progress-spinner-frames?
    (lambda (obj)
      (and (pair? obj)
           (list? obj)
           (andmap string? obj))))

  (define $rich-progress-bar-char?
    (lambda (obj)
      (and (string? obj)
           (= (string-length obj) 1))))

  (define $two-digits
    (lambda (n)
      (if (< n 10)
          (string-append "0" (number->string n))
          (number->string n))))

  (define $rich-progress-time-string
    (lambda (seconds)
      (let* ([seconds (floor seconds)]
             [minutes (quotient seconds 60)]
             [seconds (remainder seconds 60)])
        (string-append ($two-digits minutes) ":" ($two-digits seconds)))))

  (define $rich-progress-now
    (lambda (who)
      (let ([seconds ((rich-progress-current-time))])
        (unless (real? seconds)
          (errorf who "progress clock returned non-real value: ~a" seconds))
        seconds)))

  (define $rich-progress-sleep-ms
    (lambda (milliseconds)
      (if (fx>= milliseconds 1000)
          (let ([seconds (fx/ milliseconds 1000)]
                [nanoseconds (fx* 1000000 (fxmod milliseconds 1000))])
            ($sleep (make-time 'time-duration nanoseconds seconds)))
          ($sleep (make-time 'time-duration (fx* milliseconds 1000000) 0)))))

  (define $rich-progress-elapsed-seconds
    (lambda (task)
      (let ([stop (rich-progress-task-stop-time task)]
            [start (rich-progress-task-start-time task)])
        (max 0 (- (if stop stop ($rich-progress-now 'rich-progress-render)) start)))))

  (define $rich-progress-elapsed-string
    (lambda (task)
      ($rich-progress-time-string ($rich-progress-elapsed-seconds task))))

  (define $rich-progress-speed
    (lambda (task)
      (let ([elapsed ($rich-progress-elapsed-seconds task)])
        (and (> elapsed 0)
             (> (rich-progress-task-completed task) 0)
             (/ (rich-progress-task-completed task) elapsed)))))

  (define $rich-progress-remaining-string
    (lambda (task)
      (let ([total (rich-progress-task-total task)]
            [completed (rich-progress-task-completed task)]
            [speed ($rich-progress-speed task)])
        (if (and total speed (> speed 0) (< completed total))
            ($rich-progress-time-string
             (ceiling (/ (- total completed) speed)))
            "--:--"))))

  (define $rich-progress-speed-string
    (lambda (task)
      (let ([speed ($rich-progress-speed task)])
        (if speed
            (format #f "~a/s" (floor speed))
            "--/s"))))

  (define $rich-progress-stop-if-complete!
    (lambda (who task completed total)
      (when (and total
                 (= completed total)
                 (not (rich-progress-task-stop-time task)))
        (rich-progress-task-stop-time-set! task ($rich-progress-now who)))))

  (define $rich-progress-spinner-string
    (lambda (task column)
      (let* ([spec (rich-progress-column-value column)]
             [frames (vector-ref spec 0)]
             [interval (vector-ref spec 1)]
             [frame-count (length frames)]
             [index (modulo
                     (inexact->exact
                      (floor (+ (/ ($rich-progress-elapsed-seconds task) interval)
                                1e-9)))
                     frame-count)])
        (list-ref frames index))))

  (define $rich-progress-pulse-width
    (lambda (width)
      (min 3 width)))

  (define $rich-progress-pulse-bar
    (lambda (task width fill empty pulse)
      (let* ([pulse-width ($rich-progress-pulse-width width)]
             [max-start (- width pulse-width)]
             [start (if (= max-start 0)
                        0
                        (modulo
                         (inexact->exact
                          (floor (+ (/ ($rich-progress-elapsed-seconds task) 0.1)
                                    1e-9)))
                         (+ max-start 1)))]
             [end (+ start pulse-width)])
        (let loop ([i 0] [parts '()])
          (if (= i width)
              (string-append "[" (apply string-append (reverse parts)) "]")
              (loop (+ i 1)
                    (cons (if (and (>= i start) (< i end)) pulse empty)
                          parts)))))))

  (define $rich-progress-bar
    (lambda (task column completed total width)
      (let* ([spec (rich-progress-column-value column)]
             [fill (vector-ref spec 0)]
             [empty (vector-ref spec 1)]
             [pulse (vector-ref spec 2)])
        (if total
            (let ([filled (quotient (* completed width) total)])
              (string-append
               "["
               (apply string-append (make-list filled fill))
               (apply string-append (make-list (- width filled) empty))
               "]"))
            ($rich-progress-pulse-bar task width fill empty pulse)))))

  (define $rich-progress-text-placeholder
    (lambda (who placeholder task template)
      (cond [(string=? placeholder "{description}")
             (rich-progress-task-description task)]
            [(string=? placeholder "{completed}")
             (format #f "~a" (rich-progress-task-completed task))]
            [(string=? placeholder "{total}")
             ($rich-progress-total-string (rich-progress-task-total task))]
            [(string=? placeholder "{percent}")
             ($rich-progress-percent-string
              (rich-progress-task-completed task)
              (rich-progress-task-total task))]
            [else (errorf who "unsupported progress text template: ~a" template)])))

  (define $rich-progress-find-placeholder-end
    (lambda (template start)
      (let ([len (string-length template)])
        (let loop ([i start])
          (cond [(= i len) #f]
                [(char=? (string-ref template i) #\}) i]
                [else (loop (+ i 1))])))))

  (define $rich-progress-text-template
    (lambda (who template task)
      (let ([len (string-length template)])
        (let loop ([i 0] [start 0] [parts '()])
          (cond
           [(= i len)
            (apply string-append
                   (reverse (cons (substring template start len) parts)))]
           [(char=? (string-ref template i) #\{)
            (let ([end ($rich-progress-find-placeholder-end template (+ i 1))])
              (unless end
                (errorf who "unsupported progress text template: ~a" template))
              (loop
               (+ end 1)
               (+ end 1)
               (cons ($rich-progress-text-placeholder
                      who
                      (substring template i (+ end 1))
                      task
                      template)
                     (cons (substring template start i) parts))))]
           [(char=? (string-ref template i) #\})
            (errorf who "unsupported progress text template: ~a" template)]
           [else (loop (+ i 1) start parts)])))))

  (define $rich-progress-column-string
    (lambda (progress task column)
      (let ([completed (rich-progress-task-completed task)]
            [total (rich-progress-task-total task)]
            [width (rich-progress-bar-width progress)])
        (case (rich-progress-column-kind column)
          [(text) ($rich-progress-text-template
                   'rich-progress-render
                   (rich-progress-column-value column)
                   task)]
          [(bar) ($rich-progress-bar task column completed total width)]
          [(percent) ($rich-progress-percent-string completed total)]
          [(complete) ($rich-progress-count-string completed total)]
          [(spinner) ($rich-progress-spinner-string task column)]
          [(elapsed) ($rich-progress-elapsed-string task)]
          [(remaining) ($rich-progress-remaining-string task)]
          [(transfer-speed) ($rich-progress-speed-string task)]
          [else (errorf 'rich-progress-render
                        "unknown progress column kind: ~a"
                        (rich-progress-column-kind column))]))))

  (define $rich-progress-task-line
    (lambda (progress task)
      ($string-join
       " "
       (map (lambda (column) ($rich-progress-column-string progress task column))
            (rich-progress-columns progress)))))

  (define $rich-progress-live-loop
    (lambda (progress port interval-ms)
      (let loop ()
        (when ($rich-progress-live? progress)
          (rich-progress-frefresh! port progress)
          (flush-output-port port)
          ($rich-progress-sleep-ms interval-ms)
          (loop)))))

  (define $rich-progress-render-line-count
    (lambda (rendered)
      (length ($string-split-lines rendered))))

  (define $rich-progress-clear-lines
    (lambda (port count)
      (let loop ([count count])
        (when (> count 0)
          (display "\r\033[2K" port)
          (when (> count 1)
            (display "\033[1A" port))
          (loop (- count 1))))))

  (define $rich-progress-visible-tasks
    (lambda (progress)
      (let loop ([tasks (rich-progress-tasks progress)] [res '()])
        (cond [(null? tasks) (reverse res)]
              [(rich-progress-task-visible? (car tasks))
               (loop (cdr tasks) (cons (car tasks) res))]
              [else (loop (cdr tasks) res)]))))

  (define $rich-progress-column-list?
    (lambda (columns)
      (and (pair? columns)
           (andmap $rich-progress-column? columns))))

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

  #|proc:rich-progress-current-time
The `rich-progress-current-time` parameter contains a nullary procedure that
returns the current progress time in seconds.
|#
  (define rich-progress-current-time
    (make-parameter
     $current-time-seconds
     (lambda (proc)
       (if (procedure? proc)
           proc
           (errorf 'rich-progress-current-time "expected procedure: ~a" proc)))))

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

  #|proc:make-rich-progress
The `make-rich-progress` procedure constructs a progress manager. The optional
`bar-width` controls the number of characters in rendered bars.
|#
  (define-who make-rich-progress
    (case-lambda
      [() (make-rich-progress 40)]
      [(bar-width)
       (pcheck ([positive-natural? bar-width])
               (mk-rich-progress
                0 bar-width '() (rich-progress-default-columns) #f #f 1))]))

  #|proc:rich-progress?
The `rich-progress?` procedure checks whether `obj` is a rich progress manager.
|#
  (define rich-progress?
    (lambda (obj)
      ($rich-progress? obj)))

  #|proc:rich-progress-task?
The `rich-progress-task?` procedure checks whether `obj` is a progress task.
|#
  (define rich-progress-task?
    (lambda (obj)
      ($rich-progress-task? obj)))

  #|proc:rich-progress-live?
The `rich-progress-live?` procedure checks whether `progress` has an
auto-refresh thread running.
|#
  (define rich-progress-live?
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              ($rich-progress-live? progress))))

  #|proc:rich-progress-column?
The `rich-progress-column?` procedure checks whether `obj` is a progress
rendering column.
|#
  (define rich-progress-column?
    (lambda (obj)
      ($rich-progress-column? obj)))

  #|proc:rich-progress-text-column
The `rich-progress-text-column` procedure constructs a text column from a
template string. The template may contain literal text and these placeholders:
`{description}`, `{completed}`, `{total}`, and `{percent}`.
|#
  (define-who rich-progress-text-column
    (lambda (template)
      (pcheck ([string? template])
              ;; validate eagerly so invalid templates fail at construction.
              ($rich-progress-text-template
               who
               template
               (mk-rich-progress-task 0 "" 1 0 #t 0 #f))
              (mk-rich-progress-column 'text template))))

  #|proc:rich-progress-bar-column
The `rich-progress-bar-column` procedure constructs a progress bar column.
With no arguments it uses `#` for filled cells and `-` for empty cells. With
two arguments it uses `fill` and `empty` for determinate bars. With three
arguments, `pulse` controls the animated cell for indeterminate bars. Each
character argument must be a one-character string.
|#
  (define rich-progress-bar-column
    (case-lambda
      [()
       (rich-progress-bar-column "#" "-" "#")]
      [(fill empty)
       (rich-progress-bar-column fill empty fill)]
      [(fill empty pulse)
       (pcheck ([$rich-progress-bar-char? fill empty pulse])
               (mk-rich-progress-column 'bar (vector fill empty pulse)))]))

  #|proc:rich-progress-percent-column
The `rich-progress-percent-column` procedure constructs a percent column.
|#
  (define rich-progress-percent-column
    (lambda ()
      (mk-rich-progress-column 'percent #f)))

  #|proc:rich-progress-complete-column
The `rich-progress-complete-column` procedure constructs a completed/total
count column.
|#
  (define rich-progress-complete-column
    (lambda ()
      (mk-rich-progress-column 'complete #f)))

  #|proc:rich-progress-spinner-column
The `rich-progress-spinner-column` procedure constructs an animated spinner
column. With no arguments it uses a Rich-like dots spinner. With one argument,
`frames` must be a non-empty list of strings and uses the default interval.
With two arguments, `interval` is the number of seconds per frame.
|#
  (define rich-progress-spinner-column
    (case-lambda
      [()
       (rich-progress-spinner-column $rich-progress-default-spinner-frames 0.1)]
      [(frames)
       (rich-progress-spinner-column frames 0.1)]
      [(frames interval)
       (pcheck ([$rich-progress-spinner-frames? frames] [$positive-real? interval])
               (mk-rich-progress-column 'spinner (vector frames interval)))]))

  #|proc:rich-progress-elapsed-column
The `rich-progress-elapsed-column` procedure constructs an elapsed-time column.
|#
  (define rich-progress-elapsed-column
    (lambda ()
      (mk-rich-progress-column 'elapsed #f)))

  #|proc:rich-progress-remaining-column
The `rich-progress-remaining-column` procedure constructs an estimated remaining
time column.
|#
  (define rich-progress-remaining-column
    (lambda ()
      (mk-rich-progress-column 'remaining #f)))

  #|proc:rich-progress-transfer-speed-column
The `rich-progress-transfer-speed-column` procedure constructs a transfer-speed
column.
|#
  (define rich-progress-transfer-speed-column
    (lambda ()
      (mk-rich-progress-column 'transfer-speed #f)))

  #|proc:rich-progress-default-columns
The `rich-progress-default-columns` procedure returns the default progress
columns: description, bar, percent, and completed/total count.
|#
  (define rich-progress-default-columns
    (lambda ()
      (list (rich-progress-text-column "{description}")
            (rich-progress-bar-column)
            (rich-progress-percent-column)
            (rich-progress-complete-column))))

  #|proc:rich-progress-columns-set!
The `rich-progress-columns-set!` procedure replaces the progress render columns.
`columns` must be a non-empty list of progress columns.
|#
  (define rich-progress-columns-set!
    (lambda (progress columns)
      (pcheck ([$rich-progress? progress] [$rich-progress-column-list? columns])
              (rich-progress-columns-raw-set! progress columns))))

  #|proc:rich-progress-add-task!
The `rich-progress-add-task!` procedure adds a task to `progress` and returns
the task id. `total` is either a positive natural number or `#f` for an
indeterminate task.
|#
  (define-who rich-progress-add-task!
    (lambda (progress description total)
      (pcheck ([$rich-progress? progress] [string? description])
              ($rich-progress-check-total who total)
              (let ([id (rich-progress-next-id progress)])
                (rich-progress-next-id-set! progress (fx+ id 1))
                (rich-progress-tasks-set!
                 progress
                 (append (rich-progress-tasks progress)
                         (list (mk-rich-progress-task
                                id description total 0 #t
                                ($rich-progress-now who)
                                #f))))
                id))))

  #|proc:rich-progress-remove-task!
The `rich-progress-remove-task!` procedure removes the task identified by
`task-id` from `progress`.
|#
  (define-who rich-progress-remove-task!
    (lambda (progress task-id)
      (pcheck ([$rich-progress? progress] [natural? task-id])
              ($rich-progress-find-task who progress task-id)
              (rich-progress-tasks-set!
               progress
               ($list-remove
                (lambda (task) (= task-id (rich-progress-task-id task)))
                (rich-progress-tasks progress)))
              #t)))

  #|proc:rich-progress-update!
The `rich-progress-update!` procedure sets the completed amount for a task.
|#
  (define-who rich-progress-update!
    (lambda (progress task-id completed)
      (pcheck ([$rich-progress? progress] [natural? task-id completed])
              (let ([task ($rich-progress-find-task who progress task-id)])
                ($rich-progress-check-current who completed (rich-progress-task-total task))
                (rich-progress-task-completed-set! task completed)
                ($rich-progress-stop-if-complete!
                 who
                 task
                 completed
                 (rich-progress-task-total task))
                #t))))

  #|proc:rich-progress-advance!
The `rich-progress-advance!` procedure increments a task's completed amount by
`amount`.
|#
  (define-who rich-progress-advance!
    (lambda (progress task-id amount)
      (pcheck ([$rich-progress? progress] [natural? task-id amount])
              (let* ([task ($rich-progress-find-task who progress task-id)]
                     [completed (+ (rich-progress-task-completed task) amount)])
                ($rich-progress-check-current who completed (rich-progress-task-total task))
                (rich-progress-task-completed-set! task completed)
                ($rich-progress-stop-if-complete!
                 who
                 task
                 completed
                 (rich-progress-task-total task))
                #t))))

  #|proc:rich-progress-complete!
The `rich-progress-complete!` procedure marks a determinate task complete.
|#
  (define-who rich-progress-complete!
    (lambda (progress task-id)
      (pcheck ([$rich-progress? progress] [natural? task-id])
              (let* ([task ($rich-progress-find-task who progress task-id)]
                     [total (rich-progress-task-total task)])
                (unless total
                  (errorf who "cannot complete indeterminate task: ~a" task-id))
                (rich-progress-task-completed-set! task total)
                ($rich-progress-stop-if-complete! who task total total)
                #t))))

  #|proc:rich-progress-task-description-set!
The `rich-progress-task-description-set!` procedure changes the description for
the task identified by `task-id` in `progress`.
|#
  (define-who rich-progress-task-description-set!
    (lambda (progress task-id description)
      (pcheck ([$rich-progress? progress] [natural? task-id] [string? description])
              (rich-progress-task-description-raw-set!
               ($rich-progress-find-task who progress task-id)
               description)
              #t)))

  #|proc:rich-progress-task-total-set!
The `rich-progress-task-total-set!` procedure changes the total for the task
identified by `task-id` in `progress`. `total` is either a positive natural
number or `#f` for an indeterminate task. The current completed amount must not
exceed a determinate total.
|#
  (define-who rich-progress-task-total-set!
    (lambda (progress task-id total)
      (pcheck ([$rich-progress? progress] [natural? task-id])
              ($rich-progress-check-total who total)
              (let ([task ($rich-progress-find-task who progress task-id)])
                ($rich-progress-check-current
                 who
                 (rich-progress-task-completed task)
                 total)
                (rich-progress-task-total-raw-set! task total)
                ($rich-progress-stop-if-complete!
                 who
                 task
                 (rich-progress-task-completed task)
                 total)
                #t))))

  #|proc:rich-progress-start-task!
The `rich-progress-start-task!` procedure starts or restarts timing for a task.
|#
  (define-who rich-progress-start-task!
    (lambda (progress task-id)
      (pcheck ([$rich-progress? progress] [natural? task-id])
              (let ([task ($rich-progress-find-task who progress task-id)])
                (rich-progress-task-start-time-set! task ($rich-progress-now who))
                (rich-progress-task-stop-time-set! task #f)
                #t))))

  #|proc:rich-progress-stop-task!
The `rich-progress-stop-task!` procedure stops timing for a task.
|#
  (define-who rich-progress-stop-task!
    (lambda (progress task-id)
      (pcheck ([$rich-progress? progress] [natural? task-id])
              (let ([task ($rich-progress-find-task who progress task-id)])
                (rich-progress-task-stop-time-set! task ($rich-progress-now who))
                #t))))

  #|proc:rich-progress-task-visible?-set!
The `rich-progress-task-visible?-set!` procedure controls whether a task is
included in rendered progress output.
|#
  (define-who rich-progress-task-visible?-set!
    (lambda (progress task-id visible?)
      (pcheck ([$rich-progress? progress] [natural? task-id] [boolean? visible?])
              (rich-progress-task-visible?-raw-set!
               ($rich-progress-find-task who progress task-id)
               visible?)
              #t)))

  #|proc:rich-progress-render
The `rich-progress-render` procedure renders visible progress tasks as a
newline-separated string.
|#
  (define rich-progress-render
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              ($string-join
               "\n"
               (map (lambda (task) ($rich-progress-task-line progress task))
                    ($rich-progress-visible-tasks progress))))))

  #|proc:rich-progress-print
The `rich-progress-print` procedure writes rendered progress to the current
output port without appending a newline.
|#
  (define rich-progress-print
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              (display (rich-progress-render progress)))))

  #|proc:rich-progress-println
The `rich-progress-println` procedure writes rendered progress to the current
output port and appends a newline.
|#
  (define rich-progress-println
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              (display (rich-progress-render progress))
              (newline))))

  #|proc:rich-progress-fprint
The `rich-progress-fprint` procedure writes rendered progress to `port`
without appending a newline.
|#
  (define rich-progress-fprint
    (lambda (port progress)
      (pcheck ([output-port? port] [$rich-progress? progress])
              (display (rich-progress-render progress) port))))

  #|proc:rich-progress-fprintln
The `rich-progress-fprintln` procedure writes rendered progress to `port` and
appends a newline.
|#
  (define rich-progress-fprintln
    (lambda (port progress)
      (pcheck ([output-port? port] [$rich-progress? progress])
              (display (rich-progress-render progress) port)
              (newline port))))

  #|proc:rich-progress-refresh!
The `rich-progress-refresh!` procedure rewrites the current terminal line with
the rendered progress output.
|#
  (define rich-progress-refresh!
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              (rich-progress-frefresh! (current-output-port) progress))))

  #|proc:rich-progress-frefresh!
The `rich-progress-frefresh!` procedure clears the previous live progress
render from `port` and writes the current rendered progress output.
|#
  (define rich-progress-frefresh!
    (lambda (port progress)
      (pcheck ([output-port? port] [$rich-progress? progress])
              (let ([rendered (rich-progress-render progress)])
                ($rich-progress-clear-lines
                 port
                 ($rich-progress-refresh-line-count progress))
                (display rendered port)
                ($rich-progress-refresh-line-count-set!
                 progress
                 ($rich-progress-render-line-count rendered))))))

  #|proc:rich-progress-finish!
The `rich-progress-finish!` procedure writes the final rendered progress output
to the current output port and appends a newline.
|#
  (define rich-progress-finish!
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              (rich-progress-ffinish! (current-output-port) progress))))

  #|proc:rich-progress-ffinish!
The `rich-progress-ffinish!` procedure writes the final rendered progress output
to `port` and appends a newline.
|#
  (define rich-progress-ffinish!
    (lambda (port progress)
      (pcheck ([output-port? port] [$rich-progress? progress])
              (display (rich-progress-render progress) port)
              (newline port))))

  #|proc:rich-progress-start!
The `rich-progress-start!` procedure starts automatic live refreshing for
`progress`. With one argument it writes to the current output port every 100
milliseconds. With two arguments it writes to `port`. With three arguments,
`interval-ms` controls the refresh interval in milliseconds.
|#
  (define-who rich-progress-start!
    (case-lambda
      [(progress)
       (rich-progress-start! (current-output-port) progress 100)]
      [(port progress)
       (rich-progress-start! port progress 100)]
      [(port progress interval-ms)
       (pcheck ([output-port? port] [$rich-progress? progress] [positive-natural? interval-ms])
               (when ($rich-progress-live? progress)
                 (errorf who "progress is already live"))
               ($rich-progress-live?-set! progress #t)
               ($rich-progress-live-thread-set!
                progress
                (fork-thread
                 (lambda ()
                   ($rich-progress-live-loop progress port interval-ms))))
               #t)]))

  #|proc:rich-progress-stop!
The `rich-progress-stop!` procedure stops automatic live refreshing for
`progress` and joins the background refresh thread. It returns `#t` when a live
thread was stopped and `#f` when `progress` was not live.
|#
  (define rich-progress-stop!
    (lambda (progress)
      (pcheck ([$rich-progress? progress])
              (if ($rich-progress-live? progress)
                  (let ([thread ($rich-progress-live-thread progress)])
                    ($rich-progress-live?-set! progress #f)
                    ($rich-progress-live-thread-set! progress #f)
                    (when thread (thread-join thread))
                    #t)
                  #f))))

  )
