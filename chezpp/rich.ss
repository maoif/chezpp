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
          rich-table-border-style?
          rich-table-border-style-set!
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

          make-rich-tree
          rich-tree?
          rich-tree-guide-style?
          rich-tree-add!
          rich-tree-render
          rich-tree-print
          rich-tree-println
          rich-tree-fprint
          rich-tree-fprintln

          rich-rule
          rich-rule?
          rich-rule-style?
          rich-rule-render
          rich-rule-print
          rich-rule-println
          rich-rule-fprint
          rich-rule-fprintln

          rich-align
          rich-align?
          rich-align-style?
          rich-align-render
          rich-align-print
          rich-align-println
          rich-align-fprint
          rich-align-fprintln

          rich-padding
          rich-padding?
          rich-padding-render
          rich-padding-print
          rich-padding-println
          rich-padding-fprint
          rich-padding-fprintln

          rich-columns
          rich-columns?
          rich-columns-render
          rich-columns-print
          rich-columns-println
          rich-columns-fprint
          rich-columns-fprintln

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
            (mutable rows rich-table-rows rich-table-rows-set!)
            (mutable border-style rich-table-border-style rich-table-border-style-raw-set!)))

  (define-record-type ($rich-panel mk-rich-panel $rich-panel?)
    (fields (immutable body rich-panel-body)
            (immutable title rich-panel-title)
            (immutable border-style rich-panel-border-style)))

  (define-record-type ($rich-tree mk-rich-tree $rich-tree?)
    (fields (immutable label rich-tree-label)
            (mutable children rich-tree-children rich-tree-children-set!)
            (immutable guide-style rich-tree-guide-style)))

  (define-record-type ($rich-rule mk-rich-rule $rich-rule?)
    (fields (immutable title rich-rule-title)
            (immutable width rich-rule-width)
            (immutable style rich-rule-style)))

  (define-record-type ($rich-align mk-rich-align $rich-align?)
    (fields (immutable body rich-align-body)
            (immutable width rich-align-width)
            (immutable style rich-align-style)))

  (define-record-type ($rich-padding mk-rich-padding $rich-padding?)
    (fields (immutable body rich-padding-body)
            (immutable top rich-padding-top)
            (immutable right rich-padding-right)
            (immutable bottom rich-padding-bottom)
            (immutable left rich-padding-left)))

  (define-record-type ($rich-columns mk-rich-columns $rich-columns?)
    (fields (immutable items rich-columns-items)
            (immutable total-width rich-columns-total-width)
            (immutable gap rich-columns-gap)))

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

  (define $rich-border-style?
    (lambda (style)
      (and (memq style '(ascii unicode)) #t)))

  (define $rich-tree-guide-style?
    (lambda (style)
      (and (memq style '(ascii unicode)) #t)))

  (define $rich-rule-style?
    (lambda (style)
      (and (memq style '(ascii unicode)) #t)))

  (define $rich-align-style?
    (lambda (style)
      (and (memq style '(left center right)) #t)))

  (define $rich-string-list?
    (lambda (obj)
      (and (list? obj)
           (andmap string? obj))))

  (define $rich-table-border-chars
    (lambda (who style)
      (case style
        [(ascii) '#("+" "+" "+" "+" "+" "+" "-" "|")]
        [(unicode) '#("┌" "┬" "┐" "├" "┼" "┤" "─" "│"
                      "└" "┴" "┘")]
        [else (errorf who "unknown table border style: ~a" style)])))

  (define $rich-table-border-line
    (lambda (widths left middle right hchar)
      (string-append
       left
       ($string-join
        middle
        (map (lambda (width) (make-string (fx+ width 2) hchar)) widths))
       right)))

  (define $rich-table-top-border
    (lambda (widths chars)
      ($rich-table-border-line
       widths
       (vector-ref chars 0)
       (vector-ref chars 1)
       (vector-ref chars 2)
       (string-ref (vector-ref chars 6) 0))))

  (define $rich-table-middle-border
    (lambda (widths chars)
      ($rich-table-border-line
       widths
       (vector-ref chars 3)
       (vector-ref chars 4)
       (vector-ref chars 5)
       (string-ref (vector-ref chars 6) 0))))

  (define $rich-table-bottom-border
    (lambda (widths chars)
      (if (= (vector-length chars) 8)
          ($rich-table-top-border widths chars)
          ($rich-table-border-line
           widths
           (vector-ref chars 8)
           (vector-ref chars 9)
           (vector-ref chars 10)
           (string-ref (vector-ref chars 6) 0)))))

  (define $rich-table-row
    (lambda (cells widths chars)
      (let ([v (vector-ref chars 7)])
        (string-append
         v " "
         ($string-join
          (string-append " " v " ")
          (map (lambda (cell width) ($pad-right cell width)) cells widths))
         " " v))))

  (define $rich-table-lines
    (lambda (table)
      (let ([columns (rich-table-columns table)] [rows (rich-table-rows table)])
        (if (null? columns)
            '()
            (let* ([widths ($rich-table-widths columns rows)]
                   [chars ($rich-table-border-chars
                           'rich-table-render
                           (rich-table-border-style table))]
                   [top-border ($rich-table-top-border widths chars)]
                   [middle-border ($rich-table-middle-border widths chars)]
                   [bottom-border ($rich-table-bottom-border widths chars)]
                   [header ($rich-table-row columns widths chars)])
              (if (null? rows)
                  (list top-border header bottom-border)
                  (append (list top-border header middle-border)
                          (map (lambda (row) ($rich-table-row row widths chars)) rows)
                          (list bottom-border))))))))

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

  (define $rich-tree-guide-chars
    (lambda (who style)
      (case style
        [(ascii) '#("|-- " "`-- " "|   " "    ")]
        [(unicode) '#("├── " "└── " "│   " "    ")]
        [else (errorf who "unknown tree guide style: ~a" style)])))

  (define $rich-tree-child-lines
    (lambda (tree prefix last? chars)
      (let* ([connector (vector-ref chars (if last? 1 0))]
             [continuation (vector-ref chars (if last? 3 2))]
             [line (string-append prefix connector (rich-tree-label tree))]
             [child-prefix (string-append prefix continuation)])
        (cons line ($rich-tree-children-lines
                    (rich-tree-children tree)
                    child-prefix
                    chars)))))

  (define $rich-tree-children-lines
    (lambda (children prefix chars)
      (let loop ([children children] [res '()])
        (if (null? children)
            (reverse res)
            (loop (cdr children)
                  (append (reverse ($rich-tree-child-lines
                                    (car children)
                                    prefix
                                    (null? (cdr children))
                                    chars))
                          res))))))

  (define $rich-tree-lines
    (lambda (tree)
      (let ([chars ($rich-tree-guide-chars
                    'rich-tree-render
                    (rich-tree-guide-style tree))])
        (cons (rich-tree-label tree)
              ($rich-tree-children-lines (rich-tree-children tree) "" chars)))))

  (define $rich-rule-char
    (lambda (who style)
      (case style
        [(ascii) #\-]
        [(unicode) #\─]
        [else (errorf who "unknown rule style: ~a" style)])))

  (define $rich-rule-line
    (lambda (rule)
      (let* ([title (rich-rule-title rule)]
             [width (rich-rule-width rule)]
             [ch ($rich-rule-char 'rich-rule-render (rich-rule-style rule))])
        (if title
            (let ([title-width (string-length title)])
              (if (>= title-width width)
                  title
                  (let* ([decorated (string-append " " title " ")]
                         [decorated-width (string-length decorated)])
                    (if (>= decorated-width width)
                        title
                        (let* ([remaining (fx- width decorated-width)]
                               [left (quotient remaining 2)]
                               [right (fx- remaining left)])
                          (string-append (make-string left ch)
                                         decorated
                                         (make-string right ch)))))))
            (make-string width ch)))))

  (define $rich-lines-max-width
    (lambda (lines)
      (apply max (map string-length lines))))

  (define $rich-align-line
    (lambda (line width style)
      (let* ([line-width (string-length line)]
             [padding (fx- width line-width)])
        (if (fx<= padding 0)
            line
            (case style
              [(left) (string-append line (make-string padding #\space))]
              [(right) (string-append (make-string padding #\space) line)]
              [(center)
               (let* ([left (quotient padding 2)]
                      [right (fx- padding left)])
                 (string-append (make-string left #\space)
                                line
                                (make-string right #\space)))]
              [else (errorf 'rich-align-render
                            "unknown align style: ~a"
                            style)])))))

  (define $rich-align-lines
    (lambda (align)
      (let ([width (rich-align-width align)]
            [style (rich-align-style align)])
        (map (lambda (line) ($rich-align-line line width style))
             ($string-split-lines (rich-align-body align))))))

  (define $rich-padding-lines
    (lambda (padding)
      (let* ([body-lines ($string-split-lines (rich-padding-body padding))]
             [inner-width ($rich-lines-max-width body-lines)]
             [top (rich-padding-top padding)]
             [right (rich-padding-right padding)]
             [bottom (rich-padding-bottom padding)]
             [left (rich-padding-left padding)]
             [total-width (fx+ inner-width left right)]
             [blank (make-string total-width #\space)]
             [left-spaces (make-string left #\space)]
             [right-spaces (make-string right #\space)])
        (append
         (make-list top blank)
         (map (lambda (line)
                (string-append left-spaces
                               ($pad-right line inner-width)
                               right-spaces))
              body-lines)
         (make-list bottom blank)))))

  (define $rich-columns-column-count
    (lambda (items total-width gap)
      (if (null? items)
          1
          (let* ([column-width ($rich-lines-max-width items)]
                 [stride (+ column-width gap)])
            (max 1 (quotient (+ total-width gap) stride))))))

  (define $rich-columns-row
    (lambda (cells column-width gap)
      (let ([gap-spaces (make-string gap #\space)])
        (let loop ([cells cells] [parts '()])
          (cond [(null? cells)
                 (apply string-append (reverse parts))]
                [(null? (cdr cells))
                 (loop '() (cons (car cells) parts))]
                [else
                 (loop (cdr cells)
                       (cons (string-append
                              ($pad-right (car cells) column-width)
                              gap-spaces)
                             parts))])))))

  (define $rich-columns-lines
    (lambda (columns)
      (let ([items (rich-columns-items columns)])
        (if (null? items)
            '()
            (let* ([column-width ($rich-lines-max-width items)]
                   [column-count ($rich-columns-column-count
                                  items
                                  (rich-columns-total-width columns)
                                  (rich-columns-gap columns))]
                   [gap (rich-columns-gap columns)])
              (let loop ([items items] [cells '()] [count 0] [rows '()])
                (cond [(null? items)
                       (reverse
                        (if (null? cells)
                            rows
                            (cons ($rich-columns-row
                                   (reverse cells)
                                   column-width
                                   gap)
                                  rows)))]
                      [(= count column-count)
                       (loop items
                             '()
                             0
                             (cons ($rich-columns-row
                                    (reverse cells)
                                    column-width
                                    gap)
                                   rows))]
                      [else
                       (loop (cdr items)
                             (cons (car items) cells)
                             (+ count 1)
                             rows)])))))))

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
      (mk-rich-table '() '() 'ascii)))

  #|proc:rich-table?
The `rich-table?` procedure checks whether `obj` is a rich table.
|#
  (define rich-table?
    (lambda (obj)
      ($rich-table? obj)))

  #|proc:rich-table-border-style?
The `rich-table-border-style?` procedure checks whether `obj` is a supported
table border style. Supported styles are `ascii` and `unicode`.
|#
  (define rich-table-border-style?
    (lambda (obj)
      ($rich-border-style? obj)))

  #|proc:rich-table-border-style-set!
The `rich-table-border-style-set!` procedure sets the border style for `table`.
Supported styles are `ascii` and `unicode`.
|#
  (define rich-table-border-style-set!
    (lambda (table style)
      (pcheck ([$rich-table? table] [$rich-border-style? style])
              (rich-table-border-style-raw-set! table style)
              #t)))

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

  #|proc:make-rich-tree
The `make-rich-tree` procedure constructs a tree with root `label`. With one
argument it uses Unicode guide characters. With two arguments, `guide-style`
may be `unicode` or `ascii`.
|#
  (define-who make-rich-tree
    (case-lambda
      [(label) (make-rich-tree label 'unicode)]
      [(label guide-style)
       (pcheck ([string? label] [$rich-tree-guide-style? guide-style])
               (mk-rich-tree label '() guide-style))]))

  #|proc:rich-tree?
The `rich-tree?` procedure checks whether `obj` is a rich tree.
|#
  (define rich-tree?
    (lambda (obj)
      ($rich-tree? obj)))

  #|proc:rich-tree-guide-style?
The `rich-tree-guide-style?` procedure checks whether `obj` is a supported
tree guide style. Supported styles are `unicode` and `ascii`.
|#
  (define rich-tree-guide-style?
    (lambda (obj)
      ($rich-tree-guide-style? obj)))

  #|proc:rich-tree-add!
The `rich-tree-add!` procedure appends a child with `label` to `tree` and
returns the new child tree.
|#
  (define rich-tree-add!
    (lambda (tree label)
      (pcheck ([$rich-tree? tree] [string? label])
              (let ([child (mk-rich-tree label '() (rich-tree-guide-style tree))])
                (rich-tree-children-set!
                 tree
                 (append (rich-tree-children tree) (list child)))
                child))))

  #|proc:rich-tree-render
The `rich-tree-render` procedure renders `tree` as a newline-separated string
using the tree's guide style.
|#
  (define rich-tree-render
    (lambda (tree)
      (pcheck ([$rich-tree? tree])
              ($string-join "\n" ($rich-tree-lines tree)))))

  #|proc:rich-tree-print
The `rich-tree-print` procedure writes the rendered tree to the current output
port without appending a newline.
|#
  (define rich-tree-print
    (lambda (tree)
      (pcheck ([$rich-tree? tree])
              (display (rich-tree-render tree)))))

  #|proc:rich-tree-println
The `rich-tree-println` procedure writes the rendered tree to the current
output port and appends a newline.
|#
  (define rich-tree-println
    (lambda (tree)
      (pcheck ([$rich-tree? tree])
              (display (rich-tree-render tree))
              (newline))))

  #|proc:rich-tree-fprint
The `rich-tree-fprint` procedure writes the rendered tree to `port` without
appending a newline.
|#
  (define rich-tree-fprint
    (lambda (port tree)
      (pcheck ([output-port? port] [$rich-tree? tree])
              (display (rich-tree-render tree) port))))

  #|proc:rich-tree-fprintln
The `rich-tree-fprintln` procedure writes the rendered tree to `port` and
appends a newline.
|#
  (define rich-tree-fprintln
    (lambda (port tree)
      (pcheck ([output-port? port] [$rich-tree? tree])
              (display (rich-tree-render tree) port)
              (newline port))))

  #|proc:rich-rule
The `rich-rule` procedure constructs a horizontal rule. The optional `title`
is centered in the rule, `width` controls the rendered character width, and
`style` may be `ascii` or `unicode`.
|#
  (define-who rich-rule
    (case-lambda
      [() (rich-rule #f 80 'ascii)]
      [(title) (rich-rule title 80 'ascii)]
      [(title width) (rich-rule title width 'ascii)]
      [(title width style)
       (pcheck ([positive-natural? width] [$rich-rule-style? style])
               (when (and title (not (string? title)))
                 (errorf who "expected title string or #f: ~a" title))
               (mk-rich-rule title width style))]))

  #|proc:rich-rule?
The `rich-rule?` procedure checks whether `obj` is a rich rule.
|#
  (define rich-rule?
    (lambda (obj)
      ($rich-rule? obj)))

  #|proc:rich-rule-style?
The `rich-rule-style?` procedure checks whether `obj` is a supported rule
style. Supported styles are `ascii` and `unicode`.
|#
  (define rich-rule-style?
    (lambda (obj)
      ($rich-rule-style? obj)))

  #|proc:rich-rule-render
The `rich-rule-render` procedure renders `rule` as a horizontal rule string.
|#
  (define rich-rule-render
    (lambda (rule)
      (pcheck ([$rich-rule? rule])
              ($rich-rule-line rule))))

  #|proc:rich-rule-print
The `rich-rule-print` procedure writes the rendered rule to the current output
port without appending a newline.
|#
  (define rich-rule-print
    (lambda (rule)
      (pcheck ([$rich-rule? rule])
              (display (rich-rule-render rule)))))

  #|proc:rich-rule-println
The `rich-rule-println` procedure writes the rendered rule to the current
output port and appends a newline.
|#
  (define rich-rule-println
    (lambda (rule)
      (pcheck ([$rich-rule? rule])
              (display (rich-rule-render rule))
              (newline))))

  #|proc:rich-rule-fprint
The `rich-rule-fprint` procedure writes the rendered rule to `port` without
appending a newline.
|#
  (define rich-rule-fprint
    (lambda (port rule)
      (pcheck ([output-port? port] [$rich-rule? rule])
              (display (rich-rule-render rule) port))))

  #|proc:rich-rule-fprintln
The `rich-rule-fprintln` procedure writes the rendered rule to `port` and
appends a newline.
|#
  (define rich-rule-fprintln
    (lambda (port rule)
      (pcheck ([output-port? port] [$rich-rule? rule])
              (display (rich-rule-render rule) port)
              (newline port))))

  #|proc:rich-align
The `rich-align` procedure constructs a fixed-width aligned text renderable.
With two arguments it left-aligns `body`; with three arguments, `style` may be
`left`, `center`, or `right`.
|#
  (define rich-align
    (case-lambda
      [(body width) (rich-align body width 'left)]
      [(body width style)
       (pcheck ([string? body] [positive-natural? width] [$rich-align-style? style])
               (mk-rich-align body width style))]))

  #|proc:rich-align?
The `rich-align?` procedure checks whether `obj` is a rich alignment renderable.
|#
  (define rich-align?
    (lambda (obj)
      ($rich-align? obj)))

  #|proc:rich-align-style?
The `rich-align-style?` procedure checks whether `obj` is a supported alignment
style. Supported styles are `left`, `center`, and `right`.
|#
  (define rich-align-style?
    (lambda (obj)
      ($rich-align-style? obj)))

  #|proc:rich-align-render
The `rich-align-render` procedure renders aligned text as a string.
|#
  (define rich-align-render
    (lambda (align)
      (pcheck ([$rich-align? align])
              ($string-join "\n" ($rich-align-lines align)))))

  #|proc:rich-align-print
The `rich-align-print` procedure writes rendered aligned text to the current
output port without appending a newline.
|#
  (define rich-align-print
    (lambda (align)
      (pcheck ([$rich-align? align])
              (display (rich-align-render align)))))

  #|proc:rich-align-println
The `rich-align-println` procedure writes rendered aligned text to the current
output port and appends a newline.
|#
  (define rich-align-println
    (lambda (align)
      (pcheck ([$rich-align? align])
              (display (rich-align-render align))
              (newline))))

  #|proc:rich-align-fprint
The `rich-align-fprint` procedure writes rendered aligned text to `port`
without appending a newline.
|#
  (define rich-align-fprint
    (lambda (port align)
      (pcheck ([output-port? port] [$rich-align? align])
              (display (rich-align-render align) port))))

  #|proc:rich-align-fprintln
The `rich-align-fprintln` procedure writes rendered aligned text to `port` and
appends a newline.
|#
  (define rich-align-fprintln
    (lambda (port align)
      (pcheck ([output-port? port] [$rich-align? align])
              (display (rich-align-render align) port)
              (newline port))))

  #|proc:rich-padding
The `rich-padding` procedure constructs a padded text renderable. With two
arguments, the same padding is used for all sides. With three arguments,
`vertical` controls top and bottom and `horizontal` controls left and right.
With five arguments, sides are `top`, `right`, `bottom`, and `left`.
|#
  (define rich-padding
    (case-lambda
      [(body pad) (rich-padding body pad pad pad pad)]
      [(body vertical horizontal)
       (rich-padding body vertical horizontal vertical horizontal)]
      [(body top right bottom left)
       (pcheck ([string? body] [natural? top right bottom left])
               (mk-rich-padding body top right bottom left))]))

  #|proc:rich-padding?
The `rich-padding?` procedure checks whether `obj` is a rich padding renderable.
|#
  (define rich-padding?
    (lambda (obj)
      ($rich-padding? obj)))

  #|proc:rich-padding-render
The `rich-padding-render` procedure renders padded text as a string.
|#
  (define rich-padding-render
    (lambda (padding)
      (pcheck ([$rich-padding? padding])
              ($string-join "\n" ($rich-padding-lines padding)))))

  #|proc:rich-padding-print
The `rich-padding-print` procedure writes rendered padded text to the current
output port without appending a newline.
|#
  (define rich-padding-print
    (lambda (padding)
      (pcheck ([$rich-padding? padding])
              (display (rich-padding-render padding)))))

  #|proc:rich-padding-println
The `rich-padding-println` procedure writes rendered padded text to the current
output port and appends a newline.
|#
  (define rich-padding-println
    (lambda (padding)
      (pcheck ([$rich-padding? padding])
              (display (rich-padding-render padding))
              (newline))))

  #|proc:rich-padding-fprint
The `rich-padding-fprint` procedure writes rendered padded text to `port`
without appending a newline.
|#
  (define rich-padding-fprint
    (lambda (port padding)
      (pcheck ([output-port? port] [$rich-padding? padding])
              (display (rich-padding-render padding) port))))

  #|proc:rich-padding-fprintln
The `rich-padding-fprintln` procedure writes rendered padded text to `port` and
appends a newline.
|#
  (define rich-padding-fprintln
    (lambda (port padding)
      (pcheck ([output-port? port] [$rich-padding? padding])
              (display (rich-padding-render padding) port)
              (newline port))))

  #|proc:rich-columns
The `rich-columns` procedure constructs a fixed-width column layout from a
list of strings. With two arguments it uses a two-space gap. With three
arguments, `gap` controls the spaces between columns.
|#
  (define rich-columns
    (case-lambda
      [(items total-width) (rich-columns items total-width 2)]
      [(items total-width gap)
       (pcheck ([$rich-string-list? items]
                [positive-natural? total-width gap])
               (mk-rich-columns items total-width gap))]))

  #|proc:rich-columns?
The `rich-columns?` procedure checks whether `obj` is a rich columns layout.
|#
  (define rich-columns?
    (lambda (obj)
      ($rich-columns? obj)))

  #|proc:rich-columns-render
The `rich-columns-render` procedure renders a columns layout as a string.
|#
  (define rich-columns-render
    (lambda (columns)
      (pcheck ([$rich-columns? columns])
              ($string-join "\n" ($rich-columns-lines columns)))))

  #|proc:rich-columns-print
The `rich-columns-print` procedure writes rendered columns to the current
output port without appending a newline.
|#
  (define rich-columns-print
    (lambda (columns)
      (pcheck ([$rich-columns? columns])
              (display (rich-columns-render columns)))))

  #|proc:rich-columns-println
The `rich-columns-println` procedure writes rendered columns to the current
output port and appends a newline.
|#
  (define rich-columns-println
    (lambda (columns)
      (pcheck ([$rich-columns? columns])
              (display (rich-columns-render columns))
              (newline))))

  #|proc:rich-columns-fprint
The `rich-columns-fprint` procedure writes rendered columns to `port` without
appending a newline.
|#
  (define rich-columns-fprint
    (lambda (port columns)
      (pcheck ([output-port? port] [$rich-columns? columns])
              (display (rich-columns-render columns) port))))

  #|proc:rich-columns-fprintln
The `rich-columns-fprintln` procedure writes rendered columns to `port` and
appends a newline.
|#
  (define rich-columns-fprintln
    (lambda (port columns)
      (pcheck ([output-port? port] [$rich-columns? columns])
              (display (rich-columns-render columns) port)
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
