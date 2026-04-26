#!chezscheme
(import (chezpp))


(mat rich-format

     (rich-style? rich-reset)
     (rich-style? (rich-style 'bold 'red))

     (equal? "\033[1m\033[31merror\033[0m: file.ss"
             (rich-format "~a~a~a: ~a"
                          (rich-style 'bold 'red)
                          "error"
                          rich-reset
                          "file.ss"))

     (equal? "\033[4m\033[94mpath\033[0m"
             (rich-format "~a~a~a"
                          (rich-style 'underline 'bright-blue)
                          "path"
                          rich-reset))

     (equal? "error: file.ss"
             (parameterize ([rich-enable-color? #f])
               (rich-format "~a~a~a: ~a"
                            (rich-style 'bold 'red)
                            "error"
                            rich-reset
                            "file.ss")))

     )


(mat rich-print

     (equal? "\033[33mwarn\033[0m"
             (with-output-to-string
               (lambda ()
                 (rich-print "~a~a~a"
                             (rich-style 'yellow)
                             "warn"
                             rich-reset))))

     (equal? "\033[32mok\033[0m\n"
             (with-output-to-string
               (lambda ()
                 (rich-println "~a~a~a"
                               (rich-style 'green)
                               "ok"
                               rich-reset))))

     (equal? "\033[36mport\033[0m"
             (let ([p (open-output-string)])
               (rich-fprint p "~a~a~a"
                            (rich-style 'cyan)
                            "port"
                            rich-reset)
               (get-output-string p)))

     (equal? "\033[35mport\033[0m\n"
             (let ([p (open-output-string)])
               (rich-fprintln p "~a~a~a"
                              (rich-style 'magenta)
                              "port"
                              rich-reset)
               (get-output-string p)))

     )


(mat rich-errors

     (error? (rich-format 123 "x"))
     (error? (rich-fprint "not-a-port" "~a" "x"))
     (error? (rich-style "red"))
     (error? (rich-style 'not-a-style))
     (error? (parameterize ([rich-enable-color? 'yes])
               (rich-format "~a" "x")))

     )


(mat rich-table-render

     (rich-table-border-style? 'ascii)
     (rich-table-border-style? 'unicode)
     (not (rich-table-border-style? 'double))

     (let ([t (make-rich-table)])
       (and (rich-table? t)
            (equal? "" (rich-table-render t))))

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Name")
       (rich-table-add-column! t "Status")
       (equal? (string-append
                "+------+--------+\n"
                "| Name | Status |\n"
                "+------+--------+")
               (rich-table-render t)))

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Name")
       (rich-table-add-column! t "Status")
       (rich-table-add-row! t "build" "ok")
       (equal? (string-append
                "+-------+--------+\n"
                "| Name  | Status |\n"
                "+-------+--------+\n"
                "| build | ok     |\n"
                "+-------+--------+")
               (rich-table-render t)))

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Name")
       (rich-table-add-column! t "Status")
       (rich-table-add-row! t "build" "ok")
       (rich-table-border-style-set! t 'unicode)
       (equal? (string-append
                "┌───────┬────────┐\n"
                "│ Name  │ Status │\n"
                "├───────┼────────┤\n"
                "│ build │ ok     │\n"
                "└───────┴────────┘")
               (rich-table-render t)))

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Status")
       (rich-table-add-row! t (rich-format "~a~a~a"
                                           (rich-style 'green)
                                           "ok"
                                           rich-reset))
       (string-contains? (rich-table-render t) "\033[32mok\033[0m"))

     )


(mat rich-table-print

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Name")
       (equal? (string-append
                "+------+\n"
                "| Name |\n"
                "+------+")
               (with-output-to-string
                 (lambda ()
                   (rich-table-print t)))))

     (let ([t (make-rich-table)])
       (rich-table-add-column! t "Name")
       (equal? (string-append
                "+------+\n"
                "| Name |\n"
                "+------+\n")
               (with-output-to-string
                 (lambda ()
                   (rich-table-println t)))))

     (let ([t (make-rich-table)] [p (open-output-string)])
       (rich-table-add-column! t "Name")
       (rich-table-fprint p t)
       (equal? (string-append
                "+------+\n"
                "| Name |\n"
                "+------+")
               (get-output-string p)))

     (let ([t (make-rich-table)] [p (open-output-string)])
       (rich-table-add-column! t "Name")
       (rich-table-fprintln p t)
       (equal? (string-append
                "+------+\n"
                "| Name |\n"
                "+------+\n")
               (get-output-string p)))

     )


(mat rich-table-errors

     (error? (rich-table-render #f))
     (error? (rich-table-add-column! (make-rich-table) 'Name))
     (error? (rich-table-border-style-set! #f 'unicode))
     (error? (rich-table-border-style-set! (make-rich-table) 'double))

     (error? (let ([t (make-rich-table)])
               (rich-table-add-column! t "Name")
               (rich-table-add-row! t "a" "b")))

     (error? (rich-table-fprint "not-a-port" (make-rich-table)))

     )


(mat rich-panel-render

     (let ([p (rich-panel "hello")])
       (and (rich-panel? p)
            (equal? (string-append
                     "+-------+\n"
                     "| hello |\n"
                     "+-------+")
                    (rich-panel-render p))))

     (equal? (string-append
              "+- Status -+\n"
              "| hello    |\n"
              "+----------+")
             (rich-panel-render (rich-panel "hello" "Status")))

     (equal? (string-append
              "┌─ Status ─┐\n"
              "│ hello    │\n"
              "└──────────┘")
             (rich-panel-render (rich-panel "hello" "Status" 'unicode)))

     (equal? (string-append
              "+-------+\n"
              "| alpha |\n"
              "| b     |\n"
              "+-------+")
             (rich-panel-render (rich-panel "alpha\nb")))

     (string-contains?
      (rich-panel-render
       (rich-panel (rich-format "~a~a~a" (rich-style 'red) "error" rich-reset)))
      "\033[31merror\033[0m")

     )


(mat rich-panel-print

     (equal? (string-append
              "+-------+\n"
              "| hello |\n"
              "+-------+")
             (with-output-to-string
               (lambda ()
                 (rich-panel-print (rich-panel "hello")))))

     (equal? (string-append
              "+-------+\n"
              "| hello |\n"
              "+-------+\n")
             (with-output-to-string
               (lambda ()
                 (rich-panel-println (rich-panel "hello")))))

     (let ([p (open-output-string)])
       (rich-panel-fprint p (rich-panel "hello"))
       (equal? (string-append
                "+-------+\n"
                "| hello |\n"
                "+-------+")
               (get-output-string p)))

     (let ([p (open-output-string)])
       (rich-panel-fprintln p (rich-panel "hello"))
       (equal? (string-append
                "+-------+\n"
                "| hello |\n"
                "+-------+\n")
               (get-output-string p)))

     )


(mat rich-panel-errors

     (rich-panel-border-style? 'ascii)
     (rich-panel-border-style? 'unicode)
     (not (rich-panel-border-style? 'double))

     (error? (rich-panel 123))
     (error? (rich-panel "hello" 123))
     (error? (rich-panel "hello" "Title" 'double))
     (error? (rich-panel-render #f))
     (error? (rich-panel-fprint "not-a-port" (rich-panel "hello")))

     )


(mat rich-tree-render

     (let ([t (make-rich-tree "root")])
       (and (rich-tree? t)
            (rich-tree-guide-style? 'unicode)
            (rich-tree-guide-style? 'ascii)
            (not (rich-tree-guide-style? 'double))
            (equal? "root" (rich-tree-render t))))

     (let* ([t (make-rich-tree "root")]
            [src (rich-tree-add! t "src")])
       (rich-tree-add! src "main.ss")
       (rich-tree-add! t "tests")
       (equal? (string-append
                "root\n"
                "├── src\n"
                "│   └── main.ss\n"
                "└── tests")
               (rich-tree-render t)))

     (let* ([t (make-rich-tree "root" 'ascii)]
            [src (rich-tree-add! t "src")])
       (rich-tree-add! src "main.ss")
       (rich-tree-add! t "tests")
       (equal? (string-append
                "root\n"
                "|-- src\n"
                "|   `-- main.ss\n"
                "`-- tests")
               (rich-tree-render t)))

     )


(mat rich-tree-print

     (let* ([t (make-rich-tree "root")]
            [port (open-output-string)])
       (rich-tree-add! t "child")
       (rich-tree-fprint port t)
       (equal? "root\n└── child" (get-output-string port)))

     (let ([t (make-rich-tree "root")])
       (rich-tree-add! t "child")
       (equal? "root\n└── child\n"
               (with-output-to-string
                 (lambda ()
                   (rich-tree-println t)))))

     )


(mat rich-tree-errors

     (error? (make-rich-tree 123))
     (error? (make-rich-tree "root" 'double))
     (error? (rich-tree-add! #f "child"))
     (error? (rich-tree-add! (make-rich-tree "root") 123))
     (error? (rich-tree-render #f))
     (error? (rich-tree-fprint "not-a-port" (make-rich-tree "root")))

     )


(mat rich-rule-render

     (let ([r (rich-rule #f 6)])
       (and (rich-rule? r)
            (rich-rule-style? 'ascii)
            (rich-rule-style? 'unicode)
            (not (rich-rule-style? 'double))
            (equal? "------" (rich-rule-render r))))

     (equal? "-- Build ---"
             (rich-rule-render (rich-rule "Build" 12)))

     (equal? "─── Build ───"
             (rich-rule-render (rich-rule "Build" 13 'unicode)))

     (equal? "Build"
             (rich-rule-render (rich-rule "Build" 5)))

     )


(mat rich-rule-print

     (equal? "-- Build ---"
             (with-output-to-string
               (lambda ()
                 (rich-rule-print (rich-rule "Build" 12)))))

     (equal? "-- Build ---\n"
             (with-output-to-string
               (lambda ()
                 (rich-rule-println (rich-rule "Build" 12)))))

     (let ([port (open-output-string)])
       (rich-rule-fprint port (rich-rule "Build" 12))
       (equal? "-- Build ---" (get-output-string port)))

     (let ([port (open-output-string)])
       (rich-rule-fprintln port (rich-rule "Build" 12))
       (equal? "-- Build ---\n" (get-output-string port)))

     )


(mat rich-rule-errors

     (error? (rich-rule 123))
     (error? (rich-rule "Build" 0))
     (error? (rich-rule "Build" 12 'double))
     (error? (rich-rule-render #f))
     (error? (rich-rule-fprint "not-a-port" (rich-rule "Build" 12)))

     )


(mat rich-align-render

     (let ([a (rich-align "hi" 6)])
       (and (rich-align? a)
            (rich-align-style? 'left)
            (rich-align-style? 'center)
            (rich-align-style? 'right)
            (not (rich-align-style? 'middle))
            (equal? "hi    " (rich-align-render a))))

     (equal? "  hi  "
             (rich-align-render (rich-align "hi" 6 'center)))

     (equal? "    hi"
             (rich-align-render (rich-align "hi" 6 'right)))

     (equal? "  a  \nlonger"
             (rich-align-render (rich-align "a\nlonger" 5 'center)))

     )


(mat rich-align-print

     (equal? "  hi  "
             (with-output-to-string
               (lambda ()
                 (rich-align-print (rich-align "hi" 6 'center)))))

     (equal? "  hi  \n"
             (with-output-to-string
               (lambda ()
                 (rich-align-println (rich-align "hi" 6 'center)))))

     (let ([port (open-output-string)])
       (rich-align-fprint port (rich-align "hi" 6 'center))
       (equal? "  hi  " (get-output-string port)))

     (let ([port (open-output-string)])
       (rich-align-fprintln port (rich-align "hi" 6 'center))
       (equal? "  hi  \n" (get-output-string port)))

     )


(mat rich-align-errors

     (error? (rich-align 123 6))
     (error? (rich-align "hi" 0))
     (error? (rich-align "hi" 6 'middle))
     (error? (rich-align-render #f))
     (error? (rich-align-fprint "not-a-port" (rich-align "hi" 6)))

     )


(mat rich-padding-render

     (let ([p (rich-padding "x" 1)])
       (and (rich-padding? p)
            (equal? "   \n x \n   " (rich-padding-render p))))

     (equal? "  x   \n  yy  "
             (rich-padding-render (rich-padding "x\nyy" 0 2)))

     (equal? "      \n   x  "
             (rich-padding-render (rich-padding "x" 1 2 0 3)))

     )


(mat rich-padding-print

     (equal? "  x  "
             (with-output-to-string
               (lambda ()
                 (rich-padding-print (rich-padding "x" 0 2)))))

     (equal? "  x  \n"
             (with-output-to-string
               (lambda ()
                 (rich-padding-println (rich-padding "x" 0 2)))))

     (let ([port (open-output-string)])
       (rich-padding-fprint port (rich-padding "x" 0 2))
       (equal? "  x  " (get-output-string port)))

     (let ([port (open-output-string)])
       (rich-padding-fprintln port (rich-padding "x" 0 2))
       (equal? "  x  \n" (get-output-string port)))

     )


(mat rich-padding-errors

     (error? (rich-padding 123 1))
     (error? (rich-padding "x" -1))
     (error? (rich-padding "x" 1 -1))
     (error? (rich-padding "x" 1 2 3 -1))
     (error? (rich-padding-render #f))
     (error? (rich-padding-fprint "not-a-port" (rich-padding "x" 1)))

     )


(mat rich-progress-render

     (let ([p (make-rich-progress 10)])
       (and (rich-progress? p)
            (equal? "" (rich-progress-render p))))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-update! p id 4)
       (equal? "download [####------] 40% 4/10"
               (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id0 (rich-progress-add-task! p "build" 4)]
            [id1 (rich-progress-add-task! p "test" 5)])
       (and (= id0 0)
            (= id1 1)
            (rich-progress-advance! p id0 2)
            (rich-progress-complete! p id1)
            (equal? (string-append
                     "build [#####-----] 50% 2/4\n"
                     "test [##########] 100% 5/5")
                    (rich-progress-render p))))

     (parameterize ([rich-progress-current-time (lambda () 100)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "scan" #f)])
         (rich-progress-advance! p id 3)
         (and (equal? "scan [###-------] --% 3/?"
                      (rich-progress-render p))
              (parameterize ([rich-progress-current-time (lambda () 100.2)])
                (equal? "scan [--###-----] --% 3/?"
                        (rich-progress-render p))))))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-update! p id 4)
       (rich-progress-task-description-set! p id "upload")
       (rich-progress-task-total-set! p id 20)
       (equal? "upload [##--------] 20% 4/20"
               (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "scan" #f)])
       (rich-progress-advance! p id 3)
       (rich-progress-task-total-set! p id 12)
       (equal? "scan [##--------] 25% 3/12"
               (rich-progress-render p)))

     (parameterize ([rich-progress-current-time (lambda () 100)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "download" 10)])
         (rich-progress-update! p id 4)
         (rich-progress-task-total-set! p id #f)
         (equal? "download [###-------] --% 4/?"
                 (rich-progress-render p))))

     (let* ([p (make-rich-progress 10)]
            [id0 (rich-progress-add-task! p "a" 10)]
            [id1 (rich-progress-add-task! p "b" 10)])
       (rich-progress-task-visible?-set! p id0 #f)
       (rich-progress-remove-task! p id1)
       (equal? "" (rich-progress-render p)))

     )


(mat rich-progress-print

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-update! p id 4)
       (equal? "download [####------] 40% 4/10"
               (with-output-to-string
                 (lambda ()
                   (rich-progress-print p)))))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-update! p id 4)
       (equal? "download [####------] 40% 4/10\n"
               (with-output-to-string
                 (lambda ()
                   (rich-progress-println p)))))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)]
            [port (open-output-string)])
       (rich-progress-update! p id 4)
       (rich-progress-fprint port p)
       (equal? "download [####------] 40% 4/10"
               (get-output-string port)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)]
            [port (open-output-string)])
       (rich-progress-update! p id 4)
       (rich-progress-fprintln port p)
       (equal? "download [####------] 40% 4/10\n"
               (get-output-string port)))

     )


(mat rich-progress-live

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)]
            [port (open-output-string)])
       (rich-progress-update! p id 4)
       (rich-progress-frefresh! port p)
       (equal? "\r\033[2Kdownload [####------] 40% 4/10"
               (get-output-string port)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-update! p id 4)
       (equal? "\r\033[2Kdownload [####------] 40% 4/10"
               (with-output-to-string
                 (lambda ()
                   (rich-progress-refresh! p)))))

     (let* ([p (make-rich-progress 10)]
            [id0 (rich-progress-add-task! p "build" 10)]
            [id1 (rich-progress-add-task! p "test" 10)]
            [port (open-output-string)])
       (rich-progress-update! p id0 4)
       (rich-progress-update! p id1 7)
       (rich-progress-frefresh! port p)
       (rich-progress-remove-task! p id1)
       (rich-progress-update! p id0 5)
       (rich-progress-frefresh! port p)
       (equal? (string-append
                "\r\033[2Kbuild [####------] 40% 4/10\n"
                "test [#######---] 70% 7/10"
                "\r\033[2K\033[1A\r\033[2K"
                "build [#####-----] 50% 5/10")
               (get-output-string port)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)]
            [port (open-output-string)])
       (rich-progress-complete! p id)
       (rich-progress-ffinish! port p)
       (equal? "download [##########] 100% 10/10\n"
               (get-output-string port)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-complete! p id)
       (equal? "download [##########] 100% 10/10\n"
               (with-output-to-string
                 (lambda ()
                   (rich-progress-finish! p)))))

     )


(mat rich-progress-live-auto

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)]
            [port (open-output-string)])
       (and (not (rich-progress-live? p))
            (rich-progress-start! port p 1)
            (rich-progress-live? p)
            (rich-progress-update! p id 4)
            (milisleep 20)
            (rich-progress-stop! p)
            (not (rich-progress-live? p))
            (let ([out (get-output-string port)])
              (and (string-contains? out "\r\033[2K")
                   (string-contains? out "download [####------] 40% 4/10")))))

     (let ([p (make-rich-progress 10)])
       (and (not (rich-progress-live? p))
            (not (rich-progress-stop! p))))

     )


(mat rich-progress-columns

     (let ([cols (rich-progress-default-columns)])
       (and (not (null? cols))
            (andmap rich-progress-column? cols)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-text-column "{description}")
              (rich-progress-percent-column)))
       (rich-progress-update! p id 4)
       (equal? "download 40%" (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-text-column
               "{description}: {completed}/{total} ({percent})")))
       (rich-progress-update! p id 4)
       (equal? "download: 4/10 (40%)" (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "scan" #f)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-text-column "{description}: {completed}/{total}")))
       (rich-progress-advance! p id 3)
       (equal? "scan: 3/?" (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-text-column "literal progress")))
       (rich-progress-update! p id 4)
       (equal? "literal progress" (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-text-column "{completed}")
              (rich-progress-text-column "{total}")
              (rich-progress-bar-column)
              (rich-progress-complete-column)))
       (rich-progress-update! p id 4)
       (equal? "4 10 [####------] 4/10" (rich-progress-render p)))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "download" 10)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-bar-column "=" ".")))
       (rich-progress-update! p id 4)
       (equal? "[====......]" (rich-progress-render p)))

     (parameterize ([rich-progress-current-time (lambda () 100)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "scan" #f)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-bar-column "=" "." "~")))
         (rich-progress-advance! p id 3)
         (equal? "[~~~.......]" (rich-progress-render p))))

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "scan" #f)])
       (rich-progress-columns-set!
        p
        (list (rich-progress-text-column "{description}")
              (rich-progress-text-column "{total}")
              (rich-progress-percent-column)
              (rich-progress-complete-column)))
       (rich-progress-advance! p id 3)
       (equal? "scan ? --% 3/?" (rich-progress-render p)))

     )


(mat rich-progress-time-columns

     (parameterize ([rich-progress-current-time (lambda () 100)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "download" 100)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-text-column "{description}")
                (rich-progress-elapsed-column)
                (rich-progress-remaining-column)
                (rich-progress-transfer-speed-column)))
         (parameterize ([rich-progress-current-time (lambda () 110)])
           (rich-progress-update! p id 20)
           (equal? "download 00:10 00:40 2/s" (rich-progress-render p)))))

     (parameterize ([rich-progress-current-time (lambda () 100)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "scan" #f)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-elapsed-column)
                (rich-progress-remaining-column)
                (rich-progress-transfer-speed-column)))
         (parameterize ([rich-progress-current-time (lambda () 103)])
           (rich-progress-advance! p id 9)
           (equal? "00:03 --:-- 3/s" (rich-progress-render p)))))

     (parameterize ([rich-progress-current-time (lambda () 10)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "build" 10)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-elapsed-column)
                (rich-progress-remaining-column)
                (rich-progress-transfer-speed-column)))
         (rich-progress-stop-task! p id)
         (parameterize ([rich-progress-current-time (lambda () 100)])
           (equal? "00:00 --:-- --/s" (rich-progress-render p)))))

     (parameterize ([rich-progress-current-time (lambda () 10)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "build" 10)])
         (rich-progress-stop-task! p id)
         (parameterize ([rich-progress-current-time (lambda () 20)])
           (rich-progress-start-task! p id)
           (rich-progress-update! p id 5)
           (parameterize ([rich-progress-current-time (lambda () 25)])
             (rich-progress-columns-set!
              p
              (list (rich-progress-elapsed-column)
                    (rich-progress-remaining-column)
                    (rich-progress-transfer-speed-column)))
             (equal? "00:05 00:05 1/s" (rich-progress-render p))))))

     (parameterize ([rich-progress-current-time (lambda () 10)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "build" 10)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-elapsed-column)
                (rich-progress-remaining-column)
                (rich-progress-transfer-speed-column)))
         (parameterize ([rich-progress-current-time (lambda () 15)])
           (rich-progress-update! p id 10))
         (parameterize ([rich-progress-current-time (lambda () 100)])
           (equal? "00:05 --:-- 2/s" (rich-progress-render p)))))

     (parameterize ([rich-progress-current-time (lambda () 10)])
       (let* ([p (make-rich-progress 10)]
              [id0 (rich-progress-add-task! p "build" 10)]
              [id1 (rich-progress-add-task! p "test" 10)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-text-column "{description}")
                (rich-progress-elapsed-column)
                (rich-progress-transfer-speed-column)))
         (parameterize ([rich-progress-current-time (lambda () 12)])
           (rich-progress-advance! p id0 10))
         (parameterize ([rich-progress-current-time (lambda () 14)])
           (rich-progress-complete! p id1))
         (parameterize ([rich-progress-current-time (lambda () 100)])
           (equal? (string-append
                    "build 00:02 5/s\n"
                    "test 00:04 2/s")
                   (rich-progress-render p)))))

     )


(mat rich-progress-spinner-column

     (parameterize ([rich-progress-current-time (lambda () 100)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "scan" #f)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-spinner-column)
                (rich-progress-text-column "{description}")))
         (parameterize ([rich-progress-current-time (lambda () 100)])
           (and (equal? "⠋ scan" (rich-progress-render p))
                (parameterize ([rich-progress-current-time (lambda () 100.1)])
                  (equal? "⠙ scan" (rich-progress-render p)))
                (parameterize ([rich-progress-current-time (lambda () 100.8)])
                  (equal? "⠋ scan" (rich-progress-render p)))))))

     (parameterize ([rich-progress-current-time (lambda () 10)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "build" 10)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-spinner-column (list "." "o" "O") 0.5)
                (rich-progress-text-column "{percent}")))
         (parameterize ([rich-progress-current-time (lambda () 11)])
           (rich-progress-update! p id 5)
           (equal? "O 50%" (rich-progress-render p)))))

     (parameterize ([rich-progress-current-time (lambda () 10)])
       (let* ([p (make-rich-progress 10)]
              [id (rich-progress-add-task! p "build" 10)])
         (rich-progress-columns-set!
          p
          (list (rich-progress-spinner-column (list "-" "+") 1)
                (rich-progress-text-column "{description}")))
         (parameterize ([rich-progress-current-time (lambda () 11)])
           (rich-progress-stop-task! p id))
         (parameterize ([rich-progress-current-time (lambda () 20)])
           (equal? "+ build" (rich-progress-render p)))))

     )


(mat rich-progress-errors

     (error? (make-rich-progress 0))
     (error? (rich-progress-render #f))
     (error? (rich-progress-add-task! (make-rich-progress) 123 10))
     (error? (rich-progress-add-task! (make-rich-progress) "bad" 0))

     (error? (let* ([p (make-rich-progress)]
                    [id (rich-progress-add-task! p "download" 10)])
               (rich-progress-update! p id 11)))

     (error? (let* ([p (make-rich-progress)]
                    [id (rich-progress-add-task! p "download" 10)])
               (rich-progress-advance! p id 11)))

     (error? (let ([p (make-rich-progress)])
               (rich-progress-update! p 99 1)))

     (error? (let ([p (make-rich-progress)])
               (rich-progress-task-description-set! p 99 "missing")))

     (error? (let ([p (make-rich-progress)])
               (rich-progress-task-description-set! p 0 123)))

     (error? (let* ([p (make-rich-progress)]
                    [id (rich-progress-add-task! p "download" 10)])
               (rich-progress-update! p id 8)
               (rich-progress-task-total-set! p id 7)))

     (error? (let* ([p (make-rich-progress)]
                    [id (rich-progress-add-task! p "download" 10)])
               (rich-progress-task-total-set! p id 0)))

     (error? (rich-progress-fprint "not-a-port" (make-rich-progress)))
     (error? (rich-progress-frefresh! "not-a-port" (make-rich-progress)))
     (error? (rich-progress-ffinish! "not-a-port" (make-rich-progress)))
     (error? (rich-progress-start! "not-a-port" (make-rich-progress) 1))
     (error? (rich-progress-start! (open-output-string) #f 1))
     (error? (rich-progress-start! (open-output-string) (make-rich-progress) 0))
     (error? (rich-progress-bar-column "##" "-"))
     (error? (rich-progress-bar-column "#" "--"))
     (error? (rich-progress-bar-column "#" "-" "~~"))
     (error? (rich-progress-text-column "{unknown}"))
     (error? (rich-progress-columns-set! (make-rich-progress) '()))
     (error? (rich-progress-columns-set! (make-rich-progress) (list 'not-column)))
     (error? (parameterize ([rich-progress-current-time 123])
               (make-rich-progress)))
     (error? (rich-progress-start-task! (make-rich-progress) 99))
     (error? (rich-progress-stop-task! (make-rich-progress) 99))
     (error? (rich-progress-spinner-column '()))
     (error? (rich-progress-spinner-column (list "." 1) 0.1))
     (error? (rich-progress-spinner-column (list ".") 0))
     (error? (rich-progress-spinner-column (list ".") "fast"))

     )
