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
