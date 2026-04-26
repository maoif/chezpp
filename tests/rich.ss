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

     (let* ([p (make-rich-progress 10)]
            [id (rich-progress-add-task! p "scan" #f)])
       (rich-progress-advance! p id 3)
       (equal? "scan [----------] --% 3/?"
               (rich-progress-render p)))

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
        (list (rich-progress-text-column "{completed}")
              (rich-progress-text-column "{total}")
              (rich-progress-bar-column)
              (rich-progress-complete-column)))
       (rich-progress-update! p id 4)
       (equal? "4 10 [####------] 4/10" (rich-progress-render p)))

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

     (error? (rich-progress-fprint "not-a-port" (make-rich-progress)))
     (error? (rich-progress-frefresh! "not-a-port" (make-rich-progress)))
     (error? (rich-progress-ffinish! "not-a-port" (make-rich-progress)))
     (error? (rich-progress-text-column "{unknown}"))
     (error? (rich-progress-columns-set! (make-rich-progress) '()))
     (error? (rich-progress-columns-set! (make-rich-progress) (list 'not-column)))
     (error? (parameterize ([rich-progress-current-time 123])
               (make-rich-progress)))
     (error? (rich-progress-start-task! (make-rich-progress) 99))
     (error? (rich-progress-stop-task! (make-rich-progress) 99))

     )
