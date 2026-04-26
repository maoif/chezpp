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
