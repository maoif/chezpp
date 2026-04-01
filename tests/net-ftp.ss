(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ftp-common.ss")

(mat net-ftp
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [download-path "/tmp/chezpp-net-ftp-download.txt"])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-session? session)
                  (ftp-login! session "user" "pass")
                  (equal? (ftp-pwd session) "/")
                  (not (ftp-active-mode! session))
                  (ftp-passive-mode! session)
                  (let ([entries (begin
                                   (milisleep 50)
                                   (ftp-list session))])
                    (and (member "docs" entries)
                         (member "hello.txt" entries)))
                  (equal? (ftp-cwd! session "/docs") session)
                  (equal? (ftp-pwd session) "/docs")
                  (equal? (begin
                            (milisleep 50)
                            (ftp-list session))
                          '("readme.txt"))
                  (ftp-cwd! session "/")
                  (equal? (ftp-quit! session) session)))
           (lambda ()
             (stop-server))))))
