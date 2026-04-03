(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ftp-common.ss")

(define wait-ftp-nonblocking
  (lambda (proc)
    (let loop ([i 0])
      (let ([ans (proc)])
        (cond
         [ans ans]
         [(>= i 200) #f]
         [else
          (milisleep 10)
          (loop (+ i 1))])))))

(define retry-ftp-test-op
  (lambda (proc)
    (let loop ([i 0])
      (guard (c [else
                 (if (and (net-error? c) (< i 4))
                     (begin
                       (milisleep 100)
                       (loop (+ i 1)))
                     (raise c))])
        (proc)))))

(mat net-ftp-session
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-session? session)
                  (ftp-login! session "user" "pass")
                  (equal? (ftp-pwd session) "/")
                  (not (ftp-active-mode! session))
                  (ftp-passive-mode! session)
                  (equal? (ftp-quit! session) session)))
           (lambda ()
             (stop-server))))))

(mat net-ftp-list
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (let ([entries (begin
                                   (milisleep 50)
                                   (ftp-list session))])
                    (and (not (not (member "docs" entries)))
                         (not (not (member "hello.txt" entries)))))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-cwd
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [docs-path (string-append root "/docs")])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (equal? (ftp-cwd! session "/docs") session)
                  (equal? (ftp-pwd session) "/docs")
                  (equal? (directory-list docs-path)
                          '("readme.txt"))
                  (equal? (begin
                            (milisleep 50)
                            (ftp-list session))
                          '("readme.txt"))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-download
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [download-path "/tmp/chezpp-net-ftp-download.txt"])
         (dynamic-wind
           (lambda ()
             (when (file-exists? download-path)
               (delete-file download-path #f)))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (equal? (ftp-download session "/hello.txt" download-path)
                          download-path)
                  (equal? (read-u8vec download-path)
                          (string->utf8 "hello ftp"))))
           (lambda ()
             (when (file-exists? download-path)
               (delete-file download-path #f))
             (stop-server))))))

(mat net-ftp-upload
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [upload-path "/tmp/chezpp-net-ftp-upload.txt"]
             [uploaded-path (string-append root "/uploaded.txt")])
         (dynamic-wind
           (lambda ()
             (when (file-exists? upload-path)
               (delete-file upload-path #f)))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (begin
                    (write-bytevector-file upload-path (string->utf8 "upload ftp"))
                    #t)
                  (equal? (ftp-upload session upload-path "/uploaded.txt")
                          "/uploaded.txt")
                  (file-regular? uploaded-path)
                  (equal? (read-u8vec uploaded-path)
                          (string->utf8 "upload ftp"))))
           (lambda ()
             (when (file-exists? upload-path)
               (delete-file upload-path #f))
             (stop-server))))))

(mat net-ftp-rename
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [uploaded-path (string-append root "/uploaded.txt")]
             [renamed-path (string-append root "/renamed.txt")])
         (dynamic-wind
           (lambda ()
             (write-bytevector-file uploaded-path (string->utf8 "rename me")))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (eq? (ftp-rename! session "/uploaded.txt" "/renamed.txt")
                       session)
                  (not (file-exists? uploaded-path))
                  (file-regular? renamed-path)
                  (equal? (read-u8vec renamed-path)
                          (string->utf8 "rename me"))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-mkdir
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [tmpdir-path (string-append root "/tmpdir")])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (eq? (ftp-mkdir! session "/tmpdir") session)
                  (file-directory? tmpdir-path)))
           (lambda ()
             (stop-server))))))

(mat net-ftp-rmdir
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [tmpdir-path (string-append root "/tmpdir")])
         (dynamic-wind
           (lambda ()
             (mkdirs tmpdir-path))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (eq? (begin
                         (milisleep 100)
                         (ftp-rmdir! session "/tmpdir"))
                       session)
                  (not (file-exists? tmpdir-path))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-delete
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [victim-path (string-append root "/victim.txt")])
         (dynamic-wind
           (lambda ()
             (write-bytevector-file victim-path (string->utf8 "delete me")))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (eq? (retry-ftp-test-op
                        (lambda ()
                          (ftp-delete! session "/victim.txt")))
                       session)
                  (not (file-exists? victim-path))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-input-port
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (call-with-port
                   (open-ftp-input-port session "/hello.txt")
                   (lambda (ip)
                     (equal? (read-port->bytevector ip)
                             (string->utf8 "hello ftp"))))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-output-port
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [ported-path (string-append root "/ported.txt")])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (call-with-port
                   (open-ftp-output-port session "/ported.txt")
                   (lambda (op)
                     (put-bytevector op (string->utf8 "through port"))))
                  (file-regular? ported-path)
                  (equal? (read-u8vec ported-path)
                          (string->utf8 "through port"))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-list-nonblocking
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (let ([entries (wait-ftp-nonblocking
                                  (lambda ()
                                    (ftp-list/nonblocking session)))])
                    (and (list? entries)
                         (not (not (member "docs" entries)))
                         (not (not (member "hello.txt" entries)))))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-cancel-pending
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))])
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (eq? (ftp-cancel-pending! session) session)
                  (not (ftp-list/nonblocking session "/slow"))
                  (eq? (ftp-cancel-pending! session) session)
                  (let ([entries (ftp-list session)])
                    (and (list? entries)
                         (not (not (member "docs" entries)))
                         (not (not (member "hello.txt" entries)))))))
           (lambda ()
             (stop-server))))))

(mat net-ftp-download-nonblocking
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [download-path "/tmp/chezpp-net-ftp-download-nb.txt"])
         (dynamic-wind
           (lambda ()
             (when (file-exists? download-path)
               (delete-file download-path #f)))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (equal? (wait-ftp-nonblocking
                           (lambda ()
                             (ftp-download/nonblocking session
                                                       "/hello.txt"
                                                       download-path)))
                          download-path)
                  (equal? (read-u8vec download-path)
                          (string->utf8 "hello ftp"))))
           (lambda ()
             (when (file-exists? download-path)
               (delete-file download-path #f))
             (stop-server))))))

(mat net-ftp-upload-nonblocking
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [upload-path "/tmp/chezpp-net-ftp-upload-nb.txt"]
             [uploaded-path (string-append root "/uploaded-nb.txt")])
         (dynamic-wind
           (lambda ()
             (when (file-exists? upload-path)
               (delete-file upload-path #f)))
           (lambda ()
             (and (ftp-login! session "user" "pass")
                  (begin
                    (write-bytevector-file upload-path (string->utf8 "upload nonblocking"))
                    #t)
                  (equal? (wait-ftp-nonblocking
                           (lambda ()
                             (ftp-upload/nonblocking session
                                                     upload-path
                                                     "/uploaded-nb.txt")))
                          "/uploaded-nb.txt")
                  (file-regular? uploaded-path)
                  (equal? (read-u8vec uploaded-path)
                          (string->utf8 "upload nonblocking"))))
           (lambda ()
             (when (file-exists? upload-path)
               (delete-file upload-path #f))
             (stop-server))))))
