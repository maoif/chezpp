(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(define sftp-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (or (string-contains? (net-error-message c) "timed out")
                        (string-contains? (net-error-message c) "Timeout")))])
      (thunk)
      #f)))

(define sftp-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(mat net-sftp
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-sftp-test remote-root home port user))
         (lambda ()
           (stop-server)))))

(mat net-sftp-nonblocking
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-sftp-nonblocking-test remote-root home port user))
         (lambda ()
           (stop-server)))))

(mat net-sftp-timeout
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-sftp-timeout-test remote-root home port user sftp-net-error-timeout?))
         (lambda ()
           (stop-server)))))

(mat net-sftp-timeout-validation
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (with-env
            "HOME"
            home
            (lambda ()
              (let ([session (ssh-open "127.0.0.1" port user)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and
                     (eq? (ssh-auth-publickey! session user) session)
                     (let ([sftp (sftp-open session)])
                       (dynamic-wind
                         void
                         (lambda ()
                           (and
                            (sftp-session? sftp)
                            (let ([read-file (sftp-open-file sftp
                                                             (string-append remote-root "/hello.txt")
                                                             'read)]
                                  [write-file (sftp-open-file sftp
                                                              (string-append remote-root "/timeout-validation.txt")
                                                              '(write create truncate))]
                                  [buf (make-bytevector 4 0)]
                                  [bv (string->utf8 "x")])
                              (dynamic-wind
                                void
                                (lambda ()
                                  (and
                                   (sftp-error-message-contains?
                                    "timeout must be non-negative"
                                    (lambda ()
                                      (sftp-read read-file 1 -1)))
                                   (sftp-error-message-contains?
                                    "timeout must be non-negative"
                                    (lambda ()
                                      (sftp-read! read-file buf 0 1 -1)))
                                   (sftp-error-message-contains?
                                    "timeout must be non-negative"
                                    (lambda ()
                                      (sftp-write write-file bv 0 1 -1)))
                                   (sftp-error-message-contains?
                                    "timeout must be non-negative"
                                    (lambda ()
                                      (sftp-write-all write-file bv 0 1 -1)))))
                                (lambda ()
                                  (sftp-close-file write-file)
                                  (sftp-close-file read-file))))))
                         (lambda ()
                           (sftp-close sftp))))))
                  (lambda ()
                    (ssh-close session)))))))
         (lambda ()
           (stop-server)))))

(mat net-sftp-read-size-validation
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (with-env
            "HOME"
            home
            (lambda ()
              (let ([session (ssh-open "127.0.0.1" port user)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and
                     (eq? (ssh-auth-publickey! session user) session)
                     (let ([sftp (sftp-open session)])
                       (dynamic-wind
                         void
                         (lambda ()
                           (let ([file (sftp-open-file sftp
                                                       (string-append remote-root "/hello.txt")
                                                       'read)])
                             (dynamic-wind
                               void
                               (lambda ()
                                 (and
                                  (sftp-error-message-contains?
                                   "size must be non-negative"
                                   (lambda ()
                                     (sftp-read file -1)))
                                  (sftp-error-message-contains?
                                   "size must be non-negative"
                                   (lambda ()
                                     (sftp-read/nonblocking file -1)))))
                               (lambda ()
                                 (sftp-close-file file)))))
                         (lambda ()
                           (sftp-close sftp))))))
                  (lambda ()
                    (ssh-close session)))))))
         (lambda ()
           (stop-server)))))

(mat net-sftp-open-closed-ssh-session
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (with-env
            "HOME"
            home
            (lambda ()
              (let ([session (ssh-open "127.0.0.1" port user)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and
                     (eq? (ssh-auth-publickey! session user) session)
                     (begin
                       (ssh-close session)
                       #t)
                     (sftp-error-message-contains?
                      "SSH session is closed"
                      (lambda ()
                        (sftp-open session)))))
                  (lambda ()
                    (ssh-close session)))))))
         (lambda ()
           (stop-server)))))
