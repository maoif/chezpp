(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(define ssh-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (or (string-contains? (net-error-message c) "timed out")
                        (string-contains? (net-error-message c) "Timeout")))])
      (thunk)
      #f)))

(define start-stalled-ssh-banner-server
  (lambda (delay-ms)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values listener
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (milisleep delay-ms)
                     (close-socket client)
                     (close-socket listener)))))))))

(mat net-ssh
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (with-env
            "HOME"
            home
            (lambda ()
              (and
               (call-with-ssh-session "127.0.0.1" port user ssh-session?)
               (call-with-ssh-session "127.0.0.1" port user 2000 ssh-session?)
               (let ([session (ssh-open "127.0.0.1" port user)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (and
                      (guard (c [else (net-error? c)])
                        (ssh-auth-password! session user "bad-password")
                        #f)
                      (guard (c [else (net-error? c)])
                        (ssh-auth-agent! session user)
                        #f)
                      (eq? (ssh-auth-publickey! session user) session)
                      (ssh-test-pty session)
                      (ssh-test-read-slice session)
                      (ssh-test-session-exec session)
                      (ssh-test-blocking-write session)
                      (ssh-test-read-timeout session ssh-net-error-timeout?)
                      (ssh-test-setup-timeouts session remote-root ssh-net-error-timeout?)
                      (ssh-test-nonblocking-io session)
                      (ssh-test-shell session)
                      (ssh-test-port-wrappers session)))
                   (lambda ()
                     (ssh-close session))))))))
         (lambda ()
           (stop-server)))))
     (let-values ([(listener port th)
                   (start-stalled-ssh-banner-server 200)])
       (dynamic-wind
         void
         (lambda ()
           (ssh-net-error-timeout?
            (lambda ()
              (ssh-open "127.0.0.1" port #f 50))))
         (lambda ()
           (thread-join th)
           (guard (c [else #f])
             (close-socket listener)))))
