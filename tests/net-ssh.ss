(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

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
                      (ssh-test-nonblocking-io session)
                      (ssh-test-shell session)
                      (ssh-test-port-wrappers session)))
                   (lambda () (ssh-close session))))))))
         (lambda ()
           (stop-server)))))
