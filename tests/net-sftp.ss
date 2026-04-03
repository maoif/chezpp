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
