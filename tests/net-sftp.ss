(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(mat net-sftp
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-sftp-test remote-root home port user))
         (lambda ()
           (stop-server)))))
