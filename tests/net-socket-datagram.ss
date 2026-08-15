(import (chezpp))

(mat net-socket-datagram
     (let ([receiver (open-socket 'inet 'datagram)]
           [sender (open-socket 'inet 'datagram)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! receiver (make-socket-address 'inet "127.0.0.1" 0))
           (let* ([port (socket-address-port (socket-local-address receiver))]
                  [target (make-socket-address 'inet "127.0.0.1" port)]
                  [payload (string->utf8 "datagram")])
             (and (= (bytevector-length payload)
                     (socket-send-to sender payload target))
                  (let-values ([(received source) (socket-recv-from receiver 128)])
                    (and (equal? payload received)
                         (eq? 'inet (socket-address-family source))
                         (string=? "127.0.0.1" (socket-address-host source)))))))
         (lambda ()
           (close-socket sender)
           (close-socket receiver)))))
