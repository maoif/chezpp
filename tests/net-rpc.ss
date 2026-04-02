(import (chezpp)
        (chezpp net))

(load "net-common.ss")

(define-rpc-message rpc-hello-request
  ([1 name string])
  ([2 language (optional string)]))

(define-rpc-message rpc-hello-reply
  ([1 message string]))

(define-rpc-message rpc-note-request
  ([1 text string]))

(define-rpc-service rpc-greeter
  (rpc hello
    (request rpc-hello-request)
    (response rpc-hello-reply)))

(define-rpc-service rpc-admin
  (rpc note
    (request rpc-note-request)
    (response rpc-hello-reply)))

(mat net-rpc
     (let* ([port (reserve-loopback-port)]
            [server (rpc-open 'server "127.0.0.1" port)]
            [seen-note #f])
       (rpc-register-handler!
        server
        rpc-greeter-hello
        (lambda (req)
          (let ([msg (rpc-request-payload req)])
            (when (equal? (rpc-hello-request-name msg) "slow")
              (milisleep 100))
            (make-rpc-hello-reply
             (string-append "hello "
                            (rpc-hello-request-name msg)
                            "/"
                            (or (rpc-hello-request-language msg) "default"))))))
       (rpc-register-handler!
        server
        rpc-admin-note
        (lambda (req)
          (set! seen-note (rpc-note-request-text (rpc-request-payload req)))
          (make-rpc-hello-reply "ok")))
       (let ([th (fork-thread
                  (lambda ()
                    (let loop ([n 7])
                      (when (> n 0)
                        (guard (c [else #f])
                          (rpc-serve server))
                        (loop (- n 1))))))])
         (dynamic-wind
           void
           (lambda ()
             (and
              (rpc-request? (rpc-request rpc-admin-note (make-rpc-note-request "x") #t))
              (rpc-response? (rpc-response "ok"))
              (not (rpc-serve/nonblocking server))
              (let ([client (rpc-open "127.0.0.1" port)])
                (dynamic-wind
                  void
                  (lambda ()
                    (let ([reply-ok?
                           (lambda (reply message)
                             (and (rpc-hello-reply? reply)
                                  (equal? (rpc-hello-reply-message reply) message)))]
                          [wait-call
                           (lambda (name language timeout-ms)
                             (let loop ()
                               (let ([reply (rpc-call/nonblocking
                                             client
                                             rpc-greeter-hello
                                             (make-rpc-hello-request name language)
                                             timeout-ms)])
                                 (if reply
                                     reply
                                     (begin
                                       (milisleep 10)
                                       (loop))))))]
                          [call-times-out?
                           (lambda (thunk)
                             (guard (c [else
                                        (and (net-error? c)
                                             (equal? (net-error-message c)
                                                     "RPC call timed out"))])
                               (thunk)
                               #f))])
                      (and
                       (reply-ok? (rpc-call client
                                            rpc-greeter-hello
                                            (make-rpc-hello-request "alice" #f))
                                  "hello alice/default")
                       (call-times-out?
                        (lambda ()
                          (rpc-call client
                                    rpc-greeter-hello
                                    (make-rpc-hello-request "slow" #f)
                                    10)))
                       (reply-ok? (rpc-call client
                                            rpc-greeter-hello
                                            (make-rpc-hello-request "carol" "en"))
                                  "hello carol/en")
                       (eq? (rpc-notify client rpc-admin-note (make-rpc-note-request "note-1"))
                            client)
                       (reply-ok? (wait-call "bob" "fr" 200)
                                  "hello bob/fr")
                       (call-times-out?
                        (lambda ()
                          (wait-call "slow" #f 10)))
                       (reply-ok? (wait-call "dave" "es" 200)
                                  "hello dave/es"))))
                  (lambda ()
                    (rpc-close client))))
               (equal? seen-note "note-1")))
           (lambda ()
             (thread-join th)
             (rpc-close server))))))
