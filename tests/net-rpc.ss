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
                    (rpc-serve server)
                    (rpc-serve server)
                    (rpc-serve server)))])
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
                    (and
                     (let ([reply (rpc-call client
                                            rpc-greeter-hello
                                            (make-rpc-hello-request "alice" #f))])
                       (and (rpc-hello-reply? reply)
                            (equal? (rpc-hello-reply-message reply)
                                    "hello alice/default")))
                     (eq? (rpc-notify client rpc-admin-note (make-rpc-note-request "note-1"))
                          client)
                     (let loop ()
                       (let ([reply (rpc-call/nonblocking
                                     client
                                     rpc-greeter-hello
                                     (make-rpc-hello-request "bob" "fr"))])
                         (if reply
                             (and (rpc-hello-reply? reply)
                                  (equal? (rpc-hello-reply-message reply)
                                          "hello bob/fr"))
                             (begin
                               (milisleep 10)
                               (loop)))))))
                  (lambda ()
                    (rpc-close client))))
               (equal? seen-note "note-1")))
           (lambda ()
             (thread-join th)
             (rpc-close server))))))
