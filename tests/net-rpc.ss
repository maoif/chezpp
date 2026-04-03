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

(define-rpc-message rpc-watch-request
  ([1 count fixnum])
  ([2 prefix string]))

(define-rpc-message rpc-sum-part
  ([1 value fixnum]))

(define-rpc-message rpc-sum-reply
  ([1 total fixnum]))

(define-rpc-message rpc-chat-request
  ([1 text string]))

(define-rpc-message rpc-chat-reply
  ([1 text string]))

(define-rpc-service rpc-greeter
  (rpc hello
    (request rpc-hello-request)
    (response rpc-hello-reply)))

(define-rpc-service rpc-admin
  (rpc note
    (request rpc-note-request)
    (response rpc-hello-reply)))

(define-rpc-service rpc-streaming
  (rpc watch
    (stream server)
    (request rpc-watch-request)
    (response rpc-hello-reply))
  (rpc sum
    (stream client)
    (request rpc-sum-part)
    (response rpc-sum-reply))
  (rpc chat
    (stream bidi)
    (request rpc-chat-request)
    (response rpc-chat-reply)))

(define rpc-reply-ok?
  (lambda (reply message)
    (and (rpc-hello-reply? reply)
         (equal? (rpc-hello-reply-message reply) message))))

(define wait-rpc-call
  (lambda (client name language timeout-ms)
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
              (loop)))))))

(define rpc-call-times-out?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (equal? (net-error-message c)
                            "RPC call timed out"))])
      (thunk)
      #f)))

(define rpc-server-stream-ok?
  (lambda (client)
    (let ([stream (rpc-call/server-stream
                   client
                   rpc-streaming-watch
                   (make-rpc-watch-request 3 "tick-"))])
      (let ([ok?
             (let ([m1 (rpc-stream-recv stream)]
                   [m2 (rpc-stream-recv stream)]
                   [m3 (rpc-stream-recv stream)]
                   [done (rpc-stream-recv stream)])
               (and (rpc-hello-reply? m1)
                    (rpc-hello-reply? m2)
                    (rpc-hello-reply? m3)
                    (equal? (map rpc-hello-reply-message (list m1 m2 m3))
                            '("tick-0" "tick-1" "tick-2"))
                    (eof-object? done)))])
        (rpc-stream-close stream)
        ok?))))

(define rpc-client-stream-ok?
  (lambda (client)
    (let ([stream (rpc-call/client-stream client rpc-streaming-sum)])
      (let ([ok?
             (begin
               (rpc-stream-send stream (make-rpc-sum-part 3))
               (rpc-stream-send stream (make-rpc-sum-part 5))
               (rpc-stream-send stream (make-rpc-sum-part 8))
               (rpc-stream-close-send stream)
               (let ([reply (rpc-stream-recv stream)]
                     [done (rpc-stream-recv stream)])
                 (and (rpc-sum-reply? reply)
                      (= (rpc-sum-reply-total reply) 16)
                      (eof-object? done))))])
        (rpc-stream-close stream)
        ok?))))

(define rpc-bidi-stream-ok?
  (lambda (client)
    (let ([stream (rpc-call/bidi-stream client rpc-streaming-chat)])
      (let ([ok?
             (begin
               (rpc-stream-send stream (make-rpc-chat-request "one"))
               (let ([r1 (rpc-stream-recv stream)])
                 (if (and (rpc-chat-reply? r1)
                          (equal? (rpc-chat-reply-text r1) "echo:one"))
                     (begin
                       (rpc-stream-send stream (make-rpc-chat-request "two"))
                       (let ([r2 (rpc-stream-recv stream)])
                         (and (rpc-chat-reply? r2)
                              (equal? (rpc-chat-reply-text r2) "echo:two")
                              (begin
                                (rpc-stream-close-send stream)
                                (eof-object? (rpc-stream-recv stream))))))
                     #f)))])
        (rpc-stream-close stream)
        ok?))))

(mat net-rpc
     (let* ([port (reserve-loopback-port)]
            [server (rpc-open 'server "127.0.0.1" port)]
            [seen-note #f]
            [done? #f])
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
       (rpc-register-handler!
        server
        rpc-streaming-watch
        (lambda (req stream)
          (let ([msg (rpc-request-payload req)])
            (let loop ([i 0])
              (when (< i (rpc-watch-request-count msg))
                (rpc-stream-send
                 stream
                 (make-rpc-hello-reply
                  (format "~a~a"
                          (rpc-watch-request-prefix msg)
                          i)))
                (loop (+ i 1)))))))
       (rpc-register-handler!
        server
        rpc-streaming-sum
        (lambda (stream)
          (let loop ([total 0])
            (let ([msg (rpc-stream-recv stream)])
              (if (eof-object? msg)
                  (make-rpc-sum-reply total)
                  (loop (+ total (rpc-sum-part-value msg))))))))
       (rpc-register-handler!
        server
        rpc-streaming-chat
        (lambda (stream)
          (let loop ()
            (let ([msg (rpc-stream-recv stream)])
              (unless (eof-object? msg)
                (rpc-stream-send
                 stream
                 (make-rpc-chat-reply
                  (string-append "echo:" (rpc-chat-request-text msg))))
                (loop))))))
       (let ([th (fork-thread
                  (lambda ()
                    (let loop ()
                      (unless done?
                        (guard (c [else #f])
                          (rpc-serve server))
                        (loop)))))])
         (let ([ok?
                (and
                 (rpc-request? (rpc-request rpc-admin-note (make-rpc-note-request "x") #t))
                 (rpc-response? (rpc-response "ok"))
                 (not (rpc-serve/nonblocking server))
                 (let ([client (rpc-open "127.0.0.1" port)])
                   (let ([client-ok?
                          (and
                           (rpc-reply-ok? (rpc-call client
                                                    rpc-greeter-hello
                                                    (make-rpc-hello-request "alice" #f))
                                          "hello alice/default")
                           (rpc-call-times-out?
                            (lambda ()
                              (rpc-call client
                                        rpc-greeter-hello
                                        (make-rpc-hello-request "slow" #f)
                                        10)))
                           (rpc-reply-ok? (rpc-call client
                                                    rpc-greeter-hello
                                                    (make-rpc-hello-request "carol" "en"))
                                          "hello carol/en")
                           (eq? (rpc-notify client rpc-admin-note (make-rpc-note-request "note-1"))
                                client)
                           (rpc-reply-ok? (wait-rpc-call client "bob" "fr" 200)
                                          "hello bob/fr")
                           (rpc-call-times-out?
                            (lambda ()
                              (wait-rpc-call client "slow" #f 10)))
                           (rpc-server-stream-ok? client)
                           (rpc-client-stream-ok? client)
                           (rpc-bidi-stream-ok? client)
                           (rpc-reply-ok? (wait-rpc-call client "dave" "es" 200)
                                          "hello dave/es"))])
                     (rpc-close client)
                     client-ok?))
                 (equal? seen-note "note-1"))])
           (set! done? #t)
           (guard (c [else #f])
             (let ([wake (open-socket 'inet 'stream)])
               (socket-connect! wake (make-socket-address 'inet "127.0.0.1" port))
               (close-socket wake)))
           (thread-join th)
           (rpc-close server)
           ok?))))
