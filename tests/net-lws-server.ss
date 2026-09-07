(import (chezpp chez)
        (chezpp net))

(define server-test-port
  (+ 19000 (modulo (get-process-id) 20000)))

(mat net-lws-server-refuses-h2-on-http1-listener
     ;; Error case: explicit H2 must fail against an HTTP/1 listener without hanging or downgrading.
     (let* ([port (+ server-test-port 4)]
            [server (http-listen "127.0.0.1" port)]
            [client (http-open)])
       (dynamic-wind
         void
         (lambda ()
           (http-client-version-set! client 'h2)
           (http-set-timeout! client 200)
           (guard (condition [(condition? condition) #t] [else #f])
             (http-send client
                        (make-http-request 'get
                                           (format "http://127.0.0.1:~a/h2" port)))
             #f))
         (lambda () (http-close client) (http-server-close server)))))

(mat net-lws-server-handler-registry
     (let ([server (http-listen "127.0.0.1" server-test-port)])
       (dynamic-wind
         void
         (lambda ()
           (let ([handler (lambda (request)
                            (make-http-response 200 "OK" '() "ok" '() 'h1))])
             (and (not (http-register-handler! server 'get "/hello" handler))
                  (eq? handler (http-handler-ref server 'get "/hello" #f))
                  (eq? handler (http-unregister-handler! server 'get "/hello"))
                  (not (http-handler-ref server 'get "/hello" #f)))))
         (lambda () (http-server-close server)))))

(mat net-lws-server-close-is-idempotent
     (let ([server (http-listen "127.0.0.1" (+ server-test-port 1))])
       (and (eq? server (http-server-close server))
            (eq? server (http-server-close server)))))

(mat net-lws-server-live-http1
     (let* ([port (+ server-test-port 2)]
            [server (http-listen "127.0.0.1" port)]
            [server-thread #f])
       (dynamic-wind
         void
         (lambda ()
           (http-register-handler!
            server 'get "/live"
            (lambda (request)
              (make-http-response 200 "OK" '() "live-ok" '() 'h1)))
           (set! server-thread (fork-thread (lambda () (http-serve server))))
           (let ([response (http-get (format "http://127.0.0.1:~a/live" port))])
             (thread-join server-thread)
             (and (= (http-response-status response) 200)
                  (equal? (utf8->string (http-response-body response)) "live-ok"))))
         (lambda () (http-server-close server)))))

(mat net-lws-server-live-request-body
     (let* ([port (+ server-test-port 3)]
            [server (http-listen "127.0.0.1" port)]
            [server-thread #f])
       (dynamic-wind
         void
         (lambda ()
           (http-register-handler!
            server 'post "/body"
            (lambda (request)
              (make-http-response 200 "result" '()
                                  (format "~s" (http-request-body request)) '() 'h1)))
           (set! server-thread (fork-thread (lambda () (http-serve server))))
           (let ([response
                  (http-request 'post (format "http://127.0.0.1:~a/body" port)
                                '() #vu8(97 98 99))])
             (thread-join server-thread)
             (and (= (http-response-status response) 200)
                  (equal? (utf8->string (http-response-body response))
                          "#vu8(97 98 99)"))))
         (lambda () (http-server-close server)))))

(mat net-lws-server-live-header-roundtrip
     (let* ([port (+ server-test-port 5)]
            [server (http-listen "127.0.0.1" port)]
            [client (http-open)]
            [received #f]
            [server-thread #f])
       (dynamic-wind
         void
         (lambda ()
           (http-set-timeout! client 1000)
           (http-register-handler!
            server 'get "/headers"
            (lambda (request)
              (set! received (http-header-ref (http-request-headers request) "x-input" #f))
              (make-http-response 200 "OK" '(("X-Result" . "kept")
                                             ("Cache-Control" . "no-store"))
                                  "headers-ok" '() 'h1)))
           (set! server-thread (fork-thread (lambda () (http-serve server))))
           (let ([response (http-send client
                            (make-http-request 'get
                              (format "http://127.0.0.1:~a/headers" port)
                              '(("X-Input" . "received")) #f))])
             (thread-join server-thread)
             (and (equal? "received" received)
                  (equal? "kept" (http-header-ref (http-response-headers response)
                                                "x-result" #f))
                  (equal? "no-store" (http-header-ref (http-response-headers response)
                                                    "cache-control" #f)))))
         (lambda () (http-close client) (http-server-close server)))))
