(import (chezpp chez)
        (chezpp net))

(define server-test-port
  (+ 19000 (modulo (get-process-id) 20000)))

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
