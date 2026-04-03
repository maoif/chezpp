(import (chezpp)
        (chezpp net))

(load "net-common.ss")

(define tls-net-error-timeout?
  (lambda (thunk)
    (guard (c [else
               (and (net-error? c)
                    (string-contains? (net-error-message c) "timed out"))])
      (thunk)
      #f)))

(define tls-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                     (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define poll-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define start-stalled-tls-handshake-server
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

(mat net-errors
     (let ([err (make-net-error 'net-test 'parse "bad address" '(1 2 3))])
       (and (net-error? err)
            (eq? (net-error-who err) 'net-test)
            (eq? (net-error-kind err) 'parse)
            (equal? (net-error-message err) "bad address")
            (equal? (net-error-data err) '(1 2 3))))
     (guard (c [else (and (net-error? c)
                          (eq? (net-error-kind c) 'io))])
       (raise-net-error 'net-test 'io "closed")
       #f))

(mat net-ip
     (let ([ipv4 (string->ip-address "127.0.0.1")]
           [ipv6 (string->ip-address "2001:db8::1")])
       (and (ipv4-address? ipv4)
            (ipv6-address? ipv6)
            (equal? (ip-address->string ipv4) "127.0.0.1")
            (equal? (ip-address->string ipv6) "2001:db8::1")
            (ip-address-loopback? ipv4)
            (not (ip-address-loopback? ipv6))
            (ip-address-private? (string->ip-address "192.168.2.10"))
            (ip-address-private? (string->ip-address "fd12:3456::7"))
            (ip-address-multicast? (string->ip-address "239.1.2.3"))
            (ip-address-multicast? (string->ip-address "ff02::1"))))
     (let ([range (cidr-parse "192.168.10.0/24")])
       (and (cidr-contains? range (string->ip-address "192.168.10.42"))
            (not (cidr-contains? range (string->ip-address "192.168.11.42")))
            (equal? (ip-address->string (cidr-network-address range))
                    "192.168.10.0")
            (= (cidr-prefix-length range) 24)))
     (let ([range (cidr-parse "2001:db8::/32")])
       (and (cidr-contains? range (string->ip-address "2001:db8::9"))
            (not (cidr-contains? range (string->ip-address "2001:db9::1")))))
     (not (string->ip-address "999.0.0.1"))
     (not (cidr-parse "192.168.0.1/40")))

(mat net-uri
     (let ([u (string->uri "https://alice@example.com:443/a/../b/c?q=1&x=two#frag")])
       (and (uri? u)
            (equal? (uri-scheme u) "https")
            (equal? (uri-userinfo u) "alice")
            (equal? (uri-host u) "example.com")
            (= (uri-port u) 443)
            (equal? (uri-path u) "/a/../b/c")
            (equal? (uri-query u) "q=1&x=two")
            (equal? (uri-fragment u) "frag")
            (equal? (uri-authority u) "alice@example.com:443")
            (equal? (uri-path-segments u) '("a" ".." "b" "c"))
            (equal? (uri-query-alist u) '(("q" . "1") ("x" . "two")))
            (equal? (uri->string (uri-normalize u))
                    "https://alice@example.com/b/c?q=1&x=two#frag")))
     (let* ([base (string->uri "https://example.com/a/b/index.html")]
            [ref (string->uri "../api?q=test")]
            [resolved (uri-resolve base ref)])
       (equal? (uri->string resolved)
               "https://example.com/a/api?q=test"))
     (equal? (uri-encode "hello world/ok") "hello%20world%2Fok")
     (equal? (uri-decode "hello%20world%2Fok") "hello world/ok")
     (equal? (form-urlencode '(("q" . "hello world") ("lang" . "scheme")))
             "q=hello+world&lang=scheme")
     (equal? (form-urldecode "q=hello+world&lang=scheme")
             '(("q" . "hello world") ("lang" . "scheme"))))

(mat net-http
     (let* ([req (make-http-request 'get "https://example.com/api?q=1"
                                    '(("Accept" . "application/json")
                                      ("X-Test" . "one"))
                                    #vu8(1 2 3))]
            [headers0 (http-request-headers req)]
            [headers1 (http-header-add headers0 "X-Test" "two")]
            [headers2 (http-header-set headers1 'accept "text/plain")]
            [resp (make-http-response 200 "OK" headers2 "done")])
       (and (http-request? req)
            (equal? (http-request-method req) "GET")
            (equal? (uri->string (http-request-uri req))
                    "https://example.com/api?q=1")
            (equal? (http-header-ref headers0 "accept") "application/json")
            (equal? (http-header-ref headers1 "x-test") "one")
            (equal? (http-header-ref headers2 "accept") "text/plain")
            (equal? (http-request-body req) #vu8(1 2 3))
            (http-response? resp)
            (= (http-response-status resp) 200)
            (equal? (http-response-reason resp) "OK")
            (equal? (http-response-body resp) "done"))))

(mat net-address-dns
     (let ([addr (make-socket-address 'inet "127.0.0.1" 8080)])
       (and (socket-address? addr)
            (eq? (socket-address-family addr) 'inet)
            (equal? (socket-address-host addr) "127.0.0.1")
            (= (socket-address-port addr) 8080)
            (not (socket-address-path addr))))
     (let ([addr (make-socket-address 'unix "/tmp/chezpp-net.sock")])
       (and (socket-address? addr)
            (eq? (socket-address-family addr) 'unix)
            (equal? (socket-address-path addr) "/tmp/chezpp-net.sock")))
     (let ([addr (resolve-address "127.0.0.1" 80 'inet 'stream)])
       (and (socket-address? addr)
            (eq? (socket-address-family addr) 'inet)))
     (let ([addrs (resolve-addresses "localhost" 80)])
       (and (pair? addrs)
            (andmap socket-address? addrs)))
     (let ([res (dns-resolve "localhost")])
       (and (dns-result? res)
            (pair? (dns-result-addresses res))
            (or (not (dns-result-canonname res))
                (string? (dns-result-canonname res)))))
     (let ([name (dns-reverse-resolve (make-socket-address 'inet "127.0.0.1" 80))])
       (string? name)))

(mat net-socket
     (let-values ([(server port th)
                   (start-echo-server
                    (lambda (client peer)
                      (let ([payload (socket-recv client 32)])
                        (socket-send-all client payload))))])
       (let ([client (open-socket 'inet 'stream)])
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([local (socket-local-address client)]
               [peer (socket-peer-address client)]
               [payload (string->utf8 "ping")])
           (and (socket-address? local)
                (socket-address? peer)
                (socket-send-all client payload)
                (equal? (socket-recv client 32) payload)
                (begin
                  (close-socket client)
                  (thread-join th)
                  #t)))))
     (let-values ([(server port th)
                   (start-echo-server
                    (lambda (client peer)
                      (let ([payload (socket-recv client 32)])
                        (socket-send-all client payload))))])
       (let ([client (open-socket 'inet 'stream)])
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (call-with-socket-ports
          client
          (lambda (ip op)
            (put-bytevector op (string->utf8 "port"))
            (flush-output-port op)
            (equal? (get-bytevector-n ip 4) (string->utf8 "port"))))
         (close-socket client)
         (thread-join th)
         #t))
     (let ([server (open-socket 'inet 'stream)])
       (socket-set-option! server 'reuse-address #t)
       (socket-bind! server (make-socket-address 'inet "127.0.0.1" 0))
       (socket-listen! server 4)
       (let ([no-client (socket-accept/nonblocking server)]
             [reuse? (socket-get-option server 'reuse-address)])
         (close-socket server)
         (and (not no-client)
              reuse?))))

(mat net-poll
     (let ([server (open-socket 'inet 'stream)])
       (socket-set-option! server 'reuse-address #t)
       (socket-bind! server (make-socket-address 'inet "127.0.0.1" 0))
       (socket-listen! server 2)
       (let* ([target (make-poll-target server '(read))]
              [before (poll/nonblocking (list target))]
              [port (socket-address-port (socket-local-address server))]
              [client (open-socket 'inet 'stream)])
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([after (poll (list target) 100)])
           (close-socket client)
           (close-socket server)
           (and (null? (poll-target-ready-events (car before)))
                (not (not (memq 'read (poll-target-ready-events (car after))))))))))

(mat net-poll-validation
     (and
      (poll-error-message-contains?
       "poll timeout must be -1 or non-negative"
       (lambda ()
         (poll '() -2)))
      (poll-error-message-contains?
       "poll events must be a list"
       (lambda ()
         (make-poll-target 0 'read)))
      (poll-error-message-contains?
       "invalid poll event"
       (lambda ()
         (make-poll-target 0 '(bogus))))))

(mat net-socket-validation
     (let ([sock (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (and
            (tls-error-message-contains?
             "size must be non-negative"
             (lambda ()
               (socket-recv sock -1)))
            (tls-error-message-contains?
             "size must be non-negative"
             (lambda ()
               (socket-recv/nonblocking sock -1)))))
         (lambda ()
           (close-socket sock)))))

(mat net-tls
     (let-values ([(server server-ctx port th) (start-tls-echo-server)])
       (let ([client (open-socket 'inet 'stream)]
             [ctx (make-tls-context 'client)])
         (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
         (tls-context-set-verify! ctx #t)
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([session (tls-connect ctx client "localhost")]
               [payload (string->utf8 "tls")])
           (and (tls-session? session)
                (tls-verified? session)
                (certificate? (tls-peer-certificate session))
                (list? (tls-peer-certificate-chain session))
                (string? (tls-protocol-version session))
                (string? (tls-cipher-name session))
                (tls-write-all session payload)
                (equal? (tls-read session 32) payload)
                (begin
                  (close-tls-session session)
                  (close-tls-context ctx)
                  (close-socket client)
                  (thread-join th)
                  #t)))))
     (let-values ([(server server-ctx port th) (start-tls-echo-server)])
       (let ([client (open-socket 'inet 'stream)]
             [ctx (make-tls-context 'client)])
         (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
         (tls-context-set-verify! ctx #t)
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([session (tls-connect ctx client "localhost")])
           (and
            (call-with-tls-ports
             session
             (lambda (ip op)
               (put-bytevector op (string->utf8 "port"))
               (flush-output-port op)
               (equal? (get-bytevector-n ip 4) (string->utf8 "port"))))
            (begin
              (close-tls-session session)
              (close-tls-context ctx)
              (close-socket client)
              (thread-join th)
              #t)))))
     (let-values ([(server server-ctx port th) (start-tls-echo-server)])
       (let ([client (open-socket 'inet 'stream)]
             [ctx (make-tls-context 'client)])
         (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
         (tls-context-set-verify! ctx #t)
         (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
         (let ([session (tls-connect ctx client "localhost")]
               [buf (make-bytevector 8 0)])
           (socket-set-blocking! client #f)
           (let ([idle (tls-read/nonblocking session 8)]
                 [write-ready (poll (list (make-poll-target client '(write))) 100)])
             (let ([sent (tls-write/nonblocking session (string->utf8 "nb"))]
                   [read-target (make-poll-target client '(read))])
               (let loop ([attempt 8])
                 (let ([n (tls-read!/nonblocking session buf 0 2)])
                   (if (eq? n #f)
                       (if (fx= attempt 0)
                           (begin
                             (close-tls-session session)
                             (close-tls-context ctx)
                             (close-socket client)
                             (thread-join th)
                             #f)
                           (begin
                             (poll (list read-target) 100)
                             (loop (fx1- attempt))))
                       (begin
                         (close-tls-session session)
                         (close-tls-context ctx)
                         (close-socket client)
                         (thread-join th)
                         (and (not idle)
                              (memq 'write (poll-target-ready-events (car write-ready)))
                              sent
                              (= n 2)
                              (equal? (slice-bytevector buf 0 2) (string->utf8 "nb")))))))))))))

(mat net-tls-timeout
     (let-values ([(listener port th)
                   (start-stalled-tls-handshake-server 200)])
       (let ([client (open-socket 'inet 'stream)]
             [ctx (make-tls-context 'client)])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
             (tls-net-error-timeout?
              (lambda ()
                (call-with-tls-client ctx client 50 tls-session?))))
           (lambda ()
             (close-tls-context ctx)
             (guard (c [else #f])
               (close-socket client))
             (thread-join th)
             (guard (c [else #f])
               (close-socket listener))))))
     (let ([listener (open-socket 'inet 'stream)]
           [server-ctx (make-tls-context 'server)])
       (write-test-cert-files)
       (tls-context-load-cert! server-ctx "/tmp/chezpp-net-test-cert.pem")
       (tls-context-load-private-key! server-ctx "/tmp/chezpp-net-test-key.pem")
       (socket-set-option! listener 'reuse-address #t)
       (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
       (socket-listen! listener 4)
       (let ([port (socket-address-port (socket-local-address listener))]
             [client (open-socket 'inet 'stream)])
         (dynamic-wind
           void
           (lambda ()
             (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
             (let-values ([(accepted peer) (socket-accept listener)])
               (dynamic-wind
                 void
                 (lambda ()
                   (tls-net-error-timeout?
                    (lambda ()
                      (call-with-tls-server server-ctx accepted 50 tls-session?))))
                 (lambda ()
                   (guard (c [else #f])
                     (close-socket accepted))))))
           (lambda ()
             (close-tls-context server-ctx)
             (guard (c [else #f])
               (close-socket client))
             (guard (c [else #f])
               (close-socket listener)))))))

(mat net-tls-timeout-validation
     (let ([client-ctx (make-tls-context 'client)]
           [server-ctx (make-tls-context 'server)]
           [sock (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (and
            (tls-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (tls-connect client-ctx sock #f -1)))
            (tls-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (tls-accept server-ctx sock -1)))
            (tls-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (call-with-tls-client client-ctx sock -1 tls-session?)))
            (tls-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (call-with-tls-server server-ctx sock -1 tls-session?)))))
         (lambda ()
           (close-tls-context client-ctx)
           (close-tls-context server-ctx)
           (close-socket sock))))
     (let-values ([(server server-ctx port th) (start-tls-echo-server)])
       (let ([client (open-socket 'inet 'stream)]
             [ctx (make-tls-context 'client)]
             [payload (string->utf8 "x")]
             [buf (make-bytevector 1 0)])
         (dynamic-wind
           void
           (lambda ()
             (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
             (tls-context-set-verify! ctx #t)
             (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
             (let ([session (tls-connect ctx client "localhost")])
               (dynamic-wind
                 void
                 (lambda ()
                   (and
                    (tls-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (tls-read session 1 -1)))
                    (tls-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (tls-read! session buf 0 1 -1)))
                    (tls-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (tls-write session payload 0 1 -1)))
                    (tls-error-message-contains?
                     "timeout must be non-negative"
                     (lambda ()
                       (tls-write-all session payload 0 1 -1)))))
                 (lambda ()
                   (close-tls-session session)))))
           (lambda ()
             (close-tls-context ctx)
             (close-socket client)
             (thread-join th))))))

(mat net-tls-size-validation
     (let-values ([(server server-ctx port th) (start-tls-echo-server)])
       (let ([client (open-socket 'inet 'stream)]
             [ctx (make-tls-context 'client)])
         (dynamic-wind
           void
           (lambda ()
             (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
             (tls-context-set-verify! ctx #t)
             (socket-connect! client (make-socket-address 'inet "127.0.0.1" port))
             (let ([session (tls-connect ctx client "localhost")])
               (dynamic-wind
                 void
                 (lambda ()
                   (and
                    (tls-error-message-contains?
                     "size must be non-negative"
                     (lambda ()
                       (tls-read session -1)))
                    (tls-error-message-contains?
                     "size must be non-negative"
                     (lambda ()
                       (tls-read/nonblocking session -1)))))
                 (lambda ()
                   (close-tls-session session)))))
           (lambda ()
             (close-tls-context ctx)
             (close-socket client)
             (thread-join th))))))
