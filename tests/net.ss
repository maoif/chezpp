(import (chezpp)
        (chezpp net))

(define tls-test-certificate
  (string->utf8
   "-----BEGIN CERTIFICATE-----\nMIIDCTCCAfGgAwIBAgIUZBmYij7LabaZKVKlJk2qLuigxGkwDQYJKoZIhvcNAQEL\nBQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDMzMTEyMjU1NloXDTI3MDMz\nMTEyMjU1NlowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF\nAAOCAQ8AMIIBCgKCAQEAjA9cXUf3ZEqsRg112cBl/SLIlnOoNdoauWKa7mII6ZiU\nHgh1x8Ety0mU9U4A992FPpFDMQznDAzGwUyoRtQ0p19NH0NZFtXATYoho5D9fSmC\nvJlW6L8efh1YIvO+BEYsa5UNcumagyctmsIhNHSgdacyruiBQtx8TPv3kGlsetqv\no91ZxKFP2ETKR2OWICkq6lADJf59+igjiLSs593c2olRNbmKVyMXx52i5EOsNbhx\ntxhEqB3WdTzbDN9oS0tpw1OFf95ps/40O9QlMnt6nYYWMnaPbtVilzq94uvE4OpN\n7pH+tv+ehvxApIqFVaCg84ddDW9lIUmQn3dNDLa3WQIDAQABo1MwUTAdBgNVHQ4E\nFgQUbIHt/Fxargqo4TSGMRy1zbaIbacwHwYDVR0jBBgwFoAUbIHt/Fxargqo4TSG\nMRy1zbaIbacwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAI8DH\nehYADEh0HWP/Y34VAY9pbx7Kx06UZQfJME6HCAPoBAEDIVGI0Y6GMUlKtetU9SPk\n6/4/nVPg5bjCOnQx0SU3yCJdzHmZJC9GPXeqkZhjtrRQTneMnAz1Nae6bDCS6iOw\nM5nLdrao+MUYrZWimOr9/tJMHV9ffLoULXsj1bPSEZ8OJEa9Y+n+JSYLo4N35iXJ\nYkMIxVoxdStnwZyrWYgmTAIG62kQ7UmpcyQG+HVjeL5LRXTZFlwARc021HCKM1EH\nU+Bgt/w0C8m2CFenpQqnEfw2tgNzHeaIxCLHlAesUfvNrGfqVIvbV7WUyZdUEoyf\nJZGGecWkv1SBX4VXGw==\n-----END CERTIFICATE-----\n"))

(define tls-test-private-key
  (string->utf8
   "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCMD1xdR/dkSqxG\nDXXZwGX9IsiWc6g12hq5YpruYgjpmJQeCHXHwS3LSZT1TgD33YU+kUMxDOcMDMbB\nTKhG1DSnX00fQ1kW1cBNiiGjkP19KYK8mVbovx5+HVgi874ERixrlQ1y6ZqDJy2a\nwiE0dKB1pzKu6IFC3HxM+/eQaWx62q+j3VnEoU/YRMpHY5YgKSrqUAMl/n36KCOI\ntKzn3dzaiVE1uYpXIxfHnaLkQ6w1uHG3GESoHdZ1PNsM32hLS2nDU4V/3mmz/jQ7\n1CUye3qdhhYydo9u1WKXOr3i68Tg6k3ukf62/56G/ECkioVVoKDzh10Nb2UhSZCf\nd00MtrdZAgMBAAECggEADU8uX0NF9bbuc1yD41Sk1gh9C2YwYo54KKXl0OfqLrLl\nkr8KD8vb9RJOc35DzRbOh4YLq2vgjstHndNetrBWoPfUHwSDp3RZ3ErXUuQ/YMPv\nkDCs5HW44sVMfH6XnKKQeQt5pjHsEWRJMW7nwETVTkJvqG6552eG7+5BQoaKHPIE\nAn/3H7KsCseEgMPCcoDnocOdyqGHLe1buisT3k6qlCAP/QBS45LFXCROojyWgXQq\nDElzB6MqyjA9lTQ4YO8lsxwHTCe4FvD+CKH5E3WKWHSdJDsf2fmJyEaAy1oH9yYi\n4tn2mG0RoN0xeqIhCPUljQLujNkfJEPprAowggC2WwKBgQDEUULYmTjlf7wtl3TK\nlH7FzpYL8dRFdR1wQ1yq9DKrRNrlaHG0zNt5Kqm30ku1/oDchR56Y00UV2lxLACk\ndqNm2e24z0Jz4hDtLHXvcopskn+dILdMnew0OGN2PQ5gb1fOuuqtpS/OaTScYxiY\nezjFViaalbfgumR1BZwHaCbpxwKBgQC2o8UJYuCcExb2CIcFVKq20Tse2WWAEmGk\ni52M5ah8P0C8UaoEj+vW4BGrC3TXk8H34+XZPH3SAY0p2UxW8OVALcouiFtdBihh\nu629GbKF8GXm9TuZ5ou00lSqKGTVgiNfLRMRUmElv5ZOyA12OVWGIYyIFroSAbiS\nQeGx7z5V3wKBgQC4E+YVCN7jurktGsXlKgYQ3hutiYzbr+vxlwguOBnGpCKIVz2/\nJRNp8sn+1g4t0TztCVlBsxjUSP5Sosrba27eAtw3nQeXd1MdwMG4yvLmyRslr0aQ\nbcfMU09Xz/pKDD0OWA+y0KAZ8GXnebfXSjs6NgSukFJBQyTs4VyjSVKrgwKBgEHi\nzn/WVaS4Fj5nUR4RLwyIakV0s3MCLotHemyLpL49q0LESwseSDvZ1UXY+iuSuBSO\n+Cnn8pPBz4TbSPjMKkd+vUMQGbVzNToclE51aLt8v6YTrY6VZqyye6xuqgGD0vLQ\nteI3z6fod3awIHsXr8yVabbmS/WW/Vh1v8+KuPgNAoGBAKta9dICoGISq22PRgyb\ndEHFp95rTq9/FSdVhXllbhM5dfiY97uV/O9E5QiyJ9uAVReXccnkjLVyMnRUXhY/\nEoYIE4KO2iQ6JNafeuAEUgcmapt/b6VwHMVFyBBIi2FNzN4h6YkTXGl1lOEJDoVp\n0edea+G4bnZxUlckUv7+5nCV\n-----END PRIVATE KEY-----\n"))

(define write-bytevector-file
  (lambda (path bv)
    (call-with-port
     (open-file-output-port path
                            (file-options no-fail replace)
                            (buffer-mode block)
                            #f)
     (lambda (op)
       (put-bytevector op bv)))))

(define slice-bytevector
  (lambda (bv start stop)
    (let ([out (make-bytevector (- stop start) 0)])
      (bytevector-copy! bv start out 0 (- stop start))
      out)))

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

(define start-echo-server
  (lambda (handler)
    (let ([server (open-socket 'inet 'stream)])
      (socket-set-option! server 'reuse-address #t)
      (socket-bind! server (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! server 8)
      (let ([port (socket-address-port (socket-local-address server))])
        (values server
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept server)])
                     (handler client peer)
                     (close-socket client)
                     (close-socket server)))))))))

(define start-tls-echo-server
  (lambda ()
    (let ([server (open-socket 'inet 'stream)]
          [ctx (make-tls-context 'server)]
          [cert-path "/tmp/chezpp-net-test-cert.pem"]
          [key-path "/tmp/chezpp-net-test-key.pem"])
      (write-bytevector-file cert-path tls-test-certificate)
      (write-bytevector-file key-path tls-test-private-key)
      (tls-context-load-cert! ctx cert-path)
      (tls-context-load-private-key! ctx key-path)
      (socket-set-option! server 'reuse-address #t)
      (socket-bind! server (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! server 8)
      (let ([port (socket-address-port (socket-local-address server))])
        (values server
                ctx
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept server)])
                     (guard (c [else #f])
                       (let ([session (tls-accept ctx client)])
                         (let ([payload (tls-read session 32)])
                           (when (bytevector? payload)
                             (tls-write-all session payload)))
                         (guard (c [else #f])
                           (tls-shutdown! session))
                         (close-tls-session session)))
                         (close-socket client)
                         (close-socket server)
                         (close-tls-context ctx)))))))))

(define reserve-loopback-port
  (lambda ()
    (let ([sock (open-socket 'inet 'stream)])
      (socket-set-option! sock 'reuse-address #t)
      (socket-bind! sock (make-socket-address 'inet "127.0.0.1" 0))
      (let ([port (socket-address-port (socket-local-address sock))])
        (close-socket sock)
        port))))

(define write-test-cert-files
  (lambda ()
    (write-bytevector-file "/tmp/chezpp-net-test-cert.pem" tls-test-certificate)
    (write-bytevector-file "/tmp/chezpp-net-test-key.pem" tls-test-private-key)))

(define read-crlf-line
  (lambda (ip)
    (let loop ([rev '()])
      (let ([b (get-u8 ip)])
        (cond
         [(eof-object? b)
          (and (pair? rev)
               (utf8->string
                (u8-list->bytevector
                 (reverse rev))))]
         [(= b 10)
          (let ([rev (if (and (pair? rev) (= (car rev) 13))
                         (cdr rev)
                         rev)])
            (utf8->string (u8-list->bytevector (reverse rev))))]
         [else
          (loop (cons b rev))])))))

(define u8-list->bytevector
  (lambda (u8*)
    (let ([out (make-bytevector (length u8*) 0)])
      (let loop ([rest u8*] [i 0])
        (unless (null? rest)
          (bytevector-u8-set! out i (car rest))
          (loop (cdr rest) (+ i 1))))
      out)))

(define send-crlf-line
  (lambda (op line)
    (put-bytevector op (string->utf8 (string-append line "\r\n")))
    (flush-output-port op)))

(define join-path-segments
  (lambda (segments)
    (let loop ([rest segments] [out ""])
      (if (null? rest)
          out
          (loop (cdr rest)
                (if (string=? out "")
                    (car rest)
                    (string-append out "/" (car rest))))))))

(define normalize-absolute-test-path
  (lambda (path)
    (let loop ([rest (string-split path #\/)] [stack '()])
      (if (null? rest)
          (let ([joined (join-path-segments (reverse stack))])
            (if (string=? joined "")
                "/"
                (string-append "/" joined)))
          (let ([part (car rest)])
            (cond
             [(or (string=? part "") (string=? part "."))
              (loop (cdr rest) stack)]
             [(string=? part "..")
              (loop (cdr rest) (if (null? stack) '() (cdr stack)))]
             [else
              (loop (cdr rest) (cons part stack))]))))))

(define test-path-join
  (lambda (cwd path)
    (normalize-absolute-test-path
     (if (and (> (string-length path) 0)
              (char=? (string-ref path 0) #\/))
         path
         (if (string=? cwd "/")
             (string-append "/" path)
             (string-append cwd "/" path))))))

(define path-dirname
  (lambda (path)
    (let ([abs (normalize-absolute-test-path path)])
      (let loop ([i (- (string-length abs) 1)])
        (cond
         [(<= i 0) "/"]
         [(char=? (string-ref abs i) #\/)
          (if (= i 0)
              "/"
              (substring abs 0 i))]
         [else
          (loop (- i 1))])))))

(define file-basename
  (lambda (path)
    (let ([abs (normalize-absolute-test-path path)])
      (let loop ([i (- (string-length abs) 1)])
        (cond
         [(< i 0) abs]
         [(char=? (string-ref abs i) #\/)
          (substring abs (+ i 1) (string-length abs))]
         [else
          (loop (- i 1))])))))

(define read-port->bytevector
  (lambda (ip)
    (let loop ([parts '()] [total 0])
      (let ([chunk (get-bytevector-n ip 4096)])
        (if (eof-object? chunk)
            (let ([out (make-bytevector total 0)])
              (let fill ([rest (reverse parts)] [i 0])
                (if (null? rest)
                    out
                    (let* ([part (car rest)]
                           [n (bytevector-length part)])
                      (bytevector-copy! part 0 out i n)
                      (fill (cdr rest) (+ i n))))))
            (loop (cons chunk parts) (+ total (bytevector-length chunk))))))))

(define start-ftp-test-server
  (lambda ()
    (let ([root "/tmp/chezpp-net-ftp-root"])
      (when (file-exists? root)
        (file-removetree root #f))
      (mkdirs (string-append root "/docs"))
      (write-bytevector-file (string-append root "/hello.txt") (string->utf8 "hello ftp"))
      (write-bytevector-file (string-append root "/docs/readme.txt") (string->utf8 "doc file"))
      (let ([listener (open-socket 'inet 'stream)])
        (socket-set-option! listener 'reuse-address #t)
        (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
        (socket-listen! listener 8)
        (let ([port (socket-address-port (socket-local-address listener))])
          (define running? #t)
          (define client-threads '())
          (define physical-path
            (lambda (virtual-path)
              (string-append root (normalize-absolute-test-path virtual-path))))
          (define open-passive
            (lambda ()
              (let ([sock (open-socket 'inet 'stream)])
                (socket-set-option! sock 'reuse-address #t)
                (socket-bind! sock (make-socket-address 'inet "127.0.0.1" 0))
                (socket-listen! sock 1)
                sock)))
          (define close-passive
            (lambda (sock)
              (when sock
                (guard (c [else #f])
                  (close-socket sock)))))
          (define handle-client
            (lambda (client)
              (let ([ip (open-socket-input-port client)]
                    [op (open-socket-output-port client)])
                (define cwd "/")
                (define rename-from #f)
                (define passive-listener #f)
                (define passive-port #f)
                (define data-accept
                  (lambda ()
                    (unless passive-listener
                      (error 'ftp-test "passive listener missing"))
                    (let-values ([(data peer) (socket-accept passive-listener)])
                      (close-passive passive-listener)
                      (set! passive-listener #f)
                      (set! passive-port #f)
                      data)))
                (define ensure-parent-dir
                  (lambda (path)
                    (mkdirs (path-dirname path))))
                (define list-dir
                  (lambda (path)
                    (map (lambda (name) (string-append name "\r\n"))
                         (directory-list path))))
                (send-crlf-line op "220 chezpp ftp test server")
                (let loop ()
                  (let ([line (read-crlf-line ip)])
                    (when line
                      (let* ([parts (string-split line #\space)]
                             [cmd (string-upcase (car parts))]
                             [arg (if (> (string-length line) (+ (string-length (car parts)) 1))
                                      (substring line (+ (string-length (car parts)) 1)
                                                 (string-length line))
                                      "")])
                        (cond
                         [(string=? cmd "USER")
                          (send-crlf-line op "331 password required")
                          (loop)]
                         [(string=? cmd "PASS")
                          (send-crlf-line op "230 logged in")
                          (loop)]
                         [(string=? cmd "SYST")
                          (send-crlf-line op "215 UNIX Type: L8")
                          (loop)]
                         [(string=? cmd "FEAT")
                          (send-crlf-line op "211-Features")
                          (send-crlf-line op " EPSV")
                          (send-crlf-line op " UTF8")
                          (send-crlf-line op "211 End")
                          (loop)]
                         [(string=? cmd "TYPE")
                          (send-crlf-line op "200 type set")
                          (loop)]
                         [(string=? cmd "PWD")
                          (send-crlf-line op (format "257 \"~a\"" cwd))
                          (loop)]
                         [(string=? cmd "CWD")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-directory? path)
                                (begin
                                  (set! cwd target)
                                  (send-crlf-line op "250 directory changed"))
                                (send-crlf-line op "550 not a directory"))
                            (loop))]
                         [(or (string=? cmd "PASV") (string=? cmd "EPSV"))
                          (close-passive passive-listener)
                          (set! passive-listener (open-passive))
                          (set! passive-port
                                (socket-address-port (socket-local-address passive-listener)))
                          (if (string=? cmd "PASV")
                              (let ([p1 (quotient passive-port 256)]
                                    [p2 (mod passive-port 256)])
                                (send-crlf-line op
                                                (format "227 Entering Passive Mode (127,0,0,1,~a,~a)"
                                                        p1
                                                        p2)))
                              (send-crlf-line op
                                              (format "229 Entering Extended Passive Mode (|||~a|)"
                                                      passive-port)))
                          (loop)]
                         [(or (string=? cmd "LIST") (string=? cmd "NLST"))
                          (let* ([target (if (string=? arg "") cwd (test-path-join cwd arg))]
                                 [path (physical-path target)])
                            (if (file-directory? path)
                                (let ([data (data-accept)])
                                  (send-crlf-line op "150 opening data connection")
                                  (let ([dop (open-socket-output-port data)])
                                    (for-each (lambda (entry)
                                                (put-bytevector dop (string->utf8 entry)))
                                              (list-dir path))
                                    (flush-output-port dop)
                                    (close-port dop))
                                  (close-socket data)
                                  (send-crlf-line op "226 transfer complete"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "SIZE")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-regular? path)
                                (send-crlf-line op (format "213 ~a" (file-size path)))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "RETR")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (if (file-regular? path)
                                (let ([data (data-accept)])
                                  (send-crlf-line op "150 opening data connection")
                                  (let ([dop (open-socket-output-port data)])
                                    (put-bytevector dop (read-u8vec path))
                                    (flush-output-port dop)
                                    (close-port dop))
                                  (close-socket data)
                                  (send-crlf-line op "226 transfer complete"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "STOR")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (ensure-parent-dir path)
                            (let ([data (data-accept)])
                              (send-crlf-line op "150 opening data connection")
                              (let ([dip (open-socket-input-port data)])
                                (write-bytevector-file path (read-port->bytevector dip))
                                (close-port dip))
                              (close-socket data)
                              (send-crlf-line op "226 transfer complete"))
                            (loop))]
                         [(string=? cmd "DELE")
                          (let ([path (physical-path (test-path-join cwd arg))])
                            (if (file-regular? path)
                                (begin
                                  (delete-file path)
                                  (send-crlf-line op "250 deleted"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "MKD")
                          (mkdirs (physical-path (test-path-join cwd arg)))
                          (send-crlf-line op "257 created")
                          (loop)]
                         [(string=? cmd "RMD")
                          (let ([path (physical-path (test-path-join cwd arg))])
                            (if (file-directory? path)
                                (begin
                                  (delete-directory path)
                                  (send-crlf-line op "250 removed"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "RNFR")
                          (let ([path (test-path-join cwd arg)])
                            (if (file-exists? (physical-path path))
                                (begin
                                  (set! rename-from path)
                                  (send-crlf-line op "350 ready for RNTO"))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "RNTO")
                          (if rename-from
                              (let ([src (physical-path rename-from)]
                                    [dest (physical-path (test-path-join cwd arg))])
                                (ensure-parent-dir dest)
                                (file-move src dest)
                                (set! rename-from #f)
                                (send-crlf-line op "250 renamed"))
                              (send-crlf-line op "503 bad sequence"))
                          (loop)]
                         [(string=? cmd "QUIT")
                          (send-crlf-line op "221 bye")]
                         [else
                          (send-crlf-line op "502 command not implemented")
                          (loop)]))))))))
          (define spawn-client-handler
            (lambda (client)
              (let ([th
                     (fork-thread
                      (lambda ()
                        (guard (c [else #f])
                          (handle-client client))
                        (guard (c [else #f])
                          (close-socket client))))])
                (set! client-threads (cons th client-threads))
                th)))
          (define server-thread
            (fork-thread
             (lambda ()
               (let loop ()
                 (when running?
                   (let ([accepted
                          (guard (c [else #f])
                            (call-with-values
                              (lambda ()
                                (socket-accept/nonblocking listener))
                              (case-lambda
                                [(v) v]
                                [(client peer)
                                 (cons client peer)])))])
                     (if accepted
                         (let ([client (car accepted)]
                               [peer (cdr accepted)])
                           (spawn-client-handler client)
                           (loop))
                         (begin
                           (milisleep 10)
                           (loop)))))))))
          (milisleep 10)
          (values root
                  port
                  (lambda ()
                    (set! running? #f)
                    (guard (c [else #f])
                      (close-socket listener))
                    (thread-join server-thread)
                    (for-each thread-join client-threads)
                    (when (file-exists? root)
                      (file-removetree root #f)))))))))

(define make-test-http-server-context
  (lambda ()
    (write-test-cert-files)
    (let ([ctx (make-tls-context 'server)])
      (tls-context-load-cert! ctx "/tmp/chezpp-net-test-cert.pem")
      (tls-context-load-private-key! ctx "/tmp/chezpp-net-test-key.pem")
      ctx)))

(define make-test-http-client-context
  (lambda ()
    (write-test-cert-files)
    (let ([ctx (make-tls-context 'client)])
      (tls-context-set-verify! ctx #f)
      ctx)))

(define start-http-connection-server
  (case-lambda
    [(handler)
     (start-http-connection-server handler #f)]
    [(handler tls-ctx)
     (let* ([port (reserve-loopback-port)]
            [server (http-listen "127.0.0.1" port tls-ctx)])
       (values server
               port
               (fork-thread
                (lambda ()
                  (guard (c [else #f])
                    (let ([conn (http-accept server)])
                      (dynamic-wind
                        void
                        (lambda () (handler conn))
                        (lambda ()
                          (http-connection-close conn)
                          (http-server-close server)))))))))]))

(define start-http-dispatch-server
  (case-lambda
    [(setup)
     (start-http-dispatch-server setup #f)]
    [(setup tls-ctx)
     (let* ([port (reserve-loopback-port)]
            [server (http-listen "127.0.0.1" port tls-ctx)])
       (setup server)
       (values server
               port
               (fork-thread
                (lambda ()
                  (guard (c [else #f])
                    (dynamic-wind
                      void
                      (lambda () (http-serve server))
                      (lambda () (http-server-close server))))))))]))

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

(mat net-http-runtime
     (let-values ([(server port th)
                   (start-http-connection-server
                    (lambda (conn)
                      (let ([req (http-read-request conn)])
                        (http-write-response
                         conn
                         (make-http-response
                          200
                          "OK"
                          `(("Content-Type" . "text/plain")
                            ("X-Client" . ,(or (http-header-ref (http-request-headers req)
                                                                "X-Client"
                                                                #f)
                                               ""))
                            ("X-Method" . ,(http-request-method req)))
                          "hello")))))])
       (let ([client (http-open)])
         (and (not (http-accept/nonblocking server))
              (begin
                (http-set-timeout! client 250)
                #t)
              (begin
                (http-set-header! client "X-Client" "ok")
                #t)
              (let ([resp (http-send
                           client
                           (make-http-request
                            'get
                            (format "http://127.0.0.1:~a/hello?q=1" port)
                            '(("Accept" . "text/plain"))
                            #f))])
                (begin
                  (http-close client)
                  (thread-join th)
                  (and (= (http-response-status resp) 200)
                       (equal? (http-header-ref (http-response-headers resp) "X-Client")
                               "ok")
                       (equal? (http-header-ref (http-response-headers resp) "X-Method")
                               "GET")
                       (equal? (utf8->string (http-response-body resp)) "hello")))))))
     (let-values ([(server port th)
                   (start-http-dispatch-server
                    (lambda (server)
                      (http-register-handler!
                       server
                       'head
                       "/meta"
                       (lambda (req)
                         (make-http-response
                          200
                          "OK"
                          '(("X-Head" . "yes"))
                          "body-ignored")))))])
       (let ([resp (http-head (format "http://127.0.0.1:~a/meta" port))])
         (thread-join th)
         (and (= (http-response-status resp) 200)
              (equal? (http-header-ref (http-response-headers resp) "X-Head")
                      "yes")
              (not (http-response-body resp))))))

(mat net-http-transfer
     (let* ([upload-path "/tmp/chezpp-net-upload.bin"]
            [payload (string->utf8 "payload")])
       (write-bytevector-file upload-path payload)
       (let-values ([(server port th)
                     (start-http-connection-server
                      (lambda (conn)
                        (let ([req (http-read-request conn)])
                          (http-write-response
                           conn
                           (make-http-response
                            200
                            "OK"
                            `(("X-Size" . ,(number->string
                                            (bytevector-length
                                             (http-request-body req)))))
                            (http-request-body req))))))])
         (let ([client (http-open)])
           (let ([resp (http-upload client
                                    (format "http://127.0.0.1:~a/upload" port)
                                    upload-path)])
             (begin
               (http-close client)
               (thread-join th)
               (and (= (http-response-status resp) 200)
                    (equal? (http-header-ref (http-response-headers resp) "X-Size")
                            "7")
                    (equal? (http-response-body resp) payload))))))
     (let* ([download-path "/tmp/chezpp-net-download.bin"]
            [payload #vu8(1 2 3 4 5)])
       (let-values ([(server port th)
                     (start-http-dispatch-server
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/download"
                         (lambda (req)
                           (make-http-response
                            200
                            "OK"
                            '(("Content-Type" . "application/octet-stream"))
                            payload)))))])
         (let ([client (http-open)])
           (let ([resp (http-download client
                                      (format "http://127.0.0.1:~a/download" port)
                                      download-path)])
             (begin
               (http-close client)
               (thread-join th)
               (and (= (http-response-status resp) 200)
                    (equal? (http-response-body resp) payload)
                    (equal? (read-u8vec download-path) payload)))))))))

(mat net-https
     (let ([server-ctx (make-test-http-server-context)]
           [client-ctx (make-test-http-client-context)])
       (let-values ([(server port th)
                     (start-http-dispatch-server
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/secure"
                         (lambda (req)
                           (make-http-response
                            200
                            "OK"
                            '(("Content-Type" . "text/plain"))
                            "secure"))))
                      server-ctx)])
        (let ([client (http-open client-ctx)])
           (let ([resp (http-get client
                                 (format "https://127.0.0.1:~a/secure" port))])
             (begin
               (http-close client)
               (close-tls-context client-ctx)
               (close-tls-context server-ctx)
               (thread-join th)
               (and (= (http-response-status resp) 200)
                    (equal? (utf8->string (http-response-body resp)) "secure"))))))))

(mat net-ftp
     (let-values ([(root port stop-server) (start-ftp-test-server)])
       (let ([session (ftp-open (format "ftp://127.0.0.1:~a/" port))]
             [download-path "/tmp/chezpp-net-ftp-download.txt"]
             [upload-path "/tmp/chezpp-net-ftp-upload.txt"])
         (write-bytevector-file upload-path (string->utf8 "upload payload"))
         (dynamic-wind
           void
           (lambda ()
             (and (ftp-session? session)
                  (ftp-login! session "user" "pass")
                  (equal? (ftp-pwd session) "/")
                  (not (ftp-active-mode! session))
                  (ftp-passive-mode! session)
                  (let ([entries (begin
                                   (milisleep 10)
                                   (ftp-list session))])
                    (and (member "docs" entries)
                         (member "hello.txt" entries)))
                  (equal? (ftp-cwd! session "/docs") session)
                  (equal? (ftp-pwd session) "/docs")
                  (equal? (begin
                            (milisleep 10)
                            (ftp-list session))
                          '("readme.txt"))
                  (ftp-cwd! session "/")
                  (equal? (begin
                            (milisleep 10)
                            (ftp-download session "/hello.txt" download-path))
                          download-path)
                  (equal? (read-u8vec download-path) (string->utf8 "hello ftp"))
                  (equal? (begin
                            (milisleep 10)
                            (ftp-upload session upload-path "/incoming.txt"))
                          "/incoming.txt")
                  (equal? (begin
                            (milisleep 10)
                            (ftp-download session "/incoming.txt" download-path))
                          download-path)
                  (equal? (read-u8vec download-path) (string->utf8 "upload payload"))
                  (begin
                    (milisleep 10)
                    (ftp-mkdir! session "/tmpdir"))
                  (begin
                    (milisleep 10)
                    (ftp-rename! session "/incoming.txt" "/renamed.txt"))
                  (let ([entries (begin
                                   (milisleep 10)
                                   (ftp-list session "/"))])
                    (and (member "renamed.txt" entries)
                         (member "tmpdir" entries)))
                  (begin
                    (milisleep 10)
                    (ftp-delete! session "/renamed.txt"))
                  (begin
                    (milisleep 10)
                    (ftp-rmdir! session "/tmpdir"))
                  (let ([entries (begin
                                   (milisleep 10)
                                   (ftp-list session "/"))])
                    (and (not (member "renamed.txt" entries))
                         (not (member "tmpdir" entries))))
                  (equal? (ftp-quit! session) session)))
           (lambda ()
             (stop-server))))))
