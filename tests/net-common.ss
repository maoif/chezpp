(define tls-test-certificate
  (string->utf8
   "-----BEGIN CERTIFICATE-----\nMIIDCTCCAfGgAwIBAgIUZBmYij7LabaZKVKlJk2qLuigxGkwDQYJKoZIhvcNAQEL\nBQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDMzMTEyMjU1NloXDTI3MDMz\nMTEyMjU1NlowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF\nAAOCAQ8AMIIBCgKCAQEAjA9cXUf3ZEqsRg112cBl/SLIlnOoNdoauWKa7mII6ZiU\nHgh1x8Ety0mU9U4A992FPpFDMQznDAzGwUyoRtQ0p19NH0NZFtXATYoho5D9fSmC\nvJlW6L8efh1YIvO+BEYsa5UNcumagyctmsIhNHSgdacyruiBQtx8TPv3kGlsetqv\no91ZxKFP2ETKR2OWICkq6lADJf59+igjiLSs593c2olRNbmKVyMXx52i5EOsNbhx\ntxhEqB3WdTzbDN9oS0tpw1OFf95ps/40O9QlMnt6nYYWMnaPbtVilzq94uvE4OpN\n7pH+tv+ehvxApIqFVaCg84ddDW9lIUmQn3dNDLa3WQIDAQABo1MwUTAdBgNVHQ4E\nFgQUbIHt/Fxargqo4TSGMRy1zbaIbacwHwYDVR0jBBgwFoAUbIHt/Fxargqo4TSG\nMRy1zbaIbacwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAI8DH\nehYADEh0HWP/Y34VAY9pbx7Kx06UZQfJME6HCAPoBAEDIVGI0Y6GMUlKtetU9SPk\n6/4/nVPg5bjCOnQx0SU3yCJdzHmZJC9GPXeqkZhjtrRQTneMnAz1Nae6bDCS6iOw\nM5nLdrao+MUYrZWimOr9/tJMHV9ffLoULXsj1bPSEZ8OJEa9Y+n+JSYLo4N35iXJ\nYkMIxVoxdStnwZyrWYgmTAIG62kQ7UmpcyQG+HVjeL5LRXTZFlwARc021HCKM1EH\nU+Bgt/w0C8m2CFenpQqnEfw2tgNzHeaIxCLHlAesUfvNrGfqVIvbV7WUyZdUEoyf\nJZGGecWkv1SBX4VXGw==\n-----END CERTIFICATE-----\n"))

(define tls-test-private-key
  (string->utf8
   "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCMD1xdR/dkSqxG\nDXXZwGX9IsiWc6g12hq5YpruYgjpmJQeCHXHwS3LSZT1TgD33YU+kUMxDOcMDMbB\nTKhG1DSnX00fQ1kW1cBNiiGjkP19KYK8mVbovx5+HVgi874ERixrlQ1y6ZqDJy2a\nwiE0dKB1pzKu6IFC3HxM+/eQaWx62q+j3VnEoU/YRMpHY5YgKSrqUAMl/n36KCOI\ntKzn3dzaiVE1uYpXIxfHnaLkQ6w1uHG3GESoHdZ1PNsM32hLS2nDU4V/3mmz/jQ7\n1CUye3qdhhYydo9u1WKXOr3i68Tg6k3ukf62/56G/ECkioVVoKDzh10Nb2UhSZCf\nd00MtrdZAgMBAAECggEADU8uX0NF9bbuc1yD41Sk1gh9C2YwYo54KKXl0OfqLrLl\nkr8KD8vb9RJOc35DzRbOh4YLq2vgjstHndNetrBWoPfUHwSDp3RZ3ErXUuQ/YMPv\nkDCs5HW44sVMfH6XnKKQeQt5pjHsEWRJMW7nwETVTkJvqG6552eG7+5BQoaKHPIE\nAn/3H7KsCseEgMPCcoDnocOdyqGHLe1buisT3k6qlCAP/QBS45LFXCROojyWgXQq\nDElzB6MqyjA9lTQ4YO8lsxwHTCe4FvD+CKH5E3WKWHSdJDsf2fmJyEaAy1oH9yYi\n4tn2mG0RoN0xeqIhCPUljQLujNkfJEPprAowggC2WwKBgQDEUULYmTjlf7wtl3TK\nlH7FzpYL8dRFdR1wQ1yq9DKrRNrlaHG0zNt5Kqm30ku1/oDchR56Y00UV2lxLACk\ndqNm2e24z0Jz4hDtLHXvcopskn+dILdMnew0OGN2PQ5gb1fOuuqtpS/OaTScYxiY\nezjFViaalbfgumR1BZwHaCbpxwKBgQC2o8UJYuCcExb2CIcFVKq20Tse2WWAEmGk\ni52M5ah8P0C8UaoEj+vW4BGrC3TXk8H34+XZPH3SAY0p2UxW8OVALcouiFtdBihh\nu629GbKF8GXm9TuZ5ou00lSqKGTVgiNfLRMRUmElv5ZOyA12OVWGIYyIFroSAbiS\nQeGx7z5V3wKBgQC4E+YVCN7jurktGsXlKgYQ3hutiYzbr+vxlwguOBnGpCKIVz2/\nJRNp8sn+1g4t0TztCVlBsxjUSP5Sosrba27eAtw3nQeXd1MdwMG4yvLmyRslr0aQ\nbcfMU09Xz/pKDD0OWA+y0KAZ8GXnebfXSjs6NgSukFJBQyTs4VyjSVKrgwKBgEHi\nzn/WVaS4Fj5nUR4RLwyIakV0s3MCLotHemyLpL49q0LESwseSDvZ1UXY+iuSuBSO\n+Cnn8pPBz4TbSPjMKkd+vUMQGbVzNToclE51aLt8v6YTrY6VZqyye6xuqgGD0vLQ\nteI3z6fod3awIHsXr8yVabbmS/WW/Vh1v8+KuPgNAoGBAKta9dICoGISq22PRgyb\ndEHFp95rTq9/FSdVhXllbhM5dfiY97uV/O9E5QiyJ9uAVReXccnkjLVyMnRUXhY/\nEoYIE4KO2iQ6JNafeuAEUgcmapt/b6VwHMVFyBBIi2FNzN4h6YkTXGl1lOEJDoVp\n0edea+G4bnZxUlckUv7+5nCV\n-----END PRIVATE KEY-----\n"))

(define tls-test-san-certificate
  (string->utf8
   "-----BEGIN CERTIFICATE-----\nMIIDJTCCAg2gAwIBAgIUeOblSmCv6U0NeL41eRzX+uOZKh4wDQYJKoZIhvcNAQEL\nBQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDUxNjAyNTgxMVoXDTI3MDUx\nNjAyNTgxMVowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF\nAAOCAQ8AMIIBCgKCAQEAqDIKsS9yYc6rwE3Xa6QkpRFYZUHj+EsruZ76FrqzLe5/\n0DAodyrvBWa3k2UKLiJtob7U7G5EHt0Q7Ven2QX2mLepQV/IjUZ8yeYy0WZ04v/g\nFfRjkyaeYjgiGM0smtU+IKXa4j9LdtVfMIN4yPqdqCxU4uCAz5n3uQUe3idmtyEj\na+hXq8d50U5oJTdcl4/wx+pO4MFUL43upgEy6u0deNZo5XjgQgMFpjRWMIElhNdy\n4mdsh9v+gu66mETi/smlDFEvTpJgmSEeQ4Y6/cNy8NRvh6WJEpjYmNWNLmZcX/ka\nz01UNFaL0fNUuCsQlfeM/pqF4SDsGjN07CbT06HSfQIDAQABo28wbTAdBgNVHQ4E\nFgQUaophpSx6dexvPLv3f+z1sSXF2lUwHwYDVR0jBBgwFoAUaophpSx6dexvPLv3\nf+z1sSXF2lUwDwYDVR0TAQH/BAUwAwEB/zAaBgNVHREEEzARgglsb2NhbGhvc3SH\nBH8AAAEwDQYJKoZIhvcNAQELBQADggEBABRpAYBGtEbxzJwcBgZTTX/Ko4DjJrhx\n1AJ9dVPpSFDPS982w3lKtEN+HCeEW6YOmu6Hj0BKUs8+pS0nyYN7er8JFmAxEyYB\ngLYqFkwYXsPkeRgUC71RrSN6oumcq3UZ7j1X02SMvbhQLC92rnHk9vLqcQxg2OIz\nZTUN/JMMYsnqZpd5cVkLo25QetbBOWdgjPkXDkrR09GBgpZvky2x0+AwmeFQ+8Jd\nqjUwgyxb3U7EyNDmpSGrpqR8VcjNYzDc5QTjYRqHjnHh9Zjth78zE27l8eRoyyoZ\nkLn6f0r3xhVA1WScD79Rm3oDrgvr3dRep0M4fUN2tCoLzHEsGXmoi2k=\n-----END CERTIFICATE-----\n"))

(define tls-test-san-private-key
  (string->utf8
   "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCoMgqxL3JhzqvA\nTddrpCSlEVhlQeP4Syu5nvoWurMt7n/QMCh3Ku8FZreTZQouIm2hvtTsbkQe3RDt\nV6fZBfaYt6lBX8iNRnzJ5jLRZnTi/+AV9GOTJp5iOCIYzSya1T4gpdriP0t21V8w\ng3jI+p2oLFTi4IDPmfe5BR7eJ2a3ISNr6Ferx3nRTmglN1yXj/DH6k7gwVQvje6m\nATLq7R141mjleOBCAwWmNFYwgSWE13LiZ2yH2/6C7rqYROL+yaUMUS9OkmCZIR5D\nhjr9w3Lw1G+HpYkSmNiY1Y0uZlxf+RrPTVQ0VovR81S4KxCV94z+moXhIOwaM3Ts\nJtPTodJ9AgMBAAECggEAAuLM7Spfv47gwo1zX0zzNgPcrOwbAcUkbGxuZRpx/Z1E\ngyl4YEcTz9dp2fUXXiBIGkjgdWgU8iSf6DqjTiV8jDVGibEfi1EEziANiitEYSq7\nyRSm6rz0ZcIJz7whgkybYssFvPESFelmVzpCRcyLDPopwLTScP120y1j0LMDX6wC\nQdZapehVxMb/nUntVGB8fff1nwzXORmn7BWneLhnW9Nxl7ajmElPKwf7VUn4ol4I\nohQvzG3F/FtD058lFmAltstq6STpEqn6yBd80XcgC/+Ei4J+KDL34nxXzifbQAXX\nUsoPwYh9/vabc40cYnfI+yPgGgLkhkeO99oh9CGuDwKBgQDcWL+4V9utYhm90JHW\nkeswO2WrhCRBqUeK4TC71oCny3NuC5tpB4OsV2j56DVBecoVfMNxBPeG+q9gwzwq\nkqNQ2SuzUTqUNQsb8TJ+lzIT4kfUvHSCyEgUYiMjoW2VZBqacYo9VIhnt4J1IPHX\nFSduvqPE6t7/e+F35V3LY4ziEwKBgQDDaRLibU8ETG1gZAEDu7Jw/j5itH9qL5C2\n7C55f62qU9lEHuf7SVElGM48sLi6azLtl4nUcR5szYMJfsI2iiWgIvPsGLPWDKdh\nWh62YOOqnF1bKEtlEM27HiwHUSzRe8qd0XAZIRYS7OLVw/GkJHWi4H6cG3sO13F2\nY04HWmyLLwKBgQCeaX+L/D4GrJxtBmGSrV00U7+IS3v1aMnyYmsy38em2nGmD9ou\n7CQR5tWltPd0lyZdxFCOFFAQTnF/U10SDlIzca4lQKSooGnYMNNohzKNcQTLQKFS\n1MJdRSCWIRwzZsBpgG8uIHPgfOHha0nyo1ayG4SJsYGYk2tJUzDXZD6I2QKBgQCS\n7+/DU26XTODyGkCpDfqf/I3sqs0ki5/F7NMFiXyNF3/IOXU2/2qE+qIaAMjnZLRm\n9AwwMt8t8VsXSLnBSSABykagwfJ4ggtaqOlvOmv3XpzUbvhChuAsVAPHfMK2wt8Y\nzOP+AWMX2Ai5tR1+z+ulDiomOIPQJTCsVP/Xh6cXAQKBgEP4AdA9tFTAF8q8UcsB\njxnrnQ5fyOT6YL1KHpGvWrNKCj9ybhNheAf3sUWI0X0IVEqPJPecbghDL+51IaAm\nyfFEPdR3FXdfVFyYrVnF7JMSdUP/XuW3OtuWI7ixXZzQ8v5Kp963gsGedOk/ndyE\nEZ+Xm986E4UJlyXiKJNVFf34\n-----END PRIVATE KEY-----\n"))

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

(define proc-fd-count
  (lambda ()
    (length (directory-list "/proc/self/fd"))))

(define write-test-cert-files
  (lambda ()
    (write-bytevector-file "/tmp/chezpp-net-test-cert.pem" tls-test-certificate)
    (write-bytevector-file "/tmp/chezpp-net-test-key.pem" tls-test-private-key)))

(define write-test-san-cert-files
  (lambda ()
    (write-bytevector-file "/tmp/chezpp-net-test-san-cert.pem" tls-test-san-certificate)
    (write-bytevector-file "/tmp/chezpp-net-test-san-key.pem" tls-test-san-private-key)))

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

(define make-test-http-server-context
  (lambda ()
    (write-test-cert-files)
    (let ([ctx (make-tls-context 'server)])
      (tls-context-load-cert! ctx "/tmp/chezpp-net-test-cert.pem")
      (tls-context-load-private-key! ctx "/tmp/chezpp-net-test-key.pem")
      ctx)))

(define make-test-http-verified-server-context
  (lambda ()
    (write-test-san-cert-files)
    (let ([ctx (make-tls-context 'server)])
      (tls-context-load-cert! ctx "/tmp/chezpp-net-test-san-cert.pem")
      (tls-context-load-private-key! ctx "/tmp/chezpp-net-test-san-key.pem")
      ctx)))

(define make-test-http-client-context
  (lambda ()
    (write-test-cert-files)
    (let ([ctx (make-tls-context 'client)])
      (tls-context-set-verify! ctx #f)
      ctx)))

(define make-test-http-verified-client-context
  (lambda ()
    (write-test-san-cert-files)
    (let ([ctx (make-tls-context 'client)])
      (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-san-cert.pem")
      (tls-context-set-verify! ctx #t)
      ctx)))

(define make-test-http-cn-verified-client-context
  (lambda ()
    (write-test-cert-files)
    (let ([ctx (make-tls-context 'client)])
      (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
      (tls-context-set-verify! ctx #t)
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
