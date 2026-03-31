(library (chezpp net ip)
  (export ip-address?
          ipv4-address?
          ipv6-address?
          string->ip-address
          ip-address->string
          ip-address-version
          ip-address-loopback?
          ip-address-private?
          ip-address-multicast?
          cidr-parse
          cidr-contains?
          cidr-network-address
          cidr-prefix-length)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp string))

  (define-record-type (ip-address %make-ip-address ip-address?)
    (sealed #t)
    (opaque #f)
    (fields (immutable version ip-address-version)
            (immutable bytes ip-address-bytes)))

  (define-record-type (cidr %make-cidr cidr?)
    (sealed #t)
    (opaque #f)
    (fields (immutable network-address cidr-network-address)
            (immutable prefix-length cidr-prefix-length)))

  (define make-zero-list
    (lambda (n)
      (let loop ([n n] [out '()])
        (if (fx<= n 0)
            out
            (loop (fx1- n) (cons 0 out))))))

  (define join-strings
    (lambda (parts sep)
      (if (null? parts)
          ""
          (let loop ([parts (cdr parts)] [out (car parts)])
            (if (null? parts)
                out
                (loop (cdr parts) (string-append out sep (car parts))))))))

  (define parse-decimal-byte
    (lambda (str)
      (let ([n (string->number str 10)])
        (and n
             (integer? n)
             (exact? n)
             (fx<= 0 n)
             (fx<= n 255)
             n))))

  (define parse-hex-word
    (lambda (str)
      (and (not (string=? str ""))
           (fx<= (string-length str) 4)
           (let ([n (string->number str 16)])
             (and n
                  (integer? n)
                  (exact? n)
                  (fx<= 0 n)
                  (fx<= n #xffff)
                  n)))))

  (define parse-ipv4-bytevector
    (lambda (str)
      (let ([parts (string-split str #\.)])
        (and (fx= (length parts) 4)
             (let ([bv (make-bytevector 4 0)])
               (let loop ([i 0] [parts parts])
                 (if (null? parts)
                     bv
                     (let ([octet (parse-decimal-byte (car parts))])
                       (and octet
                            (begin
                              (bytevector-u8-set! bv i octet)
                              (loop (fx1+ i) (cdr parts))))))))))))

  (define parse-ipv6-piece
    (lambda (piece)
      (if (string=? piece "")
          '()
          (let ([segments (string-split piece #\:)])
            (let loop ([segments segments] [out '()])
              (cond
                ((null? segments) (reverse out))
                (else
                 (let ([segment (car segments)])
                   (cond
                     ((string=? segment "") #f)
                     ((string-contains? segment ".")
                      (and (null? (cdr segments))
                           (let ([ipv4 (parse-ipv4-bytevector segment)])
                             (and ipv4
                                  (reverse
                                   (cons (fxlogor (fxsll (bytevector-u8-ref ipv4 2) 8)
                                                  (bytevector-u8-ref ipv4 3))
                                         (cons (fxlogor (fxsll (bytevector-u8-ref ipv4 0) 8)
                                                        (bytevector-u8-ref ipv4 1))
                                               out)))))))
                     (else
                      (let ([word (parse-hex-word segment)])
                        (and word
                             (loop (cdr segments) (cons word out))))))))))))))

  (define hextets->ipv6-bytevector
    (lambda (hextets)
      (and (fx= (length hextets) 8)
           (let ([bv (make-bytevector 16 0)])
             (let loop ([i 0] [hextets hextets])
               (if (null? hextets)
                   bv
                   (let ([word (car hextets)])
                     (bytevector-u8-set! bv i (fxlogand (fxsrl word 8) #xff))
                     (bytevector-u8-set! bv (fx1+ i) (fxlogand word #xff))
                     (loop (fx+ i 2) (cdr hextets)))))))))

  (define parse-ipv6-bytevector
    (lambda (str)
      (let ([double-colon (string-search-all str "::")])
        (cond
         [(and double-colon (not (null? (cdr double-colon)))) #f]
         [double-colon
          (let* ([pos (car double-colon)]
                 [left (substring str 0 pos)]
                 [right (substring str (fx+ pos 2) (string-length str))]
                 [lhs (parse-ipv6-piece left)]
                 [rhs (parse-ipv6-piece right)])
            (and lhs
                 rhs
                 (let ([missing (fx- 8 (fx+ (length lhs) (length rhs)))])
                   (and (fx>= missing 1)
                        (hextets->ipv6-bytevector
                         (append lhs (make-zero-list missing) rhs))))))]
         [else
          (let ([hextets (parse-ipv6-piece str)])
            (and hextets
                 (fx= (length hextets) 8)
                 (hextets->ipv6-bytevector hextets)))]))))

  (define ipv6-hextets
    (lambda (bv)
      (let loop ([i 0] [out '()])
        (if (fx= i 16)
            (reverse out)
            (loop (fx+ i 2)
                  (cons (fxlogor (fxsll (bytevector-u8-ref bv i) 8)
                                 (bytevector-u8-ref bv (fx1+ i)))
                        out))))))

  (define longest-zero-run
    (lambda (hextets)
      (let loop ([rest hextets] [i 0]
                 [best-start #f] [best-len 0]
                 [cur-start #f] [cur-len 0])
        (cond
         [(null? rest)
          (if (and cur-start (fx> cur-len best-len))
              (values cur-start cur-len)
              (values best-start best-len))]
         [(fx= (car rest) 0)
          (if cur-start
              (loop (cdr rest) (fx1+ i) best-start best-len cur-start (fx1+ cur-len))
              (loop (cdr rest) (fx1+ i) best-start best-len i 1))]
         [else
          (if (and cur-start (fx> cur-len best-len))
              (loop (cdr rest) (fx1+ i) cur-start cur-len #f 0)
              (loop (cdr rest) (fx1+ i) best-start best-len #f 0))]))))

  (define take-list
    (lambda (ls n)
      (if (or (fx<= n 0) (null? ls))
          '()
          (cons (car ls) (take-list (cdr ls) (fx1- n))))))

  (define drop-list
    (lambda (ls n)
      (if (or (fx<= n 0) (null? ls))
          ls
          (drop-list (cdr ls) (fx1- n)))))

  (define format-ipv6
    (lambda (bv)
      (let* ([hextets (ipv6-hextets bv)]
             [parts (map (lambda (word) (string-downcase (number->string word 16))) hextets)])
        (let-values ([(best-start best-len) (longest-zero-run hextets)])
          (if (and best-start (fx>= best-len 2))
              (let ([left (take-list parts best-start)]
                    [right (drop-list parts (fx+ best-start best-len))])
                (cond
                 [(and (null? left) (null? right)) "::"]
                 [(null? left) (string-append "::" (join-strings right ":"))]
                 [(null? right) (string-append (join-strings left ":") "::")]
                 [else
                  (string-append (join-strings left ":")
                                 "::"
                                 (join-strings right ":"))]))
              (join-strings parts ":"))))))

  (define mask-byte
    (lambda (x keep-bits)
      (cond
       [(fx<= keep-bits 0) 0]
       [(fx>= keep-bits 8) x]
       [else
        (fxlogand x
                  (fxsll (fx1- (fxsll 1 keep-bits))
                         (fx- 8 keep-bits)))])))

  (define ip-mask-bytevector
    (lambda (bv prefix)
      (let* ([len (bytevector-length bv)]
             [out (make-bytevector len 0)])
        (let loop ([i 0] [remaining prefix])
          (if (fx= i len)
              out
              (let ([value (bytevector-u8-ref bv i)])
                (cond
                 [(fx>= remaining 8)
                  (bytevector-u8-set! out i value)
                  (loop (fx1+ i) (fx- remaining 8))]
                 [else
                  (bytevector-u8-set! out i (mask-byte value remaining))
                  (loop (fx1+ i) 0)])))))))

  (define bytevector-prefix=?
    (lambda (a b prefix)
      (let ([ma (ip-mask-bytevector a prefix)]
            [mb (ip-mask-bytevector b prefix)])
        (let loop ([i 0])
          (or (fx= i (bytevector-length ma))
              (and (fx= (bytevector-u8-ref ma i) (bytevector-u8-ref mb i))
                   (loop (fx1+ i))))))))

  #|proc:ipv4-address?
The `ipv4-address?` procedure returns whether the given value is an IPv4 address object.
|#
  (define-who ipv4-address?
    (lambda (x)
      (and (ip-address? x)
           (fx= (ip-address-version x) 4))))

  #|proc:ipv6-address?
The `ipv6-address?` procedure returns whether the given value is an IPv6 address object.
|#
  (define-who ipv6-address?
    (lambda (x)
      (and (ip-address? x)
           (fx= (ip-address-version x) 6))))

  #|proc:string->ip-address
The `string->ip-address` procedure parses an IPv4 or IPv6 address string and returns an address object, or `#f` on failure.
|#
  (define-who string->ip-address
    (lambda (str)
      (pcheck ([string? str])
              (cond
               [(parse-ipv4-bytevector str) => (lambda (bv) (%make-ip-address 4 bv))]
               [(parse-ipv6-bytevector str) => (lambda (bv) (%make-ip-address 6 bv))]
               [else #f]))))

  #|proc:ip-address->string
The `ip-address->string` procedure renders an IP address object to its canonical textual form.
|#
  (define-who ip-address->string
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (join-strings
                     (list (number->string (bytevector-u8-ref bv 0))
                           (number->string (bytevector-u8-ref bv 1))
                           (number->string (bytevector-u8-ref bv 2))
                           (number->string (bytevector-u8-ref bv 3)))
                     ".")
                    (format-ipv6 bv))))))

  #|proc:ip-address-loopback?
The `ip-address-loopback?` procedure returns whether an IP address is a loopback address.
|#
  (define-who ip-address-loopback?
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (fx= (bytevector-u8-ref bv 0) 127)
                    (and (let loop ([i 0])
                           (if (fx= i 15)
                               #t
                               (and (fx= (bytevector-u8-ref bv i) 0)
                                    (loop (fx1+ i)))))
                         (fx= (bytevector-u8-ref bv 15) 1)))))))

  #|proc:ip-address-private?
The `ip-address-private?` procedure returns whether an IP address is in a private-use range.
|#
  (define-who ip-address-private?
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (let ([a (bytevector-u8-ref bv 0)]
                          [b (bytevector-u8-ref bv 1)])
                      (or (fx= a 10)
                          (and (fx= a 172)
                               (fx<= 16 b)
                               (fx<= b 31))
                          (and (fx= a 192)
                               (fx= b 168))))
                    (let ([a (bytevector-u8-ref bv 0)])
                      (or (fx= (fxlogand a #xfe) #xfc)
                          (fx= a #xfe))))))))

  #|proc:ip-address-multicast?
The `ip-address-multicast?` procedure returns whether an IP address is a multicast address.
|#
  (define-who ip-address-multicast?
    (lambda (ip)
      (pcheck ([ip-address? ip])
              (let ([bv (ip-address-bytes ip)])
                (if (fx= (ip-address-version ip) 4)
                    (let ([a (bytevector-u8-ref bv 0)])
                      (and (fx<= 224 a) (fx<= a 239)))
                    (fx= (bytevector-u8-ref bv 0) #xff))))))

  #|proc:cidr-parse
The `cidr-parse` procedure parses a CIDR string and returns a CIDR object, or `#f` on failure.
|#
  (define-who cidr-parse
    (lambda (str)
      (pcheck ([string? str])
              (let ([parts (string-split str #\/)])
                (and (fx= (length parts) 2)
                     (let ([ip (string->ip-address (car parts))]
                           [prefix (string->number (cadr parts) 10)])
                       (and ip
                             prefix
                             (integer? prefix)
                             (exact? prefix)
                             (fx<= 0 prefix)
                             (fx<= prefix (if (fx= (ip-address-version ip) 4) 32 128))
                             (let ([network (ip-mask-bytevector (ip-address-bytes ip) prefix)])
                               (%make-cidr (%make-ip-address (ip-address-version ip) network)
                                          prefix)))))))))

  #|proc:cidr-contains?
The `cidr-contains?` procedure returns whether an IP address belongs to a CIDR range.
|#
  (define-who cidr-contains?
    (lambda (range ip)
      (pcheck ([cidr? range] [ip-address? ip])
              (and (fx= (ip-address-version (cidr-network-address range))
                        (ip-address-version ip))
                   (bytevector-prefix=?
                    (ip-address-bytes (cidr-network-address range))
                    (ip-address-bytes ip)
                    (cidr-prefix-length range))))))
  )
