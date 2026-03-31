(library (chezpp net uri)
  (export string->uri
          uri->string
          uri?
          uri-scheme
          uri-userinfo
          uri-host
          uri-port
          uri-path
          uri-query
          uri-fragment
          uri-authority
          uri-path-segments
          uri-query-alist
          uri-resolve
          uri-normalize
          uri-encode
          uri-decode
          form-urlencode
          form-urldecode)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp string))

  (define-record-type (uri %make-uri uri?)
    (sealed #t)
    (opaque #f)
    (fields (immutable scheme uri-scheme)
            (immutable userinfo uri-userinfo)
            (immutable host uri-host)
            (immutable port uri-port)
            (immutable path uri-path)
            (immutable query uri-query)
            (immutable fragment uri-fragment)))

  (define join-strings
    (lambda (parts sep)
      (if (null? parts)
          ""
          (let loop ([parts (cdr parts)] [out (car parts)])
            (if (null? parts)
                out
                (loop (cdr parts) (string-append out sep (car parts))))))))

  (define string-index
    (lambda (str ch)
      (let ([len (string-length str)])
        (let loop ([i 0])
          (cond
           [(fx= i len) #f]
           [(char=? (string-ref str i) ch) i]
           [else (loop (fx1+ i))])))))

  (define string-rindex
    (lambda (str ch)
      (let loop ([i (fx1- (string-length str))])
        (cond
         [(fx< i 0) #f]
         [(char=? (string-ref str i) ch) i]
         [else (loop (fx1- i))]))))

  (define split-first
    (lambda (str ch)
      (let ([i (string-index str ch)])
        (if i
            (values (substring str 0 i)
                    (substring str (fx1+ i) (string-length str)))
            (values str #f)))))

  (define split-scheme
    (lambda (str)
      (let ([len (string-length str)])
        (if (fx= len 0)
            (values #f str)
            (let ([ch0 (string-ref str 0)])
              (if (or (char<=? #\a ch0 #\z)
                      (char<=? #\A ch0 #\Z))
                  (let loop ([i 1])
                    (cond
                     [(fx= i len) (values #f str)]
                     [(char=? (string-ref str i) #\:)
                      (values (string-downcase (substring str 0 i))
                              (substring str (fx1+ i) len))]
                     [(or (char<=? #\a (string-ref str i) #\z)
                          (char<=? #\A (string-ref str i) #\Z)
                          (char<=? #\0 (string-ref str i) #\9)
                          (char=? (string-ref str i) #\+)
                          (char=? (string-ref str i) #\.)
                          (char=? (string-ref str i) #\-))
                      (loop (fx1+ i))]
                     [else (values #f str)]))
                  (values #f str)))))))

  (define parse-authority
    (lambda (authority)
      (let* ((at (string-rindex authority #\@))
             (userinfo (and at (substring authority 0 at)))
             (hostport (if at
                           (substring authority (fx1+ at) (string-length authority))
                           authority)))
        (cond
         ((string=? hostport "")
          (values userinfo #f #f))
         ((char=? (string-ref hostport 0) #\[)
          (let ((close (string-index hostport #\])))
            (and close
                 (let ((host (substring hostport 1 close))
                       (rest (substring hostport (fx1+ close) (string-length hostport))))
                   (if (string=? rest "")
                       (values userinfo host #f)
                       (if (and (fx>= (string-length rest) 2)
                                (char=? (string-ref rest 0) #\:))
                           (let ((port (string->number (substring rest 1 (string-length rest)) 10)))
                             (and port
                                  (integer? port)
                                  (exact? port)
                                  (values userinfo host port)))
                           #f))))))
         (else
          (let ((colon* (string-search-all hostport #\:)))
            (if (and colon* (null? (cdr colon*)))
                (let ((pos (car colon*)))
                  (let ((host (substring hostport 0 pos))
                        (port-text (substring hostport (fx1+ pos) (string-length hostport))))
                    (let ((port (string->number port-text 10)))
                      (and port
                           (integer? port)
                           (exact? port)
                           (values userinfo host port)))))
                (values userinfo hostport #f))))))))

  (define parse-uri-reference
    (lambda (str)
      (let-values ([(before-fragment fragment) (split-first str #\#)])
        (let-values ([(before-query query) (split-first before-fragment #\?)])
          (let-values ([(scheme rest0) (split-scheme before-query)])
            (if (and (fx>= (string-length rest0) 2)
                     (string=? (substring rest0 0 2) "//"))
                (let* ([rest (substring rest0 2 (string-length rest0))]
                       [slash (string-index rest #\/)]
                       [authority (if slash (substring rest 0 slash) rest)]
                       [path (if slash
                                 (substring rest slash (string-length rest))
                                 "")])
                  (call-with-values
                      (lambda () (parse-authority authority))
                    (lambda (userinfo host port)
                      (and (or host (string=? authority ""))
                           (%make-uri scheme userinfo host port path query fragment)))))
                (%make-uri scheme #f #f #f rest0 query fragment)))))))

  (define unreserved-byte?
    (lambda (u8)
      (or (and (fx<= 65 u8) (fx<= u8 90))
          (and (fx<= 97 u8) (fx<= u8 122))
          (and (fx<= 48 u8) (fx<= u8 57))
          (memv u8 '(45 46 95 126)))))

  (define byte->hex
    (lambda (u8)
      (let ([s (number->string u8 16)])
        (if (fx= (string-length s) 1)
            (string-append "0" (string-upcase s))
            (string-upcase s)))))

  (define hex-value
    (lambda (ch)
      (cond
       [(char<=? #\0 ch #\9) (fx- (char->integer ch) (char->integer #\0))]
       [(char<=? #\a ch #\f) (fx+ 10 (fx- (char->integer ch) (char->integer #\a)))]
       [(char<=? #\A ch #\F) (fx+ 10 (fx- (char->integer ch) (char->integer #\A)))]
       [else #f])))

  (define percent-decode
    (lambda (who str plus->space?)
      (let ([len (string-length str)])
        (let-values ([(port get) (open-bytevector-output-port)])
          (let loop ([i 0])
            (if (fx= i len)
                (utf8->string (get))
                (let ([ch (string-ref str i)])
                  (cond
                   [(and plus->space? (char=? ch #\+))
                    (put-u8 port 32)
                    (loop (fx1+ i))]
                   [(char=? ch #\%)
                    (if (fx<= (fx+ i 3) len)
                        (let ([a (hex-value (string-ref str (fx1+ i)))]
                              [b (hex-value (string-ref str (fx+ i 2)))])
                          (if (and a b)
                              (begin
                                (put-u8 port (fx+ (fxsll a 4) b))
                                (loop (fx+ i 3)))
                              (errorf who "invalid percent escape in ~s" str)))
                        (errorf who "truncated percent escape in ~s" str))]
                   [else
                    (put-bytevector port (string->utf8 (string ch)))
                    (loop (fx1+ i))]))))))))

  (define normalize-path
    (lambda (path)
      (let* ([absolute? (and (fx> (string-length path) 0)
                             (char=? (string-ref path 0) #\/))]
             [trailing? (and (fx> (string-length path) 0)
                             (char=? (string-ref path (fx1- (string-length path))) #\/))]
             [segments (string-split path #\/)])
        (let loop ([segments segments] [stack '()])
          (if (null? segments)
              (let* ([body (join-strings (reverse stack) "/")]
                     [base (cond
                            [(and absolute? (string=? body "")) "/"]
                            [absolute? (string-append "/" body)]
                            [else body])])
                (if (and trailing?
                         (not (string=? base ""))
                         (not (char=? (string-ref base (fx1- (string-length base))) #\/)))
                    (string-append base "/")
                    base))
              (let ([segment (car segments)])
                (cond
                 [(or (string=? segment "") (string=? segment "."))
                  (loop (cdr segments) stack)]
                 [(string=? segment "..")
                  (loop (cdr segments) (if (null? stack) stack (cdr stack)))]
                 [else
                  (loop (cdr segments) (cons segment stack))])))))))

  (define merge-path
    (lambda (base relative)
      (cond
       [(and (uri-host base) (string=? (uri-path base) ""))
        (string-append "/" relative)]
       [else
        (let* ([path (uri-path base)]
               [slash (string-rindex path #\/)])
          (if slash
              (string-append (substring path 0 (fx1+ slash)) relative)
              relative))])))

  (define default-port-for-scheme
    (lambda (scheme)
      (cond
       [(or (string=? scheme "http") (string=? scheme "ws")) 80]
       [(or (string=? scheme "https") (string=? scheme "wss")) 443]
       [(string=? scheme "ftp") 21]
       [else #f])))

  #|proc:string->uri
The `string->uri` procedure parses a URI or URI reference string and returns a URI object, or `#f` on failure.
|#
  (define-who string->uri
    (lambda (str)
      (pcheck ([string? str])
              (parse-uri-reference str))))

  #|proc:uri-authority
The `uri-authority` procedure renders the authority component of a URI, or `#f` when there is none.
|#
  (define-who uri-authority
    (lambda (u)
      (pcheck ([uri? u])
              (and (uri-host u)
                   (let ([host (if (string-contains? (uri-host u) ":")
                                   (string-append "[" (uri-host u) "]")
                                   (uri-host u))])
                     (string-append
                      (if (uri-userinfo u)
                          (string-append (uri-userinfo u) "@")
                          "")
                      host
                      (if (uri-port u)
                          (string-append ":" (number->string (uri-port u)))
                          "")))))))

  #|proc:uri->string
The `uri->string` procedure renders a URI object to text.
|#
  (define-who uri->string
    (lambda (u)
      (pcheck ([uri? u])
              (string-append
               (if (uri-scheme u)
                   (string-append (uri-scheme u) ":")
                   "")
               (if (uri-host u)
                   (string-append "//" (uri-authority u))
                   "")
               (uri-path u)
               (if (uri-query u)
                   (string-append "?" (uri-query u))
                   "")
               (if (uri-fragment u)
                   (string-append "#" (uri-fragment u))
                   "")))))

  #|proc:uri-path-segments
The `uri-path-segments` procedure splits a URI path into slash-separated segments.
|#
  (define-who uri-path-segments
    (lambda (u)
      (pcheck ([uri? u])
              (let ([parts (string-split (uri-path u) #\/)])
                (if (and (pair? parts) (string=? (car parts) ""))
                    (cdr parts)
                    parts)))))

  #|proc:uri-query-alist
The `uri-query-alist` procedure parses the query component of a URI into decoded key/value pairs.
|#
  (define-who uri-query-alist
    (lambda (u)
      (pcheck ([uri? u])
              (if (or (not (uri-query u)) (string=? (uri-query u) ""))
                  '()
                  (map (lambda (piece)
                         (let-values ([(key value) (split-first piece #\=)])
                           (cons (percent-decode who key #t)
                                 (percent-decode who (or value "") #t))))
                       (string-split (uri-query u) #\&))))))

  #|proc:uri-resolve
The `uri-resolve` procedure resolves a URI reference against a base URI.
|#
  (define-who uri-resolve
    (lambda (base ref)
      (pcheck ([uri? base] [uri? ref])
              (cond
               [(uri-scheme ref)
                (%make-uri (uri-scheme ref)
                           (uri-userinfo ref)
                           (uri-host ref)
                           (uri-port ref)
                           (normalize-path (uri-path ref))
                           (uri-query ref)
                           (uri-fragment ref))]
               [(uri-host ref)
                (%make-uri (uri-scheme base)
                           (uri-userinfo ref)
                           (uri-host ref)
                           (uri-port ref)
                           (normalize-path (uri-path ref))
                           (uri-query ref)
                           (uri-fragment ref))]
               [(string=? (uri-path ref) "")
                (%make-uri (uri-scheme base)
                           (uri-userinfo base)
                           (uri-host base)
                           (uri-port base)
                           (uri-path base)
                           (or (uri-query ref) (uri-query base))
                           (uri-fragment ref))]
               [(and (fx> (string-length (uri-path ref)) 0)
                     (char=? (string-ref (uri-path ref) 0) #\/))
                (%make-uri (uri-scheme base)
                           (uri-userinfo base)
                           (uri-host base)
                           (uri-port base)
                           (normalize-path (uri-path ref))
                           (uri-query ref)
                           (uri-fragment ref))]
               [else
                (%make-uri (uri-scheme base)
                           (uri-userinfo base)
                           (uri-host base)
                           (uri-port base)
                           (normalize-path (merge-path base (uri-path ref)))
                           (uri-query ref)
                           (uri-fragment ref))]))))

  #|proc:uri-normalize
The `uri-normalize` procedure normalizes URI casing, dot segments, and default ports.
|#
  (define-who uri-normalize
    (lambda (u)
      (pcheck ([uri? u])
              (let* ([scheme (and (uri-scheme u) (string-downcase (uri-scheme u)))]
                     [host (and (uri-host u) (string-downcase (uri-host u)))]
                     [port (uri-port u)]
                     [default-port (and scheme (default-port-for-scheme scheme))])
                (%make-uri scheme
                           (uri-userinfo u)
                           host
                           (if (and default-port port (fx= default-port port))
                               #f
                               port)
                           (normalize-path (uri-path u))
                           (uri-query u)
                           (uri-fragment u))))))

  #|proc:uri-encode
The `uri-encode` procedure percent-encodes a string for use in URI components.
|#
  (define-who uri-encode
    (lambda (str)
      (pcheck ([string? str])
              (let ([bv (string->utf8 str)])
                (let loop ([i 0] [out '()])
                  (if (fx= i (bytevector-length bv))
                      (apply string-append (reverse out))
                      (let ([u8 (bytevector-u8-ref bv i)])
                        (loop (fx1+ i)
                              (cons (if (unreserved-byte? u8)
                                        (string (integer->char u8))
                                        (string-append "%" (byte->hex u8)))
                                    out)))))))))

  #|proc:uri-decode
The `uri-decode` procedure decodes percent escapes in a URI component string.
|#
  (define-who uri-decode
    (lambda (str)
      (pcheck ([string? str])
              (percent-decode who str #f))))

  #|proc:form-urlencode
The `form-urlencode` procedure encodes an association list into an `application/x-www-form-urlencoded` string.
|#
  (define-who form-urlencode
    (lambda (alist)
      (unless (list? alist)
        (errorf who "expected association list, given ~s" alist))
      (let ([encode-component
             (lambda (x)
               (let ([bv (string->utf8 x)])
                 (let loop ([i 0] [out '()])
                   (if (fx= i (bytevector-length bv))
                       (apply string-append (reverse out))
                       (let ([u8 (bytevector-u8-ref bv i)])
                         (loop (fx1+ i)
                               (cons (cond
                                      [(fx= u8 32) "+"]
                                      [(unreserved-byte? u8) (string (integer->char u8))]
                                      [else (string-append "%" (byte->hex u8))])
                                     out)))))))])
        (join-strings
         (map (lambda (entry)
                (unless (pair? entry)
                  (errorf who "expected association list entry, given ~s" entry))
                (let ([key (car entry)] [value (cdr entry)])
                  (unless (string? key)
                    (errorf who "expected string form key, given ~s" key))
                  (unless (string? value)
                    (errorf who "expected string form value, given ~s" value))
                  (string-append (encode-component key)
                                 "="
                                 (encode-component value))))
              alist)
         "&"))))

  #|proc:form-urldecode
The `form-urldecode` procedure decodes an `application/x-www-form-urlencoded` string into an association list.
|#
  (define-who form-urldecode
    (lambda (str)
      (pcheck ([string? str])
              (if (string=? str "")
                  '()
                  (map (lambda (piece)
                         (let-values ([(key value) (split-first piece #\=)])
                           (cons (percent-decode who key #t)
                                 (percent-decode who (or value "") #t))))
                       (string-split str #\&))))))
  )
