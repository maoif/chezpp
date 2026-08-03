(define join-path-segment*
  (lambda (segment*)
    (let loop ([rest segment*] [out ""])
      (if (null? rest)
          out
          (loop (cdr rest)
                (if (string=? out "")
                    (car rest)
                    (string-append out "/" (car rest))))))))

(define normalize-absolute-upload-path
  (lambda (path)
    (let loop ([rest (string-split path #\/)] [stack '()])
      (if (null? rest)
          (let ([joined (join-path-segment* (reverse stack))])
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

(define server-path-join
  (lambda (cwd path)
    (normalize-absolute-upload-path
     (if (and (> (string-length path) 0)
              (char=? (string-ref path 0) #\/))
         path
         (if (string=? cwd "/")
             (string-append "/" path)
             (string-append cwd "/" path))))))

(define path-dirname
  (lambda (path)
    (let ([abs (normalize-absolute-upload-path path)])
      (let loop ([i (- (string-length abs) 1)])
        (cond
         [(<= i 0) "/"]
         [(char=? (string-ref abs i) #\/)
          (if (= i 0)
              "/"
              (substring abs 0 i))]
         [else
          (loop (- i 1))])))))

(define read-crlf-line
  (lambda (ip)
    (let loop ([rev '()])
      (let ([b (get-u8 ip)])
        (cond
         [(eof-object? b)
          (and (pair? rev)
               (utf8->string (u8-list->bytevector (reverse rev))))]
         [(= b 10)
          (let ([rev* (if (and (pair? rev) (= (car rev) 13))
                          (cdr rev)
                          rev)])
            (utf8->string (u8-list->bytevector (reverse rev*))))]
         [else
          (loop (cons b rev))])))))

(define send-crlf-line
  (lambda (op line)
    (put-bytevector op (string->utf8 (string-append line "\r\n")))
    (flush-output-port op)))

(define ftp-data-physical-path
  (lambda (root virtual-path)
    (string-append root (normalize-absolute-upload-path virtual-path))))

(define ftp-open-passive-listener
  (lambda ()
    (let ([sock (open-socket 'inet 'stream)])
      (socket-set-option! sock 'reuse-address #t)
      (socket-bind! sock (make-socket-address 'inet file-transfer-host 0))
      (socket-listen! sock 1)
      sock)))

(define ftp-close-passive-listener
  (lambda (sock)
    (when sock
      (guard (c [else #f])
        (close-socket sock)))))

(define write-port-input-file
  (lambda (path ip)
    (call-with-port
     (open-file-output-port path
                            (file-options no-fail replace)
                            (buffer-mode block)
                            #f)
     (lambda (op)
       (let loop ()
         (let ([chunk (get-bytevector-n ip 65536)])
           (unless (eof-object? chunk)
             (put-bytevector op chunk)
             (loop))))))))

(define ftp-handle-client
  (lambda (client root)
    (let ([ip (open-socket-input-port client)]
          [op (open-socket-output-port client)])
      (define cwd "/")
      (define passive-listener #f)
      (define data-accept
        (lambda ()
          (unless passive-listener
            (errorf 'ftp-file-server "passive listener missing"))
          (let-values ([(data peer) (socket-accept passive-listener)])
            (ftp-close-passive-listener passive-listener)
            (set! passive-listener #f)
            data)))
      (send-crlf-line op "220 chezpp file transfer ftp server")
      (let loop ()
        (let ([line (read-crlf-line ip)])
          (when line
            (let* ([part* (string-split line #\space)]
                   [cmd (string-upcase (car part*))]
                   [arg (if (> (string-length line) (+ (string-length (car part*)) 1))
                            (substring line (+ (string-length (car part*)) 1)
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
                (set! cwd (server-path-join cwd arg))
                (send-crlf-line op "250 directory changed")
                (loop)]
               [(or (string=? cmd "PASV") (string=? cmd "EPSV"))
                (ftp-close-passive-listener passive-listener)
                (set! passive-listener (ftp-open-passive-listener))
                (let ([port (socket-address-port (socket-local-address passive-listener))])
                  (if (string=? cmd "PASV")
                      (let ([p1 (quotient port 256)]
                            [p2 (mod port 256)])
                        (send-crlf-line
                         op
                         (format "227 Entering Passive Mode (127,0,0,1,~a,~a)" p1 p2)))
                      (send-crlf-line
                       op
                       (format "229 Entering Extended Passive Mode (|||~a|)" port))))
               (loop)]
               [(string=? cmd "STOR")
                (let ([path (validated-upload-path 'ftp-file-server root arg)])
                  (send-crlf-line op "150 opening data connection")
                  (let ([data (data-accept)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (call-with-port
                         (open-socket-input-port data)
                         (lambda (dip)
                           (write-port-input-file path dip))))
                      (lambda ()
                        (close-socket data))))
                  (send-crlf-line op "226 transfer complete")
                  (loop))]
               [(string=? cmd "QUIT")
                (send-crlf-line op "221 bye")]
               [else
                (send-crlf-line op "502 command not implemented")
                (loop)]))))))))

#|proc:ftp-file-server
The `ftp-file-server` procedure runs a minimal localhost FTP upload server rooted
at `dir` and returns after the client uploads the done marker.
|#
(define ftp-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (delete-file/ignore (done-marker-path dir))
      (let ([listener (open-socket 'inet 'stream)])
        (dynamic-wind
          (lambda ()
            (socket-set-option! listener 'reuse-address #t)
            (socket-bind! listener
                          (make-socket-address 'inet
                                               file-transfer-host
                                               ftp-file-transfer-port))
            (socket-listen! listener 4))
          (lambda ()
            (let loop ()
              (if (file-exists? (done-marker-path dir))
                  (begin
                    (delete-file/ignore (done-marker-path dir))
                    dir)
                  (let-values ([(client peer) (socket-accept listener)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (ftp-handle-client client dir))
                      (lambda ()
                        (close-socket client)))
                    (loop)))))
          (lambda ()
            (close-socket listener)))))))

#|proc:ftp-file-client
The `ftp-file-client` procedure uploads each file in `path*` to the localhost
FTP upload server and then uploads the done marker.
|#
(define ftp-file-client
  (lambda (path*)
    (validate-file-list 'ftp-file-client path*)
    (let ([url (format "ftp://~a:~a/" file-transfer-host ftp-file-transfer-port)]
          [done-path (path-join "/tmp" "chezpp-example-ftp-done-marker")])
      (define upload-one
        (lambda (local-path remote-path)
          (let ([session (ftp-open url 300000)])
            (dynamic-wind
              void
              (lambda ()
                (ftp-login! session "user" "pass")
                (ftp-upload session local-path remote-path))
              (lambda ()
                (ftp-close session))))))
      (dynamic-wind
        (lambda ()
          (write-bytevector-file done-path #vu8()))
        (lambda ()
          (for-each
           (lambda (path)
             (upload-one path (path-basename path)))
           path*)
          (upload-one done-path file-transfer-done-marker-name)
          path*)
        (lambda ()
          (delete-file/ignore done-path))))))
