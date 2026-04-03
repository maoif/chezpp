(define start-ftp-test-server
  (lambda ()
    (let ([root "/tmp/chezpp-net-ftp-root"])
      (when (file-exists? root)
        (file-removetree root #f))
      (mkdirs (string-append root "/docs"))
      (mkdirs (string-append root "/slow"))
      (write-bytevector-file (string-append root "/hello.txt") (string->utf8 "hello ftp"))
      (write-bytevector-file (string-append root "/docs/readme.txt") (string->utf8 "doc file"))
      (write-bytevector-file (string-append root "/slow/wait.txt") (string->utf8 "slow file"))
      (let ([listener (open-socket 'inet 'stream)])
        (socket-set-option! listener 'reuse-address #t)
        (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
        (socket-listen! listener 8)
        (let ([port (socket-address-port (socket-local-address listener))])
          (define running? #t)
          (define client-threads '())
          (define client-sockets '())
          (define remove-client-socket!
            (lambda (client)
              (let loop ([rest client-sockets] [out '()])
                (cond
                 [(null? rest)
                  (set! client-sockets (reverse out))]
                 [(eq? (car rest) client)
                  (set! client-sockets (append (reverse out) (cdr rest)))]
                 [else
                  (loop (cdr rest) (cons (car rest) out))]))))
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
                                (begin
                                  (when (string=? target "/slow")
                                    (milisleep 200))
                                  (send-crlf-line op "150 opening data connection")
                                  (let ([data (data-accept)])
                                    (let ([dop (open-socket-output-port data)])
                                      (for-each (lambda (entry)
                                                  (put-bytevector dop (string->utf8 entry)))
                                                (list-dir path))
                                      (flush-output-port dop)
                                      (close-port dop))
                                    (close-socket data)
                                    (send-crlf-line op "226 transfer complete")))
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
                                (begin
                                  (send-crlf-line op "150 opening data connection")
                                  (let ([data (data-accept)])
                                    (let ([dop (open-socket-output-port data)])
                                      (put-bytevector dop (read-u8vec path))
                                      (flush-output-port dop)
                                      (close-port dop))
                                    (close-socket data)
                                    (send-crlf-line op "226 transfer complete")))
                                (send-crlf-line op "550 unavailable"))
                            (loop))]
                         [(string=? cmd "STOR")
                          (let* ([target (test-path-join cwd arg)]
                                 [path (physical-path target)])
                            (ensure-parent-dir path)
                            (send-crlf-line op "150 opening data connection")
                            (let ([data (data-accept)])
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
              (set! client-sockets (cons client client-sockets))
              (let ([th
                     (fork-thread
                      (lambda ()
                        (guard (c [else #f])
                          (handle-client client))
                        (remove-client-socket! client)
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
                           (milisleep 50)
                           (loop)))))))))
          (milisleep 50)
          (values root
                  port
                  (lambda ()
                    (set! running? #f)
                    (guard (c [else #f])
                      (close-socket listener))
                    (for-each (lambda (client)
                                (guard (c [else #f])
                                  (close-socket client)))
                              client-sockets)
                    (thread-join server-thread)
                    (for-each thread-join client-threads)
                    (when (file-exists? root)
                      (file-removetree root #f)))))))))
