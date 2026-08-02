;; Shared configuration and helpers for the file-transfer examples.

(define file-transfer-host "127.0.0.1")

(define tcp-socket-file-transfer-port 41001)
(define http-file-transfer-port 41002)
(define ftp-file-transfer-port 41003)
(define sftp-file-transfer-port 41004)
(define websocket-file-transfer-port 41005)
(define rpc-file-transfer-port 41006)
(define grpc-file-transfer-port 41007)

(define file-transfer-done-marker-name ".chezpp-upload.done")
(define sftp-file-transfer-state-root "/tmp/chezpp-example-sftp")

(define write-bytevector-file
  (lambda (path bv)
    (call-with-port
     (open-file-output-port path
                            (file-options no-fail replace)
                            (buffer-mode block)
                            #f)
     (lambda (op)
       (put-bytevector op bv)))))

(define read-port->bytevector
  (lambda (ip)
    (let loop ([part* '()] [total 0])
      (let ([chunk (get-bytevector-n ip 4096)])
        (if (eof-object? chunk)
            (let ([out (make-bytevector total 0)])
              (let fill ([rest (reverse part*)] [i 0])
                (if (null? rest)
                    out
                    (let* ([part (car rest)]
                           [n (bytevector-length part)])
                      (bytevector-copy! part 0 out i n)
                      (fill (cdr rest) (+ i n))))))
            (loop (cons chunk part*) (+ total (bytevector-length chunk))))))))

(define u8-list->bytevector
  (lambda (u8*)
    (let ([out (make-bytevector (length u8*) 0)])
      (let loop ([rest u8*] [i 0])
        (unless (null? rest)
          (bytevector-u8-set! out i (car rest))
          (loop (cdr rest) (+ i 1))))
      out)))

(define copy-bytevector-range
  (lambda (bv start stop)
    (let ([out (make-bytevector (- stop start) 0)])
      (bytevector-copy! bv start out 0 (- stop start))
      out)))

(define read-exactly
  (lambda (ip size)
    (let loop ([part* '()] [remaining size] [total 0])
      (if (= remaining 0)
          (let ([out (make-bytevector total 0)])
            (let fill ([rest (reverse part*)] [i 0])
              (if (null? rest)
                  out
                  (let* ([part (car rest)]
                         [n (bytevector-length part)])
                    (bytevector-copy! part 0 out i n)
                    (fill (cdr rest) (+ i n))))))
          (let ([chunk (get-bytevector-n ip remaining)])
            (if (eof-object? chunk)
                (errorf 'read-exactly "unexpected EOF while reading ~a bytes" size)
                (loop (cons chunk part*)
                      (- remaining (bytevector-length chunk))
                      (+ total (bytevector-length chunk)))))))))

(define path-basename
  (lambda (path)
    (let loop ([i (- (string-length path) 1)])
      (cond
       [(< i 0) path]
       [(char=? (string-ref path i) #\/)
        (substring path (+ i 1) (string-length path))]
       [else
        (loop (- i 1))]))))

(define path-join
  (lambda (dir leaf)
    (if (or (string=? dir "")
            (char=? (string-ref dir (- (string-length dir) 1)) #\/))
        (string-append dir leaf)
        (string-append dir "/" leaf))))

(define done-marker-path
  (lambda (dir)
    (path-join dir file-transfer-done-marker-name)))

(define ensure-upload-directory
  (lambda (dir)
    (unless (string? dir)
      (errorf 'ensure-upload-directory "expected directory string, given ~s" dir))
    (mkdirs dir)
    dir))

(define delete-file/ignore
  (lambda (path)
    (guard (c [else #f])
      (when (file-exists? path)
        (delete-file path #f)))))

(define wait-for-file
  (lambda (path)
    (let loop ()
      (unless (file-exists? path)
        (milisleep 50)
        (loop)))))

(define validate-file-list
  (lambda (who path*)
    (unless (list? path*)
      (errorf who "expected list of file paths, given ~s" path*))
    (for-each
     (lambda (path)
       (unless (string? path)
         (errorf who "expected file path string, given ~s" path))
       (unless (file-regular? path)
         (errorf who "file does not exist or is not regular: ~a" path)))
     path*)
    path*))

(define u32-set!
  (lambda (bv index value)
    (bytevector-u8-set! bv index (fxlogand (fxsrl value 24) #xff))
    (bytevector-u8-set! bv (+ index 1) (fxlogand (fxsrl value 16) #xff))
    (bytevector-u8-set! bv (+ index 2) (fxlogand (fxsrl value 8) #xff))
    (bytevector-u8-set! bv (+ index 3) (fxlogand value #xff))))

(define u32-ref
  (lambda (bv index)
    (fxlogor (fxsll (bytevector-u8-ref bv index) 24)
             (fxsll (bytevector-u8-ref bv (+ index 1)) 16)
             (fxsll (bytevector-u8-ref bv (+ index 2)) 8)
             (bytevector-u8-ref bv (+ index 3)))))

(define make-transfer-frame
  (lambda (name payload)
    (let* ([name-bv (if name (string->utf8 name) #vu8())]
           [payload-bv (or payload #vu8())]
           [name-len (bytevector-length name-bv)]
           [payload-len (bytevector-length payload-bv)]
           [out (make-bytevector (+ 8 name-len payload-len) 0)])
      (u32-set! out 0 name-len)
      (u32-set! out 4 payload-len)
      (bytevector-copy! name-bv 0 out 8 name-len)
      (bytevector-copy! payload-bv 0 out (+ 8 name-len) payload-len)
      out)))

(define parse-transfer-frame
  (lambda (frame)
    (let* ([name-len (u32-ref frame 0)]
           [payload-len (u32-ref frame 4)]
           [need (+ 8 name-len payload-len)])
      (unless (= need (bytevector-length frame))
        (errorf 'parse-transfer-frame "invalid frame size ~s" (bytevector-length frame)))
      (let ([name (if (= name-len 0)
                      ""
                      (utf8->string (copy-bytevector-range frame 8 (+ 8 name-len))))]
            [payload (copy-bytevector-range frame (+ 8 name-len) need)])
        (values name payload)))))

(define transfer-done-frame?
  (lambda (name payload)
    (and (string=? name "")
         (= (bytevector-length payload) 0))))

(define store-upload!
  (lambda (dir name payload)
    (write-bytevector-file (path-join dir name) payload)
    name))

(define with-env
  (lambda (name value proc)
    (let ([old (getenv name)])
      (dynamic-wind
        (lambda () (putenv name value))
        proc
        (lambda () (putenv name (or old "")))))))

(define with-env*
  (lambda (binding* proc)
    (if (null? binding*)
        (proc)
        (with-env (caar binding*)
                  (cdar binding*)
                  (lambda ()
                    (with-env* (cdr binding*) proc))))))

(define run-command!
  (lambda (who cmd)
    (unless (= (system cmd) 0)
      (errorf who "command failed: ~a" cmd))))

(define wait-for-ready-server
  (lambda (host port)
    (let loop ([attempt 50])
      (if (= attempt 0)
          (errorf 'wait-for-ready-server "server did not start on ~a:~a" host port)
          (guard (c [else
                     (milisleep 50)
                     (loop (- attempt 1))])
            (let ([sock (open-socket 'inet 'stream)])
              (dynamic-wind
                void
                (lambda ()
                  (socket-connect! sock (make-socket-address 'inet host port)))
                (lambda ()
                  (close-socket sock)))
              #t))))))

(define with-grpc-env
  (lambda (proc)
    (with-env* '(("http_proxy" . "")
                 ("https_proxy" . "")
                 ("all_proxy" . "")
                 ("HTTP_PROXY" . "")
                 ("HTTPS_PROXY" . "")
                 ("ALL_PROXY" . "")
                 ("no_proxy" . "127.0.0.1,localhost")
                 ("NO_PROXY" . "127.0.0.1,localhost"))
               proc)))

(define benign-http-eof?
  (lambda (c)
    (and (net-error? c)
         (string=? (net-error-message c)
                   "unexpected EOF while reading HTTP request"))))

(define transfer-frame-tag-begin 0)
(define transfer-frame-tag-chunk 1)
(define transfer-frame-tag-end 2)
(define transfer-frame-tag-done 3)

(define make-stream-transfer-frame
  (lambda (tag name payload)
    (let* ([name-bv (if name (string->utf8 name) #vu8())]
           [payload-bv (or payload #vu8())]
           [name-len (bytevector-length name-bv)]
           [payload-len (bytevector-length payload-bv)]
           [out (make-bytevector (+ 9 name-len payload-len) 0)])
      (bytevector-u8-set! out 0 tag)
      (u32-set! out 1 name-len)
      (u32-set! out 5 payload-len)
      (bytevector-copy! name-bv 0 out 9 name-len)
      (bytevector-copy! payload-bv 0 out (+ 9 name-len) payload-len)
      out)))

(define parse-stream-transfer-frame
  (lambda (frame)
    (unless (>= (bytevector-length frame) 9)
      (errorf 'parse-stream-transfer-frame "invalid frame size ~s" (bytevector-length frame)))
    (let* ([tag (bytevector-u8-ref frame 0)]
           [name-len (u32-ref frame 1)]
           [payload-len (u32-ref frame 5)]
           [need (+ 9 name-len payload-len)])
      (unless (= need (bytevector-length frame))
        (errorf 'parse-stream-transfer-frame "invalid frame size ~s" (bytevector-length frame)))
      (let ([name (if (= name-len 0)
                      ""
                      (utf8->string (copy-bytevector-range frame 9 (+ 9 name-len))))]
            [payload (copy-bytevector-range frame (+ 9 name-len) need)])
        (values tag name payload)))))

(define make-upload-frame-handler
  (lambda (who dir)
    (let ([current-name #f]
          [current-op #f])
      (define close-current!
        (lambda ()
          (when current-op
            (close-port current-op)
            (set! current-op #f)
            (set! current-name #f))))
      (values
       (lambda (tag name payload)
         (case tag
           [(0)
            (when current-op
              (errorf who "received begin frame for ~a before ending ~a" name current-name))
            (when (string=? name "")
              (errorf who "begin frame requires a file name"))
            (set! current-name name)
            (set! current-op
                  (open-file-output-port (path-join dir name)
                                         (file-options no-fail replace)
                                         (buffer-mode block)
                                         #f))
            #f]
           [(1)
            (unless current-op
              (errorf who "received chunk frame without a current file"))
            (put-bytevector current-op payload)
            #f]
           [(2)
            (unless current-op
              (errorf who "received end frame without a current file"))
            (close-current!)
            #f]
           [(3)
            (close-current!)
            #t]
           [else
            (errorf who "invalid transfer frame tag ~s" tag)]))
       close-current!))))

(define send-path-via-transfer-frames
  (lambda (send-frame path)
    (let ([name (path-basename path)])
      (send-frame transfer-frame-tag-begin name #vu8())
      (call-with-port
       (open-file-input-port path
                             (file-options)
                             (buffer-mode block)
                             #f)
       (lambda (ip)
         (let loop ()
           (let ([chunk (get-bytevector-n ip 65536)])
             (unless (eof-object? chunk)
               (send-frame transfer-frame-tag-chunk "" chunk)
               (loop))))))
      (send-frame transfer-frame-tag-end "" #vu8()))))

(define await-websocket-client
  (lambda (server)
    (websocket-accept server 300000)))
