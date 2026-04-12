(define slurp-text-port
  (lambda (ip)
    (call-with-string-output-port
     (lambda (op)
       (let loop ()
         (let ([c (read-char ip)])
           (unless (eof-object? c)
             (write-char c op)
             (loop))))))))

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
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                     (format "../newpp --script net-ftp-server-process.ss ~s" root)
                     (buffer-mode block)
                     (native-transcoder))])
        (let ([port
               (let ([v (read from-stdout)])
                 (if (and (integer? v) (<= 0 v 65535))
                     v
                     (let ([stderr-stuff (slurp-text-port from-stderr)])
                       (close-port to-stdin)
                       (close-port from-stdout)
                       (close-port from-stderr)
                       (errorf 'start-ftp-test-server
                               "failed to start FTP helper server: ~a"
                               stderr-stuff))))])
          (values root
                  port
                  (lambda ()
                    (guard (c [else #f])
                      (display "stop\n" to-stdin)
                      (flush-output-port to-stdin))
                    (guard (c [else #f])
                      (close-port to-stdin))
                    (let* ([stdout-stuff (slurp-text-port from-stdout)]
                           [stderr-stuff (slurp-text-port from-stderr)]
                           [stderr-tail (string-trim stderr-stuff)])
                      (close-port from-stdout)
                      (close-port from-stderr)
                      (unless (string=? stderr-tail "")
                        (errorf 'start-ftp-test-server
                                "FTP helper server produced output\nSTDOUT:\n~a\nSTDERR:\n~a"
                                stdout-stuff
                                stderr-stuff)))
                    (when (file-exists? root)
                      (file-removetree root #f)))))))))
