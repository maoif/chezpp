(define ftp-client-commands
  '("ls" "pwd" "cd" "mkdir" "rmdir" "rm" "rename" "get" "put" "quit"))

#|proc:run-ftp-client
The `run-ftp-client` procedure runs the FTP command loop. The `host`, `port`, `user`, and
`password` parameters identify the server and credentials. Commands are read from `input`, and
command results are written to `output`. The return value is unspecified.
|#
(define run-ftp-client
  (lambda (host port user password input output)
    (pcheck ([string? host user password] [fixnum? port]
             [input-port? input] [output-port? output])
      (let ([session (ftp-open host port #f 30000)])
        (dynamic-wind
          void
          (lambda ()
            (ftp-login! session user password)
            (let loop ()
              (let ([line (get-line input)])
                (unless (eof-object? line)
                  (let ([command (interactive-command line ftp-client-commands)])
                    (when command
                      (let ([name (car command)] [arg* (cdr command)])
                        (cond
                         [(string=? name "quit") (put-string output "bye\n")]
                         [(string=? name "pwd")
                          (put-string output (string-append (ftp-pwd session) "\n"))
                          (loop)]
                         [(string=? name "cd")
                          (interactive-arity! name arg* 1 1)
                          (ftp-cwd! session (car arg*))
                          (loop)]
                         [(string=? name "ls")
                          (interactive-arity! name arg* 0 1)
                          (for-each
                           (lambda (entry)
                             (put-string
                              output
                              (format "~a\t~a\n"
                                      (ftp-directory-entry-type entry)
                                      (ftp-directory-entry-name entry))))
                           (ftp-list session (if (null? arg*) "." (car arg*))))
                          (loop)]
                         [(string=? name "mkdir")
                          (interactive-arity! name arg* 1 1)
                          (ftp-mkdir! session (car arg*))
                          (loop)]
                         [(string=? name "rmdir")
                          (interactive-arity! name arg* 1 1)
                          (ftp-rmdir! session (car arg*))
                          (loop)]
                         [(string=? name "rm")
                          (interactive-arity! name arg* 1 1)
                          (ftp-delete! session (car arg*))
                          (loop)]
                         [(string=? name "rename")
                          (interactive-arity! name arg* 2 2)
                          (ftp-rename! session (car arg*) (cadr arg*))
                          (loop)]
                         [(string=? name "get")
                          (interactive-arity! name arg* 2 2)
                          (ftp-download session (car arg*) (cadr arg*)
                                        default-transfer-policy)
                          (loop)]
                         [(string=? name "put")
                          (interactive-arity! name arg* 2 2)
                          (ftp-upload session (car arg*) (cadr arg*)
                                      default-transfer-policy)
                          (loop)]))))))))
          (lambda () (ftp-close session)))))))
