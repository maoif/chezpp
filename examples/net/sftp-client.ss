(import (chezpp))
(load "examples/net/interactive-transfer-common.ss")

(define sftp-client-commands
  '("ls" "pwd" "cd" "mkdir" "rmdir" "rm" "rename" "get" "put" "quit"))

#|proc:run-sftp-client
The `run-sftp-client` procedure runs the SFTP command loop for `host`, `port`, `user`, and an
authentication argument. The argument is `agent` or a private-key path; commands use `input` and
`output` ports and never invoke a shell.
|#
(define run-sftp-client
  (lambda (host port user authentication input output)
    (pcheck ([string? host user authentication] [fixnum? port]
             [input-port? input] [output-port? output])
      (let ([ssh (ssh-open-with-policy host port user 30000 'accept-new)])
        (dynamic-wind
          void
          (lambda ()
            (if (string=? authentication "agent")
                (ssh-auth-agent! ssh user)
                (ssh-auth-private-key! ssh user (string-append authentication ".pub") authentication #f))
            (let ([session (sftp-open ssh)])
              (dynamic-wind
                void
                (lambda ()
                  (let loop ()
                    (let ([line (get-line input)])
                      (unless (eof-object? line)
                        (let ([command (interactive-command line sftp-client-commands)])
                          (when command
                            (let ([name (car command)] [arg* (cdr command)])
                              (cond
                               [(string=? name "quit") (put-string output "bye\n")]
                               [(string=? name "pwd") (put-string output (string-append (sftp-pwd session) "\n")) (loop)]
                               [(string=? name "cd") (interactive-arity! name arg* 1 1) (sftp-cwd! session (car arg*)) (loop)]
                               [(string=? name "ls")
                                (interactive-arity! name arg* 0 1)
                                (for-each (lambda (entry) (put-string output (format "~a\t~a\n" (sftp-attributes-type entry) (sftp-attributes-name entry))))
                                          (sftp-list session (if (null? arg*) "." (car arg*))))
                                (loop)]
                               [(string=? name "mkdir") (interactive-arity! name arg* 1 1) (sftp-mkdir! session (car arg*)) (loop)]
                               [(string=? name "rmdir") (interactive-arity! name arg* 1 1) (sftp-rmdir! session (car arg*)) (loop)]
                               [(string=? name "rm") (interactive-arity! name arg* 1 1) (sftp-delete! session (car arg*)) (loop)]
                               [(string=? name "rename") (interactive-arity! name arg* 2 2) (sftp-rename! session (car arg*) (cadr arg*)) (loop)]
                               [(string=? name "get") (interactive-arity! name arg* 2 2) (sftp-download session (car arg*) (cadr arg*) default-transfer-policy) (loop)]
                               [(string=? name "put") (interactive-arity! name arg* 2 2) (sftp-upload session (car arg*) (cadr arg*) default-transfer-policy) (loop)]))))))))
                (lambda () (sftp-close session)))))
          (lambda () (ssh-close ssh)))))))

(let ([arg* (command-line-arguments)])
  (unless (= (length arg*) 4)
    (errorf 'sftp-client "usage: sftp-client HOST PORT USER AGENT-OR-PRIVATE-KEY"))
  (run-sftp-client (car arg*) (string->number (cadr arg*)) (caddr arg*) (cadddr arg*)
                   (current-input-port) (current-output-port)))
