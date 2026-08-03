(import (chezpp))

(load "examples/net/net-example-common.ss")

(define parse-ssh-open-repl-arguments
  (lambda (arg*)
    (unless (>= (length arg*) 4)
      (errorf 'ssh-open-repl
              "expected host port user auth-kind arguments, given ~s"
              arg*))
    (let ([host (car arg*)]
          [port (parse-port-argument 'ssh-open-repl (cadr arg*))]
          [user (caddr arg*)]
          [auth-kind (cadddr arg*)]
          [rest (cddddr arg*)])
      (cond
       [(string=? auth-kind "agent")
        (unless (null? rest)
          (errorf 'ssh-open-repl
                  "agent authentication does not take an extra argument, given ~s"
                  rest))
        (values host port user auth-kind #f)]
       [(or (string=? auth-kind "password")
            (string=? auth-kind "publickey"))
        (unless (= (length rest) 1)
          (errorf 'ssh-open-repl
                  "authentication kind ~s requires exactly one secret/passphrase argument"
                  auth-kind))
        (values host port user auth-kind (car rest))]
       [else
        (errorf 'ssh-open-repl "invalid SSH auth kind ~s" auth-kind)]))))

#|proc:ssh-open-repl
The `ssh-open-repl` procedure opens an authenticated interactive shell on
`host:port` and forwards local terminal input/output to the remote shell.
|#
(define ssh-open-repl
  (lambda (host port user auth-kind auth-arg)
    (pcheck ([string? host user auth-kind] [fixnum? port])
      (let ([session (ssh-open host port user)]
            [stop? (vector #f)])
        (dynamic-wind
          void
          (lambda ()
            (ssh-authenticate! 'ssh-open-repl session user auth-kind auth-arg)
            (call-with-ssh-channel
             session
             (lambda (channel)
               (ssh-request-pty! channel)
               (ssh-shell channel)
               (call-with-example-output-ports
                (lambda (out err)
                  (let ([op (open-ssh-channel-output-port channel)]
                        [ip (open-ssh-channel-input-port channel)]
                        [ep (open-ssh-channel-error-port channel)])
                    (let ([stdout-thread
                           (fork-thread
                            (lambda ()
                              (pump-binary-input-port! ip out)
                              (vector-set! stop? 0 #t)))]
                          [stderr-thread
                           (fork-thread
                            (lambda ()
                              (pump-binary-input-port! ep err)
                              (vector-set! stop? 0 #t)))]
                          [stdin-thread
                           (fork-thread
                            (lambda ()
                              (let loop ()
                                (unless (vector-ref stop? 0)
                                  (if (char-ready? (current-input-port))
                                      (let ([ch (get-char (current-input-port))])
                                        (if (eof-object? ch)
                                            (vector-set! stop? 0 #t)
                                            (begin
                                              (put-bytevector op
                                                              (string->utf8
                                                               (string ch)))
                                              (flush-output-port op)
                                              (loop))))
                                      (begin
                                        (milisleep 20)
                                        (loop)))))))])
                      (thread-join stdout-thread)
                      (thread-join stderr-thread)
                      (vector-set! stop? 0 #t)
                      (thread-join stdin-thread)
                      channel)))))))
          (lambda ()
            (ssh-close session)))))))

(let-values ([(host port user auth-kind auth-arg)
              (parse-ssh-open-repl-arguments (command-line-arguments))])
  (ssh-open-repl host port user auth-kind auth-arg))
