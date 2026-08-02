(define rpc-echo-example-host "127.0.0.1")
(define rpc-echo-example-port 41116)
(define grpc-echo-example-host "127.0.0.1")
(define grpc-echo-example-port 41117)

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

(define with-grpc-example-env
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

(define parse-port-argument
  (lambda (who s)
    (let ([n (string->number s)])
      (unless (and (integer? n) (exact? n))
        (errorf who "expected port number string, given ~s" s))
      (when (or (< n 0) (> n 65535))
        (errorf who "port must be between 0 and 65535, given ~s" n))
      n)))

(define join-command-arguments
  (lambda (arg*)
    (let loop ([rest arg*] [out ""])
      (if (null? rest)
          out
          (loop (cdr rest)
                (if (string=? out "")
                    (car rest)
                    (string-append out " " (car rest))))))))

(define scheme-value->string
  (lambda (x)
    (call-with-string-output-port
     (lambda (op)
       (write x op)))))

(define string->scheme-value
  (lambda (s)
    (call-with-port
     (open-string-input-port s)
     (lambda (ip)
       (get-datum ip)))))

(define pump-binary-input-port!
  (lambda (ip op)
    (let loop ()
      (let ([chunk (get-bytevector-n ip 4096)])
        (unless (eof-object? chunk)
          (put-bytevector op chunk)
          (flush-output-port op)
          (loop))))))

(define call-with-example-output-ports
  (lambda (proc)
    (let ([out (open-file-output-port "/proc/self/fd/1"
                                      (file-options no-fail)
                                      (buffer-mode block)
                                      #f)]
          [err (open-file-output-port "/proc/self/fd/2"
                                      (file-options no-fail)
                                      (buffer-mode block)
                                      #f)])
      (dynamic-wind
        void
        (lambda ()
          (proc out err))
        (lambda ()
          (close-port out)
          (close-port err))))))

(define example-scheme-value*
  (list #t
        42
        -3.5
        "hello"
        '(1 "two" #t)
        #vu8(1 2 3 4)))

(define ssh-authenticate!
  (lambda (who session user auth-kind auth-arg)
    (cond
     [(string=? auth-kind "agent")
      (ssh-auth-agent! session user)]
     [(string=? auth-kind "password")
      (unless (string? auth-arg)
        (errorf who "password authentication requires a password argument"))
      (ssh-auth-password! session user auth-arg)]
     [(string=? auth-kind "publickey")
      (ssh-auth-publickey! session
                           user
                           (and auth-arg
                                (not (string=? auth-arg "-"))
                                auth-arg))]
     [else
      (errorf who "invalid SSH auth kind ~s" auth-kind)])))
