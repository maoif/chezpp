(import (chezpp) (chezpp logging) (chezpp system signal)
        (only (chezpp net ffi) ffi-ssh-terminal-size ffi-ssh-terminal-raw-start
                              ffi-ssh-terminal-raw-close))
(load "examples/common.ss")
(load "examples/net/ssh-example-common.ss")

;; A local PTY owns line editing, echo and signals only when no remote PTY is in use.
(define ssh-example-terminal-size
  (lambda ()
    (let ([size (ffi-ssh-terminal-size 0)])
      (cond [(not size) #f]
            [(and (vector? size) (fixnum? (vector-ref size 0))) size]
            [else (errorf 'ssh-open-repl "cannot inspect local terminal: ~s" size)]))))

(define ssh-example-shell
  (lambda (session log pty? size)
    ;; Open a remote interactive shell; this is the remote user's shell.
    (call-with-ssh-channel
     session
     (lambda (channel)
       (when pty?
         ;; Preserve the local terminal capabilities and dimensions for prompt rendering.
         (let ([terminal (getenv "TERM")])
           (ssh-request-pty! channel
                             (if (and terminal (not (string=? terminal ""))) terminal "xterm")
                             (if size (vector-ref size 0) 80)
                             (if size (vector-ref size 1) 24) 30000)))
       ;; Start the remote user's shell on this channel.
       (ssh-shell channel 30000)
       (let ([raw-handle #f] [stdin-open? #t] [last-size size] [last-size-check 0])
         (dynamic-wind
           (lambda ()
             (when (and pty? size)
               (let ([status (ffi-ssh-terminal-raw-start 0)])
                 (unless (and (vector? status) (eq? (vector-ref status 0) 'ok))
                   (errorf 'ssh-open-repl "cannot enter raw terminal mode: ~s" status))
                 (set! raw-handle (vector-ref status 1)))))
           (lambda ()
             ;; Open each port inside its owner's cleanup scope.
             (call-with-port
              (open-ssh-channel-output-port channel)
              (lambda (output)
                (call-with-port
                 (open-file-input-port "/proc/self/fd/0" (file-options) (buffer-mode none) #f)
                 (lambda (input)
                   (ssh-example-drain
                    channel
                    (lambda ()
                      ;; Check periodically without installing a process-wide signal handler.
                      (let* ([now (current-time 'time-monotonic)]
                             [ms (+ (* (time-second now) 1000)
                                    (quotient (time-nanosecond now) 1000000))])
                        (when (and pty? size (>= (- ms last-size-check) 100))
                          (set! last-size-check ms)
                          (let ([new-size (ssh-example-terminal-size)])
                            (when (and new-size (not (equal? new-size last-size)))
                              ;; Notify the remote shell and editor when the local window changes.
                              (ssh-change-pty-size! channel (vector-ref new-size 0)
                                                    (vector-ref new-size 1) 30000)
                              (set! last-size new-size)))))
                      (when (and stdin-open?
                                 (pair? (poll-target-ready-events
                                         (car (poll (list (make-poll-target input '(read))) 0)))))
                        (let ([chunk (get-bytevector-some input)])
                          (if (eof-object? chunk)
                              (begin
                                (set! stdin-open? #f)
                                (flush-output-port output)
                                ;; Finish shell stdin while continuing to drain both streams.
                                (ssh-send-eof! channel))
                              (begin (put-bytevector output chunk)
                                     (flush-output-port output))))))))))))
           (lambda ()
             ;; Restore before channel/session shutdown or logging, including exceptions.
             (when raw-handle
               (let ([result (ffi-ssh-terminal-raw-close raw-handle)])
                 (set! raw-handle #f)
                 (unless (zero? result)
                   (errorf 'ssh-open-repl "cannot restore local terminal (errno ~a)" result)))))))))))

(ssh-example-run!
 'ssh-open-repl
 "Open a remote shell. PTY defaults to auto; credentials use environment variables."
 (append ssh-example-options
         '((pty . "Remote terminal: auto (default, when stdin is a terminal), yes, or no. Raw input and window resizing apply to terminal sessions.")))
 (lambda (ref)
   (let* ([pty (or (ref 'pty) "auto")] [size (ssh-example-terminal-size)])
     (unless (member pty '("auto" "yes" "no"))
       (errorf 'ssh-open-repl "--pty must be auto, yes, or no"))
     ;; This standalone script owns its signal handlers for the rest of its lifetime.
     ;; Convert catchable termination into Scheme unwinding so raw-mode cleanup runs.
     (for-each
      (lambda (name)
        (register-signal-handler
         (signal-number (string->signal name))
         (lambda (number) (errorf 'ssh-open-repl "terminated by signal ~a" number))))
      '("HUP" "TERM" "QUIT"))
     (exit (call-with-example-ssh
            'ssh-open-repl ref
            (lambda (session log)
              (ssh-example-shell session log
                                 (or (string=? pty "yes")
                                     (and (string=? pty "auto") (and size #t))) size))))))
 (command-line-arguments))
