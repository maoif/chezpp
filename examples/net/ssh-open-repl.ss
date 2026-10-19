(import (chezpp) (chezpp logging))
(load "examples/common.ss")
(load "examples/net/ssh-example-common.ss")

(define ssh-example-shell
  (lambda (session log pty?)
    ;; Open a remote interactive shell; this is the remote user's shell.
    (call-with-ssh-channel
     session
     (lambda (channel)
       (when pty?
         ;; Request a remote terminal for interactive terminal use.
         (ssh-request-pty! channel))
       ;; Start the remote user's shell on this channel.
       (ssh-shell channel)
       ;; Wrap channel stdin in a binary output port, closed in the cleanup path.
       (let ([output (open-ssh-channel-output-port channel)]
             [input (open-file-input-port "/proc/self/fd/0" (file-options)
                                          (buffer-mode none) #f)]
             [stdin-open? #t])
         (dynamic-wind
           void
           (lambda ()
             (ssh-example-drain
              channel
              (lambda ()
                (when (and stdin-open?
                           (pair? (poll-target-ready-events
                                   (car (poll (list (make-poll-target input '(read))) 0)))))
                  (let ([chunk (get-bytevector-some input)])
                    (if (eof-object? chunk)
                        (begin
                          (set! stdin-open? #f)
                          (flush-output-port output)
                          ;; Half-close shell stdin while continuing to drain its output.
                          (ssh-send-eof! channel))
                        (begin
                          (put-bytevector output chunk)
                          (flush-output-port output))))))))
           (lambda () (close-port input) (close-port output))))))))

(ssh-example-run!
 'ssh-open-repl
 "Open a remote shell. --pty=yes|no (default yes); credentials use environment variables."
 (append ssh-example-options '((pty . "Request a remote terminal: yes or no (default: yes); use no for piped input.")))
 (lambda (ref)
   (let ([pty (or (ref 'pty) "yes")])
     (unless (member pty '("yes" "no")) (errorf 'ssh-open-repl "--pty must be yes or no"))
     (exit (call-with-example-ssh
            'ssh-open-repl ref
            (lambda (session log) (ssh-example-shell session log (string=? pty "yes")))))))
 (command-line-arguments))
