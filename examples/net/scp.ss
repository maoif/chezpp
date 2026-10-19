(import (chezpp) (chezpp net scp) (chezpp net transfer) (chezpp logging))
(load "examples/common.ss")
(load "examples/net/transfer-progress.ss")
(load "examples/net/ssh-example-common.ss")

;; Visit original path components before any normalization can hide a symlink before '..'.
;; The inspector returns false at a missing component; later components cannot exist yet.
(define check-scp-path!
  (lambda (path inspect)
    (let loop ([parts (string-split path #\/)]
               [parent (if (string-startswith? path "/") "/" ".")])
      (unless (null? parts)
        (let* ([name (car parts)]
               [next (string-append parent "/" name)])
          (cond
           [(string=? name "") (loop (cdr parts) parent)]
           [(member name '("." "..")) (loop (cdr parts) next)]
           [(inspect parent name next) (loop (cdr parts) next)]
           [(member ".." (cdr parts))
            (errorf 'scp-example "missing path component before '..': ~a" next)]))))))

(define reject-local-scp-link!
  (lambda (path)
    (check-scp-path!
     path
     (lambda (parent name prefix)
       (when (file-symbolic-link? prefix)
         (errorf 'scp-example "local symlinks are rejected: ~a" prefix))
       (file-exists? prefix)))))

(define reject-remote-scp-link!
  (lambda (sftp path)
    (check-scp-path!
     path
     (lambda (parent name prefix)
       ;; List each parent to inspect link attributes without following the child.
       (let ([entry (find (lambda (attributes)
                            (string=? name (sftp-attributes-name attributes)))
                          (sftp-list sftp parent))])
         (when (and entry (eq? (sftp-attributes-type entry) 'symlink))
           (errorf 'scp-example "remote symlinks are rejected: ~a" prefix))
         (and entry #t))))))

;; Keep SCP exec paths and SFTP inspection paths in the same absolute namespace.
;; Resolve only the login directory; preserve user path components for symlink checks.
(define scp-example-remote-path
  (lambda (session path)
    (if (string-startswith? path "/")
        path
        ;; Query the remote shell's physical working directory for relative paths.
        (call-with-ssh-channel
         session 30000
         (lambda (channel)
           ;; Merge pwd diagnostics into stdout so one reader drains the command output.
           (ssh-exec channel "pwd -P 2>&1" 30000)
           ;; pwd does not consume input; finish stdin before reading its output.
           (ssh-send-eof! channel)
           (let-values ([(output extract) (open-bytevector-output-port)])
             (dynamic-wind
               void
               (lambda ()
                 (let read-output ([received 0])
                   ;; Bound each wait and the amount of output accepted from pwd.
                   (let ([bytes (ssh-read channel 4096 30000)])
                     (unless (eof-object? bytes)
                       (let ([received (fx+ received (bytevector-length bytes))])
                         (when (fx> received 65536)
                           (errorf 'scp-example "remote working directory output is too long"))
                         (put-bytevector output bytes)
                         (read-output received)))))
                 (let* ([text (utf8->string (extract))] [length (string-length text)])
                   ;; Require successful pwd output with its single terminating newline.
                   (unless (and (zero? (ssh-channel-exit-status channel))
                                (fx> length 1) (char=? (string-ref text 0) #\/)
                                (char=? (string-ref text (fx1- length)) #\newline))
                     (errorf 'scp-example "cannot determine remote working directory: ~s" text))
                   (string-append (substring text 0 (fx1- length)) "/" path)))
               (lambda () (close-port output)))))))))

(define scp-example
  (lambda (session log direction kind local remote policy)
    ;; Wrap the authenticated SSH session; the outer helper owns its lifetime.
    (let ([remote (scp-example-remote-path session remote)]
          [scp (scp-open session)])
      (dynamic-wind
        void
        (lambda ()
          ;; Keep one SFTP inspector open for root and recursive destination checks.
          (call-with-sftp-session
           session
           (lambda (sftp)
             (reject-local-scp-link! local)
             (reject-remote-scp-link! sftp remote)
             (with-logger log (log-infof "starting SCP ~a of ~a" direction kind))
             (if (eq? kind 'file)
                 (case direction
                   [(upload)
                    ;; Upload one file with the selected overwrite/resume policy.
                    (scp-upload scp local remote policy)]
                   [(download)
                    ;; Download one file with the selected overwrite/resume policy.
                    (scp-download scp remote local policy)])
                 ;; Copy a tree in the selected direction; reject symlinks in the tree.
                 (scp-copy-directory scp direction
                                     (if (eq? direction 'upload) local remote)
                                     (if (eq? direction 'upload) remote local)
                                     policy
                                     (lambda (relative-path attributes)
                                       ;; Check each destination before the copier creates or replaces it.
                                       (if (eq? direction 'download)
                                           (reject-local-scp-link!
                                            (string-append local "/" relative-path))
                                           (reject-remote-scp-link!
                                            sftp (string-append remote "/" relative-path)))
                                       #t)
                                     'reject))
             (void))))
        ;; Cancel pending transfers and release the SCP wrapper on all exit paths.
        (lambda () (scp-close scp))))))

(ssh-example-run!
 'scp-example
 "Copy --kind=file|directory via SCP; --overwrite=error|replace|skip, --resume=never."
 (append ssh-example-options '((direction . "Required: upload (local to remote) or download (remote to local).")
   (kind . "Required: file or directory; directory copies are recursive.")
   (local . "Required: local source path for upload or destination path for download.")
   (remote . "Required: remote destination for upload or source for download; relative to the remote login directory.")
   (overwrite . "Existing destination policy: error, replace, or skip (default: error).")
   (progress . "Show a progress bar per file on stderr: yes or no (default: yes).")
   (resume . "Resume policy: never is the only supported value (default: never).")))
 (lambda (ref)
   (let* ([direction (string->symbol (example-required 'scp-example ref 'direction))]
          [kind (string->symbol (example-required 'scp-example ref 'kind))]
          [local (example-required 'scp-example ref 'local)]
          [remote (example-required 'scp-example ref 'remote)]
          [overwrite (string->symbol (or (ref 'overwrite) "error"))]
          [resume (or (ref 'resume) "never")]
          [progress? (example-progress-enabled? 'scp-example ref)])
     (unless (memq direction '(upload download))
       (errorf 'scp-example "--direction must be upload or download"))
     (unless (memq kind '(file directory))
       (errorf 'scp-example "--kind must be file or directory"))
     (unless (memq overwrite '(error replace skip))
       (errorf 'scp-example "--overwrite must be error, replace, or skip"))
     ;; Error case: native SCP cannot safely restart an interrupted transfer.
     (unless (string=? resume "never")
       (errorf 'scp-example "SCP resume is unsupported; use --resume=never"))
     (call-with-example-ssh
      'scp-example ref
      (lambda (session log)
        (call-with-example-progress
         progress?
         (lambda (report)
           (scp-example session log direction kind local remote
                        ;; Report native SCP byte counts through the transfer policy hook.
                        (make-transfer-policy
                         'never overwrite 65536
                         (and progress?
                              (lambda (protocol direction path completed total)
                                (report path completed total)))))))
        (with-logger log (log-infof "SCP ~a completed" direction))))))
 (command-line-arguments))
