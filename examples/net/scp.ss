(import (chezpp) (chezpp net scp) (chezpp net transfer) (chezpp logging))
(load "examples/common.ss")
(load "examples/net/ssh-example-common.ss")

(define reject-scp-link!
  (lambda (session local remote)
    (when (file-symbolic-link? local)
      (errorf 'scp-example "local symlinks are rejected: ~a" local))
    ;; Directory entries retain the link type; scp-stat follows links.
    (let* ([path (path-parse 'unix remote)]
           [name (path-basename path)]
           [parent (path-render (path-dirname path))])
      (when name
        (call-with-sftp-session
         session
         (lambda (sftp)
           ;; Inspect the remote parent without following its final child entry.
           (for-each
            (lambda (attributes)
              (when (and (string=? name (sftp-attributes-name attributes))
                         (eq? (sftp-attributes-type attributes) 'symlink))
                (errorf 'scp-example "remote symlinks are rejected: ~a" remote)))
            (sftp-list sftp (if (string=? parent "") "." parent)))))))))

(define scp-example
  (lambda (session log direction kind local remote policy)
    ;; Wrap the authenticated SSH session; the outer helper owns its lifetime.
    (let ([scp (scp-open session)])
      (dynamic-wind
        void
        (lambda ()
          (reject-scp-link! session local remote)
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
                                  policy (lambda (relative-path attributes) #t) 'reject))
          (with-logger log (log-infof "SCP ~a completed" direction)))
        ;; Cancel pending transfers and release the SCP wrapper on all exit paths.
        (lambda () (scp-close scp))))))

(example-run!
 'scp-example
 "Copy --kind=file|directory via SCP; --overwrite=error|replace|skip, --resume=never."
 (append ssh-example-options '(direction kind local remote overwrite resume))
 (lambda (ref)
   (let* ([direction (string->symbol (example-required 'scp-example ref 'direction))]
          [kind (string->symbol (example-required 'scp-example ref 'kind))]
          [local (example-required 'scp-example ref 'local)]
          [remote (example-required 'scp-example ref 'remote)]
          [overwrite (string->symbol (or (ref 'overwrite) "error"))]
          [resume (or (ref 'resume) "never")])
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
        (scp-example session log direction kind local remote
                     (make-transfer-policy 'never overwrite 65536 #f))))))
 (command-line-arguments))
