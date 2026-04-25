(import (chezpp)
        (chezpp net))

(load "net-common.ss")
(load "net-ssh-common.ss")

(define scp-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define run-net-scp-basic-test
  (lambda (remote-root home port user)
    (let ([local-root (format "/tmp/chezpp-net-scp-basic-~a" port)])
      (when (file-exists? local-root)
        (file-removetree local-root #f))
      (mkdirs local-root)
      (dynamic-wind
        void
        (lambda ()
          (with-env
           "HOME"
           home
           (lambda ()
             (let ([session (ssh-open "127.0.0.1" port user)])
               (dynamic-wind
                 void
                 (lambda ()
                   (and
                    (eq? (ssh-auth-publickey! session user) session)
                    (let ([scp (scp-open session)])
                      (dynamic-wind
                        void
                        (lambda ()
                          (let ([upload-path (path-build local-root "upload.txt")]
                                [download-path (path-build local-root "download.txt")]
                                [tree-path (path-build local-root "tree")]
                                [tree-out-path (path-build local-root "tree-out")])
                            (write-u8vec! upload-path (string->utf8 "scp-upload"))
                            (mkdirs (path-build tree-path "nested"))
                            (write-u8vec! (path-build tree-path "root.txt")
                                          (string->utf8 "root-data"))
                            (write-u8vec! (path-build (path-build tree-path "nested") "leaf.txt")
                                          (string->utf8 "leaf-data"))
                            (and
                             (equal? (scp-upload scp upload-path
                                                 (string-append remote-root "/uploaded.txt"))
                                     (string-append remote-root "/uploaded.txt"))
                             (equal? (scp-download scp
                                                   (string-append remote-root "/uploaded.txt")
                                                   download-path)
                                     download-path)
                             (equal? (read-u8vec download-path)
                                     (string->utf8 "scp-upload"))
                             (equal? (scp-copy-directory scp
                                                         'upload
                                                         tree-path
                                                         (string-append remote-root "/tree-remote"))
                                     (string-append remote-root "/tree-remote"))
                             (equal? (scp-copy-directory scp
                                                         'download
                                                         (string-append remote-root "/tree-remote")
                                                         tree-out-path)
                                     tree-out-path)
                             (equal? (read-u8vec (path-build tree-out-path "root.txt"))
                                     (string->utf8 "root-data"))
                             (equal? (read-u8vec (path-build (path-build tree-out-path "nested") "leaf.txt"))
                                     (string->utf8 "leaf-data")))))
                        (lambda ()
                          (scp-close scp))))))
                 (lambda ()
                   (ssh-close session)))))))
        (lambda ()
          (when (file-exists? local-root)
            (file-removetree local-root #f)))))))

(define run-net-scp-nonblocking-test
  (lambda (remote-root home port user)
    (let ([local-root (format "/tmp/chezpp-net-scp-nonblocking-~a" port)])
      (when (file-exists? local-root)
        (file-removetree local-root #f))
      (mkdirs local-root)
      (dynamic-wind
        void
        (lambda ()
          (with-env
           "HOME"
           home
           (lambda ()
             (let ([session (ssh-open "127.0.0.1" port user)])
               (dynamic-wind
                 void
                 (lambda ()
                   (and
                    (eq? (ssh-auth-publickey! session user) session)
                    (let ([scp (scp-open session)])
                      (dynamic-wind
                        void
                        (lambda ()
                          (let ([upload-path (path-build local-root "upload-nb.txt")]
                                [download-path (path-build local-root "download-nb.txt")]
                                [tree-path (path-build local-root "tree-nb")]
                                [tree-out-path (path-build local-root "tree-nb-out")])
                            (write-u8vec! upload-path (string->utf8 "scp-upload-nb"))
                            (mkdirs (path-build tree-path "nested"))
                            (write-u8vec! (path-build tree-path "a.txt")
                                          (string->utf8 "A"))
                            (write-u8vec! (path-build (path-build tree-path "nested") "b.txt")
                                          (string->utf8 "B"))
                            (and
                             (equal? (wait-for-result
                                      (lambda ()
                                        (scp-upload/nonblocking
                                         scp
                                         upload-path
                                         (string-append remote-root "/uploaded-nb.txt"))))
                                     (string-append remote-root "/uploaded-nb.txt"))
                             (equal? (wait-for-result
                                      (lambda ()
                                        (scp-download/nonblocking
                                         scp
                                         (string-append remote-root "/uploaded-nb.txt")
                                         download-path)))
                                     download-path)
                             (equal? (read-u8vec download-path)
                                     (string->utf8 "scp-upload-nb"))
                             (equal? (wait-for-result
                                      (lambda ()
                                        (scp-copy-directory/nonblocking
                                         scp
                                         'upload
                                         tree-path
                                         (string-append remote-root "/tree-nb-remote"))))
                                     (string-append remote-root "/tree-nb-remote"))
                             (equal? (wait-for-result
                                      (lambda ()
                                        (scp-copy-directory/nonblocking
                                         scp
                                         'download
                                         (string-append remote-root "/tree-nb-remote")
                                         tree-out-path)))
                                     tree-out-path)
                             (equal? (read-u8vec (path-build tree-out-path "a.txt"))
                                     (string->utf8 "A"))
                             (equal? (read-u8vec (path-build (path-build tree-out-path "nested") "b.txt"))
                                     (string->utf8 "B")))))
                        (lambda ()
                          (scp-close scp))))))
                 (lambda ()
                   (ssh-close session)))))))
        (lambda ()
          (when (file-exists? local-root)
            (file-removetree local-root #f)))))))

(define run-net-scp-convenience-open-test
  (lambda (remote-root home port user)
    (let ([local-root (format "/tmp/chezpp-net-scp-open-~a" port)])
      (when (file-exists? local-root)
        (file-removetree local-root #f))
      (mkdirs local-root)
      (dynamic-wind
        void
        (lambda ()
          (with-env
           "HOME"
           home
           (lambda ()
             (let ([scp (scp-open "127.0.0.1" port user 'publickey #f 30000)])
               (dynamic-wind
                 void
                 (lambda ()
                   (let ([upload-path (path-build local-root "upload-open.txt")]
                         [download-path (path-build local-root "download-open.txt")])
                     (write-u8vec! upload-path (string->utf8 "scp-open"))
                     (and
                      (equal? (scp-upload scp upload-path
                                          (string-append remote-root "/uploaded-open.txt"))
                              (string-append remote-root "/uploaded-open.txt"))
                      (equal? (scp-download scp
                                            (string-append remote-root "/uploaded-open.txt")
                                            download-path)
                              download-path)
                      (equal? (read-u8vec download-path)
                              (string->utf8 "scp-open")))))
                 (lambda ()
                   (scp-close scp)))))))
        (lambda ()
          (when (file-exists? local-root)
            (file-removetree local-root #f)))))))

(define run-net-scp-closed-session-test
  (lambda (home port user)
    (with-env
     "HOME"
     home
     (lambda ()
       (let ([session (ssh-open "127.0.0.1" port user)])
         (dynamic-wind
           void
           (lambda ()
             (and
              (eq? (ssh-auth-publickey! session user) session)
              (let ([scp (scp-open session)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and
                     (begin
                       (ssh-close session)
                       #t)
                     (scp-error-message-contains?
                      "SSH session is closed"
                      (lambda ()
                        (scp-download scp "/tmp/nope" "/tmp/nope")))))
                  (lambda ()
                    (scp-close scp))))))
           (lambda ()
             (ssh-close session))))))))

(mat net-scp
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-scp-basic-test remote-root home port user))
         (lambda ()
           (stop-server)))))

(mat net-scp-nonblocking
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-scp-nonblocking-test remote-root home port user))
         (lambda ()
           (stop-server)))))

(mat net-scp-convenience-open
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-scp-convenience-open-test remote-root home port user))
         (lambda ()
           (stop-server)))))

(mat net-scp-closed-session
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (dynamic-wind
         void
         (lambda ()
           (run-net-scp-closed-session-test home port user))
         (lambda ()
           (stop-server)))))
