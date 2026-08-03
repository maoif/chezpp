(import (chezpp))

(define example-entry-point*
  '("examples/net/echo-client-grpc.ss"
    "examples/net/echo-server-grpc.ss"
    "examples/net/http-download.ss"
    "examples/net/ssh-open-repl.ss"
    "examples/net/ssh-run-cmd.ss"
    "examples/net/file-transfer/file-transfer-ftp-client.ss"
    "examples/net/file-transfer/file-transfer-ftp-server.ss"
    "examples/net/file-transfer/file-transfer-grpc-client.ss"
    "examples/net/file-transfer/file-transfer-grpc-server.ss"
    "examples/net/file-transfer/file-transfer-http-client.ss"
    "examples/net/file-transfer/file-transfer-http-server.ss"
    "examples/net/file-transfer/file-transfer-sftp-client.ss"
    "examples/net/file-transfer/file-transfer-sftp-server.ss"
    "examples/net/file-transfer/file-transfer-tcp-client.ss"
    "examples/net/file-transfer/file-transfer-tcp-server.ss"
    "examples/net/file-transfer/file-transfer-websocket-client.ss"
    "examples/net/file-transfer/file-transfer-websocket-server.ss"
    "examples/net/file-transfer/transfer-example-client.ss"
    "examples/net/file-transfer/transfer-example-server.ss"))

(define entry-point-load-path*
  (lambda (path)
    (call-with-input-file path
      (lambda (input)
        (let loop ([path* '()])
          (let ([datum (read input)])
            (cond
             [(eof-object? datum) (reverse path*)]
             [(and (pair? datum)
                   (eq? (car datum) 'load)
                   (pair? (cdr datum))
                   (string? (cadr datum)))
              (loop (cons (cadr datum) path*))]
             [else
              (loop path*)])))))))

(define all-load-paths-exist?
  (lambda (path seen)
    (if (member path seen)
        #t
        (and (file-exists? path)
             (andmap
              (lambda (dependency)
                (all-load-paths-exist? dependency (cons path seen)))
              (entry-point-load-path* path))))))

(define file-text
  (lambda (path)
    (call-with-input-file path get-string-all)))

(define contains?
  (lambda (text fragment)
    (and (string-contains? text fragment) #t)))

(mat net-example-load-paths
     (parameterize ([current-directory ".."])
       (andmap
        (lambda (entry-point)
          (all-load-paths-exist? entry-point '()))
        example-entry-point*)))

(mat net-example-script-portability
     (let ([common (file-text "../examples/net/file-transfer/file-transfer-script-common.ss")]
           [sftp (file-text "../examples/net/file-transfer/file-transfer-sftp.ss")])
       (and (not (contains? common "/home/maoif/"))
            (not (contains? common "normalize-script-arguments"))
            (not (contains? sftp "/tmp/chezpp-example-sftp"))
            (not (contains? sftp "make-uuid"))
            (not (contains? sftp "kill $(cat"))
            (not (contains? sftp " & echo $!"))
            (contains? sftp "CHEZPP_SFTP_EXAMPLE_STATE")
            (contains? sftp "sftp-ready-path")
            (contains? sftp "spawn-process")
            (contains? sftp "process-running?")
            (contains? sftp "process-terminate")
            (contains? sftp "process-wait"))))

(parameterize
    ([current-directory ".."])
  (load "examples/net/file-transfer/file-transfer-common.ss")
  (load "examples/net/file-transfer/file-transfer-sftp.ss"))

(define upload-path-error?
  (lambda (name)
    (guard (c [else #t])
      ((top-level-value 'validated-upload-path)
       'net-example-upload
       "/tmp/upload-root"
       name)
      #f)))

(mat net-example-upload-name-validation
     (string=? "/tmp/upload-root/file.txt"
               ((top-level-value 'validated-upload-path)
                'net-example-upload
                "/tmp/upload-root"
                "file.txt"))

     ;; Error case: an empty peer-supplied upload name is invalid.
     (upload-path-error? "")

     ;; Error case: an absolute peer-supplied upload name is invalid.
     (upload-path-error? "/tmp/escape")

     ;; Error case: a slash in a peer-supplied upload name is invalid.
     (upload-path-error? "sub/file")

     ;; Error case: a backslash in a peer-supplied upload name is invalid.
     (upload-path-error? "sub\\file")

     ;; Error case: a NUL byte in a peer-supplied upload name is invalid.
     (upload-path-error? (string #\a (integer->char 0) #\b))

     ;; Error case: the current-directory component is not an upload name.
     (upload-path-error? ".")

     ;; Error case: the parent-directory component is not an upload name.
     (upload-path-error? "..")

     ;; Error case: normalization must not let an upload name escape its root.
     (upload-path-error? "dir/../../escape"))

(define required-sftp-state-root/error?
  (lambda ()
    (guard (c [else #t])
      ((top-level-value 'required-sftp-state-root) 'net-example-sftp)
      #f)))

(mat net-example-sftp-state-coordination
     (with-env
      "CHEZPP_SFTP_EXAMPLE_STATE"
      "/tmp/explicit-chezpp-sftp-state"
      (lambda ()
        (let ([state-root
               ((top-level-value 'required-sftp-state-root) 'net-example-sftp)])
          (and (string=? state-root "/tmp/explicit-chezpp-sftp-state")
               (string=? ((top-level-value 'sftp-ready-path) state-root)
                         "/tmp/explicit-chezpp-sftp-state/ready")))))

     ;; Error case: server and client must not fall back to an undiscoverable state path.
     (with-env
      "CHEZPP_SFTP_EXAMPLE_STATE"
      ""
      required-sftp-state-root/error?))
