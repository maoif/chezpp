(library (chezpp system)
  (export fork vfork
          getpid gettid getppid
          shared-object-list

          os-error?)
  (import (chezpp chez)
          (chezpp system common)
          (except (chezpp system darwin) darwin?)
          (chezpp system filesystem)
          (chezpp system info)
          (except (chezpp system linux) linux?)
          (chezpp system process)
          (chezpp system signal)
          (chezpp system user)
          (except (chezpp system windows) windows?)
          (chezpp utils))

  (export (import (chezpp system common)
                  (except (chezpp system darwin) darwin?)
                  (chezpp system filesystem)
                  (chezpp system info)
                  (except (chezpp system linux) linux?)
                  (chezpp system process)
                  (chezpp system signal)
                  (chezpp system user)
                  (except (chezpp system windows) windows?)))

;;;;===----------------------------------------------------------------------===
;;;; OS errors
;;;;===----------------------------------------------------------------------===

  #|proc:os-error?
The `os-error?` procedure returns `#t` when its argument is an operating system error condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &os &error make-os-error os-error?)

  (define $err-os
    (lambda (who msg)
      (raise (condition (make-os-error)
                        (make-who-condition who)
                        (make-message-condition msg)))))

;;;;===----------------------------------------------------------------------===
;;;; processes
;;;;===----------------------------------------------------------------------===

  #|proc:fork
The `fork` procedure creates a child process and returns the child process ID in the parent and `0` in the child.
|#
  (define-who fork
    (let ([ffi (foreign-procedure "chezpp_fork" () ptr)])
      (lambda ()
        (let ([x (ffi)])
          (if (string? x)
              ($err-os who x)
              x)))))

  #|proc:vfork
The `vfork` procedure creates a child process using the operating system `vfork` operation and returns as `fork` does.
|#
  (define-who vfork
    (let ([ffi (foreign-procedure "chezpp_vfork" () ptr)])
      (lambda ()
        (let ([x (ffi)])
          (if (string? x)
              ($err-os who x)
              x)))))

  #|proc:getpid
The `getpid` procedure returns the process ID of the calling process.
|#
  (define getpid get-process-id)

  #|proc:gettid
The `gettid` procedure returns the thread ID of the calling thread.
|#
  (define gettid get-thread-id)

  #|proc:getppid
The `getppid` procedure returns the parent process ID of the calling process.
|#
  (define getppid (foreign-procedure "chezpp_getppid" () int))

  #|proc:shared-object-list
The `shared-object-list` procedure returns a list of shared objects currently loaded by the process, in load order.
|#
  (define-who shared-object-list
    (let ([ffi (foreign-procedure "chezpp_shared_object_list" () ptr)])
      (lambda ()
        (let* ([x (ffi)] [rx (reverse x)]
               [res (if (string=? "" (car rx)) (cdr rx) rx)])
          res))))

  )
