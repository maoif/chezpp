(library (chezpp os)
  (export sleep milisleep nanosleep

          unix? windows? darwin?
          hostname
          cpu-arch cpu-count

          getuser getgroup user-exists? group-exists?
          unix-user-name unix-user-passwd unix-user-uid unix-user-gid unix-user-gecos unix-user-dir unix-user-shell
          unix-group-name unix-group-passwd unix-group-gid unix-group-mems
          uid->user user->uid gid->group group->gid
          get-user-dir get-user-shell get-user-group
          getuid getgid geteuid getegid

          fork vfork
          getpid gettid getppid
          shared-object-list

          os-error?)
  (import (chezpp chez)
          (chezpp private os)
          (chezpp file)
          (chezpp internal)
          (chezpp utils))


  (define-condition-type &os &error make-os-error os-error?)

  (define $err-os
    (lambda (who msg) (raise (condition (make-os-error) (make-who-condition who) (make-message-condition msg)))))


  #|doc
  Make the current thread sleep for `t` seconds.
  |#
  (define sleep
    (lambda (t)
      (pcheck-natural (t)
                      ($sleep (make-time 'time-duration 0 t)))))

  #|doc
  Make the current thread sleep for `t` miliseconds.
  |#
  (define milisleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000)
                          (let ([sec  (fx/ t 1000)]
                                [nsec (fx* 1000000 (fxmod t 1000))])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration (fx* t 1000000) 0))))))
  #|doc
  Make the current thread sleep for `t` nanoseconds.
  |#
  (define nanosleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000000000)
                          (let ([sec  (fx/ t 1000000000)]
                                [nsec (fxmod t 1000000000)])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration t 0))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   system info
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  Return the hostname of the current operating system.
  |#
  (define hostname
    (foreign-procedure "chezpp_hostname" () ptr))


  #|doc
  Return the name of the instruction set architecture (ISA) of the current processor.
  |#
  (define cpu-arch
    (foreign-procedure "chezpp_cpu_arch" () ptr))


  #|doc
  Return the number of available logical processors.
  |#
  (define cpu-count
    (foreign-procedure "chezpp_cpu_count" () int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   credentials
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define $getpwnam (let ([ffi (foreign-procedure "chezpp_getpwnam" (string) scheme-object)])
                      (lambda (who id) (let ([x (ffi id)])
                                         (if (string? x) ($err-os who x) x)))))
  (define $getpwuid (let ([ffi (foreign-procedure "chezpp_getpwuid" (int) scheme-object)])
                      (lambda (who id) (let ([x (ffi id)])
                                         (if (string? x) ($err-os who x) x)))))
  (define $getgrnam (let ([ffi (foreign-procedure "chezpp_getgrnam" (string) scheme-object)])
                      (lambda (who id) (let ([x (ffi id)])
                                         (if (string? x) ($err-os who x) x)))))
  (define $getgrgid (let ([ffi (foreign-procedure "chezpp_getgrgid" (int) scheme-object)])
                      (lambda (who id) (let ([x (ffi id)])
                                         (if (string? x) ($err-os who x) x)))))

  (define $getpw-name   (lambda (v) (vector-ref v 0)))
  (define $getpw-passwd (lambda (v) (vector-ref v 1)))
  (define $getpw-uid    (lambda (v) (vector-ref v 2)))
  (define $getpw-gid    (lambda (v) (vector-ref v 3)))
  (define $getpw-gecos  (lambda (v) (vector-ref v 4)))
  (define $getpw-dir    (lambda (v) (vector-ref v 5)))
  (define $getpw-shell  (lambda (v) (vector-ref v 6)))

  (define $getgr-name   (lambda (v) (vector-ref v 0)))
  (define $getgr-passwd (lambda (v) (vector-ref v 1)))
  (define $getgr-gid    (lambda (v) (vector-ref v 2)))
  (define $getgr-mems   (lambda (v) (vector-ref v 3)))

  (define-record-type unix-user
    (nongenerative)
    (fields (immutable name)
            (immutable passwd)
            (immutable uid)
            (immutable gid)
            (immutable gecos)
            (immutable dir)
            (immutable shell)))
  (define-record-type unix-group
    (nongenerative)
    (fields (immutable name)
            (immutable passwd)
            (immutable gid)
            (immutable mems)))


  (define mk-unix-user
    (lambda (v)
      (let ([name   ($getpw-name v)]
            [passwd ($getpw-passwd v)]
            [uid    ($getpw-uid v)]
            [gid    ($getpw-gid v)]
            [gecos  ($getpw-gecos v)]
            [dir    ($getpw-dir v)]
            [shell  ($getpw-shell v)])
        (make-unix-user name passwd uid gid gecos dir shell))))

  (define mk-unix-group
    (lambda (v)
      (let ([name   ($getgr-name v)]
            [passwd ($getgr-passwd v)]
            [gid    ($getgr-gid v)]
            [mems   ($getgr-mems v)])
        (make-unix-group name passwd gid mems))))


  (define-who getuser
    (lambda (id)
      (cond [(natural? id)
             (let ([v ($getpwuid who id)])
               (mk-unix-user v))]
            [(string? id)
             (let ([v ($getpwnam who id)])
               (mk-unix-user v))]
            [else (errorf who "invalid user id: ~a" id)])))

  (define-who getgroup
    (lambda (id)
      (cond [(natural? id)
             (let ([v ($getgrgid who id)])
               (mk-unix-group v))]
            [(string? id)
             (let ([v ($getgrnam who id)])
               (mk-unix-group v))]
            [else (errorf who "invalid group id: ~a" id)])))


  (define-who uid->user
    (lambda (id)
      (pcheck-natural (id)
                      (let ([v ($getpwuid who id)])
                        ($getpw-name v)))))
  (define-who user->uid
    (lambda (name)
      (pcheck-string (name)
                     (let ([v ($getpwnam who name)])
                       ($getpw-uid v)))))
  (define-who gid->group
    (lambda (id)
      (pcheck-natural (id)
                      (let ([v ($getgrgid who id)])
                        ($getgr-name v)))))
  (define-who group->gid
    (lambda (name)
      (pcheck-string (name)
                     (let ([v ($getgrnam who name)])
                       ($getgr-gid v)))))


  (define-who get-user-dir
    (lambda (id)
      (cond [(string? id)  ($getpw-dir ($getpwnam who id))]
            [(natural? id) ($getpw-dir ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)])))
  (define-who get-user-shell
    (lambda (id)
      (cond [(string? id)  ($getpw-shell ($getpwnam who id))]
            [(natural? id) ($getpw-shell ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)])))
  (define-who get-user-group
    (lambda (id)
      (cond [(string? id)  ($getpw-gid ($getpwnam who id))]
            [(natural? id) ($getpw-gid ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)])))


  (define-who user-exists?
    (lambda (id)
      (guard (e [(error? e) #f])
        (if (getuser id) #t #f))))
  (define-who group-exists?
    (lambda (id)
      (guard (e [(error? e) #f])
        (if (getgroup id) #t #f))))

  ;; Return the real user ID of the calling process.
  (define getuid (foreign-procedure "chezpp_getuid" () int))
  ;; Return the real group ID of the calling process.
  (define getgid (foreign-procedure "chezpp_getgid" () int))
  ;; Return the effective user ID of the calling process.
  (define geteuid (foreign-procedure "chezpp_geteuid" () int))
  ;; Return the effective group ID of the calling process.
  (define getegid (foreign-procedure "chezpp_getegid" () int))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   processes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-who fork
    (let ([ffi (foreign-procedure "chezpp_fork" () ptr)])
      (lambda ()
        (let ([x (ffi)])
          (if (string? x)
              ($err-os who x)
              x)))))
  (define-who vfork
    (let ([ffi (foreign-procedure "chezpp_vfork" () ptr)])
      (lambda ()
        (let ([x (ffi)])
          (if (string? x)
              ($err-os who x)
              x)))))

  #|doc
  Get the process identifier.
  |#
  (define getpid get-process-id)

  #|doc
  Get the thread identifier.
  |#
  (define gettid get-thread-id)

  #|doc
  Get parent process identifier.
  |#
  (define getppid (foreign-procedure "chezpp_getppid" () int))


  #|doc
  Return the list of all shared objects currently loaded,
  in the order in which they were loaded.
  |#
  (define-who shared-object-list
    (let ([ffi (foreign-procedure "chezpp_shared_object_list" () ptr)])
      (lambda ()
        (let* ([x (ffi)] [rx (reverse x)]
               ;; skip the process image
               [res (if (string=? "" (car rx)) (cdr rx) rx)])
          res))))

  )
