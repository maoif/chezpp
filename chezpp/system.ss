(library (chezpp system)
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


  #|proc:os-error?
The `os-error?` procedure returns `#t` when its argument is an operating system error condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &os &error make-os-error os-error?)

  (define $err-os
    (lambda (who msg) (raise (condition (make-os-error) (make-who-condition who) (make-message-condition msg)))))


  #|proc:sleep
The `sleep` procedure pauses the current thread for `t` seconds.
The `t` parameter is a natural number of seconds.
  |#
  (define sleep
    (lambda (t)
      (pcheck-natural (t)
                      ($sleep (make-time 'time-duration 0 t)))))

  #|proc:milisleep
The `milisleep` procedure pauses the current thread for `t` milliseconds.
The `t` parameter is a natural number of milliseconds.
  |#
  (define milisleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000)
                          (let ([sec  (fx/ t 1000)]
                                [nsec (fx* 1000000 (fxmod t 1000))])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration (fx* t 1000000) 0))))))
  #|proc:nanosleep
The `nanosleep` procedure pauses the current thread for `t` nanoseconds.
The `t` parameter is a natural number of nanoseconds.
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


  #|proc:hostname
The `hostname` procedure returns the hostname of the current operating system.
  |#
  (define hostname
    (foreign-procedure "chezpp_hostname" () ptr))


  #|proc:cpu-arch
The `cpu-arch` procedure returns the instruction set architecture name of the current processor.
  |#
  (define cpu-arch
    (foreign-procedure "chezpp_cpu_arch" () ptr))


  #|proc:cpu-count
The `cpu-count` procedure returns the number of available logical processors.
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

  #|proc:unix-user-name
The `unix-user-name` procedure returns the login name of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  #|proc:unix-user-passwd
The `unix-user-passwd` procedure returns the password field of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  #|proc:unix-user-uid
The `unix-user-uid` procedure returns the numeric user ID of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  #|proc:unix-user-gid
The `unix-user-gid` procedure returns the primary numeric group ID of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  #|proc:unix-user-gecos
The `unix-user-gecos` procedure returns the GECOS field of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  #|proc:unix-user-dir
The `unix-user-dir` procedure returns the home directory of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  #|proc:unix-user-shell
The `unix-user-shell` procedure returns the login shell of a Unix user record.
The `unix-user` parameter is the user record returned by `getuser`.
|#
  (define-record-type unix-user
    (nongenerative)
    (fields (immutable name)
            (immutable passwd)
            (immutable uid)
            (immutable gid)
            (immutable gecos)
            (immutable dir)
            (immutable shell)))
  #|proc:unix-group-name
The `unix-group-name` procedure returns the name of a Unix group record.
The `unix-group` parameter is the group record returned by `getgroup`.
|#
  #|proc:unix-group-passwd
The `unix-group-passwd` procedure returns the password field of a Unix group record.
The `unix-group` parameter is the group record returned by `getgroup`.
|#
  #|proc:unix-group-gid
The `unix-group-gid` procedure returns the numeric group ID of a Unix group record.
The `unix-group` parameter is the group record returned by `getgroup`.
|#
  #|proc:unix-group-mems
The `unix-group-mems` procedure returns a vector of member names for a Unix group record, or `#f` when none are listed.
The `unix-group` parameter is the group record returned by `getgroup`.
|#
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


  #|proc:getuser
The `getuser` procedure returns a Unix user record for `id`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define-who getuser
    (lambda (id)
      (cond [(natural? id)
             (let ([v ($getpwuid who id)])
               (mk-unix-user v))]
            [(string? id)
             (let ([v ($getpwnam who id)])
               (mk-unix-user v))]
            [else (errorf who "invalid user id: ~a" id)])))

  #|proc:getgroup
The `getgroup` procedure returns a Unix group record for `id`.
The `id` parameter is either a natural numeric group ID or a group name string.
|#
  (define-who getgroup
    (lambda (id)
      (cond [(natural? id)
             (let ([v ($getgrgid who id)])
               (mk-unix-group v))]
            [(string? id)
             (let ([v ($getgrnam who id)])
               (mk-unix-group v))]
            [else (errorf who "invalid group id: ~a" id)])))


  #|proc:uid->user
The `uid->user` procedure returns the user name for `id`.
The `id` parameter is a natural numeric user ID.
|#
  (define-who uid->user
    (lambda (id)
      (pcheck-natural (id)
                      (let ([v ($getpwuid who id)])
                        ($getpw-name v)))))
  #|proc:user->uid
The `user->uid` procedure returns the numeric user ID for `name`.
The `name` parameter is a user name string.
|#
  (define-who user->uid
    (lambda (name)
      (pcheck-string (name)
                     (let ([v ($getpwnam who name)])
                       ($getpw-uid v)))))
  #|proc:gid->group
The `gid->group` procedure returns the group name for `id`.
The `id` parameter is a natural numeric group ID.
|#
  (define-who gid->group
    (lambda (id)
      (pcheck-natural (id)
                      (let ([v ($getgrgid who id)])
                        ($getgr-name v)))))
  #|proc:group->gid
The `group->gid` procedure returns the numeric group ID for `name`.
The `name` parameter is a group name string.
|#
  (define-who group->gid
    (lambda (name)
      (pcheck-string (name)
                     (let ([v ($getgrnam who name)])
                       ($getgr-gid v)))))


  #|proc:get-user-dir
The `get-user-dir` procedure returns the home directory for `id`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define-who get-user-dir
    (lambda (id)
      (cond [(string? id)  ($getpw-dir ($getpwnam who id))]
            [(natural? id) ($getpw-dir ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)])))
  #|proc:get-user-shell
The `get-user-shell` procedure returns the login shell for `id`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define-who get-user-shell
    (lambda (id)
      (cond [(string? id)  ($getpw-shell ($getpwnam who id))]
            [(natural? id) ($getpw-shell ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)])))
  #|proc:get-user-group
The `get-user-group` procedure returns the primary numeric group ID for `id`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define-who get-user-group
    (lambda (id)
      (cond [(string? id)  ($getpw-gid ($getpwnam who id))]
            [(natural? id) ($getpw-gid ($getpwuid who id))]
            [else (errorf who "invalid user id: ~a" id)])))


  #|proc:user-exists?
The `user-exists?` procedure returns `#t` when `id` identifies an existing user, otherwise `#f`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define-who user-exists?
    (lambda (id)
      (guard (e [(error? e) #f])
        (if (getuser id) #t #f))))
  #|proc:group-exists?
The `group-exists?` procedure returns `#t` when `id` identifies an existing group, otherwise `#f`.
The `id` parameter is either a natural numeric group ID or a group name string.
|#
  (define-who group-exists?
    (lambda (id)
      (guard (e [(error? e) #f])
        (if (getgroup id) #t #f))))

  #|proc:getuid
The `getuid` procedure returns the real user ID of the calling process.
|#
  (define getuid (foreign-procedure "chezpp_getuid" () int))
  #|proc:getgid
The `getgid` procedure returns the real group ID of the calling process.
|#
  (define getgid (foreign-procedure "chezpp_getgid" () int))
  #|proc:geteuid
The `geteuid` procedure returns the effective user ID of the calling process.
|#
  (define geteuid (foreign-procedure "chezpp_geteuid" () int))
  #|proc:getegid
The `getegid` procedure returns the effective group ID of the calling process.
|#
  (define getegid (foreign-procedure "chezpp_getegid" () int))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   processes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
               ;; skip the process image
               [res (if (string=? "" (car rx)) (cdr rx) rx)])
          res))))

  )
