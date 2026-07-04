(library (chezpp system user)
  (export getuser getgroup user-ref group-ref user-ref/default group-ref/default
          user-exists? group-exists?
          unix-user? unix-user-name unix-user-passwd unix-user-uid unix-user-gid unix-user-gecos unix-user-dir unix-user-shell
          unix-group? unix-group-name unix-group-passwd unix-group-gid unix-group-mems
          uid->user user->uid gid->group group->gid
          uid->user-name user-name->uid gid->group-name group-name->gid
          get-user-dir get-user-shell get-user-group
          getuid getgid geteuid getegid
          current-uid current-gid current-euid current-egid
          current-user current-group)
  (import (chezpp chez)
          (chezpp system common)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; credentials
;;;;===----------------------------------------------------------------------===

  (define $raise-user-not-found
    (lambda (who kind id)
      (raise (make-system-not-found-error who
                                          (format "~a not found: ~a" kind id)
                                          `((kind . ,kind) (id . ,id))))))

  (define $ffi-lookup
    (lambda (who kind id ffi)
      (let ([x (ffi id)])
        (cond [(string? x)
               ;; The current C helper returns "internal error" for passwd/group
               ;; misses where errno is 0.
               (if (string=? x "internal error")
                   ($raise-user-not-found who kind id)
                   (raise-system-error who #f x `((kind . ,kind) (id . ,id))))]
              [(and (vector? x)
                    (fx< 0 (vector-length x))
                    (let ([tag (vector-ref x 0)])
                      (memq tag '(ok errno not-found unsupported))))
               (ffi-result-ref x)]
              [else x]))))

  (define $getpwnam
    (let ([ffi (foreign-procedure "chezpp_getpwnam" (string) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'user id ffi))))

  (define $getpwuid
    (let ([ffi (foreign-procedure "chezpp_getpwuid" (int) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'user id ffi))))

  (define $getgrnam
    (let ([ffi (foreign-procedure "chezpp_getgrnam" (string) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'group id ffi))))

  (define $getgrgid
    (let ([ffi (foreign-procedure "chezpp_getgrgid" (int) scheme-object)])
      (lambda (who id)
        ($ffi-lookup who 'group id ffi))))

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

  #|proc:unix-user?
The `unix-user?` procedure returns `#t` when its argument is a Unix user record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:unix-user-name
The `unix-user-name` procedure returns the login name of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
|#
  #|proc:unix-user-passwd
The `unix-user-passwd` procedure returns the password field of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
|#
  #|proc:unix-user-uid
The `unix-user-uid` procedure returns the numeric user ID of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
|#
  #|proc:unix-user-gid
The `unix-user-gid` procedure returns the primary numeric group ID of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
|#
  #|proc:unix-user-gecos
The `unix-user-gecos` procedure returns the GECOS field of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
|#
  #|proc:unix-user-dir
The `unix-user-dir` procedure returns the home directory of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
|#
  #|proc:unix-user-shell
The `unix-user-shell` procedure returns the login shell of a Unix user record.
The `unix-user` parameter is the user record returned by `user-ref`.
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

  #|proc:unix-group?
The `unix-group?` procedure returns `#t` when its argument is a Unix group record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:unix-group-name
The `unix-group-name` procedure returns the name of a Unix group record.
The `unix-group` parameter is the group record returned by `group-ref`.
|#
  #|proc:unix-group-passwd
The `unix-group-passwd` procedure returns the password field of a Unix group record.
The `unix-group` parameter is the group record returned by `group-ref`.
|#
  #|proc:unix-group-gid
The `unix-group-gid` procedure returns the numeric group ID of a Unix group record.
The `unix-group` parameter is the group record returned by `group-ref`.
|#
  #|proc:unix-group-mems
The `unix-group-mems` procedure returns a vector of member names for a Unix group record, or `#f` when none are listed.
The `unix-group` parameter is the group record returned by `group-ref`.
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

  #|proc:user-ref
The `user-ref` procedure returns a Unix user record for `id`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define-who user-ref
    (lambda (id)
      (cond [(natural? id)
             (mk-unix-user ($getpwuid who id))]
            [(string? id)
             (mk-unix-user ($getpwnam who id))]
            [else (errorf who "invalid user id: ~a" id)])))

  #|proc:getuser
The `getuser` procedure returns a Unix user record for `id`.
The `id` parameter is either a natural numeric user ID or a user name string.
|#
  (define getuser user-ref)

  #|proc:group-ref
The `group-ref` procedure returns a Unix group record for `id`.
The `id` parameter is either a natural numeric group ID or a group name string.
|#
  (define-who group-ref
    (lambda (id)
      (cond [(natural? id)
             (mk-unix-group ($getgrgid who id))]
            [(string? id)
             (mk-unix-group ($getgrnam who id))]
            [else (errorf who "invalid group id: ~a" id)])))

  #|proc:getgroup
The `getgroup` procedure returns a Unix group record for `id`.
The `id` parameter is either a natural numeric group ID or a group name string.
|#
  (define getgroup group-ref)

  #|proc:user-ref/default
The `user-ref/default` procedure returns a Unix user record for `id`, or `default` when `id` is not found.
The `id` parameter is either a natural numeric user ID or a user name string.
The `default` parameter is the value returned when `id` does not identify an existing user.
|#
  (define user-ref/default
    (lambda (id default)
      (guard (c [(system-not-found-error? c) default])
        (user-ref id))))

  #|proc:group-ref/default
The `group-ref/default` procedure returns a Unix group record for `id`, or `default` when `id` is not found.
The `id` parameter is either a natural numeric group ID or a group name string.
The `default` parameter is the value returned when `id` does not identify an existing group.
|#
  (define group-ref/default
    (lambda (id default)
      (guard (c [(system-not-found-error? c) default])
        (group-ref id))))

  #|proc:uid->user
The `uid->user` procedure returns the user name for `id`.
The `id` parameter is a natural numeric user ID.
|#
  (define-who uid->user
    (lambda (id)
      (pcheck-natural (id)
                      ($getpw-name ($getpwuid who id)))))

  #|proc:uid->user-name
The `uid->user-name` procedure returns the user name for `uid`.
The `uid` parameter is a natural numeric user ID.
|#
  (define uid->user-name uid->user)

  #|proc:user->uid
The `user->uid` procedure returns the numeric user ID for `name`.
The `name` parameter is a user name string.
|#
  (define-who user->uid
    (lambda (name)
      (pcheck-string (name)
                     ($getpw-uid ($getpwnam who name)))))

  #|proc:user-name->uid
The `user-name->uid` procedure returns the numeric user ID for `name`.
The `name` parameter is a user name string.
|#
  (define user-name->uid user->uid)

  #|proc:gid->group
The `gid->group` procedure returns the group name for `id`.
The `id` parameter is a natural numeric group ID.
|#
  (define-who gid->group
    (lambda (id)
      (pcheck-natural (id)
                      ($getgr-name ($getgrgid who id)))))

  #|proc:gid->group-name
The `gid->group-name` procedure returns the group name for `gid`.
The `gid` parameter is a natural numeric group ID.
|#
  (define gid->group-name gid->group)

  #|proc:group->gid
The `group->gid` procedure returns the numeric group ID for `name`.
The `name` parameter is a group name string.
|#
  (define-who group->gid
    (lambda (name)
      (pcheck-string (name)
                     ($getgr-gid ($getgrnam who name)))))

  #|proc:group-name->gid
The `group-name->gid` procedure returns the numeric group ID for `name`.
The `name` parameter is a group name string.
|#
  (define group-name->gid group->gid)

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
      (guard (c [(system-not-found-error? c) #f])
        (if (user-ref id) #t #f))))

  #|proc:group-exists?
The `group-exists?` procedure returns `#t` when `id` identifies an existing group, otherwise `#f`.
The `id` parameter is either a natural numeric group ID or a group name string.
|#
  (define-who group-exists?
    (lambda (id)
      (guard (c [(system-not-found-error? c) #f])
        (if (group-ref id) #t #f))))

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

  #|proc:current-uid
The `current-uid` procedure returns the real user ID of the calling process.
|#
  (define current-uid getuid)

  #|proc:current-gid
The `current-gid` procedure returns the real group ID of the calling process.
|#
  (define current-gid getgid)

  #|proc:current-euid
The `current-euid` procedure returns the effective user ID of the calling process.
|#
  (define current-euid geteuid)

  #|proc:current-egid
The `current-egid` procedure returns the effective group ID of the calling process.
|#
  (define current-egid getegid)

  #|proc:current-user
The `current-user` procedure returns the Unix user record for the real user ID of the calling process.
|#
  (define current-user
    (lambda ()
      (user-ref (current-uid))))

  #|proc:current-group
The `current-group` procedure returns the Unix group record for the real group ID of the calling process.
|#
  (define current-group
    (lambda ()
      (group-ref (current-gid))))

  )
