(library (chezpp system user records)
  (export make-unix-user make-unix-group unix-user? unix-user-name unix-user-passwd unix-user-uid unix-user-gid unix-user-gecos unix-user-dir unix-user-shell unix-group? unix-group-name unix-group-passwd unix-group-gid unix-group-mems)
  (import (chezpp chez))

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
  The `unix-group?` procedure returns `#t` when its argument is a Unix group record, otherwise
  `#f`.
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
  The `unix-group-mems` procedure returns a vector of member names for a Unix group record, or
  `#f` when none are listed.
  The `unix-group` parameter is the group record returned by `group-ref`.
  |#
  (define-record-type unix-group
    (nongenerative)
    (fields (immutable name)
            (immutable passwd)
            (immutable gid)
            (immutable mems)))


)
