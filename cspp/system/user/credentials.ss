(library (cspp system user credentials)
  (export getuid getgid geteuid getegid current-uid current-gid current-euid current-egid)
  (import (cspp chez))

  #|proc:getuid
  The `getuid` procedure returns the real user ID of the calling process.
  |#
  (define getuid (foreign-procedure "cspp_getuid" () int))

  #|proc:getgid
  The `getgid` procedure returns the real group ID of the calling process.
  |#
  (define getgid (foreign-procedure "cspp_getgid" () int))

  #|proc:geteuid
  The `geteuid` procedure returns the effective user ID of the calling process.
  |#
  (define geteuid (foreign-procedure "cspp_geteuid" () int))

  #|proc:getegid
  The `getegid` procedure returns the effective group ID of the calling process.
  |#
  (define getegid (foreign-procedure "cspp_getegid" () int))

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


)
