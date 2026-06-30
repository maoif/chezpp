(import (chezpp))

(mat user-credentials

     (<= 0 (getuid))
     (<= 0 (getgid))
     (<= 0 (geteuid))
     (<= 0 (getegid)))

(mat current-ids

     (let ([uid (getuid)]
           [gid (getgid)]
           [euid (geteuid)]
           [egid (getegid)])
       (and (integer? uid)
            (integer? gid)
            (integer? euid)
            (integer? egid)
            (<= 0 uid)
            (<= 0 gid)
            (<= 0 euid)
            (<= 0 egid))))

(mat user-group-roundtrip

     (let* ([uid (getuid)]
            [name (uid->user uid)])
       (= uid (user->uid name)))

     (let* ([gid (getgid)]
            [name (gid->group gid)])
       (= gid (group->gid name))))

(mat process-ids

     (< 1 (getpid))
     (< 1 (getppid))
     (<= 0 (gettid)))

(mat system-info

     (string? (hostname))
     (string? (cpu-arch))
     (<= 1 (cpu-count))
     (boolean? (unix?))
     (boolean? (windows?))
     (boolean? (darwin?)))
