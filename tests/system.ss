(import (chezpp))

(mat system-errors

     (system-error? (guard (c [else c])
                      (raise-system-unsupported 'test-op "unsupported")))

     (system-unsupported-error? (guard (c [else c])
                                  (raise-system-unsupported 'test-op "unsupported")))

     (let ([c (guard (c [else c])
                (raise-system-error 'test-op 1 "operation not permitted" '((path . "/"))))])
       (and (system-error? c)
            (eq? 'test-op (system-error-operation c))
            (= 1 (system-error-code c))
            (string? (system-error-message c))
            (pair? (system-error-context c))))

     (eq? 'value (ffi-result-ref '#(ok value)))

     (system-not-found-error? (guard (c [else c])
                                (ffi-result-ref '#(not-found test-op ((path . "/missing"))))))

     (let ([c (guard (c [else c])
                (ffi-result-ref '#(errno test-op 13 "permission denied" ((path . "/")))))])
       (and (system-permission-error? c)
            (= 13 (system-error-code c)))))

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

(mat system-platform-modules

     (and (memq (system-platform) '(linux darwin windows unknown)) #t)
     (boolean? (linux?))
     (boolean? (darwin?))
     (boolean? (windows?)))

;; Error case: unimplemented platform stubs should raise unsupported.
(mat system-platform-stubs

     (if (linux?)
         (guard (c [(system-unsupported-error? c) #t] [else #f])
           (windows-system-version)
           #f)
         #t))
