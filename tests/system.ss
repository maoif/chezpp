(import (chezpp))

(mat user-credentials

     (<= 0 (getuid))
     (<= 0 (getgid))
     (<= 0 (geteuid))
     (<= 0 (getegid)))

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
