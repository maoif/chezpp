(import (chezpp))

(mat system-user-current

     (unix-user? (user-ref (getuid)))
     (unix-user? (user-ref (uid->user-name (getuid))))
     (unix-group? (group-ref (getgid)))
     (unix-group? (group-ref (gid->group-name (getgid)))))

;; Error case: looking up an impossible user name should report not-found.
(mat system-user-missing

     (guard (c [(system-not-found-error? c) #t] [else #f])
       (user-ref "__chezpp_user_that_should_not_exist__")
       #f)

     (guard (c [(system-not-found-error? c) #t] [else #f])
       (group-ref "__chezpp_group_that_should_not_exist__")
       #f))
