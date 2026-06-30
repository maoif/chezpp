(import (chezpp))

(mat process-macro-syntax

     (let ([r (capture-process "printf" "hello"
                :stdout capture
                :stderr capture)])
       (and (process-result? r)
            (process-exit-success? (process-result-status r))
            (string=? "hello" (process-result-stdout r))
            (string=? "" (process-result-stderr r))))

     (let ([r (capture-process printf hello
                :stdout capture
                :stderr capture)])
       (and (process-result? r)
            (process-exit-success? (process-result-status r))
            (string=? "hello" (process-result-stdout r))
            (string=? "" (process-result-stderr r))))

     ;; Error case: duplicate option keyword should be rejected by macro expansion.
     (guard (c [else #t])
       (eval '(capture-process "printf" "x" :stdout capture :stdout capture))
       #f)

     ;; Error case: unknown option keyword should be rejected by macro expansion.
     (guard (c [else #t])
       (eval '(capture-process "printf" "x" :bad-option capture))
       #f))
