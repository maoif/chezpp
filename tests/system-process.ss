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

(mat process-capture-behavior

     (let ([r (capture-process "sh" "-c" "printf out; printf err >&2"
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "out" (process-result-stdout r))
            (string=? "err" (process-result-stderr r))))

     (let ([r (capture-process "cat"
                :stdin "abc"
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "abc" (process-result-stdout r)))))

;; Error case: a missing executable should return an errno tagged result.
(mat process-ffi-result-tags

     (let ([raw ((foreign-procedure "chezpp_spawn_capture"
                                    (ptr ptr string ptr int int int int int int)
                                    ptr)
                 (list "sh" "-c" "true") #f "" #f 0 0 0 0 0 -1)])
       (and (vector? raw)
            (eq? 'ok (vector-ref raw 0))))

     (let ([raw ((foreign-procedure "chezpp_spawn_capture"
                                    (ptr ptr string ptr int int int int int int)
                                    ptr)
                 (list "definitely-not-a-chezpp-test-command") #f "" #f 0 0 0 0 0 -1)])
       (and (vector? raw)
            (eq? 'errno (vector-ref raw 0)))))

;; Error case: check variant should raise for nonzero exit status.
(mat process-check-errors

     (guard (c [(system-exit-error? c) #t] [else #f])
       (capture-process/check "sh" "-c" "exit 7" :stdout capture :stderr capture)
       #f))

;; Error case: process timeout should raise and clean up the child.
(mat process-timeout

     (guard (c [(system-timeout-error? c) #t] [else #f])
       (capture-process/check "sh" "-c" "sleep 2" :timeout 50 :stdout capture :stderr capture)
       #f))

(mat process-pipeline

     (let ([r (capture-pipeline
               (list (list "printf" "abc")
                     (list "tr" "a-z" "A-Z")))])
       (and (process-result? r)
            (string=? "ABC" (process-result-stdout r)))))

(mat process-expert-pipes

     (call-with-values make-pipe
       (lambda (in out)
         (put-bytevector out (string->utf8 "pipe"))
         (close-output-port out)
         (let ([data (get-bytevector-all in)])
           (close-input-port in)
           (string=? "pipe" (utf8->string data)))))

     (let ([statuses (run-pipeline
                      (list (list "printf" "abc")
                            (list "tr" "a-z" "A-Z")))])
       (and (= 1 (length statuses))
            (process-exit-success? (car statuses)))))

(mat process-pipe-processes

     (let* ([processes (pipe-processes
                        (list (list "printf" "abc")
                              (list "sh" "-c" "cat >/dev/null"))
                        '())]
            [statuses (map process-wait processes)])
       (and (= 2 (length processes))
            (andmap process? processes)
            (andmap process-exit-success? statuses))))
