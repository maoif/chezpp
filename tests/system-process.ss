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

(mat process-option-surface

     (let ([r (capture-process "cat"
                :stdin (string->utf8 "bytes")
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "bytes" (process-result-stdout r))))

     (let ([r (capture-process "/bin/sh" "-c" "printf \"$CHEZPP_PROCESS_TEST\""
                :env '(("CHEZPP_PROCESS_TEST" . "env-ok"))
                :env-mode replace
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "env-ok" (process-result-stdout r))))

     (let ([r (capture-process "/bin/sh" "-c" "pwd"
                :cwd "/tmp"
                :stdout capture
                :stderr capture)])
       (and (process-exit-success? (process-result-status r))
            (string=? "/tmp\n" (process-result-stdout r))))

     (let ([r (capture-process "/bin/sh" "-c" "printf out; printf err >&2"
                :stdout capture
                :stderr stdout)])
       (and (process-exit-success? (process-result-status r))
            (string=? "outerr" (process-result-stdout r))
            (not (process-result-stderr r))))

     (let ([s (run-process "/bin/sh" "-c" "exit 7"
                :stdin null
                :stdout null
                :stderr null
                :success '(7))])
       (and (eq? 'exit (process-exit-status-kind s))
            (= 7 (process-exit-status-code s))))

     (let ([r (capture-process/check "/bin/sh" "-c" "exit 7"
                :stdout null
                :stderr null
                :success (lambda (status)
                           (= 7 (process-exit-status-code status))))])
       (and (process-result? r)
            (= 7 (process-exit-status-code (process-result-status r))))))

(mat process-shell-helpers

     (let ([s (shell-command "exit 7"
                :stdout null
                :stderr null
                :success '(7))])
       (and (eq? 'exit (process-exit-status-kind s))
            (= 7 (process-exit-status-code s))))

     (let ([r (capture-shell-command "printf shell"
                :stdout capture
                :stderr capture)])
       (and (process-result? r)
            (process-exit-success? (process-result-status r))
            (string=? "shell" (process-result-stdout r)))))

(mat process-spawn-apis

     (let* ([p (spawn-process "sleep" '("1")
                              '((stdin . null) (stdout . null) (stderr . null)))]
            [running-before (process-running? p)])
       (process-terminate p)
       (let ([status (process-wait p)])
         (and (process? p)
              running-before
              (process-exit-status? status)
              (not (process-running? p)))))

     (let* ([p (spawn-process "sleep" '("1")
                              '((stdin . null) (stdout . null) (stderr . null)))]
            [status (process-wait/no-hang p)])
       (process-kill p 15)
       (let ([final-status (process-wait p)])
         (and (not status)
              (process-exit-status? final-status))))

     (let* ([p (spawn-process "true" '()
                              '((stdin . null) (stdout . null) (stderr . null)))]
            [status (process-wait/timeout p 1000)])
       (and (process-exit-success? status)
            (process-exit-success? (process-wait/no-hang p))))

     (let* ([p (spawn-shell-command "exit 0"
                                    '((stdin . null) (stdout . null) (stderr . null)))]
            [status (process-wait p)])
       (and (process? p)
            (process-exit-success? status)
            (not (process-stdin p))
            (not (process-stdout p))
            (not (process-stderr p)))))

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
            (string=? "ABC" (process-result-stdout r))))

     (let ([r (capture-pipeline
               (list (list "printf" "abc")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "cat")
                     (list "cat")
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
            (process-exit-success? (car statuses))))

     (let ([statuses (run-pipeline
                      (list (list "printf" "abc")
                            (list "cat")
                            (list "cat")
                            (list "cat")
                            (list "cat")
                            (list "cat")
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
            (andmap process-exit-success? statuses)))

     (let* ([processes (pipe-processes
                        (list (list "printf" "abc")
                              (list "cat")
                              (list "cat")
                              (list "cat")
                              (list "cat")
                              (list "sh" "-c" "cat >/dev/null"))
                        '())]
            [statuses (map process-wait processes)])
       (and (= 6 (length processes))
            (andmap process? processes)
            (andmap process-exit-success? statuses))))
