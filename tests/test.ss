(import (chezpp)
        (chezpp test))

(mat test-descriptors
  (test-case-descriptor?
   (make-test-case 'sample (lambda () #t) '()))
  (test-suite-descriptor?
   (make-test-suite 'suite (list (make-test-case 'child (lambda () #t) '())) '()))
  (test-expand-descriptor?
   (make-test-expand 'expansion '(+ 1 2) '()))
  (test-compile-descriptor?
   (make-test-compile 'compile '((define x 1) x) '()))
  (let ([descriptor (make-test-case 'sample (lambda () #t) '((tags . (fast))))])
    (and (eq? (test-descriptor-name descriptor) 'sample)
         (eq? (test-descriptor-phase descriptor) 'runtime)
         (equal? (test-descriptor-metadata descriptor) '((tags . (fast))))))
  (let ([suite (make-test-suite 'suite (list (make-test-case 'child (lambda () #t) '())) '())])
    (= (length (test-descriptor-children suite)) 1))
  (test-config? (default-test-config))
  (let ([config (test-config-with (default-test-config)
                  '((reporter . #f)
                    (output . #f)
                    (xfail-strict? . #t)
                    (stop-on-failure? . #t)
                    (color . never)))])
    (and (not (test-config-reporter config))
         (not (test-config-output config))
         (test-config-xfail-strict? config)
         (test-config-stop-on-failure? config)
         (eq? (test-config-color config) 'never)))
  ;; Public constructors reject invalid names.
  (error? (make-test-case "bad" (lambda () #t) '()))
  ;; Public constructors reject non-procedure runtime bodies.
  (error? (make-test-case 'bad-body 'not-a-procedure '())))

(mat test-assertions
  (begin (test-true #t) #t)
  (begin (test-false #f) #t)
  (begin (test-eq 'a 'a) #t)
  (begin (test-eqv 1 1) #t)
  (begin (test-equal '(1 2) '(1 2)) #t)
  (begin (test-= 3 (+ 1 2)) #t)
  (begin (test-pred string? "abc") #t)
  ;; Negative assertion test: failing truth assertion raises a framework failure.
  (test-failure? (guard (c [else c]) (test-true #f)))
  ;; Negative assertion test: framework failures also carry a standard message condition.
  (let ([condition (guard (c [else c]) (test-true #f))])
    (and (test-failure? condition)
         (message-condition? condition)
         (equal? (condition-message condition) "expected true value")))
  ;; Negative assertion test: mismatched expected and actual values are recorded.
  (let ([condition (guard (c [else c]) (test-equal '(1) '(2)))])
    (and (test-failure? condition)
         (equal? (test-failure-expected condition) '(1))
         (equal? (test-failure-actual condition) '(2))))
  ;; Negative code test: expected violation is accepted.
  (begin
    (test-raises violation?
      (lambda ()
        (vector-ref '#(1 2 3) 9)))
    #t)
  ;; Negative assertion test: missing expected condition fails the assertion.
  (test-failure?
   (guard (c [else c])
     (test-raises violation?
       (lambda () 1))))
  (begin (test-not-raises (lambda () (+ 1 2))) #t))

(mat test-registry-and-expansion
  (let ([registry (make-test-registry)]
        [descriptor (make-test-case 'registered (lambda () #t) '())])
    (test-register! registry descriptor)
    (equal? (test-registry-descriptors registry) (list descriptor)))
  (let ([registry (make-test-registry)])
    (test-register! registry (make-test-case 'a (lambda () #t) '()))
    (test-clear-registry! registry)
    (null? (test-registry-descriptors registry)))
  (let* ([descriptor (make-test-case 'param
                       (lambda () #t)
                       '((parameterize . (((x y) (1 2) (3 4))))))]
         [cases (test-expand-parameters descriptor)])
    (and (= (length cases) 2)
         (equal? (map test-concrete-case-parameters cases)
                 '(((x . 1) (y . 2))
                   ((x . 3) (y . 4))))))
  (let* ([descriptor (make-test-case 'product
                       (lambda () #t)
                       '((parameterize . (((x) (1) (2))
                                          ((y) (a) (b))))))]
         [cases (test-expand-parameters descriptor)])
    (and (= (length cases) 4)
         (equal? (map test-concrete-case-parameters cases)
                 '(((x . 1) (y . a))
                   ((x . 1) (y . b))
                   ((x . 2) (y . a))
                   ((x . 2) (y . b))))))
  (let ([registry (make-test-registry)])
    (test-register! registry (make-test-case 'alpha/one (lambda () #t) '((tags . (fast)))))
    (test-register! registry (make-test-case 'beta/two (lambda () #t) '((tags . (slow)))))
    (equal? (map test-concrete-case-name (test-select registry '((name-prefix . "alpha"))))
            '(alpha/one)))
  (let ([registry (make-test-registry)])
    (test-register! registry (make-test-case 'alpha/one (lambda () #t) '((tags . (fast)))))
    (test-register! registry (make-test-case 'beta/two (lambda () #t) '((tags . (slow)))))
    (equal? (map test-concrete-case-name (test-select registry '((include-tags . (slow)))))
            '(beta/two))))

(define test-silent-config
  (lambda ()
    (test-config-with (default-test-config)
      '((reporter . #f)
        (output . #f)
        (color . never)))))

(mat test-runtime-runner
  (let* ([descriptor (make-test-case 'pass (lambda () (test-true #t)) '())]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'passed)))
  (let* ([descriptor (make-test-case 'fail (lambda () (test-equal 1 2)) '())]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'failed)
         (test-failure? (test-result-condition (car results)))))
  (let* ([descriptor (make-test-case 'error (lambda () (error 'boom "bad")) '())]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'errored)))
  (let* ([descriptor (make-test-case 'skip (lambda () (test-fail "must not run"))
                                     '((skip-when . ((lambda (params) #t) . "disabled"))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'skipped)))
  (let* ([descriptor (make-test-case 'xfail (lambda () (test-equal 1 2))
                                     '((xfail-when . ((lambda (params) #t) . "known bug"))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'xfail)))
  (let* ([descriptor (make-test-case 'xpass (lambda () (test-true #t))
                                     '((xfail-when . ((lambda (params) #t) . "known bug"))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 1)
         (eq? (test-result-status (car results)) 'xpass)))
  (let* ([descriptor (make-test-case 'param-pass
                       (lambda (params)
                         (test-= (cdr (assq 'expected params))
                                 (+ (cdr (assq 'x params)) 1)))
                       '((parameterize . (((x expected) (1 2) (2 3))))))]
         [results (test-run (list descriptor) (test-silent-config))])
    (and (= (length results) 2)
         (andmap (lambda (result) (eq? (test-result-status result) 'passed)) results))))

(mat test-output-capture
  (call-with-values
    (lambda ()
      (test-capture-ports
       '(stdout stderr)
       (lambda ()
         (display "hello")
         (display "bad" (current-error-port)))))
    (lambda (values stdout stderr)
      (and (equal? stdout "hello")
           (equal? stderr "bad"))))
  (let* ([descriptor (make-test-case 'stdout
                       (lambda () (display "hello\n"))
                       '((capture . stdout) (stdout . "hello\n")))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (and (eq? (test-result-status result) 'passed)
         (equal? (test-result-stdout result) "hello\n")))
  (let* ([descriptor (make-test-case 'stdout-fail
                       (lambda () (display "actual\n"))
                       '((capture . stdout) (stdout . "expected\n")))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (and (eq? (test-result-status result) 'failed)
         (test-failure? (test-result-condition result)))))

(mat test-expand-and-compile-runner
  (let* ([descriptor (make-test-expand 'expand-ok '(+ 1 2) '())]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))
  (let* ([descriptor (make-test-expand 'expand-bad
                       '(let ([x 1] [x 2]) x)
                       '((raises . syntax-violation?)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))
  (let* ([descriptor (make-test-compile 'compile-ok '((define x 1) (+ x 2)) '())]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed))
  (let* ([descriptor (make-test-compile 'compile-bad
                       '((let ([x 1] [x 2]) x))
                       '((raises . syntax-violation?)))]
         [result (car (test-run (list descriptor) (test-silent-config)))])
    (eq? (test-result-status result) 'passed)))

(test-clear-registry! (current-test-registry))

(test-case macro/runtime-pass
  :tags (macro fast)
  :parameterize ([x expected]
                 [1 2]
                 [2 3])
  (test-= expected (+ x 1)))

(test-suite macro/suite
  :tags (suite-tag)
  (make-test-case 'suite-child (lambda () (test-true #t)) '()))

(test-expand macro/expand-ok
  '(+ 1 2))

(test-compile macro/compile-ok
  (define macro-compile-x 1)
  (+ macro-compile-x 2))

(mat test-definition-macros
  (= (length (test-registry-descriptors (current-test-registry))) 4)
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (and (= (length results) 5)
         (andmap (lambda (result) (eq? (test-result-status result) 'passed)) results)))
  (let ([selected (test-select (current-test-registry) '((include-tags . (suite-tag))))])
    (and (= (length selected) 1)
         (eq? (test-concrete-case-name (car selected)) 'suite-child))))

(mat test-reporters
  (let* ([results (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f)
                        (make-test-result 'b 'b 'failed "bad" #f "" "" '() #f))]
         [summary (test-summarize results)])
    (and (= (test-summary-total summary) 2)
         (= (test-summary-passed summary) 1)
         (= (test-summary-failed summary) 1)))
  (let ([out (open-output-string)])
    (test-report (test-text-reporter) out
                 (test-summarize
                  (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f))))
    (let ([text (get-output-string out)])
      (and (string? text)
           (> (string-length text) 0))))
  (let ([out (open-output-string)])
    (test-report (test-datum-reporter) out
                 (test-summarize
                  (list (make-test-result 'a 'a 'passed "" #f "" "" '() #f))))
    (let ([datum (read (open-input-string (get-output-string out)))])
      (and (pair? datum)
           (eq? (car datum) 'test-summary)))))

(test-clear-registry! (current-test-registry))

(test-case requirement/chez-version-pass
  :requires-chez-version (0 0 0)
  (test-true #t))

(test-case requirement/chez-version-skip
  :requires-chez-version (999 0 0)
  (test-fail "must be skipped"))

(test-case requirement/file-pass
  :requires-file "test.ss"
  (test-true #t))

(test-case requirement/file-skip
  :requires-file "missing-chezpp-test-file"
  (test-fail "must be skipped"))

(mat test-requirement-configs
  (let ([results (test-run-registry (current-test-registry) (test-silent-config))])
    (equal? (map test-result-status results)
            '(passed skipped passed skipped))))
