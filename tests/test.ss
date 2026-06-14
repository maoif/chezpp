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
