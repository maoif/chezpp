(import (chezpp))
(load "benchmarks/common.ss")

(define make-set-case
  (lambda (name size specialized? operation)
    (make-benchmark
     name
     (lambda (state n values)
       (let ([set (if specialized?
                      (make-fxtreeset fx= fx<)
                      (make-treeset = <))])
         (case operation
           [(populate)
            (for-each (lambda (value)
                        (if specialized?
                            (fxtreeset-add! set value)
                            (treeset-add! set value)))
                      values)
            (benchmark-consume
             (if specialized? (fxtreeset-size set) (treeset-size set)))]
           [(lookup)
            (for-each (lambda (value)
                        (benchmark-consume
                         (if specialized?
                             (fxtreeset-contains? set value)
                             (treeset-contains? set value))))
                      values)]
           [(traverse)
            (for-each (lambda (value) (benchmark-consume value))
                      (if specialized?
                          (fxtreeset->list (apply fxtreeset fx= fx< values))
                          (treeset->list (apply treeset = < values))))])))
     `((args (n) (,size))
       (setup . ,(lambda (state) (iota size)))
       (warmup . 0)
       (samples . 5)
       (min-time . 0.01)
       (max-iterations . 10000)))))

(define make-set-benchmarks
  (lambda (size)
    (apply append
     (map (lambda (pair)
       (map (lambda (operation)
              (make-set-case (string->symbol
                              (format "~a-~a" (if (car pair) "fxtreeset" "treeset") operation))
                             size (car pair) operation))
            '(populate lookup traverse)))
          '((#f) (#t))))))

(benchmark-cli
 'treeset-benchmark
 "Compare generic treeset and fixnum fxtreeset workloads."
 (lambda (ref)
   (let ([size (benchmark-size ref)])
     (benchmark-run-cli!
      'treeset-benchmark
      "treeset and fxtreeset"
      (make-set-benchmarks size)
      ref)))
 (command-line-arguments))
