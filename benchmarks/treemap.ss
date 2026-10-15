(import (chezpp))
(load "benchmarks/common.ss")

(define make-map-case
  (lambda (name size specialized? operation)
    (make-benchmark
     name
     (lambda (state n entries)
       (let ([map (if specialized?
                      (make-fxtreemap fx= fx<)
                      (make-treemap = <))])
         (case operation
           [(populate)
            (for-each (lambda (entry)
                        (if specialized?
                            (fxtreemap-set! map (car entry) (cdr entry))
                            (treemap-set! map (car entry) (cdr entry))))
                      entries)
            (benchmark-consume
             (if specialized? (fxtreemap-size map) (treemap-size map)))]
           [(lookup)
            (for-each (lambda (entry)
                        (benchmark-consume
                         (if specialized?
                             (fxtreemap-ref map (car entry))
                             (treemap-ref map (car entry)))))
                      entries)]
           [(traverse)
            (benchmark-consume
             (if specialized?
                 (fxtreemap->list (apply fxtreemap fx= fx< entries))
                 (treemap->list (apply treemap = < entries))))])))
     `((args (n) (,size))
       (setup . ,(lambda (state) (map (lambda (key) (cons key (* key 2)))
                                      (iota size))))
       (warmup . 0)
       (samples . 5)
       (min-time . 0.01)
       (max-iterations . 10000)))))

(define make-map-benchmarks
  (lambda (size)
    (apply append
     (map (lambda (specialized?)
       (map (lambda (operation)
              (make-map-case
               (string->symbol
                (format "~a-~a" (if specialized? "fxtreemap" "treemap") operation))
               size specialized? operation))
            '(populate lookup traverse)))
          '(#f #t)))))

(benchmark-cli
 'treemap-benchmark
 "Compare generic treemap and fixnum fxtreemap workloads."
 (lambda (ref)
   (let ([size (benchmark-size ref)])
     (benchmark-run-cli!
      'treemap-benchmark
      "treemap and fxtreemap"
      (make-map-benchmarks size)
      ref)))
 (command-line-arguments))
