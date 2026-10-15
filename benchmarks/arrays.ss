(import (chezpp))
(load "benchmarks/common.ss")

(define make-array-case
  (lambda (type operation size)
    (make-benchmark
     (string->symbol (format "~a-~a" type operation))
     (lambda (state n input)
       (let ([values (case type
                       [(array) (make-array)]
                       [(fxarray) (make-fxarray 0)]
                       [(flarray) (make-flarray 0)])])
         (case operation
           [(append)
            (do ([i 0 (+ i 1)]) ((= i n))
              (case type
                [(array) (array-add! values i)]
                [(fxarray) (fxarray-add! values i)]
                [(flarray) (flarray-add! values (exact->inexact i))]))]
           [(append-and-fold)
            (do ([i 0 (+ i 1)]) ((= i n))
              (case type
                [(array) (array-add! values i)]
                [(fxarray) (fxarray-add! values i)]
                [(flarray) (flarray-add! values (exact->inexact i))]))
            (benchmark-consume
             (case type
               [(array) (array-fold-left + 0 values)]
               [(fxarray) (fxarray-fold-left fx+ 0 values)]
               [(flarray) (flarray-fold-left fl+ 0.0 values)]))]))
       (benchmark-consume input))
     `((args (n) (,size))
       (setup . ,(lambda (state) 'input))
       (warmup . 0) (samples . 5) (min-time . 0.01) (max-iterations . 10000)))))

(define make-array-benchmarks
  (lambda (size)
    (apply append
     (map (lambda (type)
       (map (lambda (operation) (make-array-case type operation size))
            '(append append-and-fold)))
          '(array fxarray flarray)))))

(benchmark-cli
 'arrays-benchmark
 "Compare generic arrays with fixnum and flonum arrays."
 (lambda (ref)
   (let ([size (benchmark-size ref)])
     (benchmark-run-cli!
      'arrays-benchmark "array representations"
      (make-array-benchmarks size) ref)))
 (command-line-arguments))
