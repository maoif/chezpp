(import (chezpp))
(load "benchmarks/common.ss")

(define make-vector-case
  (lambda (type operation size)
    (make-benchmark
     (string->symbol (format "~a-~a" type operation))
     (lambda (state n input)
       (let ([values
              (case type
                [(vector) (vector-copy input)]
                [(fxvector) (let ([out (make-fxvector n 0)])
                              (do ([i 0 (+ i 1)]) ((= i n) out)
                                (fxvector-set! out i i)))]
                [(flvector) (let ([out (make-flvector n 0.0)])
                              (do ([i 0 (+ i 1)]) ((= i n) out)
                                (flvector-set! out i (exact->inexact i))))])])
         (case operation
           [(fill)
            (benchmark-consume
             (case type
               [(vector) (vector-set! values 0 1)]
               [(fxvector) (fxvector-set! values 0 1)]
               [(flvector) (flvector-set! values 0 1.0)]))]
           [(read-fold)
            (benchmark-consume
             (case type
               [(vector) (vector-fold-left + 0 values)]
               [(fxvector) (fxvfold-left fx+ 0 values)]
               [(flvector) (flvfold-left fl+ 0.0 values)]))])))
     `((args (n) (,size))
       (setup . ,(lambda (state) (make-vector size 0)))
       (warmup . 0) (samples . 5) (min-time . 0.01) (max-iterations . 10000)))))

(define make-vector-benchmarks
  (lambda (size)
    (apply append
     (map (lambda (type)
       (map (lambda (operation) (make-vector-case type operation size))
            '(fill read-fold)))
          '(vector fxvector flvector)))))

(benchmark-cli
 'vectors-benchmark
 "Compare generic vectors with fixnum and flonum vectors."
 (lambda (ref)
   (let ([size (benchmark-size ref)])
     (benchmark-run-cli!
      'vectors-benchmark "vector representations"
      (make-vector-benchmarks size) ref)))
 (command-line-arguments))
