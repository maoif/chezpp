(import (chezpp) (chezpp iter))
(load "examples/common.ss")

(example-run!
 'iter-example
 "Map, filter, zip, and fold iterators with deterministic cleanup."
 '(limit)
 (lambda (ref)
   (let* ([limit (example-option-natural 'iter-example ref 'limit 5)]
          [mapped (iter-map (lambda (x) (* x 2)) (range 10))]
          [filtered (iter-filter even? (range 10))]
          [zipped (iter-zip mapped filtered)])
     ;; Pull and finalize the zipped iterator even when bounded traversal stops early.
     (dynamic-wind
       void
       (lambda ()
         (let loop ([left limit] [out '()])
           (if (zero? left)
               (example-print (reverse out))
               (let ([pair (iter-next! zipped)])
                 (if (iter-end? pair)
                     (example-print (reverse out))
                     (loop (- left 1) (cons pair out)))))))
       (lambda () (iter-finalize! zipped)))
     ;; Fold a separate iterator and let iter-fold finalize it.
     (example-print (iter-fold + 0 (range 6)))
     0))
 (command-line-arguments))
