(import (chezpp))
(load "examples/common.ss")

(example-run!
 'array-example
 "Fill generic, fixnum, and flonum arrays and print their contents."
 '(size)
 (lambda (ref)
   (let ([size (example-option-natural 'array-example ref 'size 5)])
     (unless (fixnum? size)
       (errorf 'array-example "--size must fit in a fixnum"))
     ;; Construct an empty generic array.
     (let ([generic (make-array)])
       ;; Append a sequence of integers to the generic array.
       (do ([index 0 (+ index 1)]) ((= index size))
         (array-add! generic index))
       ;; Construct an empty fixnum array.
       (let ([fixnums (make-fxarray 0)])
         ;; Append fixnums to the specialized array.
         (do ([index 0 (+ index 1)]) ((= index size))
           (fxarray-add! fixnums index))
         ;; Construct an empty flonum array.
         (let ([flonums (make-flarray)])
           ;; Append finite flonum values to the specialized array.
           (do ([index 0 (+ index 1)]) ((= index size))
             (flarray-add! flonums (exact->inexact index)))
           ;; Convert each representation to a list for comparison.
           (example-print
            (list (array->list generic)
                  (fxarray->list fixnums)
                  (flarray->list flonums))))))
     0))
 (command-line-arguments))
