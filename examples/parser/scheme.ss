(import (chezpp) (chezpp parser scheme))
(load "examples/common.ss")

(example-run!
 'scheme-example
 "Parse Scheme datums from a file and print the resulting list."
 '(file)
 (lambda (ref)
   (let ([path (example-required 'scheme-example ref 'file)])
     ;; Parse all Scheme datums in this file.
     (example-print (parse-scheme-file path)))
   0)
 (command-line-arguments))
