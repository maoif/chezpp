(import (chezpp) (chezpp parser xml))
(load "examples/common.ss")

(example-run!
 'xml-example
 "Parse an XML file and print its document value."
 '(file)
 (lambda (ref)
   (let ([path (example-required 'xml-example ref 'file)])
     ;; Parse the selected XML file into an xml-document.
     (example-print (parse-xml-file path)))
   0)
 (command-line-arguments))
