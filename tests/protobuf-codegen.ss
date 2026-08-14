(import (chezpp))
(load "generated/file-transfer.pb.ss")

(define generated-environment
  (environment '(chezpp) '(chezpp examples transfer file-transfer protobuf)))

(define generated-eval
  (lambda (expression)
    (eval expression generated-environment)))

(mat protobuf-codegen
     (generated-eval
      '(let* ([message (make-file-chunk "payload.bin" 4096 #vu8(1 2 3 4)
                                        #vu8(9 8 7 6) #t)]
              [encoded (file-chunk-encode message)]
              [decoded (bytevector->file-chunk encoded)])
         (and (file-chunk? decoded)
              (string=? "payload.bin" (file-chunk-name decoded))
              (= 4096 (file-chunk-offset decoded))
              (equal? #vu8(1 2 3 4) (file-chunk-data decoded))
              (equal? #vu8(9 8 7 6) (file-chunk-sha256 decoded))
              (file-chunk-done? decoded)
              (= (bytevector-length encoded) (file-chunk-encoded-size message)))))

     (generated-eval
      '(let* ([known (file-chunk-encode (make-file-chunk "x" 0 #vu8() #vu8() #f))]
              [with-unknown (let-values ([(port get) (open-bytevector-output-port)])
                              (put-bytevector port known)
                              (put-bytevector port #vu8(152 6 7))
                              (get))]
              [decoded (bytevector->file-chunk with-unknown)])
         (and (equal? '#(#vu8(152 6 7)) (file-chunk-unknown-fields decoded))
              (equal? with-unknown (file-chunk-encode decoded)))))

     (generated-eval
      '(let* ([message (make-transfer-result 8192 #vu8(4 3 2 1))]
              [decoded (bytevector->transfer-result (transfer-result-encode message))])
         (and (= 8192 (transfer-result-size decoded))
              (equal? #vu8(4 3 2 1) (transfer-result-sha256 decoded)))))

     (string=? "/chezpp.examples.transfer.FileTransfer/Upload"
               (generated-eval 'file-transfer-upload-method))

     (string=? "/chezpp.examples.transfer.FileTransfer/Download"
               (generated-eval 'file-transfer-download-method)))
