(import (chezpp))

(define protobuf-error?
  (lambda (thunk)
    (guard (condition [else #t]) (thunk) #f)))

(mat protobuf-wire
     (equal? #vu8(150 1) (protobuf-encode-varint 150))
     (= 150 (protobuf-decode-varint #vu8(150 1)))
     (equal? #vu8(10 3 97 98 99) (protobuf-encode-field 1 'string "abc"))
     (equal? #vu8(8 1 18 3 102 111 111)
             (protobuf-encode-message '((1 bool #t) (2 string "foo"))))
     (= -1 (protobuf-decode-zigzag (protobuf-encode-zigzag -1)))

     ;; Error case: a varint cannot end while its continuation bit is set.
     (protobuf-error? (lambda () (protobuf-decode-varint #vu8(128))))

     ;; Error case: protobuf varints may not exceed ten bytes.
     (protobuf-error? (lambda () (protobuf-decode-varint #vu8(128 128 128 128 128 128 128 128 128 128))))

     ;; Error case: protobuf field number zero is reserved.
     (protobuf-error? (lambda () (protobuf-encode-field 0 'bool #t)))

     ;; Error case: unsupported field types have no protobuf wire representation.
     (protobuf-error? (lambda () (protobuf-encode-field 1 'invalid #t)))

     ;; Error case: fixed-width decoding requires the complete field width.
     (protobuf-error? (lambda () (protobuf-decode-fixed32 #vu8(1 2 3)))))
