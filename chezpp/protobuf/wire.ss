(library (chezpp protobuf wire)
  (export protobuf-encode-varint protobuf-decode-varint
          protobuf-encode-zigzag protobuf-decode-zigzag
          protobuf-encode-fixed32 protobuf-decode-fixed32
          protobuf-encode-fixed64 protobuf-decode-fixed64
          protobuf-encode-field protobuf-encode-message)
  (import (chezpp chez) (chezpp utils))

  (define protobuf-max-field-number 536870911)
  (define protobuf-max-u64 #xffffffffffffffff)

  (define protobuf-u64?
    (lambda (value)
      (and (integer? value) (exact? value) (<= 0 value protobuf-max-u64))))

  (define protobuf-bytevector-append
    (lambda (part*)
      (let ([size (fold-left (lambda (total part) (+ total (bytevector-length part))) 0 part*)])
        (let ([out (make-bytevector size 0)])
          (let loop ([rest part*] [index 0])
            (unless (null? rest)
              (let ([part (car rest)])
                (bytevector-copy! part 0 out index (bytevector-length part))
                (loop (cdr rest) (+ index (bytevector-length part))))))
          out))))

  (define protobuf-unsigned-varint
    (lambda (value)
      (let ([size (let loop ([n value] [count 1])
                    (if (< n 128) count (loop (bitwise-arithmetic-shift n -7) (+ count 1))))])
        (let ([out (make-bytevector size 0)])
          (let loop ([n value] [index 0])
            (if (< n 128)
                (begin (bytevector-u8-set! out index n) out)
                (begin
                  (bytevector-u8-set! out index (bitwise-ior #x80 (bitwise-and n #x7f)))
                  (loop (bitwise-arithmetic-shift n -7) (+ index 1)))))))))

  (define protobuf-decode-varint/at
    (lambda (who bytes start limit)
      (let loop ([index start] [shift 0] [value 0] [count 0])
        (when (= index limit) (errorf who "truncated varint"))
        (when (= count 10) (errorf who "varint exceeds 64 bits"))
        (let ([octet (bytevector-u8-ref bytes index)])
          (when (and (= count 9) (> octet 1)) (errorf who "varint exceeds 64 bits"))
          (let ([next (bitwise-ior value
                                   (bitwise-arithmetic-shift (bitwise-and octet #x7f) shift))])
            (if (zero? (bitwise-and octet #x80))
                (values next (+ index 1))
                (loop (+ index 1) (+ shift 7) next (+ count 1))))))))

  #|proc:protobuf-encode-varint
The `protobuf-encode-varint` procedure encodes unsigned 64-bit `value` as a protobuf varint.
The return value is a newly allocated bytevector.
|#
  (define protobuf-encode-varint
    (lambda (value)
      (pcheck ([protobuf-u64? value]) (protobuf-unsigned-varint value))))

  #|proc:protobuf-decode-varint
The `protobuf-decode-varint` procedure decodes the complete protobuf-varint bytevector `bytes`.
The return value is its unsigned 64-bit integer value.
|#
  (define protobuf-decode-varint
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let-values ([(value index) (protobuf-decode-varint/at 'protobuf-decode-varint
                                                                 bytes 0 (bytevector-length bytes))])
          (unless (= index (bytevector-length bytes))
            (errorf 'protobuf-decode-varint "trailing bytes after varint"))
          value))))

  #|proc:protobuf-encode-zigzag
The `protobuf-encode-zigzag` procedure encodes signed 64-bit `value` with protobuf zigzag.
The return value is a newly allocated bytevector.
|#
  (define protobuf-encode-zigzag
    (lambda (value)
      (pcheck ([integer? value])
        (protobuf-encode-varint
         (bitwise-and (bitwise-xor (bitwise-arithmetic-shift value 1)
                                   (bitwise-arithmetic-shift value -63))
                      protobuf-max-u64)))))

  #|proc:protobuf-decode-zigzag
The `protobuf-decode-zigzag` procedure decodes zigzag protobuf-varint bytevector `bytes`.
The return value is its signed integer value.
|#
  (define protobuf-decode-zigzag
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([value (protobuf-decode-varint bytes)])
          (bitwise-xor (bitwise-arithmetic-shift value -1)
                       (- (bitwise-and value 1)))))))

  (define protobuf-fixed?
    (lambda (bits value)
      (and (integer? value) (exact? value)
           (<= 0 value (- (bitwise-arithmetic-shift 1 bits) 1)))))

  #|proc:protobuf-encode-fixed32
The `protobuf-encode-fixed32` procedure encodes unsigned 32-bit `value` in little-endian order.
The return value is a four-byte bytevector.
|#
  (define protobuf-encode-fixed32
    (lambda (value)
      (pcheck ([integer? value])
        (unless (protobuf-fixed? 32 value) (errorf 'protobuf-encode-fixed32 "invalid value: ~s" value))
        (let ([out (make-bytevector 4 0)])
          (bytevector-u32-set! out 0 value (endianness little)) out))))

  #|proc:protobuf-decode-fixed32
The `protobuf-decode-fixed32` procedure decodes four little-endian bytes in `bytes`.
The return value is an unsigned 32-bit integer.
|#
  (define protobuf-decode-fixed32
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 4) (errorf 'protobuf-decode-fixed32 "four bytes required"))
        (bytevector-u32-ref bytes 0 (endianness little)))))

  #|proc:protobuf-encode-fixed64
The `protobuf-encode-fixed64` procedure encodes unsigned 64-bit `value` in little-endian order.
The return value is an eight-byte bytevector.
|#
  (define protobuf-encode-fixed64
    (lambda (value)
      (pcheck ([protobuf-u64? value])
        (let ([out (make-bytevector 8 0)])
          (bytevector-u64-set! out 0 value (endianness little)) out))))

  #|proc:protobuf-decode-fixed64
The `protobuf-decode-fixed64` procedure decodes eight little-endian bytes in `bytes`.
The return value is an unsigned 64-bit integer.
|#
  (define protobuf-decode-fixed64
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (unless (= (bytevector-length bytes) 8) (errorf 'protobuf-decode-fixed64 "eight bytes required"))
        (bytevector-u64-ref bytes 0 (endianness little)))))

  (define protobuf-field-payload
    (lambda (type value)
      (case type
       [(bool) (protobuf-encode-varint (if value 1 0))]
       [(enum uint32 uint64) (protobuf-encode-varint value)]
       [(sint32 sint64) (protobuf-encode-zigzag value)]
       [(fixed32) (protobuf-encode-fixed32 value)]
       [(fixed64) (protobuf-encode-fixed64 value)]
       [(bytes) value]
       [(string) (string->utf8 value)]
       [(message) value]
       [else (errorf 'protobuf-encode-field "unsupported field type: ~s" type)])))

  #|proc:protobuf-encode-field
The `protobuf-encode-field` procedure encodes field number `number`, protobuf `type`, and `value`.
The return value is a newly allocated wire-format bytevector.
|#
  (define protobuf-encode-field
    (lambda (number type value)
      (pcheck ([integer? number] [symbol? type])
        (unless (and (positive? number) (<= number protobuf-max-field-number))
          (errorf 'protobuf-encode-field "invalid field number: ~s" number))
        (let* ([wire-type (case type
                            [(bool enum uint32 uint64 sint32 sint64) 0]
                            [(fixed64) 1]
                            [(bytes string message) 2]
                            [(fixed32) 5]
                            [else (errorf 'protobuf-encode-field "unsupported field type: ~s" type)])]
               [payload (protobuf-field-payload type value)]
               [tag (protobuf-encode-varint (bitwise-ior (bitwise-arithmetic-shift number 3) wire-type))])
          (if (= wire-type 2)
              (protobuf-bytevector-append (list tag (protobuf-encode-varint (bytevector-length payload)) payload))
              (protobuf-bytevector-append (list tag payload)))))))

  #|proc:protobuf-encode-message
The `protobuf-encode-message` procedure encodes `field*`, a list of `(number type value)` fields.
The return value is one newly allocated protobuf message bytevector.
|#
  (define protobuf-encode-message
    (lambda (field*)
      (pcheck ([list? field*])
        (protobuf-bytevector-append
         (map (lambda (field)
                (unless (and (list? field) (= (length field) 3))
                  (errorf 'protobuf-encode-message "field must be (number type value): ~s" field))
                (protobuf-encode-field (car field) (cadr field) (caddr field)))
              field*)))))
)
