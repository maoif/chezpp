(library (chezpp examples transfer file-transfer protobuf)
  (export file-chunk? make-file-chunk file-chunk-name file-chunk-offset
          file-chunk-data file-chunk-sha256 file-chunk-done?
          file-chunk-unknown-fields file-chunk-encoded-size file-chunk-encode
          bytevector->file-chunk transfer-result? make-transfer-result
          transfer-result-size transfer-result-sha256 transfer-result-unknown-fields
          transfer-result-encoded-size transfer-result-encode
          bytevector->transfer-result file-transfer-upload-method
          file-transfer-download-method file-transfer-upload file-transfer-download
          register-file-transfer-service!)
  (import (chezpp))

  (define bytevector-list-append
    (lambda (part*)
      (let-values ([(port get) (open-bytevector-output-port)])
        (for-each (lambda (part) (put-bytevector port part)) part*)
        (get))))

  (define-record-type (file-chunk %make-file-chunk file-chunk?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name file-chunk-name)
            (immutable offset file-chunk-offset)
            (immutable data file-chunk-data)
            (immutable sha256 file-chunk-sha256)
            (immutable done? file-chunk-done?)
            (immutable unknown-fields file-chunk-unknown-fields)))

  #|proc:make-file-chunk
The `make-file-chunk` procedure creates a file chunk named `name` at byte `offset`.
`data` is its payload, `sha256` is the final digest, and `done?` marks the last chunk.
The return value is a new FileChunk record.
|#
  (define make-file-chunk
    (lambda (name offset data sha256 done?)
      (pcheck ([string? name] [natural? offset] [bytevector? data sha256]
               [boolean? done?])
        (%make-file-chunk name offset data sha256 done? '#()))))

  #|proc:file-chunk-encode
The `file-chunk-encode` procedure encodes FileChunk record `message`.
The return value is a newly allocated protobuf bytevector.
|#
  (define file-chunk-encode
    (lambda (message)
      (pcheck ([file-chunk? message])
        (bytevector-list-append
         (cons
          (protobuf-encode-message
           (append
            (if (string=? (file-chunk-name message) "") '()
                (list (list 1 'string (file-chunk-name message))))
            (if (zero? (file-chunk-offset message)) '()
                (list (list 2 'uint64 (file-chunk-offset message))))
            (if (zero? (bytevector-length (file-chunk-data message))) '()
                (list (list 3 'bytes (file-chunk-data message))))
            (if (zero? (bytevector-length (file-chunk-sha256 message))) '()
                (list (list 4 'bytes (file-chunk-sha256 message))))
            (if (file-chunk-done? message) '((5 bool #t)) '())))
          (vector->list (file-chunk-unknown-fields message)))))))

  #|proc:file-chunk-encoded-size
The `file-chunk-encoded-size` procedure measures FileChunk record `message`.
The return value is its encoded byte length.
|#
  (define file-chunk-encoded-size
    (lambda (message)
      (pcheck ([file-chunk? message]) (bytevector-length (file-chunk-encode message)))))

  #|proc:bytevector->file-chunk
The `bytevector->file-chunk` procedure decodes protobuf bytevector `bytes`.
The return value is a FileChunk record that retains unknown fields.
|#
  (define bytevector->file-chunk
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([decoder (make-protobuf-decoder bytes)] [name ""] [offset 0]
              [data #vu8()] [sha256 #vu8()] [done? #f])
          (let loop ()
            (let ([field (protobuf-decoder-next-field decoder)])
              (when field
                (case (protobuf-wire-field-number field)
                  [(1) (set! name (protobuf-decode-string
                                   (protobuf-wire-field-value field)))]
                  [(2) (set! offset (protobuf-wire-field-value field))]
                  [(3) (set! data (protobuf-wire-field-value field))]
                  [(4) (set! sha256 (protobuf-wire-field-value field))]
                  [(5) (set! done? (not (zero? (protobuf-wire-field-value field))))]
                  [else (protobuf-decoder-preserve-field! decoder field)])
                (loop))))
          (%make-file-chunk name offset data sha256 done?
                            (protobuf-decoder-unknown-fields decoder))))))

  (define-record-type (transfer-result %make-transfer-result transfer-result?)
    (sealed #t)
    (opaque #f)
    (fields (immutable size transfer-result-size)
            (immutable sha256 transfer-result-sha256)
            (immutable unknown-fields transfer-result-unknown-fields)))

  #|proc:make-transfer-result
The `make-transfer-result` procedure creates a result with byte `size` and `sha256`.
The return value is a new TransferResult record.
|#
  (define make-transfer-result
    (lambda (size sha256)
      (pcheck ([natural? size] [bytevector? sha256])
        (%make-transfer-result size sha256 '#()))))

  #|proc:transfer-result-encode
The `transfer-result-encode` procedure encodes TransferResult record `message`.
The return value is a newly allocated protobuf bytevector.
|#
  (define transfer-result-encode
    (lambda (message)
      (pcheck ([transfer-result? message])
        (bytevector-list-append
         (cons (protobuf-encode-message
                (append
                 (if (zero? (transfer-result-size message)) '()
                     (list (list 1 'uint64 (transfer-result-size message))))
                 (if (zero? (bytevector-length (transfer-result-sha256 message))) '()
                     (list (list 2 'bytes (transfer-result-sha256 message))))))
               (vector->list (transfer-result-unknown-fields message)))))))

  #|proc:transfer-result-encoded-size
The `transfer-result-encoded-size` procedure measures TransferResult record `message`.
The return value is its encoded byte length.
|#
  (define transfer-result-encoded-size
    (lambda (message)
      (pcheck ([transfer-result? message])
        (bytevector-length (transfer-result-encode message)))))

  #|proc:bytevector->transfer-result
The `bytevector->transfer-result` procedure decodes protobuf bytevector `bytes`.
The return value is a TransferResult record that retains unknown fields.
|#
  (define bytevector->transfer-result
    (lambda (bytes)
      (pcheck ([bytevector? bytes])
        (let ([decoder (make-protobuf-decoder bytes)] [size 0] [sha256 #vu8()])
          (let loop ()
            (let ([field (protobuf-decoder-next-field decoder)])
              (when field
                (case (protobuf-wire-field-number field)
                  [(1) (set! size (protobuf-wire-field-value field))]
                  [(2) (set! sha256 (protobuf-wire-field-value field))]
                  [else (protobuf-decoder-preserve-field! decoder field)])
                (loop))))
          (%make-transfer-result size sha256
                                 (protobuf-decoder-unknown-fields decoder))))))

  (define file-transfer-upload-method "/chezpp.examples.transfer.FileTransfer/Upload")
  (define file-transfer-download-method "/chezpp.examples.transfer.FileTransfer/Download")

  #|proc:file-transfer-upload
The `file-transfer-upload` procedure uploads FileChunk records in vector `message*`.
`channel` is a client gRPC channel. The return value is a TransferResult record.
|#
  (define file-transfer-upload
    (lambda (channel message*)
      (pcheck ([grpc-channel? channel] [vector? message*])
        (let ([stream (grpc-call/client-stream channel file-transfer-upload-method)])
          (dynamic-wind
            void
            (lambda ()
              (vector-for-each
               (lambda (message) (grpc-stream-send stream (file-chunk-encode message)))
               message*)
              (grpc-stream-close-send stream)
              (bytevector->transfer-result (grpc-stream-recv stream)))
            (lambda () (grpc-stream-close stream)))))))

  #|proc:file-transfer-download
The `file-transfer-download` procedure starts a download for FileChunk `request`.
`channel` is a client gRPC channel. The return value is a gRPC response stream.
|#
  (define file-transfer-download
    (lambda (channel request)
      (pcheck ([grpc-channel? channel] [file-chunk? request])
        (grpc-call/server-stream channel file-transfer-download-method
                                 (file-chunk-encode request)))))

  #|proc:register-file-transfer-service!
The `register-file-transfer-service!` procedure registers handlers on `server`.
`upload` accepts a client stream; `download` accepts a server stream. It returns `server`.
|#
  (define register-file-transfer-service!
    (lambda (server upload download)
      (pcheck ([grpc-channel? server] [procedure? upload download])
        (grpc-register-service! server file-transfer-upload-method 'client upload)
        (grpc-register-service! server file-transfer-download-method 'server download)
        server)))
)
