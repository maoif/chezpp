(library (chezpp net grpc reflection)
  (export grpc-reflection-registry? make-grpc-reflection-registry
          grpc-reflection-register-file! grpc-register-reflection!)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp protobuf)
          (chezpp net grpc))

  (define grpc-reflection-method
    "/grpc.reflection.v1.ServerReflection/ServerReflectionInfo")

  (define-record-type (grpc-reflection-registry %make-grpc-reflection-registry
                                                grpc-reflection-registry?)
    (sealed #t)
    (opaque #f)
    (fields (immutable files grpc-reflection-registry-files)
            (immutable symbols grpc-reflection-registry-symbols)
            (immutable services grpc-reflection-registry-services)
            (immutable mutex grpc-reflection-registry-mutex)))

  #|proc:make-grpc-reflection-registry
The `make-grpc-reflection-registry` procedure creates an empty synchronized descriptor registry.
The return value is a new gRPC reflection registry.
|#
  (define make-grpc-reflection-registry
    (lambda ()
      (%make-grpc-reflection-registry
       (make-hashtable string-hash string=?)
       (make-hashtable string-hash string=?)
       (make-hashtable string-hash string=?)
       (make-mutex))))

  #|proc:grpc-reflection-register-file!
The `grpc-reflection-register-file!` procedure registers one encoded file descriptor.
`registry` receives `filename`, descriptor `bytes`, defined `symbol*`, and gRPC `service*` names.
The symbol and service parameters are lists of strings. The return value is `registry`.
|#
  (define grpc-reflection-register-file!
    (lambda (registry filename bytes symbol* service*)
      (pcheck ([grpc-reflection-registry? registry] [string? filename]
               [bytevector? bytes] [list? symbol* service*])
        (unless (and (andmap string? symbol*) (andmap string? service*))
          (errorf 'grpc-reflection-register-file!
                  "symbol and service names must be strings"))
        (with-mutex (grpc-reflection-registry-mutex registry)
          (hashtable-set! (grpc-reflection-registry-files registry) filename bytes)
          (for-each
           (lambda (symbol)
             (hashtable-set! (grpc-reflection-registry-symbols registry) symbol filename))
           symbol*)
          (for-each
           (lambda (service)
             (hashtable-set! (grpc-reflection-registry-services registry) service filename))
           service*))
        registry)))

  (define reflection-error
    (lambda (request code message)
      (protobuf-encode-message
       (list (list 2 'message request)
             (list 7 'message
                   (protobuf-encode-message
                    (list (list 1 'uint32 code) (list 2 'string message))))))))

  (define reflection-file-response
    (lambda (request descriptor)
      (protobuf-encode-message
       (list (list 2 'message request)
             (list 4 'message
                   (protobuf-encode-message (list (list 1 'bytes descriptor))))))))

  (define registry-service-name*
    (lambda (registry)
      (let-values ([(key* value*)
                    (hashtable-entries (grpc-reflection-registry-services registry))])
        (sort string<? (vector->list key*)))))

  (define reflection-list-response
    (lambda (registry request)
      (let ([service-field*
             (map (lambda (name)
                    (list 1 'message
                          (protobuf-encode-message (list (list 1 'string name)))))
                  (registry-service-name* registry))])
        (protobuf-encode-message
         (list (list 2 'message request)
               (list 6 'message (protobuf-encode-message service-field*)))))))

  (define reflection-request-kind
    (lambda (bytes)
      (let ([decoder (make-protobuf-decoder bytes)] [kind #f] [value #f])
        (let loop ()
          (let ([field (protobuf-decoder-next-field decoder)])
            (when field
              (case (protobuf-wire-field-number field)
                [(3) (set! kind 'filename) (set! value (protobuf-decode-string
                                                        (protobuf-wire-field-value field)))]
                [(4) (set! kind 'symbol) (set! value (protobuf-decode-string
                                                      (protobuf-wire-field-value field)))]
                [(7) (set! kind 'list-services) (set! value #t)])
              (loop))))
        (values kind value))))

  (define reflection-answer
    (lambda (registry request)
      (with-mutex (grpc-reflection-registry-mutex registry)
        (let-values ([(kind value) (reflection-request-kind request)])
          (case kind
            [(list-services) (reflection-list-response registry request)]
            [(filename)
             (let ([descriptor
                    (hashtable-ref (grpc-reflection-registry-files registry) value #f)])
               (if descriptor
                   (reflection-file-response request descriptor)
                   (reflection-error request 5 "file descriptor not found")))]
            [(symbol)
             (let* ([filename
                     (hashtable-ref (grpc-reflection-registry-symbols registry) value #f)]
                    [descriptor
                     (and filename
                          (hashtable-ref
                           (grpc-reflection-registry-files registry) filename #f))])
               (if descriptor
                   (reflection-file-response request descriptor)
                   (reflection-error request 5 "symbol not found")))]
            [else (reflection-error request 12 "reflection request is unsupported")])))))

  #|proc:grpc-register-reflection!
The `grpc-register-reflection!` procedure registers v1 reflection on server `channel`.
`registry` supplies descriptor bytes and service names. The return value is `channel`.
|#
  (define grpc-register-reflection!
    (lambda (channel registry)
      (pcheck ([grpc-channel? channel] [grpc-reflection-registry? registry])
        (grpc-register-service!
         channel grpc-reflection-method 'bidi
         (lambda (stream)
           (let loop ()
             (let ([request (grpc-stream-recv stream)])
               (unless (eof-object? request)
                 (grpc-stream-send stream (reflection-answer registry request))
                 (loop))))))
        channel)))
)
