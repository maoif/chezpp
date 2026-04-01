(library (chezpp net private rpc)
  (export rpc-field-descriptor?
          rpc-field-descriptor-index
          rpc-field-descriptor-name
          rpc-field-descriptor-type
          rpc-field-descriptor-accessor
          rpc-field-descriptor-optional?
          make-rpc-field-descriptor
          rpc-message-descriptor?
          rpc-message-descriptor-name
          rpc-message-descriptor-wire-name
          rpc-message-descriptor-fields
          rpc-message-descriptor-predicate
          rpc-message-descriptor-constructor
          make-rpc-message-descriptor
          register-rpc-message-descriptor!
          lookup-rpc-message-descriptor
          find-rpc-message-descriptor
          rpc-method-descriptor?
          rpc-method-descriptor-service
          rpc-method-descriptor-name
          rpc-method-descriptor-full-name
          rpc-method-descriptor-request
          rpc-method-descriptor-response
          rpc-method-descriptor-stream
          make-rpc-method-descriptor
          rpc-service-descriptor?
          rpc-service-descriptor-name
          rpc-service-descriptor-methods
          make-rpc-service-descriptor
          rpc-type-predicate
          rpc-message->wire
          rpc-wire->message
          rpc-encode-payload
          rpc-decode-payload)
  (import (chezpp chez)
          (chezpp utils))

  (define-record-type (rpc-field-descriptor
                       make-rpc-field-descriptor
                       rpc-field-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable index rpc-field-descriptor-index)
            (immutable name rpc-field-descriptor-name)
            (immutable type rpc-field-descriptor-type)
            (immutable accessor rpc-field-descriptor-accessor)
            (immutable optional? rpc-field-descriptor-optional?)))

  (define-record-type (rpc-message-descriptor
                       make-rpc-message-descriptor
                       rpc-message-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name rpc-message-descriptor-name)
            (immutable wire-name rpc-message-descriptor-wire-name)
            (immutable fields rpc-message-descriptor-fields)
            (immutable predicate rpc-message-descriptor-predicate)
            (immutable constructor rpc-message-descriptor-constructor)))

  (define-record-type (rpc-method-descriptor
                       make-rpc-method-descriptor
                       rpc-method-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable service rpc-method-descriptor-service)
            (immutable name rpc-method-descriptor-name)
            (immutable full-name rpc-method-descriptor-full-name)
            (immutable request rpc-method-descriptor-request)
            (immutable response rpc-method-descriptor-response)
            (immutable stream rpc-method-descriptor-stream)))

  (define-record-type (rpc-service-descriptor
                       make-rpc-service-descriptor
                       rpc-service-descriptor?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name rpc-service-descriptor-name)
            (immutable methods rpc-service-descriptor-methods)))

  (define rpc-message-registry (make-eq-hashtable))
  (define rpc-message-registry-list '())

  (define register-rpc-message-descriptor!
    (lambda (descriptor)
      (hashtable-set! rpc-message-registry
                      (rpc-message-descriptor-name descriptor)
                      descriptor)
      (set! rpc-message-registry-list
            (cons descriptor rpc-message-registry-list))
      descriptor))

  (define lookup-rpc-message-descriptor
    (lambda (name)
      (hashtable-ref rpc-message-registry name #f)))

  (define find-rpc-message-descriptor
    (lambda (x)
      (let loop ([rest rpc-message-registry-list])
        (and (pair? rest)
             (let ([descriptor (car rest)])
               (if ((rpc-message-descriptor-predicate descriptor) x)
                   descriptor
                   (loop (cdr rest))))))))

  (define builtin-type-predicate
    (lambda (type)
      (case type
        [(any) (lambda (x) #t)]
        [(boolean) boolean?]
        [(bytevector bytes) bytevector?]
        [(char) char?]
        [(fixnum) fixnum?]
        [(integer) integer?]
        [(number) number?]
        [(real) real?]
        [(list) list?]
        [(pair) pair?]
        [(string text) string?]
        [(symbol) symbol?]
        [(vector) vector?]
        [(hashtable) hashtable?]
        [else #f])))

  (define rpc-type-predicate
    (lambda (type)
      (cond
       [(and (pair? type)
             (eq? (car type) 'optional)
             (pair? (cdr type))
             (null? (cddr type)))
        (let ([pred (rpc-type-predicate (cadr type))])
          (lambda (x)
            (or (not x) (pred x))))]
       [(builtin-type-predicate type)]
       [(rpc-message-descriptor? type)
        (rpc-message-descriptor-predicate type)]
       [(symbol? type)
        (let ([descriptor (lookup-rpc-message-descriptor type)])
          (if descriptor
              (rpc-message-descriptor-predicate descriptor)
              (errorf 'rpc-type-predicate "unknown RPC field type ~s" type)))]
       [else
        (errorf 'rpc-type-predicate "invalid RPC field type ~s" type)])))

  (define rpc-message->wire
    (lambda (descriptor obj)
      (map (lambda (field)
             (let ([value ((rpc-field-descriptor-accessor field) obj)])
               (cons (rpc-field-descriptor-index field)
                     (rpc-encode-payload value (rpc-field-descriptor-type field)))))
           (let loop ([rest (rpc-message-descriptor-fields descriptor)] [out '()])
             (if (null? rest)
                 (reverse out)
                 (let* ([field (car rest)]
                        [value ((rpc-field-descriptor-accessor field) obj)])
                   (if (and (rpc-field-descriptor-optional? field) (not value))
                       (loop (cdr rest) out)
                       (loop (cdr rest) (cons field out)))))))))

  (define field-wire-ref
    (lambda (fields index)
      (let ([entry (assq index fields)])
        (and entry (cdr entry)))))

  (define rpc-wire->message
    (lambda (descriptor fields)
      (unless (list? fields)
        (errorf 'rpc-wire->message "invalid RPC message payload ~s" fields))
      (apply (rpc-message-descriptor-constructor descriptor)
             (map (lambda (field)
                    (let ([raw (field-wire-ref fields
                                               (rpc-field-descriptor-index field))])
                      (cond
                       [raw
                        (rpc-decode-payload raw (rpc-field-descriptor-type field))]
                       [(rpc-field-descriptor-optional? field) #f]
                       [else
                        (errorf 'rpc-wire->message
                                "missing required RPC field ~s in ~s"
                                (rpc-field-descriptor-name field)
                                (rpc-message-descriptor-name descriptor))])))
                  (rpc-message-descriptor-fields descriptor)))))

  (define-values (rpc-encode-payload rpc-decode-payload)
    (letrec ((encode
              (case-lambda
                ((x) (encode x #f))
                ((x declared-type)
                 (cond
                  ((and (pair? declared-type)
                        (eq? (car declared-type) 'optional)
                        (pair? (cdr declared-type))
                        (null? (cddr declared-type)))
                   (if x
                       (encode x (cadr declared-type))
                       '#(rpc-none)))
                  ((not declared-type)
                   (let ((descriptor (find-rpc-message-descriptor x)))
                     (if descriptor
                         (vector 'rpc-message
                                 (rpc-message-descriptor-wire-name descriptor)
                                 (rpc-message->wire descriptor x))
                         (vector 'rpc-raw x))))
                  ((builtin-type-predicate declared-type)
                   (vector 'rpc-raw x))
                  ((rpc-message-descriptor? declared-type)
                   (vector 'rpc-message
                           (rpc-message-descriptor-wire-name declared-type)
                           (rpc-message->wire declared-type x)))
                  ((symbol? declared-type)
                   (let ((descriptor (lookup-rpc-message-descriptor declared-type)))
                     (if descriptor
                         (vector 'rpc-message
                                 (rpc-message-descriptor-wire-name descriptor)
                                 (rpc-message->wire descriptor x))
                         (vector 'rpc-raw x))))
                  (else
                   (errorf 'rpc-encode-payload
                           "invalid declared RPC type ~s"
                           declared-type))))))
             (decode
              (case-lambda
                ((x) (decode x #f))
                ((x declared-type)
                 (cond
                  ((and (pair? declared-type)
                        (eq? (car declared-type) 'optional)
                        (pair? (cdr declared-type))
                        (null? (cddr declared-type)))
                   (if (and (vector? x)
                            (= (vector-length x) 1)
                            (eq? (vector-ref x 0) 'rpc-none))
                       #f
                       (decode x (cadr declared-type))))
                  ((and (vector? x)
                        (= (vector-length x) 2)
                        (eq? (vector-ref x 0) 'rpc-raw))
                   (vector-ref x 1))
                  ((and (vector? x)
                        (= (vector-length x) 3)
                        (eq? (vector-ref x 0) 'rpc-message))
                   (let* ((wire-name (vector-ref x 1))
                          (fields (vector-ref x 2))
                          (descriptor
                           (cond
                            ((rpc-message-descriptor? declared-type)
                             declared-type)
                            ((symbol? declared-type)
                             (lookup-rpc-message-descriptor declared-type))
                            (else
                             (lookup-rpc-message-descriptor wire-name)))))
                     (if descriptor
                         (rpc-wire->message descriptor fields)
                         (errorf 'rpc-decode-payload
                                 "unknown RPC message type ~s"
                                 wire-name))))
                  (else
                   x))))))
      (values encode decode)))
  )
