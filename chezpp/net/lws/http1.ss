(library (chezpp net lws http1)
  (export make-lws-http1-client
          lws-http1-client-close!
          lws-http1-request/nonblocking
          lws-http1-release-operation!
          lws-http1-client-pool-metrics)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net operation)
          (chezpp net poll)
          (chezpp net lws reactor))

  (define-record-type (lws-http1-client %make-lws-http1-client lws-http1-client?)
    (sealed #t)
    (opaque #t)
    (fields (immutable reactor lws-http1-client-reactor)
            (mutable next-id lws-http1-client-next-id lws-http1-client-next-id-set!)
            (mutable closed? lws-http1-client-closed? lws-http1-client-closed?-set!)))

  (define-record-type (lws-http1-state %make-lws-http1-state lws-http1-state?)
    (sealed #t)
    (opaque #t)
    (fields (immutable client lws-http1-state-client)
            (immutable operation lws-http1-state-operation)
            (immutable request lws-http1-state-request)
            (immutable response-sink lws-http1-state-response-sink)
            (immutable finish lws-http1-state-finish)
            (immutable ready lws-http1-state-ready)
            (immutable connection-id lws-http1-state-connection-id)
            (immutable stream-id lws-http1-state-stream-id)
            (immutable generation lws-http1-state-generation)
            (immutable deadline-ms lws-http1-state-deadline-ms)
            (mutable event-index lws-http1-state-event-index lws-http1-state-event-index-set!)
            (mutable headers lws-http1-state-headers lws-http1-state-headers-set!)
            (mutable status lws-http1-state-status lws-http1-state-status-set!)
            (mutable reason lws-http1-state-reason lws-http1-state-reason-set!)
            (mutable body-parts lws-http1-state-body-parts lws-http1-state-body-parts-set!)
            (mutable body-length lws-http1-state-body-length lws-http1-state-body-length-set!)
            (mutable trailers lws-http1-state-trailers lws-http1-state-trailers-set!)
            (mutable body-source lws-http1-state-body-source lws-http1-state-body-source-set!)
            (mutable body-sent? lws-http1-state-body-sent? lws-http1-state-body-sent?-set!)
            (mutable response-finished? lws-http1-state-response-finished?
                     lws-http1-state-response-finished?-set!)
            (mutable response lws-http1-state-response lws-http1-state-response-set!)))

  (define next-generation 0)

  (define current-time-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

  (define header-payload
    (lambda (headers)
      (let-values ([(port get) (open-bytevector-output-port)])
        (for-each
         (lambda (entry)
           (put-bytevector port (string->utf8 (car entry)))
           (put-u8 port 0)
           (put-bytevector port (string->utf8 (cdr entry)))
           (put-u8 port 0))
         headers)
        (get))))

  (define split-zero-headers
    (lambda (bytes)
      (let ([length (bytevector-length bytes)])
        (let loop ([offset 0] [out '()])
          (if (>= offset length)
              (reverse out)
              (let ([name-end
                     (let find ([i offset])
                       (cond
                        [(>= i length) #f]
                        [(zero? (bytevector-u8-ref bytes i)) i]
                        [else (find (+ i 1))]))])
                (if (not name-end)
                    (reverse out)
                    (let ([value-start (+ name-end 1)]
                          [value-end
                           (let find ([i (+ name-end 1)])
                             (cond
                              [(>= i length) #f]
                              [(zero? (bytevector-u8-ref bytes i)) i]
                              [else (find (+ i 1))]))])
                      (if (not value-end)
                          (reverse out)
                          (loop (+ value-end 1)
                                (cons (cons (utf8->string
                                             (let ([part (make-bytevector (- name-end offset) 0)])
                                               (bytevector-copy! bytes offset part 0
                                                                 (- name-end offset))
                                               part))
                                            (utf8->string
                                             (let ([part (make-bytevector
                                                          (- value-end value-start) 0)])
                                               (bytevector-copy! bytes value-start part 0
                                                                 (- value-end value-start))
                                               part)))
                                      out)))))))))))

  #|proc:make-lws-http1-client
The `make-lws-http1-client` procedure creates a reactor-backed HTTP/1 client transport.
`event-capacity`, `payload-capacity`, and `command-capacity` bound native and reactor pools.
`tls-context-handle` is zero or a native TLS context. `proxy-address` and `proxy-port` select the
immutable proxy policy for the transport.
The return value is an internal transport client.
|#
  (define make-lws-http1-client
    (case-lambda
      [(event-capacity payload-capacity command-capacity)
       (make-lws-http1-client event-capacity payload-capacity command-capacity 0 "" 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle)
       (make-lws-http1-client event-capacity payload-capacity command-capacity
                              tls-context-handle "" 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle
                       proxy-address proxy-port)
       (pcheck ([positive-natural? event-capacity payload-capacity command-capacity]
                [natural? tls-context-handle proxy-port]
                [string? proxy-address])
         (let ([reactor (make-lws-reactor event-capacity payload-capacity command-capacity
                                          tls-context-handle proxy-address proxy-port)])
           (lws-reactor-start! reactor)
           (%make-lws-http1-client reactor 0 #f)))]))

  (define decode-event-headers
    (lambda (state bytes)
      (lws-http1-state-headers-set! state (split-zero-headers bytes))))

  (define append-response-body!
    (lambda (state bytes)
      (let ([count (bytevector-length bytes)]
            [sink (lws-http1-state-response-sink state)])
        (if sink
            (sink bytes 0 count)
            (begin
              (lws-http1-state-body-parts-set!
               state (cons bytes (lws-http1-state-body-parts state)))
              (lws-http1-state-body-length-set!
               state (+ count (lws-http1-state-body-length state))))))))

  (define finish-response!
    (lambda (state)
      (unless (lws-http1-state-response-finished? state)
        (let* ([sink (lws-http1-state-response-sink state)]
               [body (if sink
                         #f
                         (let ([answer (make-bytevector
                                        (lws-http1-state-body-length state) 0)])
                           (let fill ([parts (reverse (lws-http1-state-body-parts state))]
                                      [offset 0])
                             (unless (null? parts)
                               (let ([part (car parts)])
                                 (bytevector-copy! part 0 answer offset
                                                   (bytevector-length part))
                                 (fill (cdr parts)
                                       (+ offset (bytevector-length part))))))
                           answer))])
          (when sink ((lws-http1-state-finish state)))
          (lws-http1-state-response-set!
           state
           (vector (lws-http1-state-status state)
                   (lws-http1-state-reason state)
                   (lws-http1-state-headers state)
                   body
                   (lws-http1-state-trailers state)
                   (if (= (vector-length (lws-http1-state-request state)) 10)
                       (string->symbol (vector-ref (lws-http1-state-request state) 8))
                       'h1)))
          (lws-http1-state-response-finished?-set! state #t)))))

  (define process-events!
    (lambda (state)
      (let* ([reactor (lws-http1-client-reactor (lws-http1-state-client state))]
             [operation (lws-http1-state-operation state)]
             [events (lws-reactor-operation-events reactor operation)])
        (let loop ([rest (list-tail events (lws-http1-state-event-index state))]
                   [index (lws-http1-state-event-index state)])
          (unless (null? rest)
            (let* ([event (car rest)]
                   [tag (vector-ref event 0)]
                   [payload (vector-ref event 6)])
              (cond
               [(eq? tag 'headers)
                (if (negative? (vector-ref event 5))
                    (lws-http1-state-trailers-set!
                     state (split-zero-headers payload))
                    (begin
                      (lws-http1-state-status-set! state (vector-ref event 5))
                      (decode-event-headers state payload)))]
               [(and (eq? tag 'connected) (= (vector-ref event 5) -2000))
                (let ([ready (lws-http1-state-ready state)])
                  (when ready (ready)))]
               [(eq? tag 'writable)
                (let ([source (lws-http1-state-body-source state)])
                  (when (and source (not (lws-http1-state-body-sent? state)))
                    (let ([chunk (source 65536)])
                      (if (eof-object? chunk)
                          (begin
                            (lws-reactor-submit-body!
                             reactor
                             (lws-http1-state-connection-id state)
                             (lws-http1-state-stream-id state)
                             (lws-http1-state-generation state)
                             #vu8() #t)
                            (lws-http1-state-body-sent?-set! state #t))
                          (lws-reactor-submit-body!
                           reactor
                           (lws-http1-state-connection-id state)
                           (lws-http1-state-stream-id state)
                           (lws-http1-state-generation state)
                           chunk #f)))))]
               [(eq? tag 'readable)
                (append-response-body! state payload)
                (lws-reactor-consume-body!
                 reactor
                 (lws-http1-state-connection-id state)
                 (lws-http1-state-stream-id state)
                 (lws-http1-state-generation state)
                 (bytevector-length payload))]
               [(eq? tag 'complete)
                (finish-response! state)]
               [else (void)])
              (loop (cdr rest) (+ index 1))))
        (lws-http1-state-event-index-set! state (length events))))))

  #|proc:lws-http1-request/nonblocking
The `lws-http1-request/nonblocking` procedure starts normalized request vector `request` on
`client`. `response-sink` is `#f` or a write/finish procedure vector. The optional `ready`
procedure is called when an HTTP/2 connection has completed stream migration. The return value is
a `net-operation` completing with a normalized response vector.
|#
  (define-who lws-http1-request/nonblocking
    (case-lambda
      [(client request response-sink)
       (lws-http1-request/nonblocking client request response-sink #f)]
      [(client request response-sink ready)
      (pcheck ([lws-http1-client? client] [vector? request]
               [(lambda (value) (or (not value) (vector? value))) response-sink]
               [(lambda (value) (or (not value) (procedure? value))) ready])
        (when (lws-http1-client-closed? client)
          (raise-net-error who 'http "HTTP client transport is closed" client))
        (unless (or (= (vector-length request) 8) (= (vector-length request) 10))
          (errorf who "expected an eight- or ten-element normalized HTTP request vector"))
        (let* ([method (vector-ref request 0)]
               [host (vector-ref request 1)]
               [port (vector-ref request 2)]
               [tls? (vector-ref request 3)]
               [path (vector-ref request 4)]
               [headers (vector-ref request 5)]
               [body-source (vector-ref request 6)]
               [alpn (if (= (vector-length request) 10)
                         (vector-ref request 8)
                         "http/1.1")]
               [connection-id (fx1+ (lws-http1-client-next-id client))]
               [stream-id connection-id]
               [generation (fx1+ next-generation)]
               [deadline-ms (if (= (vector-length request) 10)
                                (vector-ref request 9)
                                (+ (current-time-ms) 30000))]
               [reactor (lws-http1-client-reactor client)]
               [inner (make-lws-reactor-operation reactor 'http1 connection-id stream-id
                                                   generation deadline-ms #t)]
               [state #f]
               [operation #f])
          (set! next-generation generation)
          (lws-http1-client-next-id-set! client connection-id)
          (set! state
                (%make-lws-http1-state client inner request
                                        (and response-sink (vector-ref response-sink 0))
                                        (if response-sink (vector-ref response-sink 1) void)
                                        ready
                                        connection-id stream-id generation deadline-ms
                                        0 '() #f "" '() 0 '() body-source #f #f #f))
          (unless (lws-reactor-client-start!
                   reactor connection-id stream-id generation host port tls?
                   method host path (header-payload headers)
                   #vu8() (and body-source #t) alpn)
            (raise-net-error who 'http "libwebsockets rejected HTTP request" request))
          (net-operation-step! inner)
          (set! operation
                (make-net-operation
                 'http1
                 (lambda ()
                   (when (eq? 'pending (net-operation-state inner))
                     (net-operation-step! inner))
                   (process-events! state)
                   (case (net-operation-state inner)
                     [(completed)
                      (unless (lws-http1-state-response-finished? state)
                        (finish-response! state))
                      (net-operation-completed
                       (lws-http1-state-response state))]
                     [(failed cancelled)
                      (net-operation-failed
                       (net-operation-condition inner))]
                     [else
                      (net-operation-pending
                       (list (make-poll-target inner '(read))) deadline-ms)]))
                 (lambda ()
                   (net-operation-cancel! inner))
                 (lambda ()
                   (lws-reactor-release-operation! reactor inner))))
          operation))]))

  #|proc:lws-http1-release-operation!
The `lws-http1-release-operation!` procedure releases terminal operation routing state owned by
`client`. The return value is unspecified.
|#
  (define lws-http1-release-operation!
    (lambda (client operation)
      (pcheck ([lws-http1-client? client] [net-operation? operation])
        (lws-reactor-release-operation! (lws-http1-client-reactor client) operation))))

  #|proc:lws-http1-client-close!
The `lws-http1-client-close!` procedure stops `client` and releases its reactor resources.
The return value is `client`.
|#
  (define lws-http1-client-close!
    (lambda (client)
      (pcheck ([lws-http1-client? client])
        (unless (lws-http1-client-closed? client)
          (lws-http1-client-closed?-set! client #t)
          (lws-reactor-shutdown! (lws-http1-client-reactor client)))
        client)))

  #|proc:lws-http1-client-pool-metrics
The `lws-http1-client-pool-metrics` procedure returns reactor pool metrics for `client`.
|#
  (define lws-http1-client-pool-metrics
    (lambda (client)
      (pcheck ([lws-http1-client? client])
        (lws-reactor-pool-metrics (lws-http1-client-reactor client)))))
  )
