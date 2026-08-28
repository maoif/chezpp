(library (chezpp net lws http2)
  (export make-lws-http2-client
          lws-http2-client-close!
          lws-http2-request/nonblocking
          lws-http2-release-operation!
          lws-http2-client-pool-metrics)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net operation)
          (chezpp net poll)
          (chezpp net lws http1))

  (define-record-type (lws-http2-client %make-lws-http2-client lws-http2-client?)
    (sealed #t)
    (opaque #t)
    (fields (immutable transport lws-http2-client-transport)
            (mutable origin* lws-http2-client-origin* lws-http2-client-origin*-set!)
            (mutable closed? lws-http2-client-closed? lws-http2-client-closed?-set!)))

  (define-record-type (h2-origin %make-h2-origin h2-origin?)
    (sealed #t)
    (opaque #t)
    (fields (immutable key h2-origin-key)
            (mutable ready? h2-origin-ready? h2-origin-ready?-set!)
            (mutable failed h2-origin-failed h2-origin-failed-set!)
            (mutable queued h2-origin-queued h2-origin-queued-set!)
            (mutable active h2-origin-active h2-origin-active-set!)))

  (define-record-type (h2-stream %make-h2-stream h2-stream?)
    (sealed #t)
    (opaque #t)
    (fields (immutable origin h2-stream-origin)
            (immutable request h2-stream-request)
            (immutable sink h2-stream-sink)
            (mutable inner h2-stream-inner h2-stream-inner-set!)
            (mutable cancelled? h2-stream-cancelled? h2-stream-cancelled?-set!)))

  (define h2-peer-capacity 10)

  (define make-failure
    (lambda (who message irritants)
      (condition (make-error) (make-who-condition who)
                 (make-message-condition message)
                 (make-irritants-condition irritants))))

  (define request-origin
    (lambda (request)
      (list (vector-ref request 1) (vector-ref request 2) (vector-ref request 3))))

  (define find-origin
    (lambda (client key)
      (find (lambda (origin) (equal? key (h2-origin-key origin)))
            (lws-http2-client-origin* client))))

  (define ensure-origin!
    (lambda (client request)
      (let ([key (request-origin request)])
        (or (find-origin client key)
            (let ([origin (%make-h2-origin key #f #f '() '())])
              (lws-http2-client-origin*-set!
               client (cons origin (lws-http2-client-origin* client)))
              origin)))))

  (define remove-eq
    (lambda (value value*)
      (remp (lambda (item) (eq? item value)) value*)))

  (define start-stream!
    (lambda (client stream)
      (let ([origin (h2-stream-origin stream)])
        (h2-stream-inner-set!
         stream
         (lws-http1-request/nonblocking
          (lws-http2-client-transport client) (h2-stream-request stream)
          (h2-stream-sink stream)
          (lambda () (h2-origin-ready?-set! origin #t))))
        (h2-origin-active-set! origin
                               (append (h2-origin-active origin) (list stream))))))

  (define promote-streams!
    (lambda (client origin)
      (when (and (h2-origin-ready? origin) (not (h2-origin-failed origin)))
        (let loop ()
          (when (and (pair? (h2-origin-queued origin))
                     (< (length (h2-origin-active origin)) h2-peer-capacity))
            (let ([stream (car (h2-origin-queued origin))])
              (h2-origin-queued-set! origin (cdr (h2-origin-queued origin)))
              (unless (h2-stream-cancelled? stream) (start-stream! client stream))
              (loop)))))))

  (define advance-origin!
    (lambda (client origin)
      (for-each
       (lambda (stream)
         (let ([inner (h2-stream-inner stream)])
           (when (and inner (eq? 'pending (net-operation-state inner)))
             (net-operation-step! inner))))
       (h2-origin-active origin))
      (let ([terminal*
             (filter (lambda (stream)
                       (let ([inner (h2-stream-inner stream)])
                         (and inner (not (eq? 'pending (net-operation-state inner))))) )
                     (h2-origin-active origin))])
        (when (pair? terminal*)
          (h2-origin-active-set! origin
                                 (fold-left (lambda (active stream)
                                              (remove-eq stream active))
                                            (h2-origin-active origin) terminal*))))
      (when (and (not (h2-origin-ready? origin))
                 (null? (h2-origin-active origin)))
        (h2-origin-failed-set!
         origin (make-failure 'lws-http2 "HTTP/2 leader terminated before H2 readiness"
                              (h2-origin-key origin))))
      (promote-streams! client origin)))

  (define origin-targets
    (lambda (origin deadline-ms)
      (let loop ([stream* (h2-origin-active origin)])
        (if (null? stream*)
            '()
            (let ([inner (h2-stream-inner (car stream*))])
              (if inner
                  (net-operation-poll-targets inner)
                  (loop (cdr stream*))))))))

  #|proc:make-lws-http2-client
The `make-lws-http2-client` procedure creates a multiplexed HTTP/2 client transport.
The capacity parameters bound native event, payload, and reactor command pools.
`tls-context-handle` is zero or a native TLS context handle. The return value is a client.
|#
  (define make-lws-http2-client
    (case-lambda
      [(event-capacity payload-capacity command-capacity)
       (make-lws-http2-client event-capacity payload-capacity command-capacity 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle)
       (pcheck ([positive-natural? event-capacity payload-capacity command-capacity]
                [natural? tls-context-handle])
         (%make-lws-http2-client
          (make-lws-http1-client event-capacity payload-capacity command-capacity
                                 tls-context-handle)
          '() #f))]))

  #|proc:lws-http2-request/nonblocking
The `lws-http2-request/nonblocking` procedure enqueues normalized `request` on `client`.
The request vector contains HTTP request fields, `h2` ALPN policy, and an absolute deadline.
`response-sink` is `#f` or a write/finish procedure vector. It returns a network operation.
|#
  (define-who lws-http2-request/nonblocking
    (lambda (client request response-sink)
      (pcheck ([lws-http2-client? client] [vector? request]
               [(lambda (value) (or (not value) (vector? value))) response-sink])
        (when (lws-http2-client-closed? client)
          (errorf who "HTTP/2 client transport is closed"))
        (unless (and (= (vector-length request) 10)
                     (string=? (vector-ref request 8) "h2"))
          (errorf who "expected a ten-element normalized HTTP/2 request vector"))
        (let* ([origin (ensure-origin! client request)]
               [stream (%make-h2-stream origin request response-sink #f #f)]
               [deadline-ms (vector-ref request 9)])
          (if (null? (h2-origin-active origin))
              (start-stream! client stream)
              (h2-origin-queued-set! origin
                                     (append (h2-origin-queued origin) (list stream))))
          (make-net-operation
           'http2
           (lambda ()
             (advance-origin! client origin)
             (let ([inner (h2-stream-inner stream)])
               (cond
                [(h2-stream-cancelled? stream)
                 (net-operation-failed
                  (make-failure who "HTTP/2 stream was cancelled" request))]
                [(h2-origin-failed origin)
                 (net-operation-failed (h2-origin-failed origin))]
                [(not inner)
                 (net-operation-pending (origin-targets origin deadline-ms) deadline-ms)]
                [(eq? 'completed (net-operation-state inner))
                 (net-operation-completed (net-operation-result inner))]
                [(memq (net-operation-state inner) '(failed cancelled))
                 (net-operation-failed (net-operation-condition inner))]
                [else
                 (net-operation-pending (net-operation-poll-targets inner) deadline-ms)])))
           (lambda ()
             (h2-stream-cancelled?-set! stream #t)
             (let ([inner (h2-stream-inner stream)])
               (if inner
                   (net-operation-cancel! inner)
                   (h2-origin-queued-set! origin
                                          (remove-eq stream (h2-origin-queued origin)))))
             (promote-streams! client origin))
           (lambda ()
             (let ([inner (h2-stream-inner stream)])
               (when inner
                 (lws-http1-release-operation!
                  (lws-http2-client-transport client) inner)))))))))

  #|proc:lws-http2-release-operation!
The `lws-http2-release-operation!` procedure releases terminal `operation` owned by `client`.
The return value is unspecified.
|#
  (define lws-http2-release-operation!
    (lambda (client operation)
      (pcheck ([lws-http2-client? client] [net-operation? operation])
        (void))))

  #|proc:lws-http2-client-close!
The `lws-http2-client-close!` procedure closes `client` and returns the same client.
|#
  (define lws-http2-client-close!
    (lambda (client)
      (pcheck ([lws-http2-client? client])
        (unless (lws-http2-client-closed? client)
          (lws-http2-client-closed?-set! client #t)
          (lws-http1-client-close! (lws-http2-client-transport client))
          (lws-http2-client-origin*-set! client '()))
        client)))

  #|proc:lws-http2-client-pool-metrics
The `lws-http2-client-pool-metrics` procedure returns transport pool metrics for `client`.
|#
  (define lws-http2-client-pool-metrics
    (lambda (client)
      (pcheck ([lws-http2-client? client])
        (lws-http1-client-pool-metrics (lws-http2-client-transport client)))))
  )
