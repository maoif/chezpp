(library (chezpp net lws reactor)
  (export lws-reactor?
          make-lws-reactor
          lws-reactor-state
          lws-reactor-owner-id
          lws-reactor-wakeup-fd
          lws-reactor-start!
          lws-reactor-shutdown!
          make-lws-reactor-operation
          lws-reactor-operation-events
          lws-reactor-drain-operation-events!
          lws-reactor-release-operation!
          lws-reactor-operation-lifecycle
          lws-reactor-register-waiter!
          lws-reactor-client-acquire!
          lws-reactor-client-release!
          lws-reactor-client-start!
          lws-reactor-submit-body!
          lws-reactor-consume-body!
          lws-reactor-close-stream!
          lws-reactor-inject-event!
          lws-reactor-inject-poll!
          lws-reactor-poll-snapshot
          lws-reactor-pool-metrics)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net ffi)
          (chezpp net operation)
          (chezpp net poll)
          (chezpp net lws ffi))

  (define-record-type (reactor-command %make-reactor-command reactor-command?)
    (sealed #t)
    (opaque #t)
    (fields (mutable next)
            (mutable tag)
            (mutable arguments)))

  (define-record-type (reactor-waiter %make-reactor-waiter reactor-waiter?)
    (sealed #t)
    (opaque #t)
    (fields (mutable next)
            (mutable operation)
            (mutable procedure)
            (mutable active?)))

  (define-record-type (reactor-operation-state %make-reactor-operation-state
                                               reactor-operation-state?)
    (sealed #t)
    (opaque #t)
    (fields (immutable connection-id)
            (immutable stream-id)
            (immutable generation)
            (immutable signal)
            (immutable poll-target)
            (immutable deadline-ms)
            (immutable retain?)
            (mutable lifecycle)
            (mutable events)
            (mutable result)
            (mutable failure)
            (mutable waiters)))

  (define-record-type (lws-reactor %make-lws-reactor %lws-reactor?)
    (sealed #t)
    (opaque #t)
    (fields (immutable native-context)
            (immutable mutex)
            (immutable condition)
            (immutable command-capacity)
            (immutable command-storage)
            (mutable command-free)
            (mutable command-head)
            (mutable command-tail)
            (mutable command-in-use)
            (mutable command-high-water)
            (mutable command-misses)
            (mutable command-exhaustions)
            (immutable waiter-capacity)
            (immutable waiter-storage)
            (mutable waiter-free)
            (mutable waiter-in-use)
            (mutable waiter-high-water)
            (mutable waiter-misses)
            (mutable operations)
            (mutable current-poll-snapshot)
            (mutable lifecycle)
            (mutable owner-thread-id)
            (mutable thread)
            (mutable failure)))

  (define lws-event-tag?
    (lambda (value)
      (and (symbol? value)
           (memq value '(connected headers readable writable complete closed failed
                                   reset goaway)))))

  (define lws-observed-protocol?
    (lambda (value)
      (or (fixnum? value) (memq value '(unknown http1 http2)))))

  (define lws-terminal-scope?
    (lambda (value)
      (or (fixnum? value) (memq value '(none stream connection)))))

  (define lws-poll-operation?
    (lambda (value)
      (and (symbol? value) (memq value '(add change delete)))))

  (define reactor-file-descriptor?
    (lambda (value)
      (and (fixnum? value) (fx>= value 0))))

  (define reactor?
    (lambda (value)
      (%lws-reactor? value)))

  (define initialize-command-pool!
    (lambda (reactor)
      (let ([storage (lws-reactor-command-storage reactor)])
        (let loop ([index 0] [free #f])
          (if (fx= index (vector-length storage))
              (lws-reactor-command-free-set! reactor free)
              (let ([command (%make-reactor-command free #f #f)])
                (vector-set! storage index command)
                (loop (fx1+ index) command)))))))

  (define initialize-waiter-pool!
    (lambda (reactor)
      (let ([storage (lws-reactor-waiter-storage reactor)])
        (let loop ([index 0] [free #f])
          (if (fx= index (vector-length storage))
              (lws-reactor-waiter-free-set! reactor free)
              (let ([waiter (%make-reactor-waiter free #f #f #f)])
                (vector-set! storage index waiter)
                (loop (fx1+ index) waiter)))))))

  (define make-network-condition
    (lambda (who message irritants)
      (condition (make-error)
                 (make-who-condition who)
                 (make-message-condition message)
                 (make-irritants-condition irritants))))

  (define find-operation-state-locked
    (lambda (reactor operation)
      (let ([entry (assq operation (lws-reactor-operations reactor))])
        (and entry (cdr entry)))))

  (define find-event-operation-locked
    (lambda (reactor connection-id stream-id generation)
      (let loop ([operation+state* (lws-reactor-operations reactor)])
        (and (pair? operation+state*)
             (let* ([operation+state (car operation+state*)]
                    [state (cdr operation+state)])
               (if (and (= connection-id
                           (reactor-operation-state-connection-id state))
                        (= stream-id (reactor-operation-state-stream-id state))
                        (= generation (reactor-operation-state-generation state)))
                   operation+state
                   (loop (cdr operation+state*))))))))

  (define command-acquire-locked!
    (lambda (reactor)
      (let ([command (lws-reactor-command-free reactor)])
        (if command
            (begin
              (lws-reactor-command-free-set! reactor (reactor-command-next command))
              (reactor-command-next-set! command #f)
              (lws-reactor-command-in-use-set!
               reactor (fx1+ (lws-reactor-command-in-use reactor)))
              (when (fx> (lws-reactor-command-in-use reactor)
                         (lws-reactor-command-high-water reactor))
                (lws-reactor-command-high-water-set!
                 reactor (lws-reactor-command-in-use reactor)))
              command)
            (begin
              (lws-reactor-command-misses-set!
               reactor (fx1+ (lws-reactor-command-misses reactor)))
              (lws-reactor-command-exhaustions-set!
               reactor (fx1+ (lws-reactor-command-exhaustions reactor)))
              #f)))))

  (define command-release-locked!
    (lambda (reactor command)
      (reactor-command-tag-set! command #f)
      (reactor-command-arguments-set! command #f)
      (reactor-command-next-set! command (lws-reactor-command-free reactor))
      (lws-reactor-command-free-set! reactor command)
      (lws-reactor-command-in-use-set!
       reactor (fx1- (lws-reactor-command-in-use reactor)))))

  (define enqueue-command!
    (lambda (reactor tag arguments)
      (let ([accepted?
             (with-mutex (lws-reactor-mutex reactor)
               (if (memq (lws-reactor-lifecycle reactor) '(stopping stopped))
                   #f
                   (let ([command (command-acquire-locked! reactor)])
                     (and command
                          (begin
                            (reactor-command-tag-set! command tag)
                            (reactor-command-arguments-set! command arguments)
                            (if (lws-reactor-command-tail reactor)
                                (reactor-command-next-set!
                                 (lws-reactor-command-tail reactor) command)
                                (lws-reactor-command-head-set! reactor command))
                            (lws-reactor-command-tail-set! reactor command)
                            (condition-signal (lws-reactor-condition reactor))
                            #t)))))])
        (when accepted?
          (lws-context-wakeup (lws-reactor-native-context reactor)))
        accepted?)))

  (define dequeue-command!
    (lambda (reactor)
      (with-mutex (lws-reactor-mutex reactor)
        (let ([command (lws-reactor-command-head reactor)])
          (when command
            (lws-reactor-command-head-set! reactor (reactor-command-next command))
            (when (not (lws-reactor-command-head reactor))
              (lws-reactor-command-tail-set! reactor #f))
            (reactor-command-next-set! command #f))
          command))))

  (define waiter-release-locked!
    (lambda (reactor waiter)
      (reactor-waiter-operation-set! waiter #f)
      (reactor-waiter-procedure-set! waiter #f)
      (reactor-waiter-active?-set! waiter #f)
      (reactor-waiter-next-set! waiter (lws-reactor-waiter-free reactor))
      (lws-reactor-waiter-free-set! reactor waiter)
      (lws-reactor-waiter-in-use-set!
       reactor (fx1- (lws-reactor-waiter-in-use reactor)))))

  (define release-operation-waiters-locked!
    (lambda (reactor state)
      (let loop ([waiter (reactor-operation-state-waiters state)] [procedure* '()])
        (if waiter
            (let ([next (reactor-waiter-next waiter)]
                  [procedure (and (reactor-waiter-active? waiter)
                                  (reactor-waiter-procedure waiter))])
              (waiter-release-locked! reactor waiter)
              (loop next (if procedure (cons procedure procedure*) procedure*)))
            (begin
              (reactor-operation-state-waiters-set! state #f)
              procedure*)))))

  (define terminal-event?
    (lambda (tag)
      (memq tag '(complete closed failed reset goaway))))

  (define publish-event!
    (lambda (reactor event)
      (let ([operation #f]
            [signal #f]
            [procedure* '()])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([entry (find-event-operation-locked
                        reactor (vector-ref event 2) (vector-ref event 3)
                        (vector-ref event 4))])
            (when entry
              (set! operation (car entry))
              (let* ([state (cdr entry)]
                     [tag (vector-ref event 0)])
                (when (eq? 'pending (reactor-operation-state-lifecycle state))
                  (set! signal (reactor-operation-state-signal state))
                  (reactor-operation-state-events-set!
                   state (cons event (reactor-operation-state-events state)))
                  (when (terminal-event? tag)
                    (if (eq? tag 'complete)
                        (begin
                          (reactor-operation-state-lifecycle-set! state 'completed)
                          (reactor-operation-state-result-set! state event))
                        (begin
                          (reactor-operation-state-lifecycle-set! state 'failed)
                          (reactor-operation-state-failure-set!
                           state
                           (make-network-condition
                            'lws-reactor tag
                            (list (vector-ref event 2) (vector-ref event 3)
                                  (vector-ref event 5))))))
                    (set! procedure*
                          (release-operation-waiters-locked! reactor state))))))))
        (for-each (lambda (procedure) (procedure operation)) procedure*)
        (when signal (lws-signal-notify signal)))))

  (define drain-native-events!
    (lambda (reactor)
      (let loop ()
        (let ([event (lws-context-next-event (lws-reactor-native-context reactor))])
          (when event
            (publish-event! reactor event)
            (loop))))))

  (define process-command!
    (lambda (reactor command)
      (let ([tag (reactor-command-tag command)]
            [arguments (reactor-command-arguments command)]
            [result #t])
        (set! result (case tag
          [(inject-event)
           (apply lws-context-inject-event!
                  (lws-reactor-native-context reactor) arguments)]
          [(inject-poll)
           (apply lws-context-inject-poll!
                  (lws-reactor-native-context reactor) arguments)]
          [(start)
           (apply lws-client-start
                (lws-reactor-native-context reactor) arguments)]
          [(acquire)
           (apply lws-client-acquire
                  (lws-reactor-native-context reactor) arguments)]
          [(release)
           (apply lws-client-release
                  (lws-reactor-native-context reactor) arguments)]
          [(submit-body)
           (apply lws-client-body-submit
                  (lws-reactor-native-context reactor) arguments)]
          [(consume-body)
           (apply lws-body-consumed
                  (lws-reactor-native-context reactor) arguments)]
          [(cancel)
           (apply lws-stream-cancel
                  (lws-reactor-native-context reactor) arguments)]
          [else #t]))
        ;; Command acceptance only means the command entered the queue. A rejected native
        ;; command must become a deterministic terminal failure for its owning operation.
        (when (and (eq? result #f) (>= (length arguments) 3))
          (publish-event!
           reactor
           (vector 'failed 0 (car arguments) (cadr arguments) (caddr arguments)
                   -1 (make-bytevector 0) (vector 'unknown #f #f 'stream))))
        (with-mutex (lws-reactor-mutex reactor)
          (command-release-locked! reactor command))
        (drain-native-events! reactor))))

  (define drain-commands!
    (lambda (reactor)
      (let loop ()
        (let ([command (dequeue-command! reactor)])
          (when command
            (process-command! reactor command)
            (loop))))))

  (define ready-event-mask
    (lambda (event*)
      (fold-left
       (lambda (mask event)
         (fxior mask
                 (case event
                   [(read) (net-pollin)]
                   [(write) (net-pollout)]
                   [(priority) (net-pollpri)]
                   [(error) (net-pollerr)]
                   [(hup) (net-pollhup)]
                   [(invalid) (net-pollnval)]
                   [else 0])))
       0
       event*)))

  (define requested-event-list
    (lambda (mask)
      (append (if (fxzero? (fxlogand mask (net-pollin))) '() '(read))
              (if (fxzero? (fxlogand mask (net-pollout))) '() '(write))
              (if (fxzero? (fxlogand mask (net-pollpri))) '() '(priority)))))

  (define service-poll-pass!
    (lambda (reactor)
      (let* ([context (lws-reactor-native-context reactor)]
             [snapshot (lws-context-poll-snapshot context)]
             [timeout-ms (lws-context-timeout-ms context 50)])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-current-poll-snapshot-set! reactor snapshot))
        (if (fxzero? timeout-ms)
            (lws-context-service-fd context -1 0)
            (let* ([target*
                    (cons (make-poll-target (lws-context-wakeup-fd context) '(read))
                          (map (lambda (item)
                                 (make-poll-target
                                  (vector-ref item 0)
                                  (requested-event-list (vector-ref item 1))))
                               (vector->list snapshot)))]
                   [ready*
                    (filter (lambda (target)
                              (pair? (poll-target-ready-events target)))
                            (poll target* (if (fxnegative? timeout-ms) 50 timeout-ms)))])
              (if (null? ready*)
                  (lws-context-service-fd context -1 0)
                  (for-each
                   (lambda (target)
                     (lws-context-service-fd
                      context (poll-target-fd target)
                      (ready-event-mask (poll-target-ready-events target))))
                   ready*))))
        (drain-native-events! reactor))))

  (define fail-pending-operations!
    (lambda (reactor failure)
      (let ([notification* '()])
        (with-mutex (lws-reactor-mutex reactor)
          (for-each
           (lambda (entry)
             (let ([state (cdr entry)])
               (when (eq? 'pending (reactor-operation-state-lifecycle state))
                 (reactor-operation-state-lifecycle-set! state 'failed)
                 (reactor-operation-state-failure-set! state failure)
                 ;; Close the per-operation native signal before the context is destroyed.
                 ;; `lws-signal-close` is idempotent; terminal cleanup may call it again.
                 (lws-signal-close (reactor-operation-state-signal state))
                 (set! notification*
                       (cons (cons (car entry)
                                   (release-operation-waiters-locked! reactor state))
                             notification*)))))
           (lws-reactor-operations reactor)))
        (for-each
         (lambda (notification)
           (for-each (lambda (procedure) (procedure (car notification)))
                     (cdr notification)))
         notification*))))

  (define reactor-main
    (lambda (reactor)
      (guard (failure
              [else
               (with-mutex (lws-reactor-mutex reactor)
                 (lws-reactor-failure-set! reactor failure)
                 (lws-reactor-lifecycle-set! reactor 'stopping))])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-owner-thread-id-set! reactor (get-thread-id))
          (when (eq? 'starting (lws-reactor-lifecycle reactor))
            (lws-reactor-lifecycle-set! reactor 'running))
          (condition-broadcast (lws-reactor-condition reactor)))
        (let loop ()
          (drain-commands! reactor)
          (unless (eq? 'stopping (lws-reactor-lifecycle reactor))
            (service-poll-pass! reactor)
            (loop))))
      (let ([failure
             (or (lws-reactor-failure reactor)
                 (make-network-condition 'lws-reactor-shutdown!
                                         "reactor stopped" '()))])
        (fail-pending-operations! reactor failure))
      (lws-context-close (lws-reactor-native-context reactor))
      (with-mutex (lws-reactor-mutex reactor)
        (lws-reactor-lifecycle-set! reactor 'stopped)
        (condition-broadcast (lws-reactor-condition reactor)))))

  #|proc:lws-reactor?
The `lws-reactor?` procedure returns whether `value` is a private LWS reactor.
The `value` parameter may be any Scheme object.
|#
  (define lws-reactor?
    (lambda (value)
      (pcheck () (reactor? value))))

  #|proc:make-lws-reactor
The `make-lws-reactor` procedure creates an unstarted serialized LWS reactor.
`event-capacity` bounds native events, `payload-capacity` bounds copied payloads, and
`command-capacity` bounds queued commands and operation waiters. `tls-context-handle` is zero or a
native TLS context. `proxy-address` and `proxy-port` select the context proxy. It returns the
reactor.
|#
  (define make-lws-reactor
    (case-lambda
      [(event-capacity payload-capacity command-capacity)
       (make-lws-reactor event-capacity payload-capacity command-capacity 0 "" 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle)
       (make-lws-reactor event-capacity payload-capacity command-capacity
                         tls-context-handle "" 0)]
      [(event-capacity payload-capacity command-capacity tls-context-handle
                       proxy-address proxy-port)
       (pcheck ([positive-natural? event-capacity payload-capacity command-capacity]
                [natural? tls-context-handle proxy-port]
                [string? proxy-address])
         (let ([context (lws-context-open event-capacity payload-capacity tls-context-handle
                                          proxy-address proxy-port)])
           (let ([reactor
                  (%make-lws-reactor
                   context (make-mutex 'lws-reactor) (make-condition 'lws-reactor)
                   command-capacity (make-vector command-capacity) #f #f #f 0 0 0 0
                   command-capacity (make-vector command-capacity) #f 0 0 0
                   '() '#() 'created 0 #f #f)])
             (initialize-command-pool! reactor)
             (initialize-waiter-pool! reactor)
             reactor)))]))

  #|proc:lws-reactor-state
The `lws-reactor-state` procedure returns the lifecycle symbol for `reactor`.
The result is `created`, `starting`, `running`, `stopping`, or `stopped`.
|#
  (define lws-reactor-state
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-lifecycle reactor)))))

  #|proc:lws-reactor-owner-id
The `lws-reactor-owner-id` procedure returns the owner thread id for `reactor`.
It returns zero before the owner thread starts.
|#
  (define lws-reactor-owner-id
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-owner-thread-id reactor)))))

  #|proc:lws-reactor-wakeup-fd
The `lws-reactor-wakeup-fd` procedure returns the wakeup descriptor owned by `reactor`.
|#
  (define lws-reactor-wakeup-fd
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (lws-context-wakeup-fd (lws-reactor-native-context reactor)))))

  #|proc:lws-reactor-start!
The `lws-reactor-start!` procedure starts the owner thread for `reactor`.
It returns `#t` when started and `#f` when the reactor was already started.
|#
  (define lws-reactor-start!
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (if (eq? 'created (lws-reactor-lifecycle reactor))
              (begin
                (lws-reactor-lifecycle-set! reactor 'starting)
                (lws-reactor-thread-set!
                 reactor (fork-thread (lambda () (reactor-main reactor))))
                #t)
              #f)))))

  #|proc:lws-reactor-shutdown!
The `lws-reactor-shutdown!` procedure stops `reactor`, joins its owner, and releases its context.
Repeated calls are inert. The return value is `reactor`.
|#
  (define lws-reactor-shutdown!
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (let ([state (lws-reactor-state reactor)])
          (cond
           [(eq? state 'created)
            (with-mutex (lws-reactor-mutex reactor)
              (lws-reactor-lifecycle-set! reactor 'stopping))
            (fail-pending-operations!
             reactor
             (make-network-condition 'lws-reactor-shutdown!
                                     "reactor stopped" '()))
            (lws-context-close (lws-reactor-native-context reactor))
            (with-mutex (lws-reactor-mutex reactor)
              (lws-reactor-lifecycle-set! reactor 'stopped))]
           [(memq state '(starting running))
            (with-mutex (lws-reactor-mutex reactor)
              (lws-reactor-lifecycle-set! reactor 'stopping))
            (lws-context-wakeup (lws-reactor-native-context reactor))
            (thread-join (lws-reactor-thread reactor))]
           [(eq? state 'stopping)
            (thread-join (lws-reactor-thread reactor))]
           [else (void)]))
        reactor)))

  #|proc:make-lws-reactor-operation
The `make-lws-reactor-operation` procedure registers a pending operation with `reactor`.
`kind` identifies the operation. The three identity parameters route copied native events.
`deadline-ms` is an absolute monotonic deadline or `#f`. The optional `retain?` keeps routing state
until `lws-reactor-release-operation!` is called. It returns a network operation.
|#
  (define make-lws-reactor-operation
    (case-lambda
      [(reactor kind connection-id stream-id generation deadline-ms)
       (make-lws-reactor-operation reactor kind connection-id stream-id generation
                                   deadline-ms #f)]
      [(reactor kind connection-id stream-id generation deadline-ms retain?)
       (pcheck ([reactor? reactor] [symbol? kind]
                [natural? connection-id stream-id generation]
                [(lambda (value) (or (not value) (natural? value))) deadline-ms]
                [boolean? retain?])
        (when (memq (lws-reactor-state reactor) '(stopping stopped))
          (errorf 'make-lws-reactor-operation "reactor is shutting down"))
        (let* ([signal (lws-signal-open (lws-reactor-native-context reactor))]
               [state
                (begin
                  (when (zero? signal)
                    (errorf 'make-lws-reactor-operation
                            "reactor operation signal pool is exhausted"))
                  (%make-reactor-operation-state
                   connection-id stream-id generation signal
                   (make-poll-target (lws-signal-fd signal) '(read))
                   deadline-ms retain? 'pending '() #f #f #f))]
              [operation #f])
          (set! operation
                (make-net-operation
                 kind
                 (lambda ()
                   (lws-signal-drain signal)
                   (with-mutex (lws-reactor-mutex reactor)
                     (case (reactor-operation-state-lifecycle state)
                       [(pending)
                        (net-operation-pending
                         (list (reactor-operation-state-poll-target state))
                         (reactor-operation-state-deadline-ms state))]
                       [(completed)
                        (net-operation-completed
                         (reactor-operation-state-result state))]
                       [else
                        (net-operation-failed
                         (reactor-operation-state-failure state))])))
                 (lambda ()
                   (with-mutex (lws-reactor-mutex reactor)
                     (reactor-operation-state-lifecycle-set! state 'cancelled)
                     (reactor-operation-state-failure-set!
                      state (make-network-condition kind "operation cancelled" '())))
                   (enqueue-command!
                    reactor 'cancel
                    (list connection-id stream-id generation 0)))
                 (lambda ()
                   (lws-signal-close signal)
                   (with-mutex (lws-reactor-mutex reactor)
                     (release-operation-waiters-locked! reactor state)
                     (unless (reactor-operation-state-retain? state)
                       (lws-reactor-operations-set!
                        reactor
                        (remp (lambda (entry) (eq? operation (car entry)))
                              (lws-reactor-operations reactor))))))))
          (with-mutex (lws-reactor-mutex reactor)
            (lws-reactor-operations-set!
             reactor (cons (cons operation state) (lws-reactor-operations reactor))))
          operation))]))

  #|proc:lws-reactor-release-operation!
The `lws-reactor-release-operation!` procedure removes retained `operation` routing state from
`reactor`. The operation must be terminal. The return value is unspecified.
|#
  (define-who lws-reactor-release-operation!
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (when state
              (when (eq? 'pending (reactor-operation-state-lifecycle state))
                (errorf who "cannot release a pending operation"))
              (release-operation-waiters-locked! reactor state)
              (lws-reactor-operations-set!
               reactor
               (remp (lambda (entry) (eq? operation (car entry)))
                     (lws-reactor-operations reactor)))))))))

  #|proc:lws-reactor-operation-events
The `lws-reactor-operation-events` procedure returns copied events received for `operation`.
The `reactor` parameter owns `operation`. Events are returned in callback order.
|#
  (define-who lws-reactor-operation-events
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (unless state (errorf who "operation is not owned by reactor"))
            (reverse (reactor-operation-state-events state)))))))

  #|proc:lws-reactor-drain-operation-events!
The `lws-reactor-drain-operation-events!` procedure destructively removes and returns all copied
events currently queued for `operation`. The reactor retains no payload after this call.
|#
  (define-who lws-reactor-drain-operation-events!
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (unless state (errorf who "operation is not owned by reactor"))
            (let ([events (reverse (reactor-operation-state-events state))])
              (reactor-operation-state-events-set! state '())
              events))))))

  #|proc:lws-reactor-operation-lifecycle
The `lws-reactor-operation-lifecycle` procedure returns the internal lifecycle of `operation`.
The `reactor` parameter owns `operation`.
|#
  (define-who lws-reactor-operation-lifecycle
    (lambda (reactor operation)
      (pcheck ([reactor? reactor] [net-operation? operation])
        (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)])
            (unless state (errorf who "operation is not owned by reactor"))
            (reactor-operation-state-lifecycle state))))))

  #|proc:lws-reactor-register-waiter!
The `lws-reactor-register-waiter!` procedure registers `procedure` for `operation` completion.
`procedure` has signature `(net-operation) -> unspecified`. The `reactor` owns the operation.
It returns an unregister procedure with signature `() -> unspecified`, or `#f` on exhaustion.
|#
  (define-who lws-reactor-register-waiter!
    (lambda (reactor operation procedure)
      (pcheck ([reactor? reactor] [net-operation? operation] [procedure? procedure])
        (let ([notify? #f]
              [unregister #f])
          (with-mutex (lws-reactor-mutex reactor)
          (let ([state (find-operation-state-locked reactor operation)]
                [waiter (lws-reactor-waiter-free reactor)])
            (unless state (errorf who "operation is not owned by reactor"))
            (cond
             [(not (eq? 'pending (reactor-operation-state-lifecycle state)))
              (set! notify? #t)
              (set! unregister void)]
             [(not waiter)
                (begin
                  (lws-reactor-waiter-misses-set!
                   reactor (fx1+ (lws-reactor-waiter-misses reactor)))
                  (set! unregister #f))]
             [else
                  (lws-reactor-waiter-free-set! reactor (reactor-waiter-next waiter))
                  (reactor-waiter-operation-set! waiter operation)
                  (reactor-waiter-procedure-set! waiter procedure)
                  (reactor-waiter-active?-set! waiter #t)
                  (reactor-waiter-next-set!
                   waiter (reactor-operation-state-waiters state))
                  (reactor-operation-state-waiters-set! state waiter)
                  (lws-reactor-waiter-in-use-set!
                   reactor (fx1+ (lws-reactor-waiter-in-use reactor)))
                  (when (fx> (lws-reactor-waiter-in-use reactor)
                             (lws-reactor-waiter-high-water reactor))
                    (lws-reactor-waiter-high-water-set!
                     reactor (lws-reactor-waiter-in-use reactor)))
                  (set!
                   unregister
                   (lambda ()
                     (with-mutex (lws-reactor-mutex reactor)
                       (when (reactor-waiter-active? waiter)
                         (let remove ([current (reactor-operation-state-waiters state)]
                                      [previous #f])
                           (cond
                            [(not current) (void)]
                            [(eq? current waiter)
                             (if previous
                                 (reactor-waiter-next-set!
                                  previous (reactor-waiter-next current))
                                 (reactor-operation-state-waiters-set!
                                  state (reactor-waiter-next current)))
                             (waiter-release-locked! reactor current)]
                            [else
                             (remove (reactor-waiter-next current) current)]))))))])))
          (when notify? (procedure operation))
          unregister))))

  #|proc:lws-reactor-client-acquire!
The `lws-reactor-client-acquire!` procedure reserves a logical stream lease on the reactor's
serialized owner. It returns whether the bounded command was accepted.
|#
  (define lws-reactor-client-acquire!
    (lambda (reactor connection-id stream-id generation)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation])
        (enqueue-command! reactor 'acquire
                          (list connection-id stream-id generation)))))

  #|proc:lws-reactor-client-release!
The `lws-reactor-client-release!` procedure queues release of a terminal stream lease. It returns
whether the bounded command was accepted.
|#
  (define lws-reactor-client-release!
    (lambda (reactor connection-id stream-id generation)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation])
        (enqueue-command! reactor 'release
                          (list connection-id stream-id generation)))))

  #|proc:lws-reactor-client-start!
The `lws-reactor-client-start!` procedure queues an HTTP client start command on `reactor`.
The identity and generation parameters route events. Address, port, TLS, method, host, and path
parameters configure the LWS request. It returns whether the bounded command pool accepted it.
|#
  (define lws-reactor-client-start!
    (case-lambda
      [(reactor connection-id stream-id generation address port tls? method host path)
       (lws-reactor-client-start! reactor connection-id stream-id generation address port tls?
                                   method host path #vu8() #vu8() #f "http/1.1")]
      [(reactor connection-id stream-id generation address port tls? method host path headers
                initial-body has-body?)
       (lws-reactor-client-start! reactor connection-id stream-id generation address port tls?
                                   method host path headers initial-body has-body? "http/1.1")]
      [(reactor connection-id stream-id generation address port tls? method host path headers
                initial-body has-body? alpn)
       (pcheck ([reactor? reactor] [natural? connection-id stream-id generation]
                [string? address method host path] [fixnum? port] [boolean? tls?]
                [bytevector? headers initial-body] [boolean? has-body?] [string? alpn])
         (enqueue-command!
          reactor 'start
          (list connection-id stream-id generation address port tls? method host path headers
                initial-body has-body? alpn)))]))

  #|proc:lws-reactor-submit-body!
The `lws-reactor-submit-body!` procedure queues one request body `payload` on `reactor`.
The identity and generation parameters select the stream. `final?` marks the last chunk.
It returns whether the bounded command pool accepted the command.
|#
  (define lws-reactor-submit-body!
    (lambda (reactor connection-id stream-id generation payload final?)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation]
               [bytevector? payload] [boolean? final?])
        (enqueue-command! reactor 'submit-body
                          (list connection-id stream-id generation payload final?)))))

  #|proc:lws-reactor-consume-body!
The `lws-reactor-consume-body!` procedure acknowledges `byte-count` response bytes on `reactor`.
The identity and generation parameters select the stream. It returns command acceptance.
|#
  (define lws-reactor-consume-body!
    (lambda (reactor connection-id stream-id generation byte-count)
      (pcheck ([reactor? reactor]
               [natural? connection-id stream-id generation byte-count])
        (enqueue-command! reactor 'consume-body
                          (list connection-id stream-id generation byte-count)))))

  #|proc:lws-reactor-close-stream!
The `lws-reactor-close-stream!` procedure queues cancellation of a stream on `reactor`.
The identity and generation parameters select the stream, and `status` describes the reason.
It returns whether the bounded command pool accepted the command.
|#
  (define lws-reactor-close-stream!
    (lambda (reactor connection-id stream-id generation status)
      (pcheck ([reactor? reactor] [natural? connection-id stream-id generation]
               [fixnum? status])
        (enqueue-command! reactor 'cancel
                          (list connection-id stream-id generation status)))))

  #|proc:lws-reactor-inject-event!
The `lws-reactor-inject-event!` procedure queues a copied fake native event on `reactor`.
The identity, generation, `status`, and bytevector `payload` parameters form the event.
It returns whether the bounded command pool accepted the command.
|#
  (define lws-reactor-inject-event!
    (case-lambda
      [(reactor event-tag connection-id stream-id generation status payload)
       (lws-reactor-inject-event! reactor event-tag connection-id stream-id generation status
                                   payload 0 #f 0 #f 0)]
      [(reactor event-tag connection-id stream-id generation status payload protocol reusable?
                peer-h2-capacity peer-h2-capacity-known terminal-scope)
       (pcheck ([reactor? reactor] [lws-event-tag? event-tag]
                [natural? connection-id stream-id generation]
                [fixnum? status peer-h2-capacity]
                [lws-observed-protocol? protocol]
                [lws-terminal-scope? terminal-scope]
                [bytevector? payload]
                [boolean? reusable? peer-h2-capacity-known])
         (enqueue-command! reactor 'inject-event
                           (list event-tag connection-id stream-id generation status payload
                                 protocol reusable? peer-h2-capacity peer-h2-capacity-known
                                 terminal-scope)))]))

  #|proc:lws-reactor-inject-poll!
The `lws-reactor-inject-poll!` procedure queues fake poll `operation` for `descriptor`.
The `events` parameter is the native poll mask. It returns whether the command was accepted.
|#
  (define lws-reactor-inject-poll!
    (lambda (reactor operation descriptor events)
      (pcheck ([reactor? reactor] [lws-poll-operation? operation]
               [reactor-file-descriptor? descriptor] [fixnum? events])
        (enqueue-command! reactor 'inject-poll (list operation descriptor events)))))

  #|proc:lws-reactor-poll-snapshot
The `lws-reactor-poll-snapshot` procedure returns the latest native poll snapshot for `reactor`.
|#
  (define lws-reactor-poll-snapshot
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (lws-reactor-current-poll-snapshot reactor)))))

  #|proc:lws-reactor-pool-metrics
The `lws-reactor-pool-metrics` procedure returns command and waiter pool metrics for `reactor`.
The vector contains command capacity, in-use, high-water, misses, exhaustions, then waiter
capacity, in-use, high-water, and misses.
|#
  (define lws-reactor-pool-metrics
    (lambda (reactor)
      (pcheck ([reactor? reactor])
        (with-mutex (lws-reactor-mutex reactor)
          (vector (lws-reactor-command-capacity reactor)
                  (lws-reactor-command-in-use reactor)
                  (lws-reactor-command-high-water reactor)
                  (lws-reactor-command-misses reactor)
                  (lws-reactor-command-exhaustions reactor)
                  (lws-reactor-waiter-capacity reactor)
                  (lws-reactor-waiter-in-use reactor)
                  (lws-reactor-waiter-high-water reactor)
                  (lws-reactor-waiter-misses reactor))))))
  )
