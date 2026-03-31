(library (chezpp net poll)
  (export make-poll-target
          poll-target?
          poll-target-resource
          poll-target-fd
          poll-target-events
          poll-target-ready-events
          poll
          poll/nonblocking)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private)
          (chezpp net socket))

  (define-record-type (poll-target %make-poll-target poll-target?)
    (sealed #t)
    (opaque #f)
    (fields (immutable resource poll-target-resource)
            (immutable fd poll-target-fd)
            (immutable events poll-target-events)
            (immutable ready-events poll-target-ready-events)))

  (define event-symbol->mask
    (lambda (who sym)
      (case sym
        [(read) (net-pollin)]
        [(write) (net-pollout)]
        [(priority) (net-pollpri)]
        [(error) (net-pollerr)]
        [(hup) (net-pollhup)]
        [(invalid) (net-pollnval)]
        [else (errorf who "invalid poll event ~s" sym)])))

  (define mask->event-list
    (lambda (mask)
      (let ([pairs `((read . ,(net-pollin))
                     (write . ,(net-pollout))
                     (priority . ,(net-pollpri))
                     (error . ,(net-pollerr))
                     (hup . ,(net-pollhup))
                     (invalid . ,(net-pollnval)))])
        (fold-right (lambda (entry acc)
                      (if (fx= 0 (fxlogand mask (cdr entry)))
                          acc
                          (cons (car entry) acc)))
                    '()
                    pairs))))

  (define event-list->mask
    (lambda (who event*)
      (unless (list? event*)
        (errorf who "poll events must be a list, given ~s" event*))
      (fold-left (lambda (acc ev)
                   (fxlogor acc (event-symbol->mask who ev)))
                 0
                 event*)))

  (define resource->fd
    (lambda (who resource)
      (cond
       [(socket? resource) (socket-fd resource)]
       [(fixnum? resource) resource]
       [else (errorf who "expected socket or file descriptor, given ~s" resource)])))

  (define target->spec
    (lambda (who target)
      (let ([v (make-vector 2 #f)])
        (vector-set! v 0 (poll-target-fd target))
        (vector-set! v 1 (event-list->mask who (poll-target-events target)))
        v)))

  #|proc:make-poll-target
The `make-poll-target` procedure constructs a poll target from a socket or file descriptor and a list of event symbols.
|#
  (define-who make-poll-target
    (lambda (resource event*)
      (let ([fd (resource->fd who resource)])
        (%make-poll-target resource fd event* '()))))

  #|proc:poll
The `poll` procedure waits for readiness across poll targets and returns updated targets with ready-event lists.
|#
  (define-who poll
    (case-lambda
      [(target*) (poll target* -1)]
      [(target* timeout-ms)
       (pcheck ([fixnum? timeout-ms])
               (unless (list? target*)
                 (errorf who "poll targets must be a list, given ~s" target*))
               (for-each (lambda (target)
                           (unless (poll-target? target)
                             (errorf who "poll target expected, given ~s" target)))
                         target*)
               (let ([ans (ffi-net-poll (map (lambda (target) (target->spec who target)) target*)
                                        timeout-ms)])
                 (when (ffi-error? ans)
                   (raise-net-error who 'poll (ffi-error-message ans) ans))
                 (map (lambda (target spec)
                        (%make-poll-target (poll-target-resource target)
                                           (poll-target-fd target)
                                           (poll-target-events target)
                                           (mask->event-list (vector-ref spec 2))))
                      target*
                      ans)))]))

  #|proc:poll/nonblocking
The `poll/nonblocking` procedure performs a zero-timeout poll and returns updated targets immediately.
|#
  (define-who poll/nonblocking
    (lambda (target*)
      (poll target* 0)))
  )
