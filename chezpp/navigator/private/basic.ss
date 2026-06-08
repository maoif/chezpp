(library (chezpp navigator private basic)
  (export nav-stay nav-none nav-all nav-values nav-keys nav-entries
          nav-nth nav-nth/default nav-first nav-second nav-last nav-slice
          nav-key nav-key/default nav-key-values nav-submap nav-car nav-cdr)
  (import (chezscheme)
          (chezpp utils)
          (chezpp navigator private core)
          (chezpp navigator private data))

  (define unsupported-transform!
    (lambda (who nav value)
      (nav-unsupported-error who nav value 'transform!)))

  (define make-basic-nav
    (lambda (name metadata select-proc transform-proc transform!-proc clear-proc clear!-proc)
      (letrec ([nav (make-$navigator name 'basic metadata
                                     select-proc
                                     transform-proc
                                     transform!-proc
                                     clear-proc
                                     clear!-proc)])
        nav)))

  (define unsupported-clear
    (lambda (who value)
      (nav-error who "focused value cannot be removed from: ~s" value)))

  (define index-list
    (lambda (len)
      (let loop ([i 0] [acc '()])
        (if (= i len)
            (reverse acc)
            (loop (+ i 1) (cons i acc))))))

  (define indexed-transform
    (lambda (value indexes update)
      (cond [(list? value)
             (let loop ([xs value] [i 0])
               (cond [(null? xs) '()]
                     [(memv i indexes)
                      (cons (update (car xs)) (loop (cdr xs) (+ i 1)))]
                     [else (cons (car xs) (loop (cdr xs) (+ i 1)))]))]
            [(vector? value)
             (let ([copy (vector-copy value)])
               (for-each (lambda (i)
                           (vector-set! copy i (update (vector-ref value i))))
                         indexes)
               copy)]
            [(string? value)
             (let ([copy (string-copy value)])
               (for-each (lambda (i)
                           (let ([new-char (update (string-ref value i))])
                             (if (char? new-char)
                                 (string-set! copy i new-char)
                                 (nav-error 'nav-transform
                                            "string element is not a char: ~s"
                                            new-char))))
                         indexes)
               copy)]
            [(bytevector? value)
             (let ([copy (bytevector-copy value)])
               (for-each (lambda (i)
                           (let ([new-byte (update (bytevector-u8-ref value i))])
                             (if (byte? new-byte)
                                 (bytevector-u8-set! copy i new-byte)
                                 (nav-error 'nav-transform
                                            "bytevector element is not a byte: ~s"
                                            new-byte))))
                         indexes)
               copy)]
            [else (nav-error 'nav-transform "unsupported indexed value: ~s" value)])))

  (define indexed-transform!
    (lambda (value indexes update!)
      (for-each
       (lambda (index)
         (let ([old (indexed-ref/missing value index)])
           (if (nav-missing? old)
               (nav-error 'nav-transform! "index ~s out of range for: ~s" index value)
               (indexed-set! value index (update! old)))))
       indexes)
      value))

  (define indexed-transform/default
    (lambda (value index default update)
      (let ([selected (indexed-ref/missing value index)])
        (cond [(not (nav-missing? selected))
               (indexed-transform value (list index) update)]
              [(list? value)
               (let loop ([xs value] [i 0])
                 (cond [(pair? xs)
                        (cons (car xs) (loop (cdr xs) (+ i 1)))]
                       [(= i index)
                        (list (update default))]
                       [else
                        (cons default (loop '() (+ i 1)))]))]
              [(vector? value)
               (let ([copy (make-vector (+ index 1) default)]
                     [len (vector-length value)])
                 (let loop ([i 0])
                   (when (< i len)
                     (vector-set! copy i (vector-ref value i))
                     (loop (+ i 1))))
                 (vector-set! copy index (update default))
                 copy)]
              [else
               (nav-error 'nav-transform "cannot update missing index ~s in: ~s"
                          index value)]))))

  (define emit-indexed-values
    (lambda (value emit)
      (cond [(list? value) (for-each emit value)]
            [(vector? value)
             (let ([len (vector-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (vector-ref value i))
                   (loop (+ i 1)))))]
            [(string? value)
             (let ([len (string-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (string-ref value i))
                   (loop (+ i 1)))))]
            [(bytevector? value)
             (let ([len (bytevector-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (bytevector-u8-ref value i))
                   (loop (+ i 1)))))]
            [(hashtable? value)
             (let-values ([(keys vals) (hashtable-entries value)])
               (let ([len (vector-length vals)])
                 (let loop ([i 0])
                   (when (< i len)
                     (emit (vector-ref vals i))
                     (loop (+ i 1))))))]
            [else (nav-error 'nav-all "unsupported collection: ~s" value)])))

  (define transform-all-values
    (lambda (value update)
      (cond [(hashtable? value)
             (let ([copy (hashtable-copy value #t)])
               (let-values ([(keys vals) (hashtable-entries value)])
                 (let ([len (vector-length keys)])
                   (let loop ([i 0])
                     (when (< i len)
                       (hashtable-set! copy (vector-ref keys i)
                                       (update (vector-ref vals i)))
                       (loop (+ i 1))))))
               copy)]
            [else
             (let ([len (indexed-length value)])
               (if len
                   (indexed-transform value (index-list len) update)
                   (nav-error 'nav-transform "unsupported collection: ~s" value)))])))

  (define transform-all-values!
    (lambda (value update!)
      (cond [(hashtable? value)
             (let-values ([(keys vals) (hashtable-entries value)])
               (let ([len (vector-length keys)])
                 (let loop ([i 0])
                   (when (< i len)
                     (hashtable-set! value (vector-ref keys i)
                                     (update! (vector-ref vals i)))
                     (loop (+ i 1))))))
             value]
            [else
             (let ([len (indexed-length value)])
               (if len
                   (indexed-transform! value (index-list len) update!)
                   (nav-error 'nav-transform! "unsupported collection: ~s" value)))])))

  (define emit-alist-key-values
    (lambda (alist key emit)
      (let loop ([xs alist] [selected? #f])
        (cond [(null? xs)
               (unless selected?
                 (nav-error 'nav-key "missing key ~s in: ~s" key alist))]
              [(eq? (caar xs) key)
               (emit (cdar xs))
               (loop (cdr xs) #t)]
              [else (loop (cdr xs) selected?)]))))

  (define key-ref/missing
    (lambda (value key)
      (cond [(hashtable? value) (hashtable-ref value key nav-missing)]
            [(alist? value) (alist-ref/missing value key)]
            [else (nav-error 'nav-key "unsupported keyed value: ~s" value)])))

  (define key-transform
    (lambda (key default insert? value update)
      (cond [(hashtable? value)
             (let ([old (hashtable-ref value key nav-missing)])
               (cond [(nav-missing? old)
                      (if insert?
                          (let ([copy (hashtable-copy value #t)])
                            (hashtable-set! copy key (update default))
                            copy)
                          (nav-error 'nav-transform "missing key ~s in: ~s" key value))]
                     [else
                      (let ([copy (hashtable-copy value #t)])
                        (hashtable-set! copy key (update old))
                        copy)]))]
            [(alist? value)
             (let loop ([xs value])
               (cond [(null? xs)
                      (if insert?
                          (list (cons key (update default)))
                          (nav-error 'nav-transform "missing key ~s in: ~s" key value))]
                     [(eq? (caar xs) key)
                      (cons (cons key (update (cdar xs))) (cdr xs))]
                     [else (cons (car xs) (loop (cdr xs)))]))]
            [else (nav-error 'nav-transform "unsupported keyed value: ~s" value)])))

  (define key-transform!
    (lambda (key default insert? value update!)
      (cond [(hashtable? value)
             (let ([old (hashtable-ref value key nav-missing)])
               (cond [(nav-missing? old)
                      (if insert?
                          (hashtable-set! value key (update! default))
                          (nav-error 'nav-transform! "missing key ~s in: ~s" key value))]
                     [else (hashtable-set! value key (update! old))])
               value)]
            [(alist? value)
             (let loop ([xs value])
               (cond [(null? xs)
                      (nav-error 'nav-transform! "missing key ~s in: ~s" key value)]
                     [(eq? (caar xs) key)
                      (set-cdr! (car xs) (update! (cdar xs)))
                      value]
                     [else (loop (cdr xs))]))]
            [else (nav-error 'nav-transform! "unsupported keyed value: ~s" value)])))

  (define list-remove-index
    (lambda (value index)
      (let loop ([xs value] [i index])
        (cond [(null? xs)
               (nav-error 'nav-clearval "index ~s out of range for: ~s" index value)]
              [(= i 0) (cdr xs)]
              [else (cons (car xs) (loop (cdr xs) (- i 1)))]))))

  (define alist-remove-key
    (lambda (key value)
      (let loop ([xs value])
        (cond [(null? xs) '()]
              [(eq? (caar xs) key) (cdr xs)]
              [else (cons (car xs) (loop (cdr xs)))]))))

  (define key-clear
    (lambda (key value)
      (cond [(hashtable? value)
             (let ([copy (hashtable-copy value #t)])
               (hashtable-delete! copy key)
               copy)]
            [(alist? value) (alist-remove-key key value)]
            [else (nav-error 'nav-clearval "unsupported keyed value: ~s" value)])))

  (define key-clear!
    (lambda (key value)
      (cond [(hashtable? value)
             (hashtable-delete! value key)
             value]
            [(alist? value)
             (nav-error 'nav-clearval! "association list key cannot be removed in place: ~s" key)]
            [else (nav-error 'nav-clearval! "unsupported keyed value: ~s" value)])))

  (define submap-transform
    (lambda (keys value update)
      (cond [(hashtable? value)
             (let ([subtable (make-eq-hashtable)])
               (for-each
                (lambda (key)
                  (let ([selected (hashtable-ref value key nav-missing)])
                    (if (nav-missing? selected)
                        (nav-error 'nav-transform "missing key ~s in: ~s" key value)
                        (hashtable-set! subtable key selected))))
                keys)
               (let ([updated (update subtable)]
                     [copy (hashtable-copy value #t)])
                 (for-each
                  (lambda (key)
                    (let ([selected (hashtable-ref updated key nav-missing)])
                      (if (nav-missing? selected)
                          (hashtable-delete! copy key)
                          (hashtable-set! copy key selected))))
                  keys)
                 copy))]
            [(alist? value)
             (let ([updated
                    (map update
                         (map (lambda (key)
                                (let ([entry (assq key value)])
                                  (if entry
                                      entry
                                      (nav-error 'nav-transform
                                                 "missing key ~s in: ~s"
                                                 key value))))
                              keys))])
               (let loop ([xs value])
                 (cond [(null? xs) '()]
                       [(assq (caar xs) updated)
                        => (lambda (entry)
                             (cons entry (loop (cdr xs))))]
                       [else (cons (car xs) (loop (cdr xs)))])))]
            [else (nav-error 'nav-transform "unsupported keyed value: ~s" value)])))

  #|proc:nav-stay
  The `nav-stay` navigator focuses the current value unchanged.
  |#
  (define nav-stay
    (make-basic-nav 'nav-stay 'nav-stay
                    (lambda (value emit) (emit value))
                    (lambda (value update) (update value))
                    (lambda (value update!)
                      (nav-error 'nav-transform!
                                 "cannot replace root focus in place: ~s"
                                 value))
                    (lambda (value clear) (unsupported-clear 'nav-clearval value))
                    (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-none
  The `nav-none` navigator selects no values from the current value.
  |#
  (define nav-none
    (make-basic-nav 'nav-none 'nav-none
                    (lambda (value emit) (void))
                    (lambda (value update) value)
                    (lambda (value update!) value)
                    (lambda (value clear) value)
                    (lambda (value clear!) value)))

  #|proc:nav-all
  The `nav-all` navigator focuses every value in a supported collection.
  |#
  (define nav-all
    (make-basic-nav 'nav-all 'nav-all
                    emit-indexed-values
                    transform-all-values
                    transform-all-values!
                    (lambda (value clear) (unsupported-clear 'nav-clearval value))
                    (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-values
  The `nav-values` navigator focuses collection values.
  |#
  (define nav-values
    (make-basic-nav 'nav-values 'nav-values
                    emit-indexed-values
                    transform-all-values
                    transform-all-values!
                    (lambda (value clear) (unsupported-clear 'nav-clearval value))
                    (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-keys
  The `nav-keys` navigator focuses sequence indexes or hashtable keys for
  current collection value.
  |#
  (define nav-keys
    (make-basic-nav
     'nav-keys 'nav-keys
     (lambda (value emit)
       (cond [(or (list? value) (vector? value) (string? value) (bytevector? value))
              (let ([len (indexed-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (emit i)
                    (loop (+ i 1)))))]
             [(hashtable? value)
              (let ([keys (hashtable-keys value)])
                (let ([len (vector-length keys)])
                  (let loop ([i 0])
                    (when (< i len)
                      (emit (vector-ref keys i))
                      (loop (+ i 1))))))]
             [else (nav-error 'nav-keys "unsupported collection: ~s" value)]))
     (lambda (value update)
       (cond [(hashtable? value)
              (let ([copy (make-eq-hashtable)])
                (let-values ([(keys vals) (hashtable-entries value)])
                  (let ([len (vector-length keys)])
                    (let loop ([i 0])
                      (when (< i len)
                        (hashtable-set! copy (update (vector-ref keys i))
                                        (vector-ref vals i))
                        (loop (+ i 1))))))
                copy)]
             [(indexed-length value) value]
             [else (nav-error 'nav-transform "unsupported collection: ~s" value)]))
     (lambda (value update!)
       (nav-error 'nav-transform! "navigator nav-keys does not support mutation for: ~s" value))
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-entries
  The `nav-entries` navigator focuses entries as `(key . value)` pairs.
  |#
  (define nav-entries
    (make-basic-nav
     'nav-entries 'nav-entries
     (lambda (value emit)
       (cond [(list? value)
              (let loop ([xs value] [i 0])
                (unless (null? xs)
                  (emit (cons i (car xs)))
                  (loop (cdr xs) (+ i 1))))]
             [(vector? value)
              (let ([len (vector-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (emit (cons i (vector-ref value i)))
                    (loop (+ i 1)))))]
             [(string? value)
              (let ([len (string-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (emit (cons i (string-ref value i)))
                    (loop (+ i 1)))))]
             [(bytevector? value)
              (let ([len (bytevector-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (emit (cons i (bytevector-u8-ref value i)))
                    (loop (+ i 1)))))]
             [(hashtable? value)
              (let-values ([(keys vals) (hashtable-entries value)])
                (let ([len (vector-length keys)])
                  (let loop ([i 0])
                    (when (< i len)
                      (emit (cons (vector-ref keys i) (vector-ref vals i)))
                      (loop (+ i 1))))))]
             [else (nav-error 'nav-entries "unsupported collection: ~s" value)]))
     (lambda (value update)
       (cond [(hashtable? value)
              (let ([copy (make-eq-hashtable)])
                (let-values ([(keys vals) (hashtable-entries value)])
                  (let ([len (vector-length keys)])
                    (let loop ([i 0])
                      (when (< i len)
                        (let ([entry (update (cons (vector-ref keys i)
                                                   (vector-ref vals i)))])
                          (hashtable-set! copy (car entry) (cdr entry)))
                        (loop (+ i 1))))))
                copy)]
             [else (transform-all-values value update)]))
     (lambda (value update!)
       (nav-error 'nav-transform! "navigator nav-entries does not support mutation for: ~s" value))
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-nth
  The `nav-nth` procedure returns a navigator that focuses zero-based `index`.
  |#
  (define nav-nth
    (lambda (index)
      (pcheck ([integer? index])
              (when (< index 0)
                (nav-error 'nav-nth "index must be non-negative: ~s" index))
              (make-basic-nav
               'nav-nth `(nav-nth ,index)
               (lambda (value emit)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-error 'nav-nth "index ~s out of range for: ~s" index value)
                       (emit selected))))
               (lambda (value update)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-error 'nav-transform "index ~s out of range for: ~s" index value)
                       (indexed-transform value (list index) update))))
               (lambda (value update!)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-error 'nav-transform! "index ~s out of range for: ~s" index value)
                       (begin
                         (indexed-set! value index (update! selected))
                         value))))
               (lambda (value clear)
                 (if (list? value)
                     (list-remove-index value index)
                     (unsupported-clear 'nav-clearval value)))
               (lambda (value clear!)
                 (unsupported-clear 'nav-clearval! value))))))

  #|proc:nav-nth/default
  The `nav-nth/default` procedure returns a navigator that focuses zero-based
  `index`, or `default` when the index is out of range.
  |#
  (define nav-nth/default
    (lambda (index default)
      (pcheck ([integer? index])
              (when (< index 0)
                (nav-error 'nav-nth/default "index must be non-negative: ~s" index))
              (make-basic-nav
               'nav-nth/default `(nav-nth/default ,index ,default)
               (lambda (value emit)
                 (let ([selected (indexed-ref/missing value index)])
                   (emit (if (nav-missing? selected) default selected))))
               (lambda (value update)
                 (indexed-transform/default value index default update))
               (lambda (value update!)
                 (let ([selected (indexed-ref/missing value index)])
                   (if (nav-missing? selected)
                       (nav-error 'nav-transform! "cannot update missing index ~s in: ~s"
                                  index value)
                       (begin
                         (indexed-set! value index (update! selected))
                         value))))
               (lambda (value clear)
                 (unsupported-clear 'nav-clearval value))
               (lambda (value clear!)
                 (unsupported-clear 'nav-clearval! value))))))

  #|proc:nav-first
  The `nav-first` navigator focuses the first element of a supported sequence.
  |#
  (define nav-first (nav-nth 0))

  #|proc:nav-second
  The `nav-second` navigator focuses the second element of a supported sequence.
  |#
  (define nav-second (nav-nth 1))

  #|proc:nav-last
  The `nav-last` navigator focuses the last element of a non-empty sequence.
  |#
  (define nav-last
    (make-basic-nav
     'nav-last 'nav-last
     (lambda (value emit)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (emit (indexed-ref/missing value (- len 1)))
             (nav-error 'nav-last "unsupported or empty sequence: ~s" value))))
     (lambda (value update)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (indexed-transform value (list (- len 1)) update)
             (nav-error 'nav-transform "unsupported or empty sequence: ~s" value))))
     (lambda (value update!)
       (let ([len (indexed-length value)])
         (if (and len (> len 0))
             (begin
               (indexed-set! value (- len 1)
                             (update! (indexed-ref/missing value (- len 1))))
               value)
             (nav-error 'nav-transform! "unsupported or empty sequence: ~s" value))))
     (lambda (value clear)
       (unsupported-clear 'nav-clearval value))
     (lambda (value clear!)
       (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-slice
  The `nav-slice` procedure returns a navigator that focuses indexes in
  `[start, stop)`, optionally stepping by positive `step`.
  |#
  (define nav-slice
    (case-lambda
      [(start stop) (nav-slice start stop 1)]
      [(start stop step)
       (pcheck ([integer? start stop step])
               (when (or (< start 0) (< stop start) (<= step 0))
                 (nav-error 'nav-slice "invalid slice: ~s ~s ~s" start stop step))
               (make-basic-nav
                'nav-slice `(nav-slice ,start ,stop ,step)
                (lambda (value emit)
                  (let loop ([i start])
                    (when (< i stop)
                      (let ([selected (indexed-ref/missing value i)])
                        (unless (nav-missing? selected)
                          (emit selected)))
                      (loop (+ i step)))))
                (lambda (value update)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-error 'nav-transform "unsupported collection: ~s" value))
                    (let ([limit (min stop len)])
                      (let loop ([i start] [indexes '()])
                        (if (< i limit)
                            (loop (+ i step) (cons i indexes))
                            (indexed-transform value (reverse indexes) update))))))
                (lambda (value update!)
                  (let ([len (indexed-length value)])
                    (unless len
                      (nav-error 'nav-transform! "unsupported collection: ~s" value))
                    (let ([limit (min stop len)])
                      (let loop ([i start])
                        (when (< i limit)
                          (let ([selected (indexed-ref/missing value i)])
                            (unless (nav-missing? selected)
                              (indexed-set! value i (update! selected))))
                          (loop (+ i step)))))
                    value))
                (lambda (value clear)
                  (unsupported-clear 'nav-clearval value))
                (lambda (value clear!)
                  (unsupported-clear 'nav-clearval! value))))]))

  #|proc:nav-key
  The `nav-key` procedure returns a navigator that focuses the value associated
  with key `key` in a hashtable or association list. For association lists,
  selection focuses each matching key in order.
  |#
  (define nav-key
    (lambda (key)
      (make-basic-nav
       'nav-key `(nav-key ,key)
               (lambda (value emit)
                 (cond [(alist? value) (emit-alist-key-values value key emit)]
                       [else
                        (let ([selected (key-ref/missing value key)])
                          (if (nav-missing? selected)
                              (nav-error 'nav-key "missing key ~s in: ~s" key value)
                              (emit selected)))]))
       (lambda (value update)
         (key-transform key nav-missing #f value update))
       (lambda (value update!)
         (key-transform! key nav-missing #f value update!))
       (lambda (value clear) (key-clear key value))
       (lambda (value clear!) (key-clear! key value)))))

  #|proc:nav-key/default
  The `nav-key/default` procedure returns a navigator that focuses the value for
  `key`, or focuses `default` when `key` is missing.
  |#
  (define nav-key/default
    (case-lambda
      [(key) (nav-key/default key nav-missing)]
      [(key default)
       (make-basic-nav
        'nav-key/default `(nav-key/default ,key ,default)
        (lambda (value emit)
          (let ([selected (key-ref/missing value key)])
            (emit (if (nav-missing? selected) default selected))))
        (lambda (value update)
          (key-transform key default #t value update))
        (lambda (value update!)
          (key-transform! key default #t value update!))
        (lambda (value clear) (key-clear key value))
        (lambda (value clear!) (key-clear! key value)))]))

  #|proc:nav-key-values
  The `nav-key-values` procedure returns a navigator that focuses the values for
  each key in `keys`, preserving key order.
  |#
  (define nav-key-values
    (lambda keys
      (make-basic-nav
       'nav-key-values `(nav-key-values ,@keys)
       (lambda (value emit)
         (for-each
          (lambda (key)
            (($navigator-select-proc (nav-key key)) value emit))
          keys))
       (lambda (value update)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys)
                     (($navigator-transform-proc (nav-key (car keys)))
                      value update)))))
       (lambda (value update!)
         (for-each (lambda (key)
                     (($navigator-transform!-proc (nav-key key)) value update!))
                   keys)
         value)
       (lambda (value clear)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys) (key-clear (car keys) value)))))
       (lambda (value clear!)
         (for-each (lambda (key) (key-clear! key value)) keys)
         value))))

  #|proc:nav-submap
  The `nav-submap` procedure returns a navigator that focuses the subset of
  association-list entries or hashtable entries named by `keys`.
  |#
  (define nav-submap
    (lambda keys
      (make-basic-nav
       'nav-submap `(nav-submap ,@keys)
       (lambda (value emit)
         (cond [(hashtable? value)
                (let ([table (make-eq-hashtable)])
                  (for-each
                   (lambda (key)
                     (let ([selected (hashtable-ref value key nav-missing)])
                       (if (nav-missing? selected)
                           (nav-error 'nav-submap "missing key ~s in: ~s" key value)
                           (hashtable-set! table key selected))))
                   keys)
                  (emit table))]
               [(alist? value)
                (for-each
                 (lambda (key)
                   (let ([entry (assq key value)])
                     (if entry
                         (emit entry)
                         (nav-error 'nav-submap "missing key ~s in: ~s" key value))))
                 keys)]
               [else (nav-error 'nav-submap "unsupported keyed value: ~s" value)]))
       (lambda (value update)
         (submap-transform keys value update))
       (lambda (value update!)
         (nav-unsupported-error 'nav-transform! nav-submap value 'transform!))
       (lambda (value clear)
         (let loop ([keys keys] [value value])
           (if (null? keys)
               value
               (loop (cdr keys) (key-clear (car keys) value)))))
       (lambda (value clear!)
         (for-each (lambda (key) (key-clear! key value)) keys)
         value))))

  #|proc:nav-car
  The `nav-car` navigator focuses the `car` field of a pair.
  |#
  (define nav-car
    (make-basic-nav
     'nav-car 'nav-car
     (lambda (value emit)
       (if (pair? value)
           (emit (car value))
           (nav-error 'nav-car "not a pair: ~s" value)))
     (lambda (value update)
       (if (pair? value)
           (cons (update (car value)) (cdr value))
           (nav-error 'nav-transform "not a pair: ~s" value)))
     (lambda (value update!)
       (if (pair? value)
           (begin
             (set-car! value (update! (car value)))
             value)
           (nav-error 'nav-transform! "not a pair: ~s" value)))
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  #|proc:nav-cdr
  The `nav-cdr` navigator focuses the `cdr` field of a pair.
  |#
  (define nav-cdr
    (make-basic-nav
     'nav-cdr 'nav-cdr
     (lambda (value emit)
       (if (pair? value)
           (emit (cdr value))
           (nav-error 'nav-cdr "not a pair: ~s" value)))
     (lambda (value update)
       (if (pair? value)
           (cons (car value) (update (cdr value)))
           (nav-error 'nav-transform "not a pair: ~s" value)))
     (lambda (value update!)
       (if (pair? value)
           (begin
             (set-cdr! value (update! (cdr value)))
             value)
           (nav-error 'nav-transform! "not a pair: ~s" value)))
     (lambda (value clear) (unsupported-clear 'nav-clearval value))
     (lambda (value clear!) (unsupported-clear 'nav-clearval! value))))

  (install-nav-key-proc! nav-key)
  (install-nav-nth-proc! nav-nth))
