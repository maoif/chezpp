(import (chezpp))

(define list-copy*
  (lambda (x)
    (if (pair? x)
        (cons (list-copy* (car x)) (list-copy* (cdr x)))
        x)))

(define ht
  (lambda args
    (let ([table (make-eq-hashtable)])
      (let loop ([args args])
        (if (null? args)
            table
            (begin
              (hashtable-set! table (car args) (cadr args))
              (loop (cddr args))))))))

(define ht-ref
  (lambda (table key)
    (hashtable-ref table key #f)))

(define capture-written
  (lambda (proc value)
    (call-with-string-output-port
      (lambda (port)
        (proc value port)))))

(define has-same-keys?
  (lambda (table keys)
    (let ([ks (vector->list (hashtable-keys table))])
      (and (= (length ks) (length keys))
           (andmap (lambda (key) (memq key ks)) keys)))))

(mat navigator-core
     (nav? nav-stay)
     (nav-path? nav-empty-path)
     (nav-empty-path? nav-empty-path)
     (equal? '(nav-path) (nav->datum nav-empty-path))
     (equal? '(nav-path (nav-key user) (nav-nth 0))
             (nav->datum (nav-path 'user 0)))
     (equal? '(nav-path (nav-key user) nav-all (nav-key name))
             (nav->datum (nav-path (nav-path (nav-key 'user) nav-all)
                                   (nav-key 'name))))
     (eq? nav-stay (nav-coerce nav-stay))
     (nav-path? (nav-coerce (nav-path nav-stay)))
     (equal? '(nav-key user) (nav->datum (nav-coerce 'user)))
     (equal? '(nav-nth 3) (nav->datum (nav-coerce 3)))
     ;; negative: negative indexes are not valid path coercions.
     (error? (nav-coerce -1))
     ;; negative: arbitrary data cannot be coerced into a path step.
     (error? (nav-coerce '#(bad))))

(mat navigator-debug-core
     (string? (capture-written nav-write (nav-path nav-stay nav-none)))
     (equal? "(nav-path nav-stay nav-none)"
             (capture-written nav-write (nav-path nav-stay nav-none)))
     (let ([s (capture-written nav-display (nav-path nav-stay nav-none))])
       (and (string? s) (> (string-length s) 0)))
     (eq? 'nav-stay (nav-debug-name nav-stay))
     (equal? '(nav-explain (steps (nav-stay nav-none)))
             (nav-explain (nav-path nav-stay nav-none))))

(mat navigator-selection-basic
     (equal? '(root) (nav-select nav-empty-path 'root))
     (equal? '(root) (nav-select nav-stay 'root))
     (equal? '() (nav-select nav-none 'root))
     (equal? '(a b c) (nav-select nav-all '(a b c)))
     (equal? '(a b c) (nav-select nav-all '#(a b c)))
     (equal? '(#\a #\b) (nav-select nav-all "ab"))
     (equal? '(65 66) (nav-select nav-all #vu8(65 66)))
     (let ([table (ht 'a 1 'b 2)])
       (= 2 (length (nav-select nav-all table))))
     (equal? '(b) (nav-select (nav-nth 1) '(a b c)))
     (equal? '(b) (nav-select (nav-nth 1) '#(a b c)))
     (equal? '(#\b) (nav-select (nav-nth 1) "abc"))
     (equal? '(66) (nav-select (nav-nth 1) #vu8(65 66 67)))
     (equal? '(a) (nav-select nav-first '(a b)))
     (equal? '(b) (nav-select nav-second '(a b)))
     (equal? '(c) (nav-select nav-last '(a b c)))
     (equal? '(x) (nav-select (nav-key 'name) '((name . x) (age . 1))))
     (equal? '(7) (nav-select (nav-key 'count) (ht 'count 7)))
     (equal? '(fallback) (nav-select (nav-key/default 'missing 'fallback) '()))
     (equal? '(fallback) (nav-select (nav-nth/default 9 'fallback) '(a b)))
     (equal? '(1 3) (nav-select (nav-key-values 'a 'c) '((a . 1) (b . 2) (c . 3))))
     (equal? '((a . 1) (c . 3))
             (nav-select (nav-submap 'a 'c) '((a . 1) (b . 2) (c . 3))))
     (equal? '(a) (nav-select nav-car '(a . b)))
     (equal? '(b) (nav-select nav-cdr '(a . b)))
     ;; negative: strict index selection rejects out-of-range indexes.
     (error? (nav-select (nav-nth 9) '(a b)))
     ;; negative: strict key selection rejects missing association keys.
     (error? (nav-select (nav-key 'missing) '((name . x)))))

(mat navigator-selection-actions
     (equal? '(Ada)
             (nav-select (nav-path (nav-key 'user) (nav-key 'name))
                         '((user . ((name . Ada) (active . #t))))))
     (eq? #t
          (nav-select-one (nav-path (nav-key 'user) (nav-key 'active))
                          '((user . ((active . #t))))))
     (eq? 'fallback (nav-select-first (nav-key 'missing)
                                      '((name . Ada))
                                      'fallback))
     (eq? #f (nav-select-first nav-none '(a b c)))
     (let ([head-then-error
            (make-nav 'head-then-error
                      (lambda (value emit)
                        (emit (car value))
                        (errorf 'head-then-error "traversed too far"))
                      (lambda (value update) value)
                      (lambda (value update!) value))])
       (eq? 'a (nav-select-first head-then-error '(a b c))))
     (= 3 (nav-select-count nav-all '(a b c)))
     (nav-selected? (nav-key 'name) '((name . Ada)))
     (let ([head-then-error
            (make-nav 'head-then-error
                      (lambda (value emit)
                        (emit (car value))
                        (errorf 'head-then-error "traversed too far"))
                      (lambda (value update) value)
                      (lambda (value update!) value))])
       (nav-selected? head-then-error '(a b c)))
     (not (nav-selected? nav-none '(a b c)))
     (equal? '(0:a 1:b 2:c)
             (let ([lb (make-list-builder)])
               (nav-traverse/i nav-all '(a b c)
                               (lambda (i value)
                                 (lb (string->symbol
                                      (string-append (number->string i)
                                                     ":"
                                                     (symbol->string value))))))
               (lb)))
     ;; negative: select-one rejects zero selected values.
     (error? (nav-select-one nav-none '(a b)))
     ;; negative: select-one rejects multiple selected values.
     (error? (nav-select-one nav-all '(a b))))

(mat navigator-transform-pure
     (equal? '(2 3 4) (nav-transform nav-all add1 '(1 2 3)))
     (equal? '#(2 3 4) (nav-transform nav-all add1 '#(1 2 3)))
     (equal? "ABC" (nav-transform nav-all char-upcase "abc"))
     (equal? #vu8(66 67) (nav-transform nav-all add1 #vu8(65 66)))
     (equal? '((name . Ada) (age . 38))
             (nav-setval (nav-key 'age) 38 '((name . Ada) (age . 37))))
     (equal? '((name . Ada) (age . 1))
             (nav-setval (nav-key/default 'age) 1 '((name . Ada))))
     (equal? '(a x x z)
             (nav-setval (nav-nth/default 3 'x) 'z '(a)))
     (equal? '#(a x x z)
             (nav-setval (nav-nth/default 3 'x) 'z '#(a)))
     (equal? '(a X c) (nav-setval (nav-nth 1) 'X '(a b c)))
     (equal? '#(a X c) (nav-setval (nav-nth 1) 'X '#(a b c)))
     (equal? '(a (B C))
             (nav-transform (nav-path (nav-nth 1) nav-all)
                            (lambda (x) (string->symbol (string-upcase (symbol->string x))))
                            '(a (b c))))
     (equal? '(0:a 1:b 2:c)
             (nav-transform/i nav-all
                              (lambda (i value)
                                (string->symbol
                                 (string-append (number->string i) ":"
                                                (symbol->string value))))
                              '(a b c)))
     (let* ([old '#(1 2 3)]
            [new (nav-transform nav-all add1 old)])
       (and (equal? old '#(1 2 3))
            (equal? new '#(2 3 4))
            (not (eq? old new))))
     ;; negative: strict missing key cannot be transformed.
     (error? (nav-setval (nav-key 'missing) 1 '((name . Ada))))
     ;; negative: pure string transform must return characters.
     (error? (nav-transform nav-all (lambda (ch) 65) "ab"))
     ;; negative: pure bytevector transform must return bytes.
     (error? (nav-transform nav-all (lambda (b) #\A) #vu8(65))))

(mat navigator-transform-mutating
     (let ([xs (list 'a 'b 'c)])
       (and (eq? xs (nav-setval! (nav-nth 1) 'X xs))
            (equal? xs '(a X c))))
     (let ([v (vector 'a 'b 'c)])
       (and (eq? v (nav-setval! (nav-nth 1) 'X v))
            (equal? v '#(a X c))))
     (let ([s (string-copy "abc")])
       (and (eq? s (nav-transform! nav-all char-upcase s))
            (string=? s "ABC")))
     (let ([bv (bytevector-copy #vu8(65 66))])
       (and (eq? bv (nav-transform! nav-all add1 bv))
            (equal? bv #vu8(66 67))))
     (let ([table (ht 'count 1)])
       (and (eq? table (nav-setval! (nav-key 'count) 2 table))
            (= 2 (ht-ref table 'count))))
     (let ([root (vector (ht 'count 1))])
       (and (eq? root (nav-transform! (nav-path (nav-nth 0) (nav-key 'count)) add1 root))
            (= 2 (ht-ref (vector-ref root 0) 'count))))
     (let ([v (vector 'a 'b 'c)])
       (and (eq? v (nav-transform!/i nav-all
                                     (lambda (i value)
                                       (string->symbol
                                        (string-append (number->string i) ":"
                                                       (symbol->string value))))
                                     v))
            (equal? v '#(0:a 1:b 2:c))))
     ;; negative: mutating an out-of-range index is invalid.
     (error? (nav-setval! (nav-nth 3) 'x (vector 'a)))
     ;; negative: mutating an unsupported scalar does not fall back to pure rebuild.
     (error? (nav-transform! nav-all add1 1))
     ;; negative: string mutation must receive characters.
     (error? (nav-transform! nav-all (lambda (ch) 65) (string-copy "ab"))))

(mat navigator-conditional
     (equal? '(1 2 3) (nav-select (nav-path nav-all (nav-pred number?)) '(a 1 b 2 3 c)))
     (equal? '(a b c) (nav-select (nav-path nav-all (nav-not-pred number?)) '(a 1 b 2 3 c)))
     (equal? '() (nav-select (nav-path (nav-maybe (nav-key 'missing))) '((name . Ada))))
     (equal? '(Ada)
             (nav-select (nav-if (nav-key 'name)
                                 (nav-key 'name)
                                 (nav-key 'fallback))
                         '((name . Ada) (fallback . No))))
     (equal? '(No)
             (nav-select (nav-if (nav-key 'missing)
                                 (nav-key 'name)
                                 (nav-key 'fallback))
                         '((name . Ada) (fallback . No))))
     (equal? '(Ada 37)
             (nav-select (nav-multi-path (nav-key 'name) (nav-key 'age))
                         '((name . Ada) (age . 37))))
     (equal? '(Ada)
             (nav-select (nav-choice (nav-key 'missing) (nav-key 'name))
                         '((name . Ada))))
     (equal? '((name . ADA) (age . 37))
             (nav-transform (nav-when (nav-key 'name) (nav-key 'name))
                            (lambda (x) 'ADA)
                            '((name . Ada) (age . 37))))
     (equal? '((name . Ada) (age . 37))
             (nav-transform (nav-unless (nav-key 'name) (nav-key 'age))
                            add1
                            '((name . Ada) (age . 37))))
     ;; negative: must turns missing selection into an error.
     (error? (nav-select (nav-must (nav-key 'missing)) '((name . Ada)))))

(mat navigator-custom
     (let ([point-x (nav-getter-setter 'point-x car (lambda (p x) (cons x (cdr p))))])
       (equal? '(2 . 2) (nav-transform point-x add1 '(1 . 2))))
     (let ([cell (cons 1 2)]
           [point-x (nav-getter-setter! 'point-x car
                                        (lambda (p x) (cons x (cdr p)))
                                        (lambda (p x) (set-car! p x)))])
       (and (eq? cell (nav-setval! point-x 9 cell))
            (equal? cell '(9 . 2))))
     (equal? '(1) (nav-select (nav-getter 'first car) '(1 . 2)))
     ;; negative: read-only getter cannot be transformed.
     (error? (nav-transform (nav-getter 'first car) add1 '(1 . 2)))
     ;; negative: getter-setter without mutator cannot be used with transform!.
     (error? (nav-transform! (nav-getter-setter 'point-x car
                                                (lambda (p x) (cons x (cdr p))))
                            add1
                            (cons 1 2))))

(mat navigator-clear
     (equal? '(a c) (nav-clearval (nav-nth 1) '(a b c)))
     (equal? '((name . Ada)) (nav-clearval (nav-key 'age) '((name . Ada) (age . 37))))
     (let ([table (ht 'name 'Ada 'age 37)])
       (let ([new (nav-clearval (nav-key 'age) table)])
         (and (hashtable? new)
              (eq? 'Ada (ht-ref new 'name))
              (not (hashtable-contains? new 'age))
              (hashtable-contains? table 'age))))
     (let ([table (ht 'name 'Ada 'age 37)])
       (and (eq? table (nav-clearval! (nav-key 'age) table))
            (not (hashtable-contains? table 'age))))
     ;; negative: vector slots cannot be removed.
     (error? (nav-clearval (nav-nth 1) '#(a b c)))
     ;; negative: string slots cannot be removed.
     (error? (nav-clearval (nav-nth 1) "abc"))
     ;; negative: association list mutation cannot remove the head cell safely.
     (error? (nav-clearval! (nav-key 'name) '((name . Ada)))))

(mat navigator-recursive
     (equal? '((1 b) #(2 (c 3)) 1 b 2 (c 3) c 3)
             (nav-select (nav-path nav-children)
                         '(a (1 b) #(2 (c 3)))))
     (equal? '(a b c)
             (nav-select nav-leaves '(a (b) #(c))))
     (equal? '(1 2 3)
             (nav-select (nav-walker number?) '(a (1 b) #(2 (c 3)))))
     (let ([numbers-anywhere
            (nav-rec numbers-anywhere
              (nav-choice
               (nav-path (nav-pred number?))
               (nav-path nav-children numbers-anywhere)))])
       (equal? '(1 2 3) (nav-select numbers-anywhere '(a (1 b) #(2 (c 3))))))
     (let ([expr-values
            (nav-letrec
             ([expr (nav-choice
                     (nav-path (nav-key 'literal))
                     (nav-path (nav-key 'call) call)
                     (nav-path (nav-key 'block) stmt))]
              [stmt (nav-choice
                     (nav-path (nav-key 'expr) expr)
                     (nav-path (nav-key 'body) nav-all stmt))]
              [call (nav-path (nav-key 'args) nav-all expr)])
             expr)])
       (equal? '(1 2 3)
               (nav-select expr-values
                           '((block . ((body . (((expr . ((literal . 1)))
                                                 (expr . ((call . ((args . (((literal . 2)
                                                                              (literal . 3)))))))))))))))))
     (equal? '(root 1 2)
             (nav-select (nav-before (nav-path nav-children (nav-walker number?)))
                         '(root (1) (2))))
     (equal? '(1 2 root)
             (nav-select (nav-after (nav-path nav-children (nav-walker number?)))
                         '(root (1) (2))))
     (let ([numbers-anywhere
            (nav-rec numbers-anywhere
              (nav-choice
               (nav-path (nav-pred number?))
               (nav-path nav-children numbers-anywhere)))])
       (equal? '(a (2 b) #(3 (c 4)))
               (nav-transform numbers-anywhere add1 '(a (1 b) #(2 (c 3))))))
     (let ([root (vector 'a (list 1 'b) (vector 2 (list 'c 3)))]
           [numbers-anywhere
            (nav-rec numbers-anywhere
              (nav-choice
               (nav-path (nav-pred number?))
               (nav-path nav-children numbers-anywhere)))])
       (and (eq? root (nav-transform! numbers-anywhere add1 root))
            (equal? root '#(a (2 b) #(3 (c 4))))))
     (let ([drop-flags
            (nav-rec drop-flags
              (nav-choice
               (nav-path (nav-key 'drop))
               (nav-path nav-children drop-flags)))])
       (equal? '((keep . 1) ((drop . 4) (keep . 3)))
               (nav-clearval drop-flags '((keep . 1) (drop . 2) ((drop . 4) (keep . 3))))))
     (let ([root (vector (ht 'keep 1 'drop 2) (list (ht 'drop 4 'keep 3)))]
           [drop-flags
            (nav-rec drop-flags
              (nav-multi-path
               (nav-path (nav-maybe (nav-key 'drop)))
               (nav-path nav-children drop-flags)))])
       (and (eq? root (nav-clearval! drop-flags root))
            (not (hashtable-contains? (vector-ref root 0) 'drop))
            (not (hashtable-contains? (car (vector-ref root 1)) 'drop))
            (= 1 (ht-ref (vector-ref root 0) 'keep))
            (= 3 (ht-ref (car (vector-ref root 1)) 'keep))))
     ;; negative: unresolved recursive references are rejected.
     (error? (nav-select (make-nav-ref 'missing) '(a b))))

(mat navigator-interop
     (equal? '(1 2 3) (nav-selected->list nav-all '(1 2 3)))
     (equal? '#(1 2 3) (nav-selected->vector nav-all '(1 2 3)))
     (equal? '(1 2 3)
             (let ([iter (nav-traverse->iter nav-all '(1 2 3))])
               (iter->list iter)))
     (let* ([count 0]
            [lazy-nav (make-nav 'lazy
                                (lambda (value emit)
                                  (for-each
                                   (lambda (item)
                                     (set! count (+ count 1))
                                     (when (> count 1)
                                       (errorf 'lazy-nav "traversed too far"))
                                     (emit item))
                                   value))
                                (lambda (value update) value)
                                (lambda (value update!) value))])
       (let ([iter (nav-traverse->iter lazy-nav '(a b c))])
         (and (= count 0)
              (eq? 'a (iter-next! iter))
              (= count 1))))
     (= 6
        (nav-selected-transduce nav-all
                                (lambda (reducer) reducer)
                                (case-lambda
                                  [() 0]
                                  [(acc value) (+ acc value)])
                                '(1 2 3)))
     (= 6
        (nav-selected-transduce nav-all
                                (lambda (reducer) reducer)
                                (lambda (acc value) (+ acc value))
                                0
                                '(1 2 3))))

(mat navigator-facade
     (equal? '(Ada)
             (nav-select (nav-path (nav-key 'user) (nav-key 'name))
                         '((user . ((name . Ada)))))))

(mat navigator-boundaries
     ;; negative: custom navigator name must be a symbol.
     (error? (make-nav "bad" values values values))
     ;; negative: custom navigator procedures must be procedures.
     (error? (make-nav 'bad 1 values values))
     ;; negative: nav-name expects a navigator.
     (error? (nav-name 'not-a-nav))
     ;; negative: nav-nth index must be natural.
     (error? (nav-nth -1))
     ;; negative: nav-slice step must be positive.
     (error? (nav-slice 0 3 0))
     ;; negative: nav-pred expects a procedure.
     (error? (nav-pred 'not-a-procedure))
     ;; negative: nav-traverse expects a procedure.
     (error? (nav-traverse nav-all '(a b) 'not-a-procedure))
     ;; negative: nav-transform expects a procedure.
     (error? (nav-transform nav-all 'not-a-procedure '(a b))))
