#!chezscheme
(library (chezpp glob)
  (export make-glob glob? glob-match? glob glob* glob->iter)
  (import (chezpp chez)
          (chezpp path)
          (chezpp regex)
          (chezpp utils)
          (chezpp list)
          (chezpp iter))

  (define-record-type ($glob %make-glob %glob?)
    (fields (immutable flavor glob-flavor) (immutable patterns glob-patterns)))
  (define $error (lambda (message . args) (apply errorf 'make-glob message args)))
  (define $prefix?
    (lambda (prefix s)
      (let ([n (string-length prefix)])
        (and (fx<= n (string-length s)) (string=? prefix (substring s 0 n))))))
  (define $brace-open
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0])
          (cond [(fx= i n) #f]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2)) ($error "trailing escape"))]
                [(char=? (string-ref s i) #\}) ($error "unmatched closing brace")]
                [(char=? (string-ref s i) #\{) i]
                [else (loop (fx1+ i))])))))
  (define $brace-close
    (lambda (s open)
      (let ([n (string-length s)])
        (let loop ([i (fx1+ open)] [depth 1])
          (cond [(fx= i n) ($error "unmatched opening brace")]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) depth) ($error "trailing escape"))]
                [(char=? (string-ref s i) #\{) (loop (fx1+ i) (fx1+ depth))]
                [(char=? (string-ref s i) #\})
                 (if (fx= depth 1) i (loop (fx1+ i) (fx1- depth)))]
                [else (loop (fx1+ i) depth)])))))
  (define $split
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0] [start 0] [depth 0] [out '()])
          (cond [(fx= i n) (reverse (cons (substring s start n) out))]
                [(char=? (string-ref s i) #\\)
                 (if (fx< (fx1+ i) n) (loop (fx+ i 2) start depth out)
                     ($error "trailing escape in brace"))]
                [(char=? (string-ref s i) #\{) (loop (fx1+ i) start (fx1+ depth) out)]
                [(char=? (string-ref s i) #\}) (loop (fx1+ i) start (fx1- depth) out)]
                [(and (char=? (string-ref s i) #\,) (fx= depth 0))
                 (loop (fx1+ i) (fx1+ i) depth (cons (substring s start i) out))]
                [else (loop (fx1+ i) start depth out)])))))
  (define $integer? (lambda (s) (regex-matches? (string->regex "^[+-]?[0-9]+$") s)))
  (define $range
    (lambda (s)
      (let ([dots (let loop ([i 0] [out '()])
                    (if (fx= i (string-length s))
                        (reverse out)
                        (if (and (fx< (fx1+ i) (string-length s))
                                 (char=? (string-ref s i) #\.)
                                 (char=? (string-ref s (fx1+ i)) #\.))
                            (loop (fx+ i 2) (cons i out))
                            (loop (fx1+ i) out))))])
        (and (or (fx= (length dots) 1) (fx= (length dots) 2))
             (let* ([a (car dots)]
                    [b (if (fx= (length dots) 2) (cadr dots) (string-length s))]
                    [x (substring s 0 a)]
                    [y (substring s (fx+ a 2) b)]
                    [z (and (fx= (length dots) 2)
                            (substring s (fx+ b 2) (string-length s)))])
               (and ($integer? x) ($integer? y) (or (not z) ($integer? z))
                    (let* ([start (string->number x)] [end (string->number y)]
                           [step (if z (string->number z) (if (<= start end) 1 -1))])
                      (let loop ([v start] [out '()])
                        (if (if (> step 0) (> v end) (< v end)) (reverse out)
                            (loop (+ v step) (cons (format "~a" v) out)))))))))))
  (define $expand-braces
    (lambda (s)
      (let ([open ($brace-open s)])
        (if (not open) (list s)
            (let* ([close ($brace-close s open)] [body (substring s (fx1+ open) close)]
                   [prefix (substring s 0 open)]
                   [suffix (substring s (fx1+ close) (string-length s))]
                   [values (or ($range body) ($split body))])
              (when (exists (lambda (x) (string=? x "")) values)
                ($error "empty brace alternative"))
              (apply append (map (lambda (x) ($expand-braces (string-append prefix x suffix))) values)))))))
  (define $escape-regex
    (lambda (c)
      (if (memv c '(#\. #\^ #\$ #\* #\+ #\? #\( #\) #\[ #\] #\{ #\} #\| #\\))
          (string #\\ c) (string c))))
  (define $component-regex
    (lambda (s)
      (let ([n (string-length s)])
        (let loop ([i 0] [out ""])
          (if (fx= i n) (string->regex (string-append "^" out "$"))
              (let ([c (string-ref s i)])
                (cond [(char=? c #\\)
                       (if (fx< (fx1+ i) n)
                           (loop (fx+ i 2) (string-append out ($escape-regex (string-ref s (fx1+ i)))))
                           ($error "trailing escape"))]
                      [(char=? c #\*) (loop (fx1+ i) (string-append out ".*"))]
                      [(char=? c #\?) (loop (fx1+ i) (string-append out "."))]
                      [(char=? c #\[)
                       (let scan ([j (fx1+ i)])
                         (cond [(fx= j n) ($error "unterminated class")]
                               [(char=? (string-ref s j) #\])
                                (if (fx= j (fx1+ i)) ($error "empty class")
                                    (let* ([class (substring s i (fx1+ j))]
                                           [class (if (and (fx< 1 (string-length class))
                                                           (char=? (string-ref class 1) #\!))
                                                       (string-append "[^" (substring class 2 (fx1- (string-length class))) "]")
                                                       class)])
                                      (loop (fx1+ j) (string-append out class))))]
                               [else (scan (fx1+ j))]))]
                      [else (loop (fx1+ i) (string-append out ($escape-regex c)))])))))))
  (define $compile
    (lambda (flavor s)
      (let ([p (path-parse flavor s)])
        (cons (path-root-info p)
              (map (lambda (x) (if (string=? x "**") #f ($component-regex x)))
                   (path-components p))))))
  (define $match?
    (lambda (ps vs)
      (cond [(null? ps) (null? vs)]
            [(not (car ps)) (or ($match? (cdr ps) vs) (and (pair? vs) ($match? ps (cdr vs))))]
            [(and (pair? vs) (regex-matches? (car ps) (car vs))) ($match? (cdr ps) (cdr vs))]
            [else #f])))
  (define $expand-tilde
    (lambda (flavor s)
      (let* ([p (path-parse flavor s)] [cs (path-components p)])
        (cond [(null? cs) s]
              [(string=? (car cs) "~") (path-render (path-expand-user p))]
              [($prefix? "~" (car cs)) ($error "named-user tilde is unsupported")]
              [else s]))))

  #|proc:make-glob
  Compile `pattern` for Unix or Windows path matching. The one-argument form
  uses Unix paths; the two-argument form accepts `flavor` and `pattern`.
  Brace and leading-user-tilde expansion occurs before compilation.
  |#
  (define make-glob
    (case-lambda
      [(pattern) (make-glob 'unix pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               (%make-glob flavor
                           (map (lambda (s) ($compile flavor ($expand-tilde flavor s)))
                                ($expand-braces pattern))))]))

  #|proc:glob?
  Return `#t` when `object` is a compiled glob object.
  |#
  (define glob? (lambda (object) (%glob? object)))

  (define $collect-files
    (lambda (root prefix follow-link? out)
      (let loop ([entries (directory-list root)])
        (unless (null? entries)
          (let* ([name (car entries)]
                 [path (if (string=? prefix "") name (string-append prefix "/" name))])
            (out path)
            (when (file-directory? path follow-link?)
              ($collect-files path path follow-link? out)))
          (loop (cdr entries))))))

  (define $glob-expand
    (lambda (flavor pattern follow-link? include-directories? unmatched)
      (let ([compiled (make-glob flavor pattern)] [result (make-list-builder)])
        ($collect-files "." "" follow-link?
                        (lambda (path)
                          (when (or include-directories? (not (file-directory? path follow-link?)))
                            (when (glob-match? compiled path) (result path)))))
        (let ([values (sort string<? (result))])
          (if (and (null? values) (eq? unmatched 'literal)) (list pattern) values)))))

  #|proc:glob
  Expand Unix `pattern` against the current directory and return sorted matches.
  Unmatched patterns return the empty list.
  |#
  (define glob
    (case-lambda
      [(pattern) (glob 'unix pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               ($glob-expand flavor pattern #f #f 'empty))]))

  #|proc:glob*
  Expand `pattern` with explicit traversal, directory, and unmatched policies.
  `unmatched` is either `'empty` or `'literal`.
  |#
  (define glob*
    (case-lambda
      [(pattern follow-link? include-directories? unmatched)
       (glob* 'unix pattern follow-link? include-directories? unmatched)]
      [(flavor pattern follow-link? include-directories? unmatched)
       (pcheck ([path-flavor? flavor] [string? pattern]
                [boolean? follow-link? include-directories?]
                [(lambda (x) (memq x '(empty literal))) unmatched])
               ($glob-expand flavor pattern follow-link? include-directories? unmatched))]))

  #|proc:glob->iter
  Return an iterator over the same sorted matches as `glob`.
  |#
  (define glob->iter
    (case-lambda
      [(pattern) (glob->iter 'unix pattern)]
      [(flavor pattern)
       (pcheck ([path-flavor? flavor] [string? pattern])
               (list->iter (glob flavor pattern)))]))

  #|proc:glob-match?
  Return whether complete `path` matches compiled `glob`, or compile `pattern`
  with `flavor` and match it in the three-argument form.
  |#
  (define glob-match?
    (case-lambda
      [(glob path)
       (pcheck ([glob? glob] [string? path])
               (let* ([p (path-parse (glob-flavor glob) path)] [root (path-root-info p)]
                      [values (path-components p)])
                 (exists (lambda (x) (and (equal? root (car x)) ($match? (cdr x) values)))
                         (glob-patterns glob))))]
      [(flavor pattern path)
       (pcheck ([path-flavor? flavor] [string? pattern path])
               (glob-match? (make-glob flavor pattern) path))]))
)
