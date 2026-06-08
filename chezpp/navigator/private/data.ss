(library (chezpp navigator private data)
  (export byte? indexed-length indexed-ref/missing indexed-set!
          alist? alist-ref/missing)
  (import (chezscheme)
          (chezpp navigator private core))

  (define byte?
    (lambda (x)
      (and (integer? x) (<= 0 x) (<= x 255))))

  (define indexed-length
    (lambda (value)
      (cond [(list? value) (length value)]
            [(vector? value) (vector-length value)]
            [(string? value) (string-length value)]
            [(bytevector? value) (bytevector-length value)]
            [else #f])))

  (define indexed-ref/missing
    (lambda (value index)
      (cond [(list? value)
             (if (< index (length value)) (list-ref value index) nav-missing)]
            [(vector? value)
             (if (< index (vector-length value)) (vector-ref value index) nav-missing)]
            [(string? value)
             (if (< index (string-length value)) (string-ref value index) nav-missing)]
            [(bytevector? value)
             (if (< index (bytevector-length value)) (bytevector-u8-ref value index) nav-missing)]
            [else (nav-error 'nav-nth "unsupported indexed value: ~s" value)])))

  (define indexed-set!
    (lambda (value index new-value)
      (cond [(list? value)
             (let loop ([xs value] [i index])
               (cond [(null? xs)
                      (nav-error 'nav-transform! "index ~s out of range for: ~s" index value)]
                     [(= i 0) (set-car! xs new-value)]
                     [else (loop (cdr xs) (- i 1))]))]
            [(vector? value) (vector-set! value index new-value)]
            [(string? value)
             (if (char? new-value)
                 (string-set! value index new-value)
                 (nav-error 'nav-transform!
                            "string element is not a char: ~s"
                            new-value))]
            [(bytevector? value)
             (if (byte? new-value)
                 (bytevector-u8-set! value index new-value)
                 (nav-error 'nav-transform!
                            "bytevector element is not a byte: ~s"
                            new-value))]
            [else (nav-error 'nav-transform! "unsupported indexed value: ~s" value)])))

  (define alist?
    (lambda (value)
      (and (list? value) (andmap pair? value))))

  (define alist-ref/missing
    (lambda (alist key)
      (let ([entry (assq key alist)])
        (if entry (cdr entry) nav-missing)))))
