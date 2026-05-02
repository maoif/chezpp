#!chezscheme
(library (chezpp rich text)
  (export make-rich-text
          rich-text
          rich-text?
          rich-text-append!
          rich-text-plain
          rich-text-render)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp rich style)
          (chezpp rich segment))

  (define-record-type (rich-text-record $make-rich-text-record $rich-text?)
    (fields (mutable spans $rich-text-spans $rich-text-spans-set!)
            (mutable tail $rich-text-tail $rich-text-tail-set!)))

  (define $rich-text-style?
    (lambda (x)
      (or (not x) (rich-style? x) (rich-reset? x))))

  (define $rich-text-span
    (lambda (text style)
      (cons text style)))

  (define $make-rich-text
    (lambda (spans)
      ($make-rich-text-record
       spans
       (and (pair? spans)
            (let loop ([spans spans])
              (if (null? (cdr spans))
                  spans
                  (loop (cdr spans))))))))

  #|proc:rich-text?
  The `rich-text?` procedure returns `#t` when its argument is a rich text
  value, and `#f` otherwise.
  |#
  (define rich-text?
    (lambda (x)
      ($rich-text? x)))

  #|proc:make-rich-text
  The `make-rich-text` procedure constructs a mutable rich text value. When
  `text` is supplied, it is appended as the initial span with optional `style`.
  |#
  (define make-rich-text
    (case-lambda
      [()
       ($make-rich-text '())]
      [(text)
       (make-rich-text text #f)]
      [(text style)
       (pcheck ([string? text] [$rich-text-style? style])
               ($make-rich-text (list ($rich-text-span text style))))]))

  #|proc:rich-text
  The `rich-text` procedure constructs a mutable rich text value from `text`
  and an optional `style`.
  |#
  (define rich-text
    (case-lambda
      [(text)
       (make-rich-text text)]
      [(text style)
       (make-rich-text text style)]))

  #|proc:rich-text-append!
  The `rich-text-append!` procedure appends `text` as a span with optional
  `style` to `rich-text` and returns `rich-text`.
  |#
  (define rich-text-append!
    (case-lambda
      [(rich-text text)
       (rich-text-append! rich-text text #f)]
      [(rich-text text style)
       (pcheck ([rich-text? rich-text] [string? text] [$rich-text-style? style])
               (let ([node (list ($rich-text-span text style))])
                 (if ($rich-text-tail rich-text)
                     (begin
                       (set-cdr! ($rich-text-tail rich-text) node)
                       ($rich-text-tail-set! rich-text node))
                     (begin
                       ($rich-text-spans-set! rich-text node)
                       ($rich-text-tail-set! rich-text node))))
               rich-text)]))

  #|proc:rich-text-plain
  The `rich-text-plain` procedure returns the concatenated text from all spans
  in `rich-text`.
  |#
  (define rich-text-plain
    (lambda (rich-text)
      (pcheck ([rich-text? rich-text])
              (let loop ([spans ($rich-text-spans rich-text)] [out '()])
                (if (null? spans)
                    (apply string-append (reverse out))
                    (loop (cdr spans) (cons (caar spans) out)))))))

  #|proc:rich-text-render
  The `rich-text-render` procedure returns one rendered segment line preserving
  each span's style.
  |#
  (define rich-text-render
    (lambda (rich-text)
      (pcheck ([rich-text? rich-text])
              (list
               (map (lambda (span)
                      (rich-segment (car span) (cdr span)))
                    ($rich-text-spans rich-text)))))))
