#!chezscheme
(library (chezpp rich segment)
  (export rich-segment
          make-rich-segment
          rich-segment?
          rich-segment-text
          rich-segment-style
          rich-segment-control?
          rich-segment-width
          rich-segments-width
          rich-segments->plain
          rich-strip-ansi
          rich-segment-wrap
          rich-segment-crop
          rich-segment-pad-right)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp rich private common)
          (chezpp rich style))

  (define-record-type (rich-segment-record $make-rich-segment $rich-segment?)
    (fields (immutable text $rich-segment-text)
            (immutable style $rich-segment-style)
            (immutable control? $rich-segment-control?)))

  (define $rich-segment-style?
    (lambda (x)
      (or (not x) (rich-style? x) (rich-reset? x))))

  (define $rich-segment-list?
    (lambda (x)
      (and (list? x) (rich-list-every? rich-segment? x))))

  (define $substring-end
    (lambda (str n)
      (substring str 0 (min n (string-length str)))))

  (define $csi-open-char (integer->char 91))

  (define $semicolon-char (integer->char 59))

  #|proc:rich-segment?
  The `rich-segment?` procedure returns `#t` when its argument is a rich text
  segment, and `#f` otherwise.
  |#
  (define rich-segment?
    (lambda (x)
      ($rich-segment? x)))

  #|proc:rich-segment-text
  The `rich-segment-text` procedure returns the source text stored in
  `segment`.
  |#
  (define rich-segment-text
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              ($rich-segment-text segment))))

  #|proc:rich-segment-style
  The `rich-segment-style` procedure returns the style object, reset object, or
  `#f` stored in `segment`.
  |#
  (define rich-segment-style
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              ($rich-segment-style segment))))

  #|proc:rich-segment-control?
  The `rich-segment-control?` procedure returns whether `segment` carries
  control text that should not contribute visible width.
  |#
  (define rich-segment-control?
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              ($rich-segment-control? segment))))

  #|proc:make-rich-segment
  The `make-rich-segment` procedure constructs a text segment from `text`, an
  optional style or reset object, and an optional control flag.
  |#
  (define make-rich-segment
    (case-lambda
      [(text)
       (make-rich-segment text #f #f)]
      [(text style)
       (make-rich-segment text style #f)]
      [(text style control?)
       (pcheck ([string? text] [$rich-segment-style? style] [boolean? control?])
               ($make-rich-segment text style control?))]))

  #|proc:rich-segment
  The `rich-segment` procedure constructs a rich text segment.
  |#
  (define rich-segment make-rich-segment)

  #|proc:rich-strip-ansi
  The `rich-strip-ansi` procedure removes ANSI CSI SGR escape sequences from
  `text`.
  |#
  (define rich-strip-ansi
    (lambda (text)
      (pcheck ([string? text])
              (let ([len (string-length text)])
                (let loop ([i 0] [out '()])
                  (cond [(= i len)
                         (list->string (reverse out))]
                        [(and (char=? (string-ref text i) #\esc)
                              (< (+ i 2) len)
                              (char=? (string-ref text (+ i 1)) $csi-open-char))
                         (let scan ([j (+ i 2)])
                           (cond [(= j len)
                                  (loop (+ i 1) (cons (string-ref text i) out))]
                                 [(char=? (string-ref text j) #\m)
                                  (loop (+ j 1) out)]
                                 [(or (char-numeric? (string-ref text j))
                                      (char=? (string-ref text j) $semicolon-char))
                                  (scan (+ j 1))]
                                 [else
                                  (loop (+ i 1) (cons (string-ref text i) out))]))]
                        [else
                         (loop (+ i 1) (cons (string-ref text i) out))]))))))

  #|proc:rich-segment-width
  The `rich-segment-width` procedure returns the visible width of `segment`.
  Control segments have width zero.
  |#
  (define rich-segment-width
    (lambda (segment)
      (pcheck ([rich-segment? segment])
              (if (rich-segment-control? segment)
                  0
                  (string-length (rich-strip-ansi (rich-segment-text segment)))))))

  #|proc:rich-segments-width
  The `rich-segments-width` procedure returns the sum of visible widths for
  `segments`.
  |#
  (define rich-segments-width
    (lambda (segments)
      (pcheck ([$rich-segment-list? segments])
              (let loop ([segments segments] [width 0])
                (if (null? segments)
                    width
                    (loop (cdr segments)
                          (+ width (rich-segment-width (car segments)))))))))

  #|proc:rich-segments->plain
  The `rich-segments->plain` procedure concatenates the source text of
  `segments`.
  |#
  (define rich-segments->plain
    (lambda (segments)
      (pcheck ([$rich-segment-list? segments])
              (apply string-append (map rich-segment-text segments)))))

  #|proc:rich-segment-wrap
  The `rich-segment-wrap` procedure wraps `segments` into a list of segment
  lines that fit within positive visible `width`.
  |#
  (define rich-segment-wrap
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-positive-integer? width])
              (let ([text (rich-segments->plain segments)])
                (let ([len (string-length text)])
                  (if (= len 0)
                      (list '())
                      (let loop ([i 0] [lines '()])
                        (if (>= i len)
                            (reverse lines)
                            (let ([end (min len (+ i width))])
                              (loop end
                                    (cons (list (rich-segment (substring text i end)))
                                          lines)))))))))))

  #|proc:rich-segment-crop
  The `rich-segment-crop` procedure crops `segments` to at most `width`
  visible columns.
  |#
  (define rich-segment-crop
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-nonnegative-integer? width])
              (let ([text (rich-segments->plain segments)])
                (if (= width 0)
                    '()
                    (list (rich-segment ($substring-end text width))))))))

  #|proc:rich-segment-pad-right
  The `rich-segment-pad-right` procedure appends spaces to `segments` until
  their visible width is at least `width`.
  |#
  (define rich-segment-pad-right
    (lambda (segments width)
      (pcheck ([$rich-segment-list? segments] [rich-nonnegative-integer? width])
              (let ([current (rich-segments-width segments)])
                (if (>= current width)
                    segments
                    (append segments
                            (list (rich-segment (make-string (- width current) #\space))))))))))
