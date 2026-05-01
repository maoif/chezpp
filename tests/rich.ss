#!chezscheme
(import (chezpp)
        (chezpp rich private common))

(mat rich-import

     (procedure? rich-style)
     (procedure? reset-style)
     (procedure? rich-print)
     (procedure? rich-println)
     (procedure? make-rich-console)

     )

(mat rich-print-smoke

     (equal? "123 abc"
             (with-output-to-string
               (lambda ()
                 (rich-print 123 " " "abc"))))

     (equal? "abc\n"
             (with-output-to-string
               (lambda ()
                 (rich-println "abc"))))

     )

(mat rich-style

     (rich-style? (rich-style 'bold 'red))
     (rich-reset? (reset-style))

     (equal? 'red (rich-style-fg (rich-style 'red)))
     (equal? 'blue (rich-style-fg (rich-style 'red 'blue)))
     (equal? 'on-red (rich-style-bg (rich-style 'on-red)))
     (equal? #xffaa00 (rich-style-fg (rich-style #xffaa00)))
     (equal? #x202020 (rich-style-bg (rich-style '(bg #x202020))))

     (equal? "\033[1m\033[31m"
             (rich-style->ansi #f (rich-style 'bold 'red)))

     ;; Later foreground colors override earlier foreground colors.
     (equal? "\033[34m"
             (rich-style->ansi #f (rich-style 'red 'blue)))

     ;; Later attributes override earlier conflicting attributes.
     (equal? ""
             (rich-style->ansi #f (rich-style 'underline 'no-underline)))

     (equal? "\033[38;2;255;170;0m"
             (rich-style->ansi #f (rich-style #xffaa00)))

     (equal? "\033[48;2;32;32;32m"
             (rich-style->ansi #f (rich-style '(bg #x202020))))

     (equal? "x"
             (with-output-to-string
               (lambda ()
                 (rich-print (rich-style 'red) "x" (reset-style)))))

     )

(mat rich-style-errors

     ;; Unknown style symbols are rejected.
     (error? (rich-style 'not-a-style))

     ;; Truecolor integers must be in RGB range.
     (error? (rich-style #x1000000))

     ;; Background color forms must have exactly one color value.
     (error? (rich-style '(bg)))

     )

(mat rich-theme

     (rich-color-system? 'auto)

     (let ()
       (rich-theme theme)
       (rich-theme? theme))

     (let ()
       (rich-theme theme :error (rich-style 'red) :ok (rich-style 'green))
       (and (equal? "\033[31m" (rich-style->ansi #f (rich-theme-ref theme 'error)))
            (equal? "\033[32m" (rich-style->ansi #f (rich-theme-ref theme 'ok)))))

     (let ([theme (make-rich-theme (list (cons 'error (rich-style 'red))))])
       (rich-style? (rich-theme-ref theme 'error)))

     (rich-output-target? (current-output-port))
     (rich-output-target? (make-rich-console))

     )

(mat rich-theme-errors

     ;; Theme alist values must be rich style objects.
     (error? (make-rich-theme (list (cons 'error "red"))))

     ;; Theme mutation values must be rich style objects.
     (error? (let ([theme (make-rich-theme)])
               (rich-theme-set! theme 'error "red")))

     )

(mat rich-segment

     (let ([s (rich-segment "abc")])
       (and (rich-segment? s)
            (= 3 (rich-segment-width s))
            (equal? "abc" (rich-segment-text s))))

     (equal? 5
             (rich-segments-width
              (list (rich-segment "ab")
                    (rich-segment "cde"))))

     (equal? "abcdef"
             (rich-segments->plain
              (list (rich-segment "abc")
                    (rich-segment "def"))))

     (equal? "red"
             (rich-strip-ansi "\033[31mred\033[0m"))

     (equal? (list "ab" "cd" "e")
             (map rich-segments->plain
                  (rich-segment-wrap (list (rich-segment "abcde")) 2)))

     (equal? "abc"
             (rich-segments->plain
              (rich-segment-crop (list (rich-segment "abcdef")) 3)))

     (let* ([style (rich-style 'red)]
            [lines (rich-segment-wrap (list (rich-segment "abcd" style)) 2)]
            [first (car (car lines))]
            [second (car (cadr lines))])
       (and (eq? style (rich-segment-style first))
            (eq? style (rich-segment-style second))
            (equal? "ab" (rich-segment-text first))
            (equal? "cd" (rich-segment-text second))))

     (let* ([control (rich-segment "\033[31m" #f #t)]
            [text (rich-segment "abcd")]
            [wrapped (rich-segment-wrap (list control text) 2)]
            [cropped (rich-segment-crop (list control text) 2)])
       (and (= 0 (rich-segment-width control))
            (rich-segment-control? (car (car wrapped)))
            (equal? "\033[31mab" (rich-segments->plain (car wrapped)))
            (equal? "cd" (rich-segments->plain (cadr wrapped)))
            (rich-segment-control? (car cropped))
            (equal? "\033[31mab" (rich-segments->plain cropped))))

     (begin
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-segment-lines-test))
        (lambda (value)
          (list (list (rich-segment "ab"))
                (list (rich-segment "cd")))))
       (equal? "ab\ncd"
               (with-output-to-string
                 (lambda ()
                   (rich-print 'rich-render-segment-lines-test)))))

     )

(mat rich-segment-errors

     ;; Segment text must be a string.
     (error? (rich-segment 123))

     ;; Wrap width must be positive.
     (error? (rich-segment-wrap (list (rich-segment "x")) 0))

     ;; Renderer results must be strings or segment-line lists.
     (error? (begin
               (rich-register-renderer!
                (lambda (value) (eq? value 'rich-render-invalid-test))
                (lambda (value) 123))
               (rich-print 'rich-render-invalid-test)))

     ;; rich-render rejects invalid renderer results too.
     (error? (begin
               (rich-register-renderer!
                (lambda (value) (eq? value 'rich-render-invalid-render-test))
                (lambda (value) 123))
               (rich-render (make-rich-console) 'rich-render-invalid-render-test)))

     )

(mat rich-console

     (let ([p (open-output-string)])
       (let ([c (make-rich-console)])
         (rich-console-output-port-set! c p)
         (rich-console-width-set! c 12)
         (rich-print c "ok")
         (equal? "ok" (get-output-string p))))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :width 20
         :color-system 'none)
       (and (rich-console? c)
            (= 20 (rich-console-width c))
            (begin
              (rich-print c (rich-style 'red) "x" (reset-style))
              (equal? "x" (get-output-string p)))))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :color-system 'standard)
       (rich-print c (rich-style 'red) "x" (reset-style))
       (equal? "\033[31mx\033[0m" (get-output-string p)))

     (let ()
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-styled-segment-test))
        (lambda (value)
          (list (list (rich-segment "x" (rich-style 'red))))))
       (and (let ([p (open-output-string)])
              (rich-console c
                :output-port p
                :color-system 'standard)
              (rich-print c 'rich-render-styled-segment-test)
              (equal? "\033[31mx\033[0m" (get-output-string p)))
            (let ([p (open-output-string)])
              (rich-console c
                :output-port p
                :color-system 'none)
              (rich-print c 'rich-render-styled-segment-test)
              (equal? "x" (get-output-string p)))
            (equal? "x"
                    (with-output-to-string
                      (lambda ()
                        (rich-print 'rich-render-styled-segment-test))))))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :color-system 'standard)
       (rich-print c (rich-style) (reset-style) "x")
       (equal? "x" (get-output-string p)))

     )

(mat rich-console-errors

     ;; Console width must be positive.
     (error? (rich-console c :width 0))

     ;; Console output port must be an output port.
     (error? (rich-console c :output-port "not-a-port"))

     ;; Unknown constructor macro fields are rejected.
     (error? (eval '(let () (rich-console c :unknown 1) c)))

     ;; Console input port must be an input port.
     (error? (rich-console c :input-port "not-a-port"))

     ;; Console color system must be supported.
     (error? (rich-console c :color-system 'unsupported))

     )
