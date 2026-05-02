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

(mat rich-text

     (let ([t (rich-text "hello" (rich-style 'green))])
       (and (rich-text? t)
            (equal? "hello" (rich-text-plain t))))

     (let ([t (make-rich-text)])
       (rich-text-append! t "a" (rich-style 'red))
       (rich-text-append! t "b")
       (equal? "ab" (rich-text-plain t)))

     (let* ([red (rich-style 'red)]
            [blue (rich-style 'blue)]
            [t (make-rich-text)])
       (rich-text-append! t "a" red)
       (rich-text-append! t "b" blue)
       (let ([segments (car (rich-text-render t))])
         (and (equal? "ab" (rich-segments->plain segments))
              (eq? red (rich-segment-style (car segments)))
              (eq? blue (rich-segment-style (cadr segments))))))

     (let ([p (open-output-string)])
       (rich-console c :output-port p :color-system 'standard)
       (rich-print c (rich-text "ok" (rich-style 'green)))
       (equal? "\033[32mok\033[0m" (get-output-string p)))

     (equal? "ok" (rich-export-text (rich-text "ok" (rich-style 'green))))

     )

(mat rich-pretty

     (equal? "(1 2 3)" (rich-export-text '(1 2 3)))
     (equal? "#(a b)" (rich-export-text '#(a b)))
     (equal? "\"x\"" (rich-export-text "x"))
     (equal? "x" (with-output-to-string (lambda () (rich-print "x"))))
     (equal? "(1 2 3)" (with-output-to-string
                         (lambda () (rich-print '(1 2 3)))))

     (let ([cycle (list 'a)])
       (set-cdr! cycle cycle)
       (equal? "#0=(a . #0#)" (rich-export-text cycle)))

     )

(mat rich-basic-renderables

     (equal? "--- Build ----"
             (let ()
               (rich-rule r :title "Build" :width 14)
               (rich-export-text r)))

     (= 14
        (string-length
         (let ()
           (rich-rule r :title "Build" :width 14)
           (rich-export-text r))))

     (equal? "  x  "
             (let ()
               (rich-padding p :body "x" :left 2 :right 2)
               (rich-export-text p)))

     (equal? "  x   "
             (let ()
               (rich-align a :body "x" :width 6 :align 'center)
               (rich-export-text a)))

     (equal? "a  bb\nccc"
             (let ()
               (rich-columns c :items ("a" "bb" "ccc") :width 5 :gap 2)
               (rich-export-text c)))

     (equal? "a  b"
             (let ()
               (rich-columns c :items (list "a" "b") :width 10 :gap 2)
               (rich-export-text c)))

     (equal? "\033[31mx\033[0m  y"
             (let ()
               (rich-columns c
                 :items ((rich-text "x" (rich-style 'red)) "y")
                 :width 10
                 :gap 2)
               (rich-export-ansi c)))

     (let ()
       (rich-layout root :direction 'row :items ("top" "bottom"))
       (string-contains? (rich-export-text root) "top" "bottom"))

     (equal? "top bottom"
             (let ()
               (rich-layout root :direction 'row :items (list "top" "bottom"))
               (rich-export-text root)))

     (equal? "\033[31mtop\033[0m bottom"
             (let ()
               (rich-layout root
                 :direction 'row
                 :items ((rich-text "top" (rich-style 'red)) "bottom"))
               (rich-export-ansi root)))

     (let ([chars (rich-box-chars 'ascii)])
       (vector-set! chars 0 "!")
       (equal? "+" (vector-ref (rich-box-chars 'ascii) 0)))

     )

(mat rich-basic-errors

     ;; Rule width must be positive.
     (error? (eval '(let () (rich-rule r :width 0) r)))

     ;; Padding values must be non-negative.
     (error? (eval '(let () (rich-padding p :body "x" :left -1) p)))

     ;; Alignment must be left, center, or right.
     (error? (eval '(let () (rich-align a :body "x" :align 'middle) a)))

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
                        (rich-print 'rich-render-styled-segment-test))))
            (equal? "\033[31mx\033[0m"
                    (rich-export-ansi 'rich-render-styled-segment-test))))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :color-system 'standard)
       (rich-print c (rich-style) (reset-style) "x")
       (equal? "x" (get-output-string p)))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :color-system 'standard)
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-outer-style-test))
        (lambda (value)
          (list (list (rich-segment "S" (rich-style 'blue))))))
       (rich-print c
                   (rich-style 'red)
                   "A"
                   'rich-render-outer-style-test
                   "B"
                   (reset-style))
       (equal? "\033[31mA\033[34mS\033[0m\033[31mB\033[0m"
               (get-output-string p)))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :color-system 'standard)
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-segment-reset-test))
        (lambda (value)
          (list (list (rich-segment "S" (reset-style))))))
       (rich-print c
                   (rich-style 'red)
                   "A"
                   'rich-render-segment-reset-test
                   "B"
                   (reset-style))
       (equal? "\033[31mA\033[0mS\033[31mB\033[0m"
               (get-output-string p)))

     (let ([p (open-output-string)])
       (rich-console c
         :output-port p
         :color-system 'standard)
       (rich-register-renderer!
        (lambda (value) (eq? value 'rich-render-empty-segment-style-test))
        (lambda (value)
          (list (list (rich-segment "S" (rich-style))))))
       (rich-print c
                   (rich-style 'red)
                   "A"
                   'rich-render-empty-segment-style-test
                   "B"
                   (reset-style))
       (equal? "\033[31mASB\033[0m" (get-output-string p)))

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
