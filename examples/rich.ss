#!chezscheme
(import (chezpp))

(define-record-type (example-lines make-example-lines example-lines?)
  (fields (immutable lines example-lines-lines)))

(rich-register-renderer! example-lines? example-lines-lines)

(define section
  (lambda (console title)
    (rich-println console)
    (let ([rule (make-rich-rule title 76 'heavy)])
      (rich-print console (rich-style 'bold 'cyan) rule (reset-style) "\n"))))

(define print-lines
  (lambda (console lines)
    (rich-print console (make-example-lines lines))
    (rich-println console)))

(define prompt-demo
  (lambda (console)
    (let ([in (open-input-string "release\n\ns3cr3t\nyes\n")]
          [out (open-output-string)])
      (let ([prompt-console (make-rich-console)])
        (rich-console-input-port-set! prompt-console in)
        (rich-console-output-port-set! prompt-console out)
        (rich-console-color-system-set! prompt-console 'none)
      (let* ([mode (rich-prompt prompt-console "Mode" "debug" '("debug" "release"))]
             [target (rich-prompt prompt-console "Target" "native")]
             [password (rich-password prompt-console "Token")]
             [confirmed? (rich-confirm prompt-console "Deploy?")])
        (let ([prompt-table (make-rich-table)])
          (rich-table-title-set! prompt-table "Prompt APIs")
          (rich-table-box-set! prompt-table 'rounded)
          (rich-table-add-column! prompt-table "API")
          (rich-table-add-column! prompt-table "Result")
          (rich-table-add-row! prompt-table "rich-prompt / choices" mode)
          (rich-table-add-row! prompt-table "rich-prompt / default" target)
          (rich-table-add-row! prompt-table "rich-password / string port" password)
          (rich-table-add-row! prompt-table "rich-confirm" (if confirmed? "yes" "no"))
          (rich-table-add-row! prompt-table "captured prompt text" (get-output-string out))
          (rich-print console prompt-table "\n")))))))

(define live-demo
  (lambda (console)
    (let ([progress (make-rich-progress 18 (rich-progress-default-columns))])
      (let ([compile-id (rich-progress-add-task! progress "compile" 5)]
            [test-id (rich-progress-add-task! progress "test" 4)])
        (let ([live (make-rich-live progress console 0 #f)])
          (let loop ([i 0])
            (when (<= i 5)
              (rich-progress-update! progress compile-id i)
              (rich-progress-update! progress test-id (min i 4))
              (rich-live-refresh! live)
              (milisleep 500)
              (loop (+ i 1))))
          (rich-live-stop! live)
          (rich-println console "\n"))))))

(define console (make-rich-console))
(rich-console-width-set! console 76)
(rich-console-color-system-set! console 'standard)

(rich-theme palette
  :ok (rich-style 'bold 'green)
  :warn (rich-style 'yellow)
  :hot (rich-style 'bold #xff6600)
  :dim (rich-style 'dim))
(rich-console-theme-set! console palette)

(rich-print console
            (rich-style 'bold 'green)
            "Chezpp Rich Terminal Showcase"
            (reset-style)
            "\n")
(rich-print console
            (rich-style 'italic 'cyan)
            "Styles are opaque objects; the console owns ANSI encoding."
            (reset-style)
            "\n")

(section console "Styles, Themes, Text")
(rich-print console
            (rich-style 'bold 'red)
            "bold red"
            (reset-style)
            " | "
            (rich-style 'underline 'blue)
            "underlined blue"
            (reset-style)
            " | "
            (rich-style 'reverse 'white '(bg #x303030))
            "reverse on truecolor background"
            (reset-style)
            "\n")
(let ([text (rich-text "stream " (rich-theme-ref palette 'dim))])
  (rich-text-append! text "compile " (rich-theme-ref palette 'hot))
  (rich-text-append! text "ok" (rich-theme-ref palette 'ok))
  (rich-print console text "\n"))
(rich-print console
            "Standard ANSI for "
            (rich-style 'bold #x33ccff)
            "truecolor cyan"
            (reset-style)
            ": "
            (rich-export-ansi (rich-text "sample" (rich-style #x33ccff)))
            "\n")

(section console "Segments, Wrapping, Cropping")
(let* ([wrapped (rich-segment-wrap
                 (list (rich-segment "wrapped styled segments keep their style while flowing into a narrow column"
                                     (rich-style 'magenta)))
                 24)]
       [cropped (rich-segment-crop
                 (list (rich-segment "cropped output remains styled"
                                     (rich-style 'green)))
                 14)])
  (print-lines console wrapped)
  (print-lines console (list cropped))
  (rich-print console
              "Plain width: "
              (rich-segments-width (list (rich-segment "abc")
                                         (rich-segment "\033[31m" #f #t)))
              ", stripped ANSI: "
              (rich-strip-ansi "\033[31mred\033[0m")
              "\n"))

(section console "Rules, Padding, Alignment")
(rich-rule centered-rule :title "centered heavy rule" :width 50 :style 'heavy)
(rich-padding padded
  :body (rich-text "padded body with styled text" (rich-style 'yellow))
  :top 1
  :right 4
  :bottom 1
  :left 4)
(rich-align left-align :body "left aligned" :width 34 :align 'left)
(rich-align center-align :body "center aligned" :width 34 :align 'center)
(rich-align right-align :body "right aligned" :width 34 :align 'right)
(rich-print console centered-rule "\n" padded "\n")
(rich-print console left-align "\n" center-align "\n" right-align "\n")

(section console "Boxes, Panels, Tables")
(rich-panel ascii-panel
  :title "ascii"
  :body "portable box"
  :box 'ascii)
(rich-panel rounded-panel
  :title "rounded"
  :body (rich-text "soft corners" (rich-style 'green))
  :box 'rounded)
(rich-panel heavy-panel
  :title "heavy"
  :body (rich-text "high contrast" (rich-style 'bold 'red))
  :box 'heavy)
(rich-panel double-panel
  :title "double"
  :body (rich-text "classic frame" (rich-style 'cyan))
  :box 'double)
(rich-columns box-columns
  :items (ascii-panel rounded-panel heavy-panel double-panel)
  :width 76
  :gap 3)
(rich-print console box-columns "\n")

(rich-table build-table
  :title "Build Matrix"
  :caption "Headers, captions, padding, show-lines, and styled cells"
  :box 'double
  :show-header? #t
  :show-lines? #t
  :padding 1
  :columns ("Stage" "Status" "Notes")
  :rows (("parse" (rich-text "ok" (rich-style 'green)) "reader + macro expansion")
         ("compile" (rich-text "warm" (rich-style 'yellow)) "whole-program optimization")
         ("test" (rich-text "pass" (rich-style 'bold 'green)) "rich, string, list")))
(rich-print console build-table "\n")

(section console "Tree And Pretty Printing")
(rich-tree project-tree :label (rich-text "chezpp" (rich-style 'bold 'blue)))
(let ([rich-node (rich-tree-add! project-tree (rich-text "rich/" (rich-style 'cyan)))])
  (rich-tree-add! rich-node "style.ss")
  (rich-tree-add! rich-node "console.ss")
  (rich-tree-add! rich-node (rich-text "progress.ss\nprompt.ss" (rich-style 'green))))
(let ([examples-node (rich-tree-add! project-tree "examples/")])
  (rich-tree-add! examples-node "rich.ss")
  (rich-tree-add! examples-node "rich-dashboard.ss"))
(rich-print console project-tree "\n")
(rich-panel pretty-panel
  :title "pretty"
  :body '(rich (styles . opaque) (segments . rendered) #(tables panels trees))
  :box 'square)
(rich-print console pretty-panel "\n")

(section console "Columns And Layout")
(rich-layout row-layout
  :direction 'row
  :items ((rich-text "left pane" (rich-style 'bold 'green))
          (rich-text "middle pane" (rich-style 'bold 'yellow))
          (rich-text "right pane" (rich-style 'bold 'cyan))))
(rich-layout column-layout
  :direction 'column
  :items ((rich-text "top area" (rich-style 'magenta))
          row-layout
          (rich-text "bottom area" (rich-style 'blue))))
(rich-panel layout-panel
  :title "layout"
  :body column-layout
  :box 'rounded
  :padding 2)
(rich-print console layout-panel "\n")

(section console "Live, Status, Progress")
(let ([now 0])
  (parameterize ([rich-current-time (lambda () now)])
    (let ([status (make-rich-status "Indexing deterministic demo data"
                                    console
                                    (list "◐" "◓" "◑" "◒")
                                    0.25)])
      (let ([live (make-rich-live status console 0 #f)])
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 0.25)
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 0.5)
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 0.75)
        (rich-live-refresh! live)
        (rich-println console)))))
(live-demo console)

(section console "Prompts And Passwords")
(prompt-demo console)

(section console "Exports And Custom Renderers")
(let ([plain (rich-export-text build-table)]
      [ansi (rich-export-ansi (rich-text "exported ansi" (rich-style 'bold 'green)))])
  (rich-table export-table
    :box 'rounded
    :columns ("Export" "Preview")
    :rows (("text length" (number->string (string-length plain)))
           ("ansi value" ansi)
           ("renderer?" (if (rich-renderable? build-table) "registered" "plain"))))
  (rich-print console export-table "\n"))
