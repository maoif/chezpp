#!chezscheme
(import (chezpp))

(define demo-width 72)

(define demo-section
  (lambda (title)
    (newline)
    (rich-rule-println (rich-rule title demo-width 'unicode))))

(define demo-styles
  (lambda ()
    (demo-section "Styles and formatted output")
    (rich-println "~a~a~a ~a~a~a ~a~a~a ~a~a~a"
                  (rich-style 'bold 'bright-red)
                  "bold red"
                  rich-reset
                  (rich-style 'underline 'bright-blue)
                  "underlined blue"
                  rich-reset
                  (rich-style 'italic 'green)
                  "italic green"
                  rich-reset
                  (rich-style 'reverse 'yellow)
                  "reverse yellow"
                  rich-reset)
    (rich-println "~a~a~a"
                  (rich-style 'bg-blue 'bright-white 'bold)
                  " composed foreground/background styles "
                  rich-reset)
    (rich-println "Color disabled: ~a"
                  (parameterize ([rich-enable-color? #f])
                    (rich-format "~a~a~a"
                                 (rich-style 'bold 'red)
                                 "plain text"
                                 rich-reset)))))

(define demo-tables
  (lambda ()
    (demo-section "Tables")
    (let ([table (make-rich-table)])
      (rich-table-add-column! table "Feature")
      (rich-table-add-column! table "API")
      (rich-table-add-column! table "Status")
      (rich-table-add-row! table
                           "Styled print"
                           "rich-format / rich-println"
                           (rich-format "~a~a~a"
                                        (rich-style 'green)
                                        "ready"
                                        rich-reset))
      (rich-table-add-row! table "Tables" "make-rich-table" "ready")
      (rich-table-add-row! table "Panels" "rich-panel" "ready")
      (rich-table-add-row! table "Progress" "make-rich-progress" "ready")
      (rich-table-border-style-set! table 'unicode)
      (rich-table-println table))))

(define demo-panels
  (lambda ()
    (demo-section "Panels, rules, alignment, and padding")
    (rich-panel-println
     (rich-panel
      (rich-align-render
       (rich-align
        (rich-format "~aChezpp Rich~a\nformatted terminal output for Scheme"
                     (rich-style 'bold 'bright-cyan)
                     rich-reset)
        46
        'center))
      "Panel"
      'unicode))
    (rich-padding-println
     (rich-padding
      (rich-format "~aPadding~a composes with any rendered string."
                   (rich-style 'bold 'magenta)
                   rich-reset)
      1
      4))))

(define demo-tree
  (lambda ()
    (demo-section "Tree")
    (let* ([root (make-rich-tree "chezpp" 'unicode)]
           [rich (rich-tree-add! root "rich")]
           [examples (rich-tree-add! root "examples")])
      (rich-tree-add! rich "styles")
      (rich-tree-add! rich "tables")
      (rich-tree-add! rich "panels")
      (rich-tree-add! rich "progress")
      (rich-tree-add! examples "rich.ss")
      (rich-tree-println root))))

(define demo-columns
  (lambda ()
    (demo-section "Columns")
    (rich-columns-println
     (rich-columns
      (list "rich-print"
            "rich-panel"
            "rich-tree"
            "rich-rule"
            "rich-align"
            "rich-padding"
            "rich-columns"
            "rich-status"
            "rich-progress")
      demo-width
      4))))

(define demo-status
  (lambda ()
    (demo-section "Status")
    (let ([port (current-output-port)]
          [status (make-rich-status "warming up demo"
                                    '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧")
                                    0.05)])
      (dynamic-wind
        (lambda () (rich-status-start! port status 50))
        (lambda ()
          (milisleep 220)
          (rich-status-message-set! status "rendering examples")
          (milisleep 220)
          (rich-status-message-set! status "done")
          (milisleep 120))
        (lambda ()
          (rich-status-stop! status)
          (display "\r\033[2K" port)
          (rich-status-ffinish! port status))))))

(define demo-progress
  (lambda ()
    (demo-section "Progress")
    (let* ([progress (make-rich-progress 24)]
           [compile-id (rich-progress-add-task! progress "compile" 100)]
           [test-id (rich-progress-add-task! progress "test" 80)]
           [port (current-output-port)])
      (rich-progress-columns-set!
       progress
       (list (rich-progress-spinner-column
              '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧")
              0.05)
             (rich-progress-text-column "{description}")
             (rich-progress-bar-column "=" "." "~")
             (rich-progress-percent-column)
             (rich-progress-complete-column)
             (rich-progress-elapsed-column)))
      (dynamic-wind
        (lambda () (rich-progress-start! port progress 50))
        (lambda ()
          (let loop ([step 0])
            (when (< step 5)
              (rich-progress-update! progress compile-id (* step 20))
              (rich-progress-update! progress test-id (* step 16))
              (milisleep 150)
              (loop (+ step 1))))
          (rich-progress-complete! progress compile-id)
          (rich-progress-complete! progress test-id)
          (milisleep 120))
        (lambda ()
          (rich-progress-stop! progress)
          (display "\r\033[2K\033[1A\r\033[2K" port)
          (rich-progress-ffinish! port progress))))))

(define main
  (lambda ()
    (rich-println "~aChezpp Rich demo~a"
                  (rich-style 'bold 'bright-white 'bg-blue)
                  rich-reset)
    (demo-styles)
    (demo-tables)
    (demo-panels)
    (demo-tree)
    (demo-columns)
    (demo-status)
    (demo-progress)))

(main)
