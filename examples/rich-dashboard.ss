#!chezscheme
(import (chezpp))

(define-record-type (dashboard-lines make-dashboard-lines dashboard-lines?)
  (fields (immutable lines dashboard-lines-lines)))

(rich-register-renderer! dashboard-lines? dashboard-lines-lines)

(define section
  (lambda (console title)
    (rich-println console)
    (let ([rule (make-rich-rule title 82 'double)])
      (rich-print console (rich-style 'bold #x66ddff) rule (reset-style) "\n"))))

(define print-lines
  (lambda (console lines)
    (rich-print console (make-dashboard-lines lines))
    (rich-println console)))

(define run-prompt-demo
  (lambda (console)
    (let ([in (open-input-string "us-east\nblue\nrobot-token\nn\n")]
          [out (open-output-string)])
      (let ([prompt-console (make-rich-console)])
        (rich-console-input-port-set! prompt-console in)
        (rich-console-output-port-set! prompt-console out)
        (rich-console-color-system-set! prompt-console 'none)
      (let* ([region (rich-prompt prompt-console "Region" "local" '("us-east" "eu-west" "local"))]
             [ring (rich-prompt prompt-console "Ring" "green" '("blue" "green"))]
             [secret (rich-password prompt-console "Robot token")]
             [approved? (rich-confirm prompt-console "Open traffic?")])
        (let ([prompt-table (make-rich-table)])
          (rich-table-title-set! prompt-table "Automated Prompt Session")
          (rich-table-box-set! prompt-table 'square)
          (rich-table-add-column! prompt-table "Field")
          (rich-table-add-column! prompt-table "Value")
          (rich-table-add-row! prompt-table "region" region)
          (rich-table-add-row! prompt-table "ring" ring)
          (rich-table-add-row! prompt-table "token" secret)
          (rich-table-add-row! prompt-table "approved" (if approved? "true" "false"))
          (rich-table-add-row! prompt-table "prompt transcript" (get-output-string out))
          (rich-print console prompt-table "\n")))))))

(define run-progress-demo
  (lambda (console)
    (let ([deploy (make-rich-progress 24 (rich-progress-default-columns))])
      (let ([assets (rich-progress-add-task! deploy "assets" 8)]
            [migrate (rich-progress-add-task! deploy "migrations" 5)]
            [workers (rich-progress-add-task! deploy "workers" #f)])
        (let ([live (make-rich-live deploy console 0 #f)])
          (let loop ([i 0])
            (when (<= i 8)
              (rich-progress-update! deploy assets i)
              (rich-progress-update! deploy migrate (min i 5))
              (rich-progress-advance! deploy workers 1)
              (rich-live-refresh! live)
              (milisleep 500)
              (loop (+ i 1))))
          (rich-progress-complete! deploy assets)
          (rich-progress-complete! deploy migrate)
          (rich-live-refresh! live)
          (rich-live-stop! live)
          (rich-println console "\n"))))))

(define console (make-rich-console))
(rich-console-width-set! console 82)
(rich-console-color-system-set! console 'standard)
(rich-console-force-terminal?-set! console #t)

(rich-theme dashboard-theme
  :good (rich-style 'bold 'green)
  :busy (rich-style 'bold 'yellow)
  :bad (rich-style 'bold 'red)
  :muted (rich-style 'dim)
  :accent (rich-style 'bold #x00b7ff))
(rich-console-theme-set! console dashboard-theme)

(rich-print console
            (rich-theme-ref dashboard-theme 'accent)
            "Operations Dashboard Example"
            (reset-style)
            "\n")
(rich-print console
            (rich-style 'dim)
            "A second complete Rich script with different data and layout choices."
            (reset-style)
            "\n")

(section console "Styled Signals")
(define status-text (rich-text "api " (rich-theme-ref dashboard-theme 'muted)))
(rich-text-append! status-text "healthy " (rich-theme-ref dashboard-theme 'good))
(rich-text-append! status-text "queue " (rich-theme-ref dashboard-theme 'muted))
(rich-text-append! status-text "busy " (rich-theme-ref dashboard-theme 'busy))
(rich-text-append! status-text "cache " (rich-theme-ref dashboard-theme 'muted))
(rich-text-append! status-text "warm" (rich-style 'underline 'cyan))
(rich-print console status-text "\n")
(rich-print console
            (rich-style 'bold 'white '(bg #x224466))
            "region: us-east"
            (reset-style)
            " "
            (rich-style 'italic #xffcc33)
            "canary ring active"
            (reset-style)
            "\n")

(section console "Wrapping And Cropping")
(let* ([story "the deployment note wraps across several short lines while preserving magenta styling"]
       [wrapped (rich-segment-wrap (list (rich-segment story (rich-style 'magenta))) 22)]
       [cropped (rich-segment-crop
                 (list (rich-segment "incident-2026-05-02/very-long-service-name"
                                     (rich-style 'yellow)))
                 26)])
  (print-lines console wrapped)
  (print-lines console (list cropped))
  (rich-print console "visible width check: "
              (rich-segments-width (list (rich-segment "deploy")
                                         (rich-segment "\033[1m" #f #t)))
              "\n"))

(section console "Rules, Alignment, Padding")
(rich-rule deploy-rule :title "release train" :width 58 :style 'rounded)
(rich-align left :body "left: logs" :width 38 :align 'left)
(rich-align center :body "center: service map" :width 38 :align 'center)
(rich-align right :body "right: alerts" :width 38 :align 'right)
(rich-padding spacer
  :body (rich-text "padded command output" (rich-style 'green))
  :top 1
  :right 6
  :bottom 1
  :left 6)
(rich-print console deploy-rule "\n" left "\n" center "\n" right "\n" spacer "\n")

(section console "Panels, Boxes, Columns")
(rich-panel api-panel
  :title "api"
  :body (rich-text "p95 82ms\nerrors 0.03%" (rich-style 'green))
  :box 'ascii)
(rich-panel queue-panel
  :title "queue"
  :body (rich-text "depth 142\nworkers 18" (rich-style 'yellow))
  :box 'square)
(rich-panel cache-panel
  :title "cache"
  :body (rich-text "hit 97%\nmiss 3%" (rich-style 'cyan))
  :box 'rounded)
(rich-panel deploy-panel
  :title "deploy"
  :body (rich-text "ring blue\nversion 42" (rich-style 'bold 'magenta))
  :box 'double)
(rich-panel alert-panel
  :title "alerts"
  :body (rich-text "none active" (rich-style 'bold 'green))
  :box 'heavy)
(rich-columns service-cards
  :items (api-panel queue-panel cache-panel deploy-panel alert-panel )
  :width 82
  :gap 2)
(rich-print console service-cards "\n")

(section console "Tables")
(rich-table service-table
  :title "Service Health"
  :caption "Double box, styled cells, and multiline table values"
  :box 'double
  :show-header? #t
  :show-lines? #t
  :columns ("Service" "State" "Detail")
  :rows (("api" (rich-text "green" (rich-theme-ref dashboard-theme 'good)) "p95 82ms")
         ("queue" (rich-text "yellow" (rich-theme-ref dashboard-theme 'busy)) "142 jobs\n18 workers")
         ("edge" (rich-text "green" (rich-theme-ref dashboard-theme 'good)) "2 regions")))
(rich-print console service-table "\n")

(section console "Tree, Layout, Pretty")
(rich-tree topology :label (rich-text "platform" (rich-style 'bold 'blue)))
(let ([edge (rich-tree-add! topology "edge")])
  (rich-tree-add! edge "cdn")
  (rich-tree-add! edge "waf"))
(let ([core (rich-tree-add! topology (rich-text "core" (rich-style 'cyan)))])
  (rich-tree-add! core "api")
  (rich-tree-add! core "queue")
  (rich-tree-add! core "database\nreplica"))
(rich-print console topology "\n")
(rich-layout horizontal
  :direction 'row
  :items ((rich-text "ingest" (rich-style 'green))
          (rich-text "process" (rich-style 'yellow))
          (rich-text "serve" (rich-style 'cyan))))
(rich-layout vertical
  :direction 'column
  :items ("pipeline" horizontal "observability"))
(rich-panel layout-panel
  :title "layout"
  :body vertical
  :box 'rounded)
(rich-panel pretty-panel
  :title "pretty datum"
  :body '(dashboard (region . us-east) (ring . blue) #(api queue edge))
  :box 'square)
(rich-print console layout-panel "\n" pretty-panel "\n")

(section console "Live Status And Progress")
(let ([now 0])
  (parameterize ([rich-current-time (lambda () now)])
    (let ([status (make-rich-status "Sampling telemetry"
                                    console
                                    (list "◐" "◓" "◑" "◒")
                                    1)])
      (let ([live (make-rich-live status console 0 #f)])
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 1)
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 2)
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 3)
        (rich-live-refresh! live)
        (rich-println console)))))
(run-progress-demo console)

(section console "Prompts")
(run-prompt-demo console)

(section console "Exports")
(let ([plain (rich-export-text service-table)]
      [ansi (rich-export-ansi (rich-text "dashboard ansi export" (rich-style 'bold 'green)))])
  (rich-table export-table
    :box 'rounded
    :columns ("Capability" "Value")
    :rows (("text export length" (number->string (string-length plain)))
           ("ansi export" ansi)
           ("registered renderer" (if (rich-renderable? service-table) "yes" "no"))
           ("color system" (if (rich-color-system? 'standard) "standard" "unknown"))))
  (rich-print console export-table "\n"))
