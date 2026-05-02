#!chezscheme
(import (chezpp))

(rich-console console
  :color-system 'standard
  :width 80)

(rich-print console
            (rich-style 'bold 'green)
            "Chezpp Rich"
            (reset-style)
            "\n\n")

(rich-panel status-panel
  :title "Status"
  :body "terminal rendering is available"
  :box 'rounded)

(rich-print console status-panel "\n\n")

(rich-table build-table
  :title "Build"
  :columns ("Step" "Result")
  :rows (("compile" "ok")
         ("test" "ok")))

(rich-print console build-table "\n")
