;; This file is used to mask out those unwanted names in (chezscheme).
(library (chezpp chez)
  (export (rename
           ;; os.ss
           (sleep $sleep)))
  (import (chezscheme))

  (export (import (except (chezscheme)
                          sleep
                          ;; file.ss
                          file-access-time file-change-time file-modification-time))))
