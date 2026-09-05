(library (chezpp parser private)
  (export define-parser-record-writer)
  (import (chezpp chez))

  #|macro:define-parser-record-writer
  The `define-parser-record-writer` macro registers a labeled writer for `record-type`. `tag` names
  the printed record, and each `label` and `accessor` pair prints one record field.
  |#
  (define-syntax define-parser-record-writer
    (syntax-rules ()
      [(_ record-type tag ([label accessor] ...))
       (define writer-registration
         (record-writer
          (type-descriptor record-type)
          (lambda (record port write-value)
            (display "#[" port)
            (display (quote tag) port)
            (for-each
             (lambda (field-label field-value)
               (display " " port)
               (display field-label port)
               (display ": " port)
               (write-value field-value port))
             (list (quote label) ...)
             (list (accessor record) ...))
            (display "]" port))))]))
  )
