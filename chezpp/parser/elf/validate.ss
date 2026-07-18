(library (chezpp parser elf validate)
  (export make-elf-issue elf-issue? elf-issue-offset elf-issue-message
          elf-power-of-two? elf-range-valid?)
  (import (chezpp chez))

  (define-record-type elf-issue
    (fields (immutable offset) (immutable message)))

  (define elf-power-of-two?
    (lambda (value)
      (or (= value 0)
          (and (positive? value) (= 0 (logand value (- value 1)))))))

  (define elf-range-valid?
    (lambda (offset size length)
      (and (integer? offset) (integer? size)
           (<= 0 offset length) (<= 0 size (- length offset)))))
  )
