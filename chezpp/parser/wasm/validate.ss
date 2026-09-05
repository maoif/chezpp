(library (chezpp parser wasm validate)
  (export make-wasm-issue wasm-issue? wasm-issue-offset wasm-issue-message
          wasm-section-rank wasm-standard-section? wasm-section-order-valid?
          wasm-function-code-count-valid? wasm-data-count-valid?)
  (import (chezpp chez)
          (chezpp utils))

  (define-record-type ($wasm-issue $make-wasm-issue $wasm-issue?)
    (fields (immutable offset $wasm-issue-offset)
            (immutable message $wasm-issue-message)))

  #|proc:make-wasm-issue
  Creates a structural issue at byte `offset` with diagnostic string `message`.
  |#
  (define make-wasm-issue
    (lambda (offset message)
      (pcheck ([natural? offset] [string? message])
              ($make-wasm-issue offset message))))

  #|proc:wasm-issue?
  Returns whether `object` is a WebAssembly structural issue.
  |#
  (define wasm-issue?
    (lambda (object)
      (pcheck () ($wasm-issue? object))))

  #|proc:wasm-issue-offset
  Returns the byte offset of issue `record`.
  |#
  (define wasm-issue-offset
    (lambda (record)
      (pcheck ([$wasm-issue? record])
              ($wasm-issue-offset record))))

  #|proc:wasm-issue-message
  Returns the diagnostic message of issue `record`.
  |#
  (define wasm-issue-message
    (lambda (record)
      (pcheck ([$wasm-issue? record])
              ($wasm-issue-message record))))

  #|proc:wasm-standard-section?
  Returns whether natural `id` identifies a standard Core 3.0 section.
  |#
  (define wasm-standard-section?
    (lambda (id)
      (pcheck ([natural? id])
              (<= 1 id 13))))

  #|proc:wasm-section-rank
  Returns the Core 3.0 order rank for natural section `id`, or `#f` for a custom or unknown ID.
  |#
  (define wasm-section-rank
    (lambda (id)
      (pcheck ([natural? id])
              (case id
                [(1) 1] [(2) 2] [(3) 3] [(4) 4] [(5) 5] [(13) 6]
                [(6) 7] [(7) 8] [(8) 9] [(9) 10] [(12) 11]
                [(10) 12] [(11) 13]
                [else #f]))))

  #|proc:wasm-section-order-valid?
  Returns whether natural section `id` follows natural `previous-rank` without duplication.
  |#
  (define wasm-section-order-valid?
    (lambda (previous-rank id)
      (pcheck ([natural? previous-rank id])
              (let ([rank (wasm-section-rank id)])
                (and rank (> rank previous-rank))))))

  #|proc:wasm-function-code-count-valid?
  Returns whether vectors `type-indices` and `code-bodies` have equal lengths.
  |#
  (define wasm-function-code-count-valid?
    (lambda (type-indices code-bodies)
      (pcheck ([vector? type-indices code-bodies])
              (= (vector-length type-indices) (vector-length code-bodies)))))

  #|proc:wasm-data-count-valid?
  Returns whether optional natural `declared` matches vector `data-segments`.
  |#
  (define wasm-data-count-valid?
    (lambda (declared data-segments)
      (pcheck ([(lambda (value) (or (not value) (natural? value))) declared]
               [vector? data-segments])
              (or (not declared) (= declared (vector-length data-segments))))))
  )
