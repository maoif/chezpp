#!chezscheme
(library (chezpp test runner)
  (export test-expand-parameters test-list test-select)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp test private common)
          (chezpp test descriptor))

  (define $metadata-merge
    (lambda (parent child)
      (let loop ([metadata parent] [child child])
        (if (null? child)
            metadata
            (loop (test-alist-set metadata (caar child) (cdar child))
                  (cdr child))))))

  (define $parameter-row
    (lambda (names row)
      (let loop ([names names] [row row] [out '()])
        (cond
         [(null? names) (reverse out)]
         [(null? row)
          (errorf 'test-expand-parameters "parameter row is shorter than names: ~a" row)]
         [else
          (loop (cdr names) (cdr row) (cons (cons (car names) (car row)) out))]))))

  (define $expand-parameter-table
    (lambda (table)
      (let ([names (car table)]
            [rows (cdr table)])
        (map (lambda (row) ($parameter-row names row)) rows))))

  (define $append-map
    (lambda (proc items)
      (let loop ([items items] [out '()])
        (if (null? items)
            (reverse out)
            (loop (cdr items) (append (reverse (proc (car items))) out))))))

  (define $parameter-product
    (lambda (tables)
      (let loop ([tables tables] [cases '(() )])
        (if (null? tables)
            cases
            (let ([rows ($expand-parameter-table (car tables))])
              (loop (cdr tables)
                    ($append-map
                     (lambda (case-parameters)
                       (map (lambda (row-parameters)
                              (append case-parameters row-parameters))
                            rows))
                     cases)))))))

  (define $expand-descriptor
    (lambda (descriptor inherited-metadata id)
      (let ([metadata ($metadata-merge inherited-metadata
                                       (test-descriptor-metadata descriptor))])
        (if (test-suite-descriptor? descriptor)
            (let loop ([children (test-descriptor-children descriptor)]
                       [id id]
                       [out '()])
              (if (null? children)
                  (values (reverse out) id)
                  (let-values ([(child-cases next-id)
                                ($expand-descriptor (car children) metadata id)])
                    (loop (cdr children)
                          next-id
                          (append (reverse child-cases) out)))))
            (let ([parameter-cases
                   ($parameter-product (test-metadata-ref metadata 'parameterize '()))])
              (let loop ([parameter-cases parameter-cases]
                         [id id]
                         [out '()])
                (if (null? parameter-cases)
                    (values (reverse out) id)
                    (loop (cdr parameter-cases)
                          (fx+ id 1)
                          (cons (make-test-concrete-case
                                 id
                                 (test-descriptor-name descriptor)
                                 descriptor
                                 (car parameter-cases)
                                 metadata)
                                out)))))))))

  (define $string-prefix?
    (lambda (prefix string)
      (let ([prefix-length (string-length prefix)]
            [string-length (string-length string)])
        (and (fx<= prefix-length string-length)
             (let loop ([index 0])
               (or (fx= index prefix-length)
                   (and (char=? (string-ref prefix index)
                                (string-ref string index))
                        (loop (fx+ index 1)))))))))

  (define $all-tags-present?
    (lambda (required present)
      (let loop ([required required])
        (or (null? required)
            (and (memq (car required) present)
                 (loop (cdr required)))))))

  (define $test-selectors?
    (lambda (selectors)
      (and (list? selectors)
           (let loop ([selectors selectors])
             (or (null? selectors)
                 (let ([selector (car selectors)])
                   (and (pair? selector)
                        (case (car selector)
                          [(name-prefix) (string? (cdr selector))]
                          [(include-tags) (list? (cdr selector))]
                          [else #t])
                        (loop (cdr selectors)))))))))

  (define $case-selected?
    (lambda (concrete-case selectors)
      (let loop ([selectors selectors])
        (if (null? selectors)
            #t
            (let ([selector (car selectors)])
              (case (car selector)
                [(name-prefix)
                 (and ($string-prefix?
                       (cdr selector)
                       (symbol->string (test-concrete-case-name concrete-case)))
                      (loop (cdr selectors)))]
                [(include-tags)
                 (and ($all-tags-present?
                       (cdr selector)
                       (test-metadata-ref
                        (test-concrete-case-metadata concrete-case)
                        'tags
                        '()))
                      (loop (cdr selectors)))]
                [else (errorf 'test-select "unknown selector: ~a" (car selector))]))))))

  #|proc:test-expand-parameters
The `test-expand-parameters` procedure returns concrete test cases expanded
from `descriptor`. For suite descriptors, child descriptors are flattened and
inherit suite metadata, with child metadata overriding duplicate parent keys.
For case, expansion, and compile descriptors, `parameterize` metadata supplies
parameter tables. Each table has the form `((name ...) row ...)`, and multiple
tables are combined as a Cartesian product in table order.
|#
  (define test-expand-parameters
    (lambda (descriptor)
      (pcheck ([test-descriptor? descriptor])
              (let-values ([(cases next-id) ($expand-descriptor descriptor '() 0)])
                cases))))

  #|proc:test-select
The `test-select` procedure returns concrete test cases from `registry` that
match `selectors`. `registry` is a test registry. `selectors` is an association
list; `(name-prefix . string)` keeps cases whose symbolic name starts with the
string, and `(include-tags . (tag ...))` keeps cases whose metadata `tags`
contain every listed tag.
|#
  (define test-select
    (lambda (registry selectors)
      (pcheck ([test-registry? registry] [$test-selectors? selectors])
              (let ([cases
                     ($append-map test-expand-parameters
                                  (test-registry-descriptors registry))])
                (let loop ([cases cases] [out '()])
                  (cond
                   [(null? cases) (reverse out)]
                   [($case-selected? (car cases) selectors)
                    (loop (cdr cases) (cons (car cases) out))]
                   [else (loop (cdr cases) out)]))))))

  #|proc:test-list
The `test-list` procedure returns concrete test cases from `registry` selected
by `selectors`. `registry` is a test registry, and `selectors` accepts the same
association-list filters as `test-select`.
|#
  (define test-list
    (lambda (registry selectors)
      (pcheck ([test-registry? registry] [$test-selectors? selectors])
              (test-select registry selectors))))

  )
