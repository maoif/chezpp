#!chezscheme
(library (chezpp rich renderable)
  (export rich-register-renderer!
          rich-renderer-for
          rich-renderable?)
  (import (chezpp chez)
          (chezpp utils))

  (define $renderers '())

  #|proc:rich-register-renderer!
  The `rich-register-renderer!` procedure registers a renderer predicate and
  rendering procedure for future rich output.
  |#
  (define rich-register-renderer!
    (lambda (pred renderer)
      (pcheck ([procedure? pred renderer])
              (set! $renderers (cons (cons pred renderer) $renderers)))))

  #|proc:rich-renderer-for
  The `rich-renderer-for` procedure returns the renderer registered for
  `value`, or `#f` when none is registered.
  |#
  (define rich-renderer-for
    (lambda (value)
      (let loop ([renderers $renderers])
        (cond [(null? renderers) #f]
              [((caar renderers) value) (cdar renderers)]
              [else (loop (cdr renderers))]))))

  #|proc:rich-renderable?
  The `rich-renderable?` procedure returns `#t` when a renderer is registered
  for `value`, and `#f` otherwise.
  |#
  (define rich-renderable?
    (lambda (value)
      (and (rich-renderer-for value) #t))))
