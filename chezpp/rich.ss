#!chezscheme
(library (chezpp rich)
  (export rich-style
          rich-style?
          reset-style
          rich-reset?
          rich-style-fg
          rich-style-bg
          rich-style-bold?
          rich-style-dim?
          rich-style-italic?
          rich-style-underline?
          rich-style-blink?
          rich-style-reverse?
          rich-style-hidden?
          rich-style-strike?
          rich-color-system?
          rich-style->ansi
          rich-ansi-reset
          rich-theme
          make-rich-theme
          rich-theme?
          rich-theme-ref
          rich-theme-set!

          make-rich-console
          rich-console
          rich-console?
          rich-print
          rich-println
          rich-fprint
          rich-fprintln
          rich-render
          rich-export-text
          rich-export-ansi
          rich-register-renderer!
          rich-renderer-for
          rich-renderable?)
  (import (chezpp chez)
          (chezpp rich style)
          (chezpp rich renderable)
          (chezpp rich console)))
