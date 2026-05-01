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
          rich-segment
          make-rich-segment
          rich-segment?
          rich-segment-text
          rich-segment-style
          rich-segment-control?
          rich-segment-width
          rich-segments-width
          rich-segments->plain
          rich-strip-ansi
          rich-segment-wrap
          rich-segment-crop
          rich-segment-pad-right

          make-rich-console
          rich-console
          rich-console?
          rich-console-output-port
          rich-console-output-port-set!
          rich-console-error-port
          rich-console-error-port-set!
          rich-console-input-port
          rich-console-input-port-set!
          rich-console-width
          rich-console-width-set!
          rich-console-color-system
          rich-console-color-system-set!
          rich-console-force-terminal?
          rich-console-force-terminal?-set!
          rich-console-soft-wrap?
          rich-console-soft-wrap?-set!
          rich-console-theme
          rich-console-theme-set!
          rich-console-ascii-only?
          rich-console-ascii-only?-set!
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
          (chezpp rich segment)
          (chezpp rich renderable)
          (chezpp rich console)))
