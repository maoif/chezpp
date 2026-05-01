# Rich Library Design

Date: 2026-05-01

## Goal

Implement a ChezScheme terminal rendering library modeled on Python Rich. The
target is a roughly one-to-one conceptual copy of Rich's core terminal features,
adapted to Chezpp style and ChezScheme constraints.

The design intentionally ignores the previous local `chezpp/rich.ss` work and
starts from the Rich feature/API surface plus existing Chezpp conventions.

## Scope

Include:

- Console printing and rendering.
- Styles, colors, themes, styled text, segments, measurement, wrapping, and
  cropping.
- Renderables: table, panel, tree, rule, columns, padding, alignment, and
  layout.
- Live display, status/spinner, and progress.
- Prompt and confirm input helpers.
- Scheme datum pretty printing.
- Export hooks for plain text and ANSI text, with room for HTML/SVG later.
- Extension points for future `inspect` and `json` modules.

Exclude for v1:

- BBCode-style markup.
- Markdown.
- Syntax highlighting.
- Logging integration.
- Traceback rendering.
- `inspect` and `json` feature implementations.

Reference surface:

- Rich reference index: https://rich.readthedocs.io/en/latest/reference.html
- Rich console API: https://rich.readthedocs.io/en/latest/reference/console.html
- Rich table API: https://rich.readthedocs.io/en/latest/reference/table.html
- Rich progress guide: https://rich.readthedocs.io/en/latest/progress.html
- Rich live guide: https://rich.readthedocs.io/en/latest/live.html

## Architecture

`(chezpp rich)` is the public facade library. It re-exports stable public APIs
from smaller implementation libraries:

- `(chezpp rich style)`: style objects, symbolic colors, truecolor values,
  ANSI emission, reset objects, and themes.
- `(chezpp rich segment)`: text fragments with style metadata, control
  segments, visible-width measurement, line splitting, wrapping, cropping, and
  ANSI stripping.
- `(chezpp rich console)`: console records, ports, terminal width, color policy,
  output methods, render dispatch, and export entry points.
- `(chezpp rich renderable)`: internal render protocol and custom renderer
  registry.
- `(chezpp rich text)`: styled text containers and helpers.
- `(chezpp rich renderables)`: table, panel, tree, rule, columns, padding,
  alignment, and layout records.
- `(chezpp rich live)`: live refresh machinery, status/spinner, and progress.
- `(chezpp rich prompt)`: prompt/confirm helpers.
- `(chezpp rich pretty)`: width-aware Scheme datum rendering.

The public facade hides most internal representation details. Internals can be
split further if a source file becomes too large.

## API Style

All public procedures use `lambda` or `case-lambda`; no keyword arguments are
used. Public procedures and macros use Chezpp documentation comments directly
above implementation code. Public procedure boundaries use `pcheck`.

The primitive procedural APIs follow existing Chezpp naming:

- Constructors: `make-rich-console`, `make-rich-table`, `make-rich-panel`, etc.
- Predicates: `rich-console?`, `rich-table?`, etc.
- Accessors and mutators: `rich-table-title`, `rich-table-title-set!`, etc.
- Actions: `rich-table-add-column!`, `rich-progress-advance!`,
  `rich-live-start!`, etc.
- Rendering/printing: `rich-render`, `rich-print`, `rich-println`,
  `rich-fprint`, `rich-fprintln`.

## Constructor Macros

Records with many optional fields also get constructor macros using
`[:field value]` clauses. The macro APIs are for construction only. Mutators
remain ordinary procedures.

Examples:

```scheme
(rich-console
  [:output-port (current-output-port)]
  [:error-port (current-error-port)]
  [:width 100]
  [:color-system 'truecolor]
  [:theme theme])

(rich-table
  [:title "Build"]
  [:box 'rounded]
  [:show-header? #t]
  [:columns ("Name" "Status")]
  [:rows (("compile" "ok")
          ("test" "ok"))])

(rich-progress
  [:width 80]
  [:columns (rich-progress-default-columns)]
  [:tasks (["download" 1024]
           ["compile" #f])])
```

Constructor macros expand to primitive constructors plus ordinary setter/add
calls. Unknown fields are rejected at expansion time.
Runtime values still pass through the same `pcheck` validation as procedural
use.

## Style Model

`rich-style` returns an opaque style object. It does not return raw ANSI text.
`reset-style` returns an opaque style-reset object. `rich-reset` is exported as
a constant reset object.

`rich-style` accepts a variable number of symbolic options and color forms. It
does not parse style strings. Later options override earlier conflicting
options.

Examples:

```scheme
(rich-style 'bold 'red)
(rich-style 'italic #xffaa00 '(bg #x202020))
(rich-style 'red 'blue)                ; blue wins
(rich-style 'underline 'no-underline)  ; no-underline wins
```

Style option categories:

- Attributes: `bold`, `dim`, `italic`, `underline`, `blink`, `reverse`,
  `hidden`, `strike`, plus `no-...` disabling forms.
- Foreground symbolic colors: `black`, `red`, `green`, `yellow`, `blue`,
  `magenta`, `cyan`, `white`, `default`, and bright variants.
- Background symbolic colors: either `on-red` style symbols or `(bg color)`.
- Truecolor foreground: a raw integer from `#x000000` through `#xffffff`, or
  `(fg #xrrggbb)`.
- Truecolor background: `(bg #xrrggbb)`.
- Theme aliases: symbols resolved through the active console theme.

ANSI bytes are produced only by the console encoder.

## Print Model

`rich-print` and related APIs do not take a format string. The first argument
must be a rich console or an open output port. Remaining arguments are consumed
in order and printed/rendered directly. Callers use Scheme `format` explicitly
when formatting is desired.

Examples:

```scheme
(rich-print console
            123
            " "
            (rich-style 'bold 'green)
            (format "~a" value)
            (reset-style)
            "\n")

(rich-println console
              (rich-style 'red)
              "error"
              (reset-style)
              ": "
              path)

(rich-print port table)
```

Print inputs may be strings, chars, numbers, symbols, bytevectors, pairs,
vectors, records handled by pretty printing, style objects, reset objects, or
rich renderables. Style objects affect following text until another style or
reset object appears.

## Rendering Pipeline

Rendering uses a segment pipeline:

1. Public print/render APIs receive a sequence of values.
2. Values normalize into text, style transitions, reset transitions,
   renderables, or pretty-rendered data.
3. Renderables emit logical lines of `rich-segment` values.
4. Measurement computes visible width from segment text, independent of ANSI
   bytes.
5. Wrapping, cropping, padding, alignment, table layout, panel borders, and
   layout splitting operate on segment lines.
6. The console encoder resolves theme aliases, applies the active color policy,
   emits ANSI as needed, and writes to the selected port.
7. Export APIs use the same segment lines and choose a backend such as plain
   text or ANSI text.

This avoids raw ANSI strings leaking into layout decisions.

## Core Components

### Console

`rich-console` owns output port, error port, width, color system, force-terminal
flag, soft-wrap flag, theme, and export buffers. It is responsible for render
dispatch and final encoding.

### Text And Segments

`rich-text` stores styled spans and supports append, split, wrap, justify, crop,
plain-text extraction, and conversion to segment lines.

`rich-segment` stores text, style metadata, and optional control metadata. It is
the unit of measurement and output.

### Renderables

`rich-table` supports columns, rows, title, caption, box style, padding,
header/footer controls, row separators, cell justification, overflow behavior,
and optional width constraints.

`rich-panel` frames another renderable with title, subtitle, box style, padding,
and optional width/height constraints.

`rich-tree` stores a label and child nodes, with guide style and expanded flag.

`rich-rule` renders a horizontal rule with optional title and alignment.

`rich-columns` flows renderables into columns.

`rich-padding` and `rich-align` wrap another renderable.

`rich-layout` splits terminal space into rows and columns for dashboard-style
output.

### Live, Status, And Progress

`rich-live` manages refresh state for replacing terminal output. It tracks the
renderable, target console/port, refresh rate, last rendered line count, and
live thread.

`rich-status` is a spinner plus message built on `rich-live`.

`rich-progress` manages tasks, task ids, totals, completed counts, start/stop
times, visibility, and progress columns. Default columns include description,
bar, percentage, completed/total, elapsed, remaining, speed, and spinner.

Time and sleeping hooks are parameterized for deterministic tests.

### Prompt

Prompt APIs read from an input port and write through a console. They support
prompt text, choices, defaults, confirmation, and password input. Password
input raises an unsupported error when terminal no-echo control is unavailable.

### Pretty

Pretty printing renders Scheme data with width-aware line breaks and optional
style categories for pairs, vectors, strings, symbols, numbers, booleans, and
records. Unknown records fall back to Chez's written representation unless a
custom renderer is registered.

## Extension Points

Custom renderers can be registered by predicate. A renderer receives the console
context, the value, and width constraints, and returns segment lines.

Reserved modules:

- `(chezpp rich inspect)`
- `(chezpp rich json)`

The facade does not export inspect/json user-facing APIs in v1, but the render
registry and module structure do not block those additions later.

## Error Handling

Public APIs validate at boundaries using `pcheck`.

Expected runtime errors include:

- Invalid style symbols or malformed color forms.
- Color integers outside `#x000000` to `#xffffff`.
- Closed ports or non-output ports passed to print APIs.
- Invalid widths, negative padding, or impossible layout constraints.
- Table row/column mismatch.
- Progress updates for unknown task ids.
- Starting live/status/progress objects that are already live.
- Invalid prompt choices or defaults.

Constructor macro syntax errors include unknown fields and malformed
`[:field value]` clauses.

## Testing

Add focused tests in `tests/rich.ss` and include `rich.ss` in `tests/Makefile`.
When the library is added under `chezpp/`, add `(chezpp rich)` to `chezpp.ss`
so it compiles with the full project.

Test groups:

- Style construction, reset objects, truecolor validation, theme resolution, and
  "later option wins".
- Segment measurement, ANSI stripping, wrapping, cropping, padding, and
  alignment.
- Console output to string ports and color on/off policy.
- Constructor macro expansion and unknown-field syntax errors.
- Renderable output for table, panel, tree, rule, columns, padding, align, and
  layout.
- Live/status/progress using deterministic time and refresh hooks.
- Prompt using input string ports and output string ports.
- Pretty printing for representative Scheme data.
- Negative tests with comments explaining the expected error case.

Before claiming implementation complete, run targeted Rich tests and then the
project build/test commands that are practical in the current workspace.

## Fixed Implementation Choices

Implementation libraries are Chez libraries under `chezpp/rich/`, but
only `(chezpp rich)` is the supported user-facing import surface for v1.

`rich-print` does not default to `(current-output-port)` when the first argument
is not a console or port. Callers must pass the target explicitly.

Both reset names are provided: `(reset-style)` constructs or returns a reset
object, and `rich-reset` is a constant reset object.

Initial box styles are `ascii`, `square`, `rounded`, `heavy`, and `double`.
Unicode-capable consoles use Unicode boxes by default. Consoles configured for
ascii output fall back to `ascii`.

Password prompts are included in the API. On platforms where no terminal
no-echo helper is available, password prompt calls raise a clear unsupported
error rather than silently echoing input.
