# Rich Design And Implementation

Date: 2026-05-01
Updated: 2026-05-09

## Goal

Rich is a ChezScheme terminal rendering library modeled on Python Rich's core
features, adapted to Chezpp conventions. It provides styled output, structured
renderables, live terminal refresh, progress bars, prompts, and export helpers
through the public `(chezpp rich)` facade and the aggregate `(chezpp)` import.

The v1 scope intentionally excludes markup parsing, markdown, syntax
highlighting, logging, traceback rendering, and concrete `inspect`/`json`
modules. The implementation leaves room to add `inspect` and `json` later by
using the same render registry and facade structure.

## Public Shape

Rich has two layers:

- Procedural APIs: constructors, predicates, accessors, mutators, actions, and
  print/export procedures.
- Constructor macros: expression-returning builders for records with many
  fields.

Public procedures use `lambda` or `case-lambda`, never keyword arguments.
Public procedures validate boundary values with `pcheck`. Public macros reject
unknown fields at expansion time and rely on the same procedural setters for
runtime validation.

Constructor macro clauses use Chez-style keyword symbols directly:

```scheme
(define console
  (rich-console
    :width 100
    :color-system 'standard))

(define table
  (rich-table
    :title "Build"
    :box 'rounded
    :columns ("Name" "Status")
    :rows (("compile" "ok")
           ("test" "ok"))))
```

Constructor macros return the constructed value. They do not receive or define
an identifier. Mutators remain ordinary procedures.

## Module Map

`(chezpp rich)` is the public Rich facade. `chezpp.ss` imports and exports it so
users can also import the complete `(chezpp)` surface.

Implementation libraries:

- `(chezpp rich style)`: style records, reset objects, ANSI encoding fragments,
  color validation, and themes.
- `(chezpp rich segment)`: styled text segments, control segments, visible-width
  measurement, wrapping, cropping, and ANSI stripping.
- `(chezpp rich renderable)`: render protocol predicates and custom renderer
  registry.
- `(chezpp rich text)`: mutable styled text spans and rendering to segments.
- `(chezpp rich pretty)`: Scheme datum rendering for values without custom
  renderers.
- `(chezpp rich console)`: console records, output targets, printing, export,
  color policy, and final ANSI/plain encoding.
- `(chezpp rich box)`: box-style metadata.
- `(chezpp rich basic)`: rules, padding, alignment, columns, and layout.
- `(chezpp rich panel)`: panel records and border rendering.
- `(chezpp rich tree)`: tree records and guide rendering.
- `(chezpp rich table)`: table records, rows, columns, measurement, and box
  layout.
- `(chezpp rich live)`: live refresh, status spinners, deterministic time hooks,
  and refresh threads.
- `(chezpp rich progress)`: progress records, task records, progress columns,
  and progress rendering.
- `(chezpp rich prompt)`: prompt, confirm, and password helpers.
- `(chezpp rich private common)`: shared predicates and helpers that are not
  part of the public API.

## Rendering Model

The renderer is segment-based. Layout code never measures raw ANSI strings.

1. `rich-print`, `rich-render`, and export helpers receive ordinary Scheme
   values, styles, reset objects, and renderables.
2. Render dispatch converts renderables to segment lines. A segment line is a
   list of `rich-segment` values.
3. Layout components such as tables, panels, padding, alignment, wrapping, and
   cropping operate on segment lines and visible widths.
4. The console encoder resolves styles and themes, applies the color policy,
   emits ANSI only for ANSI-capable output, and writes text to the selected
   port.
5. Plain-text export uses the same segment lines but strips style/control
   effects where appropriate.

Custom renderers are registered with `rich-register-renderer!` by predicate.
They must return a string or segment-line list. Invalid renderer output is a
runtime error.

## Style And Theme

`rich-style` returns an opaque style object. It accepts a variable number of
symbols and color forms; it does not parse style strings. Later style options
override earlier conflicting options.

Examples:

```scheme
(rich-style 'bold 'red)
(rich-style 'italic #xffaa00 '(bg #x202020))
(rich-style 'red 'blue)               ; blue wins
(rich-style 'underline 'no-underline) ; no-underline wins
```

Supported style categories:

- Attributes: `bold`, `dim`, `italic`, `underline`, `blink`, `reverse`,
  `hidden`, `strike`, plus disabling forms such as `no-underline`.
- Standard and bright foreground colors.
- Background color symbols such as `on-red`.
- Truecolor foreground integers from `#x000000` through `#xffffff`.
- Truecolor color forms such as `(fg #x33ccff)` and `(bg #x202020)`.

`reset-style` returns a reset object. `rich-print` and the console encoder use
style/reset objects as state transitions in the argument stream.

Themes map symbols to `rich-style` objects:

```scheme
(define theme
  (rich-theme
    :ok (rich-style 'bold 'green)
    :warn (rich-style 'yellow)))
```

## Console And Printing

`rich-console` stores input/output/error ports, terminal width, color system,
soft-wrap policy, force-terminal flag, ASCII-only flag, and theme. Console
records are mutable through ordinary setters.

`rich-print` and `rich-println` print argument values directly. They do not take
a format string; callers use Scheme `format` explicitly:

```scheme
(rich-print console
            123
            " "
            (rich-style 'bold 'green)
            (format "~a" value)
            (reset-style)
            "\n")
```

If the first argument is a console or output port, it is used as the target. If
not, printing defaults to `(current-output-port)` and prints all arguments.

## Constructor Macros

The constructor macros are expression builders:

- `rich-theme`
- `rich-console`
- `rich-rule`
- `rich-padding`
- `rich-align`
- `rich-columns`
- `rich-layout`
- `rich-panel`
- `rich-tree`
- `rich-live`
- `rich-status`
- `rich-progress`
- `rich-table`

Each macro expands to a `let` with a generated temporary, a primitive
constructor call, zero or more setter/action calls, and the temporary value as
the final expression. Required fields are checked at expansion time when the
field is syntactically absent. Runtime type checks stay in the primitive
constructor and setter procedures.

Examples:

```scheme
(define panel
  (rich-panel
    :title "Status"
    :body (rich-text "ready" (rich-style 'green))
    :box 'rounded))

(define progress
  (rich-progress
    :width 40
    :tasks (("download" 1024)
            ("compile" #f))))
```

Literal list fields are supported where they make code readable:

- `rich-table :columns ("Name" "Status")`
- `rich-table :rows (("compile" "ok"))`
- `rich-progress :tasks (("compile" #f))`
- `rich-columns :items ("a" panel "b")`
- `rich-layout :items ("left" "right")`

Expression forms are also accepted:

```scheme
(rich-table
  :columns (vector->list column-vector)
  :rows (map normalize-row rows))
```

## Renderables

`rich-rule` renders a horizontal rule with optional title, width, and box style.

`rich-padding` wraps a renderable with top/right/bottom/left padding.

`rich-align` pads segment lines to a target width using left, center, or right
alignment.

`rich-columns` packs renderables into rows of columns using visible width and a
configurable gap.

`rich-layout` combines renderables as either a horizontal row or vertical
column. It is intentionally small in v1 and can grow into a richer layout tree
later.

`rich-panel` frames a body renderable with a box style, title, subtitle,
padding, and optional width/height.

`rich-tree` stores a label and children. Tree rendering supports Unicode guides
and ASCII-only guides through console policy.

`rich-table` stores columns, rows, title, caption, box style, padding, header
visibility, and row-line visibility. Rows must match the column count. Cells can
be ordinary values or renderables.

## Live, Status, And Progress

`rich-live` owns a renderable, console, refresh rate, transient flag, previous
line count, thread, and stop flag. `rich-live-refresh!` clears the previous live
output with carriage-return / ANSI clear-line sequences and redraws the current
renderable. Multi-line renderables are cleared by moving back up over the
previous line count.

`make-rich-status` and `rich-status` construct a status renderable. They do not
start a background refresh loop. `rich-status-render` computes the spinner frame
from `rich-current-time`, start time, frame list, and interval. Use
`rich-status-start!` to wrap the status in `rich-live` and refresh it in place.

`rich-progress` stores progress tasks. A task has an id, description, total,
completed count, and visibility. Totals must be positive or `#f` for
indeterminate tasks. Progress renders default columns for description, bar,
percentage, completed/total, and indeterminate counts.

`rich-current-time` and `rich-sleep` are parameters so tests and examples can
drive live/status behavior deterministically.

## Prompt And Password

Prompt helpers read from the console input port and write prompt text through
the console output port. `rich-prompt` supports defaults and allowed choices.
`rich-confirm` parses yes/no input. `rich-password` reads through the same port
model; when no terminal no-echo support is available, it must fail clearly
rather than silently echoing secrets.

## Error Handling

Public API errors are either macro syntax errors or runtime validation errors.

Macro errors:

- Unknown constructor fields.
- Odd field/value clause counts.
- Missing required fields such as `rich-panel :body`, `rich-tree :label`,
  `rich-live :renderable`, and `rich-status :message`.

Runtime validation errors:

- Invalid style symbols, malformed color forms, and out-of-range RGB integers.
- Non-port console ports.
- Invalid widths, negative padding, and invalid alignment names.
- Unknown box styles.
- Table row/column mismatches.
- Progress updates for unknown task ids.
- Invalid progress totals.
- Invalid prompt choices.
- Invalid renderer return values.

## Testing And Verification

Rich tests live in `tests/rich.ss` and are run with:

```sh
cd tests
make test-some TEST='rich'
```

Important coverage:

- Style construction, color validation, reset, themes, and override order.
- Segment width, wrapping, cropping, ANSI stripping, and renderer validation.
- Console rendering with color enabled and disabled.
- Expression-returning constructor macros and unknown-field errors.
- Basic renderables, panels, trees, tables, live refresh, status frames,
  progress tasks, prompts, and exports.
- Negative tests include comments explaining the expected error case.

Scheme files touched by Rich changes should pass the repository parenthesis
checker. Examples should run through `./chez++ --script ...` after a rebuild.

Full project validation is:

```sh
make clean
make
```

Because tests load `chezpp.lib`, macro changes require a clean rebuild when the
incremental whole-library step leaves stale compiled libraries.
