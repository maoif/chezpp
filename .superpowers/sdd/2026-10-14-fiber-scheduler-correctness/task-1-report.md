# Task 1 Baseline

Added `tests/fiber.ss` with bounded regression cases covering event scheduling,
shutdown, errors, values, nested runs, cvars, mutexes, selection cleanup,
timers, sleeping, and independent contexts. Registered the file in
`tests/test-files.mk`.

Focused command:

```text
cd tests && make test fiber.ss
```

The test file compiled successfully. The current implementation fails at
`fiber-event-never-blocks` because `event-never` is still a constructor
procedure (`pcheck event? evt` receives `#<procedure event-never>`). The run
then reaches the known empty-selection and shutdown failure paths. A bounded
invocation (`timeout 30s make test fiber.ss`) exits with timeout in the current
implementation, so the baseline does not complete until scheduler shutdown
fixes are applied.

Follow-up validation used `timeout 30s make test fiber.ss`; compilation still
succeeds, while execution exceeds the timeout in the current scheduler. The
wrap case now synchronizes the source and wrapped event in one run, and the
nested-run case observes the nested thunk's side effect.

The fiber timer regression now uses the exported `milisleep-event` and
`sleep-event` APIs for millisecond and second waits. The focused run rebuilt
the fiber library and test successfully before timing out during execution.

Review follow-up added scheduler-level timer alternatives to the cvar and
select cleanup waits, so a lost wakeup or leaked registration returns a
failure marker instead of waiting indefinitely once timer scheduling works.
The nested-run case now waits for a bounded completion signal and asserts that
the nested thunk actually ran. Negative-case comments cover the watchdog and
timer restoration paths. The focused command `timeout 20s make test fiber.ss`
still timed out in the pre-fix scheduler after compiling successfully.
