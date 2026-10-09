# Project Overview

Chezpp is an extension library for ChezScheme and a build system
and package manager for ChezScheme.
The build system and package manager part are not implemented yet.

## Versioning And Release Policy

chezpp versioning/tagging: major.minor.patch.chezMajor-chezMinor-chezPatch.,
or `(major minor patch chez-major chez-minor chez-patch)`.
Git tags use `major.minor.patch@chezMajor-chezMinor-chezPatch`, such as
`0.0.0@10-4-1`.
The umbrella `(chezpp ...)` library is the only versioned public contract for now;
constituent libraries remain unversioned implementation modules. Promote a constituent
library to an independently versioned public contract only when it has a stable API,
an explicit compatibility policy, maintained documentation and tests, and an owner for
coordinating its release and dependency constraints.
Each new version of ChezScheme gives a new tag to chezpp; do not chase ChezScheme
releases unless a breaking change or required compatibility fix warrants it.

## Coding requirements

Create all worktrees under `.worktrees`; prefix wroktree name with date.
Preserve worktree after merging.

No keyword arguments in procedure definition,
only `lambda` and `case-lambda`.
ChezScheme does not support keyword arguments.

Public API parameter names should be meaningful.

Use the `pcheck` macro on public (exported) APIs to do type checking.
E.g.,

```scheme
(lambda (w x y z)
  (pcheck ([string? x] [natural? y] [bytevector? w z])
          (todo)))
```

`pcheck` is defined in `(chezpp utils)`.

Public APIs must have documentation right above the implementation code,
in this format, for procedures and macros:

```scheme
#|proc:func
The `func` procedure does ...
|#
(define func (...))

#|macro:define-xxx
The `define-xxx` macro does ...
|#
(define-syntax define-xxx (...))
```

The documentation uses markdown style, and its indentation should be the same as that of the code below.
The documentation must explain what the procedure/macro/defined name does.
For procedures, it must explain the meaning of each parameter, and what the return value is.
For procedural parameters, describe the intended behavior of the procedures.
Each line of the documentation must not exceed 100 characters.

To make a single long source file more readable, use this to describe a
block of code (no indentation needed):

```
;;;;===----------------------------------------------------------------------===
;;;; description of a group of APIs
;;;;===----------------------------------------------------------------------===
```

You can use ChezScheme extensions, not just r6rs APIs.
ChezScheme doc can be found [here](https://cisco.github.io/ChezScheme/csug/csug.html).

Read existing code to get a basic understanding of the coding style.

You should check whether parenthese are balanced for each Scheme file/script you write.

When writing negative test code, also write comment describing the error case being tested.
Use newline to seperate each testcase.

When opening a port, make sure it's closed after usage.

For public APIs that are higher-order functions, their doc must describe
the signature of their function arguments.

Group exports in libraries when they are many.

Do not by default add design docs, specs, plans, handoffs to git.

For sub-agent execution, only spwan one agent at a one.

## Build chezpp

When a new lib is finished under `chezpp/`, add an import entry
in `chezpp.ss` so it can be automatically compiled when compiling
`chezpp.ss`.

C code should be put under `chezpp/c/`.

In project root directory, **always** run `make clean && make` to build project.
Run `make run` to enter chezpp REPL, or alternatively,
run `chez++` script.

By default, `make` is equal to `make release`.
`make debug` and `make coverage` is also available.

## Run test code

You can enter `tests/` and run `make test` to run all tests, or pass selected
test files directly, for example `make test vector.ss list.ss`. Only files in
the test suite are accepted; support files are rejected.

Each testcase in `mat` form must return either `#t` to indicate success or
`#f` to indicate failure.

If there are no errors, both stderr and stdout should be empty.
If there's any output, there's error and you should parse the error
message and fix the bugs. `Expect error` is OK.

In case of `make coverage`, a coverage report is printed after test run. 

## Git commit message format

If modifications are localized in a certain subsystem (e.g., net), then the
commit message format should look like:

```
net: things done in the commit
```

If there are multiple lines of changes, list them using ordered list:

```
net:
1) ...
2) ...
3) ...
```

For changes related to the build system or other places not related to the code,
write the message without a subsystem prefix, e.g.,

```
Update launch script to support both REPL and script mode.
```

Refer to `git log` to learn the patterns.

## Macros

TODO

## FFI

Use `foreign-procedure` to bind foreign C functions in Scheme.
Use `foreign-callalble` to define Scheme procedures that can be called
outside Scheme, e.g., in C.

See existing code for usages of `foreign-procedure`.
Notice me if you want to use `foreign-callalble`.

## Docs

TODO

## Performance

Use fixnum arithemtic when the value range is known and small.
Fixnum range can be obtained by `(most-positive-fixnum)` and `(most-negative-fixnum)` in Chez.

Prefer preallocating objects (string, vector, bytevector and others) than appending them sequentially.

Prefer `make-list-builder` from `(chezpp list)` to build up a list than `cons` and `reverse`.

Use `call/cc` and continuation operations carefully, as it has performance overhead.
Use `call/1cc` if possible.
Always comment the use of `call/cc` using `;;`.


## About some files

`chezpp.ss`:
this is the main file through which users can import the entirety of chezpp and the ChezScheme compiler can
recursively compile all chezpp libs imported when just compiling this file (when `(compile-imported-libraries #t)`
is set).

When a new lib is written under `chezpp/`, say, `new-lib`, its import entry can be added in `chezpp.ss` 
as `(chezpp new-lib)`.

Specifically, `(chezpp concurrency fiber)` and `(chezpp parser combinator)` are two *experimental* libs that
are imported but NOT exported, just so they can be compiled when building chezpp. A consequence of this is 
that their wpo (whole program optimization) files have to be seperately fed into `compile-whole-library` in
`Makefile` and the resulting `fiber.lib` and `combinator.lib` have to be seperately loaded in `chez++.ss` for
them to be used.

`chez++.ss`:
this file is convenience wrapper that just loads `chezpp.lib` and other experimental libs for `chez++` to use.

`chez++`:
a shell script that use ssystem ChezScheme executable to load `chez++.ss` and thus making chezpp available to users.

You as an agent usually do not have to modify `chez++.ss` and `chez++`.


## Bytevector APIs

Avoid implement your own procedures to access bytevector data.
ChezScheme support these procedures to access bytevector data at different lengths and endianness:

```scheme
;; The bytevector-u8-ref procedure returns the byte at index k of bytevector, as an octet.
(bytevector-u8-ref bv k)
;; The bytevector-s8-ref procedure returns the byte at index k of bytevector, as a (signed) byte.
(bytevector-s8-ref bv k)

;; XXX can be one of u16,s16,u24,s24,u32,s32,u40,s40,u48,s48,u56,s56,u64,s64.
;; bytevector-XXX-ref reads an unsigned/signed number as long as XXX indicates,
;; and bytevector-XXX-set! writes an unsigned/signed number v as long as XXX indicates,
;; starting from index k, with given endianness, from or to bytevector bv.
;; endianness can be (native-endianness), (endianness big) or (endianness little).
(bytevector-XXX-ref bv k endianness)
(bytevector-XXX-set! bv k v endianness)

;; XXX can be one of u16,s16,u24,s24,u32,s32,u40,s40,u48,s48,u56,s56,u64,s64.
;; bytevector-XXX-ref reads an unsigned/signed number as long as XXX indicates,
;; and bytevector-XXX-set! writes an unsigned/signed number v as long as XXX indicates,
;; starting from index k, with current machine's native endianness, from or to bytevector bv.
(bytevector-XXX-native-ref bv k)
(bytevector-XXX-native-set! bv k v)

;; These procedures return the inexact real number object
;; that best represents the IEEE-754 single-precision number
;; represented by the four bytes beginning at index k.
(bytevector-ieee-single-native-ref bv k)
(bytevector-ieee-single-ref bv k endianness)

;; These procedures store an IEEE-754 single-precision representation of x into
;; elements k through k + 3 of bytevector, and return unspecified values.
(bytevector-ieee-single-native-set! bv k v)
(bytevector-ieee-single-set! bv k v endianness)

;; These procedures return the inexact real number object
;; that best represents the IEEE-754 double-precision number
;; represented by the eight bytes beginning at index k.
(bytevector-ieee-double-native-ref bv k)
(bytevector-ieee-double-ref bv k endianness)

;; These procedures store an IEEE-754 double-precision representation of x into
;; elements k through k +7 of bytevector, and return unspecified values.
(bytevector-ieee-double-native-set! bv k v)
(bytevector-ieee-double-set! bv k v endianness)

```
