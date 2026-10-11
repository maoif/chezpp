#!/bin/sh
# Check option handling and script execution with real ChezScheme compilation.
set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
scheme="$project_root/.chezscheme-install/bin/scheme"
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM
cd "$project_root"

"$scheme" --script /dev/stdin <<'SCHEME'
(import (chezscheme))
(load "tools/compiler-options.ss")

(define check
  (lambda (actual expected)
    (unless (equal? actual expected)
      (error 'tool-scripts "unexpected compiler setting" actual expected))))

(let-values ([(options files)
              (read-build-arguments '("o=0" "d=3" "i=yes" "psi=1" "c=f" "wpo=t"
                                      "cl=1" "cp0=100" "fc=true" "p=t" "xp=t"
                                      "--" "source with spaces.ss"))])
  (check files '("source with spaces.ss"))
  (parameterize ([optimize-level 2] [commonization-level 0] [fasl-compressed #f])
    (call-with-compiler-options options
      (lambda ()
        (check (optimize-level) 0)
        (check (debug-level) 3)
        (check (generate-inspector-information) #t)
        (check (generate-procedure-source-information) #t)
        (check (generate-covin-files) #f)
        (check (generate-wpo-files) #t)
        (check (commonization-level) 1)
        (check (cp0-effort-limit) 100)
        (check (fasl-compressed) #t)
        (check (compile-profile) 'source)
        (check (generate-profile-forms) #t)))
    (check (optimize-level) 2)
    (call-with-compiler-options '((cl . "") (fc . ""))
      (lambda ()
        (check (commonization-level) 0)
        (check (fasl-compressed) #f)))
    (call-with-compiler-options '((i . "") (psi . "") (wpo . "") (p . "f") (xp . "f"))
      (lambda ()
        (check (generate-inspector-information) #f)
        (check (generate-procedure-source-information) #f)
        (check (generate-wpo-files) #f)
        (check (compile-profile) #f)
        (check (generate-profile-forms) #f)))))
SCHEME

cd "$fixture"
cat > support.ss <<'SCHEME'
(define test-support-loaded? #t)
SCHEME

cat > 'source "quoted" name.ss' <<'SCHEME'
(eval-when (compile)
  (unless (and (= (optimize-level) 0) (= (debug-level) 3)
               (generate-inspector-information) (generate-procedure-source-information)
               (not (generate-covin-files)) (not (generate-wpo-files)))
    (error 'tool-scripts "incorrect test compilation settings")))
(unless test-support-loaded?
  (error 'tool-scripts "test support was not loaded"))
SCHEME

"$scheme" --script "$project_root/tools/compile-test.ss" o=0 d=3 -- support.ss
"$scheme" --script "$project_root/tools/compile-test.ss" o=0 d=3 -- \
    'source "quoted" name.ss' support.so
test -s 'source "quoted" name.so'
test ! -e 'source "quoted" name.wpo'
test ! -e 'source "quoted" name.covin'
cp support.so mat.so
"$scheme" --script "$project_root/tools/run-test.ss" 'source "quoted" name.so'

# A failed test must propagate a nonzero exit status from the runner.
printf '%s\n' '(error (quote tool-scripts) "intentional test failure")' > failing.ss
if "$scheme" --script "$project_root/tools/run-test.ss" failing.ss \
    > failed.stdout 2> failed.stderr; then
    printf '%s\n' 'test runner swallowed a failure' >&2
    exit 1
fi
grep -q 'intentional test failure' failed.stderr
