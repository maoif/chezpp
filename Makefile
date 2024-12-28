SCHEME := scheme
PREFIX := /usr

# TODO include Chez header file

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_TEST   := $(shell find tests/    -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c')

# use this to run tests selectively
TEST := ${SRCS_TEST}

CC := gcc
CFLAGS := -fPIC -Wall -Wextra -O2 -shared


all: chez++.exe

run: chez++.exe
	@./chez++

libchezpp.so:
	$(CC) $(CFLAGS) -o $@ $(SRCS_C)

chezpp.lib chezpp.wpo: chezpp.ss build.ss ${SRCS_CHEZPP} libchezpp.so
	@echo '(optimize-level 3)' \
	      '(compile-imported-libraries #t)'\
	      '(generate-wpo-files #t)' \
	      '(compile-file "chezpp.ss")' \
	      '(unless (null? (compile-whole-library "chezpp.wpo" "chezpp.lib"))' \
	      '  (errorf "chezpp-build" "dependency has to be null"))' \
	      | ${SCHEME} -q
	@rm -f chezpp.so

chez++.exe: chezpp.lib chezpp.wpo
	@rm -f chez++ chez++.ss
	@echo '(load "chezpp.lib")(import (chezpp))' > chez++.ss
	@echo '#!/usr/bin/env bash'      >> chez++
	@echo 'SCHEME=${SCHEME}'         >> chez++
	@echo '$$SCHEME "$$@" chez++.ss' >> chez++
	@chmod +x chez++

install: chez++.exe
	echo todo

test: ${SRCS_TESTS}
	echo todo

test-cov: ${SRCS_TESTS}
	echo todo

clean:
	@rm -f chezpp.lib chezpp.wpo chez++ chez++.ss libchezpp.so
	@find chezpp/ -name '*.so'  -delete
	@find chezpp/ -name '*.wpo' -delete


dump:
	@echo ${PREFIX}
	@echo ${SCHEME}
	@echo ${SRCS_CHEZPP}
	@echo ${SRCS_TESTS}
	@echo ${TEST}
	@echo ${SRCS_C}
