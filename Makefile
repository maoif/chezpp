SCHEME := scheme
PREFIX := /usr

# TODO include Chez header file

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_TEST    = $(shell find tests/    -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c')

CC := gcc
CFLAGS := -fPIC -Wall -Wextra -O2 -shared

chezpplibs = chezpp.lib \
             chezpp/concurrency/fiber.lib \
             chezpp/parser/combinator.lib

chezppwpos = chezpp.wpo \
             chezpp/concurrency/fiber.wpo \
             chezpp/parser/combinator.wpo

chezppdeps = ${chezpplibs} ${chezppwpos}

.PHONY: all
all: chez++.exe

.PHONY: run
run: chez++.exe
	@./chez++

libchezpp.so:
	$(CC) $(CFLAGS) -o $@ $(SRCS_C)

${chezppdeps}: chezpp.ss ${SRCS_CHEZPP} libchezpp.so
	@echo '(optimize-level 2)' \
	      '(compile-imported-libraries #t)'\
	      '(generate-wpo-files #t)' \
	      '(time (compile-file "chezpp.ss"))' \
	      '(unless (null? (compile-whole-library "chezpp.wpo" "chezpp.lib"))' \
	      '  (errorf "chezpp.lib" "dependency has to be null"))' \
	      '(unless (null? (compile-whole-library "chezpp/concurrency/fiber.wpo" "chezpp/concurrency/fiber.lib"))' \
	      '  (errorf "fiber.lib" "dependency has to be null"))' \
	      '(unless (null? (compile-whole-library "chezpp/parser/combinator.wpo" "chezpp/parser/combinator.lib"))' \
	      '  (errorf "combinator.lib" "dependency has to be null"))' \
	      | ${SCHEME} -q
	@rm -f chezpp.so

chez++.ss: ${chezpplibs}
	@rm -f chez++.ss
	@echo '(load "$(realpath chezpp.lib)")' \
	      '(load "$(realpath chezpp/concurrency/fiber.lib)")' \
	      '(load "$(realpath chezpp/parser/combinator.lib)")' \
	      '(import (chezpp))' \
	      > chez++.ss

chez++.exe: ${chezppdeps} chez++.ss
	@rm -f chez++
	@echo '#!/usr/bin/env bash'                     >> chez++
	@echo 'SCHEME=${SCHEME}'                        >> chez++
	@echo 'export LIBCHEZPP=$(CURDIR)/libchezpp.so' >> chez++
	@echo '$$SCHEME "$$@" $(realpath chez++.ss)'    >> chez++
	@chmod +x chez++

installdeps: ${chezppdeps}
	install -d $(PREFIX)/bin $(PREFIX)/lib
	install libchezpp.so  $(PREFIX)/lib
	install ${chezpplibs} $(PREFIX)/lib
	install ${chezppwpos} $(PREFIX)/lib

.PHONY: install
install: chez++.exe installdeps
	rm -f $(PREFIX)/bin/chez++ $(PREFIX)/lib/chez++.ss
	@echo '(load "$(realpath $(PREFIX)/lib/chezpp.lib)")' \
	      '(load "$(realpath $(PREFIX)/lib/fiber.lib)")' \
	      '(load "$(realpath $(PREFIX)/lib/combinator.lib)")' \
	      '(import (chezpp))' \
	      > $(PREFIX)/lib/chez++.ss
	@echo '#!/usr/bin/env bash'                                >> $(PREFIX)/bin/chez++
	@echo 'export LIBCHEZPP=$(PREFIX)/lib/libchezpp.so'        >> $(PREFIX)/bin/chez++
	@echo 'SCHEME=${SCHEME}'                                   >> $(PREFIX)/bin/chez++
	@echo '$$SCHEME "$$@" $(realpath $(PREFIX)/lib/chez++.ss)' >> $(PREFIX)/bin/chez++
	chmod +x $(PREFIX)/bin/chez++

.PHONY: clean
clean:
	@rm -f chezpp.lib chezpp.wpo chez++ chez++.ss libchezpp.so
	@find chezpp/ -name '*.so'  -delete
	@find tests/  -name '*.so'  -delete
	@find chezpp/ -name '*.wpo' -delete

.PHONY: dump
dump:
	@echo ${PREFIX}
	@echo ${SCHEME}
	@echo ${SRCS_CHEZPP}
	@echo ${SRCS_C}
