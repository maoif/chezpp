SCHEME := scheme
PREFIX := /usr

# TODO include Chez header file

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_TEST    = $(shell find tests/    -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c')

CC := gcc
CFLAGS := -fPIC -Wall -Wextra -O2 -shared

.PHONY: all
all: chez++.exe

.PHONY: run
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

chez++.ss: chezpp.lib
	@rm -f chez++.ss
	@echo '(load "$(realpath chezpp.lib)")(import (chezpp))' > chez++.ss

chez++.exe: chezpp.lib chezpp.wpo chez++.ss
	@rm -f chez++
	@echo '#!/usr/bin/env bash'                     >> chez++
	@echo 'SCHEME=${SCHEME}'                        >> chez++
	@echo 'export LIBCHEZPP=$(CURDIR)/libchezpp.so' >> chez++
	@echo '$$SCHEME "$$@" $(realpath chez++.ss)'    >> chez++
	@chmod +x chez++

.PHONY: install
install: chez++.exe
	rm -f $(PREFIX)/bin/chez++ $(PREFIX)/lib/chez++.ss
	install -d $(PREFIX)/bin $(PREFIX)/lib
	install chezpp.lib chezpp.wpo libchezpp.so $(PREFIX)/lib
	@echo '(load "$(realpath $(PREFIX)/lib/chezpp.lib)")(import (chezpp))' > $(PREFIX)/lib/chez++.ss
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
