SCHEME := scheme
SCHEME_SCRIPT := $(or $(shell command -v $(SCHEME) 2>/dev/null),$(SCHEME))
PREFIX := /usr

# TODO include Chez header file

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_TEST    = $(shell find tests/    -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c')

CC := gcc
CFLAGS := -fPIC -Wall -Wextra -O2 -shared -luuid -lssl -lcrypto -lxxhash -lblake3

chezpplibs = chezpp.lib \
             chezpp/concurrency/fiber.lib \
             chezpp/parser/combinator.lib

chezppwpos = chezpp.wpo \
             chezpp/concurrency/fiber.wpo \
             chezpp/parser/combinator.wpo

chezppdeps = ${chezpplibs} ${chezppwpos}

define generate_chezpp_launcher
	@rm -f $(1)
	@sed \
	      -e 's|@SCHEME_SCRIPT@|$(SCHEME_SCRIPT)|g' \
	      -e 's|@LIBCHEZPP@|$(2)|g' \
	      -e 's|@CHEZPP_LIB@|$(3)|g' \
	      -e 's|@FIBER_LIB@|$(4)|g' \
	      -e 's|@COMBINATOR_LIB@|$(5)|g' \
	      chez++.in > $(1)
	@chmod +x $(1)
endef

.PHONY: all
all: chez++

.PHONY: run
run: chez++
	@./chez++

libchezpp.so:
	$(CC) $(CFLAGS) -o $@ $(SRCS_C)

${chezppdeps}: chezpp.ss ${SRCS_CHEZPP} libchezpp.so
	@echo '(optimize-level 1)' \
	      '(compile-imported-libraries #t) (generate-inspector-information #t) (generate-procedure-source-information #t)'\
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

chez++: ${chezppdeps} chez++.in Makefile
	$(call generate_chezpp_launcher,chez++,$(abspath libchezpp.so),$(abspath chezpp.lib),$(abspath chezpp/concurrency/fiber.lib),$(abspath chezpp/parser/combinator.lib))

.PHONY: chez++.exe
chez++.exe: chez++

installdeps: ${chezppdeps}
	install -d $(PREFIX)/bin $(PREFIX)/lib
	install libchezpp.so  $(PREFIX)/lib
	install ${chezpplibs} $(PREFIX)/lib
	install ${chezppwpos} $(PREFIX)/lib

.PHONY: install
install: chez++ installdeps
	rm -f $(PREFIX)/bin/chez++ $(PREFIX)/lib/chez++.ss
	$(call generate_chezpp_launcher,$(PREFIX)/bin/chez++,$(abspath $(PREFIX)/lib/libchezpp.so),$(abspath $(PREFIX)/lib/chezpp.lib),$(abspath $(PREFIX)/lib/fiber.lib),$(abspath $(PREFIX)/lib/combinator.lib))

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
