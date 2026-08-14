SCHEME := scheme
SCHEME_SCRIPT := $(or $(shell command -v $(SCHEME) 2>/dev/null),$(SCHEME))
SCHEME_EXE := $(realpath $(SCHEME_SCRIPT))
SCHEME_INCLUDE_DIR := $(dir $(SCHEME_EXE))
PREFIX := /usr

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_TEST    = $(shell find tests/    -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c')

CC := gcc
CFLAGS := -fPIC -Wall -Wextra -O2 -shared -pthread
CFLAGS += -I$(SCHEME_INCLUDE_DIR)
LDLIBS := -luuid -ldl

chezpplibs = chezpp.lib \
             chezpp/concurrency/fiber.lib
chezppwpos = chezpp.wpo \
             chezpp/concurrency/fiber.wpo
chezppdeps = ${chezpplibs} ${chezppwpos}

define generate_chezpp_launcher
	@rm -f $(1)
	@sed \
	      -e 's|@SCHEME_SCRIPT@|$(SCHEME_SCRIPT)|g' \
	      -e 's|@LIBCHEZPP@|$(2)|g' \
	      -e 's|@CHEZPP_LIB@|$(3)|g' \
	      -e 's|@FIBER_LIB@|$(4)|g' \
	      chez++.in > $(1)
	@chmod +x $(1)
endef

.PHONY: all
all: chez++

.PHONY: run
run: chez++
	@./chez++

.PHONY: protobuf-generate
protobuf-generate: chez++
	@mkdir -p tests/generated
	@chmod +x tools/protoc-gen-chezpp
	@protoc --plugin=protoc-gen-chezpp=tools/protoc-gen-chezpp \
	        --chezpp_out=tests/generated --proto_path=tests/data \
	        tests/data/file-transfer.proto

.PHONY: check-scheme-header
check-scheme-header:
	@header='$(SCHEME_INCLUDE_DIR)/scheme.h'; \
	if [ ! -r "$$header" ]; then \
	  echo "error: ChezScheme header not found or unreadable: $$header" >&2; \
	  exit 1; \
	fi; \
	header_version=$$(sed -n 's/^#define VERSION "\([^"]*\)"/\1/p' "$$header" | head -n 1); \
	if [ -z "$$header_version" ]; then \
	  echo "error: ChezScheme header version not found in $$header" >&2; \
	  exit 1; \
	fi; \
	scheme_version=$$($(SCHEME) --version 2>&1); \
	if [ "$$header_version" != "$$scheme_version" ]; then \
	  echo "error: ChezScheme header version mismatch: $$header reports $$header_version; $(SCHEME) reports $$scheme_version" >&2; \
	  exit 1; \
	fi

libchezpp.so: check-scheme-header
	$(CC) $(CFLAGS) -o $@ $(SRCS_C) $(LDLIBS)

${chezppdeps}: chezpp.ss ${SRCS_CHEZPP} libchezpp.so
	@echo '(optimize-level 1)' \
	      '(compile-imported-libraries #t) (generate-inspector-information #t) (generate-procedure-source-information #t)'\
	      '(generate-wpo-files #t)' \
	      '(time (compile-file "chezpp.ss"))' \
	      '(unless (null? (compile-whole-library "chezpp.wpo" "chezpp.lib"))' \
	      '  (errorf "chezpp.lib" "dependency has to be null"))' \
	      '(unless (null? (compile-whole-library "chezpp/concurrency/fiber.wpo" "chezpp/concurrency/fiber.lib"))' \
	      '  (errorf "fiber.lib" "dependency has to be null"))' \
	      | ${SCHEME} -q
	@rm -f chezpp.so

chez++: ${chezppdeps} chez++.in Makefile
	$(call generate_chezpp_launcher,chez++,$(abspath libchezpp.so),$(abspath chezpp.lib),$(abspath chezpp/concurrency/fiber.lib))

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
