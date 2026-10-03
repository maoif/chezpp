include build-options.mk

SCHEME := scheme
SCHEME_SCRIPT := $(or $(shell command -v $(SCHEME) 2>/dev/null),$(SCHEME))
SCHEME_EXE := $(realpath $(SCHEME_SCRIPT))
SCHEME_INCLUDE_DIR := $(dir $(SCHEME_EXE))
PREFIX := /usr

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_TEST    = $(shell find tests/    -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c' ! -name 'lws_http2_fixture.c')

CC := gcc
CFLAGS := -fPIC -Wall -Wextra -O2 -shared -pthread
CFLAGS += -I$(SCHEME_INCLUDE_DIR)
LDLIBS := -luuid -ldl

chezpplibs = chezpp.lib
chezppwpos = chezpp.wpo
chezppdeps = ${chezpplibs}

.PHONY: all release debug coverage prepare-build print-build-options test

all: chez++

release:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),release) all
debug:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),debug) all
coverage:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),coverage) all

print-build-options:
	$(print-build-options)

prepare-build:
	$(call print-build-options)
	@if [ -f "$(BUILD_OPTIONS_SIGNATURE_FILE)" ]; then \
		old=$$(cat "$(BUILD_OPTIONS_SIGNATURE_FILE)"); \
		if [ "$$old" != "$(BUILD_OPTIONS_SIGNATURE)" ]; then \
			printf '%s\n' 'build options changed; running make clean'; \
			$(MAKE) --no-print-directory clean; \
		fi; \
	else \
		if [ -e chezpp.lib ] || [ -e libchezpp.so ] || find chezpp tests -name '*.so' -print -quit | grep -q .; then \
			printf '%s\n' 'build options signature missing; running make clean'; \
			$(MAKE) --no-print-directory clean; \
		fi; \
	fi

test: chez++
	@$(MAKE) --no-print-directory -C tests test \
		BUILD_VARIANT='$(VARIANT)' \
		$(foreach option,$(BUILD_OPTION_NAMES),BUILD_$(option)='$($(option))') \
		TEST='$(TEST)'

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

.PHONY: run
run: chez++
	@./chez++

.PHONY: protobuf-generate
protobuf-generate: chez++
	@mkdir -p tests/generated
	@chmod +x tools/protoc-gen-chezpp
	@protoc --plugin=protoc-gen-chezpp=tools/protoc-gen-chezpp \
	        --chezpp_out=tests/generated --proto_path=tests/data \
	        tests/data/file-transfer.proto tests/data/codegen-features.proto

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

libchezpp.so: ${SRCS_C} | check-scheme-header prepare-build
	$(CC) $(CFLAGS) -o $@ $(SRCS_C) $(LDLIBS)

chezpp.lib: Makefile build-options.mk chezpp.ss ${SRCS_CHEZPP} libchezpp.so | prepare-build
	@printf '%s\n' '$(CHEZ_COMPILER_FORMS) (compile-imported-libraries #t)' \
	      '(define old-handler (compile-library-handler))' \
	      '(define (compile-with-options thunk)' \
	      '  (parameterize ($(CHEZ_BUILD_COMPILER_FORMS)) (thunk)))' \
	      '(parameterize ([compile-library-handler' \
	      '  (lambda args (compile-with-options (lambda () (apply old-handler args))))])' \
	      '  $(CHEZ_LIBRARY_BUILD_FORMS))' \
	      | ${SCHEME} --script /dev/stdin
	@rm -f chezpp.so
	@if [ "$(wpo)" != t ]; then rm -f chezpp.wpo; fi
	@tmp="$(BUILD_OPTIONS_SIGNATURE_FILE).tmp"; \
	printf '%s\n' '$(BUILD_OPTIONS_SIGNATURE)' > "$$tmp"; \
	mv "$$tmp" "$(BUILD_OPTIONS_SIGNATURE_FILE)"

chez++: prepare-build ${chezppdeps} chez++.in Makefile
	$(call generate_chezpp_launcher,chez++,$(abspath libchezpp.so),$(abspath chezpp.lib),)

.PHONY: chez++.exe
chez++.exe: chez++

installdeps: ${chezppdeps}
	install -d $(PREFIX)/bin $(PREFIX)/lib
	install libchezpp.so  $(PREFIX)/lib
	install ${chezpplibs} $(PREFIX)/lib
	@if [ -f $(chezppwpos) ]; then install $(chezppwpos) $(PREFIX)/lib; fi

.PHONY: install
install: chez++ installdeps
	rm -f $(PREFIX)/bin/chez++ $(PREFIX)/lib/chez++.ss
	$(call generate_chezpp_launcher,$(PREFIX)/bin/chez++,$(abspath $(PREFIX)/lib/libchezpp.so),$(abspath $(PREFIX)/lib/chezpp.lib),)

.PHONY: clean
clean:
	@rm -f chezpp.lib chezpp.wpo chez++ chez++.ss libchezpp.so \
		"$(BUILD_OPTIONS_SIGNATURE_FILE)" "$(BUILD_OPTIONS_SIGNATURE_FILE).tmp"
	@find chezpp/ -name '*.so'  -delete
	@find tests/  -name '*.so'  -delete
	@find chezpp/ -name '*.wpo' -delete
	@find chezpp/ tests/ \( -name '*.covin' -o -name '*.covout' \) -delete
	@rm -f *.covin *.covout

.PHONY: dump
dump:
	@echo ${PREFIX}
	@echo ${SCHEME}
	@echo ${SRCS_CHEZPP}
	@echo ${SRCS_C}
