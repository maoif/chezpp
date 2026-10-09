.DEFAULT_GOAL := all

ifneq ($(strip $(NATIVE_OPTIONS_REPLAY)),)
include $(NATIVE_OPTIONS_REPLAY)
endif

include build-options.mk

override CHEZ_SOURCE_DIR := vendor/ChezScheme
override CHEZ_BUILD_DIR := .chezscheme-build
override CHEZ_INSTALL_DIR := .chezscheme-install
override CHEZ_SCHEME := $(abspath $(CHEZ_INSTALL_DIR)/bin/scheme)
override CHEZ_INCLUDE_DIR = $(dir $(realpath $(CHEZ_SCHEME)))
override SCHEME_SCRIPT := $(CHEZ_SCHEME)
override CHEZ_TOOLCHAIN_SIGNATURE := $(CHEZ_BUILD_DIR)/.chezscheme-signature
PREFIX := /usr

SRCS_CHEZPP := $(shell find chezpp/   -type f -name '*.ss')
SRCS_C      := $(shell find chezpp/c/ -type f -name '*.c' ! -name 'lws_http2_fixture.c' ! -name '*_unavailable.c')

ifneq ($(filter test,$(MAKECMDGOALS)),)
include tests/test-files.mk
TEST_FILE_GOALS := $(filter-out test,$(MAKECMDGOALS))
INVALID_TEST_FILES := $(filter-out $(SRCS_TEST),$(TEST_FILE_GOALS))
ifneq ($(strip $(INVALID_TEST_FILES)),)
$(error unsupported test file(s): $(INVALID_TEST_FILES))
endif
ifneq ($(strip $(TEST_FILE_GOALS)),)
.PHONY: $(TEST_FILE_GOALS)
$(TEST_FILE_GOALS): ;
endif
endif

ifeq ($(origin CC),default)
CC := gcc
endif
CC ?= gcc
CFLAGS ?= -fPIC -Wall -Wextra -O2 -pthread
LDLIBS ?=

include optional-libraries.mk

chezpplibs = chezpp.lib
chezppwpos = chezpp.wpo
chezppdeps = ${chezpplibs}

.PHONY: all release debug coverage prepare-build print-build-options test bundled-chez clean-all

all: chez++

release:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),release) all
debug:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),debug) all
coverage:
	@$(MAKE) --no-print-directory VARIANT=$(if $(filter command line,$(origin VARIANT)),$(VARIANT),coverage) all

print-build-options:
	$(print-build-options)

prepare-build: bundled-chez
	$(call print-build-options)
	@if [ -f "$(BUILD_OPTIONS_SIGNATURE_FILE)" ]; then \
		old=$$(cat "$(BUILD_OPTIONS_SIGNATURE_FILE)"); \
		new=$(call build-shell-quote,$(BUILD_OPTIONS_SIGNATURE)); \
		if [ "$$old" != "$$new" ]; then \
			printf '%s\n' 'build options changed; running make clean'; \
			$(MAKE) --no-print-directory clean; \
		fi; \
	else \
		if [ -e chezpp.lib ] || [ -e libchezpp.so ] || find chezpp tests -name '*.so' -print -quit | grep -q .; then \
			printf '%s\n' 'build options signature missing; running make clean'; \
			$(MAKE) --no-print-directory clean; \
		fi; \
	fi

bundled-chez:
	@set -eu; \
	source='$(abspath $(CHEZ_SOURCE_DIR))'; \
	build='$(abspath $(CHEZ_BUILD_DIR))'; \
	install='$(abspath $(CHEZ_INSTALL_DIR))'; \
	commit=unversioned; \
	if [ -e .git ]; then \
		submodules=$$(git submodule status --recursive -- "$(CHEZ_SOURCE_DIR)"); \
		if printf '%s\n' "$$submodules" | grep -Eq '^[-+U]'; then \
			git submodule update --init --depth 1 --filter=blob:none --recursive -- "$(CHEZ_SOURCE_DIR)"; \
		fi; \
		commit=$$(git -C "$$source" rev-parse HEAD); \
	fi; \
	signature="commit=$$commit configure=$$source/configure --installprefix=$$install"; \
	old_signature=$$(cat "$$build/.chezscheme-signature" 2>/dev/null || :); \
	if [ "$$old_signature" != "$$signature" ] || [ ! -f "$$build/Makefile" ] || \
		[ ! -x "$$install/bin/scheme" ]; then \
		$(MAKE) --no-print-directory clean; \
		rm -rf "$$build" "$$install"; \
		mkdir -p "$$build"; \
		( cd "$$build" && "$$source/configure" --installprefix="$$install" ); \
		$(MAKE) --no-print-directory -C "$$build"; \
		$(MAKE) --no-print-directory -C "$$build" install; \
		printf '%s\n' "$$signature" > "$$build/.chezscheme-signature"; \
	fi

$(CHEZ_TOOLCHAIN_SIGNATURE): | bundled-chez
	@test -f "$@"

test: chez++
	@$(MAKE) --no-print-directory -C tests test \
		BUILD_VARIANT='$(VARIANT)' \
		$(foreach option,$(BUILD_OPTION_NAMES),BUILD_$(option)='$($(option))') \
		$(TEST_FILE_GOALS)

define generate_chezpp_launcher
	@rm -f $(1)
	@sed \
	      -e 's|@SCHEME_SCRIPT@|$(if $(5),$(5),$(SCHEME_SCRIPT))|g' \
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

libchezpp.so: ${SRCS_C} chezpp/c/build-config.h $(CHEZ_TOOLCHAIN_SIGNATURE) | prepare-build
	$(CC) $(CPPFLAGS) -I$(CHEZ_INCLUDE_DIR) $(CFLAGS) $(OPTIONAL_CFLAGS) \
	  -include chezpp/c/build-config.h \
	  -shared $(LDFLAGS) -o $@ $(SRCS_C) $(LDLIBS) $(OPTIONAL_LIBS)

chezpp.lib: Makefile build-options.mk optional-libraries.mk tools/probe-optional-libraries.sh chezpp.ss ${SRCS_CHEZPP} libchezpp.so $(CHEZ_TOOLCHAIN_SIGNATURE) | prepare-build
	@printf '%s\n' '$(CHEZ_COMPILER_FORMS) (compile-imported-libraries #t)' \
	      '(define old-handler (compile-library-handler))' \
	      '(define (compile-with-options thunk)' \
	      '  (parameterize ($(CHEZ_BUILD_COMPILER_FORMS)) (thunk)))' \
	      '(parameterize ([compile-library-handler' \
	      '  (lambda args (compile-with-options (lambda () (apply old-handler args))))])' \
	      '  $(CHEZ_LIBRARY_BUILD_FORMS))' \
	      | $(CHEZ_SCHEME) --script /dev/stdin
	@rm -f chezpp.so
	@if [ "$(wpo)" != t ]; then rm -f chezpp.wpo; fi
	@tmp="$(BUILD_OPTIONS_SIGNATURE_FILE).tmp"; \
	printf '%s\n' $(call build-shell-quote,$(BUILD_OPTIONS_SIGNATURE)) > "$$tmp"; \
	mv "$$tmp" "$(BUILD_OPTIONS_SIGNATURE_FILE)"


chez++: bundled-chez prepare-build ${chezppdeps} $(NATIVE_OPTIONS_FILE) chez++.in Makefile
	$(call generate_chezpp_launcher,chez++,$(abspath libchezpp.so),$(abspath chezpp.lib),)
	@rm -f scheme
	@ln -s "$(CHEZ_SCHEME)" scheme

.PHONY: chez++.exe
chez++.exe: chez++

installdeps: ${chezppdeps}
	@case "$(PREFIX)" in /*) ;; *) printf '%s\n' 'error: PREFIX must be an absolute path' >&2; exit 2 ;; esac
	install -d $(PREFIX)/bin $(PREFIX)/lib
	install libchezpp.so  $(PREFIX)/lib
	install ${chezpplibs} $(PREFIX)/lib
	@if [ -f $(chezppwpos) ]; then install $(chezppwpos) $(PREFIX)/lib; fi


.PHONY: check-install-prefix install-bundled-chez
check-install-prefix:
	@case "$(PREFIX)" in /*) ;; *) printf '%s\n' 'error: PREFIX must be an absolute path' >&2; exit 2 ;; esac

install-bundled-chez: check-install-prefix bundled-chez
	@set -eu; \
	case "$(PREFIX)" in /*) ;; *) printf '%s\n' 'error: PREFIX must be an absolute path' >&2; exit 2 ;; esac; \
	install -d "$(PREFIX)/bin" "$(PREFIX)/lib"; \
	cp -a "$(CHEZ_INSTALL_DIR)/bin/." "$(PREFIX)/bin/"; \
	cp -a "$(CHEZ_INSTALL_DIR)/lib/." "$(PREFIX)/lib/"

.PHONY: install
install: check-install-prefix chez++ installdeps install-bundled-chez
	rm -f $(PREFIX)/bin/chez++ $(PREFIX)/lib/chez++.ss
	$(call generate_chezpp_launcher,$(PREFIX)/bin/chez++,$(abspath $(PREFIX)/lib/libchezpp.so),$(abspath $(PREFIX)/lib/chezpp.lib),,$(abspath $(PREFIX)/bin/scheme))

.PHONY: clean
clean:
	@rm -f chezpp.lib chezpp.wpo chez++ chez++.ss libchezpp.so \
		"$(BUILD_OPTIONS_SIGNATURE_FILE)" "$(BUILD_OPTIONS_SIGNATURE_FILE).tmp" \
		"$(NATIVE_OPTIONS_FILE)" "$(NATIVE_OPTIONS_FILE)".tmp.* \
		chezpp/c/build-config.h chezpp/c/build-config.h.tmp.*
	@find chezpp/ -name '*.so'  -delete
	@find tests/  -name '*.so'  -delete
	@find chezpp/ -name '*.wpo' -delete
	@find chezpp/ tests/ \( -name '*.covin' -o -name '*.covout' \) -delete
	@rm -f *.covin *.covout

clean-all: clean
	@rm -rf "$(CHEZ_BUILD_DIR)" "$(CHEZ_INSTALL_DIR)" scheme

.PHONY: dump
dump:
	@echo ${PREFIX}
	@echo ${CHEZ_SCHEME}
	@echo ${SRCS_CHEZPP}
	@echo ${SRCS_C}
