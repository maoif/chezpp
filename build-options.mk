# Shared ChezPP build-profile resolution for the root and tests Makefiles.

BUILD_ROOT ?= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILD_OPTIONS_SIGNATURE_FILE ?= $(BUILD_ROOT)/.chezpp-build-options

BUILD_OPTION_NAMES := o d cl i cp0 fc xf xl p xp bp xbp c loadspd dumpspd \
                      loadbpd dumpbpd compile pdhtml gac gic pps psi wpo
BUILD_SIGNATURE_NAMES := VARIANT $(BUILD_OPTION_NAMES)

VARIANT ?= release
ifeq ($(filter $(VARIANT),release debug coverage),)
$(error VARIANT must be release, debug, or coverage)
endif

_variant_o := 3
_variant_d := 0
_variant_cl :=
_variant_i := t
_variant_cp0 :=
_variant_fc :=
_variant_xf :=
_variant_xl :=
_variant_p :=
_variant_xp :=
_variant_bp :=
_variant_xbp :=
_variant_c := f
_variant_loadspd :=
_variant_dumpspd :=
_variant_loadbpd :=
_variant_dumpbpd :=
_variant_compile := compile-file
_variant_pdhtml :=
_variant_gac :=
_variant_gic :=
_variant_pps :=
_variant_psi := t
_variant_wpo := t

ifeq ($(VARIANT),debug)
_variant_o := 0
_variant_d := 3
endif
ifeq ($(VARIANT),coverage)
_variant_o := 0
_variant_c := t
_variant_psi := t
_variant_p := t
endif

# Command-line and environment values remain authoritative over profile defaults.
$(foreach option,$(BUILD_OPTION_NAMES),$(eval $(option) ?= $(_variant_$(option))))
BUILD_TEST_COVERAGE := f
BUILD_TEST_OPTIMIZE_LEVEL ?= $(if $(filter 0,$(o)),0,2)
BUILD_OPTIONS_SIGNATURE := $(foreach option,$(BUILD_SIGNATURE_NAMES),$(option)=$($(option)))

_release_signature := VARIANT=release o=3 d=0 cl= i=t cp0= fc= xf= xl= \
  p= xp= bp= xbp= c=f loadspd= dumpspd= loadbpd= dumpbpd= compile=compile-file \
  pdhtml= gac= gic= pps= psi=t wpo=t
_debug_signature := VARIANT=debug o=0 d=3 cl= i=t cp0= fc= xf= xl= \
  p= xp= bp= xbp= c=f loadspd= dumpspd= loadbpd= dumpbpd= compile=compile-file \
  pdhtml= gac= gic= pps= psi=t wpo=t
_coverage_signature := VARIANT=coverage o=0 d=0 cl= i=t cp0= fc= xf= xl= \
  p= xp= bp= xbp= c=t loadspd= dumpspd= loadbpd= dumpbpd= compile=compile-file \
  pdhtml= gac= gic= pps= psi=t wpo=t

BUILD_VARIANT_LABEL :=
ifeq ($(BUILD_OPTIONS_SIGNATURE),$(_release_signature))
BUILD_VARIANT_LABEL := release
endif
ifeq ($(BUILD_OPTIONS_SIGNATURE),$(_debug_signature))
BUILD_VARIANT_LABEL := debug
endif
ifeq ($(BUILD_OPTIONS_SIGNATURE),$(_coverage_signature))
BUILD_VARIANT_LABEL := coverage
endif

define print-build-options
	@printf '\033[1;36mChezPP build options\033[0m'; \
	if [ -n '$(BUILD_VARIANT_LABEL)' ]; then printf ' (variant=%s)' '$(BUILD_VARIANT_LABEL)'; fi; \
	printf '\n'; \
	printf '\033[2m%s\033[0m\n' '$(BUILD_OPTIONS_SIGNATURE)'
endef

_scheme_true := $(shell printf '\043t')
_scheme_false := $(shell printf '\043f')
_chez_bool = $(if $(filter t true yes 1,$(strip $(1))),$(_scheme_true),$(_scheme_false))
CHEZ_COMPILER_FORMS := (optimize-level $(o)) (debug-level $(d)) \
                       (generate-inspector-information $(call _chez_bool,$(i))) \
                       (generate-procedure-source-information $(call _chez_bool,$(psi)))
ifneq ($(strip $(cl)),)
CHEZ_COMPILER_FORMS += (commonization-level $(cl))
endif
ifneq ($(strip $(cp0)),)
CHEZ_COMPILER_FORMS += (cp0-effort-limit $(cp0))
endif
ifneq ($(strip $(fc)),)
CHEZ_COMPILER_FORMS += (fasl-compressed $(call _chez_bool,$(fc)))
endif
ifneq ($(strip $(c)),)
CHEZ_COMPILER_FORMS += (generate-covin-files $(call _chez_bool,$(c)))
endif
ifneq ($(strip $(p)),)
CHEZ_COMPILER_FORMS += (compile-profile $(call _chez_bool,$(p)))
endif
ifneq ($(strip $(xp)),)
CHEZ_COMPILER_FORMS += (generate-profile-forms $(call _chez_bool,$(xp)))
endif
CHEZ_COMPILER_FORMS += (generate-wpo-files $(call _chez_bool,$(wpo)))

CHEZ_TEST_COMPILER_FORMS := (optimize-level $(BUILD_TEST_OPTIMIZE_LEVEL)) (debug-level $(d)) \
                            (generate-inspector-information $(call _chez_bool,$(i))) \
                            (generate-procedure-source-information $(call _chez_bool,$(psi))) \
                            (generate-covin-files $(_scheme_false)) (generate-wpo-files $(_scheme_false))

ifeq ($(wpo),t)
CHEZ_WHOLE_LIBRARY_FORM := (compile-with-options (lambda () (unless (null? (compile-whole-library "chezpp.wpo" "chezpp.lib")) (errorf "chezpp.lib" "dependency has to be null"))))
else
CHEZ_WHOLE_LIBRARY_FORM := (compile-with-options (lambda () (compile-whole-library "chezpp.wpo" "chezpp.lib")))
endif

CHEZ_BUILD_COMPILER_FORMS := $(CHEZ_COMPILER_FORMS)
ifeq ($(wpo),f)
CHEZ_BUILD_COMPILER_FORMS := $(subst (generate-wpo-files $(_scheme_false)),,$(CHEZ_COMPILER_FORMS)) (generate-wpo-files $(_scheme_true))
endif
CHEZ_LIBRARY_BUILD_FORMS := (compile-with-options (lambda () (time (compile-file "chezpp.ss")))) \
                            $(CHEZ_WHOLE_LIBRARY_FORM)
