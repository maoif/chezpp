# Native dependency resolution is shared by all root build targets.
PKG_CONFIG ?= pkg-config
OPTIONAL_DEPENDENCY_NAMES := CARES CURL GRPC IDN2 LIBSSH WEBSOCKETS ZLIB OPENSSL \
                             UUID XXHASH BLAKE3

# Reject empty, multiple, and unknown values before running any probes.
define validate-optional-choice
$(if $(and $(filter 1,$(words $(WITH_$(1)))),$(filter auto 0 1,$(WITH_$(1)))),,\
  $(error WITH_$(1) must be auto, 0, or 1; got '$(WITH_$(1))'))
endef
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(eval WITH_$(name) ?= auto))
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(call validate-optional-choice,$(name)))
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
  $(eval $(name)_CFLAGS_SUPPLIED := $(if $(filter undefined,$(origin $(name)_CFLAGS)),0,1))\
  $(eval $(name)_LIBS_SUPPLIED := $(if $(filter undefined,$(origin $(name)_LIBS)),0,1)))

# Cleaning must still work when an explicitly required dependency is unavailable.
ifeq ($(strip $(filter-out clean,$(MAKECMDGOALS))),)
ifneq ($(strip $(MAKECMDGOALS)),)
_SKIP_OPTIONAL_PROBES := 1
endif
endif
ifeq ($(_SKIP_OPTIONAL_PROBES),1)
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(eval RESOLVED_WITH_$(name) := 0))
else
_OPTIONAL_CONFIG_FILE := $(shell env \
  CC=$(call build-shell-quote,$(CC)) CPPFLAGS=$(call build-shell-quote,$(CPPFLAGS)) \
  CFLAGS=$(call build-shell-quote,$(CFLAGS)) LDFLAGS=$(call build-shell-quote,$(LDFLAGS)) \
  PKG_CONFIG=$(call build-shell-quote,$(PKG_CONFIG)) \
  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
    WITH_$(name)=$(call build-shell-quote,$(strip $(WITH_$(name)))) \
    $(name)_CFLAGS=$(call build-shell-quote,$($(name)_CFLAGS)) \
    $(name)_LIBS=$(call build-shell-quote,$($(name)_LIBS)) \
    $(name)_CFLAGS_SUPPLIED=$($(name)_CFLAGS_SUPPLIED) \
    $(name)_LIBS_SUPPLIED=$($(name)_LIBS_SUPPLIED)) \
  sh $(call build-shell-quote,$(BUILD_ROOT)/tools/probe-optional-libraries.sh))
ifeq ($(_OPTIONAL_CONFIG_FILE),)
$(error Optional dependency configuration failed; see diagnostic above)
endif
$(eval $(file <$(_OPTIONAL_CONFIG_FILE)))
_OPTIONAL_CONFIG_REMOVED := $(shell rm -f $(call build-shell-quote,$(_OPTIONAL_CONFIG_FILE)))
endif

OPTIONAL_CFLAGS := $(strip $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(RESOLVED_$(name)_CFLAGS)))
OPTIONAL_LIBS := $(strip $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(RESOLVED_$(name)_LIBS)))
BUILD_SIGNATURE_NAMES += CC CPPFLAGS CFLAGS LDFLAGS LDLIBS \
  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),WITH_$(name) RESOLVED_WITH_$(name) \
    $(name)_CFLAGS $(name)_LIBS $(name)_CFLAGS_SUPPLIED $(name)_LIBS_SUPPLIED \
    RESOLVED_$(name)_CFLAGS RESOLVED_$(name)_LIBS)

# Shared metadata helpers stay selected and must honor the generated feature macros.
_OPTIONAL_CARES_SOURCES := chezpp/c/net/dns.c
_OPTIONAL_CURL_SOURCES := chezpp/c/net/ftp.c
_OPTIONAL_GRPC_SOURCES := chezpp/c/net/grpc.c
_OPTIONAL_IDN2_SOURCES := chezpp/c/net/idna.c
_OPTIONAL_LIBSSH_SOURCES := chezpp/c/net/ssh.c
_OPTIONAL_WEBSOCKETS_SOURCES := chezpp/c/net/lws_http.c chezpp/c/net/websocket.c
_OPTIONAL_ZLIB_SOURCES := chezpp/c/zlib_loader.c
_OPTIONAL_OPENSSL_SOURCES := chezpp/c/crypto.c chezpp/c/net/tls.c
_OPTIONAL_UUID_SOURCES := chezpp/c/uuid.c
_OPTIONAL_XXHASH_SOURCES := chezpp/c/hash.c
# digest.c implements both BLAKE3 and OpenSSL and guards their bodies independently.
_OPTIONAL_BLAKE3_SOURCES :=
_OPTIONAL_ZLIB_FALLBACKS := chezpp/c/zlib_unavailable.c
$(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),\
  $(if $(filter 0,$(RESOLVED_WITH_$(name))),\
    $(eval SRCS_C := $(filter-out $(_OPTIONAL_$(name)_SOURCES),$(SRCS_C)) \
      $(or $(_OPTIONAL_$(name)_FALLBACKS),$(patsubst %.c,%_unavailable.c,$(_OPTIONAL_$(name)_SOURCES))))))

.PHONY: force-build-config
chezpp/c/build-config.h: force-build-config | prepare-build
	@sh tools/probe-optional-libraries.sh header $@ \
	  $(foreach name,$(OPTIONAL_DEPENDENCY_NAMES),$(name)=$(RESOLVED_WITH_$(name)))
