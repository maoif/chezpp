#!/bin/sh
# Select feature policy after including LWS headers so its ABI stays unchanged.
set -eu
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
task_directory=$(mktemp -d)
trap 'rm -rf "$task_directory"' EXIT HUP INT TERM
cd "$project_root"
# Observe real adapter startup in fresh processes so each ordering starts with
# unused once controls. Link only reachable native paths; no Scheme runtime runs.
tls_source=
if [ "$(sed -n 's/^#define CSPP_WITH_OPENSSL //p' src/cspp/c/build-config.h)" = 1 ]; then
  tls_source=src/cspp/c/net/tls.c
fi
${CC:-cc} ${CPPFLAGS:-} ${LWS_CFLAGS:-} -pthread -Wall -Wextra -O2 \
  -ffunction-sections -fdata-sections -I "$CHEZ_INCLUDE_DIR" \
  tests/lws-log-init.c src/cspp/c/net/lws_loader.c src/cspp/c/net/lws_http.c \
  src/cspp/c/net/websocket.c $tls_source src/cspp/c/optional_library.c \
  -Wl,--gc-sections -Wl,--wrap=lws_set_log_level -Wl,--wrap=lws_create_context \
  ${LDFLAGS:-} ${LWS_LIBS:-} -o "$task_directory/log-init"
for ordering in http-first websocket-first concurrent; do
  timeout 15s "$task_directory/log-init" "$ordering"
done
for feature in none LWS_WITH_TLS_JIT_TRUST LWS_WITH_SYS_ASYNC_DNS \
               LWS_WITH_SYS_DHCP_CLIENT LWS_WITH_SYS_NTPCLIENT; do
  {
    printf '#include <libwebsockets.h>\n'
    # Start with a known policy, independently of the installed feature choices.
    printf '#undef LWS_WITH_TLS_JIT_TRUST\n#undef LWS_WITH_SYS_ASYNC_DNS\n'
    printf '#undef LWS_WITH_SYS_DHCP_CLIENT\n#undef LWS_WITH_SYS_NTPCLIENT\n'
    if [ "$feature" != none ]; then printf '#define %s 1\n' "$feature"; fi
  } > "$task_directory/policy.h"
  # Only loader policy uses the extra definitions. Adapter and test see real ABI headers.
  ${CC:-cc} ${CPPFLAGS:-} ${LWS_CFLAGS:-} -pthread -Wall -Wextra -O2 \
    -ffunction-sections -fdata-sections -I "$CHEZ_INCLUDE_DIR" \
    -include "$task_directory/policy.h" -c src/cspp/c/net/lws_loader.c \
    -o "$task_directory/loader.o"
  ${CC:-cc} ${CPPFLAGS:-} ${LWS_CFLAGS:-} -pthread -Wall -Wextra -O2 \
    -ffunction-sections -fdata-sections -I "$CHEZ_INCLUDE_DIR" \
    tests/lws-runtime-lock.c src/cspp/c/net/lws_http.c "$task_directory/loader.o" \
    -Wl,--gc-sections ${LDFLAGS:-} ${LWS_LIBS:-} -o "$task_directory/check"
  expected=0
  if [ "$feature" != none ] && \
     ${CC:-cc} ${CPPFLAGS:-} ${LWS_CFLAGS:-} -dM -E -include libwebsockets.h - </dev/null | \
       grep -Eq '^#define LWS_MAX_SMP 1$'; then expected=1; fi
  timeout 15s "$task_directory/check" "$expected"
done
