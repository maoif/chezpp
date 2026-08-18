#include "nghttp2_loader.h"

#include <nghttp2/nghttp2.h>
#include <pthread.h>
#include <stdio.h>

static const char *const nghttp2_names[] = {"libnghttp2.so.14", NULL};
static chezpp_optional_library nghttp2_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("nghttp2", nghttp2_names);
static pthread_once_t nghttp2_once = PTHREAD_ONCE_INIT;
static int nghttp2_available;

typedef nghttp2_info *(*nghttp2_version_fn)(int);

static const char *const required_symbols[] = {
    "nghttp2_session_callbacks_new", "nghttp2_session_callbacks_del",
    "nghttp2_session_client_new2", "nghttp2_session_server_new2",
    "nghttp2_option_new", "nghttp2_option_del",
    "nghttp2_option_set_no_auto_window_update",
    "nghttp2_session_set_user_data",
    "nghttp2_session_callbacks_set_on_header_callback",
    "nghttp2_session_callbacks_set_on_data_chunk_recv_callback",
    "nghttp2_session_callbacks_set_on_frame_recv_callback",
    "nghttp2_session_callbacks_set_on_stream_close_callback",
    "nghttp2_session_del", "nghttp2_submit_request", "nghttp2_submit_response",
    "nghttp2_submit_rst_stream", "nghttp2_submit_goaway",
    "nghttp2_session_mem_recv", "nghttp2_session_mem_send",
    "nghttp2_session_consume", "nghttp2_session_want_read",
    "nghttp2_session_want_write", "nghttp2_session_get_remote_settings", NULL};

static void initialize_nghttp2(void) {
  nghttp2_version_fn version_fn = NULL;
  nghttp2_info *info;
  size_t index;

  if (!chezpp_optional_library_open(&nghttp2_library)) return;
  if (!chezpp_optional_library_symbol(&nghttp2_library, "nghttp2_version",
                                      (void **)&version_fn))
    return;
  info = version_fn(0);
  if (info == NULL || info->version_str == NULL) {
    chezpp_optional_library_fail(&nghttp2_library,
                                 "nghttp2 returned no runtime version");
    return;
  }
  chezpp_optional_library_set_version(&nghttp2_library, info->version_str);
  if (info->version_num < 0x013400) {
    chezpp_optional_library_fail(
        &nghttp2_library, "nghttp2 runtime %s; requires SONAME 14 and version >= 1.52.0",
        info->version_str);
    return;
  }
  for (index = 0; required_symbols[index] != NULL; index++) {
    void *symbol = NULL;
    if (!chezpp_optional_library_symbol(&nghttp2_library,
                                        required_symbols[index], &symbol))
      return;
  }
  nghttp2_available = 1;
}

int chezpp_nghttp2_require(void) {
  pthread_once(&nghttp2_once, initialize_nghttp2);
  return nghttp2_available;
}

const chezpp_optional_library *chezpp_nghttp2_library(void) {
  (void)chezpp_nghttp2_require();
  return &nghttp2_library;
}

void *chezpp_nghttp2_symbol(const char *name) {
  void *symbol = NULL;
  if (name == NULL || !chezpp_nghttp2_require()) return NULL;
  if (!chezpp_optional_library_symbol(&nghttp2_library, name, &symbol)) return NULL;
  return symbol;
}
