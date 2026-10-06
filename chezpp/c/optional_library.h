#ifndef CHEZPP_OPTIONAL_LIBRARY_H
#define CHEZPP_OPTIONAL_LIBRARY_H
#include "build-config.h"
#include <pthread.h>
#include <stddef.h>
typedef struct {
  const char *name;
  pthread_mutex_t mutex;
  int state;
  char version[128];
  char error[512];
} chezpp_optional_library;
#define CHEZPP_OPTIONAL_LIBRARY_INIT(label) \
  { label, PTHREAD_MUTEX_INITIALIZER, 1, "", "" }
typedef struct {
  const char *name;
  int enabled;
  const char *(*version)(void);
  unsigned (*capabilities)(void);
  const char *error;
} chezpp_optional_descriptor;
void chezpp_optional_library_fail(chezpp_optional_library *library, const char *format, ...);
void chezpp_optional_library_set_version(chezpp_optional_library *library, const char *version);
const char *chezpp_optional_library_error(chezpp_optional_library *library);
#endif
