#ifndef CSPP_OPTIONAL_LIBRARY_H
#define CSPP_OPTIONAL_LIBRARY_H
#include "build-config.h"
#include <pthread.h>
#include <stddef.h>
typedef struct {
  const char *name;
  pthread_mutex_t mutex;
  int state;
  char version[128];
  char error[512];
} cspp_optional_library;
#define CSPP_OPTIONAL_LIBRARY_INIT(label) \
  { label, PTHREAD_MUTEX_INITIALIZER, 1, "", "" }
typedef struct {
  const char *name;
  int enabled;
  const char *(*version)(void);
  unsigned (*capabilities)(void);
  const char *error;
} cspp_optional_descriptor;
void cspp_optional_library_fail(cspp_optional_library *library, const char *format, ...);
void cspp_optional_library_set_version(cspp_optional_library *library, const char *version);
const char *cspp_optional_library_error(cspp_optional_library *library);
#endif
