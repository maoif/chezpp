#ifndef CHEZPP_OPTIONAL_LIBRARY_H
#define CHEZPP_OPTIONAL_LIBRARY_H

#include <pthread.h>
#include <stddef.h>

typedef struct {
  const char *name;
  const char *const *sonames;
  void *handle;
  pthread_mutex_t mutex;
  int state; /* 0 uninitialized, 1 ready, -1 failed */
  char loaded_name[128];
  char version[128];
  char error[512];
} chezpp_optional_library;

#define CHEZPP_OPTIONAL_LIBRARY_INIT(label, candidates)                       \
  { label, candidates, NULL, PTHREAD_MUTEX_INITIALIZER, 0, "", "", "" }

int chezpp_optional_library_open(chezpp_optional_library *library);
int chezpp_optional_library_symbol(chezpp_optional_library *library,
                                   const char *name, void **target);
void chezpp_optional_library_fail(chezpp_optional_library *library,
                                  const char *format, ...);
void chezpp_optional_library_set_version(chezpp_optional_library *library,
                                         const char *version);
const char *chezpp_optional_library_error(chezpp_optional_library *library);
void chezpp_optional_library_reset_for_test(chezpp_optional_library *library);

#endif
