#include "optional_library.h"

#include <dlfcn.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static void close_handle(chezpp_optional_library *library) {
  if (library->handle != NULL) {
    dlclose(library->handle);
    library->handle = NULL;
  }
}

static void fail_locked(chezpp_optional_library *library, const char *format,
                        va_list arguments) {
  close_handle(library);
  vsnprintf(library->error, sizeof(library->error), format, arguments);
  library->state = -1;
}

static void append_error(chezpp_optional_library *library, const char *format,
                         ...) {
  size_t used = strlen(library->error);
  va_list arguments;

  if (used >= sizeof(library->error) - 1) return;
  va_start(arguments, format);
  vsnprintf(library->error + used, sizeof(library->error) - used, format,
            arguments);
  va_end(arguments);
}

int chezpp_optional_library_open(chezpp_optional_library *library) {
  size_t index;

  if (library == NULL) return 0;
  pthread_mutex_lock(&library->mutex);
  if (library->state != 0) {
    int available = library->state == 1;
    pthread_mutex_unlock(&library->mutex);
    return available;
  }

  library->error[0] = '\0';
  library->loaded_name[0] = '\0';
  library->version[0] = '\0';
  snprintf(library->error, sizeof(library->error), "%s: unable to load",
           library->name);
  for (index = 0; library->sonames[index] != NULL; index++) {
    const char *loader_error;

    dlerror();
    library->handle =
        dlopen(library->sonames[index], RTLD_NOW | RTLD_LOCAL);
    if (library->handle != NULL) {
      snprintf(library->loaded_name, sizeof(library->loaded_name), "%s",
               library->sonames[index]);
      library->error[0] = '\0';
      library->state = 1;
      pthread_mutex_unlock(&library->mutex);
      return 1;
    }
    loader_error = dlerror();
    append_error(library, "%s%s", index == 0 ? " " : "; ",
                 library->sonames[index]);
    if (loader_error != NULL) append_error(library, " (%s)", loader_error);
  }
  library->state = -1;
  pthread_mutex_unlock(&library->mutex);
  return 0;
}

int chezpp_optional_library_symbol(chezpp_optional_library *library,
                                   const char *name, void **target) {
  const char *loader_error;

  if (library == NULL || name == NULL || target == NULL) return 0;
  pthread_mutex_lock(&library->mutex);
  if (library->state != 1 || library->handle == NULL) {
    pthread_mutex_unlock(&library->mutex);
    return 0;
  }
  dlerror();
  *target = dlsym(library->handle, name);
  loader_error = dlerror();
  if (loader_error == NULL) {
    pthread_mutex_unlock(&library->mutex);
    return 1;
  }
  close_handle(library);
  snprintf(library->error, sizeof(library->error),
           "%s: missing symbol %s: %s", library->name, name, loader_error);
  library->state = -1;
  *target = NULL;
  pthread_mutex_unlock(&library->mutex);
  return 0;
}

void chezpp_optional_library_fail(chezpp_optional_library *library,
                                  const char *format, ...) {
  va_list arguments;

  if (library == NULL || format == NULL) return;
  pthread_mutex_lock(&library->mutex);
  va_start(arguments, format);
  fail_locked(library, format, arguments);
  va_end(arguments);
  pthread_mutex_unlock(&library->mutex);
}

void chezpp_optional_library_set_version(chezpp_optional_library *library,
                                         const char *version) {
  if (library == NULL) return;
  pthread_mutex_lock(&library->mutex);
  snprintf(library->version, sizeof(library->version), "%s",
           version == NULL ? "" : version);
  pthread_mutex_unlock(&library->mutex);
}

const char *chezpp_optional_library_error(chezpp_optional_library *library) {
  if (library == NULL) return "optional library descriptor is null";
  return library->error;
}

void chezpp_optional_library_reset_for_test(chezpp_optional_library *library) {
  if (library == NULL) return;
  pthread_mutex_lock(&library->mutex);
  close_handle(library);
  library->state = 0;
  library->loaded_name[0] = '\0';
  library->version[0] = '\0';
  library->error[0] = '\0';
  pthread_mutex_unlock(&library->mutex);
}
