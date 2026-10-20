#include "optional_library.h"
#include <stdarg.h>
#include <stdio.h>
void cspp_optional_library_fail(cspp_optional_library *library, const char *format, ...) {
  va_list arguments;
  pthread_mutex_lock(&library->mutex);
  va_start(arguments, format);
  vsnprintf(library->error, sizeof(library->error), format, arguments);
  va_end(arguments);
  library->state = -1;
  pthread_mutex_unlock(&library->mutex);
}
void cspp_optional_library_set_version(cspp_optional_library *library, const char *version) {
  pthread_mutex_lock(&library->mutex);
  snprintf(library->version, sizeof(library->version), "%s", version == NULL ? "" : version);
  pthread_mutex_unlock(&library->mutex);
}
const char *cspp_optional_library_error(cspp_optional_library *library) {
  return library->error;
}
