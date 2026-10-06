#ifndef CHEZPP_NET_UNAVAILABLE_H
#define CHEZPP_NET_UNAVAILABLE_H

#include "../common.h"

/* Scheme operation results share the #(error message) ABI. */
static inline ptr chezpp_unavailable_status(const char *message) {
  ptr result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol("error"));
  Svector_set(result, 1, Sstring(message));
  return result;
}

#endif
