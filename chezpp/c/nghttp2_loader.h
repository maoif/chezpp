#ifndef CHEZPP_NGHTTP2_LOADER_H
#define CHEZPP_NGHTTP2_LOADER_H

#include "optional_library.h"

int chezpp_nghttp2_require(void);
const chezpp_optional_library *chezpp_nghttp2_library(void);

#endif
