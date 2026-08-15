#ifndef CHEZPP_ZLIB_LOADER_H
#define CHEZPP_ZLIB_LOADER_H

#include "optional_library.h"
#include "common.h"

int chezpp_zlib_require(void);
const chezpp_optional_library *chezpp_zlib_library(void);
uptr chezpp_zlib_stream_open(int compress, int gzip);
ptr chezpp_zlib_stream_process(uptr handle, ptr input, int start, int stop,
                               int finish, int maximum_output);
void chezpp_zlib_stream_close(uptr handle);

#endif
