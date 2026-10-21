#ifndef CSPP_ZLIB_LOADER_H
#define CSPP_ZLIB_LOADER_H

#include "optional_library.h"
#include "common.h"

int cspp_zlib_require(void);
const cspp_optional_library *cspp_zlib_library(void);
uptr cspp_zlib_stream_open(int compress, int gzip);
ptr cspp_zlib_stream_process(uptr handle, ptr input, int start, int stop,
                               int finish, int maximum_output);
void cspp_zlib_stream_close(uptr handle);

#endif
