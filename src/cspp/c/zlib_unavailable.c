#include "net/unavailable.h"

uptr cspp_zlib_stream_open(int compress, int gzip) {
  (void)compress;
  (void)gzip;
  return 0;
}

ptr cspp_zlib_stream_process(uptr handle, ptr input, int start, int stop,
                               int finish, int maximum_output) {
  (void)handle;
  (void)input;
  (void)start;
  (void)stop;
  (void)finish;
  (void)maximum_output;
  return cspp_unavailable_status("zlib: disabled at build time");
}

void cspp_zlib_stream_close(uptr handle) {
  (void)handle;
}
