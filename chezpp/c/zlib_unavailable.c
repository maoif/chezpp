#include "net/unavailable.h"

uptr chezpp_zlib_stream_open(int compress, int gzip) {
  (void)compress;
  (void)gzip;
  return 0;
}

ptr chezpp_zlib_stream_process(uptr handle, ptr input, int start, int stop,
                               int finish, int maximum_output) {
  (void)handle;
  (void)input;
  (void)start;
  (void)stop;
  (void)finish;
  (void)maximum_output;
  return chezpp_unavailable_status("zlib: disabled at build time");
}

void chezpp_zlib_stream_close(uptr handle) {
  (void)handle;
}
