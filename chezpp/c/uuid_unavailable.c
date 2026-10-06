#include "net/unavailable.h"

ptr chezpp_generate_uuid(void) {
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_generate_uuid_time(void) {
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_generate_uuid_md5(ptr uuid_ns_bv, const char *name) {
  (void)uuid_ns_bv;
  (void)name;
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_generate_uuid_sha1(ptr uuid_ns_bv, const char *name) {
  (void)uuid_ns_bv;
  (void)name;
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_uuid_to_string(ptr uuid_bv) {
  (void)uuid_bv;
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_uuid_to_string_upcase(ptr uuid_bv) {
  (void)uuid_bv;
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_uuid_to_string_downcase(ptr uuid_bv) {
  (void)uuid_bv;
  return chezpp_unavailable_status("uuid: disabled at build time");
}

int chezpp_uuid_compare(ptr uuid_bv1, ptr uuid_bv2) {
  (void)uuid_bv1;
  (void)uuid_bv2;
  return -1;
}

ptr chezpp_uuid_time(ptr uuid_bv) {
  (void)uuid_bv;
  return chezpp_unavailable_status("uuid: disabled at build time");
}

ptr chezpp_string_to_uuid(const char *str) {
  (void)str;
  return chezpp_unavailable_status("uuid: disabled at build time");
}
