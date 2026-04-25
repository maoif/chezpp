#include "common.h"

#include <string.h>

#include <uuid/uuid.h>

ptr chezpp_generate_uuid() {
  uuid_t uuid;
  uuid_generate(uuid);
  ptr bv = Smake_bytevector(sizeof(uuid_t), 0);
  ptr uuid_data = Sbytevector_data(bv);
  memcpy(uuid_data, uuid, sizeof(uuid_t));
  return bv;
}

// Return a vector whose first element is a boolean that indicates whether
// the timed-based UUID has been generated in a safe manner,
// and whose second element is the UUID bytevector.
ptr chezpp_generate_uuid_time() {
  uuid_t uuid;
  int res = uuid_generate_time_safe(uuid);
  ptr bv = Smake_bytevector(sizeof(uuid_t), 0);
  ptr uuid_data = Sbytevector_data(bv);
  memcpy(uuid_data, uuid, sizeof(uuid_t));

  ptr res_vec = Smake_vector(2, Sfalse);
  Svector_set(res_vec, 0, res == 0 ? Strue : Sfalse);
  Svector_set(res_vec, 1, bv);

  return res_vec;
}

ptr chezpp_generate_uuid_md5(ptr uuid_ns_bv, const char *name) {
  uuid_t uuid;
  uuid_t uuid_ns;
  memcpy(uuid_ns, Sbytevector_data(uuid_ns_bv), sizeof(uuid_t));
  uuid_generate_md5(uuid, uuid_ns, name, strlen(name));

  ptr bv = Smake_bytevector(sizeof(uuid_t), 0);
  ptr uuid_data = Sbytevector_data(bv);
  memcpy(uuid_data, uuid, sizeof(uuid_t));
  return bv;
}

ptr chezpp_generate_uuid_sha1(ptr uuid_ns_bv, const char *name) {
  uuid_t uuid;
  uuid_t uuid_ns;
  memcpy(uuid_ns, Sbytevector_data(uuid_ns_bv), sizeof(uuid_t));
  uuid_generate_sha1(uuid, uuid_ns, name, strlen(name));

  ptr bv = Smake_bytevector(sizeof(uuid_t), 0);
  ptr uuid_data = Sbytevector_data(bv);
  memcpy(uuid_data, uuid, sizeof(uuid_t));
  return bv;
}

ptr chezpp_uuid_to_string(ptr uuid_bv) {
  uuid_t uuid;
  char out[37];
  memset(out, 0, sizeof(out));
  memcpy(uuid, Sbytevector_data(uuid_bv), sizeof(uuid_t));
  uuid_unparse(uuid, out);
  return Sstring(out);
}

ptr chezpp_uuid_to_string_upcase(ptr uuid_bv) {
  uuid_t uuid;
  char out[37];
  memset(out, 0, sizeof(out));
  memcpy(uuid, Sbytevector_data(uuid_bv), sizeof(uuid_t));
  uuid_unparse_upper(uuid, out);
  return Sstring(out);
}

ptr chezpp_uuid_to_string_downcase(ptr uuid_bv) {
  uuid_t uuid;
  char out[37];
  memset(out, 0, sizeof(out));
  memcpy(uuid, Sbytevector_data(uuid_bv), sizeof(uuid_t));
  uuid_unparse_lower(uuid, out);
  return Sstring(out);
}

// Return the UUID bytevector on success,
// or #f on failure.
ptr chezpp_string_to_uuid(const char *str) {
  uuid_t uuid;
  if (uuid_parse(str, uuid) == 0) {
    ptr bv = Smake_bytevector(sizeof(uuid_t), 0);
    ptr uuid_data = Sbytevector_data(bv);
    memcpy(uuid_data, uuid, sizeof(uuid_t));
    return bv;
  }
  return Sfalse;
}

// Internal helper for uuid-time.  On 64-bit systems, return a 16-byte
// bytevector where the first 8 bytes store the seconds and the last 8 bytes
// store the microseconds.
ptr chezpp_uuid_time(ptr uuid_bv) {
  uuid_t uuid;
  struct timeval tv;
  memcpy(uuid, Sbytevector_data(uuid_bv), sizeof(uuid_t));
  (void)uuid_time(uuid, &tv);

  ptr bv = Smake_bytevector(sizeof(struct timeval), 0);
  ptr bvdata = Sbytevector_data(bv);
  memcpy(bvdata, &tv.tv_sec, sizeof(tv.tv_sec));
  memcpy(bvdata + sizeof(tv.tv_sec), &tv.tv_usec, sizeof(tv.tv_usec));

  return bv;
}

int chezpp_uuid_compare(ptr uuid_bv1, ptr uuid_bv2) {
  uuid_t uuid1;
  uuid_t uuid2;
  memcpy(uuid1, Sbytevector_data(uuid_bv1), sizeof(uuid_t));
  memcpy(uuid2, Sbytevector_data(uuid_bv2), sizeof(uuid_t));
  return uuid_compare(uuid1, uuid2);
}
