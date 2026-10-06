#ifndef CHEZPP_OPENSSL_LOADER_H
#define CHEZPP_OPENSSL_LOADER_H

#include "optional_library.h"

#if CHEZPP_WITH_OPENSSL
#include <openssl/asn1.h>
#include <openssl/bio.h>
#include <openssl/core_names.h>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/err.h>
#include <openssl/kdf.h>
#include <openssl/params.h>
#include <openssl/pem.h>
#include <openssl/ocsp.h>
#include <openssl/rand.h>
#include <openssl/rsa.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/x509v3.h>

#endif
int chezpp_openssl_require(void);
const chezpp_optional_library *chezpp_openssl_library(void);
#endif
