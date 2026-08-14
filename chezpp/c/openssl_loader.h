#ifndef CHEZPP_OPENSSL_LOADER_H
#define CHEZPP_OPENSSL_LOADER_H

#include "optional_library.h"

#include <openssl/asn1.h>
#include <openssl/bio.h>
#include <openssl/core_names.h>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/err.h>
#include <openssl/kdf.h>
#include <openssl/params.h>
#include <openssl/pem.h>
#include <openssl/rand.h>
#include <openssl/rsa.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/x509v3.h>

#define CHEZPP_OPENSSL_SYMBOLS(X)                                             \
  X(crypto, ASN1_STRING_get0_data)                                            \
  X(crypto, ASN1_STRING_length)                                               \
  X(crypto, ASN1_TIME_to_tm)                                                  \
  X(crypto, BIO_ctrl)                                                         \
  X(crypto, BIO_free)                                                         \
  X(crypto, BIO_new)                                                          \
  X(crypto, BIO_new_mem_buf)                                                  \
  X(crypto, BIO_s_mem)                                                        \
  X(crypto, CRYPTO_memcmp)                                                    \
  X(crypto, ERR_error_string_n)                                               \
  X(crypto, ERR_get_error)                                                    \
  X(crypto, EVP_CIPHER_CTX_ctrl)                                              \
  X(crypto, EVP_CIPHER_CTX_free)                                              \
  X(crypto, EVP_CIPHER_CTX_get_block_size)                                    \
  X(crypto, EVP_CIPHER_CTX_new)                                               \
  X(crypto, EVP_CIPHER_CTX_reset)                                             \
  X(crypto, EVP_CIPHER_CTX_set_padding)                                       \
  X(crypto, EVP_CIPHER_fetch)                                                 \
  X(crypto, EVP_CIPHER_free)                                                  \
  X(crypto, EVP_CIPHER_get_block_size)                                        \
  X(crypto, EVP_CIPHER_get_iv_length)                                         \
  X(crypto, EVP_CIPHER_get_key_length)                                        \
  X(crypto, EVP_CipherFinal_ex)                                               \
  X(crypto, EVP_CipherInit_ex)                                                \
  X(crypto, EVP_CipherUpdate)                                                 \
  X(crypto, EVP_DecryptFinal_ex)                                              \
  X(crypto, EVP_DecryptInit_ex)                                               \
  X(crypto, EVP_DigestFinal_ex)                                               \
  X(crypto, EVP_DigestInit_ex)                                                \
  X(crypto, EVP_DigestSign)                                                   \
  X(crypto, EVP_DigestSignFinal)                                              \
  X(crypto, EVP_DigestSignInit)                                               \
  X(crypto, EVP_DigestSignUpdate)                                             \
  X(crypto, EVP_DigestUpdate)                                                 \
  X(crypto, EVP_DigestVerify)                                                 \
  X(crypto, EVP_DigestVerifyFinal)                                            \
  X(crypto, EVP_DigestVerifyInit)                                             \
  X(crypto, EVP_DigestVerifyUpdate)                                           \
  X(crypto, EVP_EncryptFinal_ex)                                              \
  X(crypto, EVP_EncryptInit_ex)                                               \
  X(crypto, EVP_KDF_CTX_free)                                                 \
  X(crypto, EVP_KDF_CTX_new)                                                  \
  X(crypto, EVP_KDF_derive)                                                   \
  X(crypto, EVP_KDF_fetch)                                                    \
  X(crypto, EVP_KDF_free)                                                     \
  X(crypto, EVP_MAC_CTX_dup)                                                  \
  X(crypto, EVP_MAC_CTX_free)                                                 \
  X(crypto, EVP_MAC_CTX_get_mac_size)                                         \
  X(crypto, EVP_MAC_CTX_new)                                                  \
  X(crypto, EVP_MAC_fetch)                                                    \
  X(crypto, EVP_MAC_final)                                                    \
  X(crypto, EVP_MAC_free)                                                     \
  X(crypto, EVP_MAC_init)                                                     \
  X(crypto, EVP_MAC_update)                                                   \
  X(crypto, EVP_MD_CTX_copy_ex)                                               \
  X(crypto, EVP_MD_CTX_free)                                                  \
  X(crypto, EVP_MD_CTX_new)                                                   \
  X(crypto, EVP_MD_CTX_reset)                                                 \
  X(crypto, EVP_MD_fetch)                                                     \
  X(crypto, EVP_MD_free)                                                      \
  X(crypto, EVP_MD_get_block_size)                                            \
  X(crypto, EVP_MD_get_size)                                                  \
  X(crypto, EVP_PBE_scrypt)                                                   \
  X(crypto, EVP_PKEY_CTX_free)                                                \
  X(crypto, EVP_PKEY_CTX_new)                                                 \
  X(crypto, EVP_PKEY_CTX_new_from_name)                                       \
  X(crypto, EVP_PKEY_CTX_set_params)                                          \
  X(crypto, EVP_PKEY_CTX_set_rsa_padding)                                     \
  X(crypto, EVP_PKEY_CTX_set_rsa_pss_saltlen)                                 \
  X(crypto, EVP_PKEY_derive)                                                  \
  X(crypto, EVP_PKEY_derive_init)                                             \
  X(crypto, EVP_PKEY_derive_set_peer)                                         \
  X(crypto, EVP_PKEY_free)                                                    \
  X(crypto, EVP_PKEY_generate)                                                \
  X(crypto, EVP_PKEY_get_base_id)                                             \
  X(crypto, EVP_PKEY_get_bits)                                                \
  X(crypto, EVP_PKEY_keygen_init)                                             \
  X(crypto, EVP_blake2b512)                                                   \
  X(crypto, EVP_blake2s256)                                                   \
  X(crypto, EVP_md5)                                                          \
  X(crypto, EVP_sha224)                                                       \
  X(crypto, EVP_sha256)                                                       \
  X(crypto, EVP_sha384)                                                       \
  X(crypto, EVP_sha3_224)                                                     \
  X(crypto, EVP_sha3_256)                                                     \
  X(crypto, EVP_sha3_384)                                                     \
  X(crypto, EVP_sha3_512)                                                     \
  X(crypto, EVP_sha512)                                                       \
  X(crypto, EVP_sha512_224)                                                   \
  X(crypto, EVP_sha512_256)                                                   \
  X(crypto, EVP_shake128)                                                     \
  X(crypto, EVP_shake256)                                                     \
  X(crypto, GENERAL_NAMES_free)                                               \
  X(crypto, OPENSSL_cleanse)                                                  \
  X(crypto, OPENSSL_init_crypto)                                              \
  X(crypto, OPENSSL_sk_free)                                                  \
  X(crypto, OPENSSL_sk_new_null)                                              \
  X(crypto, OPENSSL_sk_num)                                                   \
  X(crypto, OPENSSL_sk_pop_free)                                              \
  X(crypto, OPENSSL_sk_push)                                                  \
  X(crypto, OPENSSL_sk_value)                                                 \
  X(crypto, OSSL_PARAM_construct_end)                                         \
  X(crypto, OSSL_PARAM_construct_int)                                         \
  X(crypto, OSSL_PARAM_construct_octet_string)                                \
  X(crypto, OSSL_PARAM_construct_utf8_string)                                 \
  X(crypto, PEM_read_bio_PUBKEY)                                              \
  X(crypto, PEM_read_bio_PrivateKey)                                          \
  X(crypto, PEM_read_bio_X509)                                                \
  X(crypto, PEM_write_bio_PUBKEY)                                             \
  X(crypto, PEM_write_bio_PrivateKey)                                         \
  X(crypto, PKCS5_PBKDF2_HMAC)                                                \
  X(crypto, RAND_bytes)                                                       \
  X(crypto, RAND_status)                                                      \
  X(crypto, X509_NAME_print_ex)                                               \
  X(crypto, X509_STORE_CTX_free)                                              \
  X(crypto, X509_STORE_CTX_get0_param)                                        \
  X(crypto, X509_STORE_CTX_init)                                              \
  X(crypto, X509_STORE_CTX_new)                                               \
  X(crypto, X509_STORE_add_cert)                                              \
  X(crypto, X509_STORE_free)                                                  \
  X(crypto, X509_STORE_new)                                                   \
  X(crypto, X509_STORE_set_default_paths)                                     \
  X(crypto, X509_VERIFY_PARAM_set1_host)                                      \
  X(crypto, X509_VERIFY_PARAM_set1_ip_asc)                                    \
  X(crypto, X509_check_host)                                                  \
  X(crypto, X509_check_ip_asc)                                                \
  X(crypto, X509_digest)                                                      \
  X(crypto, X509_free)                                                        \
  X(crypto, X509_get0_notAfter)                                               \
  X(crypto, X509_get0_notBefore)                                              \
  X(crypto, X509_get0_serialNumber)                                           \
  X(crypto, X509_get_ext_d2i)                                                 \
  X(crypto, X509_get_issuer_name)                                             \
  X(crypto, X509_get_pubkey)                                                  \
  X(crypto, X509_get_subject_name)                                            \
  X(crypto, X509_up_ref)                                                      \
  X(crypto, X509_verify_cert)                                                 \
  X(crypto, X509_verify_cert_error_string)                                    \
  X(crypto, d2i_PUBKEY_bio)                                                   \
  X(crypto, d2i_PrivateKey_bio)                                               \
  X(crypto, d2i_X509_bio)                                                     \
  X(crypto, i2d_PUBKEY_bio)                                                   \
  X(crypto, i2d_PrivateKey_bio)                                               \
  X(crypto, i2d_X509)                                                         \
  X(ssl, OPENSSL_init_ssl)                                                    \
  X(ssl, SSL_CIPHER_get_name)                                                 \
  X(ssl, SSL_CTX_check_private_key)                                           \
  X(ssl, SSL_CTX_ctrl)                                                        \
  X(ssl, SSL_CTX_free)                                                        \
  X(ssl, SSL_CTX_get0_certificate)                                            \
  X(ssl, SSL_CTX_get0_privatekey)                                             \
  X(ssl, SSL_CTX_load_verify_locations)                                       \
  X(ssl, SSL_CTX_new)                                                         \
  X(ssl, SSL_CTX_set_alpn_protos)                                             \
  X(ssl, SSL_CTX_set_alpn_select_cb)                                          \
  X(ssl, SSL_CTX_set_default_verify_paths)                                    \
  X(ssl, SSL_CTX_set_verify)                                                  \
  X(ssl, SSL_CTX_use_PrivateKey)                                              \
  X(ssl, SSL_CTX_use_PrivateKey_file)                                         \
  X(ssl, SSL_CTX_use_certificate)                                             \
  X(ssl, SSL_CTX_use_certificate_chain_file)                                  \
  X(ssl, SSL_CTX_use_certificate_file)                                        \
  X(ssl, SSL_accept)                                                          \
  X(ssl, SSL_connect)                                                         \
  X(ssl, SSL_ctrl)                                                            \
  X(ssl, SSL_free)                                                            \
  X(ssl, SSL_get0_param)                                                      \
  X(ssl, SSL_get1_peer_certificate)                                           \
  X(ssl, SSL_get_current_cipher)                                              \
  X(ssl, SSL_get_error)                                                       \
  X(ssl, SSL_get_peer_cert_chain)                                             \
  X(ssl, SSL_get_verify_result)                                               \
  X(ssl, SSL_get_version)                                                     \
  X(ssl, SSL_new)                                                             \
  X(ssl, SSL_read_ex)                                                         \
  X(ssl, SSL_select_next_proto)                                               \
  X(ssl, SSL_set1_host)                                                       \
  X(ssl, SSL_set_fd)                                                          \
  X(ssl, SSL_shutdown)                                                        \
  X(ssl, SSL_write_ex)                                                        \
  X(ssl, TLS_client_method)                                                   \
  X(ssl, TLS_server_method)

#define CHEZPP_OPENSSL_DECLARE(kind, name)                                    \
  extern __typeof__(&name) chezpp_openssl_##name;
CHEZPP_OPENSSL_SYMBOLS(CHEZPP_OPENSSL_DECLARE)
#undef CHEZPP_OPENSSL_DECLARE

int chezpp_openssl_require(void);
int chezpp_openssl_crypto_symbol(const char *name, void **target);
int chezpp_openssl_ssl_symbol(const char *name, void **target);
const chezpp_optional_library *chezpp_openssl_library(void);

#endif
