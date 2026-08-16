#ifndef CHEZPP_IDN2_LOADER_H
#define CHEZPP_IDN2_LOADER_H

#include "optional_library.h"

int chezpp_idn2_require(void);
const chezpp_optional_library *chezpp_idn2_library(void);
void *chezpp_idn2_symbol(const char *name);

#endif
