#ifndef CSPP_CARES_LOADER_H
#define CSPP_CARES_LOADER_H

#include "optional_library.h"

int cspp_cares_require(void);
const cspp_optional_library *cspp_cares_library(void);

#endif
