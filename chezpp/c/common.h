// #define _GNU_SOURCE

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>
#include <fcntl.h>
#include <poll.h>
#include <time.h>
#include <grp.h>
#include <pwd.h>


#include "scheme.h"

ptr errno_str();
ptr errno_str_vector();

char *expand_pathname(const char *inpath);
