#include "../chezpp/c/optional_library.h"
#include <stdio.h>

int main(int argc, char **argv) {
  const char *names[2];
  void *value = NULL;

  if (argc != 2) return 64;
  names[0] = argv[1];
  names[1] = NULL;
  chezpp_optional_library library =
      CHEZPP_OPTIONAL_LIBRARY_INIT("fixture", names);
  if (!chezpp_optional_library_open(&library)) {
    puts(chezpp_optional_library_error(&library));
    return 2;
  }
  if (!chezpp_optional_library_symbol(&library, "fixture_value", &value)) {
    puts(chezpp_optional_library_error(&library));
    return 3;
  }
  return value == NULL;
}
