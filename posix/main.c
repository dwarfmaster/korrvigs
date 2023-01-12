#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <SWI-Prolog.h>

static foreign_t pl_pause() {
  pause();
  PL_succeed;
}

install_t install_posix() {
  PL_register_foreign("pause", 0, pl_pause, 0);
}

