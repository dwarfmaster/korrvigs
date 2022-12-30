#include <stdio.h>
#include <stdlib.h>
#include <SWI-Prolog.h>

static foreign_t pl_get_hello(term_t to) {
  if(PL_unify_atom_chars(to, "hello")) {
    PL_succeed;
  }
  PL_fail;
}

install_t install_norg_parser() {
  PL_register_foreign("get_hello", 1, pl_get_hello, 0);
}

