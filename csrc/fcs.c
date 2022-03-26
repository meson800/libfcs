#include "fcs.h"

#include "Rts.h"
#include "HsFFI.h"

HsBool libfcs_init(void){
  int argc = 2;
  char *argv[] = {"+RTS", "-A32m", NULL};
  char **pargv = argv;
  // Initialize Haskell runtime
  hs_init(&argc, &pargv);

  // do any other initialization here and
  // return false if there was a problem
  return true;
}

void libfcs_exit(void){
  hs_exit();
}