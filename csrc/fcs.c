#include "Rts.h"
#include "HsFFI.h"

#include "fcs.h"
#include "FFI_stub.h"

bool libfcs_init(void){
  int argc = 2;
  char *argv[] = {"+RTS", "-A32m", NULL};
  char **pargv = argv;
  // Initialize Haskell runtime
  hs_init(&argc, &pargv);

  // do any other initialization here and
  // return false if there was a problem
  return true;
}

FCSFile* load_FCS(const char* filename) {
  return loadFCS((HsPtr)filename);
}
void libfcs_exit(void){
  hs_exit();
}