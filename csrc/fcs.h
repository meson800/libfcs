#ifndef LIBFCS_HEADER
#define LIBFCS_HEADER
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

struct StringUTF8 {
    uint8_t* buffer,
    size_t length
};

struct FCSFile {

};

bool libfcs_init();
void libfcs_exit();

#endif