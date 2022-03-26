#ifndef LIBFCS_HEADER
#define LIBFCS_HEADER
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct StringUTF8 {
    uint8_t* buffer;
    size_t length;
} StringUTF8;

typedef struct FCSFile {
    StringUTF8 name;
} FCSFile;

bool libfcs_init();
void libfcs_exit();

#endif