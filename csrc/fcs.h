#ifndef LIBFCS_HEADER
#define LIBFCS_HEADER
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct StringUTF8 {
    size_t length;
    uint8_t* buffer;
} StringUTF8;

typedef struct DataBuffer {
    size_t n_events;
    size_t n_parameters;
    double* data;
} DataBuffer;

typedef struct FCSFile {
    StringUTF8 name;
    DataBuffer uncompensated;
    DataBuffer compensated;
} FCSFile;

bool libfcs_init();
FCSFile* load_FCS(const char* filename);
void libfcs_exit();

#endif