#ifndef LIBFCS_HEADER
#define LIBFCS_HEADER
#ifndef LIBFCS_EXPORT
#ifdef LIBFCS_EXPORTS
#ifdef _WIN32
#define LIBFCS_EXPORT __declspec(dllexport)
#else
#define LIBFCS_EXPORT __attribute__ ((visibility("default")))
#endif
#else
#ifdef _WIN32
#define LIBFCS_EXPORT __declspec(dllimport)
#else
#define LIBFCS_EXPORT
#endif
#endif
#endif
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

#if defined(__cplusplus)
extern "C" {
#endif

LIBFCS_EXPORT bool libfcs_init();
LIBFCS_EXPORT FCSFile* load_FCS(const char* filename);
LIBFCS_EXPORT void libfcs_exit();

#if defined(__cplusplus)
}
#endif

#endif