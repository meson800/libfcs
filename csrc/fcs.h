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

enum FCSMode {
    mode_List,
    mode_MultivariateHistogram,
    mode_UnivariateHistograms
};

enum Datatype {
    type_StoredInteger,
    type_StoredFloat,
    type_StoredDouble,
    type_StoredASCII
};

enum ByteOrder {
    LittleEndian,
    BigEndian
};

typedef struct StringUTF8 {
    size_t length;
    uint8_t* buffer;
} StringUTF8;

typedef struct DataBuffer {
    size_t n_events;
    size_t n_parameters;
    double* data;
} DataBuffer;

typedef struct OptionalString {
    bool valid;
    StringUTF8 string;
} OptionalString;

typedef struct Parameter {
    int64_t bit_length;
    StringUTF8 short_name;
} Parameter;

typedef struct FCSMetadata {
    enum FCSMode mode;
    enum Datatype datatype;
    enum ByteOrder byte_order;
    uint64_t n_parameters;
    Parameter* parameters;
//    Parameters parameters;
//    uint64_t n_events;
} FCSMetadata;

typedef struct FCSFile {
    StringUTF8 name;
    FCSMetadata metadata;
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