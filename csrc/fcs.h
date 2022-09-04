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

/********* Helper Enums *********/
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

enum VizScale {
    viz_Linear,
    viz_Logarithmic
};

enum Originality {
    orig_Original,
    orig_NonDataModified,
    orig_Appended,
    orig_DataModified
};


/************ Helper structs ***********/
typedef struct StringUTF8 {
    size_t length;
    uint8_t* buffer;
} StringUTF8;

typedef struct DataBuffer {
    size_t n_rows;
    size_t n_cols;
    double* data;
} DataBuffer;

typedef struct OptionalString {
    StringUTF8 string;
    bool present;
} OptionalString;

typedef struct OptionalFloat {
    float value;
    bool present;
} OptionalFloat;

typedef struct OptionalInt64 {
    int64_t value;
    bool present;
} OptionalInt64;

typedef struct AmplificationType {
    float log_decades;
    float offset;
} AmplificationType;

typedef struct OptionalVizScale {
    enum VizScale viz_scale;
    float f1;
    float f2;
    bool present;
} OptionalVizScale;

typedef struct OptionalInt64Array {
    int64_t* vals;
    bool present;
} OptionalInt64Array;

typedef struct OptionalParamCalibration {
    float unit_conversion_factor;
    StringUTF8 unit_name;
    bool present;
} OptionalParamCalibration;

typedef struct Parameter {
    int64_t bit_length;
    AmplificationType amplification;
    StringUTF8 short_name;
    int64_t range;
    OptionalVizScale viz_scale;
    OptionalString filter;
    OptionalFloat gain;
    OptionalInt64Array excitation_wavelengths;
    OptionalInt64 excitation_power;
    OptionalFloat percent_light_collected;
    OptionalString name;
    OptionalString detector_type;
    OptionalFloat detector_voltage;
    OptionalParamCalibration calibration;
} Parameter;

typedef struct OptionalSpillover {
    int64_t n_parameters;
    StringUTF8* parameters;
    DataBuffer matrix;
    bool present;
} OptionalSpillover;

typedef struct OptionalTrigger {
    StringUTF8 trigger_channel;
    int64_t trigger_value;
    bool present;
} OptionalTrigger;

typedef struct  MapItem {
    StringUTF8 key;
    StringUTF8 value;
} MapItem;

typedef struct IntMapItem {
    int64_t key;
    int64_t value;
} IntMapItem;

typedef struct MapItems {
    uint64_t n_vals;
    MapItem* items;
} MapItems;

typedef struct IntMapItems {
    uint64_t n_vals;
    IntMapItem* items;
} IntMapItems;

typedef struct OptionalCellSubset {
    int64_t n_simultaneous_subsets;
    OptionalInt64 n_subsets;
    OptionalInt64 subset_nbits;
    IntMapItems flags; 
    bool present;
} OptionalCellSubset;

typedef struct FCSMetadata {
    enum FCSMode mode;
    enum Datatype datatype;
    enum ByteOrder byte_order;
    uint64_t n_parameters;
    Parameter* parameters;
    MapItems extra_keyvals;
    OptionalInt64 n_events_aborted;
    OptionalString acquire_time;
    OptionalString acquire_end_time;
    OptionalString acquire_date;
    OptionalCellSubset cell_subset;
    OptionalString cells;
    OptionalString comment;
    OptionalString cytometer_type;
    OptionalString cytometer_serial_number;
    OptionalString institution;
    OptionalString experimenter;
    OptionalString operator;
    OptionalString filename;
// TODO: GATE info, PKn PKNn, RnI, RnW
    OptionalString last_modified;
    OptionalString last_modifier;
    OptionalInt64 n_events_lost;
    OptionalInt64 originality;
    OptionalString plate_id;
    OptionalString plate_name;
    OptionalString project;
    OptionalString specimen;
    OptionalSpillover spillover;
    OptionalString specimen_source;
    OptionalString computer;
    OptionalFloat timestep;
    OptionalTrigger trigger;
    OptionalString well_id;
} FCSMetadata;

typedef struct FCSFile {
    FCSMetadata metadata;
    DataBuffer uncompensated;
    DataBuffer compensated;
} FCSFile;

#if defined(__cplusplus)
extern "C" {
#endif

LIBFCS_EXPORT bool libfcs_init();
LIBFCS_EXPORT FCSFile* load_FCS(const char* filename);
LIBFCS_EXPORT void free_FCS(FCSFile* fcs);
LIBFCS_EXPORT void libfcs_exit();

#if defined(__cplusplus)
}
#endif

#endif