#ifndef EXPORT_CEOS_H
#define EXPORT_CEOS_H

typedef struct
{
    char *in_data_name;     /* input data file name */
    char *in_meta_name;     /* input metadata file name */
    char *in_format;        /* input format: CEOS, ASF */
    char *data_type;        /* data type: amplitude, power, sigma, gamma,
                                beta */
    char *out_name;         /* output file name */
    char *out_format;       /* output format: ASF, CEOS, GEOTIFF, ENVI, ESRI, JPEG,
                                PPM, PNG, LAS */
    int resample;           /* resampling flag */
    int browse;             /* browse image flag */
    char *logFile;          /* log file name */
    int geocoding;          /* geocoding flag */
    int batch;              /* batch mode flag */
    char *batchFile;        /* batch file name */
    char *units;            /* units as defined in the default values file */
} s_general;

typedef struct
{
    char *projection;       /* projection: POLARSTEREO, UTM, ALBERS,
                                LAMBERT1, LAMBERT2 */
    int background;         /* background fill color */
    double pixel;           /* pixel size for geocoding */
    double height;          /* average height of the data */
} s_geocoding;

typedef struct
{
    int kernel;             /* kernel size for reducing the output image size */
} s_resampling;

typedef struct
{
    int datum;              /* datum */
    double center_lon;      /* center longitude */
    double center_lat;      /* standard parallel */
    char *units;            /* units */
} s_azimuthal;

typedef struct
{
    int datum;              /* datum */
    int zone;               /* UTM zone number */
    char *units;            /* units */
} s_utm;

typedef struct
{
    int datum;                  /* datum */
    double first_parallel;      /* first standard parallel */
    double second_parallel;     /* second standard parallel */
    double center_meridian;     /* center meridian */
    double orig_latitude;       /* latitude of projection origin */
    char *units;                /* units */
} s_conic;

typedef struct
{
    char comment[255];          /* first line for comments */
    s_general *general;         /* general processing details */
    s_geocoding *geocoding;     /* geocoding parameters */
    s_resampling *resampling;   /* resampling parameters */
    s_azimuthal *polar;         /* parameters for Polarstereo coordinates */
    s_utm *utm;                 /* parameters for UTM coordinates */
    s_conic *albers;            /* parameters for Albers coordinates */
    s_azimuthal *lambert_az;    /* parameters for Lambert Azimuthal Equal Area
                                    coordinates */
    s_conic *lambert_cc;        /* parameters for Lambert Conformal Conic
                                    coordinates */
} s_config;

typedef enum
{
    CEOS=1,
    ASF,
    GEOTIFF,
    JPEG,
    ENVI,
    ESRI,
    PPM,
    PNG,
    LAS
} file_format_t;

/* checking return values in the main program */
void check_return(int ret, char *msg);

/* configuration functions */
int strindex(char s[], char t[]);
char *read_param(char *line);
char *read_str(char *line, char *block, char *param);
int read_int(char *line, char *block, char *param);
double read_double(char *line, char *block, char *param);
int init_config(char *configFile);
int check_geocode_flag(char *configFile);
int check_resample_flag(char *configFile);
int init_projection_config(char *configFile);
int init_resample_config(char *configFile);
s_config *init_cfg(void);
s_config *init_fill_config(char *configFile);
s_config *read_config(char *configFile);
int write_config(char *configFile, s_config *cfg);

#endif
