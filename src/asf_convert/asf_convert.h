#ifndef ASF_CONVERT_H
#define ASF_CONVERT_H

typedef struct
{
  char *in_name;          // input file name
  char *out_name;         // output file name
  int import;             // import flag
  int image_stats;        // image stats flag
  int geocoding;          // geocoding flag
  int export;             // export flag
  int intermediates;      // flag to keep intermediates
  char *defaults;         // default values file
  char *batchFile;        // batch file name
  char *prefix;           // prefix for output file naming scheme
  char *suffix;           // suffix for output file naming scheme
} s_general;

typedef struct
{
  char *format;           // input format: CEOS, STF, ASF
  char *radiometry;       // data type: AMPLITUDE_IMAGE, 
                          // POWER_IMAGE, 
                          // SIGMA_IMAGE, 
                          // GAMMA_IMAGE,
                          // BETA_IMAGE
  char *lut;              // look up table file name (CIS only)
  double lat_begin;       // latitude constraint begin
  double lat_end;         // latitude constraint end
  char *prc;              // precision state vector location (to be implemented)
} s_import;

typedef struct
{
  char *values;           // value axis: LOOK, INCIDENCE, RANGE
  int bins;               // number of bins
  double interval;        // interval between bins
} s_image_stats;

typedef struct
{
  char *projection;       // projection parameters file
  double pixel;           // pixel size for geocoding
  double height;          // average height of the data
  char *datum;            // datum: WGS84, NAD27, NAD83
  char *resampling;       // resampling method: NEAREST_NEIGHBOR, BILINEAR, BICUBIC
  int force;              // force flag
} s_geocoding;

typedef struct
{
  char *format;           // output format: ASF, GEOTIFF, JPEG, PPM
  char *byte;             // conversion to byte: SIGMA, MINMAX, TRUNCATE, 
                          // HISTOGRAM_EQUALIZE
} s_export;

typedef struct
{
  char comment[255];          // first line for comments
  s_general *general;         // general processing details
  s_import *import;           // importing parameters
  s_image_stats *image_stats; // image stats parameters
  s_geocoding *geocoding;     // geocoding parameters
  s_export *export;           // exporting parameters
} convert_config;

/* checking return values in the main program */
void check_return(int ret, char *msg);

/* configuration functions */
int strindex(char s[], char t[]);
char *read_param(char *line);
char *read_str(char *line, char *param);
int read_int(char *line, char *param);
double read_double(char *line, char *param);
int init_config(char *configFile);
convert_config *init_fill_config(char *configFile);
convert_config *read_config(char *configFile);
int write_config(char *configFile, convert_config *cfg);

#endif
