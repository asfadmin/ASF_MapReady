#include "asf_raster.h"

#ifndef linux
#ifndef win32
static double
round (double arg)
{
  return floor (arg + 0.5);
}
#endif // #ifndef win32
#endif // #ifndef linux

/* Evaluate to true if floats are within tolerance of each other.  */
#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)

/* Default tolerance used for many floating point comparisons in this
   program.  */
#define ASF_EXPORT_FLOAT_MICRON 0.000000001

/* Compare floats using the default tolerance for this program.  */
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

/* Maximum image name length we can accept from the user.  Since the
   user may enter a base name only, the actual file name strings need
   to be slightly longer, to accomodate extensions.  */
#define MAX_IMAGE_NAME_LENGTH 240
/* Maximum extension length that will ever be automaticly added to a
   file name specified by the user.  */
#define MAX_EXTENSION_LENGTH 10
/* Maximum length allowed for arguments to format (-f or --format)
   options.  */
#define MAX_FORMAT_STRING_LENGTH 50
/* This value signifies that there is no maximum size is some contexts.  */
#define NO_MAXIMUM_OUTPUT_SIZE -1
/* Maximum length of a calibration comment */
#define MAX_COMMENT_LENGTH 255

#define FLAG_NOT_SET -1



/* Structure to hold elements of the command line.  */
typedef struct {
  /* Output format to use.  */
  char format[MAX_FORMAT_STRING_LENGTH];
  /* Maximum size of largest output dimension, in pixels, or
     NO_MAXIMUM_OUTPUT_SIZE.  The output image will be scaled to honor
     this value, if its maximum dimension is not already less than
     this.  */
  long size;
  /* Name of data and metadata input files  */
  char in_data_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char in_meta_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  /* Output name to use.  */
  char output_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  int verbose;                  /* Flag true if in verbose mode.  */
  int quiet;                    /* True if quiet mode is active.  */
  char leader_name[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char cal_params_file[MAX_IMAGE_NAME_LENGTH + MAX_EXTENSION_LENGTH + 1];
  char cal_comment[MAX_COMMENT_LENGTH];
  scale_t scale;
} command_line_parameters_t;

/* Output format to use.  */
typedef enum {
  ENVI,                         /* ENVI software package.  */
  ESRI,                         /* ESRI GIS package.  */
  GEOTIFF,                      /* Geotiff.  */
  TIF,                          /* Tiff. */
  JPEG,                         /* Joint Photographic Experts Group.  */
  PPM,                          /* Portable PixMap.  */
  CEOS                          /* CEOS format */
} output_format_t;

/* Ellipsoid used for the data.  */
typedef enum {
  CLARKE1866,
  GEM10C,                       /* Ellipsoid in GEM6 earth model.  */
  WGS84,                        /* Ellipsoid in EGM96 earth model.  */
  WGS66,                        /* Ancient crummy ellipsoid.  */
  USER_DEFINED                  /* Some unknown user defined ellipsoid.  */
} asf_export_ellipsoid_t;

/* Prototypes */
void usage();
void help_page();
int checkForOption (char *key, int argc, char *argv[]);
void check_return (int ret, char *msg);
void *get_image_data (meta_parameters *metadata, const char *image_data_file);
unsigned char averaging_kernel (gsl_matrix_uchar *img, int kernel_size, size_t i, size_t j);
unsigned char *average_unsigned_char_pixels (unsigned char *pixels, unsigned long *width, \
                              unsigned long *height, int kernel_size);
unsigned char *scale_floats_to_unsigned_bytes (float *daf, size_t pixel_count);
unsigned char *scale_unsigned_char_image_dimensions (unsigned char *pixels, \
                                      unsigned long max_large_dimension, \
                                      unsigned long *width, \
                                      unsigned long *height);
void export_as_envi (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name);

void export_as_esri (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name);

void export_as_geotiff (const char *metadata_file_name,
                        const char *image_data_file_name,
                        const char *output_file_name,
                        scale_t scale);

void export_as_jpeg (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name,
                     long max_size,
                     scale_t scale);

void export_as_ppm (const char *metadata_file_name,
                    const char *image_data_file_name,
                    const char *output_file_name,
                    long max_size);

void export_as_ceos (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name,
                     const char *leader_file_name,
                     const char *calibration_parameter_file,
                     const char *calibration_comment);

void export_as_tiff (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name,
                     long max_size,
                     scale_t scale);
