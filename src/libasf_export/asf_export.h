#ifndef ASF_EXPORT_INCLUDED
#define ASF_EXPORT_INCLUDED

#include <unistd.h>
#include "asf_tiff.h"
#include <png.h>
#include "asf.h"
#include "asf_jpeg.h"
#include <stdlib.h>
#include "asf_raster.h"
#include "libasf_proj.h"
#include "asf_nan.h"
#include "netcdf.h"
#include "hdf5.h"

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
#define MAX_CHANNEL_STRING_LENGTH 15

#define FLAG_NOT_SET -1

#define NUM_HIST_BINS UCHAR_MAX + 1

/* Output format to use.  */
typedef enum {
  ENVI=1,                       /* ENVI software package.  */
  ESRI,                         /* ESRI GIS package.  */
  GEOTIFF,                      /* Geotiff.  */
  TIF,                          /* Tiff. */
  JPEG,                         /* Joint Photographic Experts Group.  */
  PGM,                          /* Portable GrayMap.  */
  PNG,                          /* Portable Network Graphic */
  PNG_ALPHA,                    // Transparent PNG (8-bit)
  PNG_GE,                       // Transparent PNG (32-bit), Google Earth style
  KML,                          // JPEG with GoogleEarth overlay file
  POLSARPRO_HDR,                // PolsarPro with ENVI header
  HDF,                          // HDF5 - NASA Earth Observation standard
  NC                            // netCDF - modeler oriented format
} output_format_t;

/* Ellipsoid used for the data.  */
typedef enum {
  CLARKE1866,
  GEM10C,                       /* Ellipsoid in GEM6 earth model.  */
  WGS84,                        /* Ellipsoid in EGM96 earth model.  */
  WGS66,                        /* Ancient crummy ellipsoid.  */
  USER_DEFINED                  /* Some unknown user defined ellipsoid.  */
} asf_export_ellipsoid_t;

// netCDF pointer structure
typedef struct {
  int ncid;                     // Pointer to the netCDF file
  int var_count;                // Number of variables
  int *var_id;                  // Variable IDs
} netcdf_t;

// HDF5 pointer structure
typedef struct {
  hid_t file;                   // File identifier
  hid_t space;                  // Data space identifier
  int var_count;                // Number of variables
  hid_t *var;                   // Variable identifiers
} h5_t;

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
  scale_t sample_mapping;
  char red_channel[MAX_CHANNEL_STRING_LENGTH];
  char blue_channel[MAX_CHANNEL_STRING_LENGTH];
  char green_channel[MAX_CHANNEL_STRING_LENGTH];
  char band[MAX_CHANNEL_STRING_LENGTH];
  char look_up_table_name[MAX_IMAGE_NAME_LENGTH];
} command_line_parameters_t;

/* Prototypes */
int asf_export(output_format_t format, scale_t sample_mapping,
	       char *in_base_name, char *output_name);
int asf_export_with_lut(output_format_t, scale_t sample_mapping,
			char *lutFile, char *inFile, char *outFile);
int asf_export_bands(output_format_t format, scale_t sample_mapping, int rgb,
                     int true_color, int false_color,
                     char *look_up_table_name, char *in_base_name,
                     char *output_name, char **band_name,
                     int *noutputs, char ***output_names);

void usage();
void help_page();
int checkForOption (char *key, int argc, char *argv[]);
void *get_image_data (meta_parameters *metadata, const char *image_data_file);
unsigned char averaging_kernel (gsl_matrix_uchar *img, int kernel_size, size_t i, size_t j);
unsigned char *average_unsigned_char_pixels (unsigned char *pixels, unsigned long *width, \
                              unsigned long *height, int kernel_size);
unsigned char *map_floats_to_unsigned_bytes (float *daf, size_t pixel_count);
unsigned char *scale_unsigned_char_image_dimensions (unsigned char *pixels, \
                                      unsigned long max_large_dimension, \
                                      unsigned long *width, \
                                      unsigned long *height);
void get_statistics (FloatImage *si, scale_t sample_mapping, \
                    int sampling_stride, float *mean, \
                    float *standard_deviation, float *min_sample, \
                    float *max_sample, float *omin, float *omax, \
                    gsl_histogram **hist, float no_data);
unsigned char pixel_float2byte (float paf, scale_t sample_mapping, float omin,\
                                float omax, gsl_histogram *hist, \
                                gsl_histogram_pdf *my_hist_pdf,
                                float no_data_value);
spheroid_type_t axis2spheroid (double semimajor,
                               double semiminor);

void export_as_envi (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name);

void export_as_esri (const char *metadata_file_name,
                     const char *image_data_file_name,
                     const char *output_file_name);

void export_band_image(const char *metadata_file_name,
                       const char *image_data_file_name,
                       char *output_file_name,
                       scale_t sample_mapping,
                       char **band_name, int rgb,
                       int true_color, int false_color,
                       char *look_up_table_name,
                       output_format_t format,
                       int *noutputs,
                       char ***output_names);

void initialize_jpeg_file(const char *output_file_name,
                          meta_parameters *meta, FILE **ojpeg,
                          struct jpeg_compress_struct *cinfo, int rgb);
void finalize_jpeg_file(FILE *ojpeg, struct jpeg_compress_struct *cinfo);

void initialize_png_file(const char *output_file_name,
                         meta_parameters *meta, FILE **opng,
                         png_structp *png_ptr, png_infop *info_ptr,
                         int rgb);
void initialize_png_file_ext(const char *output_file_name,
			     meta_parameters *meta, FILE **opng,
			     png_structp *png_ptr, png_infop *info_ptr,
			     int rgb, int alpha);
void finalize_png_file(FILE *opng, png_structp png_ptr, png_infop info_ptr);
int should_write_insar_xml_meta(meta_parameters *md);
int should_write_insar_rgb(char *band_name);
int should_write_dem_xml_meta(meta_parameters *md);
void write_insar_rgb(output_format_t format, char *in_meta_name, char *in_data_name, char *out_name);
void write_insar_xml_to_file(char *output_file_name, char *xml_meta);
char* get_insar_xml_string(meta_parameters *meta, int gdal);
void write_dem_xml_to_file(char *output_file_name, char *xml_meta);
char* get_dem_xml_string(meta_parameters *meta, int gdal);

// Prototypes from export_geotiff.c
void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping,
                           int rgb, int *palette_color_tiff, char **band_names,
                           char *look_up_table_name, int is_colormapped);
GTIF* write_tags_for_geotiff (TIFF *otif, const char *metadata_file_name,
                              int rgb, char **band_names, int palette_color_tiff);
void finalize_tiff_file(TIFF *otif, GTIF *ogtif, int is_geotiff);
int lut_to_tiff_palette(unsigned short **colors, int size, char *look_up_table_name);
void dump_palette_tiff_color_map(unsigned short *colors, int map_size);
int meta_colormap_to_tiff_palette(unsigned short **colors, int *byte_image, meta_colormap *colormap);

// Prototypes from export_netcdf.c
netcdf_t *initialize_netcdf_file(const char *output_file, 
				 meta_parameters *meta);
void finalize_netcdf_file(netcdf_t *netcdf, meta_parameters *md);

// Prototypes from export_hdf.c
h5_t *initialize_h5_file(const char *output_file_name, meta_parameters *md);
void finalize_h5_file(h5_t *hdf);

// Prototypes from key.c
double spheroid_diff_from_axis (spheroid_type_t spheroid,
                                double n_semi_major, double n_semi_minor);
spheroid_type_t axis2spheroid (double re_major, double re_minor);
int UTM_2_PCS(short *pcs, datum_type_t datum, unsigned long zone, char hem);
void gcs_2_string (char *datum_str, short gcs);
void pcs_2_string (char *datum_str, short pcs);
void datum_2_string (char *datum_str, datum_type_t datum);
void write_datum_key (GTIF *ogtif, datum_type_t datum);
void write_spheroid_key (GTIF *ogtif, spheroid_type_t spheroid, double re_major,
			 double re_minor);

// Prototypes from write_line.c
void write_tiff_byte2byte(TIFF *otif, unsigned char *byte_line,
                          channel_stats_t stats, scale_t sample_mapping,
                          int sample_count, int line);
void write_tiff_float2float(TIFF *otif, float *float_line, int line);
void write_tiff_float2int(TIFF *otif, float *float_line, int line,
			  int sample_count);
void write_tiff_float2byte(TIFF *otif, float *float_line,
			   channel_stats_t stats, scale_t sample_mapping,
			   float no_data, int line, int sample_count);
void write_rgb_tiff_byte2byte(TIFF *otif,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
			      int line, int sample_count);
void write_tiff_byte2lut(TIFF *otif, unsigned char *byte_line,
			 int line, int sample_count, char *look_up_table_name);
void write_rgb_tiff_float2float(TIFF *otif,
				float *red_float_line,
				float *green_float_line,
				float *blue_float_line,
				int line, int sample_count);
void write_rgb_tiff_float2byte(TIFF *otif,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int line, int sample_count);
void write_tiff_float2lut(TIFF *otif, float *float_line,
			  channel_stats_t stats, scale_t sample_mapping,
			  float no_data, int line, int sample_count,
			  char *look_up_table_name);

void write_jpeg_byte2byte(FILE *ojpeg, unsigned char *byte_line,
                          channel_stats_t stats, scale_t sample_mapping,
                          struct jpeg_compress_struct *cinfo,
			  int sample_count);
void write_jpeg_float2byte(FILE *ojpeg, float *float_line,
			   struct jpeg_compress_struct *cinfo,
			   channel_stats_t stats,
			   scale_t sample_mapping,
			   float no_data, int sample_count);
void write_rgb_jpeg_byte2byte(FILE *ojpeg,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
			      struct jpeg_compress_struct *cinfo,
			      int sample_count);
void write_jpeg_byte2lut(FILE *ojpeg, unsigned char *byte_line,
			 struct jpeg_compress_struct *cinfo,
			 int sample_count, char *look_up_table_name);
void write_rgb_jpeg_float2byte(FILE *ojpeg,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
			       struct jpeg_compress_struct *cinfo,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int sample_count);
void write_jpeg_float2lut(FILE *ojpeg, float *float_line,
			  struct jpeg_compress_struct *cinfo,
			  channel_stats_t stats, scale_t sample_mapping,
			  float no_data, int sample_count,
			  char *look_up_table_name);

void write_png_byte2byte(FILE *opng, unsigned char *byte_line,
                         channel_stats_t stats, scale_t sample_mapping,
                         png_structp png_ptr, png_infop png_info_ptr,
                         int sample_count);
void write_png_byte2rgbalpha(FILE *opng, unsigned char *byte_line,
                             channel_stats_t stats, scale_t sample_mapping,
                             png_structp png_ptr, png_infop png_info_ptr,
                             int sample_count);
void write_png_float2byte(FILE *opng, float *float_line,
               png_structp png_ptr, png_infop png_info_ptr,
			   channel_stats_t stats,
			   scale_t sample_mapping,
			   float no_data, int sample_count);
void write_rgb_png_byte2byte(FILE *opng,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
                  png_structp png_ptr, png_infop png_info_ptr,
			      int sample_count);
void write_png_byte2lut(FILE *opng, unsigned char *byte_line,
             png_structp png_ptr, png_infop png_info_ptr,
			 int sample_count, char *look_up_table_name);
void write_rgb_png_float2byte(FILE *opng,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
                   png_structp png_ptr, png_infop png_info_ptr,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int sample_count);
void write_png_float2lut(FILE *opng, float *float_line,
              png_structp png_ptr, png_infop png_info_ptr,
			  channel_stats_t stats, scale_t sample_mapping,
			  float no_data, int sample_count,
			  char *look_up_table_name);

void write_pgm_byte2byte(FILE *opgm, unsigned char *byte_line,
                         channel_stats_t stats, scale_t sample_mapping,
                         int sample_count);
void write_pgm_float2byte(FILE *opgm, float *float_line,
			  channel_stats_t blue_stats, scale_t sample_mapping,
			  float no_data, int sample_count);

// Prototypes from brs2jpg.c
int brs2jpg(char *browseFile, char *workreportFile, char *outFile);

// Prototypes from export_band.c
int multiband(char *format, char **band_name, int band_count);

#endif
