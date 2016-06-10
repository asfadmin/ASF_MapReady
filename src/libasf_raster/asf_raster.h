#ifndef _ASF_RASTER_H_
#define _ASF_RASTER_H_
/******************************************************************************
NAME:
 asf_raster.h

DESCRIPTION:
 C header with definitions & prototypes for libasf_raster library

******************************************************************************/

#include "asf_meta.h"
#include "float_image.h"
#include "banded_float_image.h"
#include <gsl/gsl_spline.h>

typedef enum {
  TRUNCATE=1,
  MINMAX,
  MINMAX_MEDIAN,
  SIGMA,
  SIGMA3,
  HISTOGRAM_EQUALIZE,
  FIXED,
  NONE
} scale_t;

typedef enum {
  AVERAGE=1,
  GAUSSIAN,
  LAPLACE1,
  LAPLACE2,
  LAPLACE3,
  SOBEL,
  SOBEL_X,
  SOBEL_Y,
  PREWITT,
  PREWITT_X,
  PREWITT_Y,
  EDGE,
  MEDIAN,
  LEE,
  ENHANCED_LEE,
  FROST,
  ENHANCED_FROST,
  GAMMA_MAP,
  KUAN
} filter_type_t;

typedef enum {
  NEAREST=1,
  BILINEAR,
  BICUBIC,
  SPLINES,
  SINC
} interpolate_type_t;

typedef enum {
  NO_WEIGHT=1,
  KAISER,
  HAMMING,
  LANCZOS
} weighting_type_t;

typedef enum {
  EDGE_TRUNCATE=1
} edge_strategy_t;

typedef struct {
  double min;
  double max;
  double median;
  double mean;
  double standard_deviation;
  gsl_histogram *hist;
  gsl_histogram_pdf *hist_pdf;
} channel_stats_t;

typedef struct
{
  int n;
  double *x;
  double *y;
  int dateline;
} Poly;

typedef double calc_stats_formula_t(double band_values[], double no_data_value);

// Prototypes from arithmetic.c
int arithmetic(char *refFile, char *targetFile, char *operation, char *outFile);

// Prototypes from bands.c
char **extract_band_names(char *bands, int band_count);
char **find_bands(char *in_base_name, int rgb_flag, char *red_channel, char *green_channel, 
		  char *blue_channel, int *num_found);
char **find_single_band(char *in_base_name, char *band, int *num_found);
int get_band_number(char *bands, int band_count, const char *channel);
int split3(const char *rgb, char **pr, char **pg, char **pb, char sep);
char *get_band_name(char *band_str, int band_count, int band_num);

/* Prototypes from scaling.c *************************************************/
unsigned char *floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling);
unsigned char *floats_to_bytes_ext(float *data, long long pixel_count, 
  float mask, scale_t scaling, float scale_factor);
void floats_to_bytes_from_file(const char *inFile, const char *outFile,
                               char *band, float mask, scale_t scaling);
void floats_to_bytes_from_file_ext(const char *inFile, const char *outFile,
  char *band, float mask, scale_t scaling, float scale_factor);

/* Prototypes from stats.c ***************************************************/
void calc_stats_rmse_from_file(const char *inFile, char *band, double mask, double *min,
                               double *max, double *mean, double *stdDev, double *rmse,
                               gsl_histogram **histogram);
void calc_stats_rmse_from_file_ext(const char *inFile, char *band, double mask, double *min,
                               double *max, double *mean, double *stdDev, double *rmse,
                               double *valid, gsl_histogram **histogram);
void calc_stats_from_file(const char *inFile, char *band, double mask, double *min,
			  double *max, double *mean, double *stdDev,
			  gsl_histogram **histogram);
void calc_stats_from_file_ext(const char *inFile, char *band, double mask, 
        double *min, double *max, double *mean, double *stdDev, double *valid,
			  gsl_histogram **histogram);
void calc_stats(float *data, long long pixel_count, double mask, double *min,
		double *max, double *mean, double *stdDev);
void calc_stats_ext(float *data, long long pixel_count, double mask, int report,
		                double *min, double *max, double *mean, double *stdDev, 
		                double *percentValid);
void estimate_stats(FILE *fpIn, meta_parameters *meta, int lines, int samples,
		    double mask, double *min, double *max, double *mean,
		    double *stdDev);
void
calc_stats_from_file_with_formula(const char *inFile, char *bands,
                                  calc_stats_formula_t formula_callback,
                                  double mask, double *min, double *max,
                                  double *mean, double *stdDev,
                                  gsl_histogram **histogram);
void calc_minmax_median(const char *inFile, char *band, double mask, 
			double *min, double *max);
void calc_minmax_polsarpro(const char *inFile, double *min, double *max);

/* Prototypes from kernel.c **************************************************/
float kernel(filter_type_t filter_type, float *inbuf, int nLines, int nSamples,
	     int xLine, int xSample, int kernel_size, float damping_factor,
	     int nLooks);
void kernel_filter(char *inFile, char *outFile, filter_type_t filter, 
		   int kernel_size, float damping, int nLooks);

/* Prototypes from interpolate.c *********************************************/
float interpolate(interpolate_type_t interpolation, FloatImage *inbuf, float yLine,
		  float xSample, weighting_type_t weighting, int sinc_points);

/* Prototypes from trim.c ****************************************************/
int trim(char *infile, char *outfile, long long startX, long long startY,
	 long long sizeX, long long sizeY);
void trim_zeros(char *infile, char *outfile, int *startX, int *endX);
void trim_zeros_ext(char *infile, char *outfile, int update_meta,
                    int do_top, int do_left);
void trim_wedges(char *infile, char *outfile);
void trim_latlon(char *infile, char *outfile, double lat_min, double lat_max,
                 double lon_min, double lon_max);
void trim_to(char *infile, char *outfile, char *metadata_file);
void subset_by_latlon(char *infile, char *outfile, double *lat, double *lon, 
  int nCoords);
void subset_by_map(char *infile, char *outfile, double minX, double maxX,
  double minY, double maxY);
void clip_to_polygon(char *inFile, char *outFile, double *lat, double *lon, 
  int *start, int nParts, int nVertices);

// Prototypes from raster_calc.c
int raster_calc(char *outFile, char *expression, int input_count, 
		char **inFiles);

/* Prototypes from fftMatch.c ************************************************/
int fftMatch(char *inFile1, char *inFile2, char *corrFile,
	     float *dx, float *dy, float *certainty);
void fftMatch_withOffsetFile(char *inFile1, char *inFile2, char *corrFile,
			     char *offsetFileName);
int fftMatch_gridded(char *inFile1, char *inFile2, char *gridFile,
	     float *dx, float *dy, float *certainty,
             int size, double tolerance, int overlap);
int fftMatch_proj(char *inFile1, char *inFile2, float *offsetX, float *offsetY,
                float *certainty);
int fftMatch_either(char *inFile1, char *inFile2, float *offsetX,
                    float *offsetY, float *certainty);
int fftMatch_projList(char *inFile1, char *descFile);
int fftMatch_opt(char *inFile1, char *inFile2, float *offsetX, float *offsetY);

         
/* Prototypes from shaded_relief.c *******************************************/
void shaded_relief(char *inFile, char *outFile, int addSpeckle, int water);

/* Prototypes from resample.c ************************************************/
int resample(const char *infile, const char *outfile, 
             double xscalfact, double yscalfact);
int resample_ext(const char *infile, const char *outfile,
                 double xscalfact, double yscalfact, int use_nn);
int resample_nometa(const char *infile, const char *outfile,
		    double xscalfact, double yscalfact);
int resample_to_pixsiz(const char *infile, const char *outfile,
		       double xpixsiz, double ypixsiz);
int resample_to_square_pixsiz(const char *infile, const char *outfile, 
                              double pixsiz);
int resample_to_pixsiz_nn(const char *infile, const char *outfile,
                          double xpixsiz, double ypixsiz);

/* Prototypes from smooth.c **************************************************/
int smooth(const char *infile, const char *outfile, int kernel_size,
           edge_strategy_t edge_strategy);

// Prototypes from tile.c
void create_image_tiles(char *inFile, char *outBaseName, int tile_size);
void create_image_hierarchy(char *inFile, char *outBaseName, int tile_size);

// Prototypes from look_up_table.c
#define MAX_LUT_DN 8192
void apply_look_up_table_byte(char *lutFile, unsigned char *in_buffer,
			 int pixel_count, unsigned char *rgb_buffer);
void apply_look_up_table_int(char *lutFile, int *in_buffer,
			 int pixel_count, unsigned char *rgb_buffer);
int read_lut(char *lutFile, unsigned char *lut_buffer);
int is_jasc_palette_lut(const char *name);

// Prototypes from fit_warp.c
int fit_warp(const char *offsetsFile, const char *imageName, const char *outName);

// Prototypes from diffimage.c
typedef enum {
  UNKNOWN_GRAPHICS_TYPE=0,
  ASF_IMG,
  ENVI_IMG,
  JPEG_IMG,
  PGM_IMG,
  PPM_IMG,
  PBM_IMG,
  STD_TIFF_IMG,
  GEO_TIFF_IMG,
  BMP_IMG,
  GIF_IMG,
  PNG_IMG,
  ENVI_MAT,
  GEO_TIFF_MAT
} graphics_file_t;

typedef struct {
  int stats_good;
  double min;
  double max;
  double mean;
  double sdev;
  double rmse;
  gsl_histogram *hist;
  gsl_histogram_pdf *hist_pdf;
} stats_t;

typedef struct {
    stats_t i;
    stats_t q;
} complex_stats_t;

typedef struct {
  int psnr_good;
  double psnr;
} psnr_t;

typedef struct {
    psnr_t i;
    psnr_t q;
} complex_psnr_t;

typedef struct {
  int dxdy_good;
  int cert_good;
  float dx; // In whatever units the image is in
  float dy;
  float cert; // Certainty from fftMatch
} shift_data_t;

int diffimage(char *inFile1, char *inFile2, char *outputFile, char *logFile,
	      char ***bands1, char ***bands2,
	      int *num_bands1, int *num_bands2, int *complex,
	      stats_t **stats1, stats_t **stats2,
	      complex_stats_t **complex_stats1, 
	      complex_stats_t **complex_stats2,
	      psnr_t **psnrs, complex_psnr_t **complex_psnr,
	      shift_data_t **data_shift);

// wrapper for gsl_spline_eval that does bounds-checking
double gsl_spline_eval_check(gsl_spline *s, double x, gsl_interp_accel *a);

#endif
