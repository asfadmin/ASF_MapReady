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

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

typedef enum {
  TRUNCATE=1,
  MINMAX,
  SIGMA,
  HISTOGRAM_EQUALIZE,
  NONE
} scale_t;

typedef enum {
  AVERAGE=1,
  GAUSSIAN,
  LAPLACE1,
  LAPLACE2,
  LAPLACE3,
  SOBEL,
  PREWITT,
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

typedef struct {
  double min;
  double max;
  double mean;
  double standard_deviation;
  gsl_histogram *hist;
  gsl_histogram_pdf *hist_pdf;
} channel_stats_t;

// Prototypes from bands.c
char **extract_band_names(char *bands, int band_count);
char **find_bands(char *in_base_name, int rgb_flag, char *red_channel, char *green_channel, 
		  char *blue_channel, int *num_found);
char **find_single_band(char *in_base_name, char *band, int *num_found);
int get_band_number(char *bands, int band_count, char *channel);
int split3(const char *rgb, char **pr, char **pg, char **pb, char sep);

/* Prototypes from scaling.c *************************************************/
unsigned char *floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling);

/* Prototypes from stats.c ***************************************************/
void calc_stats_from_file(const char *inFile, char *band, double mask, double *min,
			  double *max, double *mean, double *stdDev,
			  gsl_histogram *histogram);
gsl_histogram *calc_histogram (float *data, long long pixel_count,
			       double min, double max, size_t num_bins);
void calc_stats(float *data, long long pixel_count, double mask, double *min,
		double *max, double *mean, double *stdDev);
void estimate_stats(FILE *fpIn, meta_parameters *meta, int lines, int samples,
		    double mask, double *min, double *max, double *mean,
		    double *stdDev);

/* Prototypes from kernel.c **************************************************/
float kernel(filter_type_t filter_type, float *inbuf, int nLines, int nSamples,
	     int xLine, int xSample, int kernel_size, float damping_factor,
	     int nLooks);

/* Prototypes from interpolate.c *********************************************/
float interpolate(interpolate_type_t interpolation, FloatImage *inbuf, float yLine,
		  float xSample, weighting_type_t weighting, int sinc_points);

/* Prototypes from trim.c ****************************************************/
int trim(char *infile, char *outfile, long long startX, long long startY,
	 long long sizeX, long long sizeY);
void trim_zeros(char *infile, char *outfile, int *startX, int *endX);

/* Prototypes from fftMatch.c ************************************************/
void fftMatch(char *inFile1, char *inFile2, char *corrFile,
	      float *dx, float *dy, float *certainty);
void fftMatch_withOffsetFile(char *inFile1, char *inFile2, char *corrFile,
			     char *offsetFileName);

/* Prototypes from shaded_relief.c *******************************************/
void shaded_relief(char *inFile, char *outFile, int addSpeckle);

/* Prototypes from resample.c ************************************************/
int resample(char *infile, char *outfile, double xscalfact, double yscalfact);
int resample_nometa(char *infile, char *outfile,
		    double xscalfact, double yscalfact);
int resample_to_pixsiz(char *infile, char *outfile,
		       double xpixsiz, double ypixsiz);
int resample_to_square_pixsiz(char *infile, char *outfile, double pixsiz);

// Prototypes from tile.c
void create_image_tiles(char *inFile, char *outBaseName, int tile_size);

#endif
