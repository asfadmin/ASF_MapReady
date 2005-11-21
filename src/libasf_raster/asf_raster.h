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

/* Prototypes from scaling.c *************************************************/
unsigned char *floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling);

/* Prototypes from stats.c ***************************************************/
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
void trim(char *infile, char *outfile, long long startX, long long startY,
          long long endX, long long endY);

/* Prototypes from fftMatch.c ************************************************/
void fftMatch(char *inFile1, char *inFile2, char *corrFile, char *descFile);

/* Prototypes from shaded_relief.c *******************************************/
void shaded_relief(char *inFile, char *outFile, int addSpeckle);

#endif
