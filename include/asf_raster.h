#ifndef _ASF_RASTER_H_
#define _ASF_RASTER_H_
/******************************************************************************
NAME:
 asf_raster.h

DESCRIPTION:
 C header with definitions & prototypes for libasf_raster library

******************************************************************************/

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

typedef enum {
  TRUNCATE=1,
  MINMAX,
  SIGMA
} scale_t;

/* Prototypes from scaling.c *************************************************/
unsigned char *floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling);

/* Prototypes from stats.c ***************************************************/
void calc_stats(float *data, long long pixel_count, double mask, double *min,
		double *max, double *mean, double *stdDev);
void estimate_stats(FILE *fpIn, meta_parameters *meta, int lines, int samples, 
		    double mask, double *min, double *max, double *mean, 
		    double *stdDev);


#endif
