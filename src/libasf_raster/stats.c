#include <math.h>
#include "asf.h"
#include "asf_nan.h"
#include "asf_raster.h"


void calc_stats_from_file(const char *inFile, char *band, double mask, double *min,
			  double *max, double *mean, double *stdDev,
			  gsl_histogram *histogram)
{
  FILE *fp;
  meta_parameters *meta;
  //char dataFile[255], metaFile[255];
  float *data;
  int band_number;
  long long pixel_count;
  long offset;

  //sprintf(dataFile, "%s.img", inFile);
  //sprintf(metaFile, "%s.meta", inFile);
  meta = meta_read(inFile);
  band_number = 
    get_band_number(meta->general->bands, meta->general->band_count, band);
  pixel_count = meta->general->line_count * meta->general->sample_count;
  offset = meta->general->line_count * band_number;
  data = (float *) MALLOC(sizeof(float) * pixel_count);
  fp = FOPEN(inFile, "rb");
  get_float_lines(fp, meta, offset, meta->general->line_count, data);
  calc_stats(data, pixel_count, mask, min, max, mean, stdDev);
  histogram = calc_histogram(data, pixel_count, *min, *max, 256);
  FCLOSE(fp);
  FREE(data);
  
  return;
}

gsl_histogram *calc_histogram (float *data, long long pixel_count,
			       double min, double max, size_t num_bins)
{
  // Initialize the histogram.
  gsl_histogram *hist = gsl_histogram_alloc (num_bins);
  gsl_histogram_set_ranges_uniform (hist, min, max);

  // Populate the histogram over every sample in the image.
  long long ii;
  for (ii=0; ii<pixel_count; ii++)
    gsl_histogram_increment (hist, data[ii]);

  return hist;
}

/* Calculate minimum, maximum, mean and standard deviation for a floating point
   image. A mask value can be defined that is excluded from this calculation.
   If no mask value is supposed to be used, pass the mask value as NAN. */
void calc_stats(float *data, long long pixel_count, double mask, double *min,
		double *max, double *mean, double *stdDev)
{
  long long ii, pix;

  /* Initialize values */
  *min = 99999;
  *max = -99999;
  *mean = 0.0;
  *stdDev = 0.0;
  pix = 0;

  for (ii=0; ii<pixel_count; ii++) {
    if (!ISNAN(mask)) {
      if (data[ii] < *min && !FLOAT_EQUIVALENT(data[ii], mask)) 
	*min = data[ii];
      if (data[ii] > *max && !FLOAT_EQUIVALENT(data[ii], mask)) 
	*max = data[ii];
      if (!FLOAT_EQUIVALENT(data[ii], mask)) {
	*mean += data[ii];
	pix++;      
      }
    }
  }
 *mean /= pix;
  for (ii=0; ii<pixel_count; ii++) {
    if (!ISNAN(mask)) {
      if (!FLOAT_EQUIVALENT(data[ii], mask)) 
	*stdDev += (data[ii] - *mean) * (data[ii] - *mean);
    }
    else
      *stdDev += (data[ii] - *mean) * (data[ii] - *mean);
  }
  *stdDev = sqrt(*stdDev/(pix - 1)); 
  
  return;
}

/* Estimate mean and standard deviation of an image by taking regular
   sampling points and doing the math with those. A mask value can be 
   defined that is excluded from this calculation. If no mask value is
   supposed to be used, pass the mask value as NAN. */
void estimate_stats(FILE *fpIn, meta_parameters *meta, int lines, int samples, 
		    double mask, double *min, double *max, double *mean, 
		    double *stdDev)
{
  float *imgLine = (float *) MALLOC(sizeof(float) * samples);
  double *stats, sum=0.0, variance=0.0;
  int ii, kk, line_increment, sample_increment;
  long pix=0, valid=0;

#define grid 100

  /* Define the necessary parameters */
  stats = (double *) MALLOC(sizeof(double) * grid * grid);
  line_increment = lines / grid;
  sample_increment = samples / grid;

  /* Collect values from sample grid */
  for (ii=0; ii<lines; ii+=line_increment) {
    get_float_line(fpIn, meta, ii, imgLine);
    for (kk=0; kk<samples; kk+=sample_increment) {
      if (!ISNAN(mask)) {
	if (FLOAT_EQUIVALENT(imgLine[kk], mask)) stats[pix] = NAN;
	else {
	  stats[pix] = imgLine[kk];
	  valid++;
	}
      }
      else {
	stats[pix] = imgLine[kk];
	valid++;
      }
      pix++;
    }
  }
  FSEEK64(fpIn, 0, 0);

  /* Estimate min, max, mean and standard deviation */
  *min = 99999;
  *max = -99999;
  for (ii=0; ii<grid*grid; ii++) 
    if (!ISNAN(stats[ii])) {
      if (stats[ii] < *min) *min = stats[ii];
      if (stats[ii] > *max) *max = stats[ii];
      sum += stats[ii];
    }
  *mean = sum / valid;
  for (ii=0; ii<grid*grid; ii++)
    if (!ISNAN(stats[ii])) variance += (stats[ii]-*mean) * (stats[ii]-*mean);
  *stdDev = sqrt(variance / (valid-1));

  return;
}

