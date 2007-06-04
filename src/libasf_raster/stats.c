#include <math.h>
#include "asf.h"
#include "asf_nan.h"
#include "asf_raster.h"

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

  asfPrintStatus("\nFinding min, max, and mean...\n");
  for (ii=0; ii<pixel_count; ii++) {
    asfPercentMeter(((double)ii/(double)pixel_count));
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
    else {
        if (data[ii] < *min) *min = data[ii];
        if (data[ii] > *max) *max = data[ii];
        *mean += data[ii];
        ++pix;
    }
  }
  asfPercentMeter(1.0);
 *mean /= pix;

 asfPrintStatus("\nCalculating standard deviation...\n");
  for (ii=0; ii<pixel_count; ii++) {
    asfPercentMeter(((double)ii/(double)pixel_count));
    if (!ISNAN(mask)) {
      if (!FLOAT_EQUIVALENT(data[ii], mask))
	*stdDev += (data[ii] - *mean) * (data[ii] - *mean);
    }
    else
      *stdDev += (data[ii] - *mean) * (data[ii] - *mean);
  }
  *stdDev = sqrt(*stdDev/(pix - 1));
  asfPercentMeter(1.0);

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

void
calc_stats_from_file_with_formula(const char *inFile, char *first_band,
                                  char *second_band,
                                  calc_stats_formula_t formula_callback,
                                  double mask, double *min, double *max,
                                  double *mean, double *stdDev,
                                  gsl_histogram **histogram)
{
    int ii,jj;
    int first_band_number, second_band_number;

    *min = 999999;
    *max = -999999;
    *mean = 0.0;

    meta_parameters *meta = meta_read(inFile);

    if (!first_band ||
        strlen(first_band) == 0 ||
        strcmp(first_band, "???") == 0)
    {
        first_band_number = 0;
    }
    else
    {
        first_band_number =
            get_band_number(meta->general->bands, meta->general->band_count,
                            first_band);
    }

    if (!second_band ||
        strlen(second_band) == 0 ||
        strcmp(second_band, "???") == 0)
    {
        second_band_number = 0;
    }
    else
    {
        second_band_number =
            get_band_number(meta->general->bands, meta->general->band_count,
                            second_band);
    }

    long first_offset = meta->general->line_count * first_band_number;
    long second_offset = meta->general->line_count * second_band_number;

    float *band1_data = MALLOC(sizeof(float) * meta->general->sample_count);
    float *band2_data = NULL;
    if (second_band_number)
        band2_data = MALLOC(sizeof(float) * meta->general->sample_count);

    // pass 1 -- calculate mean, min & max
    FILE *fp = FOPEN(inFile, "rb");
    long long pixel_count=0;
    asfPrintStatus("\nCalculating min, max, and mean...\n");
    for (ii=0; ii<meta->general->line_count; ++ii) {
      asfPercentMeter((double)ii/(double)meta->general->line_count);
        get_float_line(fp, meta, ii + first_offset, band1_data);
        if (band2_data)
        {
            get_float_line(fp, meta, ii + second_offset, band2_data);

            for (jj=0; jj<meta->general->sample_count; ++jj) {
                if (ISNAN(mask) ||
                    (!FLOAT_EQUIVALENT(band1_data[jj], mask) &&
                     !FLOAT_EQUIVALENT(band2_data[jj], mask)))
                {
                    double val =
                        formula_callback(band1_data[jj], band2_data[jj], mask);

                    if (val < *min) *min = val;
                    if (val > *max) *max = val;
                    *mean += val;

                    ++pixel_count;
                }
            }
        }
        else
        {
            for (jj=0; jj<meta->general->sample_count; ++jj) {
                if (ISNAN(mask) || !FLOAT_EQUIVALENT(band1_data[jj], mask)) {
                    double val = formula_callback(band1_data[jj], 0, mask);
                    if (val < *min) *min = val;
                    if (val > *max) *max = val;
                    *mean += val;
                    ++pixel_count;
                }
            }
        }
    }
    asfPercentMeter(1.0);
    FCLOSE(fp);

    *mean /= pixel_count;

    // Guard against weird data
    if(!(*min<*max)) *max = *min + 1;

    // Initialize the histogram.
    const int num_bins = 256;
    gsl_histogram *hist = gsl_histogram_alloc (num_bins);
    gsl_histogram_set_ranges_uniform (hist, *min, *max);
    *stdDev = 0.0;

    // pass 2 -- update histogram, calculate standard deviation
    fp = FOPEN(inFile, "rb");
    asfPrintStatus("\nCalculating standard deviation and histogram...\n");
    for (ii=0; ii<meta->general->line_count; ++ii) {
      asfPercentMeter((double)ii/(double)meta->general->line_count);
        get_float_line(fp, meta, ii + first_offset, band1_data);
        if (band2_data)
        {
            get_float_line(fp, meta, ii + second_offset, band2_data);
            for (jj=0; jj<meta->general->sample_count; ++jj) {
                if (ISNAN(mask) ||
                    (!FLOAT_EQUIVALENT(band1_data[jj], mask) &&
                     !FLOAT_EQUIVALENT(band2_data[jj], mask)))
                {
                    double val =
                        formula_callback(band1_data[jj], band2_data[jj], mask);

                    *stdDev += (val - *mean) * (val - *mean);
                    gsl_histogram_increment (hist, val);
                }
            }
        }
        else
        {
            for (jj=0; jj<meta->general->sample_count; ++jj) {
                if (ISNAN(mask) || !FLOAT_EQUIVALENT(band1_data[jj], mask)) {
                    double val = formula_callback(band1_data[jj], 0, mask);
                    *stdDev += (val - *mean) * (val - *mean);
                    gsl_histogram_increment (hist, val);
                }
            }
        }
    }
    asfPercentMeter(1.0);
    FCLOSE(fp);
    *stdDev = sqrt(*stdDev/(pixel_count - 1));

    FREE(band1_data);
    FREE(band2_data);

    *histogram = hist;
}

void
calc_stats_from_file(const char *inFile, char *band, double mask,
                     double *min, double *max, double *mean,
                     double *stdDev, gsl_histogram **histogram)
{
    int ii,jj;

    *min = 999999;
    *max = -999999;
    *mean = 0.0;

    meta_parameters *meta = meta_read(inFile);
    int band_number =
        (!band || strlen(band) == 0 || strcmp(band, "???") == 0) ? 0 :
        get_band_number(meta->general->bands, meta->general->band_count, band);
    long offset = meta->general->line_count * band_number;
    float *data = MALLOC(sizeof(float) * meta->general->sample_count);

    // pass 1 -- calculate mean, min & max
    FILE *fp = FOPEN(inFile, "rb");
    long long pixel_count=0;
    asfPrintStatus("\nCalculating min, max, and mean...\n");
    for (ii=0; ii<meta->general->line_count; ++ii) {
      asfPercentMeter(((double)ii/(double)meta->general->line_count));
        get_float_line(fp, meta, ii + offset, data);

        for (jj=0; jj<meta->general->sample_count; ++jj) {
            if (ISNAN(mask) || !FLOAT_EQUIVALENT(data[jj], mask)) {
                if (data[jj] < *min) *min = data[jj];
                if (data[jj] > *max) *max = data[jj];
                *mean += data[jj];
                ++pixel_count;
            }
        }
    }
    asfPercentMeter(1.0);
    FCLOSE(fp);

    *mean /= pixel_count;

    // Guard against weird data
    if(!(*min<*max)) *max = *min + 1;

    // Initialize the histogram.
    const int num_bins = 256;
    gsl_histogram *hist = gsl_histogram_alloc (num_bins);
    gsl_histogram_set_ranges_uniform (hist, *min, *max);
    *stdDev = 0.0;

    // pass 2 -- update histogram, calculate standard deviation
    fp = FOPEN(inFile, "rb");
    asfPrintStatus("\nCalculating standard deviation and histogram...\n");
    for (ii=0; ii<meta->general->line_count; ++ii) {
      asfPercentMeter(((double)ii/(double)meta->general->line_count));
        get_float_line(fp, meta, ii + offset, data);

        for (jj=0; jj<meta->general->sample_count; ++jj) {
            if (ISNAN(mask) || !FLOAT_EQUIVALENT(data[jj], mask)) {
                *stdDev += (data[jj] - *mean) * (data[jj] - *mean);
                gsl_histogram_increment (hist, data[jj]);
            }
        }
    }
    asfPercentMeter(1.0);
    FCLOSE(fp);
    *stdDev = sqrt(*stdDev/(pixel_count - 1));

    FREE(data);

    *histogram = hist;
}

void
calc_stats_rmse_from_file(const char *inFile, char *band, double mask,
                          double *min, double *max, double *mean,
                          double *stdDev, double *rmse,
                          gsl_histogram **histogram)
{
  double se;
  int ii,jj;

  *min = 999999;
  *max = -999999;
  *mean = 0.0;

  meta_parameters *meta = meta_read(inFile);
  int band_number =
        (!band || strlen(band) == 0 || strcmp(band, "???") == 0) ? 0 :
      get_band_number(meta->general->bands, meta->general->band_count, band);
  long offset = meta->general->line_count * band_number;
  float *data = MALLOC(sizeof(float) * meta->general->sample_count);

    // pass 1 -- calculate mean, min & max
  FILE *fp = FOPEN(inFile, "rb");
  long long pixel_count=0;
  asfPrintStatus("\nCalculating min, max, and mean...\n");
  for (ii=0; ii<meta->general->line_count; ++ii) {
    asfPercentMeter(((double)ii/(double)meta->general->line_count));
    get_float_line(fp, meta, ii + offset, data);

    for (jj=0; jj<meta->general->sample_count; ++jj) {
      if (ISNAN(mask) || !FLOAT_EQUIVALENT(data[jj], mask)) {
        if (data[jj] < *min) *min = data[jj];
        if (data[jj] > *max) *max = data[jj];
        *mean += data[jj];
        ++pixel_count;
      }
    }
  }
  asfPercentMeter(1.0);
  FCLOSE(fp);

  *mean /= pixel_count;

    // Guard against weird data
  if(!(*min<*max)) *max = *min + 1;

    // Initialize the histogram.
  const int num_bins = 256;
  gsl_histogram *hist = gsl_histogram_alloc (num_bins);
  gsl_histogram_set_ranges_uniform (hist, *min, *max);
  *stdDev = 0.0;

    // pass 2 -- update histogram, calculate standard deviation
  fp = FOPEN(inFile, "rb");
  asfPrintStatus("\nCalculating standard deviation, rmse, and histogram...\n");
  se = 0.0;
  for (ii=0; ii<meta->general->line_count; ++ii) {
    asfPercentMeter(((double)ii/(double)meta->general->line_count));
    get_float_line(fp, meta, ii + offset, data);

    for (jj=0; jj<meta->general->sample_count; ++jj) {
      if (ISNAN(mask) || !FLOAT_EQUIVALENT(data[jj], mask)) {
        *stdDev += (data[jj] - *mean) * (data[jj] - *mean);
        gsl_histogram_increment (hist, data[jj]);
        se += (data[jj] - *mean) * (data[jj] - *mean);
      }
    }
  }
  asfPercentMeter(1.0);
  FCLOSE(fp);
  *stdDev = sqrt(*stdDev/(pixel_count - 1));
  *rmse = sqrt(se/(pixel_count - 1));

  FREE(data);

  *histogram = hist;
}

