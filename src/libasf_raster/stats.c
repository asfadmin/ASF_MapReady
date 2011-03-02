#include <math.h>
#include <assert.h>
#include "asf.h"
#include "asf_endian.h"
#include "asf_nan.h"
#include "asf_raster.h"
#include "envi.h"

#define SWAP(a,b) { register float t=(a); (a)=(b); (b)=t; }
#define EPSILON 1.E-15
#define INIT_MINMAX 1.E+30 

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
calc_stats_from_file_with_formula(const char *inFile, char *bands,
                                  calc_stats_formula_t formula_callback,
                                  double mask, double *min, double *max,
                                  double *mean, double *stdDev,
                                  gsl_histogram **histogram)
{
    int ii,jj,kk;
    const int N=MAX_BANDS;

    *min = 999999;
    *max = -999999;
    *mean = 0.0;

    meta_parameters *meta = meta_read(inFile);

    int band_numbers[N];
    for (ii=0; ii<N; ++ii)
        band_numbers[ii] = -1;

    int band_count = 0;
    if (bands) {
        char *s = STRDUP(bands); // a copy we can fiddle with
        char *q, *p = s;
        do {
            q = strchr(p, ',');
            if (q)
                *q = '\0';
            if (strlen(p) > 0 && strcmp(p, "???") != 0) {
                band_numbers[band_count] = get_band_number(meta->general->bands,
                    meta->general->band_count, p);
                //printf("%s -> %d\n", p, band_numbers[band_count]);
                ++band_count;
            }
            if (q)
                p = q+1;
        } while (q);
        FREE(s);
    }

    long band_offsets[N];
    float *band_data[N];

    for (ii=0; ii<N; ++ii) {
        if (band_numbers[ii] >= 0) {
            band_offsets[ii] = meta->general->line_count * band_numbers[ii];
            band_data[ii] = MALLOC(sizeof(float)*meta->general->sample_count);
        }
        else {
            band_offsets[ii] = -1;
            band_data[ii] = NULL;
        }
    }

    // pass 1 -- calculate mean, min & max
    FILE *fp = FOPEN(inFile, "rb");
    long long pixel_count=0;
    asfPrintStatus("\nCalculating min, max, and mean...\n");
    for (ii=0; ii<meta->general->line_count; ++ii) {
        asfPercentMeter((double)ii/(double)meta->general->line_count);

        for (kk=0; kk<N; ++kk) {
            if (band_data[kk]) {
                assert(band_offsets[kk] >= 0);
                get_float_line(fp, meta, ii + band_offsets[kk], band_data[kk]);
            }
        }

        for (jj=0; jj<meta->general->sample_count; ++jj) {
            int is_masked = FALSE;
            if (ISNAN(mask)) {
                for (kk=0; kk<N; ++kk)
                    if (band_data[kk] && FLOAT_EQUIVALENT(band_data[kk][jj], mask))
                        is_masked = TRUE;
            }

            if (!is_masked) {
                double data_arr[N];
                int ll;
                for (ll=0, kk=0; kk<N; ++kk)
                    if (band_data[kk])
                        data_arr[ll++] = band_data[kk][jj];
                assert(ll==band_count);

                double val = formula_callback(data_arr, mask);

                if (val < *min) *min = val;
                if (val > *max) *max = val;
                *mean += val;

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
        asfPercentMeter((double)ii/(double)meta->general->line_count);

        for (kk=0; kk<N; ++kk) {
            if (band_data[kk]) {
                assert(band_offsets[kk] >= 0);
                get_float_line(fp, meta, ii + band_offsets[kk], band_data[kk]);
            }
        }

        for (jj=0; jj<meta->general->sample_count; ++jj) {
            int is_masked = FALSE;
            if (ISNAN(mask)) {
                for (kk=0; kk<N; ++kk)
                    if (band_data[kk] && FLOAT_EQUIVALENT(band_data[kk][jj], mask))
                        is_masked = TRUE;
            }

            if (!is_masked) {
                double data_arr[N];
                int ll;
                for (ll=0, kk=0; kk<N; ++kk)
                    if (band_data[kk])
                        data_arr[ll++] = band_data[kk][jj];
                assert(ll==band_count);

                double val = formula_callback(data_arr, mask);

                *stdDev += (val - *mean) * (val - *mean);
                gsl_histogram_increment (hist, val);
            }
        }
    }
    asfPercentMeter(1.0);
    FCLOSE(fp);
    *stdDev = sqrt(*stdDev/(pixel_count - 1));

    for (ii=0; ii<N; ++ii)
        if (band_data[ii])
            FREE(band_data[ii]);

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
    int band_number;
    if (!band || strlen(band) == 0 || strcmp(band, "???") == 0 ||
        meta->general->band_count == 1) {
      band_number = 0;
    }
    else {
      band_number = get_band_number(meta->general->bands,
                                    meta->general->band_count, band);
    }

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

void calc_minmax_polsarpro(const char *inFile, double *min, double *max)
{
  int ii,jj;
  
  *min = 999999;
  *max = -999999;
  
  char *enviName = (char *) MALLOC(sizeof(char)*(strlen(inFile) + 10));
  sprintf(enviName, "%s.hdr", inFile);
  envi_header *envi = read_envi(enviName);
  meta_parameters *meta = envi2meta(envi);
  float *data = MALLOC(sizeof(float) * meta->general->sample_count);
  
  FILE *fp = FOPEN(inFile, "rb");
  asfPrintStatus("\nCalculating min and max ...\n");
  for (ii=0; ii<meta->general->line_count; ++ii) {
    asfPercentMeter(((double)ii/(double)meta->general->line_count));
    get_float_line(fp, meta, ii, data);
    
    for (jj=0; jj<meta->general->sample_count; ++jj) {
      ieee_big32(data[jj]);
      if (data[jj] < *min) *min = data[jj];
      if (data[jj] > *max) *max = data[jj];
    }
  }
  asfPercentMeter(1.0);
  FCLOSE(fp);
  FREE(data);
  meta_free(meta);
  FREE(envi);
  FREE(enviName);
}

// Coming from "Numerical recipes in C" - Eric Pottier's implementation
static float median_value(float *data, long n)
{
  int low = 0;
  int high = n - 1;
  int median = (low + high) / 2;
  int middle, ll, hh;
  
  for (;;) {
    if (high <= low)
      return data[median];
    if (high == low + 1) {
      if (data[low] > data[high])
	SWAP(data[low], data[high]);
      return data[median];
    }
  
    middle = (low + high) / 2;
    if (data[middle] > data[high])
      SWAP(data[middle], data[high]);
    if (data[low] > data[high])
      SWAP(data[low], data[high]);
    if (data[middle] > data[low])
      SWAP(data[middle], data[low]);

    SWAP(data[middle], data[low+1]);

    ll = low + 1;
    hh = high;
    for (;;) {
      do ll++; while (data[low] > data[ll]);
      do hh--; while (data[hh] > data[low]);
      if (hh < ll)
	break;
      SWAP(data[ll], data[hh]);
    }

    SWAP(data[low], data[hh]);

    if (hh <= median) 
      low = ll;
    if (hh >= median)
      high = hh - 1;
  }
}

void calc_minmax_median(const char *inFile, char *band, double mask, 
			double *min, double *max)
{
  long long ii, jj;
  float logeps = 10.0 * log10(EPSILON);
  float median, median0;
  
  meta_parameters *meta = meta_read(inFile);
  int band_number =
    (!band || strlen(band) == 0 || strcmp(band, "???") == 0) ? 0 :
    get_band_number(meta->general->bands, meta->general->band_count, band);
  long sample_count = meta->general->sample_count;
  long line_count = meta->general->line_count;
  long pixel_count = sample_count*line_count;
  long offset = line_count * band_number;
  float *data_line = MALLOC(sizeof(float) * sample_count);
  float *data = MALLOC(sizeof(float) * pixel_count);
  float *data2 = MALLOC(sizeof(float) * pixel_count);
			    
  // Culling invalid pixels
  FILE *fp = FOPEN(inFile, "rb");
  long valid_pixel_count = 0;
  asfPrintStatus("\nCalculating min and max using median...\n");
  for (ii=0; ii<line_count; ++ii) {
    get_float_line(fp, meta, ii + offset, data_line);
    asfPercentMeter(((double)ii/(double)line_count));
    for (jj=0; jj<sample_count; ++jj) {
      if (!FLOAT_EQUIVALENT(data_line[jj], -9999.99) ||
	  !FLOAT_EQUIVALENT(data_line[jj], logeps) ||
	  ISNAN(mask)) {
	data[valid_pixel_count] = data_line[jj];
	valid_pixel_count++;
      }
    }
  }
  asfPercentMeter(1.0);
  FCLOSE(fp);
  FREE(data_line);
  
  // Determine initial min and max
  *min = INIT_MINMAX;
  *max = -INIT_MINMAX;
  float minmin = INIT_MINMAX;
  float maxmax = -INIT_MINMAX;
  for (ii=0; ii<valid_pixel_count; ++ii) {
    data2[ii] = data[ii];
    if (data[ii] < minmin)
      minmin = data[ii];
    if (data[ii] > maxmax)
      maxmax = data[ii];
  }

  median0 = median_value(data, valid_pixel_count);

  // Determine minimum
  median = median0;
  *min = median0;
  for (ii=0; ii<3; ii++) {
    pixel_count = -1;
    for (jj=0; jj<valid_pixel_count; ++jj)
      if (median0 == minmin) {
	if (data[jj] <= median) {
	  pixel_count++;
	  data2[pixel_count] = data[jj];
	}
      }
      else {
	if (data[jj] < median) {
	  pixel_count++;
	  data2[pixel_count] = data[jj];
	}
      }
    median = median_value(data2, pixel_count);
    if (median == minmin)
      median = *min;
    *min = median;
  }

  // Determine maximum
  median = median0;
  *max = median0;
  for (ii=0; ii<3; ii++) {
    pixel_count = -1;
    for (jj=0; jj<valid_pixel_count; ++jj)
      if (median0 == maxmax) {
	if (data[jj] >= median) {
	  pixel_count++;
	  data2[pixel_count] = data[jj];
	}
      }
      else {
	if (data[jj] > median) {
	  pixel_count++;
	  data2[pixel_count] = data[jj];
	}
      }
    median = median_value(data2, pixel_count);
    if (median == maxmax)
      median = *max;
    *max = median;
  }

  FREE(data);
  FREE(data2);
}
