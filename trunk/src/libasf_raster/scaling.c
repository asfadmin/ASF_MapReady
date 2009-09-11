#include "asf.h"
#include "asf_nan.h"
#include "asf_raster.h"

void floats_to_bytes_from_file(const char *inFile, const char *outFile,
                               char *band, float mask, scale_t scaling)
{
  FILE *fp;
  meta_parameters *meta;
  float *float_data;
  unsigned char *byte_data;
  int ii, band_number;
  long long pixel_count;
  long offset;

  meta = meta_read(inFile);
  band_number = (!band || strlen(band) == 0 || strcmp(band, "???")==0) ? 0 :
    get_band_number(meta->general->bands, meta->general->band_count, band);
  pixel_count = meta->general->line_count * meta->general->sample_count;
  offset = meta->general->line_count * band_number;
  float_data = (float *) MALLOC(sizeof(float) * pixel_count);
  fp = FOPEN(inFile, "rb");
  get_float_lines(fp, meta, offset, meta->general->line_count, float_data);
  FCLOSE(fp);
  byte_data = floats_to_bytes (float_data, pixel_count, mask, scaling);
  for (ii=0; ii<pixel_count; ii++)
    float_data[ii] = (float) byte_data[ii];
  meta->general->data_type = BYTE;
  meta_write(meta, outFile);
  fp = FOPEN(outFile, "wb");
  put_float_lines(fp, meta, offset, meta->general->line_count, float_data);
  FCLOSE(fp);
  FREE(float_data);
  FREE(byte_data);
  meta_free(meta);
}

unsigned char *floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling)
{
  long long ii;
  double imin=99999, imax=-99999, imean=0, isdev=0;
  double diff_min, diff_max, omin, omax, slope, offset;
  unsigned char *pixels = malloc (pixel_count * sizeof (unsigned char));

  switch (scaling) 
    {
    case TRUNCATE:
      /* Compute all the output pixels truncating the input values */
      for (ii=0; ii<pixel_count; ii++) 
	if (data[ii] < 0) {
	  pixels[ii] = 0;
	}
	else if (data[ii] > 255) {
	  pixels[ii] = 255;
	}
	else
	  pixels[ii] = data[ii] + 0.5;
      break;

    case MINMAX:
      /* Determine the minimum and maximum values for the image. Exclude the 
	 mask value for this calculation */
      for (ii=0; ii<pixel_count; ii++) {
        if (!ISNAN(mask)) {
	  if (data[ii] < imin && !FLOAT_EQUIVALENT(data[ii], mask)) 
	    imin = data[ii];
	  if (data[ii] > imax && !FLOAT_EQUIVALENT(data[ii], mask)) 
	    imax = data[ii];
	}
	else {
	  if (data[ii] < imin) imin = data[ii];
	  if (data[ii] > imax) imax = data[ii];
	}
      }
      omin = imin;
      omax = imax;

      /* Compute all the output pixels stretching the minimum and maximum value
	 into the byte range */
      slope = 255 / (omax-omin);
      offset = -slope * omin;
      for (ii=0; ii<pixel_count; ii++) {
	if ((slope * data[ii] + offset) < 0)
	  pixels[ii] = 0;
	else if ((slope * data[ii] + offset) > 255)
	  pixels[ii] = 255;
	else
	  pixels[ii] = slope * data[ii] + offset;
      }
      break;

    case SIGMA:
      /* Determine the minimum, maximum, mean, and standard deviation for the
	 image. Exclude the mask value from the calculation */
      calc_stats(data, pixel_count, 0.0, &imin, &imax, &imean, &isdev);

      /* Apply 2 sigma to calculate new minimum and maximum */
      diff_min = imean - 2*isdev;
      diff_max = imean + 2*isdev;

      omin = diff_min;
      omax = diff_max;
      
      if (diff_min < imin) omin = imin;
      if (diff_max > imax) omax = imax;
      
      /* Computing output pixels applying the sigma mapping */
      slope = 255 / (omax-omin);
      offset = -slope * omin;
      for (ii=0; ii<pixel_count; ii++) {
	if ((slope * data[ii] + offset) < 0)
	  pixels[ii] = 0;
	else if ((slope * data[ii] + offset) > 255)
	  pixels[ii] = 255;
	else 
	  pixels[ii] = slope * data[ii] + offset;
      }
      break;
    default:
      asfPrintError("Undefined scaling mechanism!");
      break;
      
    }
  
  return pixels;
}
