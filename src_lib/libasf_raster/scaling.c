#include "asf_nan.h"
#include "asf_raster.h"

unsigned char *floats_to_bytes (float *data, long long pixel_count, float mask,
				scale_t scaling)
{
  long long ii, kk;
  double imin=99999, imax=-99999, imean=0, isdev=0, pix=0;
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
      if (imin < diff_min && imax > diff_max) {
	omin = diff_min;
	omax = diff_max;
      }
      else if (diff_min < diff_max) {
	omin = imin;
	omax = imin + 2*(imean-imin);
      }
      else {
	omin = imax - 2*(imax-imean);
	omax = imax;
      }
      
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
      printf("   Undefined scaling mechanism!\n");
      exit(0);
      break;
      
    }
  
  return pixels;
}
