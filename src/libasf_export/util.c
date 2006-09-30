
#include <limits.h>

#include <asf.h>
#include <asf_export.h>
#include <asf_raster.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>


/* Get sample size in bytes of the data types represented by the
   meta_parameters_t.  */
size_t
get_sample_size (meta_parameters *metadata)
{
  size_t sample_size;
  switch ( metadata->general->data_type ) {
  case BYTE:
    sample_size = sizeof (int8_t);
    break;
  case INTEGER16:
    sample_size = sizeof (int16_t);
    break;
  case INTEGER32:
    sample_size = sizeof (int32_t);
    break;
  case REAL32:
    sample_size = sizeof (float);
    break;
  case REAL64:
    sample_size = sizeof (double);
    break;
  default:
    /* Other types aren't handled.  */
    asfPrintError ("This program can handle byte, int16, int32, real32, and real64 data types.\n");
  }

  return sample_size;
}


/* Get the image data in data file image_data_file_name, using
   metadata metadata.  A pointer to new memory containing the image
   data is returned.  */
void *
get_image_data (meta_parameters *metadata, const char *image_data_file)
{
  size_t sample_size = get_sample_size (metadata);
  size_t pixel_count;
  void *data;
  size_t read_count;
  int return_code;

  /* Read the image data itself.  */
  FILE *ifp = fopen (image_data_file, "r");
  if ( ifp == NULL )
    asfPrintError("Failed to open %s: %s", image_data_file, strerror(errno));

  /* Total number of samples in image.  */
  pixel_count = metadata->general->line_count * metadata->general->sample_count;
  data = MALLOC (pixel_count * sample_size);
  read_count = fread (data, sample_size, pixel_count, ifp);
  if ( read_count != pixel_count ) {
    if ( feof (ifp) ) {
      asfPrintError("Read wrong amount of data from %s", image_data_file);
    }
    else if ( ferror (ifp) ) {
      asfPrintError("Read of file %s failed: %s", image_data_file,
                    strerror(errno));
    }
    else {
      /* Shouldn't get here.  */
      asfPrintError("Unknown error reading %s\n", image_data_file);
    }
    exit (EXIT_FAILURE);
  }

  return_code = fclose (ifp);
  asfRequire ((return_code==0), "Error closing file %s\n", image_data_file);

  return data;
}


/* Return the average of all the kernel_size * kernel_size elements
   centered around element i, j.  The i and j arguments must be within
   the bounds of img, and the kernel_size must be odd.  The image is
   reflected at the edges and corners for the purpose of determining
   this average.  */
unsigned char
averaging_kernel (gsl_matrix_uchar *img, int kernel_size, size_t i, size_t j)
{
  int i_idx, j_idx;
  int i_min = i - kernel_size / 2;
  int i_max = i + kernel_size / 2;
  int j_min = j - kernel_size / 2;
  int j_max = j + kernel_size / 2;
  int sum = 0;
  int average; /* Truncated average.  */

  asfRequire(kernel_size%2 != 0, "Odd-sized kernels only.\n");

  for ( i_idx = i_min ; i_idx < i_max ; i_idx++ ) {
    /* The i index to use, adjusted in case we are off the edge of the
       image.  This choice (and the corresponding choice for j)
       implement a kernel that pretends that a mirror image of the
       image exists at the image edges (and corners). */
    int itu = i_idx;
    if ( itu < 0 )
      itu = -itu - 1;
    else if ( itu >= img->size1 )
      itu = img->size1 - (itu - img->size1) - 1;
    for ( j_idx = j_min ; j_idx < j_max ; j_idx++ ) {
      /* See the comment for variable itu above.  */
      int jtu = j_idx;
      if ( jtu < 0 )
        jtu = -jtu - 1;
      else if ( jtu >= img->size2 )
        jtu = img->size2 - (jtu - img->size2) - 1;
      sum += gsl_matrix_uchar_get (img, itu, jtu);
    }
  }

  average = sum / pow (kernel_size, 2); /* Truncated average.  */
  /* Since we are averaging unsigned char values, this should always
     be true.  */
  asfRequire(average<=UCHAR_MAX,
             "The average value of the image is above its maximum value.\n");

  return average;
}

/* Average together tiles of kernel_size pixels in the *width by
   *height image at pixels, reducing its size by a factor of about
   kernel size in each dimension.  The new image replaces the old in
   pixels (extra memory is freed) and a pointer to the possibly
   relocated pixel data is returned.  The new image width and height
   replace the input width and height arguments.  */
unsigned char *average_unsigned_char_pixels (unsigned char *pixels,
                                             unsigned long *width,
                                             unsigned long *height,
					     int kernel_size)
{
  /* Input width and height.  */
  size_t iwidth = *width, iheight = *height;
  gsl_matrix_uchar *iimg;
  size_t ii, jj;
  size_t owidth;
  size_t oheight;
  gsl_matrix_uchar *oimg;
  unsigned char *reallocated_pixels;

  asfRequire (kernel_size%2 != 0, "Odd-sized kernels only.\n");

  /* Make a matrix form of the input image for easy indexing.  */
  iimg = gsl_matrix_uchar_alloc (iheight, iwidth);
  for ( ii = 0 ; ii < iheight ; ii++ ) {
    for ( jj = 0 ; jj < iwidth ; jj++ ) {
      gsl_matrix_uchar_set (iimg, ii, jj, pixels[ii * iwidth + jj]);
    }
  }

  /* Dimensions of the averaged image.  */
  owidth = ceil (iwidth / kernel_size);
  oheight = ceil (iheight / kernel_size);

  /* Form the output image.  */
  oimg = gsl_matrix_uchar_alloc (oheight, owidth);
  for ( ii = 0 ; ii < oheight ; ii++ ) {
    for ( jj = 0 ; jj < owidth ; jj++ ) {
      gsl_matrix_uchar_set
        (oimg, ii, jj, averaging_kernel (iimg, kernel_size,
                                         ii * kernel_size + kernel_size / 2,
                                         jj * kernel_size + kernel_size / 2));
    }
  }

  /* Write output image back into pixel memory.  */
  for ( ii = 0 ; ii < oheight ; ii++ ) {
    for ( jj = 0 ; jj < owidth ; jj++ ) {
      pixels[ii * owidth + jj] = gsl_matrix_uchar_get (oimg, ii, jj);
    }
  }

  /* Done with matrix forms of the image.  */
  gsl_matrix_uchar_free (oimg);
  gsl_matrix_uchar_free (iimg);

  /* Modify the input/output arguments and resize the allocated space
     as promised.  */
  *width = owidth;
  *height = oheight;
  reallocated_pixels = realloc (pixels, owidth * oheight);

  return reallocated_pixels;
}

/* Given a block of pixel_count floats, map them linearly from range
   [max (minsample, mean - 3 * sigma), min (maxsample, mean + 3 * sigma)]
   into the unsigned char range, with input samples outside the above
   range clamped.  Return the data in new malloc()ed memory.  */
unsigned char *map_floats_to_unsigned_bytes (float *daf, size_t pixel_count)
{
  unsigned char *pixels = malloc (pixel_count * sizeof (unsigned char));

  /* Minimum and maximum values in the input data.  */
  float imin = gsl_stats_float_min (daf, 1, pixel_count);
  float imax = gsl_stats_float_max (daf, 1, pixel_count);
  /* Mean value of input data.  */
  float imean = gsl_stats_float_mean (daf, 1, pixel_count);
  /* Standard deviation of input data.  */
  float isdev = gsl_stats_float_sd (daf, 1, pixel_count);
  /* Minimum and maximum after clamping.  */
  float omin = GSL_MAX (imean - 3 * isdev, imin);
  float omax = GSL_MIN (imean + 3 * isdev, imax);

  /* Shift we need to apply to the data to get it to fall in the
     range of the unsigned chars.  */
  float bias = -omin + 0.25;

  /* Compute all the output pixels.  */
  size_t ii;
  for ( ii = 0 ; ii < pixel_count ; ii++ ) {
    if ( daf[ii] < omin ) {
      pixels[ii] = 0;           /* Clamp low.  */
    }
    else if ( daf[ii] > omax ) {
      pixels[ii] = UCHAR_MAX;   /* Clamp high.  */
    }
    else {
      pixels[ii] = (daf[ii] + bias) * (UCHAR_MAX / (omax - omin));
    }
  }

  return pixels;
}


/* Scale the *width x *height image at pixels st its large dimension
   is less than or equal to max_large_dimension.  The memory pointed
   to by pixels is resized and possible relocated, with the new
   location being returned.  The new image width and height are
   returned in *width and *height.  */
unsigned char *scale_unsigned_char_image_dimensions (unsigned char *pixels,
                                      unsigned long max_large_dimension,
                                      unsigned long *width,
                                      unsigned long *height)
{
  /* This assertion is pretty obvious, but since the algorithm needs
     it to work correctly, its included.  */
  asfRequire(max_large_dimension>1,
             "Image needs to be at least one pixel by one pixel.\n");
  if ( GSL_MAX (*width, *height) > max_large_dimension ) {
    int kernel_size = GSL_MAX (*width, *height) / max_large_dimension + 1;
    if ( kernel_size % 2 != 1 ) {
      kernel_size++;
    }
    pixels = average_unsigned_char_pixels (pixels, width, height, kernel_size);
  }

  return pixels;
}


/* Figure basic statistics for the image as needed for the different data
   scaling techniques available to the user */
void
get_statistics (FloatImage *si, scale_t sample_mapping, int sampling_stride,
                float *mean, float *standard_deviation,
                float *min_sample, float *max_sample,
                float *omin, float *omax, gsl_histogram **hist,
                float no_data)
{

  if ( sample_mapping == SIGMA ) {
    float_image_approximate_statistics (si, sampling_stride, mean,
                                        standard_deviation, no_data);
    *omin = *mean - 2 * (*standard_deviation);
    *omax = *mean + 2 * (*standard_deviation);
  }

  else if ( sample_mapping == MINMAX ) {
    float_image_statistics (si, min_sample, max_sample, mean,
                            standard_deviation, no_data);
    *omin = *min_sample;
    *omax = *max_sample;
  }

  else if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
    float_image_statistics (si, min_sample, max_sample, mean,
                            standard_deviation, no_data);
    // Add a little bit of tail padding on the histgram to avoid problems
    // when searching for image min or max in the histogram (gsl histogram
    // limitation)
    *omin = *min_sample * 0.025 * (*min_sample);
    *omax = *max_sample * 0.025 * (*max_sample);
    *hist = float_image_gsl_histogram (si, *omin, *omax, NUM_HIST_BINS);
  }
}


#ifndef linux
#ifndef win32
static double
round (double arg)
{
  return floor (arg + 0.5);
}
#endif // #ifndef win32
#endif // #ifndef linux

/* Create a byte value based on the floating point value and whatever scaling
   method the user chose */
unsigned char
pixel_float2byte(float paf, scale_t sample_mapping,
                 float omin, float omax, gsl_histogram *hist,
                 gsl_histogram_pdf *hist_pdf, float no_data_value)
{
  const unsigned char min_brightness=0;          // Minimum brightess via byte
  const unsigned char max_brightness=UCHAR_MAX;  // Maximum brightess via byte
  unsigned char pab;                             // Pixel As Byte.
  size_t hist_bin;                       // histogram bin for given float value

  // This case is easy -- always map the "no data" value to 0.
  if (paf == no_data_value)
      return 0;

  switch ( sample_mapping ) {
  case TRUNCATE:
    if ( paf <= (float) min_brightness ) {
      pab = min_brightness;
    }
    else if ( paf >= (float) max_brightness ) {
      pab = max_brightness;
    }
    else {
      pab = (unsigned char) paf;
    }
    break;
  case MINMAX:
  case SIGMA:
    if ( paf < omin ) {
      pab = min_brightness;
    }
    else if ( paf > omax ) {
      pab = max_brightness;
    }
    else {
      pab = round (((paf - omin) / (omax - omin)) * max_brightness);
    }
    break;
  case HISTOGRAM_EQUALIZE:
    // Since we used a mask when we called float_image_statistics() earlier,
    // we've got to account for it when writing the image out. The only way we
    // could think to do that was casting the mask value to byte :(.
    if (0==gsl_fcmp(paf, FLOAT_IMAGE_DEFAULT_MASK, 0.00000000001)) {
      pab = (unsigned char)FLOAT_IMAGE_DEFAULT_MASK;
    }
    else {
      if (paf <= gsl_histogram_min(hist)) {
	hist_bin = 0;
      }
      else if (paf >= gsl_histogram_max(hist)) {
	hist_bin = gsl_histogram_bins(hist) - 1;
      }
      else {
	gsl_histogram_find (hist, paf, &hist_bin);
      }

      double pdf_at_index = gsl_histogram_get ((gsl_histogram*)hist_pdf,
					       hist_bin);
      pab = (unsigned char)(max_brightness * pdf_at_index);
    }
    break;
  default:
    g_assert_not_reached ();
    break;
  }
  return pab;
}
