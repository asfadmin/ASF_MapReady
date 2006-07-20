#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <cla.h>
#include <envi.h>
#include <esri.h>
#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>
#include <jpeglib.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>

#define PPM_MAGIC_NUMBER "P6"

void
export_as_ppm (const char *metadata_file_name,
               const char *image_data_file_name,const char *output_file_name,
               long max_size, scale_t sample_mapping)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  /* Scale factor needed to satisfy max_size argument.  */
  size_t scale_factor;
  FILE *ofp;                    /* Output file pointer.  */
  const char *ppm_magic_number = PPM_MAGIC_NUMBER;
  int print_count;              /* For return from printf().  */
  /* The maximum color value for unsigned byte valued ppm images.  */
  const int max_color_value = 255;
  size_t ii;                    /* Index variable.  */

  asfRequire(md->general->data_type == REAL32,
             "Input data type must be in big endian 32-bit floating point "
             "format.\n");

  /* Get the image data.  */
  asfPrintStatus ("Loading image...\n");
  const off_t start_of_file_offset = 0;
  asfRequire (md->general->data_type == REAL32,
              "Input data type must be in 32-bit floating point format.\n");
  FloatImage *iim
    = float_image_new_from_file (md->general->sample_count,
                                 md->general->line_count,
                                 image_data_file_name, start_of_file_offset,
                                 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  /* We want to scale the image st the long dimension is less than or
     equal to the prescribed maximum, if any.  */
  if ( (max_size > iim->size_x && max_size > iim->size_y)
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    scale_factor = 1;
  }
  else {
    scale_factor = ceil ((double) GSL_MAX (iim->size_x, iim->size_y)
                         / max_size);
    /* The scaling code we intend to use needs odd scale factors.  */
    if ( scale_factor % 2 != 1 ) {
      scale_factor++;
    }
  }

  /* Generate the scaled version of the image, if needed.  */
  FloatImage *si;
  if ( scale_factor != 1 ) {
    asfPrintStatus ("Scaling...\n");
    si = float_image_new_from_model_scaled (iim, scale_factor);
  }
  else {
    si = iim;
  }

  /* We need a version of the data in byte form, so we have to map
     floats into bytes.  We do this by defining a region 2 sigma on
     either side of the mean to be mapped in the range of unsigned
     char linearly, and clamping everything outside this range at the
     limits of the unsigned char range.  */
  /* Make sure the unsigned char is the size we expect.  */
  asfRequire (sizeof(unsigned char) == 1,
              "Size of the unsigned char data type on this machine is "
              "different than expected.\n");

  /* Gather some statistics to help with the mapping.  Note that if
     min_sample and max_sample will actually get used for anything
     they will be set to some better values than this.  */
  float mean, standard_deviation;
  const int default_sampling_stride = 30;
  const int minimum_samples_in_direction = 10;
  int sampling_stride = GSL_MIN (default_sampling_stride,
                                 GSL_MIN (si->size_x, si->size_y)
                                 / minimum_samples_in_direction);
  float min_sample = -1.0, max_sample = -1.0;
  gsl_histogram *my_hist = NULL;
  /* Lower and upper extents of the range of float values which are to
     be mapped linearly into the output space.  */
  float omin, omax;
  get_statistics (si, sample_mapping, sampling_stride, &mean,
                  &standard_deviation, &min_sample, &max_sample, &omin, &omax,
                  &my_hist, md->general->no_data);
  asfPrintStatus ("Writing output file...\n");

  /* Open the output file to be used.  */
  ofp = fopen (output_file_name, "w");
  if ( ofp == NULL )
    asfPrintError ("Open of %s for writing failed: %s", output_file_name,
                   strerror(errno));

  /* Write the ppm header.  */
  print_count = fprintf (ofp, PPM_MAGIC_NUMBER);
  // After this we will assume that writing to the new file will work
  // correctly.
  asfRequire (print_count == strlen (ppm_magic_number),
              "Error writing to file.\n");
  fprintf (ofp, "\n");
  asfRequire (sizeof (long int) >= sizeof (size_t),
              "size of C type 'long int' less than size of C type 'size_t'");
  fprintf (ofp, "%ld\n", (long int) si->size_x);
  fprintf (ofp, "%ld\n", (long int) si->size_y);
  fprintf (ofp, "%d\n", max_color_value);

  // Stuff for histogram equalization
  gsl_histogram_pdf *my_hist_pdf = NULL;
  if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
    my_hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
    gsl_histogram_pdf_init (my_hist_pdf, my_hist);
  }
  /* Write the pixels themselves.  */
  for ( ii = 0 ; ii < si->size_y ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < si->size_x ; jj++ ) {
      double paf = float_image_get_pixel (si, jj, ii); /* Pixels as float.  */
      unsigned char pab = pixel_float2byte(paf, sample_mapping, omin, omax,
                                           my_hist, my_hist_pdf,
                                           md->general->no_data);
      /* Write red, green, and blue the same to get grey scale.  */
      fwrite (&pab, 1, 1, ofp);
      fwrite (&pab, 1, 1, ofp);
      fwrite (&pab, 1, 1, ofp);
    }
    asfLineMeter(ii, si->size_y);
  }

  FCLOSE (ofp);

  // If the scale factor wasn't one, the scaled version of the image
  // will be different from the original and so will need to be freed
  // seperately.
  if ( si != iim) {
    float_image_free (si);
  }

  float_image_free (iim);

  meta_free (md);
}
