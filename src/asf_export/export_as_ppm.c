#include <assert.h>
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
/*#include <proj_api.h>*/
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
               const char *image_data_file_name, const char *output_file_name,
               long max_size)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  /* Get image dimensions.  */
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  size_t pixel_count;
  float *daf;
  int jj;
  unsigned char *pixels;
  unsigned long width,height;
  FILE *ofp;
  const char *ppm_magic_number = PPM_MAGIC_NUMBER;
  int print_count;
  const int max_color_value = 255;
  size_t ii;
  int return_code;

  assert (md->general->data_type == REAL32);

  if ( (max_size > line_count && max_size > sample_count)
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  }
  else {
    max_large_dimension = max_size;
  }

  pixel_count = (size_t) line_count * sample_count;

  /* Get the image data.  */
  assert (md->general->data_type == REAL32);
  daf = get_image_data (md, image_data_file_name);
  /* Input is supposed to be big endian data, this converts to host
     byte order.  */
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* We need a version of the data in unsigned byte form, so we have
     to form a scaled version of the input data.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  assert (sizeof (unsigned char) == 1);

  /* This pixel space is resized later (with realloc) if the image
     dimensions are scaled.  */
  pixels = scale_floats_to_unsigned_bytes (daf, pixel_count);

  /* We want to scale the image st the long dimension is less than or
     equal to the prescribed maximum.  */
  /* Current size of the image.  */
  width = sample_count;
  height = line_count;
  /* Scale the image, modifying width and height to reflect the new
     image size.  */
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
                                                 &width, &height);

  /* Open the output file to be used.  */
  ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    char* temp;
        sprintf(temp, "Open of %s for writing failed: %s", output_file_name, strerror(errno));
        print_error(temp);
    exit (EXIT_FAILURE);
  }

  /* Write the ppm header.  */
  print_count = fprintf (ofp, PPM_MAGIC_NUMBER);
  /* After this we will assume that writing to the new file will work
     correctly.  */
  assert (print_count == strlen (ppm_magic_number));
  fprintf (ofp, "\n");
  fprintf (ofp, "%ld\n", width);
  fprintf (ofp, "%ld\n", height);
  fprintf (ofp, "%d\n", max_color_value);

  /* Write the pixels themselves.  */
  for ( ii = 0 ; ii < height ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < width ; jj++ ) {
      /* Write red, green, and blue the same to get grey scale.  */
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
      fwrite (&pixels[ii * width + jj], 1, 1, ofp);
    }
  }

  return_code = fclose (ofp);
  assert (return_code == 0);

  free (pixels);
  free (daf);
  meta_free (md);
}
