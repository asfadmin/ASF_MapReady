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
#include "asf_nan.h"
#include "asf_reporting.h"
#include <asf_export.h>
#include "asf_raster.h"

void
export_as_jpeg (const char *metadata_file_name,
                const char *image_data_file_name, const char *output_file_name,
                long max_size, scale_t scale)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  /* Maximum large dimension to allow in the output.  */
  unsigned long max_large_dimension;
  size_t pixel_count;
  float *daf;
  int jj;
  JSAMPLE test_jsample;
  unsigned char *pixels;
  double mask;
  unsigned long width, height;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  FILE *ofp;
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

  asfPrintStatus("Processing...\n");

  /* It supposed to be big endian data, this converts to host byte
     order.  */
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* We need a version of the data in JSAMPLE form, so we have to
     form a scaled version of the input data.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  assert (sizeof (unsigned char) == 1);
  assert (sizeof (unsigned char) == sizeof (JSAMPLE));
  test_jsample = 0;
  test_jsample--;
  assert (test_jsample == UCHAR_MAX); /* Did we wrap?  */
  /* This space is resized later (with realloc) if the image is
     scaled.  */
  if (md->general->image_data_type == SIGMA_IMAGE ||
      md->general->image_data_type == GAMMA_IMAGE ||
      md->general->image_data_type == BETA_IMAGE ||
      strcmp(md->general->mode, "SNA") == 0 ||
      strcmp(md->general->mode, "SNB") == 0 ||
      strcmp(md->general->mode, "SWA") == 0 ||
      strcmp(md->general->mode, "SWB") == 0)
    mask = 0.0;
  else
    mask = NAN;

  asfPrintStatus("Scaling...\n");
  pixels = floats_to_bytes (daf, pixel_count, mask, scale);

  /* We want to scale the image st the long dimesion is less than or
     equal to this value the prescribed maximum.
     Current size of the image.  */
  width = sample_count;
  height = line_count;
  /* Scale the image, modifying width and height to reflect the new
  image size.  */

  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
                                                 &width, &height);

  /* Initializae libjpg structures.  */
  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_compress (&cinfo);

  asfPrintStatus("Writing Output File...\n");

  /* Open the output file to be used.  */
  ofp = fopen (output_file_name, "w");
  if ( ofp == NULL ) {
    char *temp;
        sprintf(temp, "Open of %s for writing failed: %s", 
		output_file_name, strerror(errno));
        print_error(temp);
    exit (EXIT_FAILURE);
  }

  /* Connect jpeg output to the output file to be used.  */
  jpeg_stdio_dest (&cinfo, ofp);

  /* Set image parameters that libjpeg needs to know about.  */
  cinfo.image_width = width;
  cinfo.image_height = height;
  cinfo.input_components = 1;   /* Grey scale => 1 color component / pixel.  */
  cinfo.in_color_space = JCS_GRAYSCALE;
  jpeg_set_defaults (&cinfo);   /* Use default compression parameters.  */
  /* Reassure libjpeg that we will be writing a complete JPEG file.  */
  jpeg_start_compress (&cinfo, TRUE);

  /* Write the jpeg.  */
  while ( cinfo.next_scanline < cinfo.image_height ) {
    /* We are writing one row at a time.  */
    const int rows_to_write = 1;
    int rows_written;
    JSAMPROW *row_pointer = MALLOC (rows_to_write * sizeof (JSAMPROW));
    row_pointer[0] = &(pixels[cinfo.next_scanline * width]);
    rows_written = jpeg_write_scanlines (&cinfo, row_pointer, rows_to_write);
    assert (rows_written == rows_to_write);
    asfLineMeter(cinfo.next_scanline, cinfo.image_height);
  }

  /* Finsh compression and close the jpeg.  */
  jpeg_finish_compress (&cinfo);
  return_code = fclose (ofp);
  assert (return_code == 0);
  jpeg_destroy_compress (&cinfo);

  free (pixels);
  free (daf);
  meta_free (md);
}

