
#include <gsl/gsl_math.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_nan.h>
#include <asf_reporting.h>
#include <asf_raster.h>
#include <asf_export.h>

#define ASF_NAME_STRING "asf_export"


void
export_as_tiff (const char *metadata_file_name,
		const char *image_data_file_name,
		const char *output_file_name, long max_size, scale_t scale)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  /* Scale factor needed to satisfy max_size argument.  */
  size_t scale_factor;  
  TIFF *otif;
  /* The maximum color value for unsigned byte valued tiff images.  */
  const int max_color_value = 255;
  size_t ii, jj;		/* Index variables.  */

  asfRequire(md->general->data_type == REAL32,
             "Input data type must be in big endian 32-bit floating point "
	     "format.\n");

  /* Get the image data.  */
  asfPrintStatus ("Loading image...\n");
  const off_t start_of_file_offset = 0;
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

  /* Generate the scaled image.  */
  asfPrintStatus ("Scaling...\n");
  FloatImage *si = float_image_new_from_model_scaled (iim, scale_factor);

  /* We need a version of the data in byte form, so we have to map
     floats into bytes.  We do this by defining a region 2 sigma on
     either side of the mean to be mapped in the range of unsigned
     char linearly, and clamping everything outside this range at the
     limits of the unsigned char range.  */
  /* Make sure the unsigned char is the size we expect.  */
  asfRequire (sizeof(unsigned char) == 1,
              "Size of the unsigned char data type on this machine is "
	      "different than expected.\n");

  /* Gather some statistics to help with the mapping.  */
  float mean, standard_deviation;
  const int default_sampling_stride = 30;
  const int minimum_samples_in_direction = 10;
  int sampling_stride = GSL_MIN (default_sampling_stride, 
				 GSL_MIN (si->size_x, si->size_y) 
				 / minimum_samples_in_direction);
  float_image_approximate_statistics (si, sampling_stride, &mean, 
				      &standard_deviation);

  /* Compute the limits of the interval which will be mapped linearly
     into the byte values for output.  Values outside this interval
     will be clamped.  */
  float omin = mean - 2 * standard_deviation;
  float omax = mean + 2 * standard_deviation;

  /* We might someday want to mask out certain valus for some type of
     images, so they don't corrupt the statistics used for mapping
     floats to JSAMPLEs.  Eanbling this will require changes to the
     statistics routines and the code that does the mapping from
     floats to JSAMPLEs.  */
  /* 
   * double mask;
   * if ( md->general->image_data_type == SIGMA_IMAGE
   *      || md->general->image_data_type == GAMMA_IMAGE
   *      || md->general->image_data_type == BETA_IMAGE
   *      || strcmp(md->general->mode, "SNA") == 0.0
   *      || strcmp(md->general->mode, "SNB") == 0.0
   *      || strcmp(md->general->mode, "SWA") == 0.0
   *      || strcmp(md->general->mode, "SWB") == 0.0 )
   *   mask = 0.0;
   * else
   *   mask = NAN;
   */

  asfPrintStatus ("Writing Output File...\n");

  /* Open output tiff file */
  otif = XTIFFOpen (output_file_name, "w");
  asfRequire (otif != NULL, "Error opening output tiff file.\n");

  /* Set the normal TIFF image tags.  */
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  /* FIXME: there should be a chech here that the si->size_x and
     si->size_y fields are small enough to fit in the tiff tag they
     are being written into.  */
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, si->size_x);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, si->size_y);
  TIFFSetField(otif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField(otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField(otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(otif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(otif, TIFFTAG_ROWSPERSTRIP, 1);
  TIFFSetField(otif, TIFFTAG_XRESOLUTION, 1.0);
  TIFFSetField(otif, TIFFTAG_YRESOLUTION, 1.0);
  TIFFSetField(otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(otif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);
  TIFFSetField(otif, TIFFTAG_DATATYPE, SAMPLEFORMAT_UINT);

  /* Buffers for a line of output data.  */
  float *float_row = g_new (float, si->size_x);
  unsigned char *byte_row = g_new (unsigned char, si->size_x);

  /* Write the actual image data.  */
  for ( ii = 0 ; ii < si->size_y ; ii++ ) {
    float_image_get_row (si, ii, float_row);
    for ( jj = 0 ; jj < si->size_x ; jj++ ) {
      if ( float_row[jj] < omin ) { byte_row[jj] = 0; }
      else if ( float_row[jj] > omax ) { byte_row[jj] = max_color_value; }
      else {
	byte_row[jj] = round (((float_row[jj] - omin) / (omax - omin))
			      * max_color_value);
      }
    }
    if ( TIFFWriteScanline (otif, byte_row, ii, 0) < 0 ) {
      asfPrintError ("Error writing to output tiff file %s", output_file_name);
    }
    asfLineMeter (ii, si->size_y);
  }

  g_free (byte_row);
  g_free (float_row);

  XTIFFClose (otif);

  float_image_free (si);
 
  float_image_free (iim);

  meta_free (md);
}

