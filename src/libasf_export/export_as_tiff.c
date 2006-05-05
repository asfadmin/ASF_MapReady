
#include <gsl/gsl_math.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_reporting.h>
#include <asf_raster.h>
#include <asf_export.h>

#define ASF_NAME_STRING "asf_export"


void
export_as_tiff (const char *metadata_file_name,
                const char *image_data_file_name,
                const char *output_file_name, long max_size,
                scale_t sample_mapping)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  /* Scale factor needed to satisfy max_size argument.  */
  size_t scale_factor;
  TIFF *otif;
  size_t ii, jj;                /* Index variables.  */

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
  /* Lower and upper extents of the range of float values which are to
     be mapped linearly into the output space.  */
  float omin, omax;
  gsl_histogram *my_hist = NULL;
  get_statistics (si, sample_mapping, sampling_stride, &mean,
                  &standard_deviation, &min_sample, &max_sample, &omin, &omax,
                  &my_hist);

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

  // Stuff for histogram equalization
  gsl_histogram_pdf *my_hist_pdf = NULL;
  if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
    my_hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
    gsl_histogram_pdf_init (my_hist_pdf, my_hist);
  }

  /* Write the actual image data.  */
  for ( ii = 0 ; ii < si->size_y ; ii++ ) {
    float_image_get_row (si, ii, float_row);
    for ( jj = 0 ; jj < si->size_x ; jj++ ) {
      byte_row[jj] = pixel_float2byte(float_row[jj], sample_mapping, omin,
                                      omax, my_hist, my_hist_pdf);
    }
    if ( TIFFWriteScanline (otif, byte_row, ii, 0) < 0 ) {
      asfPrintError ("Error writing to output tiff file %s", output_file_name);
    }
    asfLineMeter (ii, si->size_y);
  }

  g_free (byte_row);
  g_free (float_row);

  XTIFFClose (otif);

  // If the scale factor wasn't one, the scaled version of the image
  // will be different from the original and so will need to be freed
  // seperately.
  if ( si != iim) {
    float_image_free (si);
  }

  float_image_free (iim);

  meta_free (md);
}

