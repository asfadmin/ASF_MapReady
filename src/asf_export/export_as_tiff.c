
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
  unsigned int line_count = md->general->line_count;
  unsigned int sample_count = md->general->sample_count;
  unsigned long width, height, max_large_dimension;
  size_t pixel_count = line_count * sample_count;
  float *daf;
  unsigned char *pixels;
  double mask;
  int jj;
  TIFF *otif;
  size_t ii;

  /* Get the image data.  */
  asfRequire (md->general->data_type == REAL32,
             "Can only ingest ASF format floating point data.");
  daf = get_image_data (md, image_data_file_name);

  asfPrintStatus ("Processing...\n");

  /* It supposed to be big endian data, this converts to host byte
     order.  */
  for ( jj = 0 ; jj < pixel_count ; jj++ ) {
    ieee_big32 (daf[jj]);
  }

  /* Open output tiff file */
  otif = XTIFFOpen (output_file_name, "w");
  asfRequire (otif != NULL, "Error opening output tiff file.\n");

  /* Scale float image down to bytes */
  if ( md->general->image_data_type == SIGMA_IMAGE ||
       md->general->image_data_type == GAMMA_IMAGE ||
       md->general->image_data_type == BETA_IMAGE ||
       strcmp(md->general->mode, "SNA") == 0 ||
       strcmp(md->general->mode, "SNB") == 0 ||
       strcmp(md->general->mode, "SWA") == 0 ||
       strcmp(md->general->mode, "SWB") == 0 ) {
    mask = 0.0;
  }
  else {
    mask = NAN;
  }

  asfPrintStatus ("Scaling...\n");
  pixels = floats_to_bytes (daf, pixel_count, mask, scale);

  /* Scale the image, modifying width and height to reflect the new
  image size.  */
  width = sample_count;
  height = line_count;
  if ( (max_size > line_count && max_size > sample_count)
       || max_size == NO_MAXIMUM_OUTPUT_SIZE ) {
    max_large_dimension = GSL_MAX (line_count, sample_count);
  }
  else {
    max_large_dimension = max_size;
  }
  pixels = scale_unsigned_char_image_dimensions (pixels, max_large_dimension,
                                                 &width, &height);

  /* Set the normal TIFF image tags.  */
  TIFFSetField(otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(otif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(otif, TIFFTAG_IMAGELENGTH, height);
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

  /* Write the actual image data.  */
  asfPrintStatus ("Writing Output File...\n");
  for ( ii = 0 ; ii < height ; ii++ ) {
    if ( TIFFWriteScanline (otif, pixels + width * ii, ii, 0) < 0 ) {
      asfPrintError ("Error writing to output tiff file %s", output_file_name);
    }
    asfLineMeter (ii, height);
  }

  XTIFFClose (otif);
  free (daf);
  meta_free (md);
}

