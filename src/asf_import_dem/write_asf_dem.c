#include <glib.h>

#include <libasf_proj.h>
#include <asf_raster.h>
#include <float_image.h>
#include <spheroids.h>

#include "geocode_options.h"
#include "parse_options.h"
#include "asf_import_dem.h"
#include "seamless_meta.h"



void
write_asf_dem ( seamless_meta_t *smeta,
                data_to_fit_t *dtf,
                double pixel_size,
                project_parameters_t *outProjPrms,
                projection_type_t projection_type,
                resample_method_t resample_method,
                char *inImageName,
                char *outImageName,
                char *outMetaName,
                double min_x, double max_x, double min_y, double max_y)
{
  // Now we are ready to produce our output image.
  asfPrintStatus ("Resampling input image into output image "
                  "coordinate space...\n");

  int ncols = seamless_meta_get_ncols(smeta);
  int nrows = seamless_meta_get_nrows(smeta);

  // Projection coordinates per pixel in output image.  There is a
  // significant assumption being made here: we assume that the
  // projection coordinates (which are in meters) come at least
  // reasonably close to the ground distances.  This way, when we set
  // the projection coordinates per pixel in the output image equal to
  // the pixel size of the input image, we should be resampling at
  // close to one-to-one (which is where resampling works and we don't
  // have to worry about pixel averaging or anything).
  double proj_coord_per_x = pixel_size;
  double proj_coord_per_y = pixel_size;

  // Maximum pixel indicies in output image.
  int out_x_max_index = ceil ((max_x - min_x) / proj_coord_per_x);
  int out_y_max_index = ceil ((max_y - min_y) / proj_coord_per_y);

  // Input gridFloat image
  float_image_byte_order_t inByteOrder;
  if (strcmp("LSBFIRST",seamless_meta_get_byteorder(smeta))==0) {
    inByteOrder = FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN;
  }
  else if (strcmp("MSBFIRST",seamless_meta_get_byteorder(smeta))==0) {
    inByteOrder = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;
  }
  else {
    asfPrintError("Unknown byte order encountered.\n");
  }
  FloatImage *inImage = float_image_new_from_file (ncols, nrows,
                                                  inImageName, 0, inByteOrder);

  // Output image
  FloatImage *outImage = float_image_new (out_x_max_index + 1,
                                          out_y_max_index + 1);

  // Translate the command line notion of the resampling method into
  // the lingo known by the float_image class.  The compiler is
  // reassured with a default.
  float_image_sample_method_t float_image_sample_method
                                          = FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
  switch ( resample_method ) {
  case RESAMPLE_NEAREST_NEIGHBOR:
    float_image_sample_method = FLOAT_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR;
    break;
  case RESAMPLE_BILINEAR:
    float_image_sample_method = FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
    break;
  case RESAMPLE_BICUBIC:
    float_image_sample_method = FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC;
    break;
  default:
    g_assert_not_reached ();
  }
  // Convenience macros for getting a pixel.
#define SET_PIXEL(x, y, value) float_image_set_pixel (outImage, x, y, value)
#define IN_IMAGE_SAMPLE float_image_sample ( \
              inImage, input_x_index, input_y_index, float_image_sample_method)

  // Set the pixels of the output image.
  int out_x_index, out_y_index;              // Output image pixel indicies.
  for ( out_y_index = 0 ; out_y_index <= out_y_max_index ; out_y_index++ ) {
    for ( out_x_index = 0 ; out_x_index <= out_x_max_index ; out_x_index++ ) {
      // Projection coordinates for the center of this pixel.
      double out_x_index_pc = ((double) out_x_index / (double)out_x_max_index)
                               * (max_x - min_x) + min_x;
      // We want projection coordinates to increase as we move from
      // the bottom of the image to the top, so that north ends up up.
      double out_y_index_pc = (1.0 - (double) out_y_index / (double)out_y_max_index)
                              * (max_y - min_y) + min_y;
      // Determine pixel of interest in input image.  The fractional
      // part is desired, we will use some sampling method to
      // interpolate between pixel values.
      double input_x_index = X_PIXEL (out_x_index_pc, out_y_index_pc);
      double input_y_index = Y_PIXEL (out_x_index_pc, out_y_index_pc);
      // If we are outside the extent of the input image, set to the
      // fill value.
      const float fill_value = 0.0;
      if (    input_x_index < 0
           || input_x_index > ncols - 1.0
           || input_y_index < 0
           || input_y_index > nrows - 1.0 ) {
        SET_PIXEL (out_x_index, out_y_index, (float) fill_value);
      }
      // Otherwise, set to the value from the appropriate position in
      // the input image.
      else {
        SET_PIXEL (out_x_index, out_y_index, IN_IMAGE_SAMPLE);
      }
    }
    asfLineMeter(out_y_index, out_y_max_index + 1 );
  }
  asfPrintStatus ("\nDone resampling image.\n");

  float_image_free (inImage);

  // Store the output image, and free image resources.
  asfPrintStatus ("Storing geocoded image...\n");
  int return_code = float_image_store (outImage, outImageName,
                                       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  asfPrintStatus ("\nDone storing geocoded image.\n\n");
  asfRequire (return_code == 0, "Error saving image.\n");
  float_image_free (outImage);

  // Now we need some metadata for the output image.
  const double wgs84_semiminor_axis
    = WGS84_SEMIMAJOR * (1 - (1 / WGS84_INV_FLATTENING));
  meta_parameters *outMeta = raw_init();
  outMeta->general->data_type = REAL32;
  outMeta->general->image_data_type = DEM;
  outMeta->general->band_number = 0;
  outMeta->general->line_count = out_y_max_index + 1;
  outMeta->general->sample_count = out_x_max_index + 1;
  outMeta->general->start_line = 0;
  outMeta->general->start_sample = 0;
  outMeta->general->x_pixel_size = proj_coord_per_x;
  outMeta->general->y_pixel_size = proj_coord_per_y;
  outMeta->general->center_latitude = seamless_meta_get_yllcorner(smeta)
                             + (nrows * seamless_meta_get_cellsize(smeta) / 2);
  outMeta->general->center_longitude = seamless_meta_get_xllcorner(smeta)
                             + (ncols * seamless_meta_get_cellsize(smeta) / 2);
  outMeta->sar->image_type = 'P';

  outMeta->projection = MALLOC(sizeof(meta_projection));
  memset (outMeta->projection, 0, sizeof(meta_projection));
  outMeta->projection->type = projection_type;
  outMeta->projection->startX = min_x;
  outMeta->projection->startY = max_y;
  outMeta->projection->perX = proj_coord_per_x;
  outMeta->projection->perY = proj_coord_per_y;
  strcpy (outMeta->projection->units, "meters");
  outMeta->projection->hem =
                      ( seamless_meta_get_yllcorner(smeta) > 0.0 ) ? 'N' : 'S';
  outMeta->projection->spheroid = WGS84_SPHEROID;
  outMeta->projection->re_major = WGS84_SEMIMAJOR;
  outMeta->projection->re_minor = wgs84_semiminor_axis;
  outMeta->projection->datum = WGS84_DATUM;
  to_degrees (projection_type, outProjPrms);
  outMeta->projection->param = *outProjPrms;

  meta_write (outMeta, outMetaName);
  meta_free (outMeta);

  // Done with the data being modeled.  Can't call revers_map_*
  // functions anymore after this (so can't use *_PIXEL macros
  // either).
  data_to_fit_destroy(dtf);
}
