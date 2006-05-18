// Geocode a DEM stored in ASF .meta format.

// Standard libraries.
#include <math.h>

// Libraries from packages outside ASF.
#include <glib.h>

// Libraries developed at ASF.
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_reporting.h>
#include <float_image.h>
#include <libasf_proj.h>

// Headers defined by this library.
#include "asf_geocode.h"

int
geocode_dem (projection_type_t projection_type,	// What we are projection to.
	     project_parameters_t *pp,    // Parameters we project to.
	     datum_type_t datum,                // Datum we project to.
	     // Pixel size of output image, in output projection units
	     // (meters or possibly degrees, if we decide to support
	     // projecting to pseudoprojected form).
	     double pixel_size,                
	     resample_method_t resample_method,	// How to resample pixels.
	     const GString *input_image, // Base name of input image.
	     const meta_parameters *imd, // Input DEM image metadata.
	     const GString *output_image  // Base name of output image.
	     )
{
  int return_code;		// Holds return codes from functions.

  // Function to use to project or unproject between latlon and input
  // or output coordinates.
  projector_t project_input; 	// latlon => input image map projection
  projector_t unproject_input;	// input image_map_projection => latlon
  projector_t project_output;	// latlon => output image map projection
  projector_t unproject_output;	// output image map projection => latlon
  // Like the above, but act on arrays.
  array_projector_t array_project_input, array_unproject_input;
  array_projector_t array_project_output, array_unproject_output;

  // We only deal with reprojection map projected DEMs.
  g_assert (imd->projection != NULL);

  // At the moment the projector_t and array_projector_t function that
  // "transform" pseudoprojection coordinates (i.e. lat/lons) into lat
  // lons don't perform any datum transformations.  They should
  // probably be changed so that they do so, renamed to fit the naming
  // scheme libasf_proj uses, and moved into libasf_proj itself.  But
  // until these things happen we have a couple of consequences:
  // 
  //    1. When reprojecting a pseudoprojected image, we must not
  //       perform any datum transformations as we go from input
  //       coordinates to lat/lon.  Since we handle pseudoprojected
  //       images generically together with all other types, this means
  //       that we must not do any datum transformations on any image
  //       types during this stage.
  //
  //    2. The implication of 1. above is that we must therefor
  //       perform any desired datum transformation during the stage
  //       where we reproject from lat/lon to the desired output
  //       projection.  But if we are trying to convert back to
  //       pseudoprojected form we're out of luck, since there is no
  //       way to preform any required datum transformation.  So we
  //       must forbid output in pseudoprojected form.
  // FIXME: the above now seems unlikely to work.  The solution is to
  // put the pseudoprojection function in libasf_proj with the correct
  // datum transformation behavior.
  g_assert (projection_type != LAT_LONG_PSEUDO_PROJECTION);

  // Get the functions we want to use for projecting and unprojecting.
  set_projection_functions (imd->projection->type, &project_input,
			    &unproject_input, &array_project_input,
			    &array_unproject_input);
  set_projection_functions (projection_type, &project_output,
			    &unproject_output, &array_project_output,
			    &array_unproject_output);

  // Input image dimensions in pixels in x and y directions.
  size_t ii_size_x = imd->general->sample_count;
  size_t ii_size_y = imd->general->line_count;

  // Convenience aliases.
  meta_projection *ipb = imd->projection;
  project_parameters_t *ipp = &imd->projection->param;

  // First we march around the entire outside of the image and compute
  // projection coordinates for every pixel, keeping track of the
  // minimum and maximum projection coordinates in each dimension.
  // This lets us determine the exact extent of the DEM in
  // output projection coordinates.
  asfPrintStatus ("Determining input image extent in projection coordinate "
		  "space... ");

  double min_x = DBL_MAX;
  double max_x = -DBL_MAX;
  double min_y = DBL_MAX;
  double max_y = -DBL_MAX;

  // In going around the edge, we are just tryin gto determine the
  // extent of the image in the horizontal, so we don't care about
  // height yet.

  { // Scoping block.
    // Number of pixels in the edge of the image.
    size_t edge_point_count = 2 * ii_size_x + 2 * ii_size_y - 4;
    double *lats = g_new (double, edge_point_count);
    double *lons = g_new (double, edge_point_count);
    size_t current_edge_point = 0;
    size_t ii = 0, jj = 0;
    for ( ; ii < ii_size_x - 1 ; ii++ ) {
      double xpc = ipb->startX + ipb->perX * ii;
      double ypc = ipb->startY - ipb->perY * jj;
      project_set_datum (imd->projection->datum);
      return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				     &(lats[current_edge_point]),
				     &(lons[current_edge_point]), NULL);
      g_assert (return_code);
      current_edge_point++;
    }
    for ( ; jj < ii_size_y - 1 ; jj++ ) {
      double xpc = ipb->startX + ipb->perX * ii;
      double ypc = ipb->startY - ipb->perY * jj;
      project_set_datum (imd->projection->datum);
      return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				     &(lats[current_edge_point]),
				     &(lons[current_edge_point]), NULL);
	g_assert (return_code);
      current_edge_point++;
    }
    for ( ; ii > 0 ; ii-- ) {
      double xpc = ipb->startX + ipb->perX * ii;
      double ypc = ipb->startY - ipb->perY * jj;
      project_set_datum (imd->projection->datum);
      return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				     &(lats[current_edge_point]),
				     &(lons[current_edge_point]), NULL);
      g_assert (return_code);
      current_edge_point++;
    }
    for ( ; jj > 0 ; jj-- ) {
      double xpc = ipb->startX + ipb->perX * ii;
      double ypc = ipb->startY - ipb->perY * jj;
      project_set_datum (imd->projection->datum); 
      return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				     &(lats[current_edge_point]),
				     &(lons[current_edge_point]), NULL);
      g_assert (return_code);
      current_edge_point++;
    }
    g_assert (current_edge_point == edge_point_count);
    // Pointers to arrays of projected coordinates to be filled in.
    // The projection function will allocate this memory itself.
    double *x = NULL, *y = NULL;
    // Project all the edge pixels.
    project_set_datum (datum);
    return_code = array_project_output (pp, lats, lons, NULL, &x, &y, NULL,
					edge_point_count);
    g_assert (return_code == TRUE);
    // Find the extents of the image in projection coordinates.
    for ( ii = 0 ; ii < edge_point_count ; ii++ ) {
      if ( x[ii] < min_x ) { min_x = x[ii]; }
      if ( x[ii] > max_x ) { max_x = x[ii]; }
      if ( y[ii] < min_y ) { min_y = y[ii]; }
      if ( y[ii] > max_y ) { max_y = y[ii]; }
    }

    free (y);
    free (x);
    g_free (lons);
    g_free (lats);
  }

  // Open the input image data file.
  GString *input_data_file = g_string_new (input_image->str);
  g_string_append (input_data_file, ".img");
  FloatImage *iim
    = float_image_new_from_file (ii_size_x, ii_size_y, input_data_file->str, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_string_free (input_data_file, TRUE);

  // Maximum pixel indicies in output image.
  size_t oix_max = ceil ((max_x - min_x) / pixel_size);
  size_t oiy_max = ceil ((max_y - min_y) / pixel_size);

  // Output image.
  FloatImage *oim = float_image_new (oix_max + 1, oiy_max + 1);

  // Convenience macros for getting a pixel.
#define SET_PIXEL(x, y, value) float_image_set_pixel (oim, x, y, value)

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

  // Set the pixels of the output image.
  g_assert (0);			/* Unfinished.  */
  oim = oim; iim = iim;		/* FIXME: remove this compiler reassurance.  */
  output_image = output_image;	/* FIXME: remove this compiler reassurance.  */

  return 0;
}
