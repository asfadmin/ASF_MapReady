// Geocode a DEM stored in ASF .meta format.

// Standard libraries.
#include <math.h>
#include <string.h>

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

// Return true iff (x, y) falls in the region
// ([0, image->size_x - 1), [0, image->size_y - 1))
static int
in_image (FloatImage *image, ssize_t x, ssize_t y)
{
  g_assert (image->size_x <= SSIZE_MAX && image->size_y <= SSIZE_MAX);
  ssize_t sz_x = image->size_x, sz_y = image->size_y;

  return 0 <= x && x < sz_x && 0 <= y && y < sz_y;
}

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

  // FIXME: what to do with background value is something that still
  // needs to be determined (probably in consultation with the guys
  // working on terrain correction).
  const float background_value = 0.0;

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

  // In going around the edge, we are just trying to determine the
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

  // Output image dimensions.
  size_t oi_size_x = oix_max + 1;
  size_t oi_size_y = oiy_max + 1;

  // Output image.
  FloatImage *oim = float_image_new (oi_size_x, oi_size_y);

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

  // We need to find the z coordinates in the output projection of all
  // the pixels in the input DEM.  We store these values in their own
  // FloatImage instance.

  //FloatImage *x_coords = float_image_new (ii_size_x, ii_size_y);
  //FloatImage *y_coords = float_image_new (ii_size_x, ii_size_y);
  FloatImage *z_coords = float_image_new (ii_size_x, ii_size_y);

  // We transform the points using the array transformation function
  // for efficiency, but we don't want to do them all at once, since
  // that would require huge gobs of memory.
  const size_t max_transform_chunk_pixels = 5000000;
  size_t rows_per_chunk = max_transform_chunk_pixels / ii_size_x;
  size_t chunk_pixels = rows_per_chunk * ii_size_x;
  double *chunk_x = g_new (double, chunk_pixels);
  double *chunk_y = g_new (double, chunk_pixels);
  double *chunk_z = g_new (double, chunk_pixels);
  double *lat = g_new (double, chunk_pixels);
  double *lon = g_new (double, chunk_pixels);
  double *height = g_new (double, chunk_pixels);

  // Transform all the chunks, storing results in the z coordinate image.
  size_t ii, jj, kk;		// Index variables.
  for ( ii = 0 ; ii < ii_size_y ; ) {
    size_t rows_remaining = ii_size_y - ii;
    size_t rows_to_load
      = rows_per_chunk < rows_remaining ? rows_per_chunk : rows_remaining;
    for ( jj = 0 ; jj < rows_to_load ; jj++ ) {
      for ( kk = 0 ; kk < ii_size_x ; kk++ ) {
	size_t current_chunk_pixel = jj * ii_size_x + kk;
	size_t current_image_row = ii * rows_per_chunk + jj;
	chunk_x[current_chunk_pixel] = ipb->startX + kk * ipb->perX;
	chunk_y[current_chunk_pixel] 
	  = ipb->startY + current_image_row * ipb->perY;
	chunk_z[current_chunk_pixel]
	  = float_image_get_pixel (iim, kk, current_image_row);
      }
    }
    long current_chunk_pixels = rows_to_load * ii_size_x;
    project_set_datum (ipb->datum);
    array_unproject_input (ipp, chunk_x, chunk_y, chunk_z, &lat, &lon,
			   &height, current_chunk_pixels);
    project_set_datum (datum);
    array_project_output (pp, lat, lon, height, &chunk_x, &chunk_y, &chunk_z,
			  current_chunk_pixels);
    for ( jj = 0 ; jj < rows_to_load ; jj++ ) {
      for ( kk = 0 ; kk < ii_size_x ; kk++ ) {
	size_t current_chunk_pixel = jj * ii_size_x + kk;
	size_t current_image_row = ii * rows_per_chunk + jj;
	// Current pixel x, y, z coordinates.
	//float cp_x = (float) chunk_x[current_chunk_pixel];
	//float cp_y = (float) chunk_y[current_chunk_pixel];
	float cp_z = (float) chunk_z[current_chunk_pixel];
	//float_image_set_pixel (x_coords, kk, current_image_row, cp_x);
	//float_image_set_pixel (y_coords, kk, current_image_row, cp_y);
	float_image_set_pixel (z_coords, kk, current_image_row, cp_z);
      }
    }

    ii += rows_to_load;
  }

  g_free (chunk_x);
  g_free (chunk_y);
  g_free (chunk_z);
  g_free (lat);
  g_free (lon);
  g_free (height);

  // Now we want to determine the pixel coordinates in the input which
  // correspond to each of the output pixels.  We can then sample the
  // new height value already computed for that input pixel to
  // determine the pixel value to use as output.

  // We want to proceed in chunks as we did when going in the other
  // direction.
  rows_per_chunk = max_transform_chunk_pixels / oi_size_x;
  chunk_pixels = rows_per_chunk * oi_size_x;
  chunk_x = g_new (double, chunk_pixels);
  chunk_y = g_new (double, chunk_pixels);
  // We don't have height information in this direction, nor do we care.
  chunk_z = NULL;
  lat = g_new (double, chunk_pixels);
  lon = g_new (double, chunk_pixels);
  // We don't have height information in this direction, nor do we care.
  height = NULL;

  // Transform all the chunks, using the results to form the output image.
  for ( ii = 0 ; ii < oi_size_y ; ) {
    size_t rows_remaining = oi_size_y - ii;
    size_t rows_to_load
      = rows_per_chunk < rows_remaining ? rows_per_chunk : rows_remaining;
    for ( jj = 0 ; jj < rows_to_load ; jj++ ) {
      for ( kk = 0 ; kk < oi_size_x ; kk++ ) {
	size_t current_chunk_pixel = jj * oi_size_x + kk;
	size_t current_image_row = ii * rows_per_chunk + jj;
	chunk_x[current_chunk_pixel] = min_x + kk * pixel_size;
	chunk_y[current_chunk_pixel] = max_y - current_image_row * pixel_size;
      }
    }
    long current_chunk_pixels = rows_to_load * ii_size_x;
    project_set_datum (datum);
    array_unproject_input (pp, chunk_x, chunk_y, NULL, &lat, &lon, NULL,
			   current_chunk_pixels);
    project_set_datum (ipb->datum);
    array_project_output (ipp, lat, lon, NULL, &chunk_x, &chunk_y, NULL,
			  current_chunk_pixels);
    for ( jj = 0 ; jj < rows_to_load ; jj++ ) {
      for ( kk = 0 ; kk < oi_size_x ; kk++ ) {
	size_t current_chunk_pixel = jj * oi_size_x + kk;
	size_t current_image_row = ii * rows_per_chunk + jj;

	// Compute pixel coordinates in input image.
	ssize_t in_x 
	  = (chunk_x[current_chunk_pixel] - ipb->startX) / ipb->perX;
	ssize_t in_y
	  = -(chunk_y[current_chunk_pixel] - ipb->startY) / ipb->perY;

	if ( in_image (z_coords, in_x, in_y) ) {
	  // FIXME: something needs to be done somewhere about
	  // propogating no data values.
	  float_image_set_pixel (oim, kk, current_image_row,
				 float_image_sample (z_coords, in_x, in_y,
						     resample_method));
	}
	else {
	  float_image_set_pixel (oim, kk, current_image_row, background_value);
	}
      }
    }

    ii += rows_to_load;
  }

  g_free (chunk_x);
  g_free (chunk_y);
  g_free (lat);
  g_free (lon);


  // Store the output image, and free image resources.
  GString *output_data_file = g_string_new (output_image->str);
  g_string_append (output_data_file, ".img");
  return_code = float_image_store (oim, output_data_file->str,
				   FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_assert (return_code == 0);
  float_image_free (oim);
  g_string_free (output_data_file, TRUE);

  // Now we need some metadata for the output image.  We will just
  // start with the metadata from the input image and add the
  // geocoding parameters.

  GString *input_meta_file = g_string_new (input_image->str);
  g_string_append (input_meta_file, ".meta");

  GString *output_meta_file = g_string_new (output_image->str);
  g_string_append (output_meta_file, ".meta");

  meta_parameters *omd = meta_read (input_meta_file->str);

  // Adjust the metadata to correspond to the output image instead of
  // the input image.

  omd->general->x_pixel_size = pixel_size;
  omd->general->y_pixel_size = pixel_size;
  omd->general->line_count = oi_size_x;
  omd->general->sample_count = oi_size_y;  

  // SAR block is not really appropriate for map projected images, but
  // since it ended up with this value that can signify map
  // projectedness in it somehow, we fill it in for safety.
  omd->sar->image_type = 'P';

  // Note that we have already verified that the input image is
  // projected, and since we initialize the output metadata from there
  // we know we will have a projection block.
  omd->projection->type = projection_type;
  omd->projection->startX = min_x;
  omd->projection->startY = max_y;
  omd->projection->perX = pixel_size;
  omd->projection->perY = -pixel_size;
  strcpy (omd->projection->units, "meters");

  // Set the spheroid axes lengths as appropriate for the output datum.
  spheroid_axes_lengths (datum_spheroid (datum), &(omd->projection->re_major),
			 &(omd->projection->re_minor));

  // What the heck, might as well set the ones in the general block as
  // well.
  spheroid_axes_lengths (datum_spheroid (datum), &(omd->general->re_major),
			 &(omd->general->re_minor));

  // Latitude and longitude at center of the output image.  We will
  // set these relative to the spheroid underlying the datum in use
  // for the projected image.  Yeah, that seems appropriate.
  double lat_0, lon_0;
  project_set_input_spheroid (datum_spheroid (datum));
  double center_x = omd->projection->startX + (omd->projection->perX
					       * omd->general->line_count / 2);
  double center_y = (omd->projection->startY
		     + (omd->projection->perY
			* omd->general->sample_count / 2));
  unproject_output (pp, center_x, center_y, ASF_PROJ_NO_HEIGHT, &lat_0, &lon_0,
		    NULL);
  omd->general->center_latitude = lat_0;
  omd->general->center_longitude = lon_0;

  // FIXME: We are ignoring the meta_location fields for now since I'm
  // not sure whether they are supposed to refer to the corner pixels
  // or the corners of the data itself.

  if ( lat_0 > 0.0 ) {
    omd->projection->hem = 'N';
  }
  else {
    omd->projection->hem = 'S';
  }

  // Convert the projection parameter values back into degrees.
  to_degrees (projection_type, pp);
  omd->projection->param = *pp;
  meta_write (omd, output_meta_file->str);
  meta_free (omd);

  g_assert (0);			/* Unfinished.  */

  return 0;
}
