// Implementation of the interface described in map_projected_dem.h.

#include <math.h>
#include <string.h>
#include <gsl/gsl_math.h>
#include <ddr.h>
#include <libasf_proj.h>
#include "map_projected_dem.h"
#include <asf_reporting.h>

MapProjectedDEM *
map_projected_dem_new_from_las (const char *las_header_file,
                                const char *las_data_file)
{
  MapProjectedDEM *self = g_new (MapProjectedDEM, 1);

  struct DDR ddr;
  lasErr error_code = c_getddr (las_header_file, &ddr);
  g_assert (error_code == 0);

  self->size_x = ddr.ns;
  self->size_y = ddr.nl;
  self->upper_left_x = ddr.upleft[1];
  self->upper_left_y = ddr.upleft[0];
  self->projection_coordinates_per_x_pixel = ddr.pdist_x;
  self->projection_coordinates_per_y_pixel = ddr.pdist_y;

  // Projection code meaning universal transverse mercator (UTM),
  // according to the scheme LAS uses.
  const int projection_code_utm = 1;

  // For now we only handle UTM, since this is reall just intended to
  // test things out.
  g_assert (ddr.proj_code == projection_code_utm);
  self->projection_type = UNIVERSAL_TRANSVERSE_MERCATOR;
  (self->projection_parameters).utm.zone = ddr.zone_code;

  // Read the actual data.
  g_assert (ddr.dtype == DTYPE_SHORT);
  self->data = float_image_new_from_file_with_sample_type
    (self->size_x, self->size_y, las_data_file, 0,
     FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN,
     FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER);

  // The signed 16 bit LAS DEMs use this magic number to mean "no
  // data".  It stays about the same in floating point after ingest,
  // and we check for it and use it to set the invalid_data_mask.
  const float no_data_value = 0.0;
  self->invalid_data_mask = uint8_image_new (self->size_x, self->size_y);
  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      if ( gsl_fcmp (float_image_get_pixel (self->data, jj, ii), no_data_value,
                     0.0001) == 0) {
        uint8_image_set_pixel (self->invalid_data_mask, jj, ii,
                               MAP_PROJECTED_DEM_INVALID);
      }
      else {
        uint8_image_set_pixel (self->invalid_data_mask, jj, ii,
                               MAP_PROJECTED_DEM_VALID);
      }
    }
  }

  // For internal consistency, the size of the images that store the
  // data and the mask had better be the same as the size of this
  // instance.
  g_assert (self->size_x == self->data->size_x
            && self->size_y == self->data->size_y
            && self->size_x == self->invalid_data_mask->size_x
            && self->size_y == self->invalid_data_mask->size_y);

  return self;
}

MapProjectedDEM *
map_projected_dem_new_from_asf_internal (const char *asf_header_file,
                                         const char *asf_data_file)
{

  meta_parameters *meta = meta_read (asf_header_file);
  asfRequire(meta->sar->image_type == 'P',
             "DEM is not projected, this makes no sense\n");
  asfRequire(meta->projection != NULL,
             "Metadata projection block is not present!\n");

  MapProjectedDEM *self = g_new (MapProjectedDEM, 1);

  self->size_x = meta->general->sample_count;
  self->size_y = meta->general->line_count;
  self->upper_left_x = meta->projection->startX;
  self->upper_left_y = meta->projection->startY;
  self->projection_coordinates_per_x_pixel = meta->projection->perX;
  self->projection_coordinates_per_y_pixel = meta->projection->perY;
  self->projection_type = meta->projection->type;
  switch (self->projection_type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
    case POLAR_STEREOGRAPHIC:
    case ALBERS_EQUAL_AREA:
    case LAMBERT_CONFORMAL_CONIC:
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    case STATE_PLANE:
      memcpy (&(self->projection_type), &(meta->projection->param),
              sizeof(project_parameters_t));
      break;
    case SCANSAR_PROJECTION:
      asfPrintError("The DEM in the SCANSAR Projection; this makes no sense.\n");
      break;
    default:
      asfPrintError("DEM has an unknown projection type.\n");
  }

  // Read the actual data.
  self->data = float_image_new_from_file (
                self->size_x, self->size_y, asf_data_file, 0,
                FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  // Use this magic number to mean "no data" to create the invalid_data_mask.
  const float no_data_value = 0.0;
  self->invalid_data_mask = uint8_image_new (self->size_x, self->size_y);
  size_t ii, jj;
  for ( ii = 0 ; ii < self->size_y ; ii++ ) {
    for ( jj = 0 ; jj < self->size_x ; jj++ ) {
      float pixel_value = float_image_get_pixel( self->data, jj, ii);
      if ( gsl_fcmp( pixel_value, no_data_value, 0.0001) == 0) {
        uint8_image_set_pixel (self->invalid_data_mask, jj, ii,
                               MAP_PROJECTED_DEM_INVALID);
      }
      else {
        uint8_image_set_pixel (self->invalid_data_mask, jj, ii,
                               MAP_PROJECTED_DEM_VALID);
      }
    }
  }

  // For internal consistency, the size of the images that store the
  // data and the mask had better be the same as the size of this
  // instance.
  asfRequire (   self->size_x == self->data->size_x
              && self->size_y == self->data->size_y
              && self->size_x == self->invalid_data_mask->size_x
              && self->size_y == self->invalid_data_mask->size_y,
              "DEM and internally generated DEM mask are inconsistently sized\n");

  return self;
}

MapProjectedDEM *
map_projected_dem_new_subdem (MapProjectedDEM *model, double x_start,
                              double x_end, double y_start, double y_end)
{
  g_assert (x_start < x_end);
  g_assert (y_start < y_end);

  MapProjectedDEM *self = g_new (MapProjectedDEM, 1);

  // Convenience aliases.
  double pcpxp = model->projection_coordinates_per_x_pixel;
  double pcpyp = model->projection_coordinates_per_y_pixel;

  // Pixel indicies in model->data of subimage to be extracted, and
  // width and heiht of subimage to be extracted.
  size_t xps, width, yps, height;

  // Find the corner and extent of the new subdem in the x direction.
  if ( x_start <= model->upper_left_x ) {
    xps = 0;
  }
  else {
    xps = floor ((x_start - model->upper_left_x) / pcpxp);
  }
  self->upper_left_x = model->upper_left_x + xps * pcpxp;
  width = ceil ((x_end - self->upper_left_x) / pcpxp);
  if ( xps + width > model->size_x ) {
    width = model->size_x - xps;
  }
  self->size_x = width;
  self->projection_coordinates_per_x_pixel = pcpxp;

  // Find the corner and extent of the new subdem in the y direction.
  // This is a bit different than the x direction computations because
  // the FloatImage y index increases in a downward direction, and map
  // projection y coordinates increase in an upward direction.
  if ( y_end >= model->upper_left_y ) {
    yps = 0;
  }
  else {
    yps = floor ((model->upper_left_y - y_end) / pcpyp);
  }
  self->upper_left_y = model->upper_left_y - yps * pcpyp;
  height = ceil ((self->upper_left_y - y_start) / pcpyp);
  if ( yps + height > model->size_y ) {
    height = model->size_y - yps;
  }
  self->size_y = height;
  self->projection_coordinates_per_y_pixel = pcpyp;

  self->data = float_image_new_subimage (model->data, xps, yps, width, height);

  self->invalid_data_mask = uint8_image_new_subimage (model->invalid_data_mask,
                                                      xps, yps, width, height);

  self->projection_type = model->projection_type;
  self->projection_parameters = model->projection_parameters;

  return self;
}

void
map_projected_dem_get_x_y_h (MapProjectedDEM *self, ssize_t pixel_x,
                             ssize_t pixel_y, double *x, double *y, double *h)
{
  *x = self->upper_left_x + self->projection_coordinates_per_x_pixel * pixel_x;
  *y = self->upper_left_y - self->projection_coordinates_per_y_pixel * pixel_y;
  *h = float_image_get_pixel (self->data, pixel_x, pixel_y);
}

void
map_projected_dem_get_latitudes_longitudes_heights
  (MapProjectedDEM *self, ssize_t row, double *latitudes, double *longitudes,
   double *heights)
{
  // Routine to use to inverse project an array of points.
  int (*project_arr_inv) (project_parameters_t *pps, double *x, double *y,
                          double **lat, double **lon, long length);
  project_arr_inv = NULL;   // Compiler reassurance.
  switch ( self->projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    project_arr_inv = project_utm_arr_inv;
    break;
  default:
    g_assert_not_reached ();
    break;
  }

  // Allocate space for the arguments.
  double *proj_x = g_new (double, self->size_x);
  double *proj_y = g_new (double, self->size_x);
  g_assert (latitudes != NULL);
  g_assert (longitudes != NULL);

  size_t ii;
  for ( ii = 0 ; ii < self->size_x ; ii++ ) {
    proj_x[ii] = (self->upper_left_x
                  + self->projection_coordinates_per_x_pixel * ii);
    proj_y[ii] = (self->upper_left_y
                  - self->projection_coordinates_per_y_pixel * row);

    // kludge to work around some proj oddities
    if (self->projection_type == UNIVERSAL_TRANSVERSE_MERCATOR)
      proj_y[ii] += 1e7;

    heights[ii] = float_image_get_pixel (self->data, ii, row);
  }

  int return_code = project_arr_inv (&(self->projection_parameters), proj_x,
                                     proj_y, &latitudes, &longitudes,
                                     self->size_x);
  g_assert (return_code == TRUE);

  g_free (proj_y);
  g_free (proj_x);
}

void
map_projected_dem_get_line_samp_from_latitude_longitude
(MapProjectedDEM *self, double latitude, double longitude, double height,
 double *line, double *samp)
{
  // Routine to use to inverse project an array of points.
  int (*project) (project_parameters_t *pps, double lat, double lon,
		  double *x, double *y);
  project = NULL;   // Compiler reassurance.
  switch ( self->projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    project = project_utm;
    break;
  default:
    g_assert_not_reached ();
    break;
  }

  double x, y;
  project_set_avg_height(height);
  project (&(self->projection_parameters), latitude, longitude, &x, &y);

  *samp = (x - self->upper_left_x) / self->projection_coordinates_per_x_pixel;
  *line = (self->upper_left_y - y) / self->projection_coordinates_per_y_pixel;
}

void
map_projected_dem_free (MapProjectedDEM *self)
{
  float_image_free (self->data);
  uint8_image_free (self->invalid_data_mask);
  g_free (self);
}
