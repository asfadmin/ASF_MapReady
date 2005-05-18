// A map projected Digital Elevation Model (DEM).

#ifndef MAP_PROJECTED_DEM_H
#define MAP_PROJECTED_DEM_H

#include <asf_meta.h>
#include <float_image.h>

// Instance structure.  All members should be considered read only.
typedef struct {
  // Size of DEM, in pixels (these better be identical to the sizes in
  // the data element, and are repeated here only for convenience).
  size_t size_x, size_y;
  // Projection coordinates of upper left pixel.
  double upper_left_x, upper_left_y;
  // Projection coordinates per pixel in positive x direction.
  double projection_coordinates_per_x_pixel;
  // Projection coordinates per pixel in positive y direction.
  double projection_coordinates_per_y_pixel;
  // Type of projection this DEM is in.
  projection_type_t projection_type;
  // Projection parameters defining the projection.
  project_parameters_t projection_parameters;
  // The actual data.
  FloatImage *data;
} MapProjectedDEM;

// Create a new instance from a Land Analysis System (LAS) header
// file/data file pair.  Note that the LAS header file name should not
// include the ".ddr" extension (it is added automatically.
MapProjectedDEM *
map_projected_dem_new_from_las (const char *las_header_file_without_extension, 
				const char *las_data_file);

// Get the x and y projection coordinates and height h above the
// reference datum of zero indexed DEM pixel (pixel_x, pixel_y).
void
map_projected_dem_get_x_y_h (MapProjectedDEM *self, ssize_t pixel_x,
			     ssize_t pixel_y, double *x, double *y, double *h);

// Get the latitudes, longitudes, and heights above the WGS84
// ellipsoid of all DEM pixels in row.  The 'latitudes' and
// 'longitudes' arguments must be pointers to arrays large enough to
// hold a rows worth of values, i.e. pointers to space sufficient to
// hold self->size_x doubles.
void
map_projected_dem_get_latitudes_longitudes_heights 
  (MapProjectedDEM *self, ssize_t row, double *latitudes, double *longitudes,
   double *heights);

// Free self.
void
map_projected_dem_free (MapProjectedDEM *self);

#endif // #ifndef MAP_PROJECTED_DEM_H



