// A map projected Digital Elevation Model (DEM).

#ifndef MAP_PROJECTED_DEM_H
#define MAP_PROJECTED_DEM_H

#include <asf_meta.h>
#include <float_image.h>
#include <uint8_image.h>

// These values are used for the pixels of self->invalid_data_mask.
#define MAP_PROJECTED_DEM_VALID 0
#define MAP_PROJECTED_DEM_INVALID 1

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
  // The actual height data.
  FloatImage *data;
  // Mask showing which pixels of the DEM have valid data.  If a pixel
  // of invalid_data_mask is MAP_PROJECTED_DEM_VALID, the DEM has
  // valid data at that pixel, if it is MAP_PROJECTED_DEM_INVALID, it
  // doesn't.
  UInt8Image *invalid_data_mask;
} MapProjectedDEM;

// Create a new instance from a Land Analysis System (LAS) header
// file/data file pair.  Note that the LAS header file name should not
// include the ".ddr" extension (it is added automatically.
MapProjectedDEM *
map_projected_dem_new_from_las (const char *las_header_file_without_extension,
				const char *las_data_file);

// Create a new instance from our internal file format (.img and .meta)
MapProjectedDEM *
map_projected_dem_new_from_asf_internal (const char *asf_header_file,
                                         const char *asf_data_file);

// Create a new DEM of minimal size covering as much as possible of
// projection coordinate ranges [x_start, x_end] in x projection
// coordinates and [y_start, y_end] in y projection coordinates.  If
// the model supplied doesn't cover part of the requested area, the
// new instance will cover as much as possible given the limitations
// of the model.  Note that the area covered by the new image is
// gauranteed to be inclusive of the above range end points provided
// they are in the model, but may extend slight them by a fraction of
// a pixel, possibly slightly more if there is rounding error.
MapProjectedDEM *
map_projected_dem_new_subdem (MapProjectedDEM *model, double x_start,
			      double x_end, double y_start, double y_end);

// Get the x and y projection coordinates and height h above the
// reference datum of zero indexed DEM pixel (pixel_x, pixel_y).
// WARNING: this method will happily return the xyh of a pixel for
// which invalid_data_mask is set.
void
map_projected_dem_get_x_y_h (MapProjectedDEM *self, ssize_t pixel_x,
			     ssize_t pixel_y, double *x, double *y, double *h);

// Get the latitudes, longitudes, and heights above the WGS84
// ellipsoid of all DEM pixels in row.  The 'latitudes' and
// 'longitudes' arguments must be pointers to arrays large enough to
// hold a rows worth of values, i.e. pointers to space sufficient to
// hold self->size_x doubles.  WARNING: this method ouput values for
// pixels for which invalid_data_mask is set.
void
map_projected_dem_get_latitudes_longitudes_heights
  (MapProjectedDEM *self, ssize_t row, double *latitudes, double *longitudes,
   double *heights);

// Get the line & sample values for the given latitude, longitude and height.
void
map_projected_dem_get_line_samp_from_latitude_longitude
(MapProjectedDEM *self, double latitude, double longitude, double height,
 double *line, double *samp);

// Free self.
void
map_projected_dem_free (MapProjectedDEM *self);

#endif // #ifndef MAP_PROJECTED_DEM_H



