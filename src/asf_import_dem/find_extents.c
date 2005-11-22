#include <glib.h>

#include <libasf_proj.h>

#include "asf_import_dem.h"
#include "seamless_meta.h"


void
find_extents ( seamless_meta_t *smeta, project_parameters_t *outProjPrms,
               PROJECT_ARR_T project_arr,
               double *min_x, double *max_x, double *min_y, double *max_y)
{
  int ncols = seamless_meta_get_ncols(smeta);
  int nrows = seamless_meta_get_nrows(smeta);
  double yllcorner = seamless_meta_get_yllcorner(smeta);
  double xllcorner = seamless_meta_get_xllcorner(smeta);
  double cellsize = seamless_meta_get_cellsize(smeta);

  // Number of pixels in the edge of the image.
  int edge_point_count = 2 * ncols + 2 * nrows - 4;

  double *lats = g_new (double, edge_point_count);
  double *lons = g_new (double, edge_point_count);
  double upper_lat = (yllcorner + cellsize * nrows) * DEG_TO_RAD;
  double lower_lat = yllcorner * DEG_TO_RAD;
  double left_lon = xllcorner * DEG_TO_RAD;
  double right_lon = (xllcorner + cellsize * ncols) * DEG_TO_RAD;
  int current_edge_point = 0;
  int ii, jj;

  // Top row  (->)
  for ( ii=0; ii<ncols; ii++ ) {
    lats[current_edge_point] = upper_lat;
    lons[current_edge_point] = (xllcorner + cellsize * ii)
                               * DEG_TO_RAD;
    current_edge_point++;
  }
  // Right column  (v)
  for ( jj=(nrows-2); jj>0 ; jj-- ) {
    lats[current_edge_point] = (yllcorner + cellsize * jj)
                               * DEG_TO_RAD;
    lons[current_edge_point] = right_lon;
    current_edge_point++;
  }
  // Bottom row  (<-)
  for ( ii=(ncols-1); ii>=0; ii-- ) {
    lats[current_edge_point] = lower_lat;
    lons[current_edge_point] = (xllcorner + cellsize * ii)
                               * DEG_TO_RAD;
    current_edge_point++;
  }
  // Left column  (^)
  for ( jj=1; jj<(nrows-1); jj++ ) {
    lats[current_edge_point] = (yllcorner + cellsize * jj)
                               * DEG_TO_RAD;
    lons[current_edge_point] = left_lon;
    current_edge_point++;
  }
  asfRequire (current_edge_point == edge_point_count);

  // Project all the edge pixels.
  // Pointers to arrays of projected coordinates to be filled in.
  // Sadly this function call doesn't account for the input (lat/lon) datum/spheriod
  double *x = NULL, *y = NULL;
  x = y = NULL;
  int return_code = FALSE;
  return_code = project_arr (outProjPrms, lats, lons, &x, &y, edge_point_count);
  g_assert (return_code == TRUE);
  g_free (lons);
  g_free (lats);

  // Find the extents of the image in projection coordinates.
  // Project all the edge pixels.
  *min_x = DBL_MAX;
  *max_x = DBL_MIN;
  *min_y = DBL_MAX;
  *max_y = DBL_MIN;
  for ( ii=0; ii<edge_point_count; ii++ ) {
    if ( x[ii] < *min_x ) { *min_x = x[ii]; }
    if ( x[ii] > *max_x ) { *max_x = x[ii]; }
    if ( y[ii] < *min_y ) { *min_y = y[ii]; }
    if ( y[ii] > *max_y ) { *max_y = y[ii]; }
  }
  FREE(y);
  FREE(x);
}
