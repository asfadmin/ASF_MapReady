
#include <glib.h>

//#include <libasf_proj.h>
#include <asf_reporting.h>

#include "asf_import_dem.h"
#include "seamless_meta.h"


// Generate some mappings between output image projection
// coordinates and input image pixel coordinates, using proj.  We
// compute transformations for points on a grid_size * grid_size
// grid and a sparse_grid_size * sparse_grid_size grid.
void
create_grid ( seamless_meta_t *smeta,  project_parameters_t *outProjPrms,
              UNPROJECT_T unproject,
              const int grid_size, const int sparse_grid_stride,
              double min_x, double max_x, double min_y, double max_y,
              data_to_fit_t *dtf)
{

  asfPrintStatus ("Performing analytical projection of a spatially "
                  "distributed\nsubset of input image pixels... ");
  fflush (stdout);
  double x_range_size = max_x - min_x, y_range_size = max_y - min_y;
  g_assert (grid_size % 2 == 1);

  // Determine the density and stride for the sparse grid.
  const int sparse_grid_sample_stride = 2;

  // Spacing between grid points, in projection coordinates.
  double x_spacing = x_range_size / (grid_size - 1);
  double y_spacing = y_range_size / (grid_size - 1);


  // put seamless meta info into variables to avoid repeated function calls
  double yllcorner = seamless_meta_get_yllcorner(smeta);
  double xllcorner = seamless_meta_get_xllcorner(smeta);
  double cellsize = seamless_meta_get_cellsize(smeta);

  // Index into the flattened list of mappings we want to produce.
  int current_mapping = 0;
  int current_sparse_mapping = 0;
  int ii=0, jj=0;
  for ( ii = 0 ; ii < grid_size ; ii++ ) {
    for ( jj = 0 ; jj < grid_size ; jj++ ) {
      // Projection coordinates for the current grid point.
      double cxproj = min_x + x_spacing * jj;
      double cyproj = min_y + y_spacing * ii;
      // Corresponding latitude and longitude.
      double lat, lon;
      int return_code = unproject (outProjPrms, cxproj, cyproj, &lat, &lon);
      // Details of the error should have already been printed.
      asfRequire ( return_code!=0, "Projection Error!\n");
      lat *= RAD_TO_DEG;
      lon *= RAD_TO_DEG;
      // Corresponding pixel indicies in input image.
      double y_pix = (yllcorner-lat)/cellsize;
      double x_pix = (xllcorner-lon)/cellsize;

      dtf->x_proj[current_mapping] = cxproj;
      dtf->y_proj[current_mapping] = cyproj;
      dtf->x_pix[current_mapping] = x_pix;
      dtf->y_pix[current_mapping] = y_pix;

      if ( ii % sparse_grid_sample_stride == 0
           && jj % sparse_grid_sample_stride == 0 ) {
        dtf->sparse_x_proj[current_sparse_mapping] = cxproj;
        dtf->sparse_y_proj[current_sparse_mapping] = cyproj;
        dtf->sparse_x_pix[current_sparse_mapping] = x_pix;
        dtf->sparse_y_pix[current_sparse_mapping] = y_pix;
        current_sparse_mapping++;
      }
      current_mapping++;
    }
  }

  asfPrintStatus ("done.\n\n");
}
