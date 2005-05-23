#include "lsm.h"

#include <glib.h>
#include <stdio.h>

#include "float_image.h"
#include "asf_nan.h"

int
lsm_mask_value_is_layover(int mask_value)
{
  return 
    mask_value == MASK_ACTIVE_LAYOVER_VALUE ||
    mask_value == MASK_PASSIVE_LAYOVER_VALUE;
}

int
lsm_mask_value_is_shadow(int mask_value)
{
  return 
    mask_value == MASK_ACTIVE_SHADOW_VALUE ||
    mask_value == MASK_PASSIVE_SHADOW_VALUE;
}

int
lsm_image_mask_value_is_layover(FloatImage *mask, int x, int y)
{
  return lsm_mask_value_is_layover(float_image_get_pixel(mask, x, y));
}

int
lsm_image_mask_value_is_shadow(FloatImage *mask, int x, int y)
{
  return lsm_mask_value_is_shadow(float_image_get_pixel(mask, x, y));
}

FloatImage *
lsm_generate_mask(DEMGeomInfo *dgi)
{
  g_assert(dgi);

  g_assert(dgi->cp_target_z);
  g_assert(dgi->slant_range_value);
  g_assert(dgi->imaging_time);
  g_assert(dgi->satellite_height);
  g_assert(dgi->dem_height);
  g_assert(dgi->nadir_distance);

  g_assert(dgi->nrows > 0);
  g_assert(dgi->ncols > 0);

  int row, col;
  int nshad = 0;
  int nlay = 0;
  int nnorm = 0;

  FloatImage *mask = float_image_new (dgi->ncols, dgi->nrows);

  for (y = 0; y < dgi->nrows; ++y) {
    for (x = 0; x < dgi->ncols; ++x) {
      lsm_mask_value_t pixel_value;

      if (y == 0 || y == dgi->nrows - 1 || 
	  x == 0 || x == dgi->ncols - 1) {
	pixel_value = MASK_NORMAL_VALUE;
      } 
      else {
	double z = dem_geom_info_get_dem_height(dgi, x, y);
	double d = dem_geom_info_get_nadir_distance(dgi, x, y);
	
	double zn = dem_geom_info_get_dem_height(dgi, x - 1, y);
	double zs = dem_geom_info_get_dem_height(dgi, x + 1, y);
	double zw = dem_geom_info_get_dem_height(dgi, x, y - 1);
	double ze = dem_geom_info_get_dem_height(dgi, x, y + 1);
	
	double tn = dem_geom_info_get_imaging_time(dgi, x - 1, y);
	double ts = dem_geom_info_get_imaging_time(dgi, x + 1, y);
	double tw = dem_geom_info_get_imaging_time(dgi, x, y - 1);
	double te = dem_geom_info_get_imaging_time(dgi, x, y + 1);
	
	double dx = ( dem_geom_info_get_x(dgi, x + 1, y) -
		      dem_geom_info_get_x(dgi, x - 1, y) );
	
	double dy = ( dem_geom_info_get_y(dgi, x, y + 1) -
		      dem_geom_info_get_y(dgi, x, y - 1) );
	
	double dd = (ze - zw)/dx * (te - tw)/dx + (zn - zs)/dy * (tn - ts)/dy;
	
	double z_s = dem_geom_info_get_satellite_height(dgi, x, y);
	
	double R = d - (z_s - z) * dd;
	double A = (z_s - z) + d * dd;
      
	if (A <= 0) {
	  ++nshad;
	  pixel_value = MASK_ACTIVE_SHADOW_VALUE;
	}
	else if (R <= 0) {
	  ++nlay;
	  pixel_value = MASK_ACTIVE_LAYOVER_VALUE;
	}
	else {
	  ++nnorm;
	  pixel_value = MASK_NORMAL_VALUE;
        }
      }

      float_image_set_pixel(mask, x, y, pixel_value);
    }
  }

  printf("Out of %d pixels:\n %4d shadow\n %4d layover\n %4d other\n",
	 nshad + nlay + nnorm, nshad, nlay, nnorm);

  int max_dim = dgi->nrows > dgi->ncols ? dgi->nrows : dgi->ncols;
  float_image_export_as_jpeg(mask, "lsm.jpg", max_dim, NAN);
  return mask;
}
