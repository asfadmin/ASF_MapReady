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
lsm_image_mask_value_is_layover(FloatImage *mask, int row, int col)
{
  return lsm_mask_value_is_layover(float_image_get_pixel(mask, row, col));
}

int
lsm_image_mask_value_is_shadow(FloatImage *mask, int row, int col)
{
  return lsm_mask_value_is_shadow(float_image_get_pixel(mask, row, col));
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

  FloatImage *mask = float_image_new (dgi->nrows, dgi->ncols);

  double dx = 1; // FIXME! 2 * self->dx;
  double dy = 1; // FIXME! 2 * self->dy;

  for (row = 0; row < dgi->nrows; ++row) {

    for (col = 0; col < dgi->ncols; ++col) {
      double z = dem_geom_info_get_dem_height(dgi, row, col);
      double d = dem_geom_info_get_nadir_distance(dgi, row, col);

      double zn = dem_geom_info_get_dem_height(dgi, row - 1, col);
      double zs = dem_geom_info_get_dem_height(dgi, row + 1, col);
      double zw = dem_geom_info_get_dem_height(dgi, row, col - 1);
      double ze = dem_geom_info_get_dem_height(dgi, row, col + 1);

      double tn = dem_geom_info_get_imaging_time(dgi, row - 1, col);
      double ts = dem_geom_info_get_imaging_time(dgi, row + 1, col);
      double tw = dem_geom_info_get_imaging_time(dgi, row, col - 1);
      double te = dem_geom_info_get_imaging_time(dgi, row, col + 1);

      double dd = (ze - zw)/dx * (te - tw)/dx + (zn - zs)/dy * (tn - ts)/dy;

      double z_s = dem_geom_info_get_satellite_height(dgi, row, col);

      double R = d - (z_s - z) * dd;
      double A = (z_s - z) + d * dd;

      if (A <= 0)
	float_image_set_pixel(mask, row, col, MASK_ACTIVE_SHADOW_VALUE);
      else if (R <= 0)
	float_image_set_pixel(mask, row, col, MASK_ACTIVE_LAYOVER_VALUE);

      // FIXME: check in a temp version that always gives an empty mask
      //        just so we can get the interace set.
      float_image_set_pixel(mask, row, col, MASK_NORMAL_VALUE);
    }
  }

  return mask;
}
