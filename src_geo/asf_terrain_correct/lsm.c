#include "lsm.h"

#include <glib.h>
#include <stdio.h>

#include "float_image.h"
#include "asf_nan.h"

/*
void
lsm_data_set(LSMData * self, double nadir_distance, double dem_height,
	     double imaging_time)
{
  self->nadir_distance = nadir_distance;
  self->dem_height = dem_height;
  self->imaging_time = imaging_time;
}

LSM *
lsm_new (int nrows, int ncols)
{
  LSM * self = g_new (LSM, 1);

  g_assert(nrows > 0);
  g_assert(ncols > 0);

  self->nrows = nrows;
  self->ncols = ncols;

  self->data = g_new (LSMData, nrows * ncols);

  self->satellite_heights = g_new(double, nrows);
  return self;
}

void
lsm_set_pixel_data(LSM *self,
		   int row, int col,
                   double nadir_distance, double dem_height, 
		   double imaging_time)
{
  g_assert(self);
  g_assert(self->data);

  int i = self->nrows * row + col;

  g_assert(i >= 0);
  g_assert(i < self->ncols * self->nrows);

  lsm_data_set(&self->data[i], nadir_distance, dem_height, imaging_time);  
}

void
lsm_set_satellite_height(LSM *self, int row, double height)
{
  g_assert(self);
  g_assert(self->satellite_heights);

  g_assert(row < self->nrows);

  self->satellite_heights[row] = height;
}

static void
lsm_assert_valid(LSM* self, int row, int col)
{
  g_assert(self);
  g_assert(self->data);
  g_assert(self->satellite_heights);

  g_assert(row >= 0);
  g_assert(col >= 0);
  g_assert(row < self->nrows);
  g_assert(col < self->ncols);
}

static double
lsm_get_dem_height_at(LSM *self, int row, int col)
{
  lsm_assert_valid(self, row, col);

  LSMData * p = &self->data[row*self->nrows + col];

  return p->dem_height;
}

static double
lsm_get_imaging_time_at(LSM *self, int row, int col)
{
  lsm_assert_valid(self, row, col);

  LSMData * p = &self->data[row*self->nrows + col];

  return p->imaging_time;
}

static double
lsm_get_nadir_distance_at(LSM *self, int row, int col)
{
  lsm_assert_valid(self, row, col);

  LSMData * p = &self->data[row*self->nrows + col];

  return p->nadir_distance;
}

static void
write_mask_file(const char *filename, lsm_mask_value_t *mask,
		int nrows, int ncols)
{
  FloatImage *fmask = float_image_new(nrows, ncols);
  int row, col;

  for (row = 0; row < nrows; ++row) {
    for (col = 0; col < ncols; ++col) {
      int n = row * nrows + col;

      float value;
      
      switch (mask[n]) {
	default:
	case MASK_NORMAL_VALUE:
	  value = 128;
	  break;
	case MASK_ACTIVE_SHADOW_VALUE:
	case MASK_PASSIVE_SHADOW_VALUE:
	  value = 0;
	  break;
	case MASK_ACTIVE_LAYOVER_VALUE:
	case MASK_PASSIVE_LAYOVER_VALUE:
	  value = 255;
	  break;
      }

      float_image_set_pixel(fmask, row, col, value);
    }
  }

  size_t max_dim = nrows > ncols ? nrows : ncols;
  float_image_export_as_jpeg(fmask, filename, max_dim, NAN);
}

void
lsm_generate(LSM *self, const char *filename)
{
  g_assert(self);
  g_assert(self->data);
  g_assert(self->satellite_heights);

  int row, col;
  int N = self->nrows * self->ncols;

  lsm_mask_value_t *mask = g_new (lsm_mask_value_t, N);

  double dx = 2 * self->dx;
  double dy = 2 * self->dy;

  for (row = 0; row < self->nrows; ++row) {
    double z_s = self->satellite_heights[row];

    for (col = 0; col < self->ncols; ++col) {
      double z = lsm_get_dem_height_at(self, row, col);
      double d = lsm_get_nadir_distance_at(self, row, col);

      double zn = lsm_get_dem_height_at(self, row - 1, col);
      double zs = lsm_get_dem_height_at(self, row + 1, col);
      double zw = lsm_get_dem_height_at(self, row, col - 1);
      double ze = lsm_get_dem_height_at(self, row, col + 1);

      double tn = lsm_get_imaging_time_at(self, row - 1, col);
      double ts = lsm_get_imaging_time_at(self, row + 1, col);
      double tw = lsm_get_imaging_time_at(self, row, col - 1);
      double te = lsm_get_imaging_time_at(self, row, col + 1);

      double dd = (ze - zw)/dx * (te - tw)/dx + (zn - zs)/dy * (tn - ts)/dy;

      double R = d - (z_s - z) * dd;
      double A = (z_s - z) + d * dd;

      int n = self->nrows * row + col;

      g_assert(n >= 0);
      g_assert(n < N);

      if (A <= 0)
	mask[n] = MASK_ACTIVE_SHADOW_VALUE;
      else if (R <= 0)
	mask[n] = MASK_ACTIVE_LAYOVER_VALUE;
      else
	mask[n] = MASK_NORMAL_VALUE;
    }
  }

  write_mask_file(filename, mask, self->nrows, self->ncols);
}

void
lsm_free(LSM *self)
{
  if (self) {
    if (self->data)
      g_free(self->data);

    if (self->satellite_heights)
      g_free(self->satellite_heights);

    g_free(self);
  }
}

*/

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
lsm_image_mask_value_is_show(FloatImage *mask, int row, int col)
{
  return lsm_mask_value_is_shadow(float_image_get_pixel(mask, row, col));
}

FloatImage *
lsm_generate_mask(DEMGeomInfo *dgi)
{
  dgi = dgi;
  return NULL;
}
