#include "dem_geom_info.h"

#include <glib.h>
#include <stdio.h>

#include "float_image.h"
#include "asf_nan.h"

DEMGeomInfo *
dem_geom_info_new(int nrows, int ncols)
{
  DEMGeomInfo * self = g_new (DEMGeomInfo, 1);

  g_assert(nrows > 0);
  g_assert(ncols > 0);

  self->nrows = nrows;
  self->ncols = ncols;

  self->cp_target_x = float_image_new(nrows, ncols);
  self->cp_target_y = float_image_new(nrows, ncols);
  self->cp_target_z = float_image_new(nrows, ncols);
  self->slant_range_value = float_image_new(nrows, ncols);
  self->imaging_time = float_image_new(nrows, ncols);
  self->satellite_height = float_image_new(nrows, ncols);
  self->dem_height = float_image_new(nrows, ncols);
  self->nadir_distance = float_image_new(nrows, ncols);

  return self;
}

void
dem_geom_info_set(DEMGeomInfo * self,
		  int row, int col,
		  Vector *cp_target,
		  double imaging_time,
		  double slant_range_value,
		  double dem_height,
		  Vector *poca)
{
  g_assert(self);

  g_assert(self->cp_target_x);
  float_image_set_pixel(self->cp_target_x, row, col, cp_target->x);

  g_assert(self->cp_target_y);
  float_image_set_pixel(self->cp_target_y, row, col, cp_target->y);

  g_assert(self->cp_target_z);
  float_image_set_pixel(self->cp_target_z, row, col, cp_target->z);

  g_assert(self->slant_range_value);
  float_image_set_pixel(self->slant_range_value, row, col, slant_range_value);

  g_assert(self->imaging_time);
  float_image_set_pixel(self->imaging_time, row, col, imaging_time);

  g_assert(self->satellite_height);
  float_image_set_pixel(self->satellite_height, row, col, poca->z);

  g_assert(self->dem_height);
  float_image_set_pixel(self->satellite_height, row, col, dem_height);

  g_assert(self->nadir_distance);
  double d = hypot(poca->x - cp_target->x, poca->y - cp_target->y);
  float_image_set_pixel(self->nadir_distance, row, col, d);
}

double
dem_geom_info_get_slant_range_value(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->slant_range_value);

  return float_image_get_pixel(self->slant_range_value, row, col);
}

double
dem_geom_info_get_imaging_time(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->imaging_time);

  return float_image_get_pixel(self->imaging_time, row, col);
}

double
dem_geom_info_get_x(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->cp_target_x);

  return float_image_get_pixel(self->cp_target_x, row, col);
}

double
dem_geom_info_get_y(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->cp_target_y);

  return float_image_get_pixel(self->cp_target_y, row, col);
}

double
dem_geom_info_get_z(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->cp_target_z);

  return float_image_get_pixel(self->cp_target_z, row, col);
}

Vector *
dem_geom_info_get_cp_target(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->cp_target_x);
  g_assert(self->cp_target_y);
  g_assert(self->cp_target_z);

  Vector * ret = vector_new(0,0,0);
  vector_set(ret,
	     float_image_get_pixel(self->cp_target_x, row, col),
	     float_image_get_pixel(self->cp_target_y, row, col),
	     float_image_get_pixel(self->cp_target_z, row, col));

  return ret;
}

double
dem_geom_info_get_nadir_distance(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->nadir_distance);

  return float_image_get_pixel(self->nadir_distance, row, col);
}

double
dem_geom_info_get_satellite_height(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->satellite_height);

  return float_image_get_pixel(self->satellite_height, row, col);
}

double
dem_geom_info_get_dem_height(DEMGeomInfo * self, int row, int col)
{
  g_assert(self);
  g_assert(self->dem_height);

  return float_image_get_pixel(self->dem_height, row, col);
}


void
dem_geom_info_free(DEMGeomInfo * self)
{
  float_image_free(self->cp_target_x);
  float_image_free(self->cp_target_y);
  float_image_free(self->cp_target_z);
  float_image_free(self->slant_range_value);
  float_image_free(self->imaging_time);
  float_image_free(self->satellite_height);
  float_image_free(self->dem_height);
  float_image_free(self->nadir_distance);

  g_free(self);
}

