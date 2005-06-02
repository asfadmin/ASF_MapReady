#include "dem_geom_info.h"

#include <glib.h>
#include <stdio.h>

#include "float_image.h"
#include "asf_nan.h"

DEMGeomInfo *
dem_geom_info_new(int size_x, int size_y)
{
  DEMGeomInfo * self = g_new (DEMGeomInfo, 1);

  g_assert(size_x > 0);
  g_assert(size_y > 0);

  self->size_x = size_x;
  self->size_y = size_y;

  self->cp_target_x = float_image_new(size_x, size_y);
  self->cp_target_y = float_image_new(size_x, size_y);
  self->cp_target_z = float_image_new(size_x, size_y);
  self->slant_range_value = float_image_new(size_x, size_y);
  self->imaging_time = float_image_new(size_x, size_y);
  self->satellite_height = float_image_new(size_x, size_y);
  self->dem_height = float_image_new(size_x, size_y);
  self->nadir_distance = float_image_new(size_x, size_y);

  return self;
}

// WARNING: this routing MUST be kept in sync with the actual instance
// structure contents and with the behavior of dem_geom_info_thaw().
void
dem_geom_info_freeze(DEMGeomInfo *self, FILE *file_pointer)
{
  size_t write_count = fwrite (&(self->size_x), sizeof (int), 1, file_pointer);
  g_assert (write_count == 1);

  write_count = fwrite (&(self->size_y), sizeof (int), 1, file_pointer);
  g_assert (write_count == 1);

  float_image_freeze (self->cp_target_x, file_pointer);
  float_image_freeze (self->cp_target_y, file_pointer);
  float_image_freeze (self->cp_target_z, file_pointer);
  float_image_freeze (self->nadir_distance, file_pointer);
  float_image_freeze (self->slant_range_value, file_pointer);
  float_image_freeze (self->imaging_time, file_pointer);
  float_image_freeze (self->satellite_height, file_pointer);
  float_image_freeze (self->dem_height, file_pointer);
}
  
// WARNING: this routing MUST be kept in sync with the actual instance
// structure contents and with the behavior of dem_geom_info_freeze().
DEMGeomInfo *
dem_geom_info_thaw (FILE *file_pointer)
{
  DEMGeomInfo *self = g_new (DEMGeomInfo, 1);

  size_t read_count = fread (&(self->size_x), sizeof (int), 1, file_pointer);
  g_assert (read_count == 1);

  read_count = fread (&(self->size_y), sizeof (int), 1, file_pointer);
  g_assert (read_count == 1);

  self->cp_target_x = float_image_thaw (file_pointer);
  self->cp_target_y = float_image_thaw (file_pointer);
  self->cp_target_z = float_image_thaw (file_pointer);
  self->nadir_distance = float_image_thaw (file_pointer);
  self->slant_range_value = float_image_thaw (file_pointer);
  self->imaging_time = float_image_thaw (file_pointer);
  self->satellite_height = float_image_thaw (file_pointer);
  self->dem_height = float_image_thaw (file_pointer);

  return self;
}

gboolean
dem_geom_info_equals (DEMGeomInfo *self, DEMGeomInfo *other, float epsilon)
{
  if ( self->size_x != other->size_x ) {
    return FALSE;
  }

  if ( self->size_y != other->size_y ) {
    return FALSE;
  }

  if (    !float_image_equals (self->cp_target_x, other->cp_target_x, epsilon)
       || !float_image_equals (self->cp_target_y, other->cp_target_y, epsilon)
       || !float_image_equals (self->cp_target_z, other->cp_target_z, epsilon)
       || !float_image_equals (self->nadir_distance, other->nadir_distance, 
			       epsilon)
       || !float_image_equals (self->slant_range_value, 
			       other->slant_range_value, epsilon)
       || !float_image_equals (self->imaging_time, other->imaging_time, 
			       epsilon)
       || !float_image_equals (self->satellite_height, other->satellite_height,
			       epsilon)
       || !float_image_equals (self->dem_height, other->dem_height, 
			       epsilon) ) {
    return FALSE;
  }

  return TRUE;
}

void
dem_geom_info_set(DEMGeomInfo * self,
		  int x, int y,
		  Vector *cp_target,
		  double imaging_time,
		  double slant_range_value,
		  double dem_height,
		  Vector *poca)
{
  g_assert(self);

  g_assert(self->cp_target_x);
  float_image_set_pixel(self->cp_target_x, x, y, cp_target->x);

  g_assert(self->cp_target_y);
  float_image_set_pixel(self->cp_target_y, x, y, cp_target->y);

  g_assert(self->cp_target_z);
  float_image_set_pixel(self->cp_target_z, x, y, cp_target->z);

  g_assert(self->slant_range_value);
  float_image_set_pixel(self->slant_range_value, x, y, slant_range_value);

  g_assert(self->imaging_time);
  float_image_set_pixel(self->imaging_time, x, y, imaging_time);

  g_assert(self->satellite_height);
  float_image_set_pixel(self->satellite_height, x, y, poca->z);

  g_assert(self->dem_height);
  float_image_set_pixel(self->dem_height, x, y, dem_height);

  g_assert(self->nadir_distance);
  double d = hypot(poca->x - cp_target->x, poca->y - cp_target->y);
  float_image_set_pixel(self->nadir_distance, x, y, d);
}

double
dem_geom_info_get_slant_range_value(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->slant_range_value);

  return float_image_get_pixel(self->slant_range_value, x, y);
}

double
dem_geom_info_get_imaging_time(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->imaging_time);

  return float_image_get_pixel(self->imaging_time, x, y);
}

double
dem_geom_info_get_x(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->cp_target_x);

  return float_image_get_pixel(self->cp_target_x, x, y);
}

double
dem_geom_info_get_y(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->cp_target_y);

  return float_image_get_pixel(self->cp_target_y, x, y);
}

double
dem_geom_info_get_z(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->cp_target_z);

  return float_image_get_pixel(self->cp_target_z, x, y);
}

Vector *
dem_geom_info_get_cp_target(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->cp_target_x);
  g_assert(self->cp_target_y);
  g_assert(self->cp_target_z);

  Vector * ret = vector_new(0,0,0);
  vector_set(ret,
	     float_image_get_pixel(self->cp_target_x, x, y),
	     float_image_get_pixel(self->cp_target_y, x, y),
	     float_image_get_pixel(self->cp_target_z, x, y));

  return ret;
}

double
dem_geom_info_get_nadir_distance(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->nadir_distance);

  return float_image_get_pixel(self->nadir_distance, x, y);
}

double
dem_geom_info_get_satellite_height(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->satellite_height);

  return float_image_get_pixel(self->satellite_height, x, y);
}

double
dem_geom_info_get_dem_height(DEMGeomInfo * self, int x, int y)
{
  g_assert(self);
  g_assert(self->dem_height);

  return float_image_get_pixel(self->dem_height, x, y);
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

