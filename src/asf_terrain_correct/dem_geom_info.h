
#ifndef DEM_GEOM_INFO_H
#define DEM_GEOM_INFO_H

#include "vector.h"
#include "float_image.h"

typedef struct dem_geom_info {
    int size_x, size_y;
    FloatImage * cp_target_x;
    FloatImage * cp_target_y;
    FloatImage * cp_target_z;
    FloatImage * nadir_distance;
    FloatImage * slant_range_value;
    FloatImage * imaging_time;
    FloatImage * satellite_height;
    FloatImage * dem_height;
} DEMGeomInfo;

DEMGeomInfo *
dem_geom_info_new(int size_x, int size_y);

// Serialize self to file_pointer (which must point into a file open
// for writing).
void
dem_geom_info_freeze(DEMGeomInfo *self, FILE *file_pointer);

// Recover previously serialized (with dem_geom_info_freeze) instance
// from data at file_pointer (which must point into a file open for
// reading).
DEMGeomInfo *
dem_geom_info_thaw (FILE *file_pointer);

// Return true iff self and other have identical sizes and constituent
// image pixels that are all approximately equal to relative accuracy
// epsilon, as understood by the GNU Scientific Library function
// gsl_fcmp.
gboolean
dem_geom_info_equals (DEMGeomInfo *self, DEMGeomInfo *other, float epsilon);

void
dem_geom_info_set(DEMGeomInfo * self,
		  int x, int y,
		  Vector *cp_target,
		  double imaging_time,
		  double slant_range_value,
		  double dem_height,
		  Vector *poca);

double
dem_geom_info_get_slant_range_value(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_imaging_time(DEMGeomInfo * self, int x, int y);

// returns a vector that should be freed, with vector_free
Vector *
dem_geom_info_get_cp_target(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_x(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_y(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_z(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_nadir_distance(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_satellite_height(DEMGeomInfo * self, int x, int y);

double
dem_geom_info_get_dem_height(DEMGeomInfo * self, int x, int y);

void
dem_geom_info_free(DEMGeomInfo * self);

#endif
