
#ifndef DEM_GEOM_INFO_H
#define DEM_GEOM_INFO_H

#include "vector.h"
#include "float_image.h"

typedef struct dem_geom_info {
    int nrows, ncols;
    FloatImage * cp_target_x;
    FloatImage * cp_target_y;
    FloatImage * cp_target_z;
    FloatImage * slant_range_value;
    FloatImage * imaging_time;
    FloatImage * satellite_height;
} DEMGeomInfo;

DEMGeomInfo *
dem_geom_info_new(int nrows, int ncols);

void
dem_geom_info_set(DEMGeomInfo * self,
		  int row, int col,
		  Vector *cp_target,
		  double imaging_time,
		  double slant_range_value,
		  double satellite_height);

double
dem_geom_info_get_slant_range_value(DEMGeomInfo * self,
				    int row, int col);

double
dem_geom_info_get_imaging_time(DEMGeomInfo * self,
			       int row, int col);

void
dem_geom_info_free(DEMGeomInfo * self);

#endif
