#ifndef _DEM_H_
#define _DEM_H_

#include "float_image.h"

typedef struct
{
    FloatImage * float_image;

    int ncols;
    int nrows;
    double xllcorner;
    double yllcorner;
    double cellsize;
    double nodata_value;
    int byte_order;
} Dem;

Dem * 
dem_new_from_file(const char *file);

double
dem_get_height(Dem * dem, double latitude, double longitude);

int
dem_contains_coordinate(Dem * dem, double latitude, double longitude);

void 
dem_free(Dem * dem);

#endif
