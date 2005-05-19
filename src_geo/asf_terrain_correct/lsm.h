
#ifndef LSM_H
#define LSM_H

#include "float_image.h"
#include "dem_geom_info.h"

typedef enum {
  MASK_NORMAL_VALUE = 0,
  MASK_ACTIVE_LAYOVER_VALUE = 1,
  MASK_PASSIVE_LAYOVER_VALUE = 2,
  MASK_ACTIVE_SHADOW_VALUE = 3,
  MASK_PASSIVE_SHADOW_VALUE = 4
} lsm_mask_value_t;

int
lsm_mask_value_is_layover(int mask_value);

int
lsm_mask_value_is_shadow(int mask_value);

int
lsm_image_mask_value_is_layover(FloatImage *mask, int row, int col);

int
lsm_image_mask_value_is_shadow(FloatImage *mask, int row, int col);

FloatImage *
lsm_generate_mask(DEMGeomInfo *dgi);

#endif
