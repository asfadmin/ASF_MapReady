#ifndef MASK_IMAGE_H
#define MASK_IMAGE_H

#include <stdio.h>
#include "float_image.h"

enum MaskValues
{
  MASK_NORMAL_VALUE = 0,
  MASK_NO_DEM_DATA = 1,
  MASK_BACKGROUND_FILL = 2,
  MASK_LAYOVER_ACTIVE = 3,
  MASK_LAYOVER_PASSIVE = 4,
  MASK_SHADOW_ACTIVE = 5,
  MASK_SHADOW_PASSIVE = 6,
  MASK_UNKNOWN_VALUE = 255,
};

typedef struct {
    unsigned char red;
    unsigned char green;
    unsigned char blue;
} RGBDATA;

typedef struct {
    // public: you may look at these
    int size_x, size_y;  // Image dimensions
    
    // private: don't query directly, use mask_image_get/set
    unsigned char *data;
} MaskImage;

///////////////////////////////////////////////////////////////////////////////
//
// Creating New Instances
//
// Including methods which create new instances by copying existing
// ones.
//
///////////////////////////////////////////////////////////////////////////////

MaskImage *
mask_image_new (int size_x, int size_y);

MaskImage *
mask_image_new_from_file (const char * filename);

///////////////////////////////////////////////////////////////////////////////
//
// Getting and Setting Mask Pixels and Regions
//
///////////////////////////////////////////////////////////////////////////////

int
mask_image_get_pixel (MaskImage *self, int x, int y);

void
mask_image_set_pixel (MaskImage *self, int x, int y, int value);

void
mask_image_set_pixel_shadow (MaskImage *self, int x, int y);

void
mask_image_set_pixel_layover (MaskImage *self, int x, int y);

void
mask_image_set_pixel_no_dem_data (MaskImage *self, int x, int y);

///////////////////////////////////////////////////////////////////////////////
//
// Storing Masks in Files
//
///////////////////////////////////////////////////////////////////////////////

void
mask_image_store (MaskImage *self, const char *filename);

///////////////////////////////////////////////////////////////////////////////
//
// Exporting Images in Various Image File Formats
//
///////////////////////////////////////////////////////////////////////////////

int
mask_image_export_as_ppm (MaskImage *self, const char *filename);

int
mask_image_export_as_ppm_with_transparency (MaskImage *self,
					    const char *filename,
					    FloatImage *f);

///////////////////////////////////////////////////////////////////////////////
//
// Freeing Instances
//
///////////////////////////////////////////////////////////////////////////////

void
mask_image_free (MaskImage *self);

#endif // #ifndef MASK_IMAGE_H
