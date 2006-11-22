#ifndef BANDED_FLOAT_IMAGE_H
#define BANDED_FLOAT_IMAGE_H

#include <stdio.h>
#include <sys/types.h>

#include <glib.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_histogram.h>

#include "float_image.h"

typedef struct
{
        int nbands;
        FloatImage **images;
} BandedFloatImage;

BandedFloatImage *
banded_float_image_new(int nbands, size_t size_x, size_t size_y);

void
banded_float_image_free(BandedFloatImage *self);

float
banded_float_image_get_pixel(BandedFloatImage *self, int nband, 
                             ssize_t x, ssize_t y);

void
banded_float_image_set_pixel(BandedFloatImage *self, int nband, 
                             ssize_t x, ssize_t y, float value);

FloatImage *
banded_float_image_get_band(BandedFloatImage *self, int nband);

BandedFloatImage *
banded_float_image_new_from_model_scaled (BandedFloatImage *model,
                                          ssize_t scale_factor);

void
banded_float_image_export_as_jpeg(BandedFloatImage *self,
                                  const char *output_name);


#endif
