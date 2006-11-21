#include "banded_float_image.h"
#include "asf.h"
#include <assert.h>

static const int do_self_tests = 1;

BandedFloatImage *
banded_float_image_new(int nbands, size_t size_x, size_t size_y)
{
    BandedFloatImage *self = MALLOC(sizeof(BandedFloatImage));

    self->images = MALLOC(sizeof(FloatImage*)*nbands);
    self->nbands = nbands;

    int i;
    for (i=0; i<nbands; ++i)
        self->images[i] = float_image_new(size_x, size_y);

    return self;
}

static void
banded_image_self_test(BandedFloatImage *self)
{
    if (!do_self_tests) return;

    if (self->nbands <= 1) {
        return;
    }

    int nl = self->images[0]->size_y;
    int ns = self->images[0]->size_x;

    int i;
    for (i=1; i < self->nbands; ++i) {
        if (nl != self->images[i]->size_y)
            asfPrintError("BandedFloatImage y consistency check failed!"
                          "band #%d size=%d: band0_size=%d\n",
                          i, self->images[i]->size_y, nl);
        if (ns != self->images[i]->size_x)
            asfPrintError("BandedFloatImage x consistency check failed!"
                          "band #%d size=%d: band0_size=%d\n",
                          i, self->images[i]->size_x, ns);
    }
}

void
banded_float_image_free(BandedFloatImage *self)
{
    int i;
    for (i=0; i<self->nbands; ++i)
        float_image_free(self->images[i]);
    free(self);
}

float
banded_float_image_get_pixel(BandedFloatImage *self, int nband, 
                             ssize_t x, ssize_t y)
{
    assert(nband < self->nbands);
    banded_image_self_test(self);
    return float_image_get_pixel(self->images[nband], x, y);
}

void
banded_float_image_set_pixel(BandedFloatImage *self, int nband, 
                             ssize_t x, ssize_t y, float value)
{
    assert(nband < self->nbands);
    banded_image_self_test(self);
    float_image_set_pixel(self->images[nband], x, y, value);
}

FloatImage *
banded_float_image_get_band(BandedFloatImage *self, int nband)
{
    banded_image_self_test(self);
    return self->images[nband];
}

ssize_t
banded_float_image_get_size_x(BandedFloatImage *self)
{
    assert(self->nbands >= 1);
    banded_image_self_test(self);
    return self->images[0]->size_x;
}

ssize_t
banded_float_image_get_size_y(BandedFloatImage *self)
{
    assert(self->nbands >= 1);
    banded_image_self_test(self);
    return self->images[0]->size_y;
}

BandedFloatImage *
banded_float_image_new_from_model_scaled (BandedFloatImage *model,
                                          ssize_t scale_factor)
{    
    banded_image_self_test(model);

    if (model->nbands < 1)
        asfPrintError("banded_float_image_new_from_model_scaled: No bands!\n");

    BandedFloatImage *self = MALLOC(sizeof(BandedFloatImage));
    self->images = MALLOC(sizeof(FloatImage*)*model->nbands);

    int i;
    for (i=0; i<model->nbands; ++i)
        self->images[i] = float_image_new_from_model_scaled(model->images[i],
                                                            scale_factor);

    return self;
}

