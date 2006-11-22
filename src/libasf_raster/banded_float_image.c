#include "banded_float_image.h"
#include "asf.h"
#include <assert.h>
#include <jpeglib.h>

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

static int scale_to_byte(float lin_min, float lin_max, float val)
{
    if (val < lin_min)
        return 0;
    if (val > lin_max)
        return 255;
    return (int) (.5 + (val - lin_min)/(lin_max - lin_min) * 255);
}

void
banded_float_image_export_as_jpeg(BandedFloatImage *self, const char *output_name)
{
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  int i;

  assert(self->nbands >= 1);

  float *min, *max, *mean, *stddev, *lin_min, *lin_max;
  min = MALLOC(sizeof(float)*self->nbands);
  max = MALLOC(sizeof(float)*self->nbands);
  mean = MALLOC(sizeof(float)*self->nbands);
  stddev = MALLOC(sizeof(float)*self->nbands);

  for (i=0; i<self->nbands; ++i) {
      float_image_statistics(self->images[i], &min[i], &max[i], &mean[i], &stddev[i], -999);
      lin_min[i] = mean[i] - 2 * stddev[i];
      lin_max[i] = mean[i] + 2 * stddev[i];
  }

  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_compress (&cinfo);

  FILE *ofp = fopen (output_name, "w");
  if ( ofp == NULL ) {
    asfPrintError("Open of %s for writing failed: %s",
                  output_name, strerror(errno));
  }

  jpeg_stdio_dest (&cinfo, ofp);

  int nl = banded_float_image_get_size_y(self);
  int ns = banded_float_image_get_size_x(self);

  cinfo.image_width = ns;
  cinfo.image_height = nl;

  cinfo.input_components = 3;
  cinfo.in_color_space = JCS_RGB;

  jpeg_set_defaults (&cinfo);
  jpeg_start_compress (&cinfo, TRUE);

  JSAMPLE *jsample_row = MALLOC(sizeof(JSAMPLE)*ns*3);
  JSAMPROW *row_pointer = MALLOC(sizeof(JSAMPROW));

  while (cinfo.next_scanline < cinfo.image_height) {
      for (i=0; i<ns; ++i) {
          int band = 0;
          int r = scale_to_byte(lin_min[band], lin_max[band],
              banded_float_image_get_pixel(self, band, cinfo.next_scanline, i));

          if (band < self->nbands-1) ++band;
          int g = scale_to_byte(lin_min[band], lin_max[band],
              banded_float_image_get_pixel(self, band, cinfo.next_scanline, i));

          if (band < self->nbands-1) ++band;
          int b = scale_to_byte(lin_min[band], lin_max[band],
              banded_float_image_get_pixel(self, band, cinfo.next_scanline, i));

          jsample_row[i*3+0] = (JSAMPLE) r;
          jsample_row[i*3+1] = (JSAMPLE) g;
          jsample_row[i*3+2] = (JSAMPLE) b;
      }
      row_pointer[0] = jsample_row;
      int written = jpeg_write_scanlines(&cinfo, row_pointer, 1);
      if (written != 1)
          asfPrintError("Failed to write the correct number of lines.\n");
      asfLineMeter(cinfo.next_scanline, cinfo.image_height);
  }

  FREE(row_pointer);
  FREE(jsample_row);
  jpeg_finish_compress (&cinfo);
  FCLOSE (ofp);
  jpeg_destroy_compress (&cinfo);
}
