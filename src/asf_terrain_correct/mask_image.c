#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <glib.h>

#include "mask_image.h"
#include "asf.h"

MaskImage *
mask_image_new (int size_x, int size_y)
{
  MaskImage *self = g_new (MaskImage, 1);

  g_assert(size_x > 0);
  g_assert(size_y > 0);

  self->size_x = size_x;
  self->size_y = size_y;

  self->data = g_new0 (unsigned char, size_x * size_y);

  return self;
}

MaskImage *
mask_image_new_from_file (const char * filename)
{
  printf("%s", filename);
  return NULL;
}

int
mask_image_get_pixel (MaskImage *self, int x, int y)
{
  g_assert(x >= 0);
  g_assert(x < self->size_x);

  g_assert(y >= 0);
  g_assert(y < self->size_y);

  return (int) self->data[y * self->size_x + x];
}

void
mask_image_set_pixel (MaskImage *self, int x, int y, int value)
{
  g_assert(x >= 0);
  g_assert(x < self->size_x);

  g_assert(y >= 0);
  g_assert(y < self->size_y);

  g_assert(value < 32);
  self->data[y * self->size_x + x] = (unsigned char) value;
}

void
mask_image_set_pixel_shadow (MaskImage *self, int x, int y)
{
  mask_image_set_pixel(self, x, y, MASK_SHADOW_ACTIVE);
}

void
mask_image_set_pixel_layover (MaskImage *self, int x, int y)
{
  mask_image_set_pixel(self, x, y, MASK_LAYOVER_ACTIVE);
}

void
mask_image_set_pixel_no_dem_data (MaskImage *self, int x, int y)
{
  mask_image_set_pixel(self, x, y, MASK_NO_DEM_DATA);
}

void
mask_image_store (MaskImage *self, const char *filename)
{
  // FIXME
  printf("%s %p\n", filename, self);
}

static void rgb_set(RGBDATA *d, unsigned char red, unsigned char green,
		    unsigned char blue)
{
  d->red = red;
  d->green = green;
  d->blue = blue;
}

int
mask_image_export_as_ppm (MaskImage *self, const char *filename)
{
  // export using this color scheme:
  //   0 = black
  //   1 = grey
  //   2 = dark grey
  //   3 = blue
  //   4 = green
  //   5 = red
  //   6 = yellow
  //   7 = purple
  RGBDATA table[32];
  int i;

  for (i = 0; i < 32; ++i)
    rgb_set(&table[i], 0, 0, 0);
  
  rgb_set(&table[1], 128, 128, 128);   // NO_DEM_DATA
  rgb_set(&table[2], 0,   255, 255);    // BACKGROUND_FILL
  rgb_set(&table[3], 0,   128,   0);   // LAYOVER_ACTIVE
  rgb_set(&table[4], 128, 128,   0);   // LAYOVER_PASSIVE
  rgb_set(&table[5], 128, 0,     0);   // SHADOW_ACTIVE
  rgb_set(&table[6], 128, 0,   128);   // SHADOW_PASSIVE
//  rgb_set(&table[6], 128, 0,   0);     
//  rgb_set(&table[7], 128, 0,   128);
//  rgb_set(&table[8], 255, 0,   255);

  FILE * fout = FOPEN(filename, "w");

  // PPM header
  char ppmbuf[256];
  sprintf(ppmbuf, "P6 %d %d 255\n", self->size_x, self->size_y);
  i = strlen(ppmbuf);
  fwrite(ppmbuf, sizeof(char), i, fout);

  RGBDATA *out = (RGBDATA *)
    MALLOC(self->size_x * self->size_y * sizeof(RGBDATA));

  int len = self->size_x * self->size_y - 1;
  for (i = 0; i < len; ++i)
    out[i] = table[self->data[i]];

  fwrite(out, sizeof(RGBDATA), len, fout);
  fclose(fout);

  return TRUE;
}

int
mask_image_export_as_ppm_with_transparency (MaskImage *self,
					    const char *filename,
					    unsigned char *ivals)
{
  RGBDATA table[32];
  int i;

  for (i = 0; i < 32; ++i)
    rgb_set(&table[i], 0, 0, 0);
  
  rgb_set(&table[1], 32,  32,  32);
  rgb_set(&table[2], 64,  64,  64);
  rgb_set(&table[3], 0,   0,   0);
  rgb_set(&table[4], 0,   128,   0);
  rgb_set(&table[5], 128,  128,  0);
//  rgb_set(&table[5], 255,  255,  255);
  rgb_set(&table[6], 128, 0,   0);
  rgb_set(&table[7], 128, 0,   128);
  rgb_set(&table[8], 255, 0,   255);

  FILE * fout = FOPEN(filename, "w");

  // PPM header
  char ppmbuf[256];
  sprintf(ppmbuf, "P6 %d %d 255\n", self->size_x, self->size_y);
  i = strlen(ppmbuf);
  fwrite(ppmbuf, sizeof(char), i, fout);

  RGBDATA *out = (RGBDATA *)
    MALLOC(self->size_x * self->size_y * sizeof(RGBDATA));

  int len = self->size_x * self->size_y - 1;
  for (i = 0; i < len; ++i)
  {
    out[i].red = table[self->data[i]].red | ivals[i];
    out[i].green = table[self->data[i]].green | ivals[i];
    out[i].blue = table[self->data[i]].blue | ivals[i];
  }

  fwrite(out, sizeof(RGBDATA), len, fout);
  fclose(fout);

  return TRUE;
}

void
mask_image_free (MaskImage *self)
{
  if (self)
  {
    if (self->data)
    {
      g_free(self->data);
    }
    g_free(self);
  }
}



