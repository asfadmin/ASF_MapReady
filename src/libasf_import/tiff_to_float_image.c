// Implementation of interface described in tiff_to_float_image.h.

#include "tiff_to_float_image.h"

FloatImage *
tiff_to_float_image (TIFF *tif)
{
  // Get the raster width and height of the image.
  uint32 width, height;
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);

  // Pull the actual image data out of the TIFF and store it as a
  // float_image.

  FloatImage *fim = float_image_new (width, height);

  // Allocate a buffer for a line of pixels.
  tdata_t buf = _TIFFmalloc (TIFFScanlineSize (tif));

  uint32 current_row;
  for ( current_row = 0 ; current_row < height ; current_row++ ) {
    // FIXME: last argument is irrelevant unless PlanarConfiguration
    // is equal to 2, but need to add a check to ensure this not the case.
    TIFFReadScanline (tif, buf, current_row, 0);
    uint32 current_column;
    for ( current_column = 0 ; current_column < width ; current_column++ ) {
      // FIXME: make sure this buf cruf from libTIFF really points floats.
      float_image_set_pixel (fim, current_column, current_row, 
			     ((float *) buf)[current_column]);
    }
  }

  return fim;
}
