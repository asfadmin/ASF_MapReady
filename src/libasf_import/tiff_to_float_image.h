// Functionality to produce FloatImage instances from open TIFF files
// of the correct variety.

#ifndef TIFF_TO_FLOAT_IMAGE_H
#define TIFF_TO_FLOAT_IMAGE_H

#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <float_image.h>

// Produce a new FloatImage insance from tif.  It is assumed that tif
// consists of floating point data (probably it is a GeoTIFF).
FloatImage *tiff_to_float_image (TIFF *tif);

FloatImage *tiff_to_band_float_image (TIFF *tif, int num_bands);

#endif // TIFF_TO_FLOAT_IMAGE_H
