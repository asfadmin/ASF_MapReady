// Functionality to produce FloatImage instances from open TIFF files
// of the correct variety.

#ifndef TIFF_TO_BYTE_IMAGE_H
#define TIFF_TO_BYTE_IMAGE_H

#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <uint8_image.h>

// Produce a new FloatImage insance from tif.  It is assumed that tif
// consists of floating point data (probably it is a GeoTIFF).
UInt8Image *tiff_to_byte_image (TIFF *tif);

UInt8Image *tiff_to_band_byte_image (TIFF *tif, int num_bands);

#endif // TIFF_TO_BYTE_IMAGE_H
