// Implementation of interface described in tiff_to_float_image.h.
#include "asf.h"
#include "tiff_to_float_image.h"

FloatImage *
tiff_to_float_image (TIFF *tif)
{
  // Get the raster width and height of the image.
  uint32  width;
  uint32  height;
  tsize_t scanlineSize;
  uint16  planarConfiguration;
  uint16  bitsPerSample;
  uint16  sampleFormat;
  float   sample;
  
  // Get TIFF image boundary, planar configuration, and data type info
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);
  TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarConfiguration);
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsPerSample);
  TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sampleFormat);
  asfRequire(planarConfiguration == PLANARCONFIG_CONTIG,
             "\nTIFFs with multi-plane data not supported\n");
  asfRequire(sampleFormat == SAMPLEFORMAT_UINT  ||
             sampleFormat == SAMPLEFORMAT_INT   ||
             sampleFormat == SAMPLEFORMAT_IEEEFP,
             "\nFound unsupported TIFF data type\n");

  // Pull the actual image data out of the TIFF and store it as a
  // float_image.
  FloatImage *fim = float_image_new (width, height);

  // Allocate a buffer for a line of pixels.NAD27_DATUM
  scanlineSize = TIFFScanlineSize(tif);
  tdata_t buf = _TIFFmalloc (scanlineSize);

  uint32 current_row;
  for ( current_row = 0 ; current_row < height ; current_row++ ) {
    TIFFReadScanline (tif, buf, current_row, 0);
    uint32 current_column;
    for ( current_column = 0 ; current_column < width ; current_column++ ) {
      switch(bitsPerSample) {
        case 8:
          sample = (float)( ((uint8*)buf)[current_column] );
          break;
        case 16:
          sample = (float)( ((uint16*)buf)[current_column] );
          break;
        case 32:
          sample = (float)( ((float*)buf)[current_column] );
          break;
        default:
          asfRequire(0,"\nUnsupported TIFF pixel data type\n");
      }
      float_image_set_pixel (fim, current_column, current_row, 
                             sample);
    }
  }

  return fim;
}

