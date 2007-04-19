// Implementation of interface described in tiff_to_float_image.h.
#include "asf.h"
#include "tiff_to_float_image.h"

FloatImage *tiff_to_float_image(TIFF *tif)
{
  FloatImage *oim = tiff_to_band_float_image(tif, 1);

  return oim;
}

FloatImage *tiff_to_band_float_image (TIFF *tif, int num_bands)
{
  // Get the raster width and height of the image.
  uint32  width;
  uint32  height;
  tsize_t scanlineSize;
  uint16  planarConfiguration;
  uint16  bitsPerSample;
  uint16  sampleFormat;
  int band;
  int offset;
  float   *sample;

  // Get TIFF image boundary, planar configuration, and data type info
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);
  TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarConfiguration);
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsPerSample);
  TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sampleFormat);
  if (num_bands > 1) {
    asfRequire(planarConfiguration == PLANARCONFIG_CONTIG,
              "\nTIFFs with multi-plane data not supported\n");
  }
  if (sampleFormat != SAMPLEFORMAT_UINT  &&
      sampleFormat != SAMPLEFORMAT_INT   &&
      sampleFormat != SAMPLEFORMAT_IEEEFP)
  {
    asfPrintWarning("TIFFTAG_SAMPLEFORMAT is missing or an unsupported type.  The import\n"
        "will continue but the data type will be assumed according to how many\n"
        "bits per sample exist.  This may cause unexpected results:\n"
        "    8-bits: Unsigned Integer8 (uint8) data is assumed.\n"
        "   16-bits: Signed Integer16 (int16) data is assumed.\n"
        "   32-bits: IEEE 32-Bit Floating Point (float) is assumed.\n"
        "     Other: Unsupported.\n");
  }

  // Pull the actual image data out of the TIFF and store it as a
  // float_image.  Note: float_image_new() asserts if it fails on a g_new()
  FloatImage *fim = float_image_new (width, height * num_bands);
  offset = height;

  // Allocate a buffer for a line of pixels.
  scanlineSize = TIFFScanlineSize(tif);
  tdata_t buf = _TIFFmalloc (scanlineSize);
  sample = (float*)MALLOC(sizeof(float*)*num_bands);

  uint32 current_row;
  for (current_row = 0 ; current_row < height; current_row++) {
    asfLineMeter(current_row, height);
    TIFFReadScanline (tif, buf, current_row, 0);
    uint32 current_column;
    for (current_column = 0 ; current_column < width ; current_column++) {
      // Read chunky-formatted data, e.g. greyscale values or rgb interlaced values
      switch(bitsPerSample) {
        case 8:
          for (band = 0; band < num_bands; band++) {
            sample[band] =
                (float)(((uint8*)buf)[(current_column*num_bands)+band]);
          }
          break;
        case 16:
          for (band = 0; band < num_bands; band++) {
            sample[band] =
                (float)(((uint16*)buf)[(current_column*num_bands)+band]);
          }
          break;
        case 32:
          for (band = 0; band < num_bands; band++) {
            sample[band] =
                (float)(((float*)buf)[(current_column*num_bands)+band]);
          }
          break;
        default:
          asfPrintError("\nUnsupported TIFF pixel data type\n");
      }
      for (band = 0; band < num_bands; band++) {
        // Write a band-sequential float image file for ASF internal use
        float_image_set_pixel (fim, current_column, (band*offset)+current_row,
                               sample[band]);
      }
    }
  }

  return fim;
}
































