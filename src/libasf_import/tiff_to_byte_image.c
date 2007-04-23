// Implementation of interface described in tiff_to_float_image.h.
#include "asf.h"
#include "tiff_to_byte_image.h"

UInt8Image *tiff_to_byte_image(TIFF *tif)
{
  UInt8Image *oim_b = tiff_to_band_byte_image(tif, 1);

  return oim_b;
}

UInt8Image *tiff_to_band_byte_image (TIFF *tif, int num_bands)
{
  // Get the raster width and height of the image.
  uint32  width;
  uint32  height;
  tsize_t scanlineSize;
  uint16  planarConfiguration;
  uint16  bitsPerSample;
  uint16  sampleFormat;
  int band;
  uint32  offset;
  uint8   *sample;

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

  // FIXME: Use resampling to reduce 16- and 32-bit data down to 8-bit byte data...
  // This situation/need is not expected to happen, or at least very rarely.
  if (bitsPerSample > 8) {
    asfPrintWarning("Image contains 16- or 32-bit data but is being imported as an 8-bit (byte) image.\n"
        "Truncation of data values may occur.\n");
  }

  // Pull the actual image data out of the TIFF and store it as a
  // byte image.
  UInt8Image *bim = uint8_image_new (width, height * num_bands);
  if (bim == NULL) return bim; // But note to you: uint8_image_new() asserts on a g_new() if it fails
  offset = height;

  // Allocate a buffer for a line of pixels.
  scanlineSize = TIFFScanlineSize(tif);
  tdata_t buf = _TIFFmalloc (scanlineSize);
  sample = (uint8*)MALLOC(sizeof(uint8)*num_bands);

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
                (uint8)(((uint8*)buf)[(current_column*num_bands)+band]);
          }
          break;
        case 16:
          for (band = 0; band < num_bands; band++) {
            sample[band] =
                (uint8)(((uint16*)buf)[(current_column*num_bands)+band]);
          }
          break;
        case 32:
          for (band = 0; band < num_bands; band++) {
            sample[band] =
                (uint8)(((float*)buf)[(current_column*num_bands)+band]);
          }
          break;
        default:
          asfPrintError("\nUnsupported TIFF pixel data type\n");
      }
      for (band = 0; band < num_bands; band++) {
        // Write a band-sequential float image file for ASF internal use
        uint8_image_set_pixel (bim, current_column, (band*offset)+current_row,
                               sample[band]);
      }
    }
  }
  if (buf) _TIFFfree(buf);
  FREE(sample);

  return bim;
}
































