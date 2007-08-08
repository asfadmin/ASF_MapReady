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
  uint16  samplesPerPixel;
  uint16  extraSamples = 0; // '0' means 'unspecified'
  float   *sample;
  int band;
  int offset;

  if (tif == NULL) {
    asfPrintError("tiff_to_float_image() ...TIFF file not open.\n");
  }

  // Get TIFF image boundary, planar configuration, and data type info
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);
  TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarConfiguration);
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsPerSample);
  TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sampleFormat);
  TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesPerPixel);
  asfRequire(planarConfiguration == PLANARCONFIG_CONTIG,
             "TIFFs with multi-plane data not supported\n");
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
  //asfRequire(bitsPerSample == 8,
             //"Bits per sample in TIFF file is not 8 (byte data)\n");
  asfRequire(samplesPerPixel == 1,
             "Samples per pixel not equal to 1 (single-plane byte image)\n");
  // The following is for when we start supporting ingest of RGB images... the
  // asfRequire() above should nuke the process before getting to this check tho'
  if (samplesPerPixel > 3) {
    TIFFGetField(tif, TIFFTAG_EXTRASAMPLES, &extraSamples);
    if (!(extraSamples == EXTRASAMPLE_ASSOCALPHA || extraSamples == EXTRASAMPLE_UNASSALPHA)) {
      asfPrintWarning("Found RGBA data type with undefined 4th channel\n");
    }
  }

  // Pull the actual image data out of the TIFF and store it as a
  // float_image.  Note: float_image_new() asserts if it fails on a g_new()
  FloatImage *fim = float_image_new (width, height * num_bands);
  offset = height;
  asfRequire (fim != NULL,
              "Unable to allocate float image (out of memory?)\n");

  // Allocate a buffer for a line of pixels.
  scanlineSize = TIFFScanlineSize(tif);
  if (scanlineSize <= 0) {
    asfPrintError("Found invalid scanline length in TIFF file (%ld bytes)\n"
                  "It's possible that your TIFF is a tiled tiff ...Tiled tiffs\n"
                  "are not yet supported (but will be in the future)\n",
                  (long)scanlineSize);
  }
  tdata_t buf = _TIFFmalloc (scanlineSize);
  sample = (float*)MALLOC(sizeof(float*)*num_bands);

  asfPrintStatus("\n");
  uint32 current_row;
  int ret;
  for (current_row = 0 ; current_row < height; current_row++) {
    asfLineMeter(current_row, height);
    ret = TIFFReadScanline (tif, buf, current_row, 0);
    asfRequire (ret != -1,
                "tiff_to_float_image()::TIFFReadScanline() failed!\n");
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
    asfLineMeter(current_row, height);
  }
  _TIFFfree(buf);

  return fim;
}
































