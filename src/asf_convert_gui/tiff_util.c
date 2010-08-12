#include "tiff_util.h"

static int read_tiff_rgb_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                            uint32 row, uint32 scanlineSize, int sample_count,
                            int band_r, int band_g, int band_b,
                            tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf);
static int ReadScanline_from_ContiguousRGB_TIFF(TIFF *tiff, uint32 row, uint32 sample_count,
                                         int band_r, int band_g, int band_b,
                                         tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf);
static int read_tiff_greyscale_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                                  uint32 row, uint32 scanlineSize, int sample_count, int band,
                                  tdata_t *tif_buf);
static int interleave_byte_rgbScanlines_to_byte_buff(unsigned char *dest,
                                              tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf,
                                              int band_r, int band_g, int band_b,
                                              uint32 row, uint32 sample_count,
                                              tiff_data_config_t *data_config);
static int copy_byte_scanline_to_byte_buff(unsigned char *dest, tdata_t *tif_buf,
                                    uint32 row, uint32 sample_count, tiff_data_config_t *data_config);

int read_tiff(const char *filename, int *nlines, int *nsamps,
              unsigned char **data)
{
  TIFF *tiff = XTIFFOpen(filename, "r");
  if (!tiff) {
    return FALSE;
  }

  uint32 width;
  uint32 height;
  TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &height);
  TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &width);

  *nlines = (int)height;
  *nsamps = (int)width;

  unsigned char *dest = CALLOC(width*height*3, sizeof(unsigned char));

  data_type_t data_type;
  tiff_data_config_t data_config;
  int num_bands, is_scanline_format, is_palette_color_tiff;
  uint32 row;

  // Determine what type of TIFF this is (scanline/strip/tiled)
  if (get_tiff_data_config(tiff,
                           &data_config.sample_format,
                           &data_config.bits_per_sample,
                           &data_config.planar_config,
                           &data_type,
                           &data_config.samples_per_pixel,
                           &is_scanline_format,
                           &is_palette_color_tiff,
                           REPORT_LEVEL_NONE))
  {
    return FALSE;
  }
  num_bands = data_config.samples_per_pixel;

  tiff_type_t tiffInfo;
  get_tiff_type(tiff, &tiffInfo);
  if (tiffInfo.imageCount > 1) {
    ; // Only first image in multi-image files will be utilized - WARN the user here?
  }
  if (tiffInfo.imageCount < 1) {
    // TIFF contains zero images ...fail
    return FALSE;
  }
  if (tiffInfo.format != SCANLINE_TIFF &&
      tiffInfo.format != STRIP_TIFF    &&
      tiffInfo.format != TILED_TIFF)
  {
    // Unrecognized TIFF type
    return FALSE;
  }
  if (tiffInfo.volume_tiff) {
    // 3-dimensional (a 'volume tiff') found ...this is unsupported
    return FALSE;
  }
  if (num_bands > 1 &&
      data_config.planar_config != PLANARCONFIG_CONTIG &&
      data_config.planar_config != PLANARCONFIG_SEPARATE)
  {
    // Invalid planar configuration setting found in TIFF file...
    return FALSE;
  }

  uint32 scanlineSize = TIFFScanlineSize(tiff);
  if (scanlineSize <= 0) {
    // Invalid scanline size found in TIFF file...
    return FALSE;
  }

  if (data_config.bits_per_sample != 8)
    return FALSE;

  int is_rgb = data_config.samples_per_pixel >= 3;

  // Populate the buffer with actual data
  if (is_rgb) {
    // TIFF read buffer (red band)
    tdata_t *rtif_buf = _TIFFmalloc(scanlineSize);
    // TIFF read buffer (green band) 
    tdata_t *gtif_buf = _TIFFmalloc(scanlineSize); 
    // TIFF read buffer (blue band)
    tdata_t *btif_buf = _TIFFmalloc(scanlineSize); 

    if (!rtif_buf || !gtif_buf || !btif_buf)
      return FALSE;

    for (row=0; row < height; row++) {
      // Read a scanline and populate r, g, and b tiff buffers
      // NOTE: Empty bands will have the no_data value populated in the
      //tiff buffer
      read_tiff_rgb_scanline(tiff, tiffInfo.format, &data_config,
                             row, scanlineSize, width,
                             0, 1, 2,
                             rtif_buf, gtif_buf, btif_buf);
      // Interleave the rgb values into an rgb buffer
      interleave_byte_rgbScanlines_to_byte_buff(dest,
                                                rtif_buf, gtif_buf, btif_buf,
                                                0, 1, 2,
                                                row, width, &data_config);
    }

    _TIFFfree(rtif_buf);
    _TIFFfree(gtif_buf);
    _TIFFfree(btif_buf);
  }
  else { // is greyscale
    // TIFF read buffer (interleaved bands)
    tdata_t *tif_buf  = _TIFFmalloc(scanlineSize); 
    if (!tif_buf)
      return FALSE;

    for (row=0; row < height; row++) {
      // Read a scanline into a tiff buffer (using first non-blank band as
      // the greyscale image)
      // NOTE: Since displaying a greyscale band specifically selects a band,
      // empty or not, the selected band is read as-is.
      read_tiff_greyscale_scanline(tiff, tiffInfo.format, &data_config,
                                   row, scanlineSize, width, 0, tif_buf);
      copy_byte_scanline_to_byte_buff(dest, tif_buf,
                                      row, width, &data_config);
    }

    _TIFFfree(tif_buf);
  }

  *data = dest;
  return TRUE;
}

static int read_tiff_rgb_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                            uint32 row, uint32 scanlineSize, int sample_count,
                            int band_r, int band_g, int band_b,
                            tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf)
{
  // NOTE: num_bands may not be greater than 1 ...open_tiff_data() decides what to
  // assign into band_r, band_g, and band_b.  They may all be the same, all different,
  // or some combination depending on what the GUI asked for versus what was available
  // in the TIFF file.  All code called after open_tiff_data() should assume that if
  // RGB is desired that the 3 band assignments are taken care of appropriate to the
  // situation.
  int num_bands = data_config->samples_per_pixel;
  if (num_bands < 1 ||
      band_r < 0 || band_r > num_bands - 1 ||
      band_g < 0 || band_g > num_bands - 1 ||
      band_b < 0 || band_b > num_bands - 1)
  {
    return FALSE;
  }
  switch (format) {
    case SCANLINE_TIFF:
      if (data_config->planar_config == PLANARCONFIG_CONTIG) {
        ReadScanline_from_ContiguousRGB_TIFF(tiff, row, sample_count,
                                             band_r, band_g, band_b,
                                             rtif_buf, gtif_buf, btif_buf);
      }
      else {
        TIFFReadScanline(tiff, rtif_buf, row, band_r); // Red band
        TIFFReadScanline(tiff, gtif_buf, row, band_g); // Green band
        TIFFReadScanline(tiff, btif_buf, row, band_b); // Blue band
      }
      break;
    case STRIP_TIFF:
      ReadScanline_from_TIFF_Strip(tiff, rtif_buf, row, band_r); // Red band
      ReadScanline_from_TIFF_Strip(tiff, gtif_buf, row, band_g); // Green band
      ReadScanline_from_TIFF_Strip(tiff, btif_buf, row, band_b); // Blue band
      break;
    case TILED_TIFF:
      ReadScanline_from_TIFF_TileRow(tiff, rtif_buf, row, band_r); // Red band
      ReadScanline_from_TIFF_TileRow(tiff, gtif_buf, row, band_g); // Green band
      ReadScanline_from_TIFF_TileRow(tiff, btif_buf, row, band_b); // Blue band
      break;
    default:
      // This code should never execute
      return FALSE;
      break;
  }

  return TRUE;
}

static int read_tiff_greyscale_scanline (TIFF *tiff, tiff_format_t format, tiff_data_config_t *data_config,
                                  uint32 row, uint32 scanlineSize, int sample_count, int band,
                                  tdata_t *tif_buf)
{
  switch (format) {
    case SCANLINE_TIFF:
      if (data_config->planar_config == PLANARCONFIG_CONTIG) {
        TIFFReadScanline(tiff, tif_buf, row, 0);
      }
      else {
        TIFFReadScanline(tiff, tif_buf, row, band);
      }
      break;
    case STRIP_TIFF:
      ReadScanline_from_TIFF_Strip(tiff, tif_buf, row, band);
      break;
    case TILED_TIFF:
      ReadScanline_from_TIFF_TileRow(tiff, tif_buf, row, band);
      break;
    default:
      // This code should never execute
      return FALSE;
      break;
  }

  return TRUE;
}

static int interleave_byte_rgbScanlines_to_byte_buff(unsigned char *dest,
                                              tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf,
                                              int band_r, int band_g, int band_b,
                                              uint32 row, uint32 sample_count, tiff_data_config_t *data_config)
{
  if (data_config->bits_per_sample != 8) return FALSE; // Bail if not byte data

  int sample;
  int spp = 3;
  int ns = sample_count;
  for (sample=0; sample < sample_count; sample++) {
    switch(data_config->sample_format) {
      case SAMPLEFORMAT_UINT:
        dest[row*ns*spp+sample*spp]   = (unsigned char)(((uint8 *)rtif_buf)[sample]); // Red component
        dest[row*ns*spp+sample*spp+1] = (unsigned char)(((uint8 *)gtif_buf)[sample]); // Green component
        dest[row*ns*spp+sample*spp+2] = (unsigned char)(((uint8 *)btif_buf)[sample]); // Blue component
        break;
      case SAMPLEFORMAT_INT:
        dest[row*ns*spp+sample*spp]   = (unsigned char)(((int8 *)rtif_buf)[sample]); // Red component
        dest[row*ns*spp+sample*spp+1] = (unsigned char)(((int8 *)gtif_buf)[sample]); // Green component
        dest[row*ns*spp+sample*spp+2] = (unsigned char)(((int8 *)btif_buf)[sample]); // Blue component
        break;
      default:
        // No such thing as an 8-bit IEEE float
        return FALSE;
        break;
    }
  }

  return TRUE;
}

// copy_byte_scanline_to_byte_buff() is for greyscale only, i.e. tif_buff is an array of bytes,
// one greyscale byte per pixel.  The destination buffer is also a byte buffer.
static int copy_byte_scanline_to_byte_buff(unsigned char *dest, tdata_t *tif_buf,
                                    uint32 row, uint32 sample_count, tiff_data_config_t *data_config)
{
  int sample;
  if (data_config->bits_per_sample != 8) return FALSE;

  for (sample=0; sample<sample_count; sample++) {
    switch (data_config->sample_format) {
      case SAMPLEFORMAT_UINT:
        dest[row*sample_count+sample] = (unsigned char)(((uint8 *)tif_buf)[sample]);
        break;
      case SAMPLEFORMAT_INT:
        dest[row*sample_count+sample] = (unsigned char)(((int8 *)tif_buf)[sample]);
        break;
      default:
        return FALSE;
        break;
      }
  }

  return TRUE;
}

// ReadScanline_from_ContiguousRGB_TIFF()
//
//   For scanline type tiffs in contiguous RGB format (scanlines are rgbrgb...) only
//   ...Not for striped or tiled tiffs.  See:
//
//        ReadScanline_from_TIFF_Strip() and
//        ReadScanline_from_TIFF_TileRow()
//
//   respectively for those needs.  For RGB tiffs with separate color planes, just
//   use:
//
//        TIFFReadScanline()
//
//   from the TIFF library directly.
//

static int ReadScanline_from_ContiguousRGB_TIFF(TIFF *tiff, uint32 row, uint32 sample_count,
                                         int band_r, int band_g, int band_b,
                                         tdata_t *rtif_buf, tdata_t *gtif_buf, tdata_t *btif_buf)
{
  data_type_t data_type;
  tiff_data_config_t data_config;
  int num_bands, is_scanline_format, is_palette_color_tiff;

  // Determine what type of TIFF this is (scanline/strip/tiled)
  if (get_tiff_data_config(tiff,
      &data_config.sample_format,
      &data_config.bits_per_sample,
      &data_config.planar_config,
      &data_type,
      &data_config.samples_per_pixel,
      &is_scanline_format,
      &is_palette_color_tiff,
      REPORT_LEVEL_NONE))
  {
    return FALSE;
  }
  num_bands = data_config.samples_per_pixel;
  if (num_bands < 3) {
    return FALSE;
  }
  if (data_config.planar_config != PLANARCONFIG_CONTIG) {
    return FALSE;
  }
  uint32 scanlineSize = TIFFScanlineSize(tiff);
  if (scanlineSize <= 0) {
    return FALSE;
  }
  tdata_t *tif_buf = _TIFFmalloc(scanlineSize);

  TIFFReadScanline(tiff, tif_buf, row, 0); // Read RGB scanline (the band number is ignored for PLANARCONFIG_CONTIG)
  int s;
  for (s=0; s<sample_count; s++) {
    switch(data_config.bits_per_sample) {
      case 8:
        switch(data_config.sample_format) {
          case SAMPLEFORMAT_UINT:
            ((uint8*)rtif_buf)[s] = (uint8)(((uint8*)tif_buf)[s*num_bands+band_r]);
            ((uint8*)gtif_buf)[s] = (uint8)(((uint8*)tif_buf)[s*num_bands+band_g]);
            ((uint8*)btif_buf)[s] = (uint8)(((uint8*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_INT:
            ((int8*)rtif_buf)[s] = (int8)(((int8*)tif_buf)[s*num_bands+band_r]);
            ((int8*)gtif_buf)[s] = (int8)(((int8*)tif_buf)[s*num_bands+band_g]);
            ((int8*)btif_buf)[s] = (int8)(((int8*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_IEEEFP:
          default:
            _TIFFfree(tif_buf);
            return FALSE;
            break;
        }
        break;
      case 16:
        switch(data_config.sample_format) {
          case SAMPLEFORMAT_UINT:
            ((uint16*)rtif_buf)[s] = (uint16)(((uint16*)tif_buf)[s*num_bands+band_r]);
            ((uint16*)gtif_buf)[s] = (uint16)(((uint16*)tif_buf)[s*num_bands+band_g]);
            ((uint16*)btif_buf)[s] = (uint16)(((uint16*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_INT:
            ((int16*)rtif_buf)[s] = (int16)(((int16*)tif_buf)[s*num_bands+band_r]);
            ((int16*)gtif_buf)[s] = (int16)(((int16*)tif_buf)[s*num_bands+band_g]);
            ((int16*)btif_buf)[s] = (int16)(((int16*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_IEEEFP:
          default:
            _TIFFfree(tif_buf);
            return FALSE;
            break;
        }
        break;
      case 32:
        switch(data_config.sample_format) {
          case SAMPLEFORMAT_UINT:
            ((uint32*)rtif_buf)[s] = (uint32)(((uint32*)tif_buf)[s*num_bands+band_r]);
            ((uint32*)gtif_buf)[s] = (uint32)(((uint32*)tif_buf)[s*num_bands+band_g]);
            ((uint32*)btif_buf)[s] = (uint32)(((uint32*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_INT:
            ((long*)rtif_buf)[s] = (long)(((long*)tif_buf)[s*num_bands+band_r]);
            ((long*)gtif_buf)[s] = (long)(((long*)tif_buf)[s*num_bands+band_g]);
            ((long*)btif_buf)[s] = (long)(((long*)tif_buf)[s*num_bands+band_b]);
            break;
          case SAMPLEFORMAT_IEEEFP:
            ((float*)rtif_buf)[s] = (float)(((float*)tif_buf)[s*num_bands+band_r]);
            ((float*)gtif_buf)[s] = (float)(((float*)tif_buf)[s*num_bands+band_g]);
            ((float*)btif_buf)[s] = (float)(((float*)tif_buf)[s*num_bands+band_b]);
            break;
          default:
            _TIFFfree(tif_buf);
            return FALSE;
            break;
        }
        break;
      default:
        _TIFFfree(tif_buf);
        return FALSE;
        break;
    }
  }
  _TIFFfree(tif_buf);

  return TRUE;
}
