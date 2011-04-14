#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "asf_tiff.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"

#include "geotiff_support.h"

int guess_planar_configuration(TIFF *tif, short *planar_config);

int PCS_2_UTM(short pcs, char *hem, datum_type_t *datum, unsigned long *zone)
{
  // The GeoTIFF standard defines the UTM zones numerically in a way that
  // let's us pick off the data mathematically (NNNzz where zz is the zone
  // number):
  //
  // For NAD83 datums, Zones 3N through 23N, NNN == 269
  // For NAD27 datums, Zones 3N through 22N, NNN == 267
  // For WGS72 datums, Zones 1N through 60N, NNN == 322
  // For WGS72 datums, Zones 1S through 60S, NNN == 323
  // For WGS84 datums, Zones 1N through 60N, NNN == 326
  // For WGS84 datums, Zones 1S through 60S, NNN == 327
  // For user-defined UTMs, Northern Hemisphere, NNN == 160
  // For user-defined UTMs, Southern Hemisphere, NNN == 161
  // For other user-defined and/or unsupported UTM projections,
  //   then NNN can be a variety of other numbers (see the
  //   GeoTIFF Standard)
  //
  // NOTE: For NAD27 and NAD83, only the restricted range of zones
  // above is supported by the GeoTIFF standard.
  //
  const short NNN_NAD27  = 267;
  const short NNN_NAD83  = 269;
  const short NNN_WGS84N = 326;
  const short NNN_WGS84S = 327;
  const short NNN_USER_DEFINED_NORTH = 160;
  const short NNN_USER_DEFINED_SOUTH = 161;
  const short USER_DEFINED_PCS = 32767; // This code means it is NOT a UTM...
  int isUTM = 0;
  short datumClassifierNNN;

  datumClassifierNNN = pcs / 100;
  if (pcs == USER_DEFINED_PCS) {
    *hem = '\0';
    *datum = UNKNOWN_DATUM;
    *zone = 0;
    isUTM = 0;
  }
  else if (datumClassifierNNN == NNN_NAD27) {
      *hem = 'N';
      *datum = NAD27_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
      if (*zone < 3 || *zone > 22) {
        *datum = UNKNOWN_DATUM;
        *zone = 0;
        isUTM = 0;
      }
  }
  else if (datumClassifierNNN == NNN_NAD83) {
      *hem = 'N';
      *datum = NAD83_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
      if (*zone < 3 || *zone > 23) {
        *datum = UNKNOWN_DATUM;
        *zone = 0;
        isUTM = 0;
      }
  }
  else if (datumClassifierNNN == NNN_WGS84N ||
           datumClassifierNNN == NNN_WGS84S) {
      *hem = datumClassifierNNN == NNN_WGS84N ? 'N' : 'S';
      *datum = WGS84_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
      if (*zone < 1 || *zone > 60) {
        *datum = UNKNOWN_DATUM;
        *zone = 0;
        isUTM = 0;
      }
  }
  else if (datumClassifierNNN == NNN_USER_DEFINED_NORTH ||
           datumClassifierNNN == NNN_USER_DEFINED_SOUTH) {
    /*
        NOTE: For the user-defined cases, the datum should be set
        from the geokey GeographicTypeGeoKey (or possibly
        GeogGeodeticDatumGeoKey) before or after the calling of
        PCS_2_UTM(), therefore *datum is not assigned anything
        here.
    */
    *hem = datumClassifierNNN == NNN_USER_DEFINED_NORTH ? 'N' : 'S';
    *zone = pcs - (pcs / 100)*100;
    isUTM = 1;
    if (*zone < 1 || *zone > 60) {
      *datum = UNKNOWN_DATUM;
      *zone = 0;
      isUTM = 0;
    }
  }
  else {
      *hem = '\0';
      *datum = UNKNOWN_DATUM;
      *zone = 0;
      isUTM = 0;
  }

  return isUTM;
}

// NOTE: Because of the degrees-to-radians conversions, this function is ONLY for
// meta_getlatlon()...
void copy_proj_parms(meta_projection *dest, meta_projection *src)
{
  dest->type = src->type;
  dest->startX = src->startX;
  dest->startY = src->startY;
  dest->perX = src->perX;
  dest->perY = src->perY;
  strcpy (dest->units, src->units);
  dest->hem = src->hem;
  dest->spheroid = src->spheroid;
  dest->re_major = src->re_major;
  dest->re_minor = src->re_minor;
  dest->datum = src->datum;
  dest->height = src->height;

  switch (src->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      dest->param.utm.zone = src->param.utm.zone;
      dest->param.utm.lat0 = src->param.utm.lat0;
      dest->param.utm.lon0 = src->param.utm.lon0;
      dest->param.utm.false_easting = src->param.utm.false_easting;
      dest->param.utm.false_northing = src->param.utm.false_northing;
      dest->param.utm.scale_factor = src->param.utm.scale_factor;
      break;
    case POLAR_STEREOGRAPHIC:
      dest->param.ps.slat = src->param.ps.slat;
      dest->param.ps.slon = src->param.ps.slon;
      dest->param.ps.is_north_pole = src->param.ps.is_north_pole;
      dest->param.ps.false_easting = src->param.ps.false_easting;
      dest->param.ps.false_northing = src->param.ps.false_northing;
      break;
    case ALBERS_EQUAL_AREA:
      dest->param.albers.std_parallel1 = src->param.albers.std_parallel1;
      dest->param.albers.std_parallel2 = src->param.albers.std_parallel2;
      dest->param.albers.center_meridian = src->param.albers.center_meridian;
      dest->param.albers.orig_latitude = src->param.albers.orig_latitude;
      dest->param.albers.false_easting = src->param.albers.false_easting;
      dest->param.albers.false_northing = src->param.albers.false_northing;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      dest->param.lamcc.plat1 = src->param.lamcc.plat1;
      dest->param.lamcc.plat2 = src->param.lamcc.plat2;
      dest->param.lamcc.lat0 = src->param.lamcc.lat0;
      dest->param.lamcc.lon0 = src->param.lamcc.lon0;
      dest->param.lamcc.false_easting = src->param.lamcc.false_easting;
      dest->param.lamcc.false_northing = src->param.lamcc.false_northing;
      dest->param.lamcc.scale_factor = src->param.lamcc.scale_factor;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      dest->param.lamaz.center_lon = src->param.lamaz.center_lon;
      dest->param.lamaz.center_lat = src->param.lamaz.center_lat;
      dest->param.lamaz.false_easting = src->param.lamaz.false_easting;
      dest->param.lamaz.false_northing = src->param.lamaz.false_northing;
      break;
    case EQUI_RECTANGULAR:
      dest->param.eqr.orig_latitude = src->param.eqr.orig_latitude;
      dest->param.eqr.central_meridian = src->param.eqr.central_meridian;
      dest->param.eqr.false_easting = src->param.eqr.false_easting;
      dest->param.eqr.false_northing = src->param.eqr.false_northing;
      break;
    case EQUIDISTANT:
      dest->param.eqc.orig_latitude = src->param.eqc.orig_latitude;
      dest->param.eqc.central_meridian = src->param.eqc.central_meridian;
      break;
    case MERCATOR:
      dest->param.mer.orig_latitude = src->param.mer.orig_latitude;
      dest->param.mer.central_meridian = src->param.mer.central_meridian;
      dest->param.mer.standard_parallel = src->param.mer.standard_parallel;
      dest->param.mer.scale_factor = src->param.mer.scale_factor;
      dest->param.mer.false_easting = src->param.mer.false_easting;
      dest->param.mer.false_northing = src->param.mer.false_northing;
      break;
    case SINUSOIDAL:
      dest->param.sin.longitude_center = src->param.sin.longitude_center;
      dest->param.sin.false_easting = src->param.sin.false_easting;
      dest->param.sin.false_northing = src->param.sin.false_northing;
      dest->param.sin.sphere = src->param.sin.sphere;
      break;
    case STATE_PLANE:
    case LAT_LONG_PSEUDO_PROJECTION:
      break;
    default:
      asfPrintError("Unsupported projection type found.\n");
      break;
  }
}

int get_tiff_data_config(TIFF *tif,
                         short *sample_format, short *bits_per_sample, short *planar_config,
                         data_type_t *data_type, short *num_bands,
                         int *is_scanline_format, int *is_palette_color_tiff, report_level_t report_level)
{
  int     ret = 0, read_count;
  uint16  planarConfiguration = 0;
  uint16  bitsPerSample = 0;
  uint16  sampleFormat = 0;
  uint16  samplesPerPixel = 0;

  *data_type = -1;
  *num_bands = 0;

  /*********************************************************/
  /*  Read all tags and accept them as-is into the fields  */
  /*********************************************************/
  // Required tag for all images
  ret = 0;

  tiff_type_t t;
  get_tiff_type(tif, &t);
  *is_scanline_format = (t.format == SCANLINE_TIFF) ? 1 : 0;
  if (t.format != SCANLINE_TIFF &&
      t.format != STRIP_TIFF    &&
      t.format != TILED_TIFF)
  {
    ret = -1;
  }

  read_count = TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsPerSample);
  if (read_count) *bits_per_sample = bitsPerSample;

  read_count += TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sampleFormat);
  if (read_count < 2          &&
      (bitsPerSample == 8   ||
       bitsPerSample == 16  ||
       bitsPerSample == 32))
  {
      asfReport(report_level,
              "Found missing or invalid sample format.  Should be unsigned integer,\n"
              "integer, or IEEE floating point.\n");
      switch (bitsPerSample) {
          case 8:
              asfReport(report_level,
                    "Data is 8-bit data ...guessing unsigned integer sample\n"
                        "format and attempting to continue.\n");
              sampleFormat = SAMPLEFORMAT_UINT;
              read_count++;
              break;
          case 16:
              asfReport(report_level,
                    "Data is 16-bit data ...guessing signed integer sample\n"
                        "format and attempting to continue.\n");
              sampleFormat = SAMPLEFORMAT_INT;
              read_count++;
              break;
          case 32:
              asfReport(report_level,
                    "Data is 32-bit data ...guessing IEEE floating point sample\n"
                        "format and attempting to continue.\n");
              sampleFormat = SAMPLEFORMAT_IEEEFP;
              read_count++;
              break;
          default:
              ret = -1;
              break;
      }
  }
  if (read_count == 2) {
    *sample_format = sampleFormat;
    switch (sampleFormat) {
      case SAMPLEFORMAT_UINT:
      {
        switch (bitsPerSample) {
          case 8:
            *data_type = BYTE;
            break;
          case 16:
            *data_type = INTEGER32;
            break;
          case 32:
            *data_type = REAL32;
            break;
          default:
            *data_type = 0;
            break;
        }
      }
      break;
      case SAMPLEFORMAT_INT:
      {
        switch (bitsPerSample) {
          case 8:
          case 16:
            *data_type = INTEGER16;
            break;
          case 32:
            *data_type = INTEGER32;
            break;
          default:
            *data_type = 0;
            break;
        }
      }
      break;
      case SAMPLEFORMAT_IEEEFP:
      {
        *data_type = REAL32;
      }
      break;
      default:
      {
        switch (bitsPerSample) {
          case 8:
              // Most likely unsigned 8-bit byte
            *data_type = BYTE;
            break;
          case 16:
              // Most likely 16-bit integer
            *data_type = INTEGER16;
            break;
          case 32:
              // Most likely 32-bot float
            *data_type = REAL32;
            break;
          default:
            *data_type = 0;
            break;
        }
      }
      break;
    }
  }

  read_count = TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesPerPixel);
  if (read_count != 1) {
      samplesPerPixel = 0;
  }

  unsigned short *maps[3];
  read_count = TIFFGetField(tif, TIFFTAG_COLORMAP, maps+0, maps+1, maps+2);
  *is_palette_color_tiff = 0;
  if (read_count) {
    if (samplesPerPixel == 1) {
      *is_palette_color_tiff = 1;
    }
    // NOTE: Do not free the colormaps ...this results in a glib double-free error
    // when closing the tiff file.
  }

  read_count = TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarConfiguration);
  if (read_count) *planar_config = planarConfiguration;

  /*********************************************************/
  /*  Determine health                                     */
  /*********************************************************/

  if (bitsPerSample != 8  &&
      bitsPerSample != 16 &&
      bitsPerSample != 32)
  {
    // Only support byte, integer16, integer32, and 32-bit floats
    ret = -1;
  }
  else {
      *num_bands = samplesPerPixel;
  }

  if (sampleFormat != SAMPLEFORMAT_UINT &&
      sampleFormat != SAMPLEFORMAT_INT  &&
      sampleFormat != SAMPLEFORMAT_IEEEFP)
  {
    ret = -1;
  }

  if (samplesPerPixel < 1 || samplesPerPixel > MAX_BANDS) {
    // Only support 1 through MAX_BANDS bands in the image
    ret = -1;
  }

  if (samplesPerPixel > 1 &&
      planarConfiguration != PLANARCONFIG_CONTIG &&
      planarConfiguration != PLANARCONFIG_SEPARATE)
  {
    ret = -1;
  }

  // Required tag only for color (multi-band) images
  if (ret == 0 && samplesPerPixel > 1) {
    if (planarConfiguration != PLANARCONFIG_CONTIG &&
        planarConfiguration != PLANARCONFIG_SEPARATE)
    {
      ret = guess_planar_configuration(tif, planar_config);
      if (ret == 0) {
          asfReport(report_level,
                "Found multi-band TIFF but the planar configuration TIFF tag\n"
                    "is not populated ...GUESSING the planar configuration to be %s\n"
                    "based on calculated scanline lengths for interlaced and band-sequential\n"
                    "TIFFs v. the actual scanline length derived from the TIFF file itself.\n",
                    (*planar_config == PLANARCONFIG_CONTIG) ?
                    "Contiguous Planes (interlaced)" :
                    (*planar_config == PLANARCONFIG_SEPARATE) ?
                    "Separate Planes (band-sequential)" :
                    "Unknown Planar Config (?)");
      }
    }
  }

  return ret;
}

int guess_planar_configuration(TIFF *tif, short *planar_config)
{
  int     ret = 0;
  uint16  planarConfiguration = 0;
  uint16  bitsPerSample = 0;  // Data width
  uint16  samplesPerPixel = 0; // Number of bands
  uint16  width = 0; // Image width (num columns)

  // Required tag for all images
  ret = 0;
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsPerSample);
  if (bitsPerSample != 8  &&
      bitsPerSample != 16 &&
      bitsPerSample != 32)
  {
    // Only support byte, integer16, integer32, and 32-bit floats
    ret = -1;
  }

  // Required tag for all images
  if (ret == 0) {
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
    if (width <= 0) {
      ret = -1;
    }
  }

  // Required tag for all images
  if (ret == 0) {
    TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesPerPixel);
    if (samplesPerPixel < 1 || samplesPerPixel > MAX_BANDS) {
      // Only support 1 through MAX_BANDS bands in the image
      ret = -1;
    }
  }

  // Required tag only for color (multi-band) images
  if (ret == 0 && samplesPerPixel > 1) {
    TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarConfiguration);
    if (planarConfiguration != PLANARCONFIG_CONTIG &&
        planarConfiguration != PLANARCONFIG_SEPARATE)
    {
      // Compare calculated scanline length to actual and then
      // guess what the planar config is, e.g. interlaced
      // band storage results in scanlines being num_bits*num_bands*width
      // while band-sequential has a scanline length of num_bits*width
      uint16 interlaced_scanline_len;
      uint16 actual_scanline_len = TIFFScanlineSize(tif);

      interlaced_scanline_len = bitsPerSample * samplesPerPixel * width;
      if (interlaced_scanline_len > actual_scanline_len) {
        // Band-sequential
        *planar_config = PLANARCONFIG_SEPARATE;
      }
      else {
        // Interlaced
        *planar_config = PLANARCONFIG_CONTIG;
      }
    }
    else {
      *planar_config = planarConfiguration;
    }
  }

  return ret;
}

// Returns true if georeferenced ...all geocoded and georeferenced GeoTIFFs qualify
int isGeotiff(const char *file)
{
    TIFF *tiff = NULL;
    GTIF *gtif = NULL;
    int num_tie_points = 0;
    int num_pixel_scales = 0;
    double *tie_point = NULL;
    double *pixel_scale = NULL;

    tiff = XTIFFOpen (file, "r");
    if (tiff != NULL) {
        gtif = GTIFNew (tiff);
    }
    if (gtif) {
        (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS, &num_tie_points, &tie_point);
        (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE, &num_pixel_scales, &pixel_scale);
    }

    if (gtif &&
        num_tie_points == 6 && num_pixel_scales == 3 &&
        pixel_scale[0] > 0.0 && pixel_scale[1] > 0.0)
    {
        GTIFFree(gtif);
        XTIFFClose(tiff);
        FREE(tie_point);
        FREE(pixel_scale);
        return 1;
    }

    if (gtif) GTIFFree(gtif);
    if (tiff) XTIFFClose(tiff);
    FREE(tie_point);
    FREE(pixel_scale);
    return 0;
}

