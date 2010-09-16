#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <asf.h>
#include "asf_tiff.h"

#include <gsl/gsl_math.h>
#include <proj_api.h>

#include "asf_jpeg.h"
#include <png.h>
#include "envi.h"

#include "dateUtil.h"
#include <time.h>
#include "matrix.h"
#include <asf_nan.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <asf_raster.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>
#include <netcdf.h>

#ifdef  MAX_RGB
#undef  MAX_RGB
#endif
#define MAX_RGB             255
#define USHORT_MAX          65535

// If you change the BAND_ID_STRING here, make sure you make an identical
// change in the import library ...the defs must match
#define BAND_ID_STRING "Color Channel (Band) Contents in RGBA+ order"

/* This constant is from the GeoTIFF spec.  It basically means that
   the system which would normally be specified by the field
   (projected coordinate system, datum, ellipsoid, whatever), in
   instead going to be specified by more detailed low level tags.  */
static const int user_defined_value_code = 32767;

int is_slant_range(meta_parameters *md);

void append_band_names(char **band_names, int rgb, char *citation, int palette_color_tiff)
{
  char band_name[256];
  int i=0;
  sprintf(citation, "%s, %s: ", citation, BAND_ID_STRING);
  if (band_names) {
    if (rgb && !palette_color_tiff) {
      // RGB tiffs that do not use a color index (palette) have 3 bands, but
      // palette color tiffs have a single band and an RGB look-up table (TIFFTAG_COLORMAP)
      //
      // When the image uses a look-up table, then meta->general->bands lists the look-up table
      // name rather than a list of individual band names.  band_names[] will therefore only have
      // a single entry.
      //
      // When a color image does not use a look-up table, then meta->general->bands contains
      // the list of band names, one for each band, separated by commas.  It's safe to expect
      // the band_names[] array to contain 3 bands in this case.
      for (i=0; i<2; i++) {
        // First 2 bands have a comma after the band name
        if (band_names[i] != NULL && strlen(band_names[i]) > 0 &&
            strncmp(band_names[i], MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) != 0)
        {
          sprintf(band_name, "%s,", strncmp("IGNORE", uc(band_names[i]), 6) == 0 ? "Empty" : band_names[i]);
          strcat(citation, band_name);
        }
        else {
          sprintf(band_name, "%02d,", i + 1);
          strcat(citation, band_name);
        }
      }
      if (band_names[i] != NULL && strlen(band_names[i]) > 0 &&
          strncmp(band_names[i], MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) != 0)
      {
        strcat(citation, strncmp("IGNORE", uc(band_names[i]), 6) == 0 ? "Empty" : band_names[i]);
      }
      else {
        sprintf(band_name, "%02d", i + 1);
        strcat(citation, band_name);
      }
    }
    else {
        // Single band image or a palette color image
        if (palette_color_tiff) {
            strcat(citation, band_names[0]);
        }
        else {
            if (band_names[0] != NULL && strlen(band_names[0]) > 0 &&
                strncmp(band_names[i], MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) != 0)
            {
                strcat(citation, strncmp("IGNORE", uc(band_names[0]), 6) == 0 ? "Empty" : band_names[0]);
            }
            else {
                strcat(citation, "01");
            }
        }
    }
  }
  else {
    if (rgb && !palette_color_tiff)
      strcat(citation, "01,02,03");
    else
      strcat(citation, "01");
  }
}

void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping,
                           int rgb, int *palette_color_tiff, char **band_names,
                           char *look_up_table_name, int is_colormapped)
{
  unsigned short sample_size;
  int max_dn, map_size = 0, palette_color = 0;
  unsigned short rows_per_strip;
  unsigned short *colors = NULL;
  int have_look_up_table = look_up_table_name && strlen(look_up_table_name) > 0;

  // Open output tiff file
  *otif = XTIFFOpen (output_file_name, "w");
  asfRequire(otif != NULL, "Error opening output TIFF file.\n");

  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);

  int byte_image = (md->general->data_type == BYTE) ||
                   !(sample_mapping == NONE && !md->optical && !have_look_up_table);
  if (!byte_image) {
      // Float image
      asfRequire(sizeof (float) == 4,
                 "Size of the unsigned char data type on this machine is "
                 "different than expected.\n");
      sample_size = 4;
      TIFFSetField(*otif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  }
  else {
      // Byte image
      asfRequire(sizeof(unsigned char) == 1,
                 "Size of the unsigned char data type on this machine is "
                 "different than expected.\n");
      if (have_look_up_table) {
          // Get max DN from lut file and if it's greater than 255, then
          // the TIFF standard will not allow indexed color.
          if (sizeof(unsigned short) != 2) {
              // The TIFF standard requires an unsigned short to be 16 bits long
              asfPrintError("Size of the unsigned short integer data type on this machine (%d bytes) is "
                      "different than expected (2 bytes).\n", sizeof(unsigned short));
          }
          // Try #1 ...Try 2 bytes just to get past lut_to_tiff_palette() with no errors
          // ...More accurately, we are using lut_to_tiff_palette() to determine max_dn, the
          // minimum number of entries that the color table must have
          sample_size = 2; // Unsigned short ...2 bytes allows a color map 65535 elements long or smaller
          map_size = 1 << (sample_size * 8); // 2^bits_per_sample;
          max_dn = lut_to_tiff_palette(&colors, map_size, look_up_table_name);
          sample_size = 1;
          palette_color = 0;
          if (max_dn <= MAX_RGB) {
              sample_size = 1;
              map_size = 1 << (sample_size * 8); // 2^bits_per_sample;
              max_dn = lut_to_tiff_palette(&colors, map_size, look_up_table_name);
              palette_color = 1;
          }
      }
      else {
          // No look-up table, so the data is already 0-255 or will be resampled down to
          // the 0-255 range.  Only need a 1-byte pixel...
          sample_size = 1;
      }
      // The sample format is 'unsigned integer', but the number of bytes per
      // sample, i.e. short, int, or long, is set by TIFFTAG_BITSPERSAMPLE below
      // which in turn is determined by the sample_size (in bytes) set above ;-)
      // Note that for color images, "bits per sample" refers to one color component,
      // not the group of 3 components.
      TIFFSetField(*otif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);
  }
  // use the metadata's colormap if we have one.  a -lut specified on the command
  // line will override, though (and in that case we should have already set
  // up the colormap via lut_to_tiff_palette())
  if (is_colormapped && md->colormap && !have_look_up_table)
  {
      asfPrintStatus("\nFound single-band image with RGB color map ...storing as a Palette\n"
          "Color TIFF\n\n");
      palette_color = 1;
      max_dn = map_size = meta_colormap_to_tiff_palette(&colors, &byte_image, md->colormap);
  }

  /* Set the normal TIFF image tags.  */
  TIFFSetField(*otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(*otif, TIFFTAG_IMAGEWIDTH, md->general->sample_count);
  TIFFSetField(*otif, TIFFTAG_IMAGELENGTH, md->general->line_count);
  TIFFSetField(*otif, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(*otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  if  (
       (!have_look_up_table && rgb           )  ||
       ( have_look_up_table && !palette_color)
      )
  {
      // Color RGB (no palette) image
      TIFFSetField(*otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
      TIFFSetField(*otif, TIFFTAG_SAMPLESPERPIXEL, 3);
  }
  else if (byte_image && palette_color) {
      // Color Palette image
      TIFFSetField(*otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
      TIFFSetField(*otif, TIFFTAG_SAMPLESPERPIXEL, 1);

      unsigned short *red, *green, *blue;
      asfRequire(colors != NULL, "Color map not allocated.\n");
      asfRequire(map_size > 0,   "Color map not initialized.\n");
      red   = colors;
      green = colors +   map_size;
      blue  = colors + 2*map_size;
      TIFFSetField(*otif, TIFFTAG_COLORMAP, red, green, blue);
  }
  else {
    // Else assume grayscale with minimum value (usually zero) means 'black'
    TIFFSetField(*otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
    TIFFSetField(*otif, TIFFTAG_SAMPLESPERPIXEL, 1);
  }

  // Set rows per strip to 1, 4, 8, or 16 ...trying to set a near optimal 8k strip size
  // FIXME: Implement the creation of TIFFS with optimized number of rows per strip ...someday
  rows_per_strip = 1;//((OPT_STRIP_BYTES / (sample_size * md->general->sample_count)) < 4)  ? 1  :
                   //((OPT_STRIP_BYTES / (sample_size * md->general->sample_count)) < 8)  ? 4  :
                   //((OPT_STRIP_BYTES / (sample_size * md->general->sample_count)) < 16) ? 8  :
                   //((OPT_STRIP_BYTES / (sample_size * md->general->sample_count)) < 32) ? 16 :
                     //                                             (unsigned short) USHORT_MAX;
  TIFFSetField(*otif, TIFFTAG_ROWSPERSTRIP, rows_per_strip);

  TIFFSetField(*otif, TIFFTAG_XRESOLUTION, 1.0);
  TIFFSetField(*otif, TIFFTAG_YRESOLUTION, 1.0);
  TIFFSetField(*otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(*otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);

  if (is_geotiff) {
      *ogtif = write_tags_for_geotiff (*otif, metadata_file_name, rgb, band_names, palette_color);
  }

  *palette_color_tiff = palette_color;

  meta_free(md);
}

GTIF* write_tags_for_geotiff (TIFF *otif, const char *metadata_file_name,
                             int rgb, char **band_names, int palette_color_tiff)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  int map_projected = is_map_projected(md);
  int is_slant_range_image = is_slant_range(md);
  GTIF *ogtif;

  /* Semi-major and -minor ellipse axis lengths.  This shows up in two
  different places in our metadata, we want the projected one if
  its available, otherwise the one from general.  */
  double re_major, re_minor;

  asfRequire (sizeof (unsigned short) == 2,
              "Unsigned short integer data type size is different than "
                  "expected.\n");
  asfRequire (sizeof (unsigned int) == 4,
              "Unsigned integer data type size is different than expected.\n");

  if (!map_projected) {
    asfPrintWarning("Image is not map-projected or the projection type is\n"
        "unrecognized or unsupported.\n\n"
        "Exporting a non-geocoded, standard TIFF file instead...\n");
  }

  if (is_slant_range_image) {
    asfPrintWarning("Image is either a SAR slant-range image or a ScanSAR\n"
        "along-track/cross-track projection.  Exporting as a GeoTIFF is not\n"
        "supported for these types.\n\n"
        "Exporting a standard non-georeferenced/non-geocoded TIFF file instead...\n");
  }

  /******************************************/
  /* Set the GeoTIFF extension image tags.  */
  ogtif = GTIFNew (otif);
  asfRequire (ogtif != NULL, "Error opening output GeoKey file descriptor.\n");

  // Common tags
  if (map_projected)
  {
    // Write common tags for map-projected GeoTIFFs
    GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
    GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
    GTIFKeySet (ogtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);

    re_major = md->projection->re_major;
    re_minor = md->projection->re_minor;
  }
  else {
    // If not map-projected, then common tags are not written.  If the file
    // is georeferenced however, then pixel scale and tie point will be
    // written below

    re_major = md->general->re_major;
    re_minor = md->general->re_minor;
  }

  // Pixel scale and tie points
  if (md->projection) {
    if (!(meta_is_valid_double(md->projection->startX) &&
          meta_is_valid_double(md->projection->startY))
       )
    {
      asfPrintWarning("Metadata projection block contains invalid startX "
          "or startY values\n");
    }
    // Write georeferencing data
    if (!is_slant_range_image) {
      double tie_point[6];
      double pixel_scale[3];
      // FIXME: Note that the following tie points are in meters, but that's
      // because we assume linear meters and a map-projected image.  If we ever
      // export a geographic (lat/long) geotiff, then this code will need to
      // smarten-up and write the tie points in lat/long or meters depending on the
      // type of image data.  Same thing applies to perX and perY etc etc.
      tie_point[0] = 0.0;
      tie_point[1] = 0.0;
      tie_point[2] = 0.0;

      // these are both meters
      tie_point[3] = md->projection->startX +
                     md->general->start_sample * md->projection->perX;
      tie_point[4] = md->projection->startY +
                     md->general->start_line * md->projection->perY;

      tie_point[5] = 0.0;
      TIFFSetField(otif, TIFFTAG_GEOTIEPOINTS, 6, tie_point);

      /* Set the scale of the pixels, in projection coordinates.  */
      if (md->projection->perX < 0.0) {
        asfPrintWarning("Unexpected non-positive perX in the "
            "projection block.\n");
      }
      else {
        pixel_scale[0] = fabs(md->projection->perX);
      }

      if (md->projection->perY > 0.0) {
        asfPrintWarning("Unexpected non-negative perY in the "
            "projection block.\n");
      }
      else {
        pixel_scale[1] = fabs(md->projection->perY);
        pixel_scale[2] = 0;
      }
      TIFFSetField (otif, TIFFTAG_GEOPIXELSCALE, 3, pixel_scale);
    }
  }

  // Write geocode (map projection) parameters
  if (map_projected) {
    int max_citation_length = 2048;
    char *citation;
    int citation_length;

    // For now, only support the Hughes ellipsoid for polar stereo
    if (md->projection->datum == HUGHES_DATUM &&
        md->projection->type != POLAR_STEREOGRAPHIC)
    {
      asfPrintError("Hughes Ellipsoid is only supported for Polar Stereographic projections.\n");
    }

    /* Write the appropriate geotiff keys for the projection type.  */
    switch (md->projection->type) {
      case UNIVERSAL_TRANSVERSE_MERCATOR:
      {
        short pcs;
        if ( UTM_2_PCS(&pcs, md->projection->datum,
             md->projection->param.utm.zone, md->projection->hem) ) {
          GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1, pcs);

          // Write the citation
          char datum_str[256];
          if (md->projection->datum == ITRF97_DATUM) {
            strcpy(datum_str, "ITRF97 (WGS 84)");
          }
          else {
            pcs_2_string (datum_str, pcs);
          }
          citation = MALLOC ((max_citation_length + 1) * sizeof (char));
          snprintf (citation, max_citation_length + 1,
                    "UTM zone %d %c projected GeoTIFF on %s "
                    "%s written by Alaska Satellite Facility tools.",
                    md->projection->param.utm.zone, md->projection->hem,
                    datum_str,
                    md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
          append_band_names(band_names, rgb, citation, palette_color_tiff);
          citation_length = strlen(citation);
          asfRequire((citation_length >= 0) && (citation_length <= max_citation_length),
                     "GeoTIFF citation too long" );
          GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
          GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
          FREE(citation);
        }
        else {
          asfPrintWarning("Unsupported combination of datum and hemisphere for a UTM\n"
              "map projection occurred.\n"
              "...GeoTIFF will be written but will not contain projection information\n"
              "other than tiepoints and pixel scales.\n");
        }
      }
        break;
      case ALBERS_EQUAL_AREA:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_AlbersEqualArea);
        if (meta_is_valid_double(md->projection->param.albers.std_parallel1)) {
          GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.std_parallel1);
        }
        if (meta_is_valid_double(md->projection->param.albers.std_parallel2)) {
          GTIFKeySet (ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.std_parallel2);
        }
        if (meta_is_valid_double(md->projection->param.albers.false_easting)) {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.albers.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.albers.orig_latitude)) {
          GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.orig_latitude);
        }
        if (meta_is_valid_double(md->projection->param.albers.center_meridian)) {
          // The following is where ArcGIS looks for the center meridian
          GTIFKeySet (ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.center_meridian);
          // The following is where the center meridian _should_ be stored
          GTIFKeySet (ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.albers.center_meridian);
        }
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        snprintf (citation, max_citation_length + 1,
                  "Albers equal-area conic projected GeoTIFF using %s "
                  "%s written by Alaska Satellite Facility "
                  "tools.", datum_str,
                      md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
        append_band_names(band_names, rgb, citation, palette_color_tiff);
        citation_length = strlen(citation);
        asfRequire (citation_length >= 0 && citation_length <= max_citation_length,
                    "bad citation length");
        // The following is not needed for any but UTM (according to the standard)
        // but it appears that everybody uses it anyway... so we'll write it
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        // The following is recommended by the standard
        GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
      }
        break;
      case LAMBERT_CONFORMAL_CONIC:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_LambertConfConic_2SP);
        if (meta_is_valid_double(md->projection->param.lamcc.plat1)) {
          GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.plat1);
        }
        if (meta_is_valid_double(md->projection->param.lamcc.plat2)) {
          GTIFKeySet (ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.plat2);
        }
        if (meta_is_valid_double(md->projection->param.lamcc.false_easting)) {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.lamcc.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.lamcc.lon0)) {
          GTIFKeySet (ogtif, ProjFalseOriginLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.lon0);
        }
        if (meta_is_valid_double(md->projection->param.lamcc.lat0)) {
          GTIFKeySet (ogtif, ProjFalseOriginLatGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.lat0);
        }
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        snprintf (citation, max_citation_length + 1,
                  "Lambert conformal conic projected GeoTIFF using %s "
                  "%s written by Alaska Satellite Facility "
                  "tools.", datum_str,
                  md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
        append_band_names(band_names, rgb, citation, palette_color_tiff);
        citation_length = strlen(citation);
        asfRequire (citation_length >= 0 && citation_length <= max_citation_length,
                    "bad citation length");
        // The following is not needed for any but UTM (according to the standard)
        // but it appears that everybody uses it anyway... so we'll write it
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        // The following is recommended by the standard
        GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
      }
      break;
      case POLAR_STEREOGRAPHIC:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_PolarStereographic);
        if (meta_is_valid_double(md->projection->param.ps.slon)) {
          GTIFKeySet (ogtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.ps.slon);
        }
        if (meta_is_valid_double(md->projection->param.ps.slat)) {
          GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.ps.slat);
        }
        if (meta_is_valid_double(md->projection->param.ps.false_easting)) {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.ps.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.ps.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.ps.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        if (md->projection->datum != HUGHES_DATUM) {
          snprintf (citation, max_citation_length + 1,
                    "Polar stereographic projected GeoTIFF using %s "
                    "datum written by Alaska Satellite Facility "
                    "tools", datum_str);
          append_band_names(band_names, rgb, citation, palette_color_tiff);
          citation_length = strlen(citation);
          asfRequire (citation_length >= 0 &&
                      citation_length <= max_citation_length,
                      "bad citation length");
        }
        else {
          // Hughes Datum
          // ...Since the datum is user-defined and the GeoTIFF is now delving outside the
          // realm of 'normal' GeoTIFFs, we put all the pertinent projection parameters
          // into the citation strings to help users if their s/w doesn't 'like' user-defined
          // datums.
	  /*
          snprintf (citation, max_citation_length + 1,
                    "Polar stereographic projected GeoTIFF using Hughes "
                    "ellipsoid written by Alaska Satellite Facility "
                    "tools, Natural Origin Latitude %lf, Straight Vertical "
                    "Pole %lf.", md->projection->param.ps.slat,
                    md->projection->param.ps.slon);
          append_band_names(band_names, rgb, citation, palette_color_tiff);
          citation_length = strlen(citation);
          asfRequire (citation_length >= 0 &&
              citation_length <= max_citation_length,
          "bad citation length");
	  */
	  sprintf(citation, "NSIDC Sea Ice Polar Stereographic North");
        }
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
      }
      break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_LambertAzimEqualArea);
        if (meta_is_valid_double(md->projection->param.lamaz.center_lon)) {
          GTIFKeySet (ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamaz.center_lon);
        }
        if (meta_is_valid_double(md->projection->param.lamaz.center_lat)) {
          GTIFKeySet (ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamaz.center_lat);
        }
        if (meta_is_valid_double(md->projection->param.lamaz.false_easting)) {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamaz.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.lamaz.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamaz.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        snprintf (citation, max_citation_length + 1,
                  "Lambert azimuthal equal area projected GeoTIFF using "
                  "%s %s written by Alaska Satellite "
                      "Facility tools.", datum_str,
                  md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
        append_band_names(band_names, rgb, citation, palette_color_tiff);
        citation_length = strlen(citation);
        asfRequire (citation_length >= 0 &&
            citation_length <= max_citation_length,
                "bad citation length");
        // The following is not needed for any but UTM (according to the standard)
        // but it appears that everybody uses it anyway... so we'll write it
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        // The following is recommended by the standard
        GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
      }
        break;
      case EQUI_RECTANGULAR:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_Equirectangular);
        if (meta_is_valid_double(md->projection->param.eqr.central_meridian)) {
          GTIFKeySet (ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.eqr.central_meridian);
        }
        if (meta_is_valid_double(md->projection->param.eqr.orig_latitude)) {
          GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.eqr.orig_latitude);
        }
        if (meta_is_valid_double(md->projection->param.eqr.false_easting)) {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.eqr.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.eqr.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.eqr.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        snprintf (citation, max_citation_length + 1,
                  "Equi-rectangular projected GeoTIFF using "
                  "%s %s written by Alaska Satellite "
                      "Facility tools.", datum_str,
                  md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
        append_band_names(band_names, rgb, citation, palette_color_tiff);
        citation_length = strlen(citation);
        asfRequire (citation_length >= 0 &&
            citation_length <= max_citation_length,
                "bad citation length");
        // The following is not needed for any but UTM (according to the standard)
        // but it appears that everybody uses it anyway... so we'll write it
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        // The following is recommended by the standard
        GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
      }
        break;
      case MERCATOR:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_Mercator);
        if (meta_is_valid_double(md->projection->param.mer.central_meridian)) {
          GTIFKeySet (ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.mer.central_meridian);
        }
        if (meta_is_valid_double(md->projection->param.mer.orig_latitude)) {
          GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.mer.orig_latitude);
        }
        if (meta_is_valid_double(md->projection->param.mer.standard_parallel)) {
	  // GeoTIFF only supports the scale factor version.
	  // Hence some fancy calculation
	  double lat = md->projection->param.mer.orig_latitude;
	  double lat1 = md->projection->param.mer.standard_parallel;
	  double re = md->projection->re_major;
	  double rp = md->projection->re_minor;
	  double e2 = sqrt(1.0 - rp*rp/(re*re));
	  double scale = (sqrt(1.0 - e2*sin(lat)*sin(lat))/cos(lat)) * 
	    (cos(lat1)/sqrt(1.0 - e2*sin(lat1)*sin(lat1)));

          GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1, scale);
        }
        if (meta_is_valid_double(md->projection->param.mer.false_easting)) {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.mer.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.mer.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.mer.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        snprintf (citation, max_citation_length + 1,
                  "Mercator projected GeoTIFF using "
                  "%s %s written by Alaska Satellite "
                      "Facility tools.", datum_str,
                  md->projection->datum == HUGHES_DATUM ? "ellipsoid" : "datum");
        append_band_names(band_names, rgb, citation, palette_color_tiff);
        citation_length = strlen(citation);
        asfRequire (citation_length >= 0 &&
            citation_length <= max_citation_length,
                "bad citation length");
        // The following is not needed for any but UTM (according to the standard)
        // but it appears that everybody uses it anyway... so we'll write it
        GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
        // The following is recommended by the standard
        GTIFKeySet (ogtif, GTCitationGeoKey, TYPE_ASCII, 1, citation);
        free (citation);
      }
        break;
      default:
        asfPrintWarning ("Unsupported map projection found.  TIFF file will not\n"
            "contain projection information.\n");
	free(citation);
        break;
    }
  }

  // NOTE: GTIFWriteKeys() must be called to finalize the writing of geokeys
  // to the GeoTIFF file ...see finalize_tiff_file() ...do not insert a call
  // to GTIFWriteKeys() here plz.

  meta_free (md);

  return ogtif;
}

void finalize_tiff_file(TIFF *otif, GTIF *ogtif, int is_geotiff)
{
  // Finalize the GeoTIFF
  if (is_geotiff && ogtif != NULL) {
    int ret;

    ret = GTIFWriteKeys (ogtif);
    asfRequire (ret, "Error writing GeoTIFF keys.\n");
    GTIFFree (ogtif);
  }

  // Finalize the TIFF file
  if (otif != NULL) {
    XTIFFClose (otif);
  }
}

// lut_to_tiff_palette() converts an ASF-style look-up table (text file) into
// a TIFF standard RGB palette (indexed color map for TIFFTAG_COLORMAP)
int lut_to_tiff_palette(unsigned short **colors, int map_size, char *look_up_table_name)
{
    int i, max_lut_dn;
    unsigned char *lut = NULL;
    float r, g, b;

    asfRequire(map_size >= 1, "TIFF palette color map size less than 1 element\n");
    asfRequire(look_up_table_name != NULL && strlen(look_up_table_name) > 0,
               "Invalid look up table name\n");

    // Use CALLOC so the entire look up table is initialized to zero
    lut = (unsigned char *)CALLOC(MAX_LUT_DN * 3, sizeof(unsigned char));

    // Read the look up table from the file.  It will be 3-color and have MAX_LUT_DN*3 elements
    // in the array, in packed format <rgbrgbrgb...rgb>, and values range from 0 to 255
    max_lut_dn = read_lut(look_up_table_name, lut);
    if (max_lut_dn > map_size) {
        FREE(lut);
        asfPrintError("Look-up table too large (%d) for TIFF.  Maximum TIFF\n"
                "look-up table size for the data type is %d elements long.\n", max_lut_dn, map_size);
    }
    *colors = (unsigned short *)_TIFFmalloc(sizeof(unsigned short) * 3 * map_size);
    asfRequire(*colors != NULL, "Could not allocate TIFF palette.\n");
    for (i = 0; i < 3 * map_size; i++) (*colors)[i] = (unsigned short)0; // Init to all zeros

    // Normalize the look-up table values into the range specified by the TIFF standard
    // (0 through 65535 ...the maximum unsigned short value)
    for (i=0; i<max_lut_dn; i++) {
        // TIFF color map structure is a one-row array, all the reds, then all the
        // greens, then all the blues, i.e. <all reds><all greens><all blues>, each
        // section 'map_size' elements long
        //
        // Grab rgb values from the lut
        r = (float)lut[i*3  ];
        g = (float)lut[i*3+1];
        b = (float)lut[i*3+2];

        // Normalize the lut values to the 0-65535 range as specified by the TIFF standard for
        // color maps / palettes
        (*colors)[i             ] = (unsigned short) ((r/(float)MAX_RGB)*(float)USHORT_MAX + 0.5);
        (*colors)[i +   map_size] = (unsigned short) ((g/(float)MAX_RGB)*(float)USHORT_MAX + 0.5);
        (*colors)[i + 2*map_size] = (unsigned short) ((b/(float)MAX_RGB)*(float)USHORT_MAX + 0.5);
    }

    return max_lut_dn;
}

// Assumes the color map is made from unsigned shorts (uint16 or equiv).  a) This is mandated
// by v6 of the TIFF standard regardless of data type in the tiff, b) The only valid
// data types for a colormap TIFF is i) 4-bit unsigned integer, or ii) 8-bit unsigned integer,
// and c) the values in a TIFF colormap range from 0 to 65535 (max uint16) and must be normalized
// to 0-255 as for display (the range 0-255 is normalize to 0-65535 when creating the colormap
// in the first place.)
void dump_palette_tiff_color_map(unsigned short *colors, int map_size)
{
    int i;
    asfRequire(map_size <= 256, "Map size too large.\n");

    fprintf(stderr, "\n\nColor Map (DN: red  green  blue):\n");
    for (i=0; i<map_size; i++){
        fprintf(stderr, "%d:\t%d\t%d\t%d\n", i,
                (int)((float)colors[i +          0]*((float)MAX_RGB/(float)USHORT_MAX) + 0.5),
                (int)((float)colors[i +   map_size]*((float)MAX_RGB/(float)USHORT_MAX) + 0.5),
                (int)((float)colors[i + 2*map_size]*((float)MAX_RGB/(float)USHORT_MAX) + 0.5));
    }
}

int meta_colormap_to_tiff_palette(unsigned short **tiff_palette, int *byte_image, meta_colormap *colormap)
{
    int i, size;
    unsigned short r, g, b;
    meta_colormap *cm = colormap; // Convenience ptr

    asfRequire(colormap != NULL, "Unallocated colormap.\n");
    size = cm->num_elements;
    *tiff_palette = (unsigned short *)CALLOC(3*size, sizeof(unsigned short));

    // A TIFF palette is a one dimensional array ...all the reds, then all the greens, then all the blues
    // Create TIFF palette by normalizing 0-255 range into 0-65535 and storing in the band sequential
    // array.
    for (i=0; i<size; i++) {
        r = cm->rgb[i].red;
        g = cm->rgb[i].green;
        b = cm->rgb[i].blue;
        (*tiff_palette)[i +      0] = (unsigned short) (((float)r/(float)MAX_RGB)*(float)USHORT_MAX + 0.5);
        (*tiff_palette)[i +   size] = (unsigned short) (((float)g/(float)MAX_RGB)*(float)USHORT_MAX + 0.5);
        (*tiff_palette)[i + 2*size] = (unsigned short) (((float)b/(float)MAX_RGB)*(float)USHORT_MAX + 0.5);
    }

    return size;
}
