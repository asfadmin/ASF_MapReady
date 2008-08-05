#include <assert.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <geokeys.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <gsl/gsl_math.h>
#include <proj_api.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>

#include <jpeglib.h>
#include <png.h>

#include <asf.h>
#include <asf_nan.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_export.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>

#define PGM_MAGIC_NUMBER "P5"

// If you change the BAND_ID_STRING here, make sure you make an identical
// change in the import library ...the defs must match
#define BAND_ID_STRING "Color Channel (Band) Contents in RGBA+ order"

/* This constant is from the GeoTIFF spec.  It basically means that
   the system which would normally be specified by the field
   (projected coordinate system, datum, ellipsoid, whatever), in
   instead going to be specified by more detailed low level tags.  */
static const int user_defined_value_code = 32767;

int is_slant_range(meta_parameters *md);
void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping,
                           int rgb, char **band_names, int have_look_up_table);
GTIF* write_tags_for_geotiff (TIFF *otif, const char *metadata_file_name,
                              int rgb, char **band_names, int have_look_up_table);
void finalize_tiff_file(TIFF *otif, GTIF *ogtif, int is_geotiff);
void append_band_names(char **band_names, int rgb, char *citation, int have_look_up_table);

void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping,
                           int rgb, char **band_names, int have_look_up_table)
{
  unsigned short sample_size;
  unsigned short sample_format;

  // Open output tiff file
  *otif = XTIFFOpen (output_file_name, "w");
  asfRequire(otif != NULL, "Error opening output TIFF file.\n");

  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);

  if (sample_mapping == NONE && !md->optical) {
    // Float image
    asfRequire(sizeof (float) == 4,
               "Size of the unsigned char data type on this machine is "
                   "different than expected.\n");
    sample_size = 4;
    sample_format = SAMPLEFORMAT_IEEEFP;

  }
  else {
    // Byte image
    asfRequire(sizeof(unsigned char) == 1,
               "Size of the unsigned char data type on this machine is "
                   "different than expected.\n");
    sample_size = 1;
    sample_format = SAMPLEFORMAT_UINT;
  }

  /* Set the normal TIFF image tags.  */
  TIFFSetField(*otif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField(*otif, TIFFTAG_IMAGEWIDTH, md->general->sample_count);
  TIFFSetField(*otif, TIFFTAG_IMAGELENGTH, md->general->line_count);
  TIFFSetField(*otif, TIFFTAG_BITSPERSAMPLE, sample_size * 8);
  TIFFSetField(*otif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  if (rgb) {
    // Color RGB (no palette) image
    TIFFSetField(*otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    TIFFSetField(*otif, TIFFTAG_SAMPLESPERPIXEL, 3);
  }
  else {
    // Else assume grayscale with minimum value (usually zero) means 'black'
    TIFFSetField(*otif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
    TIFFSetField(*otif, TIFFTAG_SAMPLESPERPIXEL, 1);
  }
  TIFFSetField(*otif, TIFFTAG_ROWSPERSTRIP, 1);
  TIFFSetField(*otif, TIFFTAG_XRESOLUTION, 1.0);
  TIFFSetField(*otif, TIFFTAG_YRESOLUTION, 1.0);
  TIFFSetField(*otif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_NONE);
  TIFFSetField(*otif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(*otif, TIFFTAG_SAMPLEFORMAT, sample_format);

  if (is_geotiff) {
    *ogtif = write_tags_for_geotiff (*otif, metadata_file_name, rgb, band_names, have_look_up_table);
  }
}

void initialize_png_file(const char *output_file_name,
                         meta_parameters *meta, FILE **opng,
                         png_structp *png_ptr, png_infop *info_ptr,
                         int rgb)
{
    *opng = FOPEN(output_file_name, "wb");
    *png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
        NULL, NULL, NULL);
    if (!(*png_ptr))
        asfPrintError("Error creating PNG write structure!\n");

    *info_ptr = png_create_info_struct(*png_ptr);
    if (!(*info_ptr))
        asfPrintError("Error creating PNG info structure!\n");

    png_init_io(*png_ptr, *opng);

    int width = meta->general->sample_count;
    int height = meta->general->line_count;
    png_byte color_type = rgb ? PNG_COLOR_TYPE_RGB : PNG_COLOR_TYPE_GRAY;

    png_set_IHDR(*png_ptr, *info_ptr, width, height, 8, color_type,
            PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
            PNG_FILTER_TYPE_DEFAULT);

    png_write_info(*png_ptr, *info_ptr);
}

void initialize_jpeg_file(const char *output_file_name,
        meta_parameters *meta, FILE **ojpeg,
        struct jpeg_compress_struct *cinfo, int rgb)
{
  struct jpeg_error_mgr *jerr = MALLOC(sizeof(struct jpeg_error_mgr));

  /* We need a version of the data in JSAMPLE form, so we have to map
     floats into JSAMPLEs.  We do this by defining a region 2 sigma on
     either side of the mean to be mapped in the range of JSAMPLE
     linearly, and clamping everything outside this range at the
     limits o the JSAMPLE range.  */
  /* Here are some very funky checks to try to ensure that the JSAMPLE
     really is the type we expect, so we can scale properly.  */
  asfRequire(sizeof(unsigned char) == 1,
             "Size of the unsigned char data type on this machine is "
             "different than expected.\n");
  asfRequire(sizeof(unsigned char) == sizeof (JSAMPLE),
             "Size of unsigned char data type on this machine is different "
             "than JPEG byte size.\n");
  JSAMPLE test_jsample = 0;
  test_jsample--;
  asfRequire(test_jsample == UCHAR_MAX,
             "Something wacky happened, like data overflow.\n");

  // Initializae libjpg structures.
  cinfo->err = jpeg_std_error (jerr);
  jpeg_create_compress (cinfo);

  // Open the output file to be used.
  *ojpeg = FOPEN(output_file_name, "wb");

  // Connect jpeg output to the output file to be used.
  jpeg_stdio_dest (cinfo, *ojpeg);

  // Set image parameters that libjpeg needs to know about.
  cinfo->image_width = meta->general->sample_count;
  cinfo->image_height = meta->general->line_count;
  if (rgb) {
    cinfo->in_color_space = JCS_RGB;
    cinfo->input_components = 3;
  }
  else {
    cinfo->in_color_space = JCS_GRAYSCALE;
    cinfo->input_components = 1;
  }
  jpeg_set_defaults (cinfo);   // Use default compression parameters.
  jpeg_set_quality(cinfo, 100, 1);


  // Reassure libjpeg that we will be writing a complete JPEG file.
  jpeg_start_compress (cinfo, TRUE);

  return;
}

void initialize_pgm_file(const char *output_file_name,
       meta_parameters *meta, FILE **opgm)
{
  const int max_color_value = 255;

  *opgm = FOPEN (output_file_name, "w");

  fprintf (*opgm, "%s\n", PGM_MAGIC_NUMBER);
  fprintf (*opgm, "%ld\n", (long int) meta->general->sample_count);
  fprintf (*opgm, "%ld\n", (long int) meta->general->line_count);
  fprintf (*opgm, "%d\n", max_color_value);

  return;
}

GTIF* write_tags_for_geotiff (TIFF *otif, const char *metadata_file_name,
                             int rgb, char **band_names, int have_look_up_table)
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
        "Exporting a non-geocoded, but georeferenced if possible, TIFF file instead...\n");
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
          append_band_names(band_names, rgb, citation, have_look_up_table);
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
        append_band_names(band_names, rgb, citation, have_look_up_table);
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
          GTIFKeySet (ogtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.false_easting);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1, 0.0);
        }
        if (meta_is_valid_double(md->projection->param.lamcc.false_northing)) {
          GTIFKeySet (ogtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1,
                      md->projection->param.lamcc.false_northing);
        }
        else {
          GTIFKeySet (ogtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1, 0.0);
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
        append_band_names(band_names, rgb, citation, have_look_up_table);
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
          append_band_names(band_names, rgb, citation, have_look_up_table);
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
          snprintf (citation, max_citation_length + 1,
                    "Polar stereographic projected GeoTIFF using Hughes "
                    "ellipsoid written by Alaska Satellite Facility "
                    "tools, Natural Origin Latitude %lf, Straight Vertical "
                    "Pole %lf.", md->projection->param.ps.slat,
                    md->projection->param.ps.slon);
          append_band_names(band_names, rgb, citation, have_look_up_table);
          citation_length = strlen(citation);
          asfRequire (citation_length >= 0 &&
              citation_length <= max_citation_length,
          "bad citation length");
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
        append_band_names(band_names, rgb, citation, have_look_up_table);
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

void finalize_jpeg_file(FILE *ojpeg, struct jpeg_compress_struct *cinfo)
{
  jpeg_finish_compress (cinfo);
  FCLOSE (ojpeg);
  jpeg_destroy_compress (cinfo);
}

void finalize_png_file(FILE *opng, png_structp png_ptr, png_infop info_ptr)
{
    png_write_end(png_ptr, NULL);
    png_destroy_write_struct(&png_ptr, &info_ptr);
    FCLOSE(opng);
}

void finalize_ppm_file(FILE *oppm)
{
  FCLOSE(oppm);
}

static int all_valid(double *values, int num, double no_data)
{
    // returns true if all pixel values are valid
    // (i.e., not "no data" and not NAN)
    if (meta_is_valid_double(no_data))
    {
        int i;
        for (i=0; i<num; ++i) {
            if (values[i] == no_data)
                return FALSE; // found a "no data" value
            else if (!meta_is_valid_double(values[i]))
                return FALSE; // found a "NAN"
        }
        return TRUE; // no "no data" found
    }
    else
    {
        // "no data" value is NAN, just check for NANs
        int i;
        for (i=0; i<num; ++i) {
            if (!meta_is_valid_double(values[i]))
                return FALSE; // found a "NAN"
        }
        return TRUE; // no "no data" found
    }

    // not reached
    assert(0);
    return FALSE;
}

static double pauli_red(double *band_values, double no_data)
{
    if (!all_valid(band_values, 4, no_data))
        return no_data;

    double HH_amp = band_values[0];
    double HH_phase = band_values[1];
    double VV_amp = band_values[2];
    double VV_phase = band_values[3];

    // |HH-VV|

    double HH_re = HH_amp*cos(HH_phase);
    double HH_im = HH_amp*sin(HH_phase);
    double VV_re = VV_amp*cos(VV_phase);
    double VV_im = VV_amp*sin(VV_phase);

    double re = HH_re - VV_re;
    double im = HH_im - VV_im;

    return hypot(re, im);
}

static double pauli_blue(double *band_values, double no_data)
{
    if (!all_valid(band_values, 4, no_data))
        return no_data;

    double HH_amp = band_values[0];
    double HH_phase = band_values[1];
    double VV_amp = band_values[2];
    double VV_phase = band_values[3];

    // |HH+VV|

    double HH_re = HH_amp*cos(HH_phase);
    double HH_im = HH_amp*sin(HH_phase);
    double VV_re = VV_amp*cos(VV_phase);
    double VV_im = VV_amp*sin(VV_phase);

    double re = HH_re + VV_re;
    double im = HH_im + VV_im;

    return hypot(re, im);
}

static double pauli_green(double *band_values, double no_data)
{
    // |HV|
    return band_values[0];
}

static double sinclair_green(double *band_values, double no_data)
{
    if (!all_valid(band_values, 2, no_data))
        return no_data;

    double HV = band_values[0];
    double VH = band_values[1];

    return .5*(HV+VH);
}

static double sinclair_green_cpx(double *band_values, double no_data)
{
    if (!all_valid(band_values, 4, no_data))
        return no_data;

    double HV_amp = band_values[0];
    double HV_phase = band_values[1];
    double VH_amp = band_values[2];
    double VH_phase = band_values[3];

    // |HV+VH| / 2

    double HV_re = HV_amp*cos(HV_phase);
    double HV_im = HV_amp*sin(HV_phase);
    double VH_re = VH_amp*cos(VH_phase);
    double VH_im = VH_amp*sin(VH_phase);

    double re = HV_re + VH_re;
    double im = HV_im + VH_im;

    return .5*hypot(re, im);
}

void
export_band_image (const char *metadata_file_name,
                   const char *image_data_file_name,
                   char *output_file_name,
                   scale_t sample_mapping,
                   char **band_name, int rgb,
                   int true_color, int false_color,
                   int pauli, int sinclair,
                   char *look_up_table_name,
                   output_format_t format)
{
  int map_projected;
  int is_geotiff = 1;
  TIFF *otif = NULL; // FILE* pointer for TIFF files
  GTIF *ogtif = NULL;
  FILE *ojpeg = NULL, *opgm=NULL, *opng=NULL;
  struct jpeg_compress_struct cinfo;
  png_structp png_ptr;
  png_infop png_info_ptr;
  int ii,jj;
  int have_look_up_table = look_up_table_name && strlen(look_up_table_name)>0;

  meta_parameters *md = meta_read (metadata_file_name);
  map_projected = is_map_projected(md);

  asfRequire( !(look_up_table_name == NULL &&
        sample_mapping == TRUNCATE &&
                (md->general->radiometry == r_SIGMA ||
                 md->general->radiometry == r_BETA  ||
                 md->general->radiometry == r_GAMMA)
               ),
              "Downsampling a Sigma, Beta, or Gamma type image (power or dB)\n"
              "from floating point to byte using truncation is not supported.\n"
              "All values would map to black.\n");

  if (md->general->band_count < 1 || md->general->band_count > MAX_BANDS) {
    asfPrintError ("Unsupported number of channels found (%d).  Only 1 through\n"
        "%d channels are supported.\n", md->general->band_count, MAX_BANDS);
  }

  // NOTE: if the truecolorFlag or falsecolorFlag was set, then 'rgb' is true
  // and the band assignments are in the band_name[] array already.  The true_color
  // and false_color parameters are provided separately just in case we decide
  // to do anything different, e.g. normal rgb processing plus something specific
  // to true_color or false_color (like contrast expansion)
  if (rgb && !have_look_up_table) {

    // Initialize the chosen format
    if (format == TIF) {
      is_geotiff = 0;
      initialize_tiff_file(&otif, &ogtif, output_file_name,
         metadata_file_name, is_geotiff,
         sample_mapping, rgb, band_name, have_look_up_table);
    }
    else if (format == GEOTIFF) {
      initialize_tiff_file(&otif, &ogtif, output_file_name,
         metadata_file_name, is_geotiff,
         sample_mapping, rgb, band_name, have_look_up_table);
    }
    else if (format == JPEG) {
      initialize_jpeg_file(output_file_name, md, &ojpeg, &cinfo, rgb);
    }
    else if (format == PNG) {
      initialize_png_file(output_file_name, md, &opng, &png_ptr,
          &png_info_ptr, rgb);
    }
    int ignored[4] = {0, 0, 0, 0};
    int red_channel=-1, green_channel=-1, blue_channel=-1, nir_channel=-1;
    for (ii = 0; ii < 4; ii++) {
        if (ii < md->general->band_count - 1) {
            ignored[ii] = strncmp("IGNORE", uc(band_name[ii]), 6) == 0 ? 1 : 0;
        }
        else {
            // Ignore bands that exceed band count
            ignored[ii] = 1;
        }
    }

    // these are used only in the sinclair decompositions
    int HH_channel=0, VV_channel=0, HV_channel=0, VH_channel=0;
    // these are used only in the pauli/sinclair decomposition
    int HH_amp_channel=0, HH_phase_channel=0,
        HV_amp_channel=0, HV_phase_channel=0,
        VH_amp_channel=0, VH_phase_channel=0,
        VV_amp_channel=0, VV_phase_channel=0;

    // Determines which band names we need to pass to the calculation
    // routines -- the band names are different for the various flavors
    // of data.
    // this is used by sinclair & pauli only
    const int QP_TYPE_NON_CPX=0;
    const int QP_TYPE_PALSAR11=1;
    const int QP_TYPE_AIRSAR=2;
    int quad_pol_type=0;

    // names of the required bands -- these are determined by the
    // kind of data (airsar or palsar)
    char *pauli_red_bands=NULL;
    char *pauli_green_bands=NULL;
    char *pauli_blue_bands=NULL;
    char *sinclair_red_bands=NULL;
    char *sinclair_green_bands=NULL;
    char *sinclair_blue_bands=NULL;

    channel_stats_t red_stats, blue_stats, green_stats;
    red_stats.hist = NULL; red_stats.hist_pdf = NULL;
    green_stats.hist = NULL; green_stats.hist_pdf = NULL;
    blue_stats.hist = NULL; blue_stats.hist_pdf = NULL;

    if (!md->optical) {
      asfRequire (sizeof(unsigned char) == 1,
                  "Size of the unsigned char data type on this machine is "
                  "different than expected.\n");

      if (sinclair)
      {
          // Sanity checks -- require quad-pol data
          if (md->general->band_count == 4) {
            // palsar 1.5 data looks like this
            asfRequire(strcmp(band_name[0], "HH") == 0 &&
                       strcmp(band_name[1], "HV") == 0 &&
                       strcmp(band_name[2], "VH") == 0 &&
                       strcmp(band_name[3], "VV") == 0,
                "Attempted to apply Sinclair to non-quad-pol data.\n");
            quad_pol_type=QP_TYPE_NON_CPX;
            sinclair_red_bands="VV";
            sinclair_green_bands="HV,HV";
            sinclair_blue_bands="HH";
          } else if (md->general->band_count == 8) {
            // palsar 1.1 data looks like this
            asfRequire(strcmp(band_name[0], "AMP-HH") == 0 &&
                       strcmp(band_name[1], "PHASE-HH") == 0 &&
                       strcmp(band_name[2], "AMP-HV") == 0 &&
                       strcmp(band_name[3], "PHASE-HV") == 0 &&
                       strcmp(band_name[4], "AMP-VH") == 0 &&
                       strcmp(band_name[5], "PHASE-VH") == 0 &&
                       strcmp(band_name[6], "AMP-VV") == 0 &&
                       strcmp(band_name[7], "PHASE-VV") == 0,
                "Attempted to apply Sinclair to non-quad-pol data.\n");
            quad_pol_type=QP_TYPE_PALSAR11;
            sinclair_red_bands="AMP-VV";
            sinclair_green_bands="AMP-HV,PHASE-HV,AMP-VH,PHASE-VH";
            sinclair_blue_bands="AMP-HH";
          } else if (md->general->band_count == 9) {
            // airsar data looks like this
            asfRequire(strcmp(band_name[0], "POWER") == 0 &&
                       strcmp(band_name[1], "SHH_AMP") == 0 &&
                       strcmp(band_name[2], "SHH_PHASE") == 0 &&
                       strcmp(band_name[3], "SHV_AMP") == 0 &&
                       strcmp(band_name[4], "SHV_PHASE") == 0 &&
                       strcmp(band_name[5], "SVH_AMP") == 0 &&
                       strcmp(band_name[6], "SVH_PHASE") == 0 &&
                       strcmp(band_name[7], "SVV_AMP") == 0 &&
                       strcmp(band_name[8], "SVV_PHASE") == 0,
                "Attempted to apply Sinclair to non-quad pole data.\n");
            quad_pol_type=QP_TYPE_AIRSAR;
            sinclair_red_bands="SVV_AMP";
            sinclair_green_bands="SHV_AMP,SHV_PHASE,SVH_AMP,SVH_PHASE";
            sinclair_blue_bands="SHH_AMP";
          } else {
            asfPrintError("Attempted to apply Sinclair to non-quad-pol data.\n");
          }
      }
      else if (pauli)
      {
          // Sanity checks -- require quad-pol complex data
          if (md->general->band_count == 8) {
            // palsar 1.1
            asfRequire(strcmp(band_name[0], "AMP-HH") == 0 &&
                       strcmp(band_name[1], "PHASE-HH") == 0 &&
                       strcmp(band_name[2], "AMP-HV") == 0 &&
                       strcmp(band_name[3], "PHASE-HV") == 0 &&
                       strcmp(band_name[4], "AMP-VH") == 0 &&
                       strcmp(band_name[5], "PHASE-VH") == 0 &&
                       strcmp(band_name[6], "AMP-VV") == 0 &&
                       strcmp(band_name[7], "PHASE-VV") == 0,
                "Attempted to apply Pauli to non-quad-pol, non-complex data.\n");

            quad_pol_type=QP_TYPE_PALSAR11;
            pauli_red_bands="AMP-HH,PHASE-HH,AMP-VV,PHASE-VV";
            pauli_green_bands="AMP-HV,PHASE-HV,AMP-VH,PHASE-VH";
            pauli_blue_bands="AMP-HH,PHASE-HH,AMP-VV,PHASE-VV";
            sinclair_red_bands="AMP-VV";
            sinclair_green_bands="AMP-HV,PHASE-HV,AMP-VH,PHASE-VH";
            sinclair_blue_bands="AMP-HH";
          } else if (md->general->band_count == 9) {
            // airsar
            asfRequire(strcmp(band_name[0], "POWER") == 0 &&
                       strcmp(band_name[1], "SHH_AMP") == 0 &&
                       strcmp(band_name[2], "SHH_PHASE") == 0 &&
                       strcmp(band_name[3], "SHV_AMP") == 0 &&
                       strcmp(band_name[4], "SHV_PHASE") == 0 &&
                       strcmp(band_name[5], "SVH_AMP") == 0 &&
                       strcmp(band_name[6], "SVH_PHASE") == 0 &&
                       strcmp(band_name[7], "SVV_AMP") == 0 &&
                       strcmp(band_name[8], "SVV_PHASE") == 0,
                "Attempted to apply Pauli to non-quad-pol, non-complex data.\n");

            quad_pol_type=QP_TYPE_AIRSAR;
            pauli_red_bands="SHH_AMP,SHH_PHASE,SVV_AMP,SVV_PHASE";
            pauli_green_bands="SHV_AMP,SHV_PHASE,SVH_AMP,SVH_PHASE";
            pauli_blue_bands="SHH_AMP,SHH_PHASE,SVV_AMP,SVV_PHASE";
            sinclair_red_bands="SVV_AMP";
            sinclair_green_bands="SHV_AMP,SHV_PHASE,SVH_AMP,SVH_PHASE";
            sinclair_blue_bands="SHH_AMP";
          } else {
            asfPrintError("Attempted to apply Pauli to non-quad-pol, non-complex data.\n");
          }
      }

      if (pauli && sample_mapping != NONE)
      {
        // Red channel statistics
        asfPrintStatus("\nGathering red channel statistics ...\n");
        calc_stats_from_file_with_formula(image_data_file_name,
                             pauli_red_bands, pauli_red, md->general->no_data,
                             &red_stats.min, &red_stats.max, &red_stats.mean,
                             &red_stats.standard_deviation, &red_stats.hist);
        if (sample_mapping == SIGMA) {
            double omin = red_stats.mean - 2*red_stats.standard_deviation;
            double omax = red_stats.mean + 2*red_stats.standard_deviation;
            if (omin > red_stats.min) red_stats.min = omin;
            if (omax < red_stats.max) red_stats.max = omax;
        }
        if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
            red_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
            gsl_histogram_pdf_init (red_stats.hist_pdf, red_stats.hist);
        }

        // Green channel statistics
        asfPrintStatus("\nGathering green channel statistics ...\n");
        calc_stats_from_file_with_formula(image_data_file_name,
                             pauli_green_bands, pauli_green, md->general->no_data,
                             &green_stats.min, &green_stats.max,
                             &green_stats.mean, &green_stats.standard_deviation,
                             &green_stats.hist);
        if (sample_mapping == SIGMA) {
            double omin = green_stats.mean - 2*green_stats.standard_deviation;
            double omax = green_stats.mean + 2*green_stats.standard_deviation;
            if (omin > green_stats.min) green_stats.min = omin;
            if (omax < green_stats.max) green_stats.max = omax;
        }
        if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
            green_stats.hist_pdf = gsl_histogram_pdf_alloc(256);
            gsl_histogram_pdf_init (green_stats.hist_pdf, green_stats.hist);
        }

        // Blue channel statistics
        asfPrintStatus("\nGathering blue channel statistics ...\n");
        calc_stats_from_file_with_formula(image_data_file_name,
                             pauli_blue_bands, pauli_blue, md->general->no_data,
                             &blue_stats.min, &blue_stats.max,
                             &blue_stats.mean, &blue_stats.standard_deviation,
                             &blue_stats.hist);
        if (sample_mapping == SIGMA) {
            double omin = blue_stats.mean - 2*blue_stats.standard_deviation;
            double omax = blue_stats.mean + 2*blue_stats.standard_deviation;
            if (omin > blue_stats.min) blue_stats.min = omin;
            if (omax < blue_stats.max) blue_stats.max = omax;
        }
        if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
            blue_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
            gsl_histogram_pdf_init (blue_stats.hist_pdf, blue_stats.hist);
        }
      }
      else if (sinclair && sample_mapping != NONE)
      {
        // Red channel statistics
        asfPrintStatus("\nGathering red channel statistics ...\n");
        calc_stats_from_file(image_data_file_name, sinclair_red_bands,
                             md->general->no_data,
                             &red_stats.min, &red_stats.max, &red_stats.mean,
                             &red_stats.standard_deviation, &red_stats.hist);
        if (sample_mapping == SIGMA) {
            double omin = red_stats.mean - 2*red_stats.standard_deviation;
            double omax = red_stats.mean + 2*red_stats.standard_deviation;
            if (omin > red_stats.min) red_stats.min = omin;
            if (omax < red_stats.max) red_stats.max = omax;
        }
        if (sample_mapping == HISTOGRAM_EQUALIZE ) {
            red_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
            gsl_histogram_pdf_init (red_stats.hist_pdf, red_stats.hist);
        }

        // Green channel statistics
        asfPrintStatus("\nGathering green channel statistics ...\n");
        calc_stats_from_file_with_formula(image_data_file_name,
                         sinclair_green_bands,
                         sinclair_green_cpx, md->general->no_data,
                         &green_stats.min, &green_stats.max,
                         &green_stats.mean,
                         &green_stats.standard_deviation,
                         &green_stats.hist);
        if (sample_mapping == SIGMA) {
            double omin = green_stats.mean - 2*green_stats.standard_deviation;
            double omax = green_stats.mean + 2*green_stats.standard_deviation;
            if (omin > green_stats.min) green_stats.min = omin;
            if (omax < green_stats.max) green_stats.max = omax;
        }
        if (sample_mapping == HISTOGRAM_EQUALIZE ) {
            green_stats.hist_pdf = gsl_histogram_pdf_alloc(256);
            gsl_histogram_pdf_init (green_stats.hist_pdf, green_stats.hist);
        }

        // Blue channel statistics
        asfPrintStatus("\nGathering blue channel statistics ...\n");
        calc_stats_from_file(image_data_file_name, sinclair_blue_bands,
                             md->general->no_data,
                             &blue_stats.min, &blue_stats.max,
                             &blue_stats.mean,
                             &blue_stats.standard_deviation,
                             &blue_stats.hist);
        if (sample_mapping == SIGMA) {
            double omin = blue_stats.mean - 2*blue_stats.standard_deviation;
            double omax = blue_stats.mean + 2*blue_stats.standard_deviation;
            if (omin > blue_stats.min) blue_stats.min = omin;
            if (omax < blue_stats.max) blue_stats.max = omax;
        }
        if (sample_mapping == HISTOGRAM_EQUALIZE ) {
            blue_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
            gsl_histogram_pdf_init (blue_stats.hist_pdf, blue_stats.hist);
        }
      }
      else
      {
        /*** Normal straight per-channel stats (no combined-band stats) */

        // Red channel statistics
        if (!ignored[0]                           &&  // Non-blank band
            sample_mapping != NONE                &&  // Float-to-byte resampling needed
            sample_mapping != HISTOGRAM_EQUALIZE  &&  // A histogram is not needed
            md->stats      != NULL                &&  // Stats exist and are valid
            md->stats       > 0                   &&
            meta_is_valid_string(band_name[0])    &&  // Band name exists and is valid
            strlen(band_name[0]) > 0)
        {
          // If the stats already exist, then use them
          int band_no = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[0]);
          red_stats.min  = md->stats->band_stats[band_no].min;
          red_stats.max  = md->stats->band_stats[band_no].max;
          red_stats.mean = md->stats->band_stats[band_no].mean;
          red_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
          red_stats.hist     = NULL;
          red_stats.hist_pdf = NULL;
          if (sample_mapping == SIGMA) {
            double omin = red_stats.mean - 2*red_stats.standard_deviation;
            double omax = red_stats.mean + 2*red_stats.standard_deviation;
            if (omin > red_stats.min) red_stats.min = omin;
            if (omax < red_stats.max) red_stats.max = omax;
          }
        }
        else {
          // Calculate the stats if you have to...
          if (sample_mapping != NONE && !ignored[0]) { // byte image
            asfPrintStatus("\nGathering red channel statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[0],
                                md->general->no_data,
                                &red_stats.min, &red_stats.max, &red_stats.mean,
                                &red_stats.standard_deviation, &red_stats.hist);
            if (sample_mapping == SIGMA) {
              double omin = red_stats.mean - 2*red_stats.standard_deviation;
              double omax = red_stats.mean + 2*red_stats.standard_deviation;
              if (omin > red_stats.min) red_stats.min = omin;
              if (omax < red_stats.max) red_stats.max = omax;
            }
            if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
              red_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
              gsl_histogram_pdf_init (red_stats.hist_pdf, red_stats.hist);
            }
          }
        }

        // Green channel statistics
        if (!ignored[0]                           &&  // Non-blank band
             sample_mapping != NONE                &&  // Float-to-byte resampling needed
             sample_mapping != HISTOGRAM_EQUALIZE  &&  // A histogram is not needed
             md->stats      != NULL                &&  // Stats exist and are valid
             md->stats       > 0                   &&
             meta_is_valid_string(band_name[0])    &&  // Band name exists and is valid
             strlen(band_name[0]) > 0)
        {
          // If the stats already exist, then use them
          int band_no = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[0]);
          green_stats.min  = md->stats->band_stats[band_no].min;
          green_stats.max  = md->stats->band_stats[band_no].max;
          green_stats.mean = md->stats->band_stats[band_no].mean;
          green_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
          green_stats.hist     = NULL;
          green_stats.hist_pdf = NULL;
          if (sample_mapping == SIGMA) {
            double omin = green_stats.mean - 2*green_stats.standard_deviation;
            double omax = green_stats.mean + 2*green_stats.standard_deviation;
            if (omin > green_stats.min) green_stats.min = omin;
            if (omax < green_stats.max) green_stats.max = omax;
          }
        }
        else {
          // Calculate the stats if you have to...
          if (sample_mapping != NONE && !ignored[1]) { // byte image
            asfPrintStatus("\nGathering green channel statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[1],
                                md->general->no_data,
                                &green_stats.min, &green_stats.max,
                                &green_stats.mean,
                                &green_stats.standard_deviation,
                                &green_stats.hist);
            if (sample_mapping == SIGMA) {
              double omin = green_stats.mean - 2*green_stats.standard_deviation;
              double omax = green_stats.mean + 2*green_stats.standard_deviation;
              if (omin > green_stats.min) green_stats.min = omin;
              if (omax < green_stats.max) green_stats.max = omax;
            }
            if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
              green_stats.hist_pdf = gsl_histogram_pdf_alloc(256);
              gsl_histogram_pdf_init (green_stats.hist_pdf, green_stats.hist);
            }
          }
        }

        // Blue channel statistics
        if (!ignored[0]                           &&  // Non-blank band
             sample_mapping != NONE                &&  // Float-to-byte resampling needed
             sample_mapping != HISTOGRAM_EQUALIZE  &&  // A histogram is not needed
             md->stats      != NULL                &&  // Stats exist and are valid
             md->stats       > 0                   &&
             meta_is_valid_string(band_name[0])    &&  // Band name exists and is valid
             strlen(band_name[0]) > 0)
        {
          // If the stats already exist, then use them
          int band_no = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[0]);
          blue_stats.min  = md->stats->band_stats[band_no].min;
          blue_stats.max  = md->stats->band_stats[band_no].max;
          blue_stats.mean = md->stats->band_stats[band_no].mean;
          blue_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
          blue_stats.hist     = NULL;
          blue_stats.hist_pdf = NULL;
          if (sample_mapping == SIGMA) {
            double omin = blue_stats.mean - 2*blue_stats.standard_deviation;
            double omax = blue_stats.mean + 2*blue_stats.standard_deviation;
            if (omin > blue_stats.min) blue_stats.min = omin;
            if (omax < blue_stats.max) blue_stats.max = omax;
          }
        }
        else {
          // Calculate the stats if you have to...
          if (sample_mapping != NONE && !ignored[2]) { // byte image
            asfPrintStatus("\nGathering blue channel statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[2],
                                md->general->no_data,
                                &blue_stats.min, &blue_stats.max,
                                &blue_stats.mean,
                                &blue_stats.standard_deviation,
                                &blue_stats.hist);
            if (sample_mapping == SIGMA) {
              double omin = blue_stats.mean - 2*blue_stats.standard_deviation;
              double omax = blue_stats.mean + 2*blue_stats.standard_deviation;
              if (omin > blue_stats.min) blue_stats.min = omin;
              if (omax < blue_stats.max) blue_stats.max = omax;
            }
            if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
              blue_stats.hist_pdf = gsl_histogram_pdf_alloc (256);
              gsl_histogram_pdf_init (blue_stats.hist_pdf, blue_stats.hist);
            }
          }
        }
      }
    }

    // Get channel numbers
    if (sinclair && quad_pol_type==QP_TYPE_NON_CPX) {
        HH_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "HH");
        HV_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "HV");
        VH_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "VH");
        VV_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "VV");
    }
    else if (quad_pol_type==QP_TYPE_PALSAR11 && (pauli || sinclair)) {
        HH_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "AMP-HH");
        HV_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "AMP-HV");
        VH_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "AMP-VH");
        VV_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "AMP-VV");
        HH_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "PHASE-HH");
        HV_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "PHASE-HV");
        VH_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "PHASE-VH");
        VV_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "PHASE-VV");
    }
    else if (quad_pol_type==QP_TYPE_AIRSAR && (pauli || sinclair)) {
        HH_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SHH_AMP");
        HV_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SHV_AMP");
        VH_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SVH_AMP");
        VV_amp_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SVV_AMP");
        HH_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SHH_PHASE");
        HV_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SHV_PHASE");
        VH_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SVH_PHASE");
        VV_phase_channel = get_band_number(md->general->bands,
                                     md->general->band_count, "SVV_PHASE");
    }
    else {
        red_channel = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[0]);
        if (!ignored[0] && !(red_channel >= 0 && red_channel < MAX_BANDS)) {
            asfPrintError("Band number (%d) out of range for %s channel.\n",
                          red_channel, "red");
        }
        green_channel = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[1]);
        if (!ignored[1] && !(green_channel >= 0 && green_channel < MAX_BANDS)) {
            asfPrintError("Band number (%d) out of range for %s channel.\n",
                          green_channel, "green");
        }
        blue_channel = get_band_number(md->general->bands,
                                       md->general->band_count,
                                       band_name[2]);
        if (!ignored[2] && !(blue_channel >= 0 && blue_channel < MAX_BANDS)) {
            asfPrintError("Band number (%d) out of range for %s channel.\n",
                          blue_channel, "blue");
        }
    }

    float *red_float_line = NULL;
    float *green_float_line = NULL;
    float *blue_float_line = NULL;

    // these are only used by the pauli decomposition
    float *amp_HH_buf = NULL;
    float *phase_HH_buf = NULL;
    float *amp_VV_buf = NULL;
    float *phase_VV_buf = NULL;

    // these are only used by the sinclair decomposition, for complex data
    float *amp_HV_buf = NULL;
    float *phase_HV_buf = NULL;
    float *amp_VH_buf = NULL;
    float *phase_VH_buf = NULL;

    unsigned char *red_byte_line = NULL;
    unsigned char *green_byte_line = NULL;
    unsigned char *blue_byte_line = NULL;

    // Write the data to the file
    FILE *fp = FOPEN(image_data_file_name, "rb");

    int sample_count = md->general->sample_count;
    int offset = md->general->line_count;

    // Allocate some memory
    if (md->optical) {
      if (ignored[0])
        red_byte_line = (unsigned char *) CALLOC(sample_count, sizeof(char));
      else
        red_byte_line = (unsigned char *) MALLOC(sample_count * sizeof(char));

      if (ignored[1])
        green_byte_line = (unsigned char *) CALLOC(sample_count, sizeof(char));
      else
        green_byte_line = (unsigned char *) MALLOC(sample_count * sizeof(char));

      if (ignored[2])
        blue_byte_line = (unsigned char *) CALLOC(sample_count, sizeof(char));
      else
        blue_byte_line = (unsigned char *) MALLOC(sample_count * sizeof(char));
    }
    else {
      // Not optical data
      red_float_line = (float *) MALLOC(sample_count * sizeof(float));
      if (ignored[0]){
        for (ii=0; ii<sample_count; ++ii) {
          red_float_line[ii] = md->general->no_data;
        }
      }

      green_float_line = (float *) MALLOC(sample_count * sizeof(float));
      if (ignored[1]) {
        for (ii=0; ii<sample_count; ++ii) {
          green_float_line[ii] = md->general->no_data;
        }
      }

      blue_float_line = (float *) MALLOC(sample_count * sizeof(float));
      if (ignored[2]) {
        for (ii=0; ii<sample_count; ++ii) {
          blue_float_line[ii] = md->general->no_data;
        }
      }
    }

    if (pauli) {
        // These are temporary arrays for values used in the pauli calculation
        amp_HH_buf = MALLOC(sample_count * sizeof(float));
        phase_HH_buf = MALLOC(sample_count * sizeof(float));
        amp_VV_buf = MALLOC(sample_count * sizeof(float));
        phase_VV_buf = MALLOC(sample_count * sizeof(float));
    }

    if (sinclair && quad_pol_type!=QP_TYPE_NON_CPX) {
        // These are temporary arrays for values used in the sinclair calculation,
        // when the data is complex.  (For sinclair data, the calculation can
        // still be done on amplitude only data.)
        amp_HV_buf = MALLOC(sample_count * sizeof(float));
        phase_HV_buf = MALLOC(sample_count * sizeof(float));
        amp_VH_buf = MALLOC(sample_count * sizeof(float));
        phase_VH_buf = MALLOC(sample_count * sizeof(float));
    }

    double r_omin=0, r_omax=0;
    double g_omin=0, g_omax=0;
    double b_omin=0, b_omax=0;

    if (md->optical && (true_color || false_color)) {
      // NOTE: Using the stats from the metadata, if available, is only valid
      // if no histogram is necessary, else one must be generated via the
      // stats functions.  If true_color or false_color are selected, then sample_mapping
      // should NOT be HISTOGRAM_EQUALIZE in particular and should always be set
      // to SIGMA
      if (sample_mapping == NONE && (true_color || false_color)) {
          sample_mapping = SIGMA;
      }
      if (sample_mapping != SIGMA) {
        asfPrintWarning("Cannot combine true or false color options with sample mappings\n"
            "other than 2-sigma.  You selected %s.  Defaulting to 2-sigma...\n",
            sample_mapping == TRUNCATE ? "TRUNCATE" :
            sample_mapping == MINMAX ? "MINMAX" :
            sample_mapping == HISTOGRAM_EQUALIZE ? "HISTOGRAM_EQUALIZE" :
            "UNKNOWN or INVALID");
      }

      asfPrintStatus("\nSampling color channels for 2-sigma contrast-expanded %s output...\n",
                     true_color ? "True Color" : false_color ? "False Color" : "Unknown");

      // Set up red resampling
      if (md->stats                           &&
          md->stats->band_count >= 3          &&
          meta_is_valid_string(band_name[0])  &&
          strlen(band_name[0]) > 0            &&
          sample_mapping != HISTOGRAM_EQUALIZE)
      {
          // If the stats already exist, then use them
        int band_no = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[0]);
        red_stats.min  = md->stats->band_stats[band_no].min;
        red_stats.max  = md->stats->band_stats[band_no].max;
        red_stats.mean = md->stats->band_stats[band_no].mean;
        red_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
        red_stats.hist     = NULL;
        red_stats.hist_pdf = NULL;
      }
      else {
        asfPrintStatus("\nGathering red channel statistics...\n");
        calc_stats_from_file(image_data_file_name, band_name[0],
                             md->general->no_data,
                             &red_stats.min, &red_stats.max, &red_stats.mean,
                             &red_stats.standard_deviation, &red_stats.hist);
      }
      r_omin = red_stats.mean - 2*red_stats.standard_deviation;
      r_omax = red_stats.mean + 2*red_stats.standard_deviation;
      if (r_omin < red_stats.min) r_omin = red_stats.min;
      if (r_omax > red_stats.max) r_omax = red_stats.max;

      // Set up green resampling
      if (md->stats                           &&
          md->stats->band_count >= 3          &&
          meta_is_valid_string(band_name[1])  &&
          strlen(band_name[1]) > 0            &&
          sample_mapping != HISTOGRAM_EQUALIZE)
      {
        // If the stats already exist, then use them
        int band_no = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[1]);
        green_stats.min  = md->stats->band_stats[band_no].min;
        green_stats.max  = md->stats->band_stats[band_no].max;
        green_stats.mean = md->stats->band_stats[band_no].mean;
        green_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
        green_stats.hist     = NULL;
        green_stats.hist_pdf = NULL;
      }
      else {
        asfPrintStatus("\nGathering green channel statistics...\n");
        calc_stats_from_file(image_data_file_name, band_name[1],
                              md->general->no_data,
                              &green_stats.min, &green_stats.max, &green_stats.mean,
                              &green_stats.standard_deviation, &green_stats.hist);
      }
      g_omin = green_stats.mean - 2*green_stats.standard_deviation;
      g_omax = green_stats.mean + 2*green_stats.standard_deviation;
      if (g_omin < green_stats.min) g_omin = green_stats.min;
      if (g_omax > green_stats.max) g_omax = green_stats.max;

      // Set up blue resampling
      if (md->stats                           &&
          md->stats->band_count >= 3          &&
          meta_is_valid_string(band_name[2])  &&
          strlen(band_name[2]) > 0            &&
          sample_mapping != HISTOGRAM_EQUALIZE)
      {
        // If the stats already exist, then use them
        int band_no = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[2]);
        blue_stats.min  = md->stats->band_stats[band_no].min;
        blue_stats.max  = md->stats->band_stats[band_no].max;
        blue_stats.mean = md->stats->band_stats[band_no].mean;
        blue_stats.standard_deviation = md->stats->band_stats[band_no].std_deviation;
        blue_stats.hist     = NULL;
        blue_stats.hist_pdf = NULL;
      }
      else {
        asfPrintStatus("\nGathering blue channel statistics...\n\n");
        calc_stats_from_file(image_data_file_name, band_name[2],
                             md->general->no_data,
                             &blue_stats.min, &blue_stats.max, &blue_stats.mean,
                             &blue_stats.standard_deviation, &blue_stats.hist);
      }
      b_omin = blue_stats.mean - 2*blue_stats.standard_deviation;
      b_omax = blue_stats.mean + 2*blue_stats.standard_deviation;
      if (b_omin < blue_stats.min) b_omin = blue_stats.min;
      if (b_omax > blue_stats.max) b_omax = blue_stats.max;

      asfPrintStatus("Applying 2-sigma contrast expansion to color bands...\n\n");
    }

    for (ii=0; ii<md->general->line_count; ii++) {
      if (md->optical) {
        // Optical images come as byte in the first place
        if (!ignored[0])
          get_byte_line(fp, md, ii+red_channel*offset, red_byte_line);
        if (!ignored[1])
          get_byte_line(fp, md, ii+green_channel*offset, green_byte_line);
        if (!ignored[2])
          get_byte_line(fp, md, ii+blue_channel*offset, blue_byte_line);
        // If true or false color flag was set, then (re)sample with 2-sigma
        // contrast expansion
        if (true_color || false_color) {
          for (jj=0; jj<sample_count; jj++) {
            red_byte_line[jj] =
                pixel_float2byte((float)red_byte_line[jj], SIGMA,
                                  r_omin, r_omax,
                                  red_stats.hist, red_stats.hist_pdf, NAN);
            green_byte_line[jj] =
                pixel_float2byte((float)green_byte_line[jj], SIGMA,
                                  g_omin, g_omax,
                                  green_stats.hist, green_stats.hist_pdf, NAN);
            blue_byte_line[jj] =
                pixel_float2byte((float)blue_byte_line[jj], SIGMA,
                                  b_omin, b_omax,
                                  blue_stats.hist, blue_stats.hist_pdf, NAN);
          }
        }
        if (format == TIF || format == GEOTIFF)
          write_rgb_tiff_byte2byte(otif, red_byte_line, green_byte_line,
                                   blue_byte_line, ii, sample_count);
        else if (format == JPEG)
          write_rgb_jpeg_byte2byte(ojpeg, red_byte_line, green_byte_line,
                                   blue_byte_line, &cinfo, sample_count);
        else if (format == PNG)
          write_rgb_png_byte2byte(opng, red_byte_line, green_byte_line,
                                  blue_byte_line, png_ptr, png_info_ptr,
                                  sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }
      else if (pauli) {
        // first need HH-VV && HH+VV, into red & blue, respectively
        get_float_line(fp, md, ii+HH_amp_channel*offset, amp_HH_buf);
        get_float_line(fp, md, ii+HH_phase_channel*offset, phase_HH_buf);
        get_float_line(fp, md, ii+VV_amp_channel*offset, amp_VV_buf);
        get_float_line(fp, md, ii+VV_phase_channel*offset, phase_VV_buf);
        for (jj=0; jj<md->general->sample_count; ++jj) {
          double val_arr[4];
          val_arr[0] = amp_HH_buf[jj];
          val_arr[1] = phase_HH_buf[jj];
          val_arr[2] = amp_VV_buf[jj];
          val_arr[3] = phase_VV_buf[jj];
          red_float_line[jj] = pauli_red(val_arr, md->general->no_data);
          blue_float_line[jj] = pauli_blue(val_arr, md->general->no_data);
        }

        // now HV into green
        get_float_line(fp, md, ii+HV_amp_channel*offset, green_float_line);

        // write out as normal
        if (format == TIF || format == GEOTIFF) {
            if (sample_mapping == NONE)
              write_rgb_tiff_float2float(otif, red_float_line, green_float_line,
                                    blue_float_line, ii, sample_count);
            else
              write_rgb_tiff_float2byte(otif, red_float_line, green_float_line,
                                    blue_float_line, red_stats, green_stats,
                                    blue_stats, sample_mapping,
                                    md->general->no_data, ii, sample_count);
        } else if (format == JPEG)
          write_rgb_jpeg_float2byte(ojpeg, red_float_line, green_float_line,
                                    blue_float_line, &cinfo, red_stats,
                                    green_stats, blue_stats, sample_mapping,
                                    md->general->no_data, sample_count);
        else if (format == PNG)
          write_rgb_png_float2byte(opng, red_float_line, green_float_line,
                                   blue_float_line, png_ptr, png_info_ptr,
                                   red_stats, green_stats, blue_stats,
                                   sample_mapping, md->general->no_data,
                                   sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }
      else if (sinclair) {
          if (quad_pol_type != QP_TYPE_NON_CPX) {
            // first do the green (cross-term average) calculation
            get_float_line(fp, md, ii+HV_amp_channel*offset, amp_HV_buf);
            get_float_line(fp, md, ii+HV_phase_channel*offset, phase_HV_buf);
            get_float_line(fp, md, ii+VH_amp_channel*offset, amp_VH_buf);
            get_float_line(fp, md, ii+VH_phase_channel*offset, phase_VH_buf);

            for (jj=0; jj<md->general->sample_count; ++jj) {
                double val_arr[4];
                val_arr[0] = amp_HV_buf[jj];
                val_arr[1] = phase_HV_buf[jj];
                val_arr[2] = amp_VH_buf[jj];
                val_arr[3] = phase_VH_buf[jj];
                green_float_line[jj] = sinclair_green_cpx(val_arr, md->general->no_data);
            }

            // now VV into red, and HH into blue
            get_float_line(fp, md, ii+HH_amp_channel*offset, blue_float_line);
            get_float_line(fp, md, ii+VV_amp_channel*offset, red_float_line);
          }
          else { // sinclair && !is_complex
            // temporary, load HV & VH into red & blue
            get_float_line(fp, md, ii+HV_channel*offset, red_float_line);
            get_float_line(fp, md, ii+VH_channel*offset, blue_float_line);

            for (jj=0; jj<md->general->sample_count; ++jj) {
                double val_arr[2];
                val_arr[0] = red_float_line[jj]; // HV
                val_arr[1] = blue_float_line[jj]; // VH
                green_float_line[jj] = sinclair_green(val_arr, md->general->no_data);
            }

            // now VV into red, and HH into blue
            get_float_line(fp, md, ii+HH_channel*offset, blue_float_line);
            get_float_line(fp, md, ii+VV_channel*offset, red_float_line);
          }

          // write out as normal
          if (format == TIF || format == GEOTIFF) {
            if (sample_mapping == NONE)
                write_rgb_tiff_float2float(otif, red_float_line, green_float_line,
                                        blue_float_line, ii, sample_count);
            else
                write_rgb_tiff_float2byte(otif, red_float_line, green_float_line,
                                        blue_float_line, red_stats, green_stats,
                                        blue_stats, sample_mapping,
                                        md->general->no_data, ii, sample_count);
          } else if (format == JPEG) {
            write_rgb_jpeg_float2byte(ojpeg, red_float_line, green_float_line,
                                    blue_float_line, &cinfo, red_stats,
                                    green_stats, blue_stats, sample_mapping,
                                    md->general->no_data, sample_count);
          } else if (format == PNG) {
            write_rgb_png_float2byte(opng, red_float_line, green_float_line,
                                    blue_float_line, png_ptr, png_info_ptr,
                                    red_stats, green_stats, blue_stats,
                                    sample_mapping, md->general->no_data,
                                    sample_count);
          } else {
            asfPrintError("Impossible: unexpected format %d\n", format);
          }
      }
      else if (sample_mapping == NONE) {
        // Write float->float lines if float image
        if (!ignored[0])
          get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        if (!ignored[1])
          get_float_line(fp, md, ii+green_channel*offset, green_float_line);
        if (!ignored[2])
          get_float_line(fp, md, ii+blue_channel*offset, blue_float_line);
        if (format == GEOTIFF)
          write_rgb_tiff_float2float(otif, red_float_line, green_float_line,
                                     blue_float_line, ii, sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }
      else {
        // Write float->byte lines if byte image
        if (!ignored[0])
          get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        if (!ignored[1])
          get_float_line(fp, md, ii+green_channel*offset, green_float_line);
        if (!ignored[2])
          get_float_line(fp, md, ii+blue_channel*offset, blue_float_line);
        if (format == TIF || format == GEOTIFF)
          write_rgb_tiff_float2byte(otif, red_float_line, green_float_line,
                                    blue_float_line, red_stats, green_stats,
                                    blue_stats, sample_mapping,
                                    md->general->no_data, ii, sample_count);
        else if (format == JPEG)
          write_rgb_jpeg_float2byte(ojpeg, red_float_line, green_float_line,
                                    blue_float_line, &cinfo, red_stats,
                                    green_stats, blue_stats, sample_mapping,
                                    md->general->no_data, sample_count);
        else if (format == PNG)
          write_rgb_png_float2byte(opng, red_float_line, green_float_line,
                                   blue_float_line, png_ptr, png_info_ptr,
                                   red_stats, green_stats, blue_stats,
                                   sample_mapping, md->general->no_data,
                                   sample_count);
        else
          asfPrintError("Impossible: unexpected format %d\n", format);
      }

      asfLineMeter(ii, md->general->line_count);
    }

    // Free memory
    FREE(red_byte_line);
    FREE(green_byte_line);
    FREE(blue_byte_line);
    FREE(red_float_line);
    FREE(green_float_line);
    FREE(blue_float_line);
    FREE(amp_HH_buf);
    FREE(phase_HH_buf);
    FREE(amp_VV_buf);
    FREE(phase_VV_buf);
    FREE(amp_HV_buf);
    FREE(phase_HV_buf);
    FREE(amp_VH_buf);
    FREE(phase_VH_buf);

    // Finalize the chosen format
    if (format == TIF || format == GEOTIFF)
      finalize_tiff_file(otif, ogtif, is_geotiff);
    else if (format == JPEG)
      finalize_jpeg_file(ojpeg, &cinfo);
    else if (format == PNG)
      finalize_png_file(opng, png_ptr, png_info_ptr);
    else
      asfPrintError("Impossible: unexpected format %d\n", format);

    if (red_stats.hist) gsl_histogram_free(red_stats.hist);
    if (red_stats.hist_pdf) gsl_histogram_pdf_free(red_stats.hist_pdf);
    if (green_stats.hist) gsl_histogram_free(green_stats.hist);
    if (green_stats.hist_pdf) gsl_histogram_pdf_free(green_stats.hist_pdf);
    if (blue_stats.hist) gsl_histogram_free(blue_stats.hist);
    if (blue_stats.hist_pdf) gsl_histogram_pdf_free(blue_stats.hist_pdf);

    FCLOSE(fp);
  }
  else {
    // Single-band image output (one grayscale file for each available band)
    int free_band_names=FALSE;
    int band_count = md->general->band_count;
    char base_name[255], bands[1024];
    strcpy(bands, md->general->bands);
    strcpy(base_name, output_file_name);

    if (!band_name)
    {
      // caller did not pass in the band names -- we will have
      // to come up with some band names ourselves
      if (band_count == 1) {
        // only one band, just call it "01"
        band_name = (char **) CALLOC(MAX_BANDS, sizeof(char*));
        band_name[0] = (char*) MALLOC(sizeof(char)*100);
        strcpy(band_name[0], "01");
      }
      else if (have_look_up_table) {
        // when exporting a look up table, number the bands
        band_name = (char **) CALLOC(MAX_BANDS, sizeof(char*));
        int i;
        for (i=0; i<3; i++) {
          band_name[i] = (char*) MALLOC(sizeof(char)*100);
          sprintf(band_name[i], "%02d", i + 1);
        }
      }
      else {
        // get what is in the metadata
        int n;
        char *b = stripExt(image_data_file_name);
        band_name = find_single_band(b, "all", &n);
        asfRequire (n == band_count, "Band count inconsistent: %d != %d\n",
                    n, band_count);
        FREE(b);
      }

      // in all three cases, we must free "band_name"
      // (normally not freed, it the caller's)
      free_band_names = TRUE;
    }

    int kk;
    for (kk=0; kk<band_count; kk++) {
      if (band_name[kk]) {

        if (strcmp(band_name[0], MAGIC_UNSET_STRING) != 0)
          asfPrintStatus("Writing band '%s' ...\n", band_name[kk]);

        // Initialize the chosen format
        if (band_count > 1)
          append_band_ext(base_name, output_file_name, band_name[kk]);
        else
          append_band_ext(base_name, output_file_name, NULL);

        if (format == TIF) {
          is_geotiff = 0;
          append_ext_if_needed (output_file_name, ".tif", ".tiff");
          initialize_tiff_file(&otif, &ogtif, output_file_name,
                  metadata_file_name, is_geotiff,
                  sample_mapping, rgb, band_name, have_look_up_table);
        }
        else if (format == GEOTIFF) {
          append_ext_if_needed (output_file_name, ".tif", ".tiff");
          initialize_tiff_file(&otif, &ogtif, output_file_name,
                  metadata_file_name, is_geotiff,
                  sample_mapping, rgb, band_name, have_look_up_table);
        }
        else if (format == JPEG) {
          append_ext_if_needed (output_file_name, ".jpg", ".jpeg");
          initialize_jpeg_file(output_file_name, md,
                  &ojpeg, &cinfo, rgb);
        }
        else if (format == PNG) {
          append_ext_if_needed (output_file_name, ".png", NULL);
          initialize_png_file(output_file_name, md,
                  &opng, &png_ptr, &png_info_ptr, rgb);
        }
        else if (format == PGM) {
          append_ext_if_needed (output_file_name, ".pgm", ".pgm");
          initialize_pgm_file(output_file_name, md, &opgm);
        }
        else {
          asfPrintError("Impossible: unexpected format %d\n", format);
        }

        // Determine which channel to read
        int channel;
        if (md->general->band_count == 1)
          channel = 0;
        else
          channel = get_band_number(bands, band_count, band_name[kk]);
        asfRequire(channel >= 0 && channel <= MAX_BANDS,
                   "Band number out of range\n");

        int sample_count = md->general->sample_count;
        int offset = md->general->line_count;

        // Get the statistics if necessary
        channel_stats_t stats;
              stats.hist = NULL; stats.hist_pdf = NULL;

        if (!md->optical || sample_mapping != NONE) {
          asfRequire (sizeof(unsigned char) == 1,
                "Size of the unsigned char data type on this machine is "
                "different than expected.\n");
          if (md->stats                  &&
              md->stats->band_count > 0  &&
              sample_mapping != HISTOGRAM_EQUALIZE)
          {
            stats.min  = md->stats->band_stats[channel].min;
            stats.max  = md->stats->band_stats[channel].max;
            stats.mean = md->stats->band_stats[channel].mean;
            stats.standard_deviation = md->stats->band_stats[channel].std_deviation;
            stats.hist = NULL;
          }
          else {
            asfPrintStatus("Gathering statistics ...\n");
            calc_stats_from_file(image_data_file_name, band_name[kk],
                                md->general->no_data,
                                &stats.min, &stats.max, &stats.mean,
                                &stats.standard_deviation, &stats.hist);
          }
          if (sample_mapping == TRUNCATE) {
            if (stats.mean >= 255)
              asfPrintWarning("The image contains HIGH values and will turn out very\n"
                              "bright or all-white.\n  Min : %f\n  Max : %f\n  Mean: %f\n"
                              "=> Consider using a sample mapping method other than TRUNCATE\n",
                              stats.min, stats.max, stats.mean);
            if (stats.mean < 10)
              asfPrintWarning("The image contains LOW values and will turn out very\n"
                              "dark or all-black.\n  Min : %f\n  Max : %f\n  Mean: %f\n"
                              "=> Consider using a sample mapping method other than TRUNCATE\n",
                              stats.min, stats.max, stats.mean);
          }
          if (sample_mapping == SIGMA)
          {
            double omin = stats.mean - 2*stats.standard_deviation;
            double omax = stats.mean + 2*stats.standard_deviation;
            if (omin > stats.min) stats.min = omin;
            if (omax < stats.max) stats.max = omax;
          }
          if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
            stats.hist_pdf = gsl_histogram_pdf_alloc (256); //NUM_HIST_BINS);
            gsl_histogram_pdf_init (stats.hist_pdf, stats.hist);
          }
        }

        // Write the output image
        FILE *fp = FOPEN(image_data_file_name, "rb");
        float *float_line = (float *) MALLOC(sizeof(float) * sample_count);
        unsigned char *byte_line = MALLOC(sizeof(unsigned char) * sample_count);

        asfPrintStatus("\nWriting output file...\n");
        if (have_look_up_table) { // Apply look up table
          for (ii=0; ii<md->general->line_count; ii++ ) {
            if (md->optical) {
              get_byte_line(fp, md, ii+channel*offset, byte_line);
              if (format == TIF || format == GEOTIFF)
                write_tiff_byte2lut(otif, byte_line, ii, sample_count,
                                    look_up_table_name);
              else if (format == JPEG)
                write_jpeg_byte2lut(ojpeg, byte_line, &cinfo, sample_count,
                                    look_up_table_name);
              else if (format == PNG)
                write_png_byte2lut(opng, byte_line, png_ptr, png_info_ptr,
                                   sample_count, look_up_table_name);
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            else {
              get_float_line(fp, md, ii+channel*offset, float_line);
              if (format == TIF || format == GEOTIFF)
                write_tiff_float2lut(otif, float_line, stats, sample_mapping,
                                     md->general->no_data, ii, sample_count,
                                     look_up_table_name);
              else if (format == JPEG)
                write_jpeg_float2lut(ojpeg, float_line, &cinfo, stats,
                                     sample_mapping, md->general->no_data,
                                     sample_count, look_up_table_name);
              else if (format == PNG)
                write_png_float2lut(opng, float_line, png_ptr, png_info_ptr,
                                    stats, sample_mapping, md->general->no_data,
                                    sample_count, look_up_table_name);
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            asfLineMeter(ii, md->general->line_count);
          }
        }
        else { // Regular old single band image
          for (ii=0; ii<md->general->line_count; ii++ ) {
            if (md->optical) {
              get_byte_line(fp, md, ii+channel*offset, byte_line);
              if (strncmp(md->general->sensor_name, "PRISM", 5) == 0) {
                if (format == TIF || format == GEOTIFF)
                  write_tiff_byte2byte(otif, byte_line, stats, sample_mapping,
                                       sample_count, ii);
                else if (format == JPEG)
                  write_jpeg_byte2byte(ojpeg, byte_line, stats, sample_mapping,
                                       &cinfo, sample_count);
                else if (format == PNG)
                  write_png_byte2byte(opng, byte_line, stats, sample_mapping,
                                      png_ptr, png_info_ptr, sample_count);
                else if (format == PGM)
                  write_pgm_byte2byte(opgm, byte_line, stats, sample_mapping,
                                      sample_count);
                else
                  asfPrintError("Impossible: unexpected format %d\n", format);
              }
              else { // Not Prism
                if (sample_mapping != NONE) {
                  static int warned_just_once = FALSE;
                  if (!warned_just_once)
                    asfPrintWarning("Byte to byte sample remapping not supported for\n"
                                    "exporting multi-band optical images into individual\n"
                                    "output files.\n"
                                    " ...defaulting to no remapping.\n");
                    warned_just_once = TRUE;
                  }
                  if (format == TIF || format == GEOTIFF)
                    write_tiff_byte2byte(otif, byte_line, stats, NONE,
                                         sample_count, ii);
                  else if (format == JPEG)
                    write_jpeg_byte2byte(ojpeg, byte_line, stats, NONE,
                                         &cinfo, sample_count);
                  else if (format == PNG)
                    write_png_byte2byte(opng, byte_line, stats, NONE,
                                        png_ptr, png_info_ptr, sample_count);
                  else if (format == PGM)
                    write_pgm_byte2byte(opgm, byte_line, stats, NONE,
                                        sample_count);
                  else
                    asfPrintError("Impossible: unexpected format %d\n", format);
              }
            }
            else if (sample_mapping == NONE) {
              get_float_line(fp, md, ii+channel*offset, float_line);
              if (format == GEOTIFF)
                write_tiff_float2float(otif, float_line, ii);
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            else {
              get_float_line(fp, md, ii+channel*offset, float_line);
              if (format == TIF || format == GEOTIFF)
                write_tiff_float2byte(otif, float_line, stats, sample_mapping,
                                      md->general->no_data, ii, sample_count);
              else if (format == JPEG)
                write_jpeg_float2byte(ojpeg, float_line, &cinfo, stats,
                                      sample_mapping, md->general->no_data,
                                      sample_count);
              else if (format == PNG)
                write_png_float2byte(opng, float_line, png_ptr, png_info_ptr,
                                     stats, sample_mapping, md->general->no_data,
                                     sample_count);
              else if (format == PGM)
                write_pgm_float2byte(opgm, float_line, stats, sample_mapping,
                                     md->general->no_data, sample_count);
              else
                asfPrintError("Impossible: unexpected format %d\n", format);
            }
            asfLineMeter(ii, md->general->line_count);
          } // End for each line
        } // End if multi or single band

        // Free memory
        FREE(float_line);
        FREE(byte_line);
        if (stats.hist) gsl_histogram_free(stats.hist);
        if (stats.hist_pdf) gsl_histogram_pdf_free(stats.hist_pdf);

        // Finalize the chosen format
        if (format == TIF || format == GEOTIFF)
          finalize_tiff_file(otif, ogtif, is_geotiff);
        else if (format == JPEG)
          finalize_jpeg_file(ojpeg, &cinfo);
        else if (format == PNG)
          finalize_png_file(opng, png_ptr, png_info_ptr);
        else if (format == PGM)
          finalize_ppm_file(opgm);

        FCLOSE(fp);
      }
    } // End for each band (kk is band number)

    if (free_band_names) {
      for (ii=0; ii<band_count; ++ii)
        FREE(band_name[ii]);
      FREE(band_name);
    }
  }

  meta_free (md);
}

void append_band_names(char **band_names, int rgb, char *citation, int have_look_up_table)
{
  char band_name[256];
  int i=0;
  sprintf(citation, "%s, %s: ", citation, BAND_ID_STRING);
  if (band_names) {
    if (rgb || have_look_up_table) {
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
  else {
    if (rgb || have_look_up_table)
      strcat(citation, "01,02,03");
    else
      strcat(citation, "01");
  }
}














































