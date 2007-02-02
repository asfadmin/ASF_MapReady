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

#include <asf.h>
#include <asf_nan.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_geocode.h>
#include <asf_export.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>

#define ASF_NAME_STRING "asf_export"
#define PPM_MAGIC_NUMBER "P6"

/* This constant is from the GeoTIFF spec.  It basically means that
   the system which would normally be specified by the field
   (projected coordinate system, datum, ellipsoid, whatever), in
   instead going to be specified by more detailed low level tags.  */
static const int user_defined_value_code = 32767;

int is_slant_range(meta_parameters *md);
double spheroid_diff_from_axis (spheroid_type_t spheroid,
                                double n_semi_major, double n_semi_minor);
spheroid_type_t axis2spheroid (double re_major, double re_minor);
int UTM_2_PCS(short *pcs, datum_type_t datum, unsigned long zone, char hem);
void gcs_2_string (char *datum_str, short gcs);
void pcs_2_string (char *datum_str, short pcs);
void datum_2_string (char *datum_str, datum_type_t datum);
void write_datum_key (GTIF *ogtif, datum_type_t datum, double re_major, double re_minor);
void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping, int rgb);
void write_tags_for_geotiff (GTIF *ogtif, const char *metadata_file_name);
void finalize_tiff_file(TIFF *otif, GTIF *ogtif, int is_geotiff);

void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping, int rgb)
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
    *ogtif = GTIFNew (*otif);
    asfRequire (*ogtif != NULL, "Error opening output GeoKey file descriptor.\n");

    write_tags_for_geotiff (*ogtif, metadata_file_name);
  }
  else {
    *ogtif = NULL;
  }
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
  // Reassure libjpeg that we will be writing a complete JPEG file.
  jpeg_start_compress (cinfo, TRUE);

  return;
}

void initialize_ppm_file(const char *output_file_name, 
			 meta_parameters *meta, FILE **oppm)
{
  const int max_color_value = 255;

  *oppm = FOPEN (output_file_name, "w");

  fprintf (*oppm, "%s\n", PPM_MAGIC_NUMBER);
  fprintf (*oppm, "%ld\n", (long int) meta->general->sample_count);
  fprintf (*oppm, "%ld\n", (long int) meta->general->line_count);
  fprintf (*oppm, "%d\n", max_color_value);

  return;  
}

void write_tags_for_geotiff (GTIF *ogtif, const char *metadata_file_name)
{
  /* Get the image metadata.  */
  meta_parameters *md = meta_read (metadata_file_name);
  int map_projected = is_map_projected(md);
  int is_slant_range_image = is_slant_range(md);
  TIFF *otif;

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
  if (map_projected)
  {
    // Write common tags for map-projected GeoTIFFs
    GTIFKeySet (ogtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet (ogtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
    GTIFKeySet (ogtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
    GTIFKeySet (ogtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
    GTIFKeySet (ogtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);

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

  if (md->projection) {
    if (!(meta_is_valid_double(md->projection->startX) &&
          meta_is_valid_double(md->projection->startY)) ||
        md->projection->startX < 0 ||
        md->projection->startY < 0
       )
    {
      asfPrintWarning("Metadata projection block contains invalid startX "
          "or startY values\n");
    }
    // Write georeferencing data
    if (!is_slant_range_image) {
      double tie_point[6];
      double pixel_scale[3];

      tie_point[0] = 0.0;
      tie_point[1] = 0.0;
      tie_point[2] = 0.0;
      tie_point[3] = md->projection->startX;
      tie_point[4] = md->projection->startY;
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
    int max_citation_length = 512;
    char *citation;
    int citation_length;

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
          citation_length = snprintf (citation, max_citation_length + 1,
                                      "UTM zone %d %c projected GeoTIFF on %s "
                                      "datum written by Alaska Satellite Facility tools.",
                                      md->projection->param.utm.zone, md->projection->hem,
                                      datum_str);
          asfRequire((citation_length >= 0) && (citation_length <= max_citation_length),
                     "GeoTIFF citation too long" );
          GTIFKeySet (ogtif, PCSCitationGeoKey, TYPE_ASCII, 1, citation);
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
        GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.std_parallel1);
        GTIFKeySet (ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.std_parallel2);
        GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.false_easting);
        GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.false_northing);
        GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.orig_latitude);
        // The following is where ArcGIS looks for the center meridian
        GTIFKeySet (ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.center_meridian);
        // The following is where the center meridian _should_ be stored
        GTIFKeySet (ogtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.albers.center_meridian);
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length =
            snprintf (citation, max_citation_length + 1,
                      "Albers equal-area conic projected GeoTIFF using %s "
                      "datum written by Alaska Satellite Facility "
                      "tools.", datum_str);
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
        GTIFKeySet (ogtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.plat1);
        GTIFKeySet (ogtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.plat2);
        GTIFKeySet (ogtif, ProjFalseOriginEastingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.false_easting);
        GTIFKeySet (ogtif, ProjFalseOriginNorthingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.false_northing);
        GTIFKeySet (ogtif, ProjFalseOriginLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.lon0);
        GTIFKeySet (ogtif, ProjFalseOriginLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamcc.lat0);
       // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length =
            snprintf (citation, max_citation_length + 1,
                      "Lambert conformal conic projected GeoTIFF using %s "
                      "datum written by Alaska Satellite Facility "
                      "tools.", datum_str);
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
        GTIFKeySet (ogtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.ps.slon);
        GTIFKeySet (ogtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.ps.slat);
        GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.ps.false_easting);
        GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.ps.false_northing);
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length =
            snprintf (citation, max_citation_length + 1,
                      "Polar stereographic projected GeoTIFF using %s "
                      "datum written by Alaska Satellite Facility "
                      "tools.", datum_str);
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
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      {
        GTIFKeySet (ogtif, ProjectedCSTypeGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjectionGeoKey, TYPE_SHORT, 1,
                    user_defined_value_code);
        GTIFKeySet (ogtif, ProjCoordTransGeoKey, TYPE_SHORT, 1,
                    CT_LambertAzimEqualArea);
        GTIFKeySet (ogtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamaz.center_lon);
        GTIFKeySet (ogtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamaz.center_lat);
        GTIFKeySet (ogtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamaz.false_easting);
        GTIFKeySet (ogtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE, 1,
                    md->projection->param.lamaz.false_northing);
        // This writes the GeographicTypeGeoKey
        write_datum_key (ogtif, md->projection->datum, re_major, re_minor);

        /* Set the citation key.  */
        char datum_str[256];
        datum_2_string (datum_str, md->projection->datum);
        citation = MALLOC ((max_citation_length + 1) * sizeof (char));
        citation_length =
            snprintf (citation, max_citation_length + 1,
                      "Lambert azimuthal equal area projected GeoTIFF using "
                      "%s datum written by Alaska Satellite "
                      "Facility tools.", datum_str);
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

void finalize_ppm_file(FILE *oppm)
{
  FCLOSE(oppm);
}

void write_tiff_byte2byte(TIFF *otif, unsigned char *byte_line, int line)
{
  TIFFWriteScanline (otif, byte_line, line, 0);
}

void write_tiff_float2float(TIFF *otif, float *float_line, int line)
{
  TIFFWriteScanline (otif, float_line, line, 0);
}

void write_tiff_float2byte(TIFF *otif, float *float_line,
			   channel_stats_t stats, scale_t sample_mapping,
			   float no_data, int line, int sample_count)
{
  int jj;
  unsigned char *byte_line;

  byte_line = (unsigned char *) MALLOC(sizeof(unsigned char) * sample_count);

  for (jj=0; jj<sample_count; jj++) {
    byte_line[jj] =
      pixel_float2byte(float_line[jj], sample_mapping, stats.min, stats.max,
		       stats.hist, stats.hist_pdf, no_data);
  }
  TIFFWriteScanline (otif, byte_line, line, 0);
  FREE(byte_line);
}

void write_rgb_tiff_byte2byte(TIFF *otif,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
			      int line, int sample_count)
{
  int jj;
  unsigned char *rgb_byte_line;

  rgb_byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_byte_line[jj*3] = red_byte_line[jj];
    rgb_byte_line[(jj*3)+1] = green_byte_line[jj];
    rgb_byte_line[(jj*3)+2] = blue_byte_line[jj];
  }
  TIFFWriteScanline (otif, rgb_byte_line, line, 0);
  FREE(rgb_byte_line);
}

void write_rgb_tiff_float2float(TIFF *otif,
				float *red_float_line,
				float *green_float_line,
				float *blue_float_line,
				int line, int sample_count)
{
  int jj;
  float *rgb_float_line;

  rgb_float_line = (float *) MALLOC(sizeof(float) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_float_line[jj*3] = red_float_line[jj];
    rgb_float_line[(jj*3)+1] = green_float_line[jj];
    rgb_float_line[(jj*3)+2] = blue_float_line[jj];
  }
  TIFFWriteScanline (otif, rgb_float_line, line, 0);
  FREE(rgb_float_line);
}

void write_rgb_tiff_float2byte(TIFF *otif,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int line, int sample_count)
{
  int jj;
  unsigned char *rgb_byte_line;

  rgb_byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_byte_line[jj*3] =
      pixel_float2byte(red_float_line[jj], sample_mapping,
		       red_stats.min, red_stats.max, red_stats.hist,
		       red_stats.hist_pdf, no_data);
    rgb_byte_line[(jj*3)+1] =
      pixel_float2byte(green_float_line[jj], sample_mapping,
		       green_stats.min, green_stats.max, green_stats.hist,
		       green_stats.hist_pdf, no_data);
    rgb_byte_line[(jj*3)+2] =
      pixel_float2byte(blue_float_line[jj], sample_mapping,
		       blue_stats.min, blue_stats.max, blue_stats.hist,
		       blue_stats.hist_pdf, no_data);
  }
  TIFFWriteScanline (otif, rgb_byte_line, line, 0);
  FREE(rgb_byte_line);
}

void write_jpeg_byte2byte(FILE *ojpeg, unsigned char *byte_line,
			  struct jpeg_compress_struct *cinfo,
			  int line, int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj] = (JSAMPLE) byte_line[jj];
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_jpeg_float2byte(FILE *ojpeg, float *float_line,
			   struct jpeg_compress_struct *cinfo,
			   channel_stats_t stats,
			   scale_t sample_mapping,
			   float no_data, int line, int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj] = (JSAMPLE)
      pixel_float2byte(float_line[jj], sample_mapping, stats.min, stats.max,
		       stats.hist, stats.hist_pdf, no_data);
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_rgb_jpeg_byte2byte(FILE *ojpeg,
			      unsigned char *red_byte_line,
			      unsigned char *green_byte_line,
			      unsigned char *blue_byte_line,
			      struct jpeg_compress_struct *cinfo,
			      int line, int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count * 3);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj*3] = (JSAMPLE) red_byte_line[jj];
    jsample_row[(jj*3)+1] = (JSAMPLE) green_byte_line[jj];
    jsample_row[(jj*3)+2] = (JSAMPLE) blue_byte_line[jj];
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_rgb_jpeg_float2byte(FILE *ojpeg,
			       float *red_float_line,
			       float *green_float_line,
			       float *blue_float_line,
			       struct jpeg_compress_struct *cinfo,
			       channel_stats_t red_stats,
			       channel_stats_t green_stats,
			       channel_stats_t blue_stats,
			       scale_t sample_mapping,
			       float no_data, int line, int sample_count)
{
  int jj;

  JSAMPLE *jsample_row = g_new (JSAMPLE, sample_count * 3);
  JSAMPROW *row_pointer = MALLOC (sizeof (JSAMPROW));

  for (jj=0; jj<sample_count; jj++) {
    jsample_row[jj*3] = (JSAMPLE)
      pixel_float2byte(red_float_line[jj], sample_mapping,
		       red_stats.min, red_stats.max, red_stats.hist,
		       red_stats.hist_pdf, no_data);
    jsample_row[(jj*3)+1] = (JSAMPLE)
      pixel_float2byte(green_float_line[jj], sample_mapping,
		       green_stats.min, green_stats.max, green_stats.hist,
		       green_stats.hist_pdf, no_data);
    jsample_row[(jj*3)+2] = (JSAMPLE)
      pixel_float2byte(blue_float_line[jj], sample_mapping,
		       blue_stats.min, blue_stats.max, blue_stats.hist,
		       blue_stats.hist_pdf, no_data);
  }
  row_pointer[0] = jsample_row;
  jpeg_write_scanlines (cinfo, row_pointer, 1);
  g_free (jsample_row);
  FREE (row_pointer);
}

void write_ppm_byte2byte(FILE *oppm,
			 unsigned char *red_byte_line,
			 unsigned char *green_byte_line,
			 unsigned char *blue_byte_line,
			 int line, int sample_count)
{
  int jj;
  unsigned char *rgb_byte_line;

  rgb_byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_byte_line[jj*3] = red_byte_line[jj];
    rgb_byte_line[(jj*3)+1] = green_byte_line[jj];
    rgb_byte_line[(jj*3)+2] = blue_byte_line[jj];
  }
  FWRITE(rgb_byte_line, sizeof(unsigned char)*3, sample_count, oppm);
  FREE(rgb_byte_line);
}

void write_ppm_float2byte(FILE *oppm,
			  float *red_float_line,
			  float *green_float_line,
			  float *blue_float_line,
			  channel_stats_t red_stats,
			  channel_stats_t green_stats,
			  channel_stats_t blue_stats,
			  scale_t sample_mapping,
			  float no_data, int line, int sample_count)
{
  int jj;
  unsigned char *rgb_byte_line;

  rgb_byte_line = (unsigned char *)
    MALLOC(sizeof(unsigned char) * sample_count * 3);

  for (jj=0; jj<sample_count; jj++) {
    rgb_byte_line[jj*3] =
      pixel_float2byte(red_float_line[jj], sample_mapping,
		       red_stats.min, red_stats.max, red_stats.hist,
		       red_stats.hist_pdf, no_data);
    rgb_byte_line[(jj*3)+1] =
      pixel_float2byte(green_float_line[jj], sample_mapping,
		       green_stats.min, green_stats.max, green_stats.hist,
		       green_stats.hist_pdf, no_data);
    rgb_byte_line[(jj*3)+2] =
      pixel_float2byte(blue_float_line[jj], sample_mapping,
		       blue_stats.min, blue_stats.max, blue_stats.hist,
		       blue_stats.hist_pdf, no_data);
  }
  FWRITE(rgb_byte_line, sizeof(unsigned char)*3, sample_count, oppm);
  FREE(rgb_byte_line);
}

void
export_band_image (const char *metadata_file_name,
		   const char *image_data_file_name,
		   char *output_file_name,
		   scale_t sample_mapping,
		   char **band_name, int rgb,
		   output_format_t format)
{
  int map_projected;
  int is_geotiff = 1;
  TIFF *otif = NULL; // FILE* pointer for TIFF files
  GTIF *ogtif = NULL;
  FILE *ojpeg = NULL, *oppm=NULL;
  struct jpeg_compress_struct cinfo;
  ssize_t ii;

  meta_parameters *md = meta_read (metadata_file_name);
  map_projected = is_map_projected(md);

  if (rgb) {
    
    // Initialize the chosen format
    if (format == TIF) {
      is_geotiff = 0;
      initialize_tiff_file(&otif, &ogtif, output_file_name,
			   metadata_file_name, is_geotiff,
			   sample_mapping, rgb);
    }
    else if (format == GEOTIFF) {
      initialize_tiff_file(&otif, &ogtif, output_file_name,
			   metadata_file_name, is_geotiff,
			   sample_mapping, rgb);
    }
    else if (format == JPEG) {
      initialize_jpeg_file(output_file_name, md, &ojpeg, &cinfo, rgb);
    }
    else if (format == PPM) {
      initialize_ppm_file(output_file_name, md, &oppm);
    }

    channel_stats_t red_stats, blue_stats, green_stats;

    if (!md->optical) {
      // Red channel statistics
      if (sample_mapping != NONE) { // byte image
	asfRequire (sizeof(unsigned char) == 1,
		    "Size of the unsigned char data type on this machine is "
		    "different than expected.\n");
	asfPrintStatus("Gathering red channel statistics ...\n");
	calc_stats_from_file(image_data_file_name, band_name[0], 0.0,
			     &red_stats.min, &red_stats.max, &red_stats.mean,
			     &red_stats.standard_deviation, red_stats.hist);
	if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	  red_stats.hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	  gsl_histogram_pdf_init (red_stats.hist_pdf, red_stats.hist);
	}
      }

      // Green channel statistics
      if (sample_mapping != NONE) { // byte image
	asfRequire (sizeof(unsigned char) == 1,
		    "Size of the unsigned char data type on this machine is "
		    "different than expected.\n");
	asfPrintStatus("Gathering green channel statistics ...\n");
	calc_stats_from_file(image_data_file_name, band_name[1], 0.0,
			     &green_stats.min, &green_stats.max,
			     &green_stats.mean,
			     &green_stats.standard_deviation,
			     green_stats.hist);
	if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	  green_stats.hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	  gsl_histogram_pdf_init (green_stats.hist_pdf, green_stats.hist);
	}
      }

      // Blue channel statistics
      if (sample_mapping != NONE) { // byte image
	asfRequire (sizeof(unsigned char) == 1,
		    "Size of the unsigned char data type on this machine is "
		    "different than expected.\n");
	asfPrintStatus("Gathering blue channel statistics ...\n");
	calc_stats_from_file(image_data_file_name, band_name[2], 0.0,
			     &blue_stats.min, &blue_stats.max,
			     &blue_stats.mean,
			     &blue_stats.standard_deviation,
			     blue_stats.hist);
	if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	  blue_stats.hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	  gsl_histogram_pdf_init (blue_stats.hist_pdf, blue_stats.hist);
	}
      }
    }

    // Write the actual image data
    FILE *fp;
    float *red_float_line, *green_float_line, *blue_float_line;
    unsigned char *red_byte_line, *green_byte_line, *blue_byte_line;

    fp = FOPEN(image_data_file_name, "rb");

    int red_channel = get_band_number(md->general->bands,
                                      md->general->band_count,
                                      band_name[0]);
    int green_channel = get_band_number(md->general->bands,
                                        md->general->band_count,
                                        band_name[1]);
    int blue_channel = get_band_number(md->general->bands,
                                       md->general->band_count,
                                       band_name[2]);
    asfRequire(red_channel >= 0 && red_channel < MAX_BANDS &&
               green_channel >= 0 && green_channel < MAX_BANDS &&
               blue_channel >= 0 && blue_channel < MAX_BANDS,
               "Band number out of range\n");

    int sample_count = md->general->sample_count;
    int offset = md->general->line_count;

    // Allocate some memory
    if (md->optical) {
      red_byte_line = (unsigned char *) MALLOC(sizeof(char) * sample_count);
      green_byte_line = (unsigned char *) MALLOC(sizeof(char) * sample_count);
      blue_byte_line = (unsigned char *) MALLOC(sizeof(char) * sample_count);
    }
    else {
      red_float_line = (float *) MALLOC(sizeof(float) * sample_count);
      green_float_line = (float *) MALLOC(sizeof(float) * sample_count);
      blue_float_line = (float *) MALLOC(sizeof(float) * sample_count);
    }

    for (ii=0; ii<md->general->line_count; ii++) {
      if (md->optical) {
	// Optical images come as byte in the first place
	get_byte_line(fp, md, ii+red_channel*offset, red_byte_line);
	get_byte_line(fp, md, ii+green_channel*offset, green_byte_line);
	get_byte_line(fp, md, ii+blue_channel*offset, blue_byte_line);
	if (format == TIF || format == GEOTIFF)
	  write_rgb_tiff_byte2byte(otif, red_byte_line, green_byte_line,
				   blue_byte_line, ii, sample_count);
	else if (format == JPEG)
	  write_rgb_jpeg_byte2byte(ojpeg, red_byte_line, green_byte_line,
				   blue_byte_line, &cinfo, ii, sample_count);
	else if (format == PPM)
	  write_ppm_byte2byte(oppm, red_byte_line, green_byte_line,
			      blue_byte_line, ii, sample_count);
      }
      else if (sample_mapping == NONE) {
        // Write float lines if float image
        get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        get_float_line(fp, md, ii+green_channel*offset, green_float_line);
        get_float_line(fp, md, ii+blue_channel*offset, blue_float_line);
	if (format == TIF || format == GEOTIFF)
	  write_rgb_tiff_float2float(otif, red_float_line, green_float_line,
				     blue_float_line, ii, sample_count);
      }
      else {
        // Write float lines if byte image
        get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        get_float_line(fp, md, ii+green_channel*offset, green_float_line);
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
				    md->general->no_data, ii, sample_count);
	else if (format == PPM)
	  write_ppm_float2byte(oppm, red_float_line, green_float_line,
			       blue_float_line, red_stats, green_stats,
			       blue_stats, sample_mapping,
			       md->general->no_data, ii, sample_count);
      }
      asfLineMeter(ii, md->general->line_count);
    }

    // Free memory
    if (md->optical) {
      if (red_byte_line) FREE(red_byte_line);
      if (green_byte_line) FREE(green_byte_line);
      if (blue_byte_line) FREE(blue_byte_line);
    }
    else {
      if (red_float_line) FREE(red_float_line);
      if (green_float_line) FREE(green_float_line);
      if (blue_float_line) FREE(blue_float_line);
    }
    
    // Finalize the chosen format
    if (format == TIF || format == GEOTIFF)
      finalize_tiff_file(otif, ogtif, is_geotiff);
    else if (format == JPEG)
      finalize_jpeg_file(ojpeg, &cinfo);
    else if (format == PPM)
      finalize_ppm_file(oppm);
  }
  else { // Single-band image output

    int band_count = md->general->band_count;
    char base_name[25], bands[25];
    sprintf(bands, md->general->bands);
    sprintf(base_name, output_file_name);
    int kk;
    for (kk=0; kk<band_count; kk++) {
      if (band_name[kk]) {

	if (band_name[1])
	  asfPrintStatus("Writing band '%s' ...\n", band_name[kk]);

	// Initialize the chosen format
	if (band_name[1])
	  append_band_ext(base_name, output_file_name, band_name[kk]);
	else
	  append_band_ext(base_name, output_file_name, NULL);
	if (format == TIF) {
	  is_geotiff = 0;
	  append_ext_if_needed (output_file_name, ".tif", ".tiff");
	  initialize_tiff_file(&otif, &ogtif, output_file_name,
			       metadata_file_name, is_geotiff,
			       sample_mapping, rgb);
	}
	else if (format == GEOTIFF) {
	  append_ext_if_needed (output_file_name, ".tif", ".tiff");
	  initialize_tiff_file(&otif, &ogtif, output_file_name, 
			       metadata_file_name, is_geotiff,
			       sample_mapping, rgb);
	}
	else if (format == JPEG) {
	  append_ext_if_needed (output_file_name, ".jpg", ".jpeg");
	  initialize_jpeg_file(output_file_name, md, 
			       &ojpeg, &cinfo, rgb);
	}
	else if (format == PPM) {
	  append_ext_if_needed (output_file_name, ".ppm", ".ppm");
	  initialize_ppm_file(output_file_name, md, &oppm);
	}

	// Determine which channel to read
	int channel = get_band_number(bands,
				      band_count,
				      band_name[kk]);
	asfRequire(channel >= 0 && channel <= MAX_BANDS,
		   "Band number out of range\n");
	
	int sample_count = md->general->sample_count;
	int offset = md->general->line_count;
	
	// Get the statistics if necessary
	channel_stats_t stats;
	if (!md->optical && sample_mapping != NONE) {
	  asfRequire (sizeof(unsigned char) == 1,
		      "Size of the unsigned char data type on this machine is "
		      "different than expected.\n");
	  asfPrintStatus("Gathering statistics ...\n");
	  calc_stats_from_file(image_data_file_name, band_name[0], 0.0,
			       &stats.min, &stats.max, &stats.mean, 
			       &stats.standard_deviation, stats.hist);
	  if ( sample_mapping == HISTOGRAM_EQUALIZE ) {
	    stats.hist_pdf = gsl_histogram_pdf_alloc (NUM_HIST_BINS);
	    gsl_histogram_pdf_init (stats.hist_pdf, stats.hist);
	  }
	}
	
	FILE *fp;
	float *float_line;
	unsigned char *byte_line;
	
	// Write the output image
	fp = FOPEN(image_data_file_name, "rb");
	float_line = (float *) MALLOC(sizeof(float) * sample_count);
	byte_line = 
	  (unsigned char *) MALLOC(sizeof(unsigned char) * sample_count);
	for (ii=0; ii<md->general->line_count; ii++ ) {
	  if (md->optical) {
	    get_byte_line(fp, md, ii+channel*offset, byte_line);
	    if (format == TIF || format == GEOTIFF)
	      write_tiff_byte2byte(otif, byte_line, ii);
	    else if (format == JPEG)
	      write_jpeg_byte2byte(ojpeg, byte_line, &cinfo, ii, sample_count);
	    else if (format == PPM)
	      write_ppm_byte2byte(oppm, byte_line, byte_line, byte_line, 
				  ii, sample_count);
	  }
	  else if (sample_mapping == NONE) {
	    get_float_line(fp, md, ii+channel*offset, float_line);
	    if (format == TIF || format == GEOTIFF)
	      write_tiff_float2float(otif, float_line, ii);
	  }
	  else {
	    get_float_line(fp, md, ii+channel*offset, float_line);
	  if (format == TIF || format == GEOTIFF)
	    write_tiff_float2byte(otif, float_line, stats, sample_mapping,
				  md->general->no_data, ii, sample_count);
	  else if (format == JPEG)
	    write_jpeg_float2byte(ojpeg, float_line, &cinfo, stats, 
				  sample_mapping, md->general->no_data, 
				  ii, sample_count);
	  else if (format == PPM)
	    write_ppm_float2byte(oppm, float_line, float_line, float_line, 
				 stats, stats, stats, sample_mapping,
				 md->general->no_data, ii, sample_count);
	  }
	  asfLineMeter(ii, md->general->line_count);
	}
	
	// Free memory
	if (float_line) FREE(float_line);
	if (byte_line) FREE(byte_line);

	// Finalize the chosen format
	if (format == TIF || format == GEOTIFF)
	  finalize_tiff_file(otif, ogtif, is_geotiff);
	else if (format == JPEG)
	  finalize_jpeg_file(ojpeg, &cinfo);
	else if (format == PPM)
	  finalize_ppm_file(oppm);
      }
    }
  }
  
  meta_free (md);
}

spheroid_type_t axis2spheroid (double re_major, double re_minor)
{
  struct fit {
    spheroid_type_t spheroid;
    double diff;
  }
  diff_array[10];

  // Find the fits (note: no guarantee that the enums will differ by whole numbers, so
  // step through manually rather than in a for-loop... )
  diff_array[0].spheroid = BESSEL_SPHEROID;
  diff_array[0].diff = spheroid_diff_from_axis(diff_array[0].spheroid, re_major, re_minor);

  diff_array[1].spheroid = CLARKE1866_SPHEROID;
  diff_array[1].diff = spheroid_diff_from_axis(diff_array[1].spheroid, re_major, re_minor);

  diff_array[2].spheroid = CLARKE1880_SPHEROID;
  diff_array[2].diff = spheroid_diff_from_axis(diff_array[2].spheroid, re_major, re_minor);

  diff_array[3].spheroid = GEM6_SPHEROID;
  diff_array[3].diff = spheroid_diff_from_axis(diff_array[3].spheroid, re_major, re_minor);

  diff_array[4].spheroid = GEM10C_SPHEROID;
  diff_array[4].diff = spheroid_diff_from_axis(diff_array[4].spheroid, re_major, re_minor);

  diff_array[5].spheroid = GRS1980_SPHEROID;
  diff_array[5].diff = spheroid_diff_from_axis(diff_array[5].spheroid, re_major, re_minor);

  diff_array[6].spheroid = INTERNATIONAL1924_SPHEROID;
  diff_array[6].diff = spheroid_diff_from_axis(diff_array[6].spheroid, re_major, re_minor);

  diff_array[7].spheroid = INTERNATIONAL1967_SPHEROID;
  diff_array[7].diff = spheroid_diff_from_axis(diff_array[7].spheroid, re_major, re_minor);

  diff_array[8].spheroid = WGS72_SPHEROID;
  diff_array[8].diff = spheroid_diff_from_axis(diff_array[8].spheroid, re_major, re_minor);

  diff_array[9].spheroid = WGS84_SPHEROID;
  diff_array[9].diff = spheroid_diff_from_axis(diff_array[9].spheroid, re_major, re_minor);

  // NOTE: Counting down (see below) rather than up puts a preference on using a newer or
  // more common spheroids rather than an older or less common ...look at the list above.
  // => GRS1980 and GEM10C will have similar results in general, so in this case in particular,
  // counting down will 'prefer' GRS1980 rather than GEM10C
  int min = 0;
  int i;
  for (i=9; i>=0; i--) {
    min = (diff_array[i].diff < diff_array[min].diff) ? i : min;
  }

  return diff_array[min].spheroid;
}

// Utility function written solely for the purpose of
// making if-statement (etc) conditional expressions
// cleaner/smaller/lighter/intuitive
//
// This function wouldn't be necessary except for the
// fact that ScanSAR images have the sar->image_type
// set to 'P' for 'projected', but unless geocoded,
// this means that they are projected to
// along-track/cross-track ...not map-projected.
// If 'P' exists, then additional checking is required
// to determine if the image is MAP-projected or not.
int is_slant_range(meta_parameters *md)
{
  // Convenience pointers
  meta_sar *ms = md->sar;
  meta_projection *mp = md->projection;

  // Return true if the image is projected and the
  // projection is one of the ASF-supported map
  // projection types
  if (ms) {
    return ( (ms->image_type == 'P' && mp->type == SCANSAR_PROJECTION) ||
	     ms->image_type == 'S'
	     ) ? 1 : 0;
  }
  else
    return 0;
}

double spheroid_diff_from_axis (spheroid_type_t spheroid, double n_semi_major, double n_semi_minor)
{
  double s_semi_major;
  double s_semi_minor;

  asfRequire(n_semi_major >= 0.0 && n_semi_minor >= 0,
             "Negative semi-major or semi-minor values found\n");
  asfRequire(n_semi_major <= MAXREAL && n_semi_minor <= MAXREAL,
             "Semi-major and/or semi-minor axis too large.\n");

  switch (spheroid) {
    case BESSEL_SPHEROID:
      s_semi_major = BESSEL_SEMIMAJOR;
      s_semi_minor = BESSEL_SEMIMAJOR * (1.0 - 1.0/BESSEL_INV_FLATTENING);
      break;
    case CLARKE1866_SPHEROID:
      s_semi_major = CLARKE1866_SEMIMAJOR;
      s_semi_minor = CLARKE1866_SEMIMAJOR * (1.0 - 1.0/CLARKE1866_INV_FLATTENING);
      break;
    case CLARKE1880_SPHEROID:
      s_semi_major = CLARKE1880_SEMIMAJOR;
      s_semi_minor = CLARKE1880_SEMIMAJOR * (1.0 - 1.0/CLARKE1880_INV_FLATTENING);
      break;
    case GEM6_SPHEROID:
      s_semi_major = GEM6_SEMIMAJOR;
      s_semi_minor = GEM6_SEMIMAJOR * (1.0 - 1.0/GEM6_INV_FLATTENING);
      break;
    case GEM10C_SPHEROID:
      s_semi_major = GEM10C_SEMIMAJOR;
      s_semi_minor = GEM10C_SEMIMAJOR * (1.0 - 1.0/GEM10C_INV_FLATTENING);
      break;
    case GRS1980_SPHEROID:
      s_semi_major = GRS1980_SEMIMAJOR;
      s_semi_minor = GRS1980_SEMIMAJOR * (1.0 - 1.0/GRS1980_INV_FLATTENING);
      break;
    case INTERNATIONAL1924_SPHEROID:
      s_semi_major = INTERNATIONAL1924_SEMIMAJOR;
      s_semi_minor = INTERNATIONAL1924_SEMIMAJOR * (1.0 - 1.0/INTERNATIONAL1924_INV_FLATTENING);
      break;
    case INTERNATIONAL1967_SPHEROID:
      s_semi_major = INTERNATIONAL1967_SEMIMAJOR;
      s_semi_minor = INTERNATIONAL1967_SEMIMAJOR * (1.0 - 1.0/INTERNATIONAL1967_INV_FLATTENING);
      break;
    case WGS72_SPHEROID:
      s_semi_major = WGS72_SEMIMAJOR;
      s_semi_minor = WGS72_SEMIMAJOR * (1.0 - 1.0/WGS72_INV_FLATTENING);
      break;
    case WGS84_SPHEROID:
      s_semi_major = WGS84_SEMIMAJOR;
      s_semi_minor = WGS84_SEMIMAJOR * (1.0 - 1.0/WGS84_INV_FLATTENING);
      break;
    default:
      asfPrintError("ERROR: Unsupported spheroid type in spheroid_axis_fit()\n");
      break;
  }

  // The following calculates an approximation of the sum-squared-error (SSE)
  // over a quarter-span of an ellipse defined by the passed-in semi-major/semi-minor
  // axis versus an ellipse defined by the semi-major/semi-minor axis from one of
  // our supported types of spheroids.  The square root of this value is returned
  // as a measure of fit between the two.
  //
  // Method:
  // The calculated x,y for one ellipse is used as a first reference point, then
  // for that x and y, points are found on the second ellipse by first using the
  // x and then by using the y.  This defines 2 points on the second ellipse over
  // a short span.  The average of these 2 points is fairly close to where a
  // normal (from either ellipse) would subtend the second ellipse.  We define this
  // as a second reference point.  The distance between these two reference points
  // is interpreted as the 'error' between the curves and what we square and sum
  // over the quarter-ellipse.
  //
  // This summation is a very good approximation of the true SSE if the two ellipsis
  // are not too different from each other, and for map projection spheroids,
  // this is a good assumption to make.
  //
  // FIXME: We can probably optimize the math below for better speed, but for now,
  // the code shows the goings-on in an intuitive manner instead.  I'll profile the
  // code later... although it won't get called much and speed shouldn't really BE
  // an issue :)
  //
  // Off we go...
  double SSE = 0.0;
  double x1, y1, x2, y2, xt1, yt1, xt2, yt2; // 2 ref pts, 2 tmp pts
  // 100,000 angular steps from 0 to PI/2 radians results in steps of about
  // 1 km in size along the surface.  The worst-case SSE caculated below
  // will still be far within max double limits ...but a higher or lower number
  // will slow or speed these calcs.  I think this number of steps is somewhat
  // optimal, noting that this function should rarely be called anyway.
  double dx, dy;
  double num_steps = 100000.0;
  double theta;
  double angular_step_size = (PI/2.0) / num_steps;

  // Use the minimum of the major axis for converting angle to x
  // since this is the upper limit for x, not because it is the
  // best answer (least-bias answer would be to use average of
  // all 4 axii).  A circle approximation should be close 'nuf
  // for determining x and x-step size.  Angular steps result in
  // (nearly) equal-distance steps along the ellipse's locus of
  // points (no polar or equatorial bias.)
  double h = MIN(s_semi_major, n_semi_major);
  double pi_over_2 = PI / 2.0;

  for (theta = 0.0; theta <= pi_over_2; theta += angular_step_size) {
    // Find first reference point, (x1, y1), on first ellipse
    x1 = h * cos(theta);
    y1 = sqrt(fabs(s_semi_minor * s_semi_minor *
        (1.0 - (x1 * x1)/(s_semi_major * s_semi_major))));

    // Find first temporary point, (xt1, yt1), on second ellipse
    xt1 = x1;
    yt1 = sqrt(fabs(n_semi_minor * n_semi_minor *
        (1.0 - (xt1 * xt1)/(n_semi_major * n_semi_major))));

    // Find second temporary point, (xt2, yt2), on second ellipse and
    // average the two temporary points
    yt2 = y1;
    xt2 = sqrt(fabs(n_semi_major * n_semi_major *
        (1.0 - (yt2 * yt2)/(n_semi_minor * n_semi_minor))));

    // On the chord from (xt1, yt1) to (xt2, yt2), find the
    // mid-point and 'pretend' it's on the second ellipse and
    // along the normal from the first to second (or vice versa).
    // => For small (dxt, dyt), this is a valid approximation.
    x2 = (xt1 + xt2)/2.0;
    y2 = (yt1 + yt2)/2.0;

    // Sum the squared Euclidean distance (the error between ellipsis)
    // into the sum-squared-error (SSE)
    dx = x2 - x1;
    dy = y2 - y1;
    SSE += dx*dx + dy*dy;
  }
  // Add in the error at theta = 0 and theta = PI/2
  dy = s_semi_minor - n_semi_minor;
  dx = s_semi_major - n_semi_major;
  SSE +=  dx*dx + dy*dy;

  // Return the square root of the SSE as a measure of fit
  return sqrt(SSE);
}

int UTM_2_PCS(short *pcs, datum_type_t datum, unsigned long zone, char hem)
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
  // For user-defined and unsupported UTM projections, NNN can be
  //   a variety of other numbers (see the GeoTIFF Standard)
  //
  // NOTE: For NAD27 and NAD83, only the restricted range of zones
  // above is supported by the GeoTIFF standard.
  //
  // NOTE: For ALOS's ITRF97 datum, note that it is based on
  // WGS84 and subsituting WGS84 for ITRF97 because the GeoTIFF
  // standard does not contain a PCS for ITRF97 (or any ITRFxx)
  // will result in errors of less than one meter.  So when
  // writing GeoTIFFs, we choose to use WGS84 when ITRF97 is
  // desired.
  //

  const short NNN_NAD27N = 267;
  const short NNN_NAD83N = 269;
  //const short NNN_WGS72N = 322; // Currently unsupported
  //const short NNN_WGS72S = 323; // Currently unsupported
  const short NNN_WGS84N = 326;
  const short NNN_WGS84S = 327;
  char uc_hem;
  int supportedUTM;
  int valid_Zone_and_Datum_and_Hemisphere;

  // Substitute WGS84 for ITRF97 per comment above
  if (datum == ITRF97_DATUM) {
    datum = WGS84_DATUM;
  }

  // Check for valid datum, hemisphere, and zone combination
  uc_hem = toupper(hem);
  valid_Zone_and_Datum_and_Hemisphere =
      (
      (datum == NAD27_DATUM && uc_hem == 'N' && zone >= 3 && zone <= 22) ||
      (datum == NAD83_DATUM && uc_hem == 'N' && zone >= 3 && zone <= 23) ||
      (datum == WGS84_DATUM                  && zone >= 1 && zone <= 60)
      ) ? 1 : 0;

  // Build the key for ProjectedCSTypeGeoKey, GCS_WGS84 etc
  if (valid_Zone_and_Datum_and_Hemisphere) {
    supportedUTM = 1;
    switch (datum) {
      case NAD27_DATUM:
        *pcs = (short)zone + NNN_NAD27N * 100;
        break;
      case NAD83_DATUM:
        *pcs = (short)zone + NNN_NAD83N * 100;
        break;
      case WGS84_DATUM:
        if (uc_hem == 'N') {
          *pcs = (short)zone + NNN_WGS84N * 100;
        }
        else {
          *pcs = (short)zone + NNN_WGS84S * 100;
        }
        break;
      default:
        supportedUTM = 0;
        *pcs = 0;
        break;
    }
  }
  else {
    supportedUTM = 0;
    *pcs = 0;
  }

  return supportedUTM;
}

void gcs_2_string (char *datum_str, short gcs)
{
  switch (gcs) {
    case GCS_NAD27:
      strcpy (datum_str, "NAD27");
      break;
    case GCS_NAD83:
      strcpy (datum_str, "NAD83");
      break;
    case GCS_WGS_72:
      strcpy (datum_str, "WGS 72");
      break;
    case GCS_WGS_84:
    case GCSE_WGS84:
      strcpy (datum_str, "WGS 84");
      break;
    case GCSE_Bessel1841:
      strcpy (datum_str, "Bessel ellipsoid-only");
      break;
    case GCSE_Clarke1866:
      strcpy (datum_str, "Clarke 1866 ellipsoid-only");
      break;
    case GCSE_GRS1980:
      strcpy (datum_str, "GRS 1980 ellipsoid-only");
      break;
    case GCSE_International1924:
      strcpy (datum_str, "International 1924 ellipsoid-only");
      break;
    case GCSE_International1967:
      strcpy (datum_str, "International 1967 ellipsoid-only");
      break;
    case GCSE_GEM10C:
      strcpy (datum_str, "GEM 10C ellipsoid-only");
      break;
    case GCSE_Clarke1880:
      strcpy (datum_str, "Clarke 1880 ellipsoid-only");
      break;
    default:
      strcpy (datum_str, "UNKNOWN or UNSUPPORTED");
      break;
  }
}

void pcs_2_string (char *datum_str, short pcs) {
  const short NNN_NAD27N = 267;
  const short NNN_NAD83N = 269;
  //const short NNN_WGS72N = 322; // Currently unsupported
  //const short NNN_WGS72S = 323; // Currently unsupported
  const short NNN_WGS84N = 326;
  const short NNN_WGS84S = 327;
  short pcsNNN;

  pcsNNN = pcs/100;
  if (pcsNNN == NNN_NAD27N) {
    strcpy (datum_str, "NAD27");
  }
  else if (pcsNNN == NNN_NAD83N) {
    strcpy (datum_str, "NAD83");
  }
  else if (pcsNNN == NNN_WGS84N || pcsNNN == NNN_WGS84S) {
    strcpy (datum_str, "WGS 84");
  }
  else {
    strcpy (datum_str, "UNKNOWN or UNSUPPORTED");
  }
}

void datum_2_string (char *datum_str, datum_type_t datum)
{
  switch (datum)
  {
    case NAD27_DATUM:
      strcpy (datum_str, "NAD27");
      break;
    case NAD83_DATUM:
      strcpy (datum_str, "NAD83");
      break;
    case WGS84_DATUM:
      strcpy (datum_str, "WGS 84");
      break;
    case ITRF97_DATUM:
      strcpy(datum_str, "ITRF97 (WGS 84)");
      break;
    default:
      strcpy (datum_str, "UNKNOWN or UNSUPPORTED");
      break;
  }
}

void write_datum_key (GTIF *ogtif, datum_type_t datum,
                      double re_major, double re_minor)
{
  //
  // NOTE: There is no GCS or GCSE value available for ITRF97,
  // but ITRF97 is based on WGS84.  Using WGS84 rather than ITRF97
  // results in errors of less than one meter, so we choose to
  // use WGS84 whenever ITRF97 is specified (except in citation
  // strings)
  //

  // If datum is recognized, then write a GCS_ datum to GeographicTypeGeoKey
  switch (datum) {
    case ED50_DATUM:
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_ED50);
      break;
    case NAD27_DATUM:
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_NAD27);
      break;
    case NAD83_DATUM:
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_NAD83);
      break;
    case WGS72_DATUM:
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_72);
      break;
    case ITRF97_DATUM:
    case WGS84_DATUM:
      GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCS_WGS_84);
      break;
    case EGM96_DATUM:
    case ETRF89_DATUM:
    case ETRS89_DATUM:
    default:
    {
        // Else fit an ellipsoid to the major/minor axis and write an
        // ellipsoid-only datum to the GeographicTypeGeoKey instead
        // (according to the GeoTIFF standard).  This allows the GeoTIFF
        // to be used, but attempts to minimize the error when utilizing
        // the stored datum.  If we ever allow user-defined coordinate systems,
        // including datum, then that case should be handled in the default:
        // case below.
      spheroid_type_t spheroid = axis2spheroid (re_major, re_minor);
      switch (spheroid) {
        case BESSEL_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_Bessel1841);
          break;
        case CLARKE1866_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_Clarke1866);
          break;
        case CLARKE1880_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_Clarke1880);
          break;
        case GEM10C_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_GEM10C);
          break;
        case GRS1980_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_GRS1980);
          break;
        case INTERNATIONAL1924_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_International1924);
          break;
        case INTERNATIONAL1967_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_International1967);
          break;
        case WGS84_SPHEROID:
          GTIFKeySet (ogtif, GeographicTypeGeoKey, TYPE_SHORT, 1, GCSE_WGS84);
          break;
        default:
            // Don't write anything into GeographicTypeGeoKey (including 'user-defined'
            // since writing 'user-defined' into it would imply that several other keys
            // are written to complete the definition, e.g. GeogGeodeticDatumGeoKey etc.
            // It's better to leave GeographicTypeGeoKey empty)
          asfPrintWarning ("Unsupported or unrecognized datum\n");
          break;
      }
    }
    break;
  }
}
