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
#include <asf_export.h>
#include <float_image.h>
#include <spheroids.h>
#include <typlim.h>

#define ASF_NAME_STRING "asf_export"
#define PGM_MAGIC_NUMBER "P5"

/* This constant is from the GeoTIFF spec.  It basically means that
   the system which would normally be specified by the field
   (projected coordinate system, datum, ellipsoid, whatever), in
   instead going to be specified by more detailed low level tags.  */
static const int user_defined_value_code = 32767;

int is_slant_range(meta_parameters *md);
void initialize_tiff_file (TIFF **otif, GTIF **ogtif,
                           const char *output_file_name,
                           const char *metadata_file_name,
                           int is_geotiff, scale_t sample_mapping, int rgb);
GTIF* write_tags_for_geotiff (TIFF *otif, const char *metadata_file_name);
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
    *ogtif = write_tags_for_geotiff (*otif, metadata_file_name);
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

GTIF* write_tags_for_geotiff (TIFF *otif, const char *metadata_file_name)
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

void finalize_ppm_file(FILE *oppm)
{
  FCLOSE(oppm);
}

void
export_band_image (const char *metadata_file_name,
		   const char *image_data_file_name,
		   char *output_file_name,
		   scale_t sample_mapping,
		   char **band_name, int rgb,
		   char *look_up_table_name,
		   output_format_t format)
{
  int map_projected;
  int is_geotiff = 1;
  TIFF *otif = NULL; // FILE* pointer for TIFF files
  GTIF *ogtif = NULL;
  FILE *ojpeg = NULL, *opgm=NULL;
  struct jpeg_compress_struct cinfo;
  ssize_t ii;
  int free_band_names=FALSE;
  int have_look_up_table = look_up_table_name && strlen(look_up_table_name)>0;

  meta_parameters *md = meta_read (metadata_file_name);
  map_projected = is_map_projected(md);

  asfRequire( !(sample_mapping == TRUNCATE &&
                (md->general->image_data_type == SIGMA_IMAGE ||
                 md->general->image_data_type == BETA_IMAGE  ||
                 md->general->image_data_type == GAMMA_IMAGE)
               ),
              "Remapping a power (sigma, beta, or gamma) type image into\n"
              "a byte image using truncation is not supported.  All values\n"
              "would map to black...\n");

  if (rgb && !have_look_up_table) {

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

    int ignored[3] = {0, 0, 0};
    int red_channel, green_channel, blue_channel;
    for (ii=0; ii<3; ii++) {
      ignored[ii] = strncmp("IGNORE", uc(band_name[ii]), 6) == 0 ? 1 : 0;
    }

    channel_stats_t red_stats, blue_stats, green_stats;
    red_stats.hist = NULL; red_stats.hist_pdf = NULL;
    green_stats.hist = NULL; green_stats.hist_pdf = NULL;
    blue_stats.hist = NULL; blue_stats.hist_pdf = NULL;

    if (!md->optical) {
      // Red channel statistics
      if (sample_mapping != NONE && !ignored[0]) { // byte image
	asfRequire (sizeof(unsigned char) == 1,
		    "Size of the unsigned char data type on this machine is "
		    "different than expected.\n");
	asfPrintStatus("Gathering red channel statistics ...\n");
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
	  red_stats.hist_pdf = gsl_histogram_pdf_alloc (256); //NUM_HIST_BINS);
	  gsl_histogram_pdf_init (red_stats.hist_pdf, red_stats.hist);
	}
      }

      // Green channel statistics
      if (sample_mapping != NONE && !ignored[1]) { // byte image
	asfRequire (sizeof(unsigned char) == 1,
		    "Size of the unsigned char data type on this machine is "
		    "different than expected.\n");
	asfPrintStatus("Gathering green channel statistics ...\n");
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
	  green_stats.hist_pdf = gsl_histogram_pdf_alloc (256); //NUM_HIST_BINS);
	  gsl_histogram_pdf_init (green_stats.hist_pdf, green_stats.hist);
	}
      }

      // Blue channel statistics
      if (sample_mapping != NONE && !ignored[2]) { // byte image
	asfRequire (sizeof(unsigned char) == 1,
		    "Size of the unsigned char data type on this machine is "
		    "different than expected.\n");
	asfPrintStatus("Gathering blue channel statistics ...\n");
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
	  blue_stats.hist_pdf = gsl_histogram_pdf_alloc (256); //NUM_HIST_BINS);
	  gsl_histogram_pdf_init (blue_stats.hist_pdf, blue_stats.hist);
	}
      }
    }

    // Get channel numbers
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

    // Write the data to the file
    FILE *fp;
    float *red_float_line, *green_float_line, *blue_float_line;
    unsigned char *red_byte_line, *green_byte_line, *blue_byte_line;

    fp = FOPEN(image_data_file_name, "rb");

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
      if (ignored[0])
        red_float_line = (float *) CALLOC(sample_count, sizeof(float));
      else
        red_float_line = (float *) MALLOC(sample_count * sizeof(float));

      if (ignored[1])
        green_float_line = (float *) CALLOC(sample_count, sizeof(float));
      else
        green_float_line = (float *) MALLOC(sample_count * sizeof(float));

      if (ignored[2])
        blue_float_line = (float *) CALLOC(sample_count, sizeof(float));
      else
        blue_float_line = (float *) MALLOC(sample_count * sizeof(float));
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
        if (format == TIF || format == GEOTIFF)
          write_rgb_tiff_byte2byte(otif, red_byte_line, green_byte_line,
                                   blue_byte_line, ii, sample_count);
        else if (format == JPEG)
          write_rgb_jpeg_byte2byte(ojpeg, red_byte_line, green_byte_line,
                                   blue_byte_line, &cinfo, sample_count);
      }
      else if (sample_mapping == NONE) {
        // Write float lines if float image
        if (!ignored[0])
          get_float_line(fp, md, ii+red_channel*offset, red_float_line);
        if (!ignored[1])
          get_float_line(fp, md, ii+green_channel*offset, green_float_line);
        if (!ignored[2])
          get_float_line(fp, md, ii+blue_channel*offset, blue_float_line);
        if (format == GEOTIFF)
          write_rgb_tiff_float2float(otif, red_float_line, green_float_line,
                                     blue_float_line, ii, sample_count);
      }
      else {
        // Write float lines if byte image
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

    if (red_stats.hist) gsl_histogram_free(red_stats.hist);
    if (red_stats.hist_pdf) gsl_histogram_pdf_free(red_stats.hist_pdf);
    if (green_stats.hist) gsl_histogram_free(green_stats.hist);
    if (green_stats.hist_pdf) gsl_histogram_pdf_free(green_stats.hist_pdf);
    if (blue_stats.hist) gsl_histogram_free(blue_stats.hist);
    if (blue_stats.hist_pdf) gsl_histogram_pdf_free(blue_stats.hist_pdf);
  }
  else { // Single-band image output (one grayscale file for each available band)

    int band_count = md->general->band_count;
    char base_name[255], bands[25];
    strcpy(bands, md->general->bands);
    strcpy(base_name, output_file_name);

    if (!band_name) {
        // allow passing in NULL for the band names to mean "who cares!"
        // when exporting a single band image (mostly useful with a
        // look up table).
        band_name = (char **) MALLOC(sizeof(char*)*1);
        band_name[0] = (char*) MALLOC(sizeof(char)*4);
        strcpy(band_name[0], "???");
        free_band_names = TRUE;
        band_count = 1;
    }

    int kk;
    for (kk=0; kk<band_count; kk++) {
      if (band_name[kk]) {

	if (strcmp(band_name[0], "???") != 0)
	  asfPrintStatus("Writing band '%s' ...\n", band_name[kk]);

	// Initialize the chosen format
	if (strcmp(band_name[0], "???") != 0)
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
	else if (format == PGM) {
	  append_ext_if_needed (output_file_name, ".pgm", ".pgm");
	  initialize_pgm_file(output_file_name, md, &opgm);
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
        stats.hist = NULL; stats.hist_pdf = NULL;

	if (!md->optical || sample_mapping != NONE) {
	  asfRequire (sizeof(unsigned char) == 1,
		      "Size of the unsigned char data type on this machine is "
		      "different than expected.\n");
	  asfPrintStatus("Gathering statistics ...\n");
	  calc_stats_from_file(image_data_file_name, band_name[0],
                               md->general->no_data,
			       &stats.min, &stats.max, &stats.mean,
			       &stats.standard_deviation, &stats.hist);
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
	  if (sample_mapping == SIGMA) {
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

	FILE *fp;
	float *float_line;
	unsigned char *byte_line;

	// Write the output image
	fp = FOPEN(image_data_file_name, "rb");
	float_line = (float *) MALLOC(sizeof(float) * sample_count);
	byte_line =
	  (unsigned char *) MALLOC(sizeof(unsigned char) * sample_count);

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
	       else if (format == PGM)
                 write_pgm_byte2byte(opgm, byte_line, stats, sample_mapping,
                                    sample_count);
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
               else if (format == PGM)
                 write_pgm_byte2byte(opgm, byte_line, stats, NONE,
                                     sample_count);
             }
	    }
	    else if (sample_mapping == NONE) {
	      get_float_line(fp, md, ii+channel*offset, float_line);
	      if (format == GEOTIFF)
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
				      sample_count);
	      else if (format == PGM)
		write_pgm_float2byte(opgm, float_line, stats, sample_mapping,
				     md->general->no_data, sample_count);
	    }
	    asfLineMeter(ii, md->general->line_count);
	  }
	}

	// Free memory
	if (float_line) FREE(float_line);
	if (byte_line) FREE(byte_line);
        if (stats.hist) gsl_histogram_free(stats.hist);
        if (stats.hist_pdf) gsl_histogram_pdf_free(stats.hist_pdf);

	// Finalize the chosen format
	if (format == TIF || format == GEOTIFF)
	  finalize_tiff_file(otif, ogtif, is_geotiff);
	else if (format == JPEG)
	  finalize_jpeg_file(ojpeg, &cinfo);
	else if (format == PGM)
	  finalize_ppm_file(opgm);
      }
    }
  }

  if (free_band_names) {
      FREE(band_name[0]); FREE(band_name);
  }

  meta_free (md);
}

