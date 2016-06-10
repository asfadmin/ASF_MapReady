// Import a generic GeoTIFF (a projected GeoTIFF flavor), including
// projection data from its metadata file (ERDAS MIF HFA .aux file) into
// our own ASF Tools format (.img, .meta)
//
// NOTE:
// 1. At this time, only supports Albers Equal Area Conic, Lambert Azimuthal
//    Equal Area, Lambert Conformal Conic, Polar Stereographic, and UTM
// 2. There may be some data duplication between the GeoTIFF tag contents
//    in the TIFF file and the data contents of the metadata (.aux) file.
// 3. Data and parameters found in the metadata (.aux) file supercede the
//    data and parameter values found in the TIFF file
//

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>

#include "float_image.h"
#include "asf_tiff.h"

#include <gsl/gsl_math.h>

#include <uint8_image.h>
#include <spheroids.h>
#include <proj.h>
#include <libasf_proj.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "asf_raster.h"
#include "asf_tiff.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"

#include "projected_image_import.h"
#include "tiff_to_float_image.h"
#include "tiff_to_byte_image.h"
#include "write_meta_and_img.h"
#include "import_generic_geotiff.h"
#include "arcgis_geotiff_support.h"
#include "geotiff_support.h"

#define BAD_VALUE_SCAN_ON

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define IMPORT_GENERIC_FLOAT_MICRON 0.000000001
#ifdef  FLOAT_EQUIVALENT
#undef  FLOAT_EQUIVALENT
#endif
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, IMPORT_GENERIC_FLOAT_MICRON))
#define FLOAT_TOLERANCE 0.00001

#define DEFAULT_UTM_SCALE_FACTOR     0.9996

#define DEFAULT_SCALE_FACTOR         1.0
#define UNKNOWN_PROJECTION_TYPE       -1

#define NAD27_DATUM_STR   "NAD27"
#define NAD83_DATUM_STR   "NAD83"
#define HARN_DATUM_STR    "HARN"
#define WGS84_DATUM_STR   "WGS84"
#define HUGHES_DATUM_STR  "HUGHES"

#define USER_DEFINED_PCS             32767
#define USER_DEFINED_KEY             32767
#define BAND_NAME_LENGTH  12

#ifdef USHORT_MAX
#undef USHORT_MAX
#endif
#define USHORT_MAX  65535

#ifdef MAX_RGB
#undef MAX_RGB
#endif
#define MAX_RGB  255

// Do not change the BAND_ID_STRING.  It will break ingest of legacy TIFFs exported with this
// string in their citation strings.  If you are changing the citation string to have some _other_
// identifying string, then use a _new_ definition rather than replace what is in this one.
// Then, make sure that your code supports both the legacy string and the new string.
// Also see the export library string definitions (which MUST match these here) and
// for associated code that needs to reflect the changes you are making here.
#define BAND_ID_STRING "Color Channel (Band) Contents in RGBA+ order"

spheroid_type_t SpheroidName2spheroid(char *sphereName);
void check_projection_parameters(meta_projection *mp);
int  band_float_image_write(FloatImage *oim, meta_parameters *meta_out,
                            const char *outBaseName, int num_bands, int *ignore);
int  band_byte_image_write(UInt8Image *oim_b, meta_parameters *meta_out,
                           const char *outBaseName, int num_bands, int *ignore);
int check_for_vintage_asf_utm_geotiff(const char *citation, int *geotiff_data_exists,
                                      short *model_type, short *raster_type, short *linear_units);
int check_for_datum_in_string(const char *citation, datum_type_t *datum);
int check_for_ellipse_definition_in_geotiff(GTIF *input_gtif, spheroid_type_t *spheroid);
int vintage_utm_citation_to_pcs(const char *citation, int *zone, char *hem, datum_type_t *datum, short *pcs);
static int UTM_2_PCS(short *pcs, datum_type_t datum, unsigned long zone, char hem);
void classify_geotiff(GTIF *input_gtif, short *model_type, short *raster_type, short *linear_units, short *angular_units,
                      int *geographic_geotiff, int *geocentric_geotiff, int *map_projected_geotiff,
                      int *geotiff_data_exists);
char *angular_units_to_string(short angular_units);
char *linear_units_to_string(short linear_units);
void get_look_up_table_name(char *citation, char **look_up_table);

// Import an ERDAS ArcGIS GeoTIFF (a projected GeoTIFF flavor), including
// projection data from its metadata file (ERDAS MIF HFA .aux file) into
// our own ASF Tools format (.img, .meta)
//
void import_generic_geotiff (const char *inFileName, const char *outBaseName, ...)
{
  TIFF *input_tiff;
  meta_parameters *meta;
  data_type_t data_type;
  int is_scanline_format;
  int is_palette_color_tiff;
  short num_bands;
  short int bits_per_sample, sample_format, planar_config;
  va_list ap;
  char image_data_type[256];
  char *pTmpChar;
  int ignore[MAX_BANDS]; // Array of band flags ...'1' if a band is to be ignored (empty band)
                         // Do NOT allocate less than MAX_BANDS bands since other tools will
                         // receive the 'ignore' array and may assume there are MAX_BANDS in
                         // the array, e.g. read_tiff_meta() in the asf_view tool.

  _XTIFFInitialize();

  // Open the input tiff file.
  //TIFFErrorHandler oldHandler = TIFFSetWarningHandler(NULL);
  input_tiff = XTIFFOpen (inFileName, "r");
  if (input_tiff == NULL)
    asfPrintError ("Error opening input TIFF file:\n    %s\n", inFileName);

  if (get_tiff_data_config(input_tiff,
                           &sample_format,    // TIFF type (uint == 1, int == 2, float == 3)
                           &bits_per_sample,  // 8, 16, or 32
                           &planar_config,    // Contiguous == 1 (RGB or RGBA) or separate == 2 (separate bands)
                           &data_type,        // ASF datatype, (BYTE, INTEGER16, INTEGER32, or REAL32 ...no complex)
                           &num_bands,        // Initial number of bands
                           &is_scanline_format,
                           &is_palette_color_tiff,
                           REPORT_LEVEL_WARNING))
  {
    // Failed to determine tiff info or tiff info was bad
    char msg[1024];
    tiff_type_t t;
    get_tiff_type(input_tiff, &t);
    sprintf(msg, "FOUND TIFF tag data as follows:\n"
        "         Sample Format: %s\n"
        "       Bits per Sample: %d\n"
        "  Planar Configuration: %s\n"
        "       Number of Bands: %d\n"
        "                Format: %s\n"
        "              Colormap: %s\n",
        (sample_format == SAMPLEFORMAT_UINT) ? "Unsigned Integer" :
        (sample_format == SAMPLEFORMAT_INT) ? "Signed Integer" :
        (sample_format == SAMPLEFORMAT_IEEEFP) ? "Floating Point" : "Unknown or Unsupported",
        bits_per_sample,
        (planar_config == PLANARCONFIG_CONTIG) ? "Contiguous (chunky RGB or RGBA etc) / Interlaced" :
          (planar_config == PLANARCONFIG_SEPARATE) ? "Separate planes (band-sequential)" :
            "Unknown or unrecognized",
        num_bands,
        t.format == SCANLINE_TIFF ? "SCANLINE TIFF" :
        t.format == STRIP_TIFF    ? "STRIP TIFF"    :
        t.format == TILED_TIFF    ? "TILED TIFF"    : "UNKNOWN",
        is_palette_color_tiff ? "PRESENT" : "NOT PRESENT");
    switch (t.format) {
      case STRIP_TIFF:
        sprintf(msg, "%s"
                "        Rows per Strip: %d\n",
                msg, t.rowsPerStrip);
        break;
      case TILED_TIFF:
        sprintf(msg, "%s"
                "            Tile Width: %d\n"
                "           Tile Height: %d\n",
                msg, t.tileWidth, t.tileLength);
        break;
      case SCANLINE_TIFF:
      default:
        break;
    }
    asfPrintWarning(msg);

    XTIFFClose(input_tiff);
    asfPrintError("  Unsupported TIFF type found or required TIFF tags are missing\n"
        "    in TIFF File \"%s\"\n\n"
        "  TIFFs must contain the following:\n"
        "        Sample format: Unsigned or signed integer or IEEE floating point data\n"
        "                       (ASF is not yet supporting TIFF files with complex number type data),\n"
        "        Planar config: Contiguous (Greyscale, RGB, or RGBA) or separate planes (band-sequential.)\n"
        "      Bits per sample: 8, 16, or 32\n"
        "      Number of bands: 1 through %d bands allowed.\n"
        "               Format: Scanline, strip, or tiled\n"
        "             Colormap: Present or not present (only valid for 1-band images)\n",
        inFileName, MAX_BANDS);
  }
  XTIFFClose(input_tiff);

  // Get the image data type from the variable arguments list
  va_start(ap, outBaseName);
  pTmpChar = (char *)va_arg(ap, char *);
  va_end(ap);

  // Read the metadata (map-projection data etc) from the TIFF
  asfPrintStatus("\nImporting TIFF/GeoTIFF image to ASF Internal format...\n\n");
  int i;
  for (i=0; i<MAX_BANDS; i++) ignore[i]=0; // Default to ignoring no bands
  if (pTmpChar != NULL) {
    strcpy(image_data_type, pTmpChar);
    meta = read_generic_geotiff_metadata(inFileName, ignore, image_data_type);
  }
  else {
    meta = read_generic_geotiff_metadata(inFileName, ignore, NULL);
  }

  // Write the Metadata file
  meta_write(meta, outBaseName);

  // Write the binary file
  input_tiff = XTIFFOpen (inFileName, "r");
  if (input_tiff == NULL)
    asfPrintError ("Error opening input TIFF file:\n    %s\n", inFileName);
  if (geotiff_band_image_write(input_tiff, meta, outBaseName, num_bands, ignore,
                               bits_per_sample, sample_format, planar_config))
  {
    XTIFFClose(input_tiff);
    meta_free(meta);
    meta=NULL;
    asfPrintError("Unable to write binary image...\n    %s\n", outBaseName);
  }
  XTIFFClose(input_tiff);
  if (meta) meta_free(meta);
}

meta_parameters * read_generic_geotiff_metadata(const char *inFileName, int *ignore, ...)
{
  int geotiff_data_exists;
  short num_bands;
  char *bands[MAX_BANDS]; // list of band IDs
  int band_num = 0;
  int count;
  int read_count;
  int ret;
  short model_type=-1;
  short raster_type=-1;
  short linear_units=-1;
  short angular_units=-1;
  double scale_factor;
  TIFF *input_tiff;
  GTIF *input_gtif;
  meta_parameters *meta_out; // Return value
  data_type_t data_type;
  datum_type_t datum;
  va_list ap;

  _XTIFFInitialize();

  /***** INITIALIZE PARAMETERS *****/
  /*                               */
  // Create a new metadata object for the image.
  meta_out = raw_init ();
  meta_out->optical = NULL;
  meta_out->thermal = NULL;
  meta_out->projection = meta_projection_init ();
  meta_out->stats = NULL; //meta_stats_init ();
  meta_out->state_vectors = NULL;
  meta_out->location = meta_location_init ();
  meta_out->colormap = NULL; // Updated below if palette color TIFF
  // Don't set any of the deprecated structure elements.
  meta_out->stVec = NULL;
  meta_out->geo = NULL;
  meta_out->ifm = NULL;
  meta_out->info = NULL;
  datum = UNKNOWN_DATUM;

  // Set up convenience pointers
  meta_general *mg = meta_out->general;
  meta_projection *mp = meta_out->projection;
  meta_location *ml = meta_out->location;
  // FIXME:
  // The following function works perfectly fine when called from asf_view.
  // However, over here it resulted in a segmentation fault. Did not get the
  // time to find a solution before the release. - RG
  //meta_out->insar = populate_insar_metadata(inFileName);

  // Init
  mp->spheroid = UNKNOWN_SPHEROID; // meta_projection_init() 'should' initialize this, but doesn't

  // Open the input tiff file.
  input_tiff = XTIFFOpen (inFileName, "r");
  if (input_tiff == NULL) {
    asfPrintError("Error opening input TIFF file:\n  %s\n", inFileName);
  }

  // Open the structure that contains the geotiff keys.
  input_gtif = GTIFNew (input_tiff);
  if (input_gtif == NULL) {
    asfPrintError("Error reading GeoTIFF keys from input TIFF file:\n  %s\n", inFileName);
  }


  /***** GET WHAT WE CAN FROM THE TIFF FILE *****/
  /*                                            */
  // Malloc the band names and give them numeric names as defaults
  // (Updated from citation string later ...if the info exists)
  for (band_num = 0; band_num < MAX_BANDS; band_num++) {
    bands[band_num] = MALLOC(sizeof(char) * (BAND_NAME_LENGTH+1));
    sprintf (bands[band_num], "%02d", band_num+1);
  }

  // The data type returned is an ASF data type, e.g. BYTE or REAL32 etc
  //
  // The number of bands returned is based on the samples (values) per
  // pixel stored in the TIFF tags ...but some bands may be blank or ignored.
  // Later on, band-by-band statistics will determine if individual bands should
  // be ignored (not imported to an img file) and if the citation string contains
  // a band ID list, then any band marked with the word 'Empty' will also be ignored.
  // the number of bands will be updated to reflect these findings.
  //
  short sample_format;    // TIFFTAG_SAMPLEFORMAT
  short bits_per_sample;  // TIFFTAG_BITSPERSAMPLE
  short planar_config;    // TIFFTAG_PLANARCONFIG
  int is_scanline_format; // False if tiled or strips > 1 TIFF file format
  int is_palette_color_tiff;
  ret = get_tiff_data_config(input_tiff,
                             &sample_format, // TIFF type (uint, int, float)
                             &bits_per_sample, // 8, 16, or 32
                             &planar_config, // Contiguous (RGB or RGBA) or separate (band sequential, not interlaced)
                             &data_type, // ASF datatype, (BYTE, INTEGER16, INTEGER32, or REAL32 ...no complex
                             &num_bands, // Initial number of bands
                             &is_scanline_format,
                             &is_palette_color_tiff,
                             REPORT_LEVEL_WARNING);

  if (ret != 0) {
    char msg[1024];
    tiff_type_t t;
    get_tiff_type(input_tiff, &t);
    sprintf(msg, "FOUND TIFF tag data as follows:\n"
        "         Sample Format: %s\n"
        "       Bits per Sample: %d\n"
        "  Planar Configuration: %s\n"
        "       Number of Bands: %d\n"
        "                Format: %s\n",
        (sample_format == SAMPLEFORMAT_UINT) ? "Unsigned Integer" :
          (sample_format == SAMPLEFORMAT_INT) ? "Signed Integer" :
            (sample_format == SAMPLEFORMAT_IEEEFP) ? "Floating Point" : "Unknown or Unsupported",
        bits_per_sample,
        (planar_config == PLANARCONFIG_CONTIG) ? "Contiguous (chunky RGB or RGBA etc) / Interlaced" :
          (planar_config == PLANARCONFIG_SEPARATE) ? "Separate planes (band-sequential)" :
            "Unknown or unrecognized",
        num_bands,
        t.format == SCANLINE_TIFF ? "SCANLINE TIFF" :
        t.format == STRIP_TIFF ? "STRIP TIFF" :
        t.format == TILED_TIFF ? "TILED TIFF" : "UNKNOWN");
    switch (t.format) {
      case STRIP_TIFF:
        sprintf(msg, "%s"
                "        Rows per Strip: %d\n",
                msg, t.rowsPerStrip);
        break;
      case TILED_TIFF:
        sprintf(msg, "%s"
                "            Tile Width: %d\n"
                "           Tile Height: %d\n",
                msg, t.tileWidth, t.tileLength);
        break;
      case SCANLINE_TIFF:
      default:
        break;
    }
    asfPrintWarning(msg);

    asfPrintError("  Unsupported TIFF type found or required TIFF tags are missing\n"
        "    in TIFF File \"%s\"\n\n"
        "  TIFFs must contain the following:\n"
        "        Sample format: Unsigned or signed integer or IEEE floating point data\n"
        "                       (ASF is not yet supporting TIFF files with complex number type data),\n"
        "        Planar config: Contiguous (Greyscale, RGB, or RGBA) or separate planes (band-sequential.)\n"
        "      Bits per sample: 8, 16, or 32\n"
        "      Number of bands: 1 through %d bands allowed.\n"
        "               Format: Scanline, strip, or tiled\n",
        inFileName, MAX_BANDS);
  }
  asfPrintStatus("\n   Found %d-banded Generic GeoTIFF with %d-bit %s type data\n"
      "        (Note: Empty or missing bands will be ignored)\n",
                 num_bands, bits_per_sample,
      (sample_format == SAMPLEFORMAT_UINT)   ? "Unsigned Integer" :
      (sample_format == SAMPLEFORMAT_INT)    ? "Signed Integer"   :
      (sample_format == SAMPLEFORMAT_IEEEFP) ? "Floating Point"   : "Unknown or Unsupported");

  char *citation = NULL;
  int citation_length;
  int typeSize;
  tagtype_t citation_type;
  citation_length = GTIFKeyInfo(input_gtif, GTCitationGeoKey, &typeSize, &citation_type);
  if (citation_length > 0) {
    citation = MALLOC ((citation_length) * typeSize);
    GTIFKeyGet (input_gtif, GTCitationGeoKey, citation, 0, citation_length);
    asfPrintStatus("\nCitation: %s\n\n", citation);
  }
  else {
    citation_length = GTIFKeyInfo(input_gtif, PCSCitationGeoKey, &typeSize, &citation_type);
    if (citation_length > 0) {
      citation = MALLOC ((citation_length) * typeSize);
      GTIFKeyGet (input_gtif, PCSCitationGeoKey, citation, 0, citation_length);
      asfPrintStatus("\nCitation: %s\n\n", citation);
    }
    else {
      asfPrintStatus("\nCitation: The GeoTIFF citation string is MISSING (Not req'd)\n\n");
    }
  }

  // If this is a single-band TIFF with an embedded RGB colormap, then
  // grab it for the metadata and write it out as an ASF LUT file
  if (is_palette_color_tiff) {
      // Produce metadata
      char *look_up_table = NULL;
      unsigned short *red = NULL;
      unsigned short *green = NULL;
      unsigned short *blue = NULL;
      int i;
      int map_size = 1<<bits_per_sample;

      asfRequire(map_size > 0 && map_size <= 256, "Invalid colormap size\n");

      asfPrintStatus("\nFound single-band TIFF with embedded RGB colormap\n\n");
      meta_colormap *mc = meta_out->colormap = meta_colormap_init();

      get_look_up_table_name(citation, &look_up_table);
      strcpy(mc->look_up_table, look_up_table ? look_up_table : MAGIC_UNSET_STRING);
      FREE(look_up_table);

      read_count = TIFFGetField(input_tiff, TIFFTAG_COLORMAP, &red, &green, &blue);
      if (!read_count) {
          asfPrintWarning("TIFF appears to be a palette-color TIFF, but the embedded\n"
                  "color map (TIFFTAG_COLORMAP) appears to be missing.  Ingest\n"
                  "will continue, but as a non-RGB single-band greyscale image.\n");
          FREE(mc->look_up_table);
          FREE(mc);
      }
      else {
          // Populate the RGB colormap
	char band_str[255];
	strcpy(band_str, bands[0]);
	for (i=1; i<num_bands; i++)
	  sprintf(band_str, ",%s", bands[i]);
	strcpy(mc->band_id, band_str);
          mc->num_elements = map_size;
          mc->rgb = (meta_rgb *)CALLOC(map_size, sizeof(meta_rgb));
          for (i=0; i<map_size; i++) {
              mc->rgb[i].red   = (unsigned char)((red[i]/(float)USHORT_MAX)*(float)MAX_RGB);
              mc->rgb[i].green = (unsigned char)((green[i]/(float)USHORT_MAX)*(float)MAX_RGB);
              mc->rgb[i].blue  = (unsigned char)((blue[i]/(float)USHORT_MAX)*(float)MAX_RGB);
          }
      }
      // NOTE: Do NOT free the red/green/blue arrays ...this will result in a
      // glib double-free error when the TIFF file is closed.

      // Now that we have good metadata, produce the LUT
      char *lut_file = appendExt(inFileName, ".lut");
      asfPrintStatus("\nSTORING TIFF file embedded color map in look up table file:\n    %s\n", lut_file);
      FILE *lutFP = (FILE *)FOPEN(lut_file, "wt");
      fprintf(lutFP, "# Look up table type: %s\n", mc->look_up_table);
      fprintf(lutFP, "# Originating source: %s\n", inFileName);
      fprintf(lutFP, "# Index   Red   Green   Blue\n");
      for (i=0; i<map_size; i++) {
          fprintf(lutFP, "%03d    %03d    %03d    %03d\n",
                  i, mc->rgb[i].red, mc->rgb[i].green, mc->rgb[i].blue);
      }
      fprintf(lutFP, "\n");
      FCLOSE(lutFP);
      FREE(lut_file);
  }

  // Get the tie point which defines the mapping between raster
  // coordinate space and geographic coordinate space.  Although
  // geotiff theoretically supports multiple tie points, we don't
  // (rationale: ArcView currently doesn't either, and multiple tie
  // points don't make sense with the pixel scale option, which we
  // need).
  // NOTE: Since neither ERDAS or ESRI store tie points in the .aux
  // file associated with their geotiffs, it is _required_ that they
  // are found in their tiff files.
  double *tie_point = NULL;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_TIEPOINTS, &count,
                               &tie_point);
  if (count != 6) {
    asfPrintError ("GeoTIFF file does not contain tie points\n");
  }
  // Get the scale factors which define the scale relationship between
  // raster pixels and geographic coordinate space.
  double *pixel_scale = NULL;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_PIXELSCALE, &count,
                               &pixel_scale);
  if (count != 3) {
    asfPrintError ("GeoTIFF file does not contain pixel scale parameters\n");
  }
  if (pixel_scale[0] <= 0.0 || pixel_scale[1] <= 0.0) {
    asfPrintError ("GeoTIFF file contains invalid pixel scale parameters\n");
  }

  // CHECK TO SEE IF THE GEOTIFF CONTAINS USEFUL DATA:
  //  If the tiff file contains geocoded information, then the model type
  // will be ModelTypeProjected.
  // FIXME: Geographic (lat/long) type geotiffs with decimal degrees are
  // supported, but arc-sec are not yet ...

  int geographic_geotiff, map_projected_geotiff, geocentric_geotiff;
  classify_geotiff(input_gtif, &model_type, &raster_type, &linear_units, &angular_units,
                   &geographic_geotiff, &geocentric_geotiff, &map_projected_geotiff,
                   &geotiff_data_exists);
  asfPrintStatus ("Input GeoTIFF key GTModelTypeGeoKey is %s\n",
                  (model_type == ModelTypeGeographic) ? "ModelTypeGeographic" :
                  (model_type == ModelTypeGeocentric) ? "ModelTypeGeocentric" :
                  (model_type == ModelTypeProjected)  ? "ModelTypeProjected"  : "Unknown");
  asfPrintStatus ("Input GeoTIFF key GTRasterTypeGeoKey is %s\n",
                  (raster_type == RasterPixelIsArea)  ? "RasterPixelIsArea" : 
		  (raster_type == RasterPixelIsPoint) ? "RasterPixelIsPoint":
		  "(Unsupported type)");
  if (map_projected_geotiff) {
      asfPrintStatus ("Input GeoTIFF key ProjLinearUnitsGeoKey is %s\n",
                      (linear_units == Linear_Meter)                  ? "Linear_Meter"                  :
		      (linear_units == Linear_Foot)                   ? "Linear_Foot"                   :
		      (linear_units == Linear_Foot_US_Survey)         ? "Linear_Foot_US_Survey"         :
		      (linear_units == Linear_Foot_Modified_American) ? "Linear_Foot_Modified_American" :
		      (linear_units == Linear_Foot_Clarke)            ? "Linear_Foot_Clarke"            :
		      (linear_units == Linear_Foot_Indian)            ? "Linear_Foot_Indian"            :
                                                                        "(Unsupported type of linear units)");
  }
  else if (geographic_geotiff) {
      asfPrintStatus ("Input GeoTIFF key GeogAngularUnitsGeoKey is %s\n",
                      (angular_units == Angular_Arc_Second)  ? "Angular_Arc_Second" :
                      (angular_units == Angular_Degree)      ? "Angular_Degree"     :
                                                               "(Unsupported type of angular units)");
  }
  else {
      asfPrintError ("Cannot determine type of linear or angular units in GeoTIFF\n");
  }
  /***** READ PROJECTION PARAMETERS FROM TIFF IF GEO DATA EXISTS                 *****/
  /***** THEN READ THEM FROM THE METADATA (.AUX) FILE TO SUPERCEDE IF THEY EXIST *****/
  /*                                                                                 */
  /*                                                */
  // import_arcgis_geotiff() would not be called (see detect_geotiff_flavor())
  // unless the model_type is either unknown or is ModelTypeProjected.  If
  // ModelTypeProjected, then there are projection parameters inside the
  // GeoTIFF file.  If not, then they must be parsed from the complementary
  // ArcGIS metadata (.aux) file
  // Read the model type from the GeoTIFF file ...expecting that it is
  // unknown, but could possibly be ModelTypeProjection
  //
  // Start of reading projection parameters from geotiff
  if (model_type == ModelTypeProjected && geotiff_data_exists) {
    char hemisphere;
    projection_type_t projection_type=UNKNOWN_PROJECTION;
    unsigned long pro_zone; // UTM zone (UTM only)
    short proj_coords_trans = UNKNOWN_PROJECTION_TYPE;
    short pcs;
    short geokey_datum=0;
    double false_easting = MAGIC_UNSET_DOUBLE;
    double false_northing = MAGIC_UNSET_DOUBLE;
    double lonOrigin = MAGIC_UNSET_DOUBLE;
    double latOrigin = MAGIC_UNSET_DOUBLE;
    double stdParallel1 = MAGIC_UNSET_DOUBLE;
    double stdParallel2 = MAGIC_UNSET_DOUBLE;
    double lonPole = MAGIC_UNSET_DOUBLE;

    //////// ALL PROJECTIONS /////////
    // Set the projection block data that we know at this point
    if (tie_point[0] != 0 || tie_point[1] != 0 || tie_point[2] != 0) {
      // NOTE: To support tie points at other locations, or a set of other locations,
      // then things rapidly get more complex ...and a transformation must either be
      // derived or provided (and utilized etc).  We're not at that point yet...
      //
      asfPrintError("Unsupported initial tie point type.  Initial tie point must be for\n"
          "raster location (0,0) in the image.\n");
    }
    mp->startX = tie_point[3];
    mp->startY = tie_point[4];
    if (pixel_scale[0] < 0 ||
        pixel_scale[1] < 0) {
      asfPrintWarning("Unexpected negative pixel scale values found in GeoTIFF file.\n"
          "Continuing ingest, but defaulting perX to fabs(x pixel scale) and\n"
          "perY to (-1)*fabs(y pixel scale)... Results may vary.");
    }
    mp->perX = fabs(pixel_scale[0]);
    mp->perY = -(fabs(pixel_scale[1]));
    // Special treatment for RasterPixelIsPoint data
    // Those need a shift by half a pixel
    if (raster_type == RasterPixelIsPoint) {
      mp->startX -= mp->perX / 2.0;
      //mp->startY -= fabs(mp->perY) / 2.0;
      mp->startY -= mp->perY / 2.0;
    }
    mp->height = 0.0;
    if (linear_units == Linear_Meter) {
      strcpy(mp->units, "meters");
    }
    else if (linear_units == Linear_Foot ||
	     linear_units == Linear_Foot_US_Survey ||
	     linear_units == Linear_Foot_Modified_American ||
	     linear_units == Linear_Foot_Clarke ||
	     linear_units == Linear_Foot_Indian)
      strcpy(mp->units, "feet");
    else {
      asfPrintError("Unsupported linear unit found in map-projected GeoTIFF.  Only meters and feet are currently supported.\n");
    }

    ///////// STANDARD UTM (PCS CODE) //////////
    // Get datum and zone as appropriate
    read_count = GTIFKeyGet (input_gtif, ProjectedCSTypeGeoKey, &pcs, 0, 1);
    // Quick hack for Rick's State Plane data
    // Only supports State Plane 
    if (read_count && pcs >= 26931 && pcs <=26940) {
      mp->type = STATE_PLANE;
      projection_type = STATE_PLANE;
      proj_coords_trans = CT_TransverseMercator;
      datum = mp->datum = NAD83_DATUM;
      mp->spheroid = GRS1980_SPHEROID;
    }

    // Some GeoTIFF are entirely defined by their predefined project coordinate
    // system key - the use of gdalwarp for geocoding using an EPSG code does
    // that, for example
    if (read_count && pcs == 3413) {
      // WGS 84 / NSIDC Sea Ice Polar Stereographic North
      mp->type = POLAR_STEREOGRAPHIC;
      mp->hem = 'N';
      mp->param.ps.slat = 0.0;
      mp->param.ps.slon = 0.0;
      mp->param.ps.is_north_pole = TRUE;
      mp->param.ps.false_easting = 0.0;
      mp->param.ps.false_northing = 0.0;
      datum = mp->datum = WGS84_DATUM;
      mp->spheroid = WGS84_SPHEROID;
    }
    else if (read_count && pcs == 3031) {
      // WGS 84 / Antarctic Polar Stereographic
      mp->type = POLAR_STEREOGRAPHIC;
      mp->hem = 'S';
      mp->param.ps.slat = 0.0;
      mp->param.ps.slon = 0.0;
      mp->param.ps.is_north_pole = FALSE;
      mp->param.ps.false_easting = 0.0;
      mp->param.ps.false_northing = 0.0;
      datum = mp->datum = WGS84_DATUM;
      mp->spheroid = WGS84_SPHEROID;
    }

    if (!read_count) {
      // Check to see if this is a vintage ASF UTM geotiff (they only had the UTM
      // description in the UTM string rather than in the ProejctedCSTypeGeoKey)
      int sleepy;
      datum_type_t dopey;
      char sneezy;
      read_count = vintage_utm_citation_to_pcs(citation, &sleepy, &sneezy, &dopey, &pcs);
    }
    if (read_count == 1 && PCS_2_UTM(pcs, &hemisphere, &datum, &pro_zone)) {
      mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      mp->hem = hemisphere;
      mp->param.utm.zone = pro_zone;
      mp->param.utm.false_easting = 500000.0;
      if (hemisphere == 'N') {
        mp->param.utm.false_northing = 0.0;
      }
      else {
        mp->param.utm.false_northing = 10000000.0;
      }
      mp->param.utm.lat0 = 0.0;
      mp->param.utm.lon0 = utm_zone_to_central_meridian(pro_zone);
      if (datum != UNKNOWN_DATUM) {
        mp->datum = datum;
      }
      else {
        asfPrintError("Unsupported or unknown datum found in GeoTIFF file.\n");
      }
      char msg[256];
      sprintf(msg,"UTM scale factor defaulting to %0.4f\n", DEFAULT_UTM_SCALE_FACTOR);
      asfPrintStatus(msg);
      mp->param.utm.scale_factor = DEFAULT_UTM_SCALE_FACTOR;
    }
    ////////// ALL OTHER PROJECTION TYPES - INCLUDING GCS/USER-DEFINED UTMS /////////
    else if (projection_type != STATE_PLANE) { // Hack !!!!

      // Not recognized as a supported UTM PCS or was a user-defined or unknown type of PCS...
      //
      // The ProjCoordTransGeoKey will be true if the PCS was user-defined or if the PCS was
      // not in the geotiff file... or so the standard says.  If the ProjCoordTransGeoKey is
      // false, it means that an unsupported (by us) UTM or State Plane projection was
      // discovered (above.)  All other projection types make use of the ProjCoordTransGeoKey
      // geokey.

      ////// GCS-CODE DEFINED UTM ///////
      // Check for a user-defined UTM projection (A valid UTM code may be in
      // ProjectionGeoKey, although this is not typical)
      read_count = GTIFKeyGet (input_gtif, ProjectionGeoKey, &pcs, 0, 0);
      if (read_count == 1 && PCS_2_UTM(pcs, &hemisphere, &datum, &pro_zone)) {
        mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
        mp->hem = hemisphere;
        mp->param.utm.zone = pro_zone;
        mp->param.utm.false_easting = 500000.0;
        if (hemisphere == 'N') {
          mp->param.utm.false_northing = 0.0;
        }
        else {
          mp->param.utm.false_northing = 10000000.0;
        }
        mp->param.utm.lat0 = utm_zone_to_central_meridian(pro_zone);
        mp->param.utm.lon0 = 0.0;
        if (datum != UNKNOWN_DATUM) {
          mp->datum = datum;
        }
        else if (pcs/100 == 160 || pcs/100 == 161) {
            // With user-defined UTMs (16001-16060, 16101-16160 in ProjectionGeoKey
            // the zone and hemisphere is defined, but not the datum... We should try
            // to determine the datum as follows:
            //
            // Read GeographicTypeGeoKey:
            //
            // GeographicTypeGeoKey, If the code is recognized, assign as appropriate.
            //                       If the code is 32676 (user-defined ...which also often
            //                         means "undefined" rather than "user defined") then
            //                         check to see if the datum is specifically defined
            //                         elsewhere:
            //                         - Check GeogGeodeticDatumGeoKey
            //                         - Check PCSCitationGeoKey, GeogCitationGeoKey, and
            //                           GTCitationGeoKey to see if it is textually described
            //                         - Check to see if semi-major and inv. flattening (etc)
            //                           is defined and then do a best-fit to determine a
            //                           ellipsoid
            //                         - Default to WGS-84 (if GeographicTypeGeoKey is
            //                           160xx or 161xx format), else error out
            //
            //asfPrintError("Unsupported or unknown datum found in GeoTIFF file.\n");https://rt/Ticket/Display.html?id=7763
            short gcs;
            read_count = GTIFKeyGet (input_gtif, GeographicTypeGeoKey, &gcs, 0, 1);
            if (read_count == 1) {
                switch(geokey_datum){
                    case GCS_WGS_84:
                    case GCSE_WGS84:
                        datum = WGS84_DATUM;
                        break;
                    case GCS_NAD27:
                        datum = NAD27_DATUM;
                        break;
                    case GCS_NAD83:
                        datum = NAD83_DATUM;
                        break;
		    case GCS_ED50:
		      datum = ED50_DATUM;
		      break;
		    case GCS_SAD69:
		      datum = SAD69_DATUM;
		      break;
                    default:
                        datum = UNKNOWN_DATUM;
                        break;
                }
            }
            if (datum == UNKNOWN_DATUM) {
                // The datum is not typically stored in GeogGeodeticDatumGeoKey, but some s/w
                // does put it there
                read_count = GTIFKeyGet (input_gtif, GeogGeodeticDatumGeoKey, &geokey_datum, 0, 1);
                if (read_count == 1) {
                    switch(geokey_datum){
                        case Datum_WGS84:
                            datum = WGS84_DATUM;
                            break;
                        case Datum_North_American_Datum_1927:
                            datum = NAD27_DATUM;
                            break;
                        case Datum_North_American_Datum_1983:
                            datum = NAD83_DATUM;
                            break;
		        case 6655: // ITRF97
			  datum = ITRF97_DATUM;
			  break;
		        case 6054: // HUGHES80
			  datum = HUGHES_DATUM;
			  break;
                        default:
                            datum = UNKNOWN_DATUM;
                            break;
                    }
                }
            }
            if (datum == UNKNOWN_DATUM) {
                // Try citation strings to see if the datum was textually described
                char *citation = NULL;
                int citation_length;
                int typeSize;
                tagtype_t citation_type;
                citation_length = GTIFKeyInfo(input_gtif, GeogCitationGeoKey, &typeSize, &citation_type);
                if (citation_length > 0) {
                    citation = MALLOC ((citation_length) * typeSize);
                    GTIFKeyGet (input_gtif, GeogCitationGeoKey, citation, 0, citation_length);
                    check_for_datum_in_string(citation, &datum);
                    FREE(citation);
                }
                if (datum == UNKNOWN_DATUM) {
                    citation_length = GTIFKeyInfo(input_gtif, GTCitationGeoKey, &typeSize, &citation_type);
                    if (citation_length > 0) {
                        citation = MALLOC ((citation_length) * typeSize);
                        GTIFKeyGet (input_gtif, GTCitationGeoKey, citation, 0, citation_length);
                        check_for_datum_in_string(citation, &datum);
                        FREE(citation);
                    }
                }
                if (datum == UNKNOWN_DATUM) {
                    citation_length = GTIFKeyInfo(input_gtif, PCSCitationGeoKey, &typeSize, &citation_type);
                    if (citation_length > 0) {
                        citation = MALLOC ((citation_length) * typeSize);
                        GTIFKeyGet (input_gtif, PCSCitationGeoKey, citation, 0, citation_length);
                        check_for_datum_in_string(citation, &datum);
                        FREE(citation);
                    }
                }
                if (datum == UNKNOWN_DATUM) {
                    spheroid_type_t spheroid;
                    check_for_ellipse_definition_in_geotiff(input_gtif, &spheroid);
                    switch (spheroid) {
                        case BESSEL_SPHEROID:
                        case CLARKE1866_SPHEROID:
                        case CLARKE1880_SPHEROID:
                        case GEM6_SPHEROID:
                        case GEM10C_SPHEROID:
                        case GRS1980_SPHEROID:
                        case INTERNATIONAL1924_SPHEROID:
                        case INTERNATIONAL1967_SPHEROID:
                        case WGS72_SPHEROID:
                        case WGS84_SPHEROID:
                        case HUGHES_SPHEROID:
                        case UNKNOWN_SPHEROID:
                        default:
                            datum = UNKNOWN_DATUM;
                            break;
                    }
                }
                if (datum == UNKNOWN_DATUM) {
                    // If all else fails, make it a WGS-84 and spit out a warning
                    datum = WGS84_DATUM;
                    mp->datum = datum;
                    asfPrintWarning("Could not determine datum type from GeoTIFF, but since this\n"
                            "is a EPSG 160xx/161xx type UTM projection (WGS84 typ.),\n"
                            "a WGS84 datum type is assumed.\n");
                }
            }
        }
        char msg[256];
        sprintf(msg,"UTM scale factor defaulting to %0.4f\n", DEFAULT_UTM_SCALE_FACTOR);
        asfPrintStatus(msg);
        mp->param.utm.scale_factor = DEFAULT_UTM_SCALE_FACTOR;
      }
      /////// OTHER PROJECTION DEFINITIONS - INCLUDING USER-DEFINED UTMS///////
      else {
        // Some other type of projection may exist, including a projection-coordinate-transformation
        // UTM (although that is not typical)

        // Get the projection coordinate transformation key (identifies the projection type)
        read_count = GTIFKeyGet (input_gtif, ProjCoordTransGeoKey, &proj_coords_trans, 0, 1);
        if (read_count != 1 || proj_coords_trans == UNKNOWN_PROJECTION_TYPE) {
          asfPrintWarning("Unable to determine type of projection coordinate system in GeoTIFF file\n");
        }

        // Attempt to find a defined datum (the standard says to store it in GeographicTypeGeoKey)
        read_count = GTIFKeyGet (input_gtif, GeographicTypeGeoKey, &geokey_datum, 0, 1);
        if (read_count == 1) {
          switch(geokey_datum){
            case GCS_WGS_84:
            case GCSE_WGS84:
              datum = WGS84_DATUM;
              break;
            case GCS_NAD27:
              datum = NAD27_DATUM;
              break;
            case GCS_NAD83:
              datum = NAD83_DATUM;
              break;
	    case GCS_ED50:
	      datum = ED50_DATUM;
	      break;
	    case GCS_SAD69:
	      datum = SAD69_DATUM;
	      break;
            default:
              datum = UNKNOWN_DATUM;
              break;
          }
        }
        if (datum == UNKNOWN_DATUM) {
          // The datum is not typically stored in GeogGeodeticDatumGeoKey, but some s/w
          // does put it there
          read_count = GTIFKeyGet (input_gtif, GeogGeodeticDatumGeoKey, &geokey_datum, 0, 1);
          if (read_count == 1) {
            switch(geokey_datum){
              case Datum_WGS84:
                datum = WGS84_DATUM;
                break;
              case Datum_North_American_Datum_1927:
                datum = NAD27_DATUM;
                break;
              case Datum_North_American_Datum_1983:
                datum = NAD83_DATUM;
                break;
	      case 6655: // ITRF97
		datum = ITRF97_DATUM;
		break;
	      case 6054: // HUGHES80
		datum = HUGHES_DATUM;
		break;
              default:
                datum = UNKNOWN_DATUM;
                break;
            }
          }
        }
        // Hughes datum support ...The Hughes-1980 datum is user-defined and the
        // typically defined by the major and inv-flattening
        // FIXME: There are several ways of representing otherwise-undefined datum
        // datum types ...maybe consider supporting those?  (Probably not...)
        // FIXME: Found out that there is an EPSG GCS code for NSIDC SSM/I polar
        // stereo ...for PS using Hughes, we should write this numeric value out
        // and avoid using a user-defined datum (technically there is no such thing as
        // a 'hughes datum' ...it's an earth-centered reference spheroid and the datum
        // is undetermined.  Sigh... works exactly the same either way blah blah blah.)
        if (datum == UNKNOWN_DATUM) {
          short int ellipsoid_key;
          read_count = GTIFKeyGet(input_gtif, GeogEllipsoidGeoKey, &ellipsoid_key, 0, 1);
          if (read_count && ellipsoid_key == USER_DEFINED_KEY) {
            double semi_major = 0.0;
            double semi_minor = 0.0;
            double inv_flattening = 0.0;
            double hughes_semiminor = HUGHES_SEMIMAJOR * (1.0 - 1.0/HUGHES_INV_FLATTENING);
            read_count = GTIFKeyGet(input_gtif, GeogSemiMajorAxisGeoKey, &semi_major, 0, 1);
            read_count += GTIFKeyGet(input_gtif, GeogInvFlatteningGeoKey, &inv_flattening, 0, 1);
            read_count += GTIFKeyGet(input_gtif, GeogSemiMinorAxisGeoKey, &semi_minor, 0, 1);
            if (read_count >= 2 &&
                     semi_major != USER_DEFINED_KEY &&
                     inv_flattening != USER_DEFINED_KEY &&
                     FLOAT_COMPARE_TOLERANCE(semi_major, HUGHES_SEMIMAJOR, FLOAT_TOLERANCE) &&
                     FLOAT_COMPARE_TOLERANCE(inv_flattening, HUGHES_INV_FLATTENING, FLOAT_TOLERANCE))
            {
              datum = HUGHES_DATUM;
            }
            else if (read_count >= 2 &&
                     semi_major != USER_DEFINED_KEY &&
                     semi_minor != USER_DEFINED_KEY &&
                     FLOAT_COMPARE_TOLERANCE(semi_major, HUGHES_SEMIMAJOR, FLOAT_TOLERANCE) &&
                     FLOAT_COMPARE_TOLERANCE(semi_minor, hughes_semiminor, FLOAT_TOLERANCE))
            {
              datum = HUGHES_DATUM;
            }
            else if (read_count >= 2 &&
                     semi_minor != USER_DEFINED_KEY &&
                     inv_flattening != USER_DEFINED_KEY &&
                     FLOAT_COMPARE_TOLERANCE(semi_minor, hughes_semiminor, FLOAT_TOLERANCE) &&
                     FLOAT_COMPARE_TOLERANCE(inv_flattening, HUGHES_INV_FLATTENING, FLOAT_TOLERANCE))
            {
              datum = HUGHES_DATUM;
            }
            else {
              datum = UNKNOWN_DATUM;
            }
          }
          else {
            datum = UNKNOWN_DATUM;
          }
        }
        if (datum == UNKNOWN_DATUM) {
          asfPrintWarning("Unable to determine datum type from GeoTIFF file\n"
                        "Defaulting to WGS-84 ...This may result in projection errors\n");
          datum = WGS84_DATUM;
        }
        // Take whatever datum we have at this point
        mp->datum = datum;

        // Base on the type of projection coordinate transformation, e.g. type of projection,
        // retrieve the rest of the projection parameters
        projection_type = UNKNOWN_PROJECTION;
        scale_factor = DEFAULT_SCALE_FACTOR;
        switch(proj_coords_trans) {
          case CT_TransverseMercator:
          case CT_TransvMercator_Modified_Alaska:
          case CT_TransvMercator_SouthOriented:
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintStatus("No false easting in ProjFalseEastingGeoKey ...OK for a UTM\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintStatus("No false northing in ProjFalseNorthingGeoKey ...OK for a UTM\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintStatus("No center longitude in ProjNatOriginLongGeoKey ...OK for a UTM\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintStatus("No center latitude in ProjNatOriginLatGeoKey ...OK for a UTM\n");
            }
            else {
              latOrigin = 0.0;
            }
            read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
            if (read_count == 0) {
              scale_factor = DEFAULT_UTM_SCALE_FACTOR;

              char msg[256];
              sprintf(msg,"UTM scale factor defaulting to %0.4f ...OK for a UTM\n", scale_factor);
              asfPrintStatus(msg);
            }
            mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
            mp->hem = (false_northing == 0.0) ? 'N' : (false_northing == 10000000.0) ? 'S' : '?';
            mp->param.utm.zone = utm_zone(lonOrigin);
            mp->param.utm.false_easting = false_easting;
            mp->param.utm.false_northing = false_northing;
            mp->param.utm.lat0 = latOrigin;
            mp->param.utm.lon0 = lonOrigin;
            mp->param.utm.scale_factor = scale_factor;
            check_projection_parameters(mp);
            break;
          // Albers Conical Equal Area case IS tested
          case CT_AlbersEqualArea:
            read_count = GTIFKeyGet (input_gtif, ProjStdParallel1GeoKey, &stdParallel1, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine first standard parallel from GeoTIFF file\n"
                  "using ProjStdParallel1GeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjStdParallel2GeoKey, &stdParallel2, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine second standard parallel from GeoTIFF file\n"
                  "using ProjStdParallel2GeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false easting from GeoTIFF file\n"
                  "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                  "using ProjFalseNorthingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning("Unable to determine center longitude from GeoTIFF file\n"
                  "using ProjNatOriginLongGeoKey.  Trying ProjCenterLongGeoKey...\n");
              read_count = GTIFKeyGet (input_gtif, ProjCenterLongGeoKey, &lonOrigin, 0, 1);
              if (read_count != 1) {
                asfPrintWarning("Unable to determine center longitude from GeoTIFF file\n"
                    "using ProjCenterLongGeoKey as well...\n");
              }
              else {
                asfPrintStatus("\nFound center longitude from ProjCenterLongGeoKey in GeoTIFF"
                               "file...\n\n");
              }
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjNatOriginLatGeoKey\n");
            }
            mp->type = ALBERS_EQUAL_AREA;
            mp->hem = (latOrigin > 0.0) ? 'N' : 'S';
            mp->param.albers.std_parallel1 = stdParallel1;
            mp->param.albers.std_parallel2 = stdParallel2;
            mp->param.albers.center_meridian = lonOrigin;
            mp->param.albers.orig_latitude = latOrigin;
            mp->param.albers.false_easting = false_easting;
            mp->param.albers.false_northing = false_northing;
            check_projection_parameters(mp);
            break;
          // FIXME: The Lambert Conformal Conic 1-Std Parallel case is UNTESTED
          case CT_LambertConfConic_1SP:
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                        "Unable to determine false easting from GeoTIFF file\n"
                  "using ProjFalseEastingGeoKey.  Assuming 0.0 meters and continuing...\n");
              false_easting = 0.0;
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                      "using ProjFalseNorthingGeoKey.  Assuming 0.0 meters and continuing...\n");
              false_northing = 0.0;
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center longitude from GeoTIFF file\n"
                  "using ProjNatOriginLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjNatOriginLatGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
            if (read_count != 1) {
              scale_factor = DEFAULT_SCALE_FACTOR;

              char msg[256];
              sprintf(msg,
                      "Lambert Conformal Conic scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4f\n",
                      scale_factor);
              asfPrintWarning(msg);
            }
            mp->type = LAMBERT_CONFORMAL_CONIC;
            mp->hem = (latOrigin > 0.0) ? 'N' : 'S';
            mp->param.lamcc.plat1 = latOrigin;
            mp->param.lamcc.plat2 = latOrigin;
            mp->param.lamcc.lat0 = latOrigin;
            mp->param.lamcc.lon0 = lonOrigin;
            mp->param.lamcc.false_easting = false_easting;
            mp->param.lamcc.false_northing = false_northing;
            mp->param.lamcc.scale_factor = scale_factor;
            check_projection_parameters(mp);
            break;
          case CT_LambertConfConic_2SP:
            read_count = GTIFKeyGet (input_gtif, ProjStdParallel1GeoKey, &stdParallel1, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine first standard parallel from GeoTIFF file\n"
                  "using ProjStdParallel1GeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjStdParallel2GeoKey, &stdParallel2, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine second standard parallel from GeoTIFF file\n"
                  "using ProjStdParallel2GeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false easting from GeoTIFF file\n"
                      "using ProjFalseEastingGeoKey.  Assuming 0.0 meters and continuing...\n");
              false_easting = 0.0;
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                      "using ProjFalseNorthingGeoKey.  Assuming 0.0 meters and continuing...\n");
              false_northing = 0.0;
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center longitude from GeoTIFF file\n"
                  "using ProjFalseOriginLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjFalseOriginLatGeoKey\n");
            }
            mp->type = LAMBERT_CONFORMAL_CONIC;
            mp->hem = (latOrigin > 0.0) ? 'N' : 'S';
            mp->param.lamcc.plat1 = stdParallel1;
            mp->param.lamcc.plat2 = stdParallel2;
            mp->param.lamcc.lat0 = latOrigin;
            mp->param.lamcc.lon0 = lonOrigin;
            mp->param.lamcc.false_easting = false_easting;
            mp->param.lamcc.false_northing = false_northing;
            mp->param.lamcc.scale_factor = scale_factor;
            check_projection_parameters(mp);
            break;
          case CT_PolarStereographic:
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjNatOriginLatGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjStraightVertPoleLongGeoKey, &lonPole, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine vertical pole longitude from GeoTIFF file\n"
                  "using ProjStraightVertPoleLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false easting from GeoTIFF file\n"
                  "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                  "using ProjFalseNorthingGeoKey\n");
            }
            // NOTE: The scale_factor exists in the ProjScaleAtNatOriginGeoKey, but we do not
            // use it, e.g. it is not current written to the meta data file with meta_write().
            mp->type = POLAR_STEREOGRAPHIC;
            mp->hem = (latOrigin > 0) ? 'N' : 'S';
            mp->param.ps.slat = latOrigin;
            mp->param.ps.slon = lonPole;
            mp->param.ps.is_north_pole = (mp->hem == 'N') ? 1 : 0;
            mp->param.ps.false_easting = false_easting;
            mp->param.ps.false_northing = false_northing;
            check_projection_parameters(mp);
            break;
          case CT_LambertAzimEqualArea:
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false easting from GeoTIFF file\n"
                  "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                  "using ProjFalseNorthingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjCenterLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center longitude from GeoTIFF file\n"
                  "using ProjCenterLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjCenterLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjCenterLatGeoKey\n");
            }
            mp->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
            mp->hem = (latOrigin > 0) ? 'N' : 'S';
            mp->param.lamaz.center_lon = lonOrigin;
            mp->param.lamaz.center_lat = latOrigin;
            mp->param.lamaz.false_easting = false_easting;
            mp->param.lamaz.false_northing = false_northing;
            check_projection_parameters(mp);
            break;
	  case CT_Equirectangular:
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjNatOriginLatGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center longitude from GeoTIFF file\n"
                  "using ProjNatOriginLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false easting from GeoTIFF file\n"
                  "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                  "using ProjFalseNorthingGeoKey\n");
            }
            mp->type = EQUI_RECTANGULAR;
            mp->hem = (latOrigin > 0.0) ? 'N' : 'S';
            mp->param.eqr.orig_latitude = latOrigin;
            mp->param.eqr.central_meridian = lonOrigin;
            mp->param.eqr.false_easting = false_easting;
            mp->param.eqr.false_northing = false_northing;
            check_projection_parameters(mp);
	    break;
	  case CT_Mercator:
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center latitude from GeoTIFF file\n"
                  "using ProjNatOriginLatGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine center longitude from GeoTIFF file\n"
                  "using ProjNatOriginLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false easting from GeoTIFF file\n"
                  "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                  "using ProjFalseNorthingGeoKey\n");
            }
            // FIXME: convert scale factor into standard parallel
            mp->type = MERCATOR;
            mp->hem = (latOrigin > 0.0) ? 'N' : 'S';
            mp->param.mer.orig_latitude = latOrigin;
            mp->param.mer.central_meridian = lonOrigin;
            mp->param.mer.false_easting = false_easting;
            mp->param.mer.false_northing = false_northing;
            check_projection_parameters(mp);
	    break;
	  case CT_Sinusoidal:
            read_count = GTIFKeyGet (input_gtif, ProjCenterLongGeoKey, 
				     &lonOrigin, 0, 1);
            if (read_count != 1) {
              asfPrintWarning("Unable to determine center longitude from "
			      "GeoTIFF file\nusing ProjCenterLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, 
				     &false_easting, 0, 1);
            if (read_count != 1) {
              asfPrintWarning("Unable to determine false easting from GeoTIFF "
			      "file\nusing ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, 
				     &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning("Unable to determine false northing from GeoTIFF"
			      " file\nusing ProjFalseNorthingGeoKey\n");
            }
	    mp->type = SINUSOIDAL;
	    mp->hem = mg->center_latitude > 0.0 ? 'N' : 'S';
	    mp->param.sin.longitude_center = lonOrigin;
	    mp->param.sin.false_easting = false_easting;
	    mp->param.sin.false_northing = false_northing;
	    mp->param.sin.sphere = mp->re_major;
            check_projection_parameters(mp);
      	    break;
          default:
            asfPrintWarning(
                "Unable to determine projection type from GeoTIFF file\n"
                "using ProjectedCSTypeGeoKey or ProjCoordTransGeoKey\n");
            asfPrintWarning("Projection parameters missing in the GeoTIFF\n"
                "file.  Projection parameters may be incomplete unless\n"
                "they are found in the associated .aux file (if it exists.)\n");
            break;
        }
      }
    } // End of if UTM else OTHER projection type
  } // End of reading projection parameters from geotiff ...if it existed
  else if (model_type == ModelTypeGeographic && geotiff_data_exists) {
    // Set the projection block data that we know at this point
    if (tie_point[0] != 0 || tie_point[1] != 0 || tie_point[2] != 0) {
      // NOTE: To support tie points at other locations, or a set of other locations,
      // then things rapidly get more complex ...and a transformation must either be
      // derived or provided (and utilized etc).  We're not at that point yet...
      //
      asfPrintError("Unsupported initial tie point type.  Initial tie point must be for\n"
          "raster location (0,0) in the image.\n");
    }
    short geographic_type;
    read_count
        = GTIFKeyGet (input_gtif, GeographicTypeGeoKey, &geographic_type, 0, 1);
    //asfRequire (read_count == 1, "GTIFKeyGet failed.\n");
    datum = UNKNOWN_DATUM;
    switch ( geographic_type ) {
      case GCS_WGS_84:
        datum = WGS84_DATUM;
        break;
      case GCS_NAD27:
        datum = NAD27_DATUM;
        break;
      case GCS_NAD83:
        datum = NAD83_DATUM;
        break;
      default:
        asfPrintWarning ("Unsupported GeographicTypeGeoKey value in GeoTIFF file");
        datum = WGS84_DATUM;
        break;
        break;
    }
    spheroid_type_t spheroid = datum_spheroid (datum);

    // NOTE: For geographic geotiffs, all angular units are converted to decimal
    // degrees upon ingest, hence the setting of mp->units to "degrees" here.
    // the angular_units variable contains, at this point, the type of unit that is
    // used within the geotiff itself, i.e. Angular_Arc_Second, Angular_Degree, etc.
    strcpy (mp->units, "degrees");
    mp->type = LAT_LONG_PSEUDO_PROJECTION;
    mp->hem = mg->center_latitude > 0.0 ? 'N' : 'S';
    mp->spheroid = spheroid;

    // These fields should be the same as the ones in the general block.
    mp->re_major = mg->re_major;
    mp->re_minor = mg->re_minor;

    mp->datum = datum;
    mp->height = 0.0; // Set to the mean from the statistics later (for DEMs)
  }
  else if (!geotiff_data_exists) {
    asfPrintWarning("Projection parameters missing in the GeoTIFF\n"
                "file.  Projection parameters may be incomplete unless.\n"
                "they are found in the associated .aux file (if it exists.)\n");
  }

  /*****************************************************/
  /***** CHECK TO SEE IF THIS IS AN ARCGIS GEOTIFF *****/
  /*                                                   */
  if (model_type == ModelTypeProjected && isArcgisGeotiff(inFileName)) {
    // If the TIFF is an ArcGIS / IMAGINE GeoTIFF, then read the
    // projection parameters from the aux file (if it exists)
    asfPrintStatus("Checking ArcGIS GeoTIFF auxiliary file (.aux) for\n"
        "projection parameters...\n");
    int ret = readArcgisAuxProjectionParameters(inFileName, mp);
    if (mp->datum != UNKNOWN_DATUM) {
      datum = mp->datum;
    }
    if (ret == 0) {
      asfPrintStatus("\nSUCCESS ...Found projection parameters in the .aux file.\n"
          "(These will supercede any found in the TIFF - which should have been\n"
          " the same anyway.)\n\n");
    }
  }
  /*                                                   */
  /*****************************************************/

  asfPrintStatus("\nLoading input TIFF/GeoTIFF file into %d-banded %s image structure...\n\n",
                 num_bands, (data_type == ASF_BYTE) ? "8-bit byte" :
                            (data_type == INTEGER16) ? "16-bit integer" :
                            (data_type == INTEGER32) ? "32-bit integer" :
                            (data_type == REAL32)    ? "32-bit float"   : "unknown(?)");

  // Get the raster width and height of the image.
  uint32 width;
  uint32 height;

  TIFFGetField(input_tiff, TIFFTAG_IMAGELENGTH, &height);
  TIFFGetField(input_tiff, TIFFTAG_IMAGEWIDTH, &width);
  if (height <= 0 || width <= 0) {
    asfPrintError("Invalid height and width parameters in TIFF file,\n"
        "Height = %ld, Width = %ld\n", height, width);
  }
  
  /***** FILL IN THE REST OF THE META DATA (Projection parms should already exist) *****/
  /*                                                                                   */
  char image_data_type[256];
  mg->data_type = data_type;
  int is_usgs_seamless_geotiff = 0;
  if (angular_units == Angular_Degree &&
      citation && strncmp(citation, "IMAGINE GeoTIFF Support", 23) == 0) {
    // This is a good guess since the only source of lat/long geotiffs that I know of
    // are the USGS Seamless Server geotiffs.  Note that the image_data_type setting
    // will be overridden by the parameter list if the caller specified something.
    //
    // Note that even if this guess is wrong, it should still work fine for other
    // angular degree geotiffs except that the image_data_type and sensor string may
    // be misleading ...this won't affect processing by any of our tools.
    asfPrintStatus("\nGeoTIFF contains lat/long in decimal degrees.  Assuming this is a\n"
            "USGS Seamless Server (or compatible) type of DEM GeoTIFF with pixel\n"
            "size of 30, 60, 90, or 190 meters, i.e. SRTM, NED, DTED, etcetera...\n");
    strcpy(mg->sensor, "");
    strcpy(image_data_type, ""); 
    mg->image_data_type = DEM;
    is_usgs_seamless_geotiff = 1;
  }
  else if (angular_units == Angular_Degree ||
           angular_units == Angular_Arc_Second)
  {
    // All other angular units
      asfPrintStatus("\nGeoTIFF contains lat/long in %s.  Assuming this is a\n"
              "USGS Seamless Server (or compatible) type of DEM GeoTIFF with pixel\n"
              "size of 30, 60, 90, 190 meters, i.e. SRTM, NED, DTED, etcetera...\n",
              angular_units_to_string(angular_units));
      strcpy(mg->sensor, "");
      is_usgs_seamless_geotiff = 1; // This will turn on conversion of pixel size from degrees to meters
  }
  else {
      strcpy(mg->sensor, "GEOTIFF");
      strcpy(mg->sensor_name, "GEOTIFF");
  }
  strcpy(mg->basename, inFileName);

  // Get the image data type from the variable arguments list
  char *pTmpChar=NULL;
  va_start(ap, ignore); // 'ignore' is the last argument before ", ..."
  pTmpChar = (char *)va_arg(ap, char *);
  if (pTmpChar != NULL &&
      strlen(pTmpChar) >= 3 &&
      (strncmp(uc(pTmpChar), "DEM", 3) == 0 ||
       strncmp(uc(pTmpChar), "MASK", 4) == 0))
  {
    strcpy(image_data_type, uc(pTmpChar));
  }
  else {
    if (is_usgs_seamless_geotiff) {
      strcpy(image_data_type, "DEM");
    }
    else if (geotiff_data_exists) {
      strcpy(image_data_type, "GEOCODED_IMAGE");
    }
    else {
      strcpy(image_data_type, "IMAGE");
    }
  }
  va_end(ap);
  if (strncmp(image_data_type, "DEM", 3) == 0) {
    mg->image_data_type = DEM;
  }
  else if (strncmp(image_data_type, "MASK", 4) == 0) {
    mg->image_data_type = MASK;
  }
  else if (strncmp(image_data_type, "AMPLITUDE_IMAGE", 15) == 0) {
    mg->image_data_type = AMPLITUDE_IMAGE;
  }
  else if (strncmp(image_data_type, "GEOCODED_IMAGE", 15) == 0) {
    mg->image_data_type = GEOCODED_IMAGE;
  }
  else if (strncmp(image_data_type, "IMAGE", 5) == 0) {
    mg->image_data_type = IMAGE;
  }

  mg->line_count = height;
  mg->sample_count = width;

  mg->start_line = 0;
  mg->start_sample = 0;

  float base_x_pixel_scale = pixel_scale[0];
  float base_y_pixel_scale = pixel_scale[1];
  if (is_usgs_seamless_geotiff) {
      // Convert angular units to decimal degrees if necessary
      switch(angular_units) {
          case Angular_Arc_Second:
              base_x_pixel_scale *= ARCSECONDS2DEGREES;
              base_y_pixel_scale *= ARCSECONDS2DEGREES;
              break;
          case Angular_Degree:
          default:
              break;
      }

      mg->x_pixel_size = base_x_pixel_scale;
      mg->y_pixel_size = base_y_pixel_scale;
  }
  else if (linear_units == Linear_Foot ||
	   linear_units == Linear_Foot_US_Survey ||
	   linear_units == Linear_Foot_Modified_American ||
	   linear_units == Linear_Foot_Clarke ||
	   linear_units == Linear_Foot_Indian) {
    // Hack: The exact number for unit 'ft' needs to be extracted from the file
    base_x_pixel_scale *= 0.3048;
    base_y_pixel_scale *= 0.3048;
    mg->x_pixel_size = base_x_pixel_scale;
    mg->y_pixel_size = base_y_pixel_scale;
    asfPrintWarning("Units converted from feet to meters by adjusting the pixel size.\n"
		    "Azimuth pixel size changed from %.3f ft to %.3f m.\n"
		    "Range pixel size changed from %.3f ft to %.3f m.\n", 
		    pixel_scale[0], base_x_pixel_scale, pixel_scale[1], base_y_pixel_scale);
  }
  else {
      mg->x_pixel_size = pixel_scale[0];
      mg->y_pixel_size = pixel_scale[1];
  }

  // For now we are going to insist that the meters per pixel in the
  // X and Y directions are identical(ish).  I believe asf_geocode at
  // least would work for non-square pixel dimensions, with the
  // caveats that output pixels would still be square, and would have
  // default size derived solely from the input pixel size
  if (fabs(mg->x_pixel_size - mg->y_pixel_size) > 0.0001) {
    char msg[256];
    sprintf(msg, "Pixel size is (x,y): (%f, %f)\n", mg->x_pixel_size, mg->y_pixel_size);
    asfPrintStatus(msg);
    asfPrintWarning("Found non-square pixels: x versus y pixel size differs\n"
        "by more than 0.0001 <units>\n");
  }

  // Image raster coordinates of tie point.
  double raster_tp_x = tie_point[0];
  double raster_tp_y = tie_point[1]; // Note: [2] is zero for 2D space

  // Coordinates of tie point in projection space.
  // NOTE: These are called tp_lon and tp_lat ...but the tie points
  // will be in either linear units (meters typ.) *OR* lat/long depending
  // on what type of image data is in the file, e.g. map-projected geographic or
  // geocentric respectively (but we only support map-projected at this point.)
  double tp_lon = tie_point[3]; // x
  double tp_lat = tie_point[4]; // y, Note: [5] is zero for 2D space

  // Calculate center of image data ...using linear meters or decimal degrees
  double center_x = MAGIC_UNSET_DOUBLE;
  double center_y = MAGIC_UNSET_DOUBLE;
  if (linear_units == Linear_Meter ||
      linear_units == Linear_Foot ||
      linear_units == Linear_Foot_US_Survey ||
      linear_units == Linear_Foot_Modified_American ||
      linear_units == Linear_Foot_Clarke ||
      linear_units == Linear_Foot_Indian) {
      // NOTE: center_x and center_y are in meters (map projection coordinates)
      // and are converted to lat/lon for center latitude/longitude below.  Therefore,
      // since geographic geotiffs already contain angular measure, they don't need
      // a center_x, center_y calculated.
      // FIXME: Is tp_lon and tp_lat in degrees or meters for this case?
      center_x = (width / 2.0 - raster_tp_x) * base_x_pixel_scale + tp_lon;
      center_y = (height / 2.0 - raster_tp_y) * (-base_y_pixel_scale) + tp_lat;
  }

  // If the datum and/or spheroid are unknown at this point, then fill
  // them out, and the major/minor axis, as best we can.
  if (datum != UNKNOWN_DATUM && mp->spheroid == UNKNOWN_SPHEROID) {
      // Guess the spheroid from the type of datum (a fairly safe guess)
      mp->spheroid = datum_spheroid(mp->datum);
  }
  if (datum == UNKNOWN_DATUM && mp->spheroid != UNKNOWN_SPHEROID) {
      // Can't guess a datum, so leave it be
      datum = spheroid_datum(mp->spheroid);
  }
  if (datum == UNKNOWN_DATUM && mp->spheroid == UNKNOWN_SPHEROID && mp &&
      mp->re_major != MAGIC_UNSET_DOUBLE && mp->re_minor != MAGIC_UNSET_DOUBLE)
  {
      // If neither the datum nor spheroid are known, try to derive them from
      // the axis lengths in the map projection record
      mp->spheroid = axis_to_spheroid(mp->re_major, mp->re_minor);
      datum = spheroid_datum(mp->spheroid);
  }
  if (mp->spheroid != UNKNOWN_SPHEROID) {
      spheroid_axes_lengths (mp->spheroid, &mg->re_major, &mg->re_minor);
  }

  if (isArcgisGeotiff(inFileName)        &&
      mp->re_major != MAGIC_UNSET_DOUBLE &&
      mp->re_minor != MAGIC_UNSET_DOUBLE)
  {
    // The ArcGIS metadata reader sets the projection parms, not the general
    // block parms, so copy them over...
    mg->re_major = mp->re_major;
    mg->re_minor = mp->re_minor;
  }

  if (!isArcgisGeotiff(inFileName) &&
      (mp->startX == MAGIC_UNSET_DOUBLE ||
      mp->startY == MAGIC_UNSET_DOUBLE ||
      mp->perX == MAGIC_UNSET_DOUBLE ||
      mp->perY == MAGIC_UNSET_DOUBLE))
  {
    mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
    mp->startY = (0.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;
    mp->perX = mg->x_pixel_size;
    mp->perY = -mg->y_pixel_size;
  }
  else if (is_usgs_seamless_geotiff) {
      if (linear_units == Linear_Meter) {
          // FIXME: Is tp_lon and tp_lat in degrees or meters for this case?
          mp->startX = (0.0 - raster_tp_x) * base_x_pixel_scale + tp_lon;
          mp->startY = (0.0 - raster_tp_y) * (-base_y_pixel_scale) + tp_lat;
      }
      else if (angular_units == Angular_Degree) {
          mp->startX = (0.0 - raster_tp_x) * base_x_pixel_scale + tp_lon;
          mp->startY = (0.0 - raster_tp_y) * (-base_y_pixel_scale) + tp_lat;
      }
      else if (angular_units == Angular_Arc_Second) {
          mp->startX = (0.0 - raster_tp_x) * (base_x_pixel_scale * ARCSECONDS2DEGREES) + tp_lon;
          mp->startY = (0.0 - raster_tp_y) * (-(base_y_pixel_scale * ARCSECONDS2DEGREES)) + tp_lat;
      }
      mp->perX = pixel_scale[0];
      mp->perY = -pixel_scale[1];

      if (raster_type == RasterPixelIsPoint) {
        mp->startX -= mp->perX / 2.0; 
        //mp->startY -= fabs(mp->perY) / 2.0;
        mp->startY -= mp->perY / 2.0; 
      }    
  }

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  // Fill out the number of bands and the band names
  strcpy(mg->bands, "");
  mg->band_count = num_bands;
  int *empty = (int*)CALLOC(num_bands, sizeof(int)); // Defaults to 'no empty bands'
  char *band_str;
  band_str = (char*)MALLOC(100*sizeof(char)); // '100' is the array length of mg->bands (see asf_meta.h) ...yes, I know.
  int num_found_bands;
  char *tmp_citation = (citation != NULL) ? STRDUP(citation) : NULL;
  int is_asf_geotiff = 0;
  if (tmp_citation) is_asf_geotiff = strstr(tmp_citation, "Alaska Satellite Fac") ? 1 : 0;
  get_bands_from_citation(&num_found_bands, &band_str, empty, tmp_citation, num_bands);
  meta_statistics *stats = NULL;
  double mask_value = MAGIC_UNSET_DOUBLE;

  // Look for a non-standard GDAL tag that contains the no data value
  char* charDummy = NULL;
  if (TIFFGetField(input_tiff,TIFFTAG_GDAL_NODATA,&charDummy)) {
    if (strcmp_case( charDummy, "nan" ) != 0) {
      mg->no_data = (double)atof(charDummy);
    }
  }

  if (!is_asf_geotiff) {
    asfPrintStatus("\nNo ASF-exported band names found in GeoTIFF citation tag.\n"
        "Band names will be assigned in numerical order.\n");
  }
  else {
    // This is an ASF GeoTIFF so we must check to see if any bands are empty (blank)
    // Since some blank-band GeoTIFFs exported by ASF tools do NOT have the list of
    // bands placed in the citation string, we will need to make a best-guess based
    // on band statistics... but only if the citation isn't cooperating.
    if (num_found_bands < 1) {
      asfPrintStatus("\nGathering image statistics (per available band)...\n");
      switch(meta_out->general->data_type) {
        case ASF_BYTE:
        case INTEGER16:
        case INTEGER32:
          mask_value = UINT8_IMAGE_DEFAULT_MASK;
          break;
        case REAL32:
          mask_value = FLOAT_IMAGE_DEFAULT_MASK;
          break;
        default:
          mask_value = 0.0;
          break;
      }
      mg->no_data = mask_value;
      // If there are no band names in the citation, then collect stats and check for empty
      // bands that way
      stats = meta_statistics_init(num_bands);
      int is_dem = (mg->image_data_type == DEM) ? 1 : 0;
      if(!stats) asfPrintError("Out of memory.  Cannot allocate statistics struct.\n");
      int ii, nb;
      for (ii=0, nb=num_bands; ii<num_bands; ii++) {
        int ret;
        ret = tiff_image_band_statistics(input_tiff, meta_out,
                                      &stats->band_stats[ii], is_dem,
                                      num_bands, ii,
                                      bits_per_sample, sample_format,
                                      planar_config, 0, mask_value);
        if (ret != 0 ||
            (stats->band_stats[ii].mean == stats->band_stats[ii].min &&
             stats->band_stats[ii].mean == stats->band_stats[ii].max &&
             stats->band_stats[ii].mean == stats->band_stats[ii].std_deviation)) {
          // Band data is blank, e.g. no variation ...all pixels the same
          asfPrintStatus("\nFound empty band (see statistics below):\n"
              "   min = %f\n"
              "   max = %f\n"
              "  mean = %f\n"
              "  sdev = %f\n\n",
              stats->band_stats[ii].min,
              stats->band_stats[ii].max,
              stats->band_stats[ii].mean,
              stats->band_stats[ii].std_deviation);
          ignore[ii] = 1; // EMPTY BAND FOUND
          nb--;
        }
        else {
          ignore[ii] = 0;
          asfPrintStatus("\nBand Statistics:\n"
              "   min = %f\n"
              "   max = %f\n"
              "  mean = %f\n"
              "  sdev = %f\n\n",
            stats->band_stats[ii].min,
            stats->band_stats[ii].max,
            stats->band_stats[ii].mean,
            stats->band_stats[ii].std_deviation);
        }
      }
    }
  }

  if (is_usgs_seamless_geotiff) {
    // USGS Seamless geotiffs are DEMs, which means they are one-banded and the mean
    // data value is the average height in the image ...we need to calculate the stats
    // in this case, so we can populate mp->mean properly.
    // NOTE: Even though USGS DEMs are one-banded, this code is written generically for
    // any number of bands in an arcsec or angular degrees lat/long geotiff
    asfPrintStatus("\nCalculating average height for USGS Seamless (SRTM, NED, etc) or DTED DEM...\n\n");
    stats = meta_statistics_init(num_bands);
    int is_dem = (mg->image_data_type == DEM) ? 1 : 0;
    if(!stats) asfPrintError("Out of memory.  Cannot allocate statistics struct.\n");
    int ii;
    int ret = 0;
    for (ii=0; ii<num_bands; ii++) {
      ret = tiff_image_band_statistics(input_tiff, meta_out,
                                       &stats->band_stats[ii], is_dem,
                                       num_bands, ii,
                                       bits_per_sample, sample_format,
                                       planar_config, 0, mask_value);
      asfPrintStatus("\nBand Statistics:\n"
                     "   min = %f\n"
                     "   max = %f\n"
                     "  mean = %f\n"
                     "  sdev = %f\n\n",
                     stats->band_stats[ii].min,
                     stats->band_stats[ii].max,
                     stats->band_stats[ii].mean,
                     stats->band_stats[ii].std_deviation);
      // Empty band?
      if (ret != 0) {
          asfPrintWarning("USGS Seamless (NED, SRTM, etc) or DTED DEM band %d appears to have no data.\n"
                  "Setting the average height to 0.0m and continuing...\n", ii+1);
          mp->height = 0.0;
          ignore[ii] = 1;
      }
      else {
          mp->height = stats->band_stats[0].mean;
          ignore[ii] = 0;
      }
    }
  }

  if ( num_found_bands > 0 && strlen(band_str) > 0) {
    // If a valid list of bands were in the citation string, then let the empty[] array,
    // which indicates which bands in the TIFF were listed as 'empty' overrule the
    // ignore[] array since it was just a best-guess based on band statistics
    //
    // Note:  The ignore[] array will be used when writing the binary file so that empty
    // bands in the TIFF won't be written to the output file
    int band_no, num_empty = 0;
    for (band_no=0; band_no<num_bands; band_no++) {
      ignore[band_no] = empty[band_no];
      num_empty += ignore[band_no] ? 1 : 0;
    }

    // Note: mg->band_count is set to the number of found bands after the
    // binary file is written ...if you do it before, then put_band_float_line()
    // will fail.
    strcpy(mg->bands, band_str);
    mg->band_count -= num_empty;
  }
  else {
    // Use the default band names if none were found in the citation string
    // Note: For the case where there is no list of band names
    // in the citation string, we are either importing somebody
    // else's geotiff, or we are importing one of our older ones.
    // The only way, in that case, to know if a band is empty is
    // to rely on the band statistics from above.  The results
    // of this analysis is stored in the 'ignore[<band_no>]' array.

    // Note: num_bands is from the samples per pixel TIFF tag and is
    // the maximum number of valid (non-ignored) bands in the file
    int band_no, tmp_num_bands = num_bands;
    for (band_no=0; band_no<tmp_num_bands; band_no++) {
      if (ignore[band_no]) {
        // Decrement the band count for each ignored band
        num_bands--;
      }
      else {
        // Band is not ignored, so give it a band name
        if (band_no == 0) {
          sprintf(mg->bands, "%s", bands[band_no]);
        }
        else {
          sprintf(mg->bands, "%s,%s", mg->bands, bands[band_no]);
        }
      }
    }
    mg->band_count = num_bands;
  }
  FREE(band_str);
  if (mg->band_count <= 0 || strlen(mg->bands) <= 0) {
    asfPrintError("GeoTIFF file must contain at least one non-empty color channel (band)\n");
  }

  // Populate band stats if it makes sense
  if (((is_asf_geotiff && num_found_bands < 1) || is_usgs_seamless_geotiff) && stats) {
    // If this is an ASF GeoTIFF and no band names were found in the citation string,
    // then we HAD to have tried to identify blank bands with statistics ...if so, then
    // we may as well save the stats results in the metadata so some other processing
    // step can use them if it needs them (without having to recalculate them)
    //
    char **band_names=NULL;
    if (strlen(mg->bands) && strncmp(mg->bands, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) != 0) {
      band_names = extract_band_names(mg->bands, mg->band_count);
    }
    int bn;
    meta_out->stats = meta_statistics_init(num_bands);
    meta_statistics *mst = meta_out->stats;
    if (mst) {
      int ii;
      for (ii=0, bn=0; ii<num_bands; ii++) {
        if (!ignore[ii]) {
          if (band_names && band_names[bn] != NULL) {
            strcpy(mst->band_stats[bn].band_id, band_names[bn]);
          }
          else {
            sprintf(mst->band_stats[bn].band_id, "%02d", bn + 1);
          }
          mst->band_stats[bn].min = stats->band_stats[ii].min;
          mst->band_stats[bn].max = stats->band_stats[ii].max;
          mst->band_stats[bn].mean = stats->band_stats[ii].mean;
          mst->band_stats[bn].rmse = meta_is_valid_double(stats->band_stats[ii].rmse) ?
              stats->band_stats[ii].rmse : stats->band_stats[ii].std_deviation;
          mst->band_stats[bn].std_deviation = stats->band_stats[ii].std_deviation;
          mst->band_stats[bn].mask = mask_value;
          bn++;
        }
      }
    }
    else asfPrintError("Out of memory.  Cannot allocate statistics struct.\n");
  }

  // Calculate the center latitude and longitude now that the projection
  // parameters are stored.
  double center_latitude;
  double center_longitude;
  double dummy_var;
  meta_projection proj;

  // Copy all fields just in case of future code rearrangements...
  if (!is_usgs_seamless_geotiff) {
    copy_proj_parms (&proj, mp);
    proj_to_latlon(&proj,center_x, center_y, 0.0,
      &center_latitude, &center_longitude, &dummy_var);
    mg->center_latitude = R2D*center_latitude;
    mg->center_longitude = R2D*center_longitude;
  }
  else {
    mg->center_latitude = (height / 2.0 - raster_tp_y) * mp->perY + tp_lat;
    mg->center_longitude = (width / 2.0 - raster_tp_x) * mp->perX + tp_lon;
    mp->hem = (mg->center_latitude > 0.0) ? 'N' : 'S';
  }

  // Set up the location block
  if (is_usgs_seamless_geotiff) {
    ml->lon_start_near_range = mp->startX;
    ml->lat_start_near_range = mp->startY;
    ml->lon_start_far_range = mp->startX + mp->perX * width;
    ml->lat_start_far_range = mp->startY;
    ml->lon_end_near_range = mp->startX;
    ml->lat_end_near_range = mp->startY + mp->perY * height;
    ml->lon_end_far_range = mp->startX + mp->perX * width;
    ml->lat_end_far_range = mp->startY + mp->perY * height;
  }
  else {
    double lat, lon;
    proj_to_latlon(&proj, mp->startX, mp->startY, 0.0,
                  &lat, &lon, &dummy_var);
    ml->lat_start_near_range = R2D*lat;
    ml->lon_start_near_range = R2D*lon;

    proj_to_latlon(&proj, mp->startX + mp->perX * width, mp->startY, 0.0,
                  &lat, &lon, &dummy_var);
    ml->lat_start_far_range = R2D*lat;
    ml->lon_start_far_range = R2D*lon;

    proj_to_latlon(&proj, mp->startX, mp->startY + mp->perY * height, 0.0,
                  &lat, &lon, &dummy_var);
    ml->lat_end_near_range = R2D*lat;
    ml->lon_end_near_range = R2D*lon;

    proj_to_latlon(&proj, mp->startX + mp->perX * width, mp->startY + mp->perY * height, 0.0,
                  &lat, &lon, &dummy_var);
    ml->lat_end_far_range = R2D*lat;
    ml->lon_end_far_range = R2D*lon;
  }

  // Clean up
  GTIFFree(input_gtif);
  XTIFFClose(input_tiff);
  if(stats)FREE(stats);
  FREE (tmp_citation);
  FREE (citation);
    FREE (tie_point);
    FREE (pixel_scale);
  for (band_num = 0; band_num < MAX_BANDS; band_num++) {
    FREE(bands[band_num]);
  }

  return meta_out;
}

// Checking routine for projection parameter input.
void check_projection_parameters(meta_projection *mp)
{
  project_parameters_t *pp = &mp->param;

// FIXME: Hughes datum stuff commented out for now ...until Hughes is implemented in the trunk
   if (mp->datum == HUGHES_DATUM && mp->type != POLAR_STEREOGRAPHIC) {
     asfPrintError("Hughes ellipsoid is only supported for polar stereographic projections.\n");
   }

  switch (mp->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      // Tests for outside of allowed ranges errors:
      //
      // Valid UTM projections:
      //
      //   WGS84 + zone 1 thru 60 + N or S hemisphere
      //   NAD83 + zone 2 thru 23 + N hemisphere
      //   NAD27 + zone 2 thru 22 + N hemisphere
      //
      if (!meta_is_valid_int(pp->utm.zone)) {
        asfPrintError("Invalid zone number found (%d).\n", pp->utm.zone);
      }
      if (!meta_is_valid_double(pp->utm.lat0) || pp->utm.lat0 != 0.0) {
        asfPrintWarning("Invalid Latitude of Origin found (%.4f).\n"
            "Setting Latitude of Origin to 0.0\n", pp->utm.lat0);
        pp->utm.lat0 = 0.0;
      }
      if (pp->utm.lon0 != utm_zone_to_central_meridian(pp->utm.zone)) {
        asfPrintWarning("Invalid Longitude of Origin (%.4f) found\n"
            "for the given zone (%d).\n"
            "Setting Longitude of Origin to %f for zone %d\n",
            utm_zone_to_central_meridian(pp->utm.zone),
            pp->utm.zone);
        pp->utm.lon0 = utm_zone_to_central_meridian(pp->utm.zone);
      }
      switch(mp->datum) {
        case NAD27_DATUM:
          if (pp->utm.zone < 2 || pp->utm.zone > 22) {
            asfPrintError("Zone '%d' outside the supported range (2 to 22) for NAD27...\n"
                "  WGS 84, Zone 1 thru 60, Latitudes between -90 and +90\n"
                "  NAD83,  Zone 2 thru 23, Latitudes between   0 and +90\n"
                "  NAD27,  Zone 2 thru 22, Latitudes between   0 and +90\n\n",
                pp->utm.zone);
          }
          break;
        case NAD83_DATUM:
          if (pp->utm.zone < 2 || pp->utm.zone > 23) {
            asfPrintError("Zone '%d' outside the supported range (2 to 23) for NAD83...\n"
                "  WGS 84, Zone 1 thru 60, Latitudes between -90 and +90\n"
                "  NAD83,  Zone 2 thru 23, Latitudes between   0 and +90\n"
                "  NAD27,  Zone 2 thru 22, Latitudes between   0 and +90\n\n",
                pp->utm.zone);
          }
          break;
        case WGS84_DATUM:
          if (pp->utm.zone < 1 || pp->utm.zone > 60) {
            asfPrintError("Zone '%d' outside the valid range of (1 to 60) for WGS-84\n", pp->utm.zone);
          }
          break;
        case ITRF97_DATUM:
          if (pp->utm.zone < 1 || pp->utm.zone > 60) {
            asfPrintError("Zone '%d' outside the valid range of (1 to 60) for ITRF-97\n", pp->utm.zone);
          }
          break;
        default:
          asfPrintError("Unrecognized or unsupported datum found in projection parameters.\n");
          break;
      }
      if (!meta_is_valid_double(pp->utm.lon0) || pp->utm.lon0 < -180 || pp->utm.lon0 > 180) {
        asfPrintError("Longitude of Origin (%.4f) undefined or outside the defined range "
            "(-180 deg to 180 deg)\n", pp->utm.lon0);
      }
      if (!meta_is_valid_double(pp->utm.lat0) || pp->utm.lat0 != 0.0) {
        asfPrintError("Latitude of Origin (%.4f) undefined or invalid (should be 0.0)\n",
            pp->utm.lat0);
      }
      if (!meta_is_valid_double(pp->utm.scale_factor) || !FLOAT_EQUIVALENT(pp->utm.scale_factor, 0.9996)) {
        asfPrintError("Scale factor (%.4f) undefined or different from default value (0.9996)\n",
                    pp->utm.scale_factor);
      }
      if (!meta_is_valid_double(pp->utm.false_easting) || !FLOAT_EQUIVALENT(pp->utm.false_easting, 500000)) {
        asfPrintError("False easting (%.1f) undefined or different from default value (500000)\n",
                    pp->utm.false_easting);
      }
      if (mp->hem == 'N') {
        if (!meta_is_valid_double(pp->utm.false_northing) || !FLOAT_EQUIVALENT(pp->utm.false_northing, 0)) {
          asfPrintError("False northing (%.1f) undefined or different from default value (0)\n",
                      pp->utm.false_northing);
        }
      }
      else {
        if (!meta_is_valid_double(pp->utm.false_northing) || !FLOAT_EQUIVALENT(pp->utm.false_northing, 10000000)) {
          asfPrintError("False northing (%.1f) undefined or different from default value (10000000)\n",
                        pp->utm.false_northing);
        }
      }
      break;

    case POLAR_STEREOGRAPHIC:
      // Outside range tests
      if (!meta_is_valid_double(pp->ps.slat) || pp->ps.slat < -90 || pp->ps.slat > 90) {
        asfPrintError("Latitude of origin (%.4f) undefined or outside the defined range "
            "(-90 deg to 90 deg)\n", pp->ps.slat);
      }
      if (!meta_is_valid_double(pp->ps.slon) || pp->ps.slon < -180 || pp->ps.slon > 180) {
        asfPrintError("Central meridian (%.4f) undefined or outside the defined range "
            "(-180 deg to 180 deg)\n", pp->ps.slon);
      }

      // Distortion test - only areas with a latitude above 60 degrees North or
      // below -60 degrees South are permitted
      if (!meta_is_valid_int(pp->ps.is_north_pole) ||
           (pp->ps.is_north_pole != 0 && pp->ps.is_north_pole != 1)) {
        asfPrintError("Invalid north pole flag (%s) found.\n",
                    pp->ps.is_north_pole == 0 ? "SOUTH" :
                        pp->ps.is_north_pole == 1 ? "NORTH" : "UNKNOWN");
      }
      break;

    case ALBERS_EQUAL_AREA:
      // Outside range tests
      if (!meta_is_valid_double(pp->albers.std_parallel1) ||
           pp->albers.std_parallel1 < -90 ||
           pp->albers.std_parallel1 > 90) {
        asfPrintError("First standard parallel (%.4f) undefined or outside the defined range "
            "(-90 deg to 90 deg)\n", pp->albers.std_parallel1);
      }
      if (!meta_is_valid_double(pp->albers.std_parallel2) ||
           pp->albers.std_parallel2 < -90 ||
           pp->albers.std_parallel2 > 90) {
        asfPrintError("Second standard parallel (%.4f) undefined or outside the defined range "
            "(-90 deg to 90 deg)\n", pp->albers.std_parallel2);
      }
      if (!meta_is_valid_double(pp->albers.center_meridian) ||
           pp->albers.center_meridian < -180 ||
           pp->albers.center_meridian > 180) {
        asfPrintError("Central meridian (%.4f) undefined or outside the defined range "
            "(-180 deg to 180 deg)\n", pp->albers.center_meridian);
      }
      if (!meta_is_valid_double(pp->albers.orig_latitude) ||
           pp->albers.orig_latitude < -90 ||
           pp->albers.orig_latitude > 90) {
        asfPrintError("Latitude of origin (%.4f) undefined or outside the defined range "
            "(-90 deg to 90 deg)\n", pp->albers.orig_latitude);
      }
      break;

    case LAMBERT_CONFORMAL_CONIC:
      // Outside range tests
      if (!meta_is_valid_double(pp->lamcc.plat1) ||
           pp->lamcc.plat1 < -90 || pp->lamcc.plat1 > 90) {
        asfPrintError("First standard parallel (%.4f) undefined or outside the defined range "
            "(-90 deg to 90 deg)\n", pp->lamcc.plat1);
      }
      if (!meta_is_valid_double(pp->lamcc.plat2) ||
           pp->lamcc.plat2 < -90 || pp->lamcc.plat2 > 90) {
        asfPrintError("Second standard parallel '%.4f' outside the defined range "
            "(-90 deg to 90 deg)\n", pp->lamcc.plat2);
      }
      if (!meta_is_valid_double(pp->lamcc.lon0) ||
           pp->lamcc.lon0 < -180 || pp->lamcc.lon0 > 180) {
        asfPrintError("Central meridian '%.4f' outside the defined range "
            "(-180 deg to 180 deg)\n", pp->lamcc.lon0);
      }
      if (!meta_is_valid_double(pp->lamcc.lat0) ||
           pp->lamcc.lat0 < -90 || pp->lamcc.lat0 > 90) {
        asfPrintError("Latitude of origin '%.4f' outside the defined range "
            "(-90 deg to 90 deg)\n", pp->lamcc.lat0);
      }
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      // Outside range tests
      if (!meta_is_valid_double(pp->lamaz.center_lon) ||
           pp->lamaz.center_lon < -180 || pp->lamaz.center_lon > 180) {
        asfPrintError("Central meridian '%.4f' outside the defined range "
            "(-180 deg to 180 deg)\n", pp->lamaz.center_lon);
      }
      if (!meta_is_valid_double(pp->lamaz.center_lat) ||
           pp->lamaz.center_lat < -90 || pp->lamaz.center_lat > 90) {
        asfPrintError("Latitude of origin '%.4f' outside the defined range "
            "(-90 deg to 90 deg)\n", pp->lamaz.center_lat);
      }
      break;
    case SINUSOIDAL:
      if (!meta_is_valid_double(pp->sin.longitude_center) ||
	  pp->sin.longitude_center < -180 || 
	  pp->sin.longitude_center > 180) {
	asfPrintError("Longitude center '%.4f' outside the defined range "
		      "(-180 deg to 180 deg)\n", pp->lamaz.center_lon);
      }
      break;      
      
    default:
      break;
  }
}

int  band_float_image_write(FloatImage *oim, meta_parameters *omd,
                            const char *outBaseName, int num_bands,
                            int *ignore)
{
  char *outName;
  int row, col, band, offset;
  float *buf;

  buf = (float*)MALLOC(sizeof(float)*omd->general->sample_count);
  outName = (char*)MALLOC(sizeof(char)*strlen(outBaseName) + 5);
  strcpy(outName, outBaseName);
  append_ext_if_needed(outName, ".img", ".img");
  offset = omd->general->line_count;

  for (band=0; band < num_bands; band++) {
    if (num_bands > 1) {
      asfPrintStatus("Writing band %02d...\n", band+1);
    }
    else
    {
      asfPrintStatus("Writing binary image...\n");
    }
    FILE *fp=(FILE*)FOPEN(outName, band > 0 ? "ab" : "wb");
    if (fp == NULL) return 1;
    if (!ignore[band]) {
      for (row=0; row < omd->general->line_count; row++) {
        asfLineMeter(row, omd->general->line_count);
        for (col=0; col < omd->general->sample_count; col++) {
          buf[col] = float_image_get_pixel(oim, col, row+(offset*band));
        }
        put_float_line(fp, omd, row, buf);
      }
    }
    else {
      asfPrintStatus("  Empty band found ...ignored\n");
    }
    FCLOSE(fp);
  }
  FREE(buf);
  FREE(outName);

  return 0;
}

int  band_byte_image_write(UInt8Image *oim_b, meta_parameters *omd,
                           const char *outBaseName, int num_bands,
                           int *ignore)
{
  char *outName;
  int row, col, band, offset;
  float *buf;

  buf = (float*)MALLOC(sizeof(float)*omd->general->sample_count);
  outName = (char*)MALLOC(sizeof(char)*strlen(outBaseName) + 5);
  strcpy(outName, outBaseName);
  append_ext_if_needed(outName, ".img", ".img");
  offset = omd->general->line_count;

  for (band=0; band < num_bands; band++) {
    if (num_bands > 1) {
      asfPrintStatus("Writing band %02d...\n", band+1);
    }
    else
    {
      asfPrintStatus("Writing binary image...\n");
    }
    FILE *fp=(FILE*)FOPEN(outName, band > 0 ? "ab" : "wb");
    if (fp == NULL) return 1;
    if (!ignore[band]) {
      for (row=0; row < omd->general->line_count; row++) {
        asfLineMeter(row, omd->general->line_count);
        for (col=0; col < omd->general->sample_count; col++) {
          int curr_row = row+(offset*band);
          buf[col] = (float)uint8_image_get_pixel(oim_b, col, curr_row); //row+(offset*band));
        }
        put_float_line(fp, omd, row, buf);
      }
    }
    else {
      asfPrintStatus("  Empty band found ...ignored\n");
    }
    FCLOSE(fp);
  }
  FREE(buf);
  FREE(outName);

  return 0;
}

int tiff_image_band_statistics (TIFF *tif, meta_parameters *omd,
                                meta_stats *stats, int is_dem,
                                int num_bands, int band_no,
                                short bits_per_sample, short sample_format,
                                short planar_config,
                                int use_mask_value, double mask_value)
{
  tiff_type_t tiffInfo;

  // Determine what type of TIFF this is (scanline/strip/tiled)
  get_tiff_type(tif, &tiffInfo);
  if (tiffInfo.imageCount > 1) {
    asfPrintWarning("Found multi-image TIFF file.  Statistics will only be\n"
        "calculated from the bands in the first image in the file\n");
  }
  if (tiffInfo.imageCount < 1) {
    asfPrintError ("TIFF file contains zero images\n");
  }
  if (tiffInfo.format != SCANLINE_TIFF &&
      tiffInfo.format != STRIP_TIFF    &&
      tiffInfo.format != TILED_TIFF)
  {
    asfPrintError("Unrecognized TIFF type\n");
  }
  if (tiffInfo.volume_tiff) {
    asfPrintError("Multi-dimensional TIFF found ...only 2D TIFFs are supported.\n");
  }

  // Minimum and maximum sample values as integers.
  double fmin = FLT_MAX;
  double fmax = -FLT_MAX;
  double cs=0.0; // Current sample value

  stats->mean = 0.0;
  double s = 0.0;
  uint32 scanlineSize = 0;
  uint32 sample_count = 0;      // Samples considered so far.
  uint32 ii, jj;
  scanlineSize = TIFFScanlineSize(tif);
  if (scanlineSize <= 0) {
    return 1;
  }
  if (num_bands > 1 &&
      planar_config != PLANARCONFIG_CONTIG &&
      planar_config != PLANARCONFIG_SEPARATE)
  {
    return 1;
  }
  tdata_t *buf = _TIFFmalloc(scanlineSize);

  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all rows in the TIFF
    for ( ii = 0; ii < omd->general->line_count; ii++ )
    {
      asfPercentMeter((double)ii/(double)omd->general->line_count);
      // Get a data line from the TIFF
      switch (tiffInfo.format) {
        case SCANLINE_TIFF:
          if (planar_config == PLANARCONFIG_CONTIG || num_bands == 1) {
            TIFFReadScanline(tif, buf, ii, 0);
          }
          else {
            // Planar configuration is band-sequential
            TIFFReadScanline(tif, buf, ii, band_no);
          }
          break;
        case STRIP_TIFF:
          ReadScanline_from_TIFF_Strip(tif, buf, ii, band_no);
          break;
        case TILED_TIFF:
//          if (planar_config == PLANARCONFIG_CONTIG || num_bands == 1) {
//            ReadScanline_from_TIFF_TileRow(tif, buf, ii, 0);
//          }
//          else {
            // Planar configuration is band-sequential
          ReadScanline_from_TIFF_TileRow(tif, buf, ii, band_no);
//          }
          break;
        default:
          asfPrintError("Invalid TIFF format found.\n");
          break;
      }
      for (jj = 0 ; jj < omd->general->sample_count; jj++ ) {
        // iterate over each pixel sample in the scanline
        switch(bits_per_sample) {
          case 8:
            switch(sample_format) {
              case SAMPLEFORMAT_UINT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((uint8*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint8*)(buf))[jj]);
                }
                break;
              case SAMPLEFORMAT_INT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((int8*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((int8*)(buf))[jj]);   // Current sample.
                }
                break;
              default:
                // There is no such thing as an IEEE 8-bit floating point
                asfPrintError("Unexpected data type in GeoTIFF ...Cannot calculate statistics.\n");
                return 1;
                break;
            }
            if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
              continue;
            }
            break;
          case 16:
            switch(sample_format) {
              case SAMPLEFORMAT_UINT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((uint16*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint16*)(buf))[jj]);   // Current sample.
                }
                break;
              case SAMPLEFORMAT_INT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((int16*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint16*)(buf))[jj]);   // Current sample.
                }
                break;
              default:
                // There is no such thing as an IEEE 16-bit floating point
                asfPrintError("Unexpected data type in TIFF/GeoTIFF ...Cannot calculate statistics.\n");
                return 1;
                break;
            }
            if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
              continue;
            }
            break;
          case 32:
            switch(sample_format) {
              case SAMPLEFORMAT_UINT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((uint32*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint32*)(buf))[jj]);   // Current sample.
                }
                break;
              case SAMPLEFORMAT_INT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((long*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((long*)(buf))[jj]);   // Current sample.
                }
                break;
              case SAMPLEFORMAT_IEEEFP:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((float*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((float*)(buf))[jj]);   // Current sample.
                }
                if (is_dem && cs < -10e10) {
                  // Bad value removal for DEMs (really an adjustment, not a removal)
                  // -> This only applies to USGS Seamless DEMs and REAL32 data type <-
                  cs = -999.0;
                }
                break;
              default:
                asfPrintError("Unexpected data type in GeoTIFF ...Cannot calculate statistics.\n");
                return 1;
                break;
            }
            if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
              continue;
            }
            break;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = stats->mean;
        stats->mean += (cs - stats->mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - stats->mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    for ( ii = 0; ii < omd->general->line_count; ii++ )
    {
      asfPercentMeter((double)ii/(double)omd->general->line_count);
      // Get a data line from the TIFF
      switch (tiffInfo.format) {
        case SCANLINE_TIFF:
          if (planar_config == PLANARCONFIG_CONTIG || num_bands == 1) {
            TIFFReadScanline(tif, buf, ii, 0);
          }
          else {
            // Planar configuration is band-sequential
            TIFFReadScanline(tif, buf, ii, band_no);
          }
          break;
        case STRIP_TIFF:
          ReadScanline_from_TIFF_Strip(tif, buf, ii, band_no);
          break;
        case TILED_TIFF:
            // Planar configuration is band-sequential
          ReadScanline_from_TIFF_TileRow(tif, buf, ii, band_no);
          break;
        default:
          asfPrintError("Invalid TIFF format found.\n");
          break;
      }
      for (jj = 0 ; jj < omd->general->sample_count; jj++ ) {
        // iterate over each pixel sample in the scanline
        switch(bits_per_sample) {
          case 8:
            switch(sample_format) {
              case SAMPLEFORMAT_UINT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((uint8*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint8*)(buf))[jj]);
                }
                break;
              case SAMPLEFORMAT_INT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((int8*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((int8*)(buf))[jj]);   // Current sample.
                }
                break;
              default:
                // There is no such thing as an IEEE 8-bit floating point
                asfPrintError("Unexpected data type in GeoTIFF ...Cannot calculate statistics.\n");
                return 1;
                break;
            }
            break;
          case 16:
            switch(sample_format) {
              case SAMPLEFORMAT_UINT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((uint16*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint16*)(buf))[jj]);   // Current sample.
                }
                break;
              case SAMPLEFORMAT_INT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((int16*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint16*)(buf))[jj]);   // Current sample.
                }
                break;
              default:
                // There is no such thing as an IEEE 16-bit floating point
                asfPrintError("Unexpected data type in GeoTIFF ...Cannot calculate statistics.\n");
                return 1;
                break;
            }
            break;
          case 32:
            switch(sample_format) {
              case SAMPLEFORMAT_UINT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((uint32*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((uint32*)(buf))[jj]);   // Current sample.
                }
                break;
              case SAMPLEFORMAT_INT:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((long*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((long*)(buf))[jj]);   // Current sample.
                }
                break;
              case SAMPLEFORMAT_IEEEFP:
                if (planar_config == PLANARCONFIG_CONTIG && num_bands > 1) {
                  cs = (double)(((float*)(buf))[(jj*num_bands)+band_no]);   // Current sample.
                }
                else {
                  // Planar configuration is band-sequential or single-banded
                  cs = (double)(((float*)(buf))[jj]);   // Current sample.
                }
                if (is_dem && cs < -10e10) {
                  // Bad value removal for DEMs (really an adjustment, not a removal)
                  // -> This only applies to USGS Seamless DEMs and REAL32 data type <-
                  cs = -999.0;
                }
                break;
              default:
                asfPrintError("Unexpected data type in GeoTIFF ...Cannot calculate statistics.\n");
                return 1;
                break;
            }
            break;
          default:
            asfPrintError("Unexpected data type in GeoTIFF ...Cannot calculate statistics.\n");
            return 1;
            break;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = stats->mean;
        stats->mean += (cs - stats->mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - stats->mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  if (buf) _TIFFfree(buf);

  // Verify the new extrema have been found.
  //if (fmin == FLT_MAX || fmax == -FLT_MAX)
  if (gsl_fcmp (fmin, FLT_MAX, 0.00000000001) == 0 ||
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0)
    return 1;

  stats->min = fmin;
  stats->max = fmax;
  stats->std_deviation = sqrt (s / (sample_count - 1));

  // The new extrema had better be in the range supported range
  if (fabs(stats->mean) > FLT_MAX || fabs(stats->std_deviation) > FLT_MAX)
    return 1;

  return 0;
}

int  geotiff_band_image_write(TIFF *tif, meta_parameters *omd,
                              const char *outBaseName, int num_bands,
                              int *ignore, short bits_per_sample,
                              short sample_format, short planar_config)
{
  char *outName;
  int num_ignored;
  uint32 row, col, band;
  float *buf;
  tsize_t scanlineSize;

  // Determine what type of TIFF this is (scanline/strip/tiled)
  tiff_type_t tiffInfo;
  get_tiff_type(tif, &tiffInfo);
  if (tiffInfo.imageCount > 1) {
    asfPrintWarning("Found multi-image TIFF file.  Only the first image in the file\n"
                   "will be exported.\n");
  }
  if (tiffInfo.imageCount < 1) {
    asfPrintError ("TIFF file contains zero images\n");
  }
  if (tiffInfo.format != SCANLINE_TIFF &&
      tiffInfo.format != STRIP_TIFF    &&
      tiffInfo.format != TILED_TIFF)
  {
    asfPrintError("Unrecognized TIFF type\n");
  }
  if (tiffInfo.volume_tiff) {
    asfPrintError("Multi-dimensional TIFF found ...only 2D TIFFs are supported.\n");
  }

  buf = (float*)MALLOC(sizeof(float)*omd->general->sample_count);
  outName = (char*)MALLOC(sizeof(char)*strlen(outBaseName) + 5);
  strcpy(outName, outBaseName);
  append_ext_if_needed(outName, ".img", ".img");
  if (num_bands > 1 &&
      planar_config != PLANARCONFIG_CONTIG &&
      planar_config != PLANARCONFIG_SEPARATE)
  {
    asfPrintError("Unexpected planar configuration found in TIFF file\n");
  }

  scanlineSize = TIFFScanlineSize(tif);
  if (scanlineSize <= 0) {
    return 1;
  }
  tdata_t *tif_buf = _TIFFmalloc(scanlineSize);
  if (!tif_buf) {
    asfPrintError("Cannot allocate buffer for reading TIFF lines\n");
  }

  for (band=0, num_ignored=0; band < num_bands; band++) {
    if (num_bands > 1) {
      asfPrintStatus("\nWriting band %02d...\n", band+1);
    }
    else
    {
      asfPrintStatus("\nWriting binary image...\n");
    }
    FILE *fp=(FILE*)FOPEN(outName, band > 0 ? "ab" : "wb");
    if (fp == NULL) return 1;
    if (!ignore[band]) {
      for (row=0; row < omd->general->line_count; row++) {
        asfLineMeter(row, omd->general->line_count);
        switch (tiffInfo.format) {
          case SCANLINE_TIFF:
            if (planar_config == PLANARCONFIG_CONTIG || num_bands == 1) {
              TIFFReadScanline(tif, tif_buf, row, 0);
            }
            else {
            // Planar configuration is band-sequential
              TIFFReadScanline(tif, tif_buf, row, band);
            }
            break;
          case STRIP_TIFF:
            ReadScanline_from_TIFF_Strip(tif, tif_buf, row, band);
            break;
          case TILED_TIFF:
            // Planar configuration is band-sequential
            ReadScanline_from_TIFF_TileRow(tif, tif_buf, row, band);
            break;
          default:
            asfPrintError("Invalid TIFF format found.\n");
            break;
        }
        for (col=0; col < omd->general->sample_count; col++) {
          switch (bits_per_sample) {
            case 8:
              switch(sample_format) {
                case SAMPLEFORMAT_UINT:
                  ((float*)buf)[col] = (float)(((uint8*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_INT:
                  ((float*)buf)[col] = (float)(((int8*)tif_buf)[col]);
                  break;
                default:
                  // No such thing as an 8-bit IEEE float
                  asfPrintError("Unexpected data type in TIFF file ...cannot write ASF-internal\n"
                      "format file.\n");
                  break;
              }
              break;
            case 16:
              switch(sample_format) {
                case SAMPLEFORMAT_UINT:
                  ((float*)buf)[col] = (float)(((uint16*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_INT:
                  ((float*)buf)[col] = (float)(((int16*)tif_buf)[col]);
                  break;
                default:
                  // No such thing as an 16-bit IEEE float
                  asfPrintError("Unexpected data type in TIFF file ...cannot write ASF-internal\n"
                      "format file.\n");
                  break;
              }
              break;
            case 32:
              switch(sample_format) {
                case SAMPLEFORMAT_UINT:
                  ((float*)buf)[col] = (float)(((uint32*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_INT:
                  ((float*)buf)[col] = (float)(((long*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_IEEEFP:
                  ((float*)buf)[col] = (float)(((float*)tif_buf)[col]);
                  break;
                default:
                  asfPrintError("Unexpected data type in TIFF file ...cannot write ASF-internal\n"
                      "format file.\n");
                  break;
              }
              break;
            default:
              asfPrintError("Unexpected data type in TIFF file ...cannot write ASF-internal\n"
                  "format file.\n");
              break;
          }
        }
        put_band_float_line(fp, omd, band - num_ignored, (int)row, buf);
      }
    }
    else {
      asfPrintStatus("  Empty band found ...ignored\n");
      num_ignored++;
    }
    FCLOSE(fp);
  }
  FREE(buf);
  FREE(outName);
  if (tif_buf) _TIFFfree(tif_buf);

  return 0;
}

void ReadScanline_from_TIFF_Strip(TIFF *tif, tdata_t buf, unsigned long row, int band)
{
  int read_count;
  tiff_type_t t;
  tdata_t sbuf=NULL;
  tstrip_t strip;
  uint32 strip_row; // The row within the strip that contains the requested data row

  if (tif == NULL) {
    asfPrintError("TIFF file not open for read\n");
  }

  get_tiff_type(tif, &t);
  uint32 strip_size = TIFFStripSize(tif);
  sbuf = _TIFFmalloc(strip_size);

  short planar_config;    // TIFFTAG_PLANARCONFIG
  read_count = TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planar_config);
  if (read_count < 1) {
    asfPrintError("Cannot determine planar configuration from TIFF file.\n");
  }
  short samples_per_pixel;
  read_count = TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samples_per_pixel); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the number of samples per pixel from TIFF file.\n");
  }
  if (band < 0 || band > samples_per_pixel - 1) {
    asfPrintError("Invalid band number (%d).  Band number should range from %d to %d.\n",
                  0, samples_per_pixel - 1);
  }
  uint32 height;
  read_count = TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height); // Number of rows
  if (read_count < 1) {
    asfPrintError("Could not read the number of lines from TIFF file.\n");
  }
  uint32 width;
  read_count = TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width); // Number of pixels per row
  if (read_count < 1) {
    asfPrintError("Could not read the number of pixels per line from TIFF file.\n");
  }
  short bits_per_sample;
  read_count = TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bits_per_sample);
  if (read_count < 1) {
      asfPrintError("Could not read the bits per sample from TIFF file.\n");
  }
  short sample_format;
  read_count = TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sample_format); // int or float, signed or unsigned
  if (read_count < 1) {
      switch(bits_per_sample) {
          case 8:
              sample_format = SAMPLEFORMAT_UINT;
              break;
          case 16:
              sample_format = SAMPLEFORMAT_INT;
              break;
          case 32:
              sample_format = SAMPLEFORMAT_IEEEFP;
              break;
          default:
              asfPrintError("Could not read the sample format (data type) from TIFF file.\n");
              break;
      }
  }
  short orientation;
  read_count = TIFFGetField(tif, TIFFTAG_ORIENTATION, &orientation); // top-left, left-top, bot-right, etc
  if (read_count < 1) {
    orientation = ORIENTATION_TOPLEFT;
    read_count = 1;
  }
  if (read_count && orientation != ORIENTATION_TOPLEFT) {
    asfPrintError("Unsupported orientation found (%s)\n",
                  orientation == ORIENTATION_TOPRIGHT ? "TOP RIGHT" :
                  orientation == ORIENTATION_BOTRIGHT ? "BOTTOM RIGHT" :
                  orientation == ORIENTATION_BOTLEFT  ? "BOTTOM LEFT" :
                  orientation == ORIENTATION_LEFTTOP  ? "LEFT TOP" :
                  orientation == ORIENTATION_RIGHTTOP ? "RIGHT TOP" :
                  orientation == ORIENTATION_RIGHTBOT ? "RIGHT BOTTOM" :
                  orientation == ORIENTATION_LEFTBOT  ? "LEFT BOTTOM" : "UNKNOWN");
  }
  // Check for valid row number
  if (row >= height) {
    asfPrintError("Invalid row number (%d) found.  Valid range is 0 through %d\n",
                  row, height - 1);
  }

  // Reading a contiguous RGB strip results in a strip (of several rows) with rgb data
  // in each row, but reading a strip from a file with separate color planes results in
  // a strip with just the one color in each strip (and row)
  strip     = TIFFComputeStrip(tif, row, band);
  strip_row = row - (strip * t.rowsPerStrip);
  tsize_t stripSize = TIFFStripSize(tif);
  uint32 bytes_per_sample = (bits_per_sample / 8);

  // This returns a decoded strip which contains 1 or more rows.  The index calculated
  // below needs to take the row into account ...the strip_row is the row within a strip
  // assuming the first row in a strip is '0'.
  tsize_t bytes_read = TIFFReadEncodedStrip(tif, strip, sbuf, (tsize_t) -1);
  if (read_count &&
      bytes_read > 0)
  {
    uint32 col;
    uint32 idx = 0;
    for (col = 0; col < width && (idx * bytes_per_sample) < stripSize; col++) {
      // NOTE: t.scanlineSize is in bytes (not pixels)
      if (planar_config == PLANARCONFIG_SEPARATE) {
        idx = strip_row * (t.scanlineSize / bytes_per_sample) + col*samples_per_pixel;
      }
      else {
        // PLANARCONFIG_CONTIG
        idx = strip_row * (t.scanlineSize / bytes_per_sample) + col*samples_per_pixel + band;
      }
      if (idx * bytes_per_sample >= stripSize)
        continue; // Prevents over-run if last strip or scanline (within a strip) is not complete
      switch (bits_per_sample) {
        case 8:
          switch (sample_format) {
            case SAMPLEFORMAT_UINT:
              ((uint8*)buf)[col] = (uint8)(((uint8*)sbuf)[idx]);
              break;
            case SAMPLEFORMAT_INT:
              ((int8*)buf)[col] = (int8)(((int8*)sbuf)[idx]);
              break;
            default:
              asfPrintError("Unexpected data type in TIFF file\n");
              break;
          }
          break;
        case 16:
          switch (sample_format) {
            case SAMPLEFORMAT_UINT:
              ((uint16*)buf)[col] = (uint16)(((uint16*)sbuf)[idx]);
              break;
            case SAMPLEFORMAT_INT:
              ((int16*)buf)[col] = (int16)(((int16*)sbuf)[idx]);
              break;
            default:
              asfPrintError("Unexpected data type in TIFF file\n");
              break;
          }
          break;
        case 32:
          switch (sample_format) {
            case SAMPLEFORMAT_UINT:
              ((uint32*)buf)[col] = (uint32)(((uint32*)sbuf)[idx]);
              break;
            case SAMPLEFORMAT_INT:
              ((long*)buf)[col] = (long)(((long*)sbuf)[idx]);
              break;
            case SAMPLEFORMAT_COMPLEXINT:
              ((uint32*)buf)[col] = (uint32)(((uint32*)sbuf)[idx]);
              break;
            case SAMPLEFORMAT_IEEEFP:
              ((float*)buf)[col] = (float)(((float*)sbuf)[idx]);
              break;
            default:
              asfPrintError("Unexpected data type in TIFF file\n");
              break;
          }
          break;
        default:
          asfPrintError("Usupported bits per sample found in TIFF file\n");
          break;
      }
    }
  }

  if (sbuf)
    _TIFFfree(sbuf);
}

void ReadScanline_from_TIFF_TileRow(TIFF *tif, tdata_t buf, unsigned long row, int band)
{
  int read_count;
  tiff_type_t t;
  tdata_t tbuf=NULL;

  if (tif == NULL) {
    asfPrintError("TIFF file not open for read\n");
  }

  get_tiff_type(tif, &t);
  if (t.format != TILED_TIFF) {
    asfPrintError("Programmer error: ReadScanline_from_TIFF_TileRow() called when the TIFF file\n"
        "was not a tiled TIFF.\n");
  }
  tsize_t tileSize = TIFFTileSize(tif);
  if (tileSize > 0) {
    tbuf = _TIFFmalloc(tileSize);
    if (tbuf == NULL) {
      asfPrintError("Unable to allocate tiled TIFF scanline buffer\n");
    }
  }
  else {
    asfPrintError("Invalid TIFF tile size in tiled TIFF.\n");
  }

  short planar_config;    // TIFFTAG_PLANARCONFIG
  read_count = TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planar_config);
  if (read_count < 1) {
    asfPrintError("Cannot determine planar configuration from TIFF file.\n");
  }

  short samples_per_pixel;
  read_count = TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samples_per_pixel); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the number of samples per pixel from TIFF file.\n");
  }
  if (band < 0 || band > samples_per_pixel - 1) {
    asfPrintError("Invalid band number (%d).  Band number should range from %d to %d.\n",
                  0, samples_per_pixel - 1);
  }
  uint32 height;
  read_count = TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the number of lines from TIFF file.\n");
  }
  uint32 width;
  read_count = TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the number of pixels per line from TIFF file.\n");
  }
  short bits_per_sample;
  read_count = TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bits_per_sample); // Number of bands
  if (read_count < 1) {
      asfPrintError("Could not read the bits per sample from TIFF file.\n");
  }
  short sample_format;
  read_count = TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sample_format); // Number of bands
  if (read_count < 1) {
      switch(bits_per_sample) {
          case 8:
              sample_format = SAMPLEFORMAT_UINT;
              break;
          case 16:
              sample_format = SAMPLEFORMAT_INT;
              break;
          case 32:
              sample_format = SAMPLEFORMAT_IEEEFP;
              break;
          default:
              asfPrintError("Could not read the sample format (data type) from TIFF file.\n");
              break;
      }
  }
  short orientation;
  read_count = TIFFGetField(tif, TIFFTAG_ORIENTATION, &orientation); // top-left, left-top, bot-right, etc
  if (read_count < 1) {
    orientation = ORIENTATION_TOPLEFT;
  }
  if (read_count && orientation != ORIENTATION_TOPLEFT) {
    asfPrintError("Unsupported orientation found (%s)\n",
                  orientation == ORIENTATION_TOPRIGHT ? "TOP RIGHT" :
                  orientation == ORIENTATION_BOTRIGHT ? "BOTTOM RIGHT" :
                  orientation == ORIENTATION_BOTLEFT  ? "BOTTOM LEFT" :
                  orientation == ORIENTATION_LEFTTOP  ? "LEFT TOP" :
                  orientation == ORIENTATION_RIGHTTOP ? "RIGHT TOP" :
                  orientation == ORIENTATION_RIGHTBOT ? "RIGHT BOTTOM" :
                  orientation == ORIENTATION_LEFTBOT  ? "LEFT BOTTOM" : "UNKNOWN");
  }
  // Check for valid row number
  if (row >= height) {
    asfPrintError("Invalid row number (%d) found.  Valid range is 0 through %d\n",
                  row, height - 1);
  }
  // Develop a buffer with a line of data from a single band in it
//  ttile_t tile;
  uint32 bytes_per_sample = bits_per_sample / 8;
  uint32 row_in_tile;

  if (width > 0 &&
      height > 0 &&
      samples_per_pixel > 0 &&
      bits_per_sample % 8 == 0 &&
      t.tileWidth > 0 &&
      t.tileLength > 0)
  {
    uint32 tile_col;
    uint32 buf_col;
    uint32 bytes_read;
    for (tile_col = 0, buf_col = 0;
         tile_col < width;
         tile_col += t.tileWidth)
    {
      // NOTE:  t.tileLength and t.tileWidth are in pixels (not bytes)
      // NOTE:  TIFFReadTile() is a wrapper over TIFFComputeTile() and
      //        TIFFReadEncodedTile() ...in other words, it automatically
      //        takes into account whether the file has contigious (interlaced)
      //        color bands or separate color planes, and automagically
      //        decompresses the tile during the read.  The return below,
      //        is an uncompressed tile in raster format (row-order 2D array
      //        in memory.)
      bytes_read = TIFFReadTile(tif, tbuf, tile_col, row, 0, band);
      if (bytes_read <= 0) {
        //asfPrintWarning("No data\n");
      }
      uint32 num_preceding_tile_rows = floor(row / t.tileLength);
      row_in_tile = row - (num_preceding_tile_rows * t.tileLength);
      uint32 i;
      uint32 idx = 0;
      for (i = 0; i < t.tileWidth && buf_col < width && (idx * bytes_per_sample) < tileSize; i++) {
        if (planar_config == PLANARCONFIG_SEPARATE) {
          idx = row_in_tile * t.tileWidth + i;
        }
        else {
          // PLANARCONFIG_CONTIG
          idx = row_in_tile * (t.tileWidth * samples_per_pixel) + i * samples_per_pixel + band;
        }
        switch (bits_per_sample) {
          case 8:
            switch (sample_format) {
              case SAMPLEFORMAT_UINT:
                ((uint8*)buf)[buf_col] = ((uint8*)tbuf)[idx];
                buf_col++;
                break;
              case SAMPLEFORMAT_INT:
                ((int8*)buf)[buf_col] = ((int8*)tbuf)[idx];
                buf_col++;
                break;
              default:
                asfPrintError("Unexpected data type in TIFF file\n");
                break;
            }
            break;
          case 16:
            switch (sample_format) {
              case SAMPLEFORMAT_UINT:
                ((uint16*)buf)[buf_col] = ((uint16*)tbuf)[idx];
                buf_col++;
                break;
              case SAMPLEFORMAT_INT:
                ((int16*)buf)[buf_col] = ((int16*)tbuf)[idx];
                buf_col++;
                break;
              default:
                asfPrintError("Unexpected data type in TIFF file\n");
                break;
            }
            break;
          case 32:
            switch (sample_format) {
              case SAMPLEFORMAT_UINT:
                ((uint32*)buf)[buf_col] = ((uint32*)tbuf)[idx];
                buf_col++;
                break;
              case SAMPLEFORMAT_INT:
                ((int32*)buf)[buf_col] = ((int32*)tbuf)[idx];
                buf_col++;
                break;
              case SAMPLEFORMAT_IEEEFP:
                ((float*)buf)[buf_col] = ((float*)tbuf)[idx];
                buf_col++;
                break;
              default:
                asfPrintError("Unexpected data type in TIFF file\n");
                break;
            }
            break;
          default:
            asfPrintError("Usupported bits per sample found in TIFF file\n");
            break;
        }
      }
    }
  }

  if (tbuf) _TIFFfree(tbuf);
}

int check_for_vintage_asf_utm_geotiff(const char *citation, int *geotiff_data_exists,
                                      short *model_type, short *raster_type, short *linear_units)
{
  int ret=0;
  int zone=0, is_utm=0;
  datum_type_t datum=UNKNOWN_DATUM;
  char hem='\0';

  if (citation && strstr(citation, "Alaska Satellite Facility")) {
    short pcs=0;
    is_utm = vintage_utm_citation_to_pcs(citation, &zone, &hem, &datum, &pcs);
  }
  if (is_utm &&
      zone >=1 && zone <= 60 &&
      (hem == 'N' || hem == 'S') &&
      datum == WGS84_DATUM)
  {
    *model_type = ModelTypeProjected;
    *raster_type = RasterPixelIsArea;
    *linear_units = Linear_Meter;
    *geotiff_data_exists = 1;
    ret = 3; // As though the three geokeys were read successfully
  }

  return ret;
}

// Copied from libasf_import:keys.c ...Didn't want to introduce a dependency on
// the export library or vice versa
static int UTM_2_PCS(short *pcs, datum_type_t datum, unsigned long zone, char hem)
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

// If the UTM description is in citation, then pick the data out and return it
int vintage_utm_citation_to_pcs(const char *citation, int *zone, char *hem, datum_type_t *datum, short *pcs)
{
  int is_utm=0;
  int found_zone=0, found_utm=0;

  *zone=0;
  *hem='\0';
  *datum=UNKNOWN_DATUM;
  *pcs=0;

  if (citation && strstr(citation, "Alaska Satellite Facility")) {
    char *s = STRDUP(citation);
    char *tokp;

    tokp = strtok(s, " ");
    do
    {
      if (strncmp(uc(tokp),"UTM",3) == 0) {
        found_utm = 1;
        found_zone=0;
      }
      else if (strncmp(uc(tokp),"ZONE",1) == 0) {
        if (*zone == 0) found_zone=1;
      }
      else if (found_zone && isdigit((int)*tokp)) {
        *zone = (int)strtol(tokp,(char**)NULL,10);
        found_zone=0;
      }
      else if (strlen(tokp) == 1 && (*(uc(tokp)) == 'N' || *(uc(tokp)) == 'S')) {
        *hem = *(uc(tokp)) == 'N' ? 'N' : 'S';
        found_zone=0;
      }
      else if (strncmp(uc(tokp),"WGS84",5) == 0) {
        *datum = WGS84_DATUM;
        found_zone=0;
      }
    } while (tokp && (tokp = strtok(NULL, " ")));

    if (s) FREE (s);
  }
  if (found_utm &&
      *zone >=1 && *zone <= 60 &&
      (*hem == 'N' || *hem == 'S') &&
      *datum == WGS84_DATUM)
  {
    is_utm=1;
    UTM_2_PCS(pcs, *datum, *zone, *hem);
  }
  else {
    is_utm = 0;
    *zone=0;
    *hem='\0';
    *datum=UNKNOWN_DATUM;
    *pcs=0;
  }

  return is_utm;
}

void classify_geotiff(GTIF *input_gtif,
                      short *model_type, short *raster_type, short *linear_units, short *angular_units,
                      int *geographic_geotiff, int *geocentric_geotiff, int *map_projected_geotiff,
                      int *geotiff_data_exists)
{
    int read_count, vintage_asf_utm;
    char *citation = NULL;
    int citation_length;
    int typeSize;
    tagtype_t citation_type;

    ////// Defaults //////
    *model_type = *raster_type = *linear_units = *angular_units = -1; // Invalid value
    *geographic_geotiff = *geocentric_geotiff = *map_projected_geotiff = *geotiff_data_exists = 0; // Fails

    ////////////////////////////////////////////////////////////////////////////////////////
    // Check for a vintage ASF type of geotiff (all projection info is in the citation, and the
    // normal projection geokeys are left unpopulated)
    citation_length = GTIFKeyInfo(input_gtif, GTCitationGeoKey, &typeSize, &citation_type);
    if (citation_length > 0) {
        citation = MALLOC ((citation_length) * typeSize);
        GTIFKeyGet (input_gtif, GTCitationGeoKey, citation, 0, citation_length);
    }
    else {
        citation_length = GTIFKeyInfo(input_gtif, PCSCitationGeoKey, &typeSize, &citation_type);
        if (citation_length > 0) {
            citation = MALLOC ((citation_length) * typeSize);
            GTIFKeyGet (input_gtif, PCSCitationGeoKey, citation, 0, citation_length);
        }
    }
    if (citation != NULL && strlen(citation) > 0) {
        vintage_asf_utm = check_for_vintage_asf_utm_geotiff(citation, geotiff_data_exists,
                model_type, raster_type, linear_units);
        if (vintage_asf_utm) {
            // Found a vintage ASF UTM geotiff
            *geographic_geotiff    = *geocentric_geotiff  = 0;
            *map_projected_geotiff = *geotiff_data_exists = 1;
            GTIFKeyGet (input_gtif, GTRasterTypeGeoKey, raster_type, 0, 0);
            return;
        }
    }
    FREE(citation);

    ////////////////////////////////////////////////////////////////////////////////////////
    // Check for other types of geotiffs...
    //
    // Read the basic (normally required) classification parameters ...bail if we hit any
    // unsupported types
    int m = 0, r = 0, l = 0, a = 0;
    m = GTIFKeyGet (input_gtif, GTModelTypeGeoKey, model_type, 0, 1);
    if (m && *model_type == ModelTypeGeocentric) {
        asfPrintError("Geocentric (x, y, z) GeoTIFFs are unsupported (so far.)\n");
    }
    if (m && *model_type == 32767 /* User Defined */) {
        asfPrintStatus("Geotiff Model Type is User Defined\n");
        l = GTIFKeyGet (input_gtif, ProjLinearUnitsGeoKey, linear_units, 0, 1);
        a = GTIFKeyGet (input_gtif, GeogAngularUnitsGeoKey, angular_units, 0, 1);
        if (l && !a) {
          asfPrintStatus("Guessing that this is projected data.\n\n");
          *model_type = ModelTypeProjected;
        }
        else if (a && !l) {
          asfPrintStatus("Guessing that this is geographic (lat/lon) data.\n\n");
          *model_type = ModelTypeGeographic;
        }
        else {
          asfPrintError("Unable to guess at the Geotiff Model Type.\n"
                        "Do not know how to handle this geotiff.\n");
        }
    }
    if (m && *model_type != ModelTypeProjected && *model_type != ModelTypeGeographic) {
        asfPrintError("Unrecognized type of GeoTIFF encountered.  Must be map-projected\n"
                "or geographic (lat/long)\n");
    }
    r = GTIFKeyGet (input_gtif, GTRasterTypeGeoKey, raster_type, 0, 0);
    /*
    if (r && *raster_type != RasterPixelIsArea) {
        asfPrintWarning("GeoTIFFs with 'point' type raster pixels are unsupported (so far.)\nContinuing, however geolocations may be off by up to a pixel.\n");
    }
    */
    if (m && *model_type == ModelTypeProjected) {
        l = GTIFKeyGet (input_gtif, ProjLinearUnitsGeoKey, linear_units, 0, 1);
    }
    if (m && *model_type == ModelTypeGeographic) {
        a = GTIFKeyGet (input_gtif, GeogAngularUnitsGeoKey, angular_units, 0, 1);
    }
    if (a &&
        *angular_units != Angular_Arc_Second    &&
        *angular_units != Angular_Degree)
    {
        // Temporarily choose not to support arcsec geotiffs ...needs more testing
        asfPrintError("Found a Geographic (lat/lon) GeoTIFF with an unsupported type of angular\n"
                "units (%s) in it.\n", angular_units_to_string(*angular_units));
    }
    if (l && (*linear_units != Linear_Meter &&
	      *linear_units != Linear_Foot &&
	      *linear_units != Linear_Foot_US_Survey &&
	      *linear_units != Linear_Foot_Modified_American &&
	      *linear_units != Linear_Foot_Clarke &&
	      *linear_units != Linear_Foot_Indian)) {
        // Linear units was populated but wasn't a supported type...
        asfPrintError("Found a map-projected GeoTIFF with an unsupported type of linear\n"
                "units (%s) in it.\n", linear_units_to_string(*linear_units));
    }
    read_count = m + r + l + a;

    //////////////////////////////////////////////////////////////////////////////////////////
    // Attempt to classify the geotiff as a geographic, geocentric, or map-projected geotiff
    // and try to fill in missing information if necessary

    // Case: 3 valid keys found
    if (read_count == 3) {
        // Check for map-projected geotiff
        if (m && *model_type == ModelTypeProjected)
        {
            ////////
            // GeoTIFF is map-projected
            if (!r ||
                (r &&
                 *raster_type != RasterPixelIsArea &&
                 *raster_type != RasterPixelIsPoint))
            {
                asfPrintWarning("Invalid raster type found.\n"
                                "Guessing RasterPixelIsArea and continuing...\n");
                r = 1;
                *raster_type = RasterPixelIsArea;
            }
            if (r &&
                *raster_type != RasterPixelIsArea)
            {
                asfPrintWarning("Only map-projected GeoTIFFs with pixels that represent area are supported.");
            }
            if (a && !l) {
                asfPrintWarning("Invalid map-projected GeoTIFF found ...angular units set to %s and\n"
                        "linear units were not set.  Guessing Linear_Meter units and continuing...\n",
                        angular_units_to_string(*angular_units));
                l = 1;
                *linear_units = Linear_Meter;
                a = 0;
                *angular_units = -1;
            }
            if (l && (*linear_units == Linear_Meter ||
		      *linear_units == Linear_Foot ||
		      *linear_units == Linear_Foot_US_Survey ||
		      *linear_units == Linear_Foot_Modified_American ||
		      *linear_units == Linear_Foot_Clarke ||
		      *linear_units == Linear_Foot_Indian))
            {
                *geographic_geotiff    = *geocentric_geotiff  = 0;
                *map_projected_geotiff = *geotiff_data_exists = 1;
                return;
            }
            else {
                asfPrintError("Only map-projected GeoTIFFs with linear meters or with a linear foot unit are supported.\n");
            }
        }
        else if (m && *model_type == ModelTypeGeographic) {
            ////////
            // GeoTIFF is geographic (lat/long, degrees or arc-seconds (typ))
            // Ignore *raster_type ...it might be set to 'area', but that would be meaningless
            // *raster_type has no meaning in a lat/long GeoTIFF
            if (l && !a) {
                asfPrintWarning("Invalid Geographic (lat/lon) GeoTIFF found ...linear units set to %s and\n"
                        "angular units were not set.  Guessing Angular_Degree units and continuing...\n",
                        linear_units_to_string(*linear_units));
                a = 1;
                *angular_units = Angular_Degree;
                l = 0;
                *linear_units = -1;
            }
            if (a &&
                (*angular_units == Angular_Degree ||
                 *angular_units == Angular_Arc_Second))
            {
                *geographic_geotiff    = *geotiff_data_exists = 1;
                *map_projected_geotiff = *geocentric_geotiff  = 0;
                return;
            }
            else {
                asfPrintError("Found Geographic GeoTIFF with invalid or unsupported angular units (%s)\n"
                        "Only geographic GeoTIFFs with angular degrees are supported.\n",
                        angular_units_to_string(*angular_units));
            }
        }
        else {
            // Should not get here
            asfPrintError("Invalid or unsupported model type\n");
        }
    }
    // Case: 2 valid keys found, 1 key missing
    else if (read_count == 2) {
        // Only found 2 of 3 necessary parameters ...let's try to guess the 3rd
        if (*model_type != ModelTypeProjected  && *model_type != ModelTypeGeographic)
        {
            // The model type is unknown, raster_type and linear_units are both known and
            // valid for their types
            if (*raster_type == RasterPixelIsArea && *linear_units == Linear_Meter) {
                // Guess map-projected
                asfPrintWarning("Missing model type definition in GeoTIFF.  GeoTIFF contains area-type\n"
                        "pixels and linear meters ...guessing the GeoTIFF is map-projected and\n"
                        "attempting to continue...\n");
                *model_type = ModelTypeProjected;
                *geographic_geotiff    = *geocentric_geotiff  = 0;
                *map_projected_geotiff = *geotiff_data_exists = 1;
                return;
            }
            else if (*angular_units == Angular_Degree || *angular_units == Angular_Arc_Second) {
                // Guess geographic
                asfPrintWarning("Missing model type definition in GeoTIFF.  GeoTIFF contains angular\n"
                        "units ...guessing the GeoTIFF is geographic (lat/long) and\n"
                        "attempting to continue...\n");
                *model_type = ModelTypeGeographic;
                *geographic_geotiff    = *geotiff_data_exists = 1;
                *map_projected_geotiff = *geocentric_geotiff  = 0;
                return;
            }
            else {
                asfPrintError("Found unsupported type of GeoTIFF or a GeoTIFF with too many missing keys.\n");
            }
        } // End of guessing because the ModelType was unknown - Check unknown raster_type case
        else if (*raster_type != RasterPixelIsArea && *raster_type != RasterPixelIsPoint) {
            // Raster type is missing ...let's take a guess.  Model type and linear
            // units are both known and valid for their types
            if (*model_type == ModelTypeProjected) {
                if (*linear_units != Linear_Meter) {
                    asfPrintError("Only meters are supported for map-projected GeoTIFFs\n");
                }
                // Guess pixel type is area
                asfPrintWarning("Missing raster type in GeoTIFF, but since the GeoTIFF is map-projected,\n"
                        "guessing RasterPixelIsArea and attempting to continue...\n");
                *raster_type = RasterPixelIsArea;
                *geographic_geotiff    = *geocentric_geotiff  = 0;
                *map_projected_geotiff = *geotiff_data_exists = 1;
                return;
            }
            else if (*model_type == ModelTypeGeographic) {
                // Guess pixel type is area
                if (*angular_units != Angular_Degree && *angular_units != Angular_Arc_Second) {
                    asfPrintError("Only angular degrees are supported for geographic GeoTIFFs\n");
                }
                *geographic_geotiff    = *geotiff_data_exists = 1;
                *map_projected_geotiff = *geocentric_geotiff  = 0;
                return;
            }
            else {
                asfPrintError("Found geocentric (x, y, z) type of GeoTIFF ...currently unsupported.\n");
            }
        } // End of guessing because the RasterType was unknown
        else if (*linear_units  != Linear_Meter   &&
                 *angular_units != Angular_Degree &&
                 *angular_units != Angular_Arc_Second)
        {
            // Pixel unit type is missing ...let's take a guess.  Model type and raster type are
            // known and valid for their types
            if (*model_type == ModelTypeProjected) {
	      /*
                if (*raster_type != RasterPixelIsArea) {
                    asfPrintError("Map projected GeoTIFFs with pixels that represent something\n"
                            "other than area (meters etc) are not supported.\n");
                }
	      */
                // Looks like a valid map projection.  Guess linear meters for the units
                asfPrintWarning("Missing linear units in GeoTIFF.  Guessing linear meters for the units and attempting\n"
                        "to continue...\n");
                *linear_units = Linear_Meter;
                *angular_units = -1;
                *geographic_geotiff    = *geocentric_geotiff  = 0;
                *map_projected_geotiff = *geotiff_data_exists = 1;
                return;
            }
            else if (*model_type == ModelTypeGeographic) {
                // Looks like a valid geographic (lat/long) geotiff
                asfPrintWarning("Found geographic type GeoTIFF with missing linear units setting.\n"
                        "Guessing angular degrees and attempting to continue...\n");
                *angular_units = Angular_Degree;
                *linear_units = -1;
                *geographic_geotiff    = *geotiff_data_exists = 1;
                *map_projected_geotiff = *geocentric_geotiff  = 0;
                return;
            }
            else {
                asfPrintError("Found geocentric (x, y, z) GeoTIFF... Geographic GeoTIFFs are\n"
                        "unsupported at this time.\n");
            }
        }
    }
    // Case: 1 valid key found, 2 keys missing
    else if (read_count == 1) {
        // Only found 1 of 3 necessary parameters ...let's try to guess the other 2 (dangerous ground!)
        if (*model_type == ModelTypeProjected) {
            // Only the model type is known ...guess the rest
            asfPrintWarning("Both the raster type and linear units is missing in the GeoTIFF.  The model\n"
                    "type is map-projected, so guessing that the raster type is RasterPixelIsArea and\n"
                    "that the linear units are in meters ...attempting to continue\n");
            *raster_type = RasterPixelIsArea;
            *linear_units = Linear_Meter;
            *angular_units = -1;
            *geographic_geotiff    = *geocentric_geotiff  = 0;
            *map_projected_geotiff = *geotiff_data_exists = 1;
            return;
        }
        else if (*model_type == ModelTypeGeographic) {
            // Only the model type is known ...guess the rest
            asfPrintWarning("Both the raster type and linear units is missing in the GeoTIFF.  The model\n"
                    "type is geographic (lat/long), so guessing that the angular units are in decimal\n"
                    "degrees ...attempting to continue\n");
            *angular_units = Angular_Degree;
            *linear_units = -1;
            *geographic_geotiff    = *geotiff_data_exists = 1;
            *map_projected_geotiff = *geocentric_geotiff  = 0;
            return;
        }
        else if (*model_type == ModelTypeGeocentric) {
            asfPrintError("Geocentric (x, y, z) GeoTIFFs are not supported (yet.)\n");
        }
        else if (*raster_type == RasterPixelIsArea) {
            // Only the raster type is known ...guess the rest
            asfPrintWarning("Both the model type and linear units is missing in the GeoTIFF.  The raster\n"
                    "type is RasterPixelIsArea, so guessing that the model type is map-projected and\n"
                    "that the linear units are in meters ...attempting to continue\n");
            *model_type = ModelTypeProjected;
            *linear_units = Linear_Meter;
            *angular_units = -1;
            *geographic_geotiff    = *geocentric_geotiff  = 0;
            *map_projected_geotiff = *geotiff_data_exists = 1;
            return;
        }
        else if (*raster_type == RasterPixelIsPoint) {
            // Only the raster type is known, but cannot guess the rest... bail.
            asfPrintError("Found invalid or unsupported GeoTIFF.  Raster type is 'point' rather than\n"
                    "area.  The model type (map projected, geographic, geocentric) is unknown.\n"
                    "And the linear units are unknown.  Cannot guess what type of GeoTIFF this\n"
                    "is.  Aborting.\n");
        }
        else if (*linear_units == Linear_Meter) {
            // Only linear units is known and it's meters.  Guess map projected and pixels are
            // area pixels.
            asfPrintWarning("Found GeoTIFF with undefined model and raster type.  Linear units\n"
                    "is defined to be meters.  Guessing that the GeoTIFF is map-projected and\n"
                    "that pixels represent area.  Attempting to continue...\n");
            *model_type = ModelTypeProjected;
            *raster_type = RasterPixelIsArea;
            *geographic_geotiff    = *geocentric_geotiff  = 0;
            *map_projected_geotiff = *geotiff_data_exists = 1;
            return;
        }
        else if (*angular_units == Angular_Degree) {
            // Only linear units is known and it's angular degrees.  Guess geographic and pixels
            // type is 'who cares'
            asfPrintWarning("Found GeoTIFF with undefined model and raster type.  Linear units\n"
                    "is defined to be angular degrees.  Guessing that the GeoTIFF is geographic.\n"
                    "Attempting to continue...\n");
            *model_type = ModelTypeGeographic;
            *geographic_geotiff    = *geotiff_data_exists = 1;
            *map_projected_geotiff = *geocentric_geotiff  = 0;
            return;
        }
        else if (*angular_units == Angular_Arc_Second) {
            // Only linear units is known and it's angular degrees.  Guess geographic and pixels
            // type is 'who cares'
            asfPrintWarning("Found GeoTIFF with undefined model and raster type.  Linear units\n"
                    "is defined to be angular degrees.  Guessing that the GeoTIFF is geographic.\n"
                    "Attempting to continue...\n");
            *model_type = ModelTypeGeographic;
            *linear_units = -1;
            *geographic_geotiff    = *geotiff_data_exists = 1;
            *map_projected_geotiff = *geocentric_geotiff  = 0;
            return;
        }
        else {
            asfPrintError("Found unsupported or invalid GeoTIFF.  Model type and raster type\n"
                    "is undefined, and linear units are either undefined or of an unsupported\n"
                    "type.  Aborting...\n");
        }
    }
    // Case: No valid keys found
    else {
        // All classification parameters are missing!
        *geographic_geotiff    = *geocentric_geotiff  = 0;
        *map_projected_geotiff = *geotiff_data_exists = 0;
        return;
    }
}


int check_for_datum_in_string(const char *citation, datum_type_t *datum)
{
    int ret = 0; // not found
    return ret;
}

int check_for_ellipse_definition_in_geotiff(GTIF *input_gtif, spheroid_type_t *spheroid)
{
    int ret = 0; // failure
    return ret;
}

char *angular_units_to_string(short angular_units)
{
    return
        (angular_units == Angular_Radian)            ? "Angular_Radian"          :
        (angular_units == Angular_Degree)            ? "Angular_Degree"          :
        (angular_units == Angular_Arc_Minute)        ? "Angular_Arc_Minute"      :
        (angular_units == Angular_Arc_Second)        ? "Angular_Arc_Second"      :
        (angular_units == Angular_Grad)              ? "Angular_Grad"            :
        (angular_units == Angular_Gon)               ? "Angular_Gon"             :
        (angular_units == Angular_DMS)               ? "Angular_DMS"             :
        (angular_units == Angular_DMS_Hemisphere)    ? "Angular_DMS_Hemisphere"  :
                                                        "Unrecognized unit";
}

char *linear_units_to_string(short linear_units)
{
    return
        (linear_units == Linear_Foot)                        ? "Linear_Foot"                         :
        (linear_units == Linear_Foot_US_Survey)              ? "Linear_Foot_US_Survey"               :
        (linear_units == Linear_Foot_Modified_American)      ? "Linear_Foot_Modified_American"       :
        (linear_units == Linear_Foot_Clarke)                 ? "Linear_Foot_Clarke"                  :
        (linear_units == Linear_Foot_Indian)                 ? "Linear_Foot_Indian"                  :
        (linear_units == Linear_Link)                        ? "Linear_Link"                         :
        (linear_units == Linear_Link_Benoit)                 ? "Linear_Link_Benoit"                  :
        (linear_units == Linear_Link_Sears)                  ? "Linear_Link_Sears"                   :
        (linear_units == Linear_Chain_Benoit)                ? "Linear_Chain_Benoit"                 :
        (linear_units == Linear_Chain_Sears)                 ? "Linear_Chain_Sears"                  :
        (linear_units == Linear_Yard_Sears)                  ? "Linear_Yard_Sears"                   :
        (linear_units == Linear_Yard_Indian)                 ? "Linear_Yard_Indian"                  :
        (linear_units == Linear_Fathom)                      ? "Linear_Fathom"                       :
        (linear_units == Linear_Mile_International_Nautical) ? "Linear_Mile_International_Nautical"  :
                                                                "Unrecognized unit";
}

void get_look_up_table_name(char *citation, char **look_up_table)
{
    *look_up_table = (char *)MALLOC(256 * sizeof(char));
    strcpy(*look_up_table, "UNKNOWN");
}
