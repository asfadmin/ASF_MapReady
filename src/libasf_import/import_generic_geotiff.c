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

#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include <gsl/gsl_math.h>

#include <float_image.h>
#include <uint8_image.h>
#include <spheroids.h>
#include <proj.h>
#include <libasf_proj.h>

#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"

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
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, IMPORT_GENERIC_FLOAT_MICRON))
#define FLOAT_TOLERANCE 0.00001

#define DEFAULT_UTM_SCALE_FACTOR     0.9996

#define DEFAULT_SCALE_FACTOR         1.0
#define UNKNOWN_PROJECTION_TYPE       -1

#define NAD27_DATUM_STR   "NAD27"
#define NAD83_DATUM_STR   "NAD83"
#define HARN_DATUM_STR    "HARN"
#define WGS84_DATUM_STR   "WGS 84"
#define HUGHES_DATUM_STR  "HUGHES"

#define USER_DEFINED_PCS             32767
#define USER_DEFINED_KEY             32767
#define BAND_NAME_LENGTH  12

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
int geotiff_band_image_write(TIFF *tif, meta_parameters *omd,
                             const char *outBaseName, int num_bands,
                             int *ignore, short bits_per_sample,
                             short sample_format, short planar_config);

// Import an ERDAS ArcGIS GeoTIFF (a projected GeoTIFF flavor), including
// projection data from its metadata file (ERDAS MIF HFA .aux file) into
// our own ASF Tools format (.img, .meta)
//
void import_generic_geotiff (const char *inFileName, const char *outBaseName, ...)
{
  // Counts holding the size of the returns from gt_methods.get method
  // calls.  The geo_keyp.h header has the interface specification for
  // this method.  gt_methods.get doesn't seem to be documented as
  // part of the public GeoTIFF API, however, it works, unlike the
  // TIFFGetField call, which seg faults.  Maybe I'm missing some
  // setup call that I need, but: a. I can't find anything in the
  // incomplete API documentation telling me what that might be, and
  // b. I'm using a sequence of calls analogous to that use in
  // export_as_geotiff, which works, and c. suspiciously, the listgeo
  // program that comes with libgeotiff also uses this gt_methods.get
  // approach and doesn't use TIFFGetField at all so far as I can
  // tell.
  int geotiff_data_exists;
  char *bands[MAX_BANDS]; // list of band IDs
  int num_bands;
  int band_num = 0;
  int count;
  int read_count;
  int ret;
  short model_type=-1;
  short raster_type=-1;
  short linear_units=-1;
  double scale_factor;
  TIFF *input_tiff;
  GTIF *input_gtif;
  meta_parameters *meta_out;
  data_type_t data_type;
  datum_type_t datum;
  va_list ap;

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
  // Don't set any of the deprecated structure elements.
  meta_out->stVec = NULL;
  meta_out->geo = NULL;
  meta_out->ifm = NULL;
  meta_out->info = NULL;
  datum = UNKNOWN_DATUM;

  // Set up convenience pointers
  meta_general *mg = meta_out->general;
  meta_projection *mp = meta_out->projection;
  meta_sar *msar = meta_out->sar;
//  meta_stats *ms = meta_out->stats; // Convenience alias.
  meta_location *ml = meta_out->location; // Convenience alias.

  // Init
  mp->spheroid = UNKNOWN_SPHEROID; // meta_projection_init() 'should' initialize this, but doesn't

  // Open the input tiff file.
  input_tiff = XTIFFOpen (inFileName, "r");
  asfRequire (input_tiff != NULL, "Error opening input TIFF file.\n");

  // Open the structure that contains the geotiff keys.
  input_gtif = GTIFNew (input_tiff);
  asfRequire (input_gtif != NULL,
        "Error reading GeoTIFF keys from input TIFF file.\n");


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
  ret = get_tiff_data_config(input_tiff,
                             &sample_format, // TIFF type (uint, int, float)
                             &bits_per_sample, // 8, 16, or 32
                             &planar_config, // Contiguous (RGB or RGBA) or separate (band sequential, not interlaced)
                             &data_type, // ASF datatype, (BYTE, INTEGER16, INTEGER32, or REAL32 ...no complex
                             &num_bands, // Initial number of bands
                             &is_scanline_format);

  // FIXME: Modify the band stats functions and image writing functions to support separate bands instead
  // of requiring interlaced bands (which is really only good for up to 4 bands...)
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
      (sample_format == SAMPLEFORMAT_UINT) ? "Unsigned Integer" :
          (sample_format == SAMPLEFORMAT_INT) ? "Signed Integer" :
              (sample_format == SAMPLEFORMAT_IEEEFP) ? "Floating Point" : "Unknown or Unsupported");
  asfPrintStatus
      ("   Output data type: ASF format\n");

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

  // Get the tie point which defines the mapping between raster
  // coordinate space and geographic coordinate space.  Although
  // geotiff theoretically supports multiple tie points, we don't
  // (rationale: ArcView currently doesn't either, and multiple tie
  // points don't make sense with the pixel scale option, which we
  // need).
  // NOTE: Since neither ERDAS or ESRI store tie points in the .aux
  // file associated with their geotiffs, it is _required_ that they
  // are found in their tiff files.
  double *tie_point;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_TIEPOINTS, &count,
                               &tie_point);
  if (count != 6) {
    asfPrintError ("GeoTIFF file does not contain tie points\n");
  }
  // Get the scale factors which define the scale relationship between
  // raster pixels and geographic coordinate space.
  double *pixel_scale;
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
  // will be ModelTypeProjected.  We add the requirement that pixels
  // represent area and that the units are in meters because that's what
  // we support to date.  FIXME: For now, ignore other model types and ignore
  // lat/long (geographic) type geotiffs ...maybe add later.

  read_count
      = GTIFKeyGet (input_gtif, GTModelTypeGeoKey, &model_type, 0, 1);
  read_count
      += GTIFKeyGet (input_gtif, GTRasterTypeGeoKey, &raster_type, 0, 0);
  read_count
      += GTIFKeyGet (input_gtif, ProjLinearUnitsGeoKey, &linear_units, 0, 1);
  if (read_count == 3                   &&
      model_type == ModelTypeProjected  &&
      raster_type == RasterPixelIsArea  &&
      linear_units == Linear_Meter      )
  {
    // GeoTIFF appears to contain the projection parameters, but note that
    // (ProjectedCSTypeGeoKey must either be a UTM type) -or-
    // (ProjectedCSTypeGeoKey is not UTM and ProjCoordTransGeoKey is a supported
    // type).  See the if(geotiff_data_exists) section on reading parameters below.
    geotiff_data_exists = 1;
  }

  else {
    geotiff_data_exists = 0;
  }
  // Try to do some intelligent guess-work to make up for missing keys...
  if (model_type != ModelTypeProjected &&
      model_type != ModelTypeGeographic &&
      model_type != ModelTypeGeocentric)
  {
    // GTModelTypeGeoKey is unpopulated
    if (raster_type == RasterPixelIsArea &&
        linear_units == Linear_Meter)
    {
      asfPrintWarning("GeoTIFF contains linear meters but GTModelTypeGeoKey is\n"
          "unpopulated ...can't tell if this is a map-projected GeoTIFF or only\n"
          "a georeferenced GeoTIFF.\n"
          "  Continuing ...but assuming that it is a map-projected GeoTIFF until\n"
          "  reading more information from the file proves otherwise.\n");
      model_type = ModelTypeProjected;
      geotiff_data_exists = 1;
    }
  }
  if (model_type == ModelTypeProjected &&
      raster_type == RasterPixelIsArea &&
      linear_units != Linear_Meter) {
    read_count
        = GTIFKeyGet (input_gtif, ProjLinearUnitsGeoKey, &linear_units, 0, 1);
    if (read_count == 0) {
      asfPrintWarning("Map-Projected GeoTIFF found, but the linear units GeoKey is not set.\n"
          "  Continuing ...but assuming Linear Meters.\n");
      linear_units = Linear_Meter;
      geotiff_data_exists = 1;
    }
  }
  asfPrintStatus ("Input GeoTIFF key GTModelTypeGeoKey is %s\n",
                  (model_type == ModelTypeGeographic) ?
                      "ModelTypeGeographic" :
                      (model_type == ModelTypeGeocentric) ?
                      "ModelTypeGeocentric" :
                      (model_type == ModelTypeProjected) ?
                      "ModelTypeProjected" :
                      "Unknown");
  asfPrintStatus ("Input GeoTIFF key GTRasterTypeGeoKey is %s\n",
                  (raster_type == RasterPixelIsArea) ?
                      "RasterPixelIsArea" : "(Unsupported type)");
  asfPrintStatus ("Input GeoTIFF key ProjLinearUnitsGeoKey is %s\n",
                  (linear_units == Linear_Meter) ?
                      "Linear_Meters" : "(Unsupported type)");
  if (model_type != ModelTypeProjected ||
      raster_type != RasterPixelIsArea ||
      linear_units != Linear_Meter) {
    // FIXME: For now, we only import map-projected images in linear meters.  If
    // the image was a lat/long image, then the angular units would be set AND
    // the model_type would be ModelTypeGeographic ...but oh well.
    asfPrintError("Geographic (ModelTypeGeographic), linear units other than meters,\n"
                 "and raster types other than RasterPixelIsArea are not supported.\n");
  }
  else {
    geotiff_data_exists = 1;
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
  if (geotiff_data_exists) {
    char hemisphere;
    projection_type_t projection_type;
    unsigned long pro_zone; // UTM zone (UTM only)
    short proj_coords_trans = UNKNOWN_PROJECTION_TYPE;
    short pcs;
    short geokey_datum;
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
    mp->height = 0.0;
    if (linear_units == Linear_Meter) {
      strcpy(mp->units, "meters");
    }
    else {
      asfPrintError("Unsupported linear unit found in GeoTIFF.  Only meters is currently supported.\n");
    }

    ///////// STANDARD UTM (PCS CODE) //////////
    // Get datum and zone as appropriate
    read_count = GTIFKeyGet (input_gtif, ProjectedCSTypeGeoKey, &pcs, 0, 1);
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
      sprintf(msg,"UTM scale factor defaulting to %0.4lf\n", DEFAULT_UTM_SCALE_FACTOR);
      asfPrintStatus(msg);
      mp->param.utm.scale_factor = DEFAULT_UTM_SCALE_FACTOR;
    }
    ////////// ALL OTHER PROJECTION TYPES - INCLUDING GCS/USER-DEFINED UTMS /////////
    else {
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
        else {
          asfPrintError("Unsupported or unknown datum found in GeoTIFF file.\n");
        }
        char msg[256];
        sprintf(msg,"UTM scale factor defaulting to %0.4lf\n", DEFAULT_UTM_SCALE_FACTOR);
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
    //          asfPrintWarning(
    //                   "Unable to determine false easting from GeoTIFF file\n"
    //                   "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
    //          asfPrintWarning(
    //                   "Unable to determine false northing from GeoTIFF file\n"
    //                   "using ProjFalseNorthingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
            if (read_count != 1) {
    //          asfPrintWarning(
    //              "Unable to determine center longitude from GeoTIFF file\n"
    //              "using ProjNatOriginLongGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
            if (read_count != 1) {
    //          asfPrintWarning(
    //              "Unable to determine center latitude from GeoTIFF file\n"
    //              "using ProjNatOriginLatGeoKey\n");
            }
            else {
              latOrigin = 0.0;
            }
            read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
            if (read_count == 0) {
              scale_factor = DEFAULT_UTM_SCALE_FACTOR;

              char msg[256];
    //          sprintf(msg,
    //                  "UTM scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4lf\n",
    //                  scale_factor);
    //          asfPrintWarning(msg);
              sprintf(msg,"UTM scale factor defaulting to %0.4lf\n", scale_factor);
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
                      "Lambert Conformal Conic scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4lf\n",
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
                  "using ProjFalseEastingGeoKey\n");
            }
            read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
            if (read_count != 1) {
              asfPrintWarning(
                      "Unable to determine false northing from GeoTIFF file\n"
                  "using ProjFalseNorthingGeoKey\n");
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
  else {
    asfPrintWarning("Projection parameters missing in the GeoTIFF\n"
                "file.  Projection parameters may be incomplete unless.\n"
                "they are found in the associated .aux file (if it exists.)\n");
  }

  /***** CHECK TO SEE IF THIS IS AN ARCGIS GEOTIFF *****/
  /*                                                   */
  if (isArcgisGeotiff(inFileName)) {
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

  asfPrintStatus("\nConverting input TIFF image into %d-banded %s ASF-format image...\n\n",
                 num_bands, (data_type == BYTE) ? "8-bit byte" :
                                (data_type == INTEGER16) ? "16-bit integer" :
                                    (data_type == INTEGER32) ? "32-bit integer" :
                                        (data_type == REAL32) ? "32-bit float" : "unknown(?)");

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
  /*                               */
  mg->data_type = data_type;

  // Get the image data type from the variable arguments list
  char image_data_type[256];
  char *pTmpChar;
  va_start(ap, outBaseName); // outBaseName is the last argument before ", ..."
  pTmpChar = (char *)va_arg(ap, char *);
  if (pTmpChar != NULL) {
    strcpy(image_data_type, pTmpChar);
  }
  else {
    if (geotiff_data_exists) {
      strcpy(image_data_type, "GEOCODED_IMAGE");
    }
    else if ((tie_point[0] || tie_point[1]) &&
              pixel_scale[0] && pixel_scale[1]) {
      strcpy(image_data_type, "GEOREFERENCED_IMAGE");
    }
    else {
      strcpy(image_data_type, MAGIC_UNSET_STRING);
    }
  }
  va_end(ap);
  if (strncmp(image_data_type, "GEOCODED_IMAGE", 14) == 0) {
    mg->image_data_type = GEOCODED_IMAGE;
  }
  else if (strncmp(image_data_type, "GEOREFERENCED_IMAGE", 19) == 0) {
    mg->image_data_type = GEOREFERENCED_IMAGE;
  }
  else if (strncmp(image_data_type, "DEM", 3) == 0) {
    mg->image_data_type = DEM;
  }
  else if (strncmp(image_data_type, "MASK", 4) == 0) {
    mg->image_data_type = MASK;
  }
  // else leave it at the initialized value

  mg->line_count = height;
  mg->sample_count = width;

  mg->start_line = 0;
  mg->start_sample = 0;

  mg->x_pixel_size = pixel_scale[0];
  mg->y_pixel_size = pixel_scale[1];

  // For now we are going to insist that the meters per pixel in the
  // X and Y directions are identical(ish).  I believe asf_geocode at
  // least would work for non-square pixel dimensions, with the
  // caveats that output pixels would still be square, and would have
  // default size derived solely from the input pixel size
  if (fabs (mg->x_pixel_size - mg->y_pixel_size) > 0.0001) {
    char msg[256];
    sprintf(msg, "Pixel size is (x,y): (%lf, %lf)\n", mg->x_pixel_size, mg->y_pixel_size);
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
  // on what type of image data is in the file, e.g. map-projected or
  // geographic respectively (but we only support map-projected at this point.)
  double tp_lon = tie_point[3]; // x
  double tp_lat = tie_point[4]; // y, Note: [5] is zero for 2D space

  // Center latitude and longitude of image data (the following works for
  // linear or angular units since pixel sizes should be in the same units
  // as the tie points
  double center_x;
  double center_y;
  center_x = (width / 2.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
  center_y = (height / 2.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;

  // converts to center_latitude and center_longitude below...

  // If the datum and/or spheroid are unknown at this point, then fill
  // them out, and the major/minor axis, as best we can.
  if (datum != UNKNOWN_DATUM && mp->spheroid == UNKNOWN_SPHEROID) {
    // Guess the spheroid from the type of datum (a fairly safe guess)
    mp->spheroid = datum_spheroid(mp->datum);
    spheroid_axes_lengths (mp->spheroid, &mg->re_major, &mg->re_minor);
  }
  else if (datum == UNKNOWN_DATUM && mp->spheroid != UNKNOWN_SPHEROID) {
    // Can't guess a datum, so leave it be
    spheroid_axes_lengths (mp->spheroid, &mg->re_major, &mg->re_minor);
  }
  else if (datum == UNKNOWN_DATUM && mp->spheroid == UNKNOWN_SPHEROID) {
    // Bad news ...neither the datum nor the spheroid is known, so we must
    // spread the disease to the major and minor as well...
    mg->re_major = MAGIC_UNSET_DOUBLE;
    mg->re_minor = MAGIC_UNSET_DOUBLE;
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

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  // FIXME: When we start calculating stats again, then turn the bad data scan back ON
  // (Needs to use TIFFReadScanline() and offsets into a buf with [(jj*num_bands)+band_no]
  // to pick out individual floats to check)
#undef BAD_VALUE_SCAN_ON
#ifdef BAD_VALUE_SCAN_ON
  /***** FIX DEM IMAGE'S BAD DATA *****/
  /*                                  */
  // Since the import could be a DEM, and certain DEMs may have bad (way negative)
  // data values that make the statistics hopeless, saturate output, etc.,
  // map these bad values to a less negative magic number of our own making
  // that still lets things work somewhat (assumes the bad data values are rare).
  //
  // Note to you: We only support BYTE and REAL32 geotiff imports...
  //
  if (data_type == REAL32 && mg->image_data_type == DEM) {
    asfPrintStatus("\nScanning float image for bad data values...\n");
    int offset;
    char bad_values_existed = 0;
    const float bad_data_ceiling = -10e10;
    const float new_bad_data_magic_number = -999.0;
    size_t ii, jj;

    offset = oim->size_y;
    for ( ii = 0 ; ii < oim->size_y ; ii++ ) {
      asfPercentMeter((double)ii / (double)(mg->line_count));
      for ( jj = 0 ; jj < mg->sample_count ; jj++ ) {
        if ( float_image_get_pixel (oim, jj, ii) < bad_data_ceiling ) {
          float_image_set_pixel (oim, jj, ii, new_bad_data_magic_number);
          bad_values_existed = 1;
        }
      }
    }
    asfPercentMeter(1.0);
    if (bad_values_existed) {
      asfPrintWarning("Float image contained extra-negative values (< -10e10) that may\n"
          "result in inaccurate image statistics.\n");
      asfPrintStatus("Extra-negative values found within the float image have been removed\n");
    }
    if (buf) _TIFFfree(buf);
  }
#endif

  asfPrintStatus("\nGathering image statistics (per available band)...\n");
  int *ignore;
  meta_statistics *stats;
  double mask_value;
  stats = meta_statistics_init(MAX_BANDS);
  switch(meta_out->general->data_type) {
    case BYTE:
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
  ignore = CALLOC(MAX_BANDS, sizeof(int)); // Contains '1' if a band is to be ignored (empty band)
  int ii;
  for (ii=0; ii<num_bands; ii++) {
    int ret;
    ret = tiff_image_band_statistics(input_tiff, meta_out,
                                     &stats->band_stats[ii],
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

      ignore[ii] = 1;
    }
    else {
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

  // Fill out the number of bands and the band names
  strcpy(mg->bands, "");
  mg->band_count = num_bands;
  int *empty = (int*)CALLOC(num_bands, sizeof(int)); // Defaults to 'no empty bands'
  char *band_str;
  band_str = (char*)MALLOC(25*sizeof(char)); // '25' is the array length of mg->bands (see asf_meta.h) ...yes, I know.
  int num_found_bands;
  char *tmp_citation = (citation != NULL) ? STRDUP(citation) : NULL;
  get_bands_from_citation(&num_found_bands, &band_str, empty, tmp_citation);
  if (num_found_bands < 1) {
    asfPrintWarning("No ASF-exported band names found in GeoTIFF citation tag.\n"
       "Band names will be assigned in numerical order.\n");
  }
  if ( num_found_bands > 0 && strlen(band_str) > 0) {
    // If a valid list of bands were in the citation string, then let the empty[] array,
    // which indicates which bands in the TIFF were listed as 'empty' overrule the
    // ignore[] array since it was just a best-guess based on band statistics
    //
    // Note:  The ignore[] array will be used when writing the binary file so that empty
    // bands in the TIFF won't be written to the output file
    int band_no;
    for (band_no=0; band_no<num_bands; band_no++) {
      ignore[band_no] = empty[band_no];
    }

    // Note: mg->band_count is set to the number of found bands after the
    // binary file is written ...if you do it before, then put_band_float_line()
    // will fail.
    strcpy(mg->bands, band_str);
  }
  else {
    // Use the default band names if none were found in the citation string
    // Note: For the case where there is no list of band names
    // in the citation string, we are either importing somebody
    // else's geotiff, or we are importing one of our older ones.
    // The only way, in that case, to know if a band is empty is
    // to rely on the band statistics from above.  The results
    // of this analysis is stored in the 'ignore[<band_no>]' array.
    int band_no;

    // Note: num_bands is from the samples per pixel TIFF tag and is
    // the maximum number of valid (non-ignored) bands in the file
    for (band_no=0; band_no<num_bands-1; band_no++) {
      if (ignore[band_no]) {
        // Decrement the band count for each ignored band
        num_bands--;
      }
      else {
        // Band is not ignored, so give it a band name
        sprintf(mg->bands, "%s%s,", mg->bands, bands[band_no]);
      }
    }
    if (ignore[band_no]) {
      // Decrement the band count for each ignored band
      num_bands--;
    }
    else {
      // Band is not ignored, so give it a band name
      sprintf(mg->bands, "%s%s", mg->bands, bands[band_no]);
    }
    mg->band_count = num_bands;
  }
  if (mg->band_count <= 0 || strlen(mg->bands) <= 0) {
    asfPrintError("GeoTIFF file must contain at least one non-empty color channel (band)\n");
  }

  // Calculate the center latitude and longitude now that the projection
  // parameters are stored.
  double center_latitude;
  double center_longitude;
  double dummy_var;
  meta_projection proj;

  // Copy all fields just in case of future code rearrangements...
  copy_proj_parms (&proj, mp);
  proj_to_latlon(&proj,center_x, center_y, 0.0,
     &center_latitude, &center_longitude, &dummy_var);
  mg->center_latitude = R2D*center_latitude;
  mg->center_longitude = R2D*center_longitude;

  if (msar)
      msar->image_type = 'P'; // Map Projected

  // Set up the location block
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

  asfPrintStatus("\nWriting new '.meta' and '.img' files...\n");

  // Write the binary file
  ret = geotiff_band_image_write(input_tiff, meta_out, outBaseName, num_bands, ignore,
                                 bits_per_sample, sample_format, planar_config);
  // Write the Metadata file
  if ( num_found_bands > 0 && strlen(band_str) > 0) {
    mg->band_count = num_found_bands;
  }
  meta_write(meta_out, outBaseName);

  if (ret != 0) {
    asfPrintError("Unable to write binary image...\n");
  }

  // We're now done with the data and metadata.
  GTIFFree(input_gtif);
  XTIFFClose(input_tiff);
  meta_free (meta_out);
  FREE(stats);
  FREE(ignore);

  // We must be done with the citation string too :)
  FREE (tmp_citation);
  FREE (citation);
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

int get_bands_from_citation(int *num_bands, char **band_str, int *empty, char *citation)
{
  char *s;
  int band_no;
  *num_bands = 0;
  strcpy(*band_str, "");

  if (citation == NULL || strlen(citation) < 1) {
    s = NULL;
  }
  else {
    s = strstr(citation, BAND_ID_STRING);
  }
  if (s != NULL) {
    // Found a band ID string in the citation
    char *pcTmp, *pcTmp2;

    // Get the first band (token)
    band_no = 0;
    s += strlen(BAND_ID_STRING) + 2;
    if (s > citation + strlen(citation)) {
      return 0;
    }
    pcTmp = strtok_r(s, ",", &pcTmp2);
    if (pcTmp != NULL) {
      if (strncmp(uc(pcTmp), "EMPTY", 5) != 0) {
        *num_bands += 1;
        strcat(*band_str, pcTmp);
        empty[band_no] = 0;
      }
      else {
        empty[band_no] = 1;
      }
    }

    // Get subsequent bands (tokens)
    pcTmp = strtok_r(NULL, ",", &pcTmp2);
    while (pcTmp != NULL) {
      band_no++;
      if (strncmp(uc(pcTmp), "EMPTY", 5) != 0) {
        *num_bands += 1;
        sprintf(*band_str, "%s,%s", *band_str, pcTmp);
        empty[band_no] = 0;
      }
      else {
        empty[band_no] = 1;
      }
      pcTmp = strtok_r(NULL, ",", &pcTmp2);
    }
  }

  return *num_bands;
}

int tiff_image_band_statistics (TIFF *tif, meta_parameters *omd,
                                meta_stats *stats,
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

  for (band=0; band < num_bands; band++) {
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
//            if (planar_config == PLANARCONFIG_CONTIG || num_bands == 1) {
//              ReadScanline_from_TIFF_TileRow(tif, tif_buf, row, 0);
//            }
//            else {
            // Planar configuration is band-sequential
            ReadScanline_from_TIFF_TileRow(tif, tif_buf, row, band);
//            }
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
                  buf[col] = (float)(((uint8*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_INT:
                  buf[col] = (float)(((int8*)tif_buf)[col]);
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
                  buf[col] = (float)(((uint16*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_INT:
                  buf[col] = (float)(((int16*)tif_buf)[col]);
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
                  buf[col] = (float)(((uint32*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_INT:
                  buf[col] = (float)(((long*)tif_buf)[col]);
                  break;
                case SAMPLEFORMAT_IEEEFP:
                  buf[col] = (float)(((float*)tif_buf)[col]);
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
        put_band_float_line(fp, omd, band, (int)row, buf);
      }
    }
    else {
      asfPrintStatus("  Empty band found ...ignored\n");
    }
    FCLOSE(fp);
  }
  FREE(buf);
  FREE(outName);
  if (tif_buf) _TIFFfree(tif_buf);

  return 0;
}

void get_tiff_type(TIFF *tif, tiff_type_t *tiffInfo)
{
  int rc[64], ret;
  tiff_type_t *t=tiffInfo; // convenience ptr

  // Init to loser values
  t->format = 0; // Unknown type
  t->scanlineSize = 0;
  t->numStrips = 0;
  t->rowsPerStrip = 0;
  t->tileWidth = 0;
  t->tileLength = 0;
  t->imageCount = 0;
  t->volume_tiff = 0;

  // Count number of images stored in this TIFF
  do{
    t->imageCount++;
    ret = TIFFReadDirectory(tif);  // Makes next image directory the current directory until no more are left
  }while (ret);

  // Check to see if this TIFF has more than 2 dimensions
  uint32 imagedepth;
  rc[3] = TIFFGetField(tif, TIFFTAG_IMAGEDEPTH, &imagedepth);
  if (rc[3] && imagedepth > 1) {
    t->volume_tiff = 1;
  }

  // Read settings from TIFF
  if (t->imageCount > 0 && !t->volume_tiff) {
    tsize_t ss = TIFFScanlineSize(tif); // Length of a scanline in bytes (regardless of data type or rgb/gray)
    uint32 nstrips = TIFFNumberOfStrips(tif); // Number of strips in file
    uint32 rowsperstrip = 0;
    rc[0] = TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &rowsperstrip); // Rows per strip ...zero for scanline and tile files
    //tsize_t stripsize = TIFFStripSize(tif); // Equals lines * samples * samples_per_pixel for scanline images
    uint32 tile_width = 0, tile_length = 0;
    rc[1] = TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tile_width);
    rc[2] = TIFFGetField(tif, TIFFTAG_TILELENGTH, &tile_length);

    // Determine TIFF type
    if (rc[1] && rc[2] && tile_width > 1 && tile_length > 1 && TIFFIsTiled(tif)) {
      uint32 tiledepth;
      rc[4] = TIFFGetField(tif, TIFFTAG_TILEDEPTH, &tiledepth);
      if (rc[4] && tiledepth > 1) {
        t->volume_tiff = 1;
      }
      else if (tile_width % 16 == 0 && tile_length % 16 == 0) {
        t->format = TILED_TIFF;
        t->scanlineSize = ss;
        t->tileWidth = tile_width;
        t->tileLength = tile_length;
      }
    }
    else if (rc[0] && nstrips >= 1 && rowsperstrip >= 1) {
      t->format = STRIP_TIFF;
      t->scanlineSize = ss;
      t->numStrips = nstrips;
      t->rowsPerStrip = rowsperstrip;
    }
    else if (ss > 0) {
      t->format = SCANLINE_TIFF;
      t->scanlineSize = ss;
    }
  }
}

void ReadScanline_from_TIFF_Strip(TIFF *tif, tdata_t buf, unsigned long row, int band)
{
  int read_count;
  tiff_type_t t;
  tdata_t sbuf;
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
  read_count = TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the number of lines from TIFF file.\n");
  }
  uint32 width;
  read_count = TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the number of pixels per line from TIFF file.\n");
  }
  short sample_format;
  read_count = TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sample_format); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the sample format (data type) from TIFF file.\n");
  }
  short bits_per_sample;
  read_count = TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bits_per_sample); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the sample format (data type) from TIFF file.\n");
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
  if (row < 0 || row >= height) {
    asfPrintError("Invalid row number (%d) found.  Valid range is 0 through %d\n",
                  row, height - 1);
  }

  // Develop a buffer with a line of data from a single band in it
  strip     = TIFFComputeStrip(tif, row, band);
  strip_row = row - (strip * t.rowsPerStrip);
  tsize_t stripSize = TIFFStripSize(tif);
  uint32 bytes_per_sample = bits_per_sample / 8;

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
              ((uint8*)buf)[col] = ((uint8*)sbuf)[idx];
              break;
            case SAMPLEFORMAT_INT:
              ((int8*)buf)[col] = ((int8*)sbuf)[idx];
              break;
            default:
              asfPrintError("Unexpected data type in TIFF file\n");
              break;
          }
          break;
        case 16:
          switch (sample_format) {
            case SAMPLEFORMAT_UINT:
              ((uint16*)buf)[col] = ((uint16*)sbuf)[idx];
              break;
            case SAMPLEFORMAT_INT:
              ((int16*)buf)[col] = ((int16*)sbuf)[idx];
              break;
            default:
              asfPrintError("Unexpected data type in TIFF file\n");
              break;
          }
          break;
        case 32:
          switch (sample_format) {
            case SAMPLEFORMAT_UINT:
              ((uint32*)buf)[col] = ((uint32*)sbuf)[idx];
              break;
            case SAMPLEFORMAT_INT:
              ((int32*)buf)[col] = ((int32*)sbuf)[idx];
              break;
            case SAMPLEFORMAT_IEEEFP:
              ((float*)buf)[col] = ((float*)sbuf)[idx];
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
  short sample_format;
  read_count = TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sample_format); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the sample format (data type) from TIFF file.\n");
  }
  short bits_per_sample;
  read_count = TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bits_per_sample); // Number of bands
  if (read_count < 1) {
    asfPrintError("Could not read the sample format (data type) from TIFF file.\n");
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
  if (row < 0 || row >= height) {
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




























