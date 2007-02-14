// Import a generic GeoTIFF (a projected GeoTIFF flavor) into
// our own ASF Tools format (.img, .meta)
//
// NOTE:
// 1. At this time, only supports Albers Equal Area Conic, Lambert Azimuthal
//    Equal Area, Lambert Conformal Conic, Polar Stereographic, and UTM
// 2. Some GeoTIFF files do not store the projection paremeters in geokeys
//    as is usual, but store the information in the (type) double and (type)
//    ASCII arrays and/or one of two citation strings.  This import tool
//    currently only supports projection data stored in the standard geokeys
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

#include <float_image.h>
#include <spheroids.h>
#include <proj.h>
#include <libasf_proj.h>

#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"

#include "tiff_to_float_image.h"
#include "write_meta_and_img.h"

#include "import_generic_geotiff.h"

#define DEFAULT_UTM_SCALE_FACTOR     0.9996
#define DEFAULT_SCALE_FACTOR         1.0

#define GEOTIFF_NAD27_DATUM                  "NAD27"
#define GEOTIFF_NAD83_DATUM                  "NAD83"
#define GEOTIFF_HARN_DATUM                   "HARN"
#define GEOTIFF_WGS84_DATUM                  "WGS 84"

#define UNKNOWN_PROJECTION_TYPE             -1

#define GEOTIFF_NUM_PROJDPARAMS              8
#define GEOTIFF_PROJPARAMS_STATE_PLANE       0
#define GEOTIFF_PROJPARAMS_USER_INDEX1       1
#define GEOTIFF_PROJPARAMS_STD_PARALLEL1     2
#define GEOTIFF_PROJPARAMS_STD_PARALLEL2     3
#define GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN  4
#define GEOTIFF_PROJPARAMS_LAT_ORIGIN        5
#define GEOTIFF_PROJPARAMS_FALSE_EASTING     6
#define GEOTIFF_PROJPARAMS_FALSE_NORTHING    7

#define MAX_NAME_LEN                         256

spheroid_type_t SpheroidName2spheroid(char *sphereName);
int PCS_2_UTM (short pcs, datum_type_t *datum, unsigned long *zone);
void copy_proj_parms(meta_projection *dest, meta_projection *src);

// Import a generic GeoTIFF (a projected GeoTIFF flavor) into
// our own ASF Tools format (.img, .meta)
//
void
import_generic_geotiff (const char *inFileName, const char *outBaseName, ...)
{
  int count;
  int read_count;
  int i;
  short model_type;
  short projection_type;
  short raster_type;
  short linear_units;
  double scale_factor;
  double proParams[GEOTIFF_NUM_PROJDPARAMS];
  TIFF *input_tiff;
  GTIF *input_gtif;
  meta_parameters *meta_out;
  datum_type_t datum;
  va_list ap;

  /***** INITIALIZE PARAMETERS *****/
  /*                               */
  // Create a new metadata object for the image.
  meta_out = raw_init ();
  meta_out->optical = NULL;
  meta_out->thermal = NULL;
  meta_out->projection = meta_projection_init ();
  meta_out->stats = meta_stats_init ();
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
  meta_stats *ms = meta_out->stats; // Convenience alias.
  meta_location *ml = meta_out->location; // Convenience alias.

  // Let the user know what format we are working on.
  asfPrintStatus
      ("   Input data type: GeoTIFF (ASF generic importer)\n");
  asfPrintStatus
      ("   Output data type: ASF format\n");

  // Open the input tiff file.
  input_tiff = XTIFFOpen (inFileName, "r");
  asfRequire (input_tiff != NULL, "Error opening input TIFF file.\n");

  // Open the structure that contains the geotiff keys.
  input_gtif = GTIFNew (input_tiff);
  asfRequire (input_gtif != NULL,
	      "Error reading GeoTIFF keys from input TIFF file.\n");


  /***** READ TIFF TAGS AND GEOKEYS *****/
  /*                                            */
  // Read GeoTIFF file citation (general info from the maker of the file)
  // NOTE: The citation may or may not exist ...
  char *citation;
  int citation_length;
  int typeSize;
  tagtype_t citation_type;
  citation_length = GTIFKeyInfo(input_gtif, GTCitationGeoKey, &typeSize, &citation_type);
  if (citation_length <= 0) {
    asfPrintWarning("\nMissing GT citation string in GeoTIFF file\n");
    citation = NULL;
  }
  else {
    citation = MALLOC ((citation_length) * typeSize);
    GTIFKeyGet (input_gtif, GTCitationGeoKey, citation, 0, citation_length);
    asfPrintStatus("GT Citation: %s\n", citation);
  }

  char *PCScitation;
  citation_length = GTIFKeyInfo(input_gtif, PCSCitationGeoKey, &typeSize, &citation_type);
  if (citation_length <= 0) {
    asfPrintWarning("\nMissing PCS citation string in GeoTIFF file\n");
    PCScitation = NULL;
  }
  else {
    PCScitation = MALLOC ((citation_length) * typeSize);
    GTIFKeyGet (input_gtif, PCSCitationGeoKey, citation, 0, citation_length);
    asfPrintStatus("PCS Citation: %s\n", citation);
  }

  // Get the tie point which defines the mapping between raster
  // coordinate space and geographic coordinate space.  Although
  // geotiff theoretically supports multiple tie points, we don't
  // (rationale: ArcView currently doesn't either, and multiple tie
  // points don't make sense with the pixel scale option, which we
  // need).
  double *tie_point;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_TIEPOINTS, &count,
  &tie_point);
  asfRequire (count == 6,
              "\nGeoTIFF file does not contain tie points\n");
  // Get the scale factors which define the scale relationship between
  // raster pixels and geographic coordinate space.
  double *pixel_scale;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_PIXELSCALE, &count,
  &pixel_scale);
  asfRequire (count == 3,
              "\nGeoTIFF file does not contain pixel scale parameters\n");
  asfRequire (pixel_scale[0] > 0.0 && pixel_scale[1] > 0.0,
              "\nGeoTIFF file contains invalid pixel scale parameters\n");

  // CHECK TO SEE IF THE GEOTIFF DOES CONTAIN USEFUL DATA:
  //  If the tiff file contains geocoded information, then the model type
  // will be ModelTypeProjected.  We add the requirement that pixels
  // represent area and that the units are in meters because that's what
  // we support to date.

  read_count
      = GTIFKeyGet (input_gtif, GTModelTypeGeoKey, &model_type, 0, 1);
  read_count
      += GTIFKeyGet (input_gtif, GTRasterTypeGeoKey, &raster_type, 0, 0);
  read_count
      += GTIFKeyGet (input_gtif, ProjLinearUnitsGeoKey, &linear_units, 0, 1);
  asfRequire (read_count == 3           &&
      model_type == ModelTypeProjected  &&
      raster_type == RasterPixelIsArea  &&
      linear_units == Linear_Meter,
      "Missing or unsupported model type, raster type,\nor linear unit information in GeoTIFF.\n"
      "Model type must be ModelTypeProjected, raster type RasterPixelIsArea,\n"
      "and linear units equal to Linear_Meter\n");

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
                      "meters" : "(Unsupported type)");

  /***** READ PROJECTION PARAMETERS FROM TIFF IF GEO DATA EXISTS                 *****/
  /*                                                                                 */
  // Init projection parameters
  for (i=0; i<GEOTIFF_NUM_PROJDPARAMS; i++) {
    proParams[i] = MAGIC_UNSET_DOUBLE;
  }

  short proj_coords_trans;
  short pcs;
  short geokey_datum;
  long proZone;
  double false_easting;
  double false_northing;
  double lonOrigin;
  double latOrigin;
  double stdParallel1;
  double stdParallel2;
  double lonPole;

  // Get datum and zone as appropriate
  read_count = GTIFKeyGet (input_gtif, ProjectedCSTypeGeoKey, &pcs, 0, 1);
  if (read_count == 1 && PCS_2_UTM(pcs, &datum, &proZone)) {
    proj_coords_trans = CT_TransverseMercator;
  }
  else {
    // Not recognized as a supported UTM PCS or was a user-defined or unknown type of PCS...
    //
    // The ProjCoordTransGeoKey will be true if the PCS was user-defined or if the PCS was
    // not in the geotiff file... or so the standard says.  If the ProjCoordTransGeoKey is
    // false, it means that an unsupported (by us) UTM or State Plane projection was
    // discovered (above.)  All other projection types make use of the ProjCoordTransGeoKey
    // geokey.
    read_count = GTIFKeyGet (input_gtif, ProjCoordTransGeoKey, &proj_coords_trans, 0, 1);
    asfRequire(read_count == 1,
               "\nUnable to determine type of projection coordinate system in GeoTIFF file\n");
    datum = UNKNOWN_DATUM;
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
          break;
      }
    }
    if (datum == UNKNOWN_DATUM) {
      read_count = GTIFKeyGet (input_gtif, GeographicTypeGeoKey, &geokey_datum, 0, 1);
      if (read_count == 1) {
        switch(geokey_datum){
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
            break;
        }
      }
    }
    if (geokey_datum == UNKNOWN_DATUM) {
      asfPrintWarning("\nUnable to determine datum type from GeoTIFF file\n"
          "...Will try to select a datum based on spheroid\n");
    }
  }

  char proName[MAX_NAME_LEN];
  long proNumber;
  projection_type = UNKNOWN_PROJECTION_TYPE;
  scale_factor = DEFAULT_SCALE_FACTOR;
  switch(proj_coords_trans) {
    case CT_TransverseMercator:
    case CT_TransvMercator_Modified_Alaska:
    case CT_TransvMercator_SouthOriented:
      // Zone, and usually the datum, should already be defined at this point (see above.)
      strcpy(proName, "UTM");
      asfPrintStatus("Found %s projection type\n", proName);
      asfRequire(proZone >= 1 && proZone <= 60,
                 "Missing zone in UTM-geocoded GeoTIFF\n");
      projection_type = UTM;
      proNumber = projection_type;
      read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false easting from GeoTIFF file\n"
                 "using ProjFalseEastingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
      }
      read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false northing from GeoTIFF file\n"
                 "using ProjFalseNorthingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
      }
      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
            "\nUnable to determine center longitude from GeoTIFF file\n"
            "using ProjNatOriginLongGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;
      }
      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
            "\nUnable to determine center latitude from GeoTIFF file\n"
            "using ProjNatOriginLatGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
      }
      read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
      if (read_count == 0) {
        scale_factor = DEFAULT_UTM_SCALE_FACTOR;

        char msg[256];
        sprintf(msg,
                "UTM scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4lf\n",
                scale_factor);
        asfPrintWarning(msg);
        //asfPrintWarning("UTM scale factor not found in GeoTIFF ...defaulting to 0.9996\n");
      }
      break;
    // Albers Conical Equal Area case IS tested
    case CT_AlbersEqualArea:
      projection_type = ALBERS;
      proNumber = projection_type;
      strcpy(proName, "Albers Conical Equal Area");
      asfPrintStatus("Found %s projection type\n", proName);

      read_count = GTIFKeyGet (input_gtif, ProjStdParallel1GeoKey, &stdParallel1, 0, 1);
      asfRequire(read_count == 1,
               "\nUnable to determine first standard parallel from GeoTIFF file\n"
               "using ProjStdParallel1GeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] = D2R*stdParallel1;

      read_count = GTIFKeyGet (input_gtif, ProjStdParallel2GeoKey, &stdParallel2, 0, 1);
      asfRequire(read_count == 1,
             "\nUnable to determine second standard parallel from GeoTIFF file\n"
             "using ProjStdParallel2GeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL2] = D2R*stdParallel2;

      read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false easting from GeoTIFF file\n"
                     "using ProjStdParallel2GeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
      }

      read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false northing from GeoTIFF file\n"
                     "using ProjFalseNorthingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
      }

      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
      if (read_count != 1) {
        read_count = GTIFKeyGet (input_gtif, ProjCenterLongGeoKey, &lonOrigin, 0, 1);
      }
      asfRequire (read_count == 1,
           "\nUnable to determine center longitude from GeoTIFF file\n"
           "using ProjNatOriginLongGeoKey or ProjCenterLongGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;

      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
      asfRequire(read_count != 1,
           "\nUnable to determine center latitude from GeoTIFF file\n"
           "using ProjNatOriginLatGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
      break;
    case CT_LambertConfConic_1SP:
      projection_type = LAMCC;
      proNumber = projection_type;
      strcpy(proName, "Lambert Conformal Conic");
      asfPrintStatus("Found %s projection type, single standard parallel type\n", proName);

      read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                   "\nUnable to determine false easting from GeoTIFF file\n"
                     "using ProjFalseEastingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
      }
      read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false northing from GeoTIFF file\n"
                     "using ProjFalseNorthingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
      }

      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center longitude from GeoTIFF file\n"
           "using ProjNatOriginLongGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;

      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center latitude from GeoTIFF file\n"
           "using ProjNatOriginLatGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;

      read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
      if (read_count != 1) {
        scale_factor = DEFAULT_SCALE_FACTOR;

        char msg[256];
        sprintf(msg,
                "%s scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4lf\n",
                proName, scale_factor);
        asfPrintWarning(msg);
      }
      break;
    case CT_LambertConfConic_2SP:
      projection_type = LAMCC;
      proNumber = projection_type;
      strcpy(proName, "Lambert Conformal Conic");
      asfPrintStatus("Found %s projection type, 2 standard parallels\n", proName);

      read_count = GTIFKeyGet (input_gtif, ProjStdParallel1GeoKey, &stdParallel1, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine first standard parallel from GeoTIFF file\n"
           "using ProjStdParallel1GeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] = D2R*stdParallel1;

      read_count = GTIFKeyGet (input_gtif, ProjStdParallel2GeoKey, &stdParallel2, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine second standard parallel from GeoTIFF file\n"
           "using ProjStdParallel2GeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL2] = D2R*stdParallel2;

      read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false easting from GeoTIFF file\n"
                     "using ProjFalseEastingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
      }

      read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false northing from GeoTIFF file\n"
                     "using ProjFalseNorthingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
      }

      read_count = GTIFKeyGet (input_gtif, ProjFalseOriginLongGeoKey, &lonOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center longitude from GeoTIFF file\n"
           "using ProjFalseOriginLongGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;

      read_count = GTIFKeyGet (input_gtif, ProjFalseOriginLatGeoKey, &latOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center latitude from GeoTIFF file\n"
           "using ProjFalseOriginLatGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
      break;
    case CT_PolarStereographic:
      projection_type = PS;
      proNumber = projection_type;
      strcpy(proName, "Polar Stereographic");
      asfPrintStatus("Found %s projection type\n", proName);

      read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center latitude from GeoTIFF file\n"
           "using ProjNatOriginLatGeoKey\n");
      // NOTE: Storing the latitude of origin in the Std Parallel #1 element is in
      // alignment with where this value is found when coming from the .aux file.
      // These values are copied to the meta data parameters later on and the assumption
      // is made that THIS is where this data item will be...
      proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] = D2R*latOrigin;

      read_count = GTIFKeyGet (input_gtif, ProjStraightVertPoleLongGeoKey, &lonPole, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine vertical pole longitude from GeoTIFF file\n"
           "using ProjStraightVertPoleLongGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonPole;

      read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false easting from GeoTIFF file\n"
                     "using ProjFalseEastingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
      }

      read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false northing from GeoTIFF file\n"
                     "using ProjFalseNorthingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
      }
      // NOTE: The scale_factor exists in the ProjScaleAtNatOriginGeoKey, but we do not
      // use it, e.g. it is not current written to the meta data file with meta_write().
      break;
    case CT_LambertAzimEqualArea:
      projection_type = LAMAZ;
      proNumber = projection_type;
      strcpy(proName, "Lambert Azimuthal Equal-area");
      asfPrintStatus("Found %s projection type\n", proName);

      read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false easting from GeoTIFF file\n"
                     "using ProjFalseEastingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
      }

      read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
      if (read_count != 1) {
        asfPrintWarning(
                 "\nUnable to determine false northing from GeoTIFF file\n"
                     "using ProjFalseNorthingGeoKey\n");
      }
      else {
        proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
      }

      read_count = GTIFKeyGet (input_gtif, ProjCenterLongGeoKey, &lonOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center longitude from GeoTIFF file\n"
           "using ProjCenterLongGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;

      read_count = GTIFKeyGet (input_gtif, ProjCenterLatGeoKey, &latOrigin, 0, 1);
      asfRequire(read_count == 1,
           "\nUnable to determine center latitude from GeoTIFF file\n"
           "using ProjCenterLatGeoKey\n");
      proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
      break;
    default:
      break;
  }

  /***** CONVERT TIFF TO FLOAT IMAGE *****/
  /*                                     */
  // Note to self:  tiff_to_float_image gets height/width from TIFF tags,
  // and asserts if width and height not greater than zero
  asfPrintStatus("\nConverting input GeoTIFF image into float image...\n");
  FloatImage *image = tiff_to_float_image (input_tiff);

  /***** EXPORT FLOAT IMAGE AS JPEG *****/
  /*                                    */
  // Note to self:  float_image_export_as_jpeg asserts if it fails
  //
//  asfPrintStatus("\nConverting original image to jpeg format and saving to disk...\n");
//  float_image_export_as_jpeg
//    (image, "pre_bad_data_remap.jpeg",
//     image->size_x > image->size_y ? image->size_x : image->size_y, NAN);

  /***** FIX DEM IMAGE'S BAD DATA *****/
  /*                                  */
  // Since the import could be a DEM, and DEMs of this flavor tend to be full
  // of bad data values that make the statistics hopeless, saturate output, etc.
  // For now we deal with this by mapping these values to a less negative magic number
  // of our own that still lets things work somewhat (assuming the bad
  // data values are rare at least).
  //
  // UPDATE: I'm leaving this scan in for now (11-1-06) ...but if it never issues
  // any warnings when ingesting geotiffs then it's probably not necessary
  // to scan for 'bad data'.  AND, it's possible that the stats calculations can handle
  // the bad data now ...needs testing.
  asfPrintStatus("\nScanning image for bad data values...\n");
  const float bad_data_ceiling = -10e10;
  const float new_bad_data_magic_number = -999.0;
  size_t ii, jj;
  char bad_values_existed = 0;
  for ( ii = 0 ; ii < image->size_y ; ii++ ) {
    for ( jj = 0 ; jj < image->size_x ; jj++ ) {
      if ( float_image_get_pixel (image, jj, ii) < bad_data_ceiling ) {
	float_image_set_pixel (image, jj, ii, new_bad_data_magic_number);
        bad_values_existed = 1;
      }
    }
  }
  if (bad_values_existed) {
    asfPrintWarning("Float image contained extra-negative values (< -10e10) that may\n"
        "result in inaccurate image statistics.\n\n");
    asfPrintStatus("Extra-negative values found within the float image have been removed\n\n");
  }

  // Get the raster width and height of the image.
  uint32 width = image->size_x;
  uint32 height = image->size_y;


  /***** FILL IN THE META DATA *****/
  /*                               */

  // Data type is REAL32 because the image is converted to float
  mg->data_type = REAL32;

  // Get the image data type from the variable arguments list
  char image_data_type[256];
  char *pTmpChar;
  va_start(ap, outBaseName); // outBaseName is the last argument before ", ..."
  pTmpChar = (char *)va_arg(ap, char *);
  if (pTmpChar != NULL) {
    strcpy(image_data_type, pTmpChar);
  }
  else {
    strcpy(image_data_type, MAGIC_UNSET_STRING);
  }
  va_end(ap);
  if (strncmp(image_data_type, "GEOCODED_IMAGE", 14) == 0) {
    mg->image_data_type = GEOCODED_IMAGE;
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
  double raster_tp_y = tie_point[1]; // [2] is zero for 2D space

  // Coordinates of tie point in pseudoprojection space.
  double tp_lon = tie_point[3];
  double tp_lat = tie_point[4]; // [5] is zero for 2D space

  // Center latitude and longitude of image data
  double center_x = (height / 2.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;
  double center_y = (width / 2.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
  // converts to center_latitude and center_longitude below...

  if (datum != UNKNOWN_DATUM) { // Data came successfully from TIFF file
    spheroid_type_t spheroid = datum_spheroid (datum);
    spheroid_axes_lengths (spheroid, &mg->re_major, &mg->re_minor);
  }
  else {
    mg->re_major = MAGIC_UNSET_DOUBLE;
    mg->re_minor = MAGIC_UNSET_DOUBLE;
  }

  switch (projection_type) {
    case UTM:     // Universal Transverse Mercator (UTM)
      mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      mp->param.utm.zone = proZone;
      mp->param.utm.false_easting =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.false_northing =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.lat0 =
          (proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.lon0 =
          (proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.scale_factor = scale_factor;
      break;
    case ALBERS:  // Albers Equal Area Conic (aka Albers Conical Equal Area)
      mp->type = ALBERS_EQUAL_AREA;
      mp->param.albers.std_parallel1 =
          (proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.std_parallel2 =
          (proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL2] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL2] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.center_meridian =
          (proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.orig_latitude =
          (proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.false_easting =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.false_northing =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      break;
    case LAMCC:   // Lambert Conformal Conic
      mp->type = LAMBERT_CONFORMAL_CONIC;
      mp->param.lamcc.plat1 =
          (proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.plat2 =
          (proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL2] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL2] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.lat0 =
          (proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.lon0 =
          (proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.false_easting =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.false_northing =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.scale_factor = scale_factor;
      break;
    case PS:      // Polar Stereographic
      mp->type = POLAR_STEREOGRAPHIC;
      mp->param.ps.slat =
          (proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_STD_PARALLEL1] :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.slon =
          (proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.is_north_pole =
          (proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          (  (proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] > 0) ? 1 : 0)   :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.false_easting =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.false_northing =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      // Note: A GeoTIFF (.tif) file does have a scale factor tag in it for Polar Stereographic.
      // We do not use it however.  It does not exist in the meta data struct and is also
      // not written to the meta data file with meta_write().  See the GeoTIFF
      // Standard, key ProjScaleAtNatOriginGeoKey for polar stereographic.
      break;
    case LAMAZ:   // Lambert Azimuthal Equal Area
      mp->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
      mp->param.lamaz.center_lon =
          (proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamaz.center_lat =
          (proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamaz.false_easting =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamaz.false_northing =
          (proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*proParams[GEOTIFF_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      break;
    default:
      break;
  }

  mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
  mp->startY = (0.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;

  mp->perX = mg->x_pixel_size;
  mp->perY = -mg->y_pixel_size;

  char *units = (linear_units == Linear_Meter) ? "meters" : MAGIC_UNSET_STRING;
  strcpy (mp->units, units);

  if (datum != UNKNOWN_DATUM) {
    mp->spheroid = datum_spheroid (datum);
  } // else leave it at initial value

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  mp->datum = datum;

  float min, max;
  float mean, standard_deviation;
  float_image_statistics (image, &min, &max, &mean, &standard_deviation,
                          FLOAT_IMAGE_DEFAULT_MASK);
  mp->height = mean;

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
  mp->hem = mg->center_latitude >= 0.0 ? 'N' : 'S';

  msar->image_type = 'P'; // Map Projected
  ms->mean = mean;
  // The root mean square error and standard deviation are very close
  // by definition when the number of samples is large, there seems to
  // be some confusion about the definitions of one relative to the
  // other, and I don't think its worth agonizing about the best thing
  // to do.
  ms->rmse = standard_deviation;
  ms->std_deviation = standard_deviation;
  ms->mask = FLOAT_IMAGE_DEFAULT_MASK;

  ml->lat_start_near_range = mp->startX;
  ml->lon_start_near_range = mp->startY;
  ml->lat_start_far_range = mp->startX + mp->perX * width;
  ml->lon_start_far_range = mp->startY;
  ml->lat_end_near_range = mp->startX;
  ml->lon_end_near_range = mp->startY + mp->perY * height;
  ml->lat_end_far_range = mp->startX + mp->perX * width;
  ml->lon_end_far_range = mp->startY + mp->perY * height;

  asfPrintStatus("\nWriting new '.meta' and '.img' files...\n");
  int return_code = write_meta_and_img (outBaseName, meta_out, image);
  asfRequire (return_code == 0,
	      "Failed to write new '.meta' and '.img' files.");

  // We're now done with the data and metadata.
  meta_free (meta_out);
  float_image_free (image);

  // We must be done with the citation string too :)
  if (citation) {
    FREE (citation);
  }
  if (PCScitation) {
    FREE (PCScitation);
  }
}

/******************** local_machine_is_little_endian() ********************/
/*  COMMENTS:                                                             */
/*    - Returns non-zero if the local machine architecture/OS writes      */
/*      data to memory/disk in little-endian order, e.g. Intel format     */
/*      else returns zero                                                 */
/*    - 'testlong' is a multi-byte unsigned integer with a non-zero value */
/*      stored in its least-significant byte (LSB).  If the local machine */
/*      writes in big-endian format, then '*(unsigned char*)&testlong' (the byte  */
/*      at the lowest address in memory where 'testlong' is written) will */
/*      be zero.  But if the local machine is writing in little-endian    */
/*      format, then the bytes of 'testlong' will be in reverse order     */
/*      and the LSB will be located at the lowest byte address, e.g.      */
/*      '*(unsigned char*)&testlong' will return the LSB ...non-zero in this case */
unsigned char local_machine_is_little_endian()
{
  unsigned long testlong=1L;
  unsigned char rtn = *(unsigned char*)&testlong;

  return rtn;
}

int PCS_2_UTM(short pcs, datum_type_t *datum, unsigned long *zone)
{
  // The GeoTIFF standard defines the UTM zones numerically in a way that
  // let's us pick off the data mathematically (NNNzz where zz is the zone
  // number):
  //
  // For NAD83 datums, Zones 3N through 23N, NNN == 269
  // For NAD27 datums, Zones 3N through 22N, NNN == 367
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
  const short NNN_NAD27 = 367;
  const short NNN_NAD83 = 269;
  const short NNN_WGS84 = 326;
  int isUTM = 0;
  short datumClassifierNNN;

  datumClassifierNNN = pcs / 100;
  if (datumClassifierNNN == NNN_NAD27) {
      *datum = NAD27_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
  }
  else if (datumClassifierNNN == NNN_NAD83) {
      *datum = NAD83_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
  }
  else if (datumClassifierNNN == NNN_WGS84) {
      *datum = WGS84_DATUM;
      *zone = pcs - (pcs / 100)*100;
      isUTM = 1;
  }
  else {
      *datum = UNKNOWN_DATUM;
      *zone = 0;
      isUTM = 0;
  }

  return isUTM;
}

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
      dest->param.utm.lat0 = D2R*src->param.utm.lat0;
      dest->param.utm.lon0 = D2R*src->param.utm.lon0;
      dest->param.utm.false_easting = src->param.utm.false_easting;
      dest->param.utm.false_northing = src->param.utm.false_northing;
      dest->param.utm.scale_factor = src->param.utm.scale_factor;
      break;
    case POLAR_STEREOGRAPHIC:
      dest->param.ps.slat = D2R*src->param.ps.slat;
      dest->param.ps.slon = D2R*src->param.ps.slon;
      dest->param.ps.is_north_pole = src->param.ps.is_north_pole;
      dest->param.ps.false_easting = src->param.ps.false_easting;
      dest->param.ps.false_northing = src->param.ps.false_northing;
      break;
    case ALBERS_EQUAL_AREA:
      dest->param.albers.std_parallel1 = D2R*src->param.albers.std_parallel1;
      dest->param.albers.std_parallel2 = D2R*src->param.albers.std_parallel2;
      dest->param.albers.center_meridian = D2R*src->param.albers.center_meridian;
      dest->param.albers.orig_latitude = D2R*src->param.albers.orig_latitude;
      dest->param.albers.false_easting = src->param.albers.false_easting;
      dest->param.albers.false_northing = src->param.albers.false_northing;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      dest->param.lamcc.plat1 = D2R*src->param.lamcc.plat1;
      dest->param.lamcc.plat2 = D2R*src->param.lamcc.plat2;
      dest->param.lamcc.lat0 = D2R*src->param.lamcc.lat0;
      dest->param.lamcc.lon0 = D2R*src->param.lamcc.lon0;
      dest->param.lamcc.false_easting = src->param.lamcc.false_easting;
      dest->param.lamcc.false_northing = src->param.lamcc.false_northing;
      dest->param.lamcc.scale_factor = src->param.lamcc.scale_factor;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      dest->param.lamaz.center_lon = D2R*src->param.lamaz.center_lon;
      dest->param.lamaz.center_lat = D2R*src->param.lamaz.center_lat;
      dest->param.lamaz.false_easting = src->param.lamaz.false_easting;
      dest->param.lamaz.false_northing = src->param.lamaz.false_northing;
      break;
    default:
      asfPrintError("Unsupported projection type found.\n");
      exit(EXIT_FAILURE);
      break;
  }
}
