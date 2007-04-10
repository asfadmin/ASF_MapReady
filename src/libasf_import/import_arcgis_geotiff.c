// Import an ERDAS ArcGIS GeoTIFF (a projected GeoTIFF flavor), including
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

#include <float_image.h>
#include <spheroids.h>
#include <proj.h>
#include <libasf_proj.h>

#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"

#include "projected_image_import.h"
#include "tiff_to_float_image.h"
#include "write_meta_and_img.h"
#include "geotiff_support.h"
#include "arcgis_spheroids.h"
#include "find_arcgis_geotiff_aux_name.h"
#include "import_arcgis_geotiff.h"

#define ARCGIS_DEFAULT_UTM_SCALE_FACTOR     0.9996
#define ARCGIS_DEFAULT_SCALE_FACTOR         1.0

#define ARCGIS_NUM_PROJDPARAMS              15
#define ARCGIS_PROJPARAMS_STATE_PLANE       0
#define ARCGIS_PROJPARAMS_USER_INDEX1       1
#define ARCGIS_PROJPARAMS_STD_PARALLEL1     2
#define ARCGIS_PROJPARAMS_STD_PARALLEL2     3
#define ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN  4
#define ARCGIS_PROJPARAMS_LAT_ORIGIN        5
#define ARCGIS_PROJPARAMS_FALSE_EASTING     6
#define ARCGIS_PROJPARAMS_FALSE_NORTHING    7
#define ARCGIS_PROJPARAMS_USER_INDEX8       8
#define ARCGIS_PROJPARAMS_USER_INDEX9       9
#define ARCGIS_PROJPARAMS_USER_INDEX10      10
#define ARCGIS_PROJPARAMS_USER_INDEX11      11
#define ARCGIS_PROJPARAMS_USER_INDEX12      12
#define ARCGIS_PROJPARAMS_USER_INDEX13      13
#define ARCGIS_PROJPARAMS_USER_INDEX14      14

#define ARCGIS_NUM_DATUMDPARAMS             7
#define ARCGIS_PROJPARAMS_STATE_PLANE       0
#define ARCGIS_PROJPARAMS_USER_INDEX1       1
#define ARCGIS_PROJPARAMS_STD_PARALLEL1     2
#define ARCGIS_PROJPARAMS_STD_PARALLEL2     3
#define ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN  4
#define ARCGIS_PROJPARAMS_LAT_ORIGIN        5
#define ARCGIS_PROJPARAMS_FALSE_EASTING     6

#define ARCGIS_NAD27_DATUM                  "NAD27"
#define ARCGIS_NAD83_DATUM                  "NAD83"
#define ARCGIS_HARN_DATUM                   "HARN"
#define ARCGIS_WGS84_DATUM                  "WGS 84"

#define ARCGIS_USER_DEFINED_PCS             32767

#define UNKNOWN_PROJECTION_TYPE             -1

typedef struct {
  char sphereName[MAX_EHFA_ENTRY_NAMESTRING_LEN]; // Spheroid name
  double a; // Major axis
  double b; // Minor axis
  double eSquared; // e^2
  double radius; // Radius at equator
} arcgisSpheroidParms_t;

typedef struct {
  unsigned long proNumber; // Projection number (equates to a type, e.g. "Albers..."
  unsigned long proZone; // Only applies to UTM
  double proParams[ARCGIS_NUM_PROJDPARAMS]; // Array of 15 projection parameters
  unsigned short proType; // Index for projection type enum
  char proExeName[MAX_EHFA_ENTRY_NAMESTRING_LEN]; // Name of exe for converting between EPRJ_INTERNAL and EPRJ_EXTERNAL
  char proName[MAX_EHFA_ENTRY_NAMESTRING_LEN]; // Name of type of projection
  arcgisSpheroidParms_t proSpheroid;
} arcgisProjParms_t;

typedef struct {
  char datumname[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char gridname[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  unsigned short type;
  double params[ARCGIS_NUM_DATUMDPARAMS];
} arcgisDatumParms_t;

typedef struct {
  double x;
  double y;
} arcgisCoordinate_t;

typedef struct {
  double width;
  double height;
} arcgisSize_t;

typedef struct {
  char proName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  arcgisCoordinate_t upperLeftCenter;
  arcgisCoordinate_t lowerRightCenter;
  arcgisSize_t pixelSize;
  char units[MAX_EHFA_ENTRY_NAMESTRING_LEN];
} arcgisMapInfo_t;

typedef struct {
  char projection[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char units[MAX_EHFA_ENTRY_NAMESTRING_LEN];
} arcgisEimg_MapInformation_t;

void getArcgisProjParameters(char *infile, arcgisProjParms_t *proParms);
void getArcgisDatumParameters(char *infile, arcgisDatumParms_t *datumParms);
void getArcgisMapInfo(char *infile, arcgisMapInfo_t *arcgisMapInfo);
void getArcgisEimg_MapInformation (char *infile,
                   arcgisEimg_MapInformation_t *arcgisEimg_MapInformation);
void readArcgisEimg_MapInformation (FILE *fp, unsigned long offset,
                   arcgisEimg_MapInformation_t *arcgisEimg_MapInformation);
void DHFAGetDoubleValFromOffset(FILE *fp, unsigned long offset, double *val);
void DHFAGetDoubleVal(FILE *fp, double *val);
void DHFAGetFloatValFromOffset(FILE *fp, unsigned long offset, float *val);
void DHFAGetFloatVal(FILE *fp, float *val);
void DHFAGetStringValFromOffset(FILE *fp, unsigned long offset,
                                unsigned long strLen, char *str);
void DHFAGetStringVal(FILE *fp, unsigned long strLen, char *str);
spheroid_type_t arcgisSpheroidName2spheroid(char *sphereName);

// Import an ERDAS ArcGIS GeoTIFF (a projected GeoTIFF flavor), including
// projection data from its metadata file (ERDAS MIF HFA .aux file) into
// our own ASF Tools format (.img, .meta)
//
void
import_arcgis_geotiff (const char *inFileName, const char *outBaseName, ...)
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
  int auxDataExists;
  int count;
  int read_count;
  int i;
  short model_type;
  short projection_type;
  short raster_type;
  short linear_units;
  double scale_factor;
  GString *inGeotiffAuxName;
  arcgisProjParms_t arcgisProjParms;
  arcgisDatumParms_t arcgisDatumParms;
  arcgisMapInfo_t arcgisMapInfo;
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
    ("\n   Input data type: GeoTIFF (ArcGIS "
      "flavor with ArcGIS metadata (.aux) file)\n");
  asfPrintStatus
    ("   Output data type: ASF format\n");

  // Open the input tiff file.
  input_tiff = XTIFFOpen (inFileName, "r");
  asfRequire (input_tiff != NULL, "Error opening input TIFF file.\n");

  // Open the structure that contains the geotiff keys.
  input_gtif = GTIFNew (input_tiff);
  asfRequire (input_gtif != NULL,
	      "Error reading GeoTIFF keys from input TIFF file.\n");


  /***** GET WHAT WE CAN FROM THE TIFF FILE *****/
  /*                                            */
  // Read GeoTIFF file citation (general info from the maker of the file)
  // NOTE: The citation may or may not exist ...it is not required by the
  // file standard but we need it to help check for ERDAS IMAGINE type format
  int citation_length;
  int typeSize;
  tagtype_t citation_type;
  citation_length = GTIFKeyInfo(input_gtif, GTCitationGeoKey, &typeSize, &citation_type);
  asfRequire (citation_length > 0,
              "Missing citation string in GeoTIFF file\n");
  char *citation = MALLOC ((citation_length) * typeSize);
  GTIFKeyGet (input_gtif, GTCitationGeoKey, citation, 0, citation_length);
  asfPrintStatus("\nCitation: %s\n", citation);

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
  asfRequire (count == 6,
              "GeoTIFF file does not contain tie points\n");
  // Get the scale factors which define the scale relationship between
  // raster pixels and geographic coordinate space.
  double *pixel_scale;
  (input_gtif->gt_methods.get)(input_gtif->gt_tif, GTIFF_PIXELSCALE, &count,
  &pixel_scale);
  asfRequire (count == 3,
              "GeoTIFF file does not contain pixel scale parameters\n");
  asfRequire (pixel_scale[0] > 0.0 && pixel_scale[1] > 0.0,
              "GeoTIFF file contains invalid pixel scale parameters\n");

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
  if (model_type != ModelTypeProjected) {
    // FIXME: For now, we only import map-projected images in linear meters.  If
    // the image was a lat/long image, then the angular units would be set AND
    // the model_type would be ModelType_Geographic ...but oh well.
    asfPrintError("Only map-projected ArcGIS/Imagine type images using linear units\n"
        "are supported.  Geographic (lat/long) ArcGIS/Imagine type images are not.\n");
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
  // Start of reading projection parameters from geotiff ...if it exists //
  char hemisphere;
  if (geotiff_data_exists) {
    // Init ArcGIS projection parameters
    // NOTE: A direct copy from these into the meta data occurs below, so we
    // should write into them whether geocode data exists in the tiff or not.
    // If the .aux file exists, then anything read there will supercede these
    // values as well.
    arcgisProjParms.proType = 0;
    arcgisProjParms.proNumber = 0L;
    strcpy(arcgisProjParms.proExeName, MAGIC_UNSET_STRING);
    strcpy(arcgisProjParms.proName, MAGIC_UNSET_STRING);
    arcgisProjParms.proZone = 0L;
    for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++) {
      arcgisProjParms.proParams[i] = MAGIC_UNSET_DOUBLE;
    }
    strcpy(arcgisProjParms.proSpheroid.sphereName, MAGIC_UNSET_STRING);
    arcgisProjParms.proSpheroid.a = MAGIC_UNSET_DOUBLE;
    arcgisProjParms.proSpheroid.b = MAGIC_UNSET_DOUBLE;
    arcgisProjParms.proSpheroid.eSquared = MAGIC_UNSET_DOUBLE;
    arcgisProjParms.proSpheroid.radius = MAGIC_UNSET_DOUBLE;

    short proj_coords_trans = UNKNOWN_PROJECTION_TYPE;
    short pcs;
    short geokey_datum;
    double false_easting;
    double false_northing;
    double lonOrigin;
    double latOrigin;
    double stdParallel1;
    double stdParallel2;
    double lonPole;

    // Get datum and zone as appropriate
    read_count = GTIFKeyGet (input_gtif, ProjectedCSTypeGeoKey, &pcs, 0, 1);
    if (read_count == 1 && PCS_2_UTM(pcs, &hemisphere, &datum, &arcgisProjParms.proZone)) {
      proj_coords_trans = CT_TransverseMercator;
      arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = 500000.0;
      if (hemisphere == 'N') {
        arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = 0.0;
      }
      else {
        arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = 10000000.0;
      }
    }
    else {
      // Not recognized as a supported UTM PCS or was a user-defined or unknown type of PCS...
      //
      // The ProjCoordTransGeoKey will be true if the PCS was user-defined or if the PCS was
      // not in the geotiff file... or so the standard says.  If the ProjCoordTransGeoKey is
      // false, it means that an unsupported (by us) UTM or State Plane projection was
      // discovered (above.)  All other projection types make use of the ProjCoordTransGeoKey
      // geokey.

      // Check for a user-defined UTM projection
      read_count = GTIFKeyGet (input_gtif, ProjectionGeoKey, &pcs, 0, 0);
      if (read_count == 1 && PCS_2_UTM(pcs, &hemisphere, &datum, &arcgisProjParms.proZone)) {
        proj_coords_trans = CT_TransverseMercator;
        arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = 500000.0;
        if (hemisphere == 'N') {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = 0.0;
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = 10000000.0;
        }
      }
      else { // Some other type of projection may exist
        read_count = GTIFKeyGet (input_gtif, ProjCoordTransGeoKey, &proj_coords_trans, 0, 1);
      }
      asfRequire(read_count == 1 && proj_coords_trans != UNKNOWN_PROJECTION_TYPE,
                 "Unable to determine type of projection coordinate system in GeoTIFF file\n");

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
              break;
          }
        }
      }
      if (datum == UNKNOWN_DATUM) {
        asfPrintWarning("Unable to determine datum type from GeoTIFF file\n");
      }
    }
    //asfPrintWarning("GeoTIFF projection data found in ArcGIS GeoTIFF ...Data will be over-written\n"
        //"with data found in the ArcGIS metadata (.aux) file.\n");

    projection_type = UNKNOWN_PROJECTION_TYPE;
    scale_factor = ARCGIS_DEFAULT_SCALE_FACTOR;
    switch(proj_coords_trans) {
      case CT_TransverseMercator:
      case CT_TransvMercator_Modified_Alaska:
      case CT_TransvMercator_SouthOriented:
        // Zone and datum should already be defined at this point (see above.)
        projection_type = UTM;
        arcgisProjParms.proNumber = projection_type;
        strcpy(arcgisProjParms.proName, "UTM");
        read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
        if (read_count != 1) {
//          asfPrintWarning(
//                   "Unable to determine false easting from GeoTIFF file\n"
//                   "using ProjFalseEastingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = false_easting;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
        if (read_count != 1) {
//          asfPrintWarning(
//                   "Unable to determine false northing from GeoTIFF file\n"
//                   "using ProjFalseNorthingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = false_northing;
        }
        read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
        if (read_count != 1) {
//          asfPrintWarning(
//              "Unable to determine center longitude from GeoTIFF file\n"
//              "using ProjNatOriginLongGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
        if (read_count != 1) {
//          asfPrintWarning(
//              "Unable to determine center latitude from GeoTIFF file\n"
//              "using ProjNatOriginLatGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
        if (read_count == 0) {
          scale_factor = ARCGIS_DEFAULT_UTM_SCALE_FACTOR;

          char msg[256];
//          sprintf(msg,
//                  "UTM scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4lf\n",
//                  scale_factor);
//          asfPrintWarning(msg);
          sprintf(msg,"UTM scale factor defaulting to %0.4lf\n", scale_factor);
          asfPrintStatus(msg);
        }
        break;
      // Albers Conical Equal Area case IS tested
      case CT_AlbersEqualArea:
        projection_type = ALBERS;
        arcgisProjParms.proNumber = projection_type;
        strcpy(arcgisProjParms.proName, "Albers Conical Equal Area");
        read_count = GTIFKeyGet (input_gtif, ProjStdParallel1GeoKey, &stdParallel1, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine first standard parallel from GeoTIFF file\n"
              "using ProjStdParallel1GeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] = D2R*stdParallel1;
        }
        read_count = GTIFKeyGet (input_gtif, ProjStdParallel2GeoKey, &stdParallel2, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine second standard parallel from GeoTIFF file\n"
              "using ProjStdParallel2GeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL2] = D2R*stdParallel2;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false easting from GeoTIFF file\n"
              "using ProjFalseEastingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false northing from GeoTIFF file\n"
              "using ProjFalseNorthingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
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
        if (read_count == 1) {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center latitude from GeoTIFF file\n"
              "using ProjNatOriginLatGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
        }
        break;
      // FIXME: The Lambert Conformal Conic 1-Std Parallel case is UNTESTED
      case CT_LambertConfConic_1SP:
        projection_type = LAMCC;
        arcgisProjParms.proNumber = projection_type;
        strcpy(arcgisProjParms.proName, "Lambert Conformal Conic");
        read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                     "Unable to determine false easting from GeoTIFF file\n"
              "using ProjFalseEastingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false northing from GeoTIFF file\n"
              "using ProjFalseNorthingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
        }
        read_count = GTIFKeyGet (input_gtif, ProjNatOriginLongGeoKey, &lonOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center longitude from GeoTIFF file\n"
              "using ProjNatOriginLongGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center latitude from GeoTIFF file\n"
              "using ProjNatOriginLatGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjScaleAtNatOriginGeoKey, &scale_factor, 0, 1);
        if (read_count != 1) {
          scale_factor = ARCGIS_DEFAULT_SCALE_FACTOR;

          char msg[256];
          sprintf(msg,
                  "Lambert Conformal Conic scale factor from ProjScaleAtNatOriginGeoKey not found in GeoTIFF ...defaulting to %0.4lf\n",
                  scale_factor);
          asfPrintWarning(msg);
        }
        break;
      case CT_LambertConfConic_2SP:
        projection_type = LAMCC;
        arcgisProjParms.proNumber = projection_type;
        strcpy(arcgisProjParms.proName, "Lambert Conformal Conic");
        read_count = GTIFKeyGet (input_gtif, ProjStdParallel1GeoKey, &stdParallel1, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine first standard parallel from GeoTIFF file\n"
              "using ProjStdParallel1GeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] = D2R*stdParallel1;
        }
        read_count = GTIFKeyGet (input_gtif, ProjStdParallel2GeoKey, &stdParallel2, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine second standard parallel from GeoTIFF file\n"
              "using ProjStdParallel2GeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL2] = D2R*stdParallel2;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false easting from GeoTIFF file\n"
              "using ProjFalseEastingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false northing from GeoTIFF file\n"
              "using ProjFalseNorthingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseOriginLongGeoKey, &lonOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center longitude from GeoTIFF file\n"
              "using ProjFalseOriginLongGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseOriginLatGeoKey, &latOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center latitude from GeoTIFF file\n"
              "using ProjFalseOriginLatGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
        }
        break;
      case CT_PolarStereographic:
        projection_type = PS;
        arcgisProjParms.proNumber = projection_type;
        strcpy(arcgisProjParms.proName, "Polar Stereographic");
        read_count = GTIFKeyGet (input_gtif, ProjNatOriginLatGeoKey, &latOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center latitude from GeoTIFF file\n"
              "using ProjNatOriginLatGeoKey\n");
        }
        else {
          // NOTE: Storing the latitude of origin in the Std Parallel #1 element is in
          // alignment with where this value is found when coming from the .aux file.
          // These values are copied to the meta data parameters later on and the assumption
          // is made that THIS is where this data item will be...
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] = D2R*latOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjStraightVertPoleLongGeoKey, &lonPole, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine vertical pole longitude from GeoTIFF file\n"
              "using ProjStraightVertPoleLongGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonPole;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false easting from GeoTIFF file\n"
              "using ProjFalseEastingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false northing from GeoTIFF file\n"
              "using ProjFalseNorthingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
        }
        // NOTE: The scale_factor exists in the ProjScaleAtNatOriginGeoKey, but we do not
        // use it, e.g. it is not current written to the meta data file with meta_write().
        break;
      case CT_LambertAzimEqualArea:
        projection_type = LAMAZ;
        arcgisProjParms.proNumber = projection_type;
        strcpy(arcgisProjParms.proName, "Lambert Azimuthal Equal-area");
        read_count = GTIFKeyGet (input_gtif, ProjFalseEastingGeoKey, &false_easting, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false easting from GeoTIFF file\n"
              "using ProjFalseEastingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] = D2R*false_easting;
        }
        read_count = GTIFKeyGet (input_gtif, ProjFalseNorthingGeoKey, &false_northing, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine false northing from GeoTIFF file\n"
              "using ProjFalseNorthingGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] = D2R*false_northing;
        }
        read_count = GTIFKeyGet (input_gtif, ProjCenterLongGeoKey, &lonOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center longitude from GeoTIFF file\n"
              "using ProjCenterLongGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] = D2R*lonOrigin;
        }
        read_count = GTIFKeyGet (input_gtif, ProjCenterLatGeoKey, &latOrigin, 0, 1);
        if (read_count != 1) {
          asfPrintWarning(
                   "Unable to determine center latitude from GeoTIFF file\n"
              "using ProjCenterLatGeoKey\n");
        }
        else {
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] = D2R*latOrigin;
        }
        break;
      default:
        asfPrintWarning(
            "Unable to determine projection type from GeoTIFF file\n"
            "using ProjectedCSTypeGeoKey or ProjCoordTransGeoKey\n");
        geotiff_data_exists = 0;
        break;
    }
  } // End of reading projection parameters from geotiff ...if it existed

  /***** GET ALL VALUES THAT ARE IN THE ARCGIS METADATA FILE (.aux)  *****/
  /*     IF THE FILE EXISTS.                                             */
  char inBaseName[256];
  strcpy(inBaseName, inFileName);
  *(findExt(inBaseName)) = '\0';
  inGeotiffAuxName = find_arcgis_geotiff_aux_name(inBaseName);
  if ( inGeotiffAuxName == NULL) {
    asfPrintWarning("No ArcGIS metadata (.aux) file was found using <basename>.\n"
        "Meta data may be incomplete.\n");
    auxDataExists = 0;
  }
  if ( inGeotiffAuxName != NULL ) { // ArcGIS metadata file (.aux) exists
    short proj_type;
    auxDataExists = 1;
    proj_type = getArcgisProjType (inGeotiffAuxName->str);
    if (  proj_type != UTM    &&
          proj_type != ALBERS &&
          proj_type != LAMCC  &&
          proj_type != PS     &&
          proj_type != LAMAZ)
    {
      //asfPrintWarning ("Missing or unsupported projection parameters found in\n"
          //"ArcGIS metadata (.aux) file\n");
      auxDataExists = 0;
    }
    else {
      if ( proj_type == UTM     ||
           proj_type == ALBERS  ||
           proj_type == LAMCC   ||
           proj_type == PS      ||
           proj_type == LAMAZ   )
      {
        projection_type = proj_type;
      }
      // Read projection parameters from .aux file
      getArcgisProjParameters(inGeotiffAuxName->str,
                              &arcgisProjParms);
      // Try to get datum record from .aux file
      getArcgisDatumParameters(inGeotiffAuxName->str, &arcgisDatumParms);
      if (strncmp(arcgisDatumParms.datumname, ARCGIS_NAD27_DATUM, strlen(ARCGIS_NAD27_DATUM)) == 0) {
        datum = NAD27_DATUM;
      }
      else if (strncmp(arcgisDatumParms.datumname, ARCGIS_NAD83_DATUM, strlen(ARCGIS_NAD83_DATUM)) == 0) {
        datum = NAD83_DATUM;
      }
      else if (strncmp(arcgisDatumParms.datumname, ARCGIS_WGS84_DATUM, strlen(ARCGIS_WGS84_DATUM)) == 0) {
        datum = WGS84_DATUM;
      }
      else if (!geotiff_data_exists) {
        asfPrintWarning("Couldn't identify datum in GeoTIFF or ArcGIS metadata (.aux) file...\n");
        // NOTE: The ArcGIS .aux file may have contained "HARN" for High Accuracy Reference Network
        // (a GPS-enhanced NAD83), but we don't separately support it at this time and I'm not sure
        // if it's OK to just call it NAD83 ...
      }
      // Read map info data from .aux file
      getArcgisMapInfo(inGeotiffAuxName->str, &arcgisMapInfo);
    }
  }
  g_string_free (inGeotiffAuxName, TRUE);

  // If at this point, the projection parameters were found in neither file
  // then quit ...
  asfRequire(geotiff_data_exists || auxDataExists,
              "Projection parameters missing in both the GeoTIFF\n"
              "file and the ArcGIS metadata (.aux) file.  Projection\n"
              "parameters will be incomplete.\n");

  /***** CONVERT TIFF TO FLOAT IMAGE *****/
  /*                                     */
  // Note to self:  tiff_to_float_image gets height/width from TIFF tags,
  // and asserts if width and height not greater than zero
  asfPrintStatus("\nConverting input TIFF image into float image...\n");
  FloatImage *image = tiff_to_float_image (input_tiff);

  /***** EXPORT FLOAT IMAGE AS JPEG *****/
  /*                                    */
  // Note to self:  float_image_export_as_jpeg asserts if it fails
  //
  // UPDATE: I'm commenting out the creation of a pre-bad data scan jpeg because
  // for now it doesn't seem likely that ArcGIS geotiffs will have problems
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
  // any warnings when ingesting ArcGIS geotiffs then it's probably not necessary
  // to scan for 'bad data'.  AND, it's possible that the stats calculations can handle
  // the bad data now ...needs testing.

  // UPDATE: I'm taking this scan out for now (01-25-07)
  //
  //asfPrintStatus("\nScanning image for bad data values...\n");
  //const float bad_data_ceiling = -10e10;
  //const float new_bad_data_magic_number = -999.0;
  //size_t ii, jj;
  //char bad_values_existed = 0;
  //for ( ii = 0 ; ii < image->size_y ; ii++ ) {
    //for ( jj = 0 ; jj < image->size_x ; jj++ ) {
      //if ( float_image_get_pixel (image, jj, ii) < bad_data_ceiling ) {
	//float_image_set_pixel (image, jj, ii, new_bad_data_magic_number);
        //bad_values_existed = 1;
      //}
    //}
  //}
  //if (bad_values_existed) {
    //asfPrintWarning("Float image contained extra-negative values (< -10e10) that may\n"
        //"result in inaccurate image statistics.\n");
    //asfPrintStatus("Extra-negative values found within the float image have been removed\n");
  //}

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

  //strcpy(mg->bands, MAGIC_UNSET_STRING);
  mg->line_count = height;
  mg->sample_count = width;

  mg->start_line = 0;
  mg->start_sample = 0;

  if (!geotiff_data_exists && auxDataExists) { // If the data was read from the aux file
    mg->x_pixel_size = arcgisMapInfo.pixelSize.width;
    mg->y_pixel_size = arcgisMapInfo.pixelSize.height;
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

  // Coordinates of tie point in pseudoprojection space.
  // NOTE: These are called tp_lon and tp_lat ...but the tie points
  // will be in either linear units (meters typ.) *OR* lat/long depending
  // on what type of image data is in the file, e.g. map-projected or
  // geographic respectively.
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

  if (!geotiff_data_exists && auxDataExists) { // Data came from .aux file
    mg->re_major = arcgisProjParms.proSpheroid.a;
    mg->re_minor = arcgisProjParms.proSpheroid.b;
  }
  else {
    if (datum != UNKNOWN_DATUM) { // Data came successfully from TIFF file
      spheroid_type_t spheroid = datum_spheroid (datum);
      spheroid_axes_lengths (spheroid, &mg->re_major, &mg->re_minor);
    }
    else {
      mg->re_major = MAGIC_UNSET_DOUBLE;
      mg->re_minor = MAGIC_UNSET_DOUBLE;
    }
  }

  // NOTE: The arcgisProjParms structure is populated either from the GeoTIFF or
  // the .aux file at this point, so no need to check geotiff_data_exists etc
  switch (projection_type) {
    case UTM:     // Universal Transverse Mercator (UTM)
      mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      mp->param.utm.zone = arcgisProjParms.proZone;
      mp->param.utm.false_easting =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.false_northing =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.lat0 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.lon0 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.utm.scale_factor = scale_factor;
      break;
    case ALBERS:  // Albers Equal Area Conic (aka Albers Conical Equal Area)
      mp->type = ALBERS_EQUAL_AREA;
      mp->param.albers.std_parallel1 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.std_parallel2 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL2] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL2] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.center_meridian =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.orig_latitude =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.false_easting =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.albers.false_northing =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      break;
    case LAMCC:   // Lambert Conformal Conic
      mp->type = LAMBERT_CONFORMAL_CONIC;
      mp->param.lamcc.plat1 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.plat2 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL2] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL2] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.lat0 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.lon0 =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.false_easting =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.false_northing =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamcc.scale_factor = scale_factor;
      break;
    case PS:      // Polar Stereographic
      mp->type = POLAR_STEREOGRAPHIC;
      mp->param.ps.slat =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_STD_PARALLEL1] :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.slon =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.is_north_pole =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          (  (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] > 0) ? 1 : 0)   :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.false_easting =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.ps.false_northing =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      // Note: A GeoTIFF (.tif) file, if that's where the parameters came from,
      // does have a scale factor tag in it for Polar Stereographic.  We do not
      // use it however.  It does not exist in the meta data struct and is also
      // not written to the meta data file with meta_write().  See the GeoTIFF
      // Standard, key ProjScaleAtNatOriginGeoKey for polar stereographic.
      break;
    case LAMAZ:   // Lambert Azimuthal Equal Area
      mp->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
      mp->param.lamaz.center_lon =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_CENTRAL_MERIDIAN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamaz.center_lat =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_LAT_ORIGIN] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamaz.false_easting =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_EASTING] :
          MAGIC_UNSET_DOUBLE;
      mp->param.lamaz.false_northing =
          (arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] != MAGIC_UNSET_DOUBLE) ?
          R2D*arcgisProjParms.proParams[ARCGIS_PROJPARAMS_FALSE_NORTHING] :
          MAGIC_UNSET_DOUBLE;
      break;
    default:
      break;
  }

  if (!geotiff_data_exists && auxDataExists) { // Data came from .aux file
    mp->startX = arcgisMapInfo.upperLeftCenter.x - (arcgisMapInfo.pixelSize.width / 2.0);
    mp->startY = arcgisMapInfo.upperLeftCenter.y + (arcgisMapInfo.pixelSize.height / 2.0);
  }
  else {
    mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
    mp->startY = (0.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;
  }
  mp->perX = mg->x_pixel_size;
  mp->perY = -mg->y_pixel_size;

  if (!geotiff_data_exists && auxDataExists) { // Data came from .aux file
    strcpy (mp->units, arcgisMapInfo.units);
  }
  else {
    char *units = (linear_units == Linear_Meter) ? "meters" : MAGIC_UNSET_STRING;
    strcpy (mp->units, units);
  }

  if (!geotiff_data_exists && auxDataExists) { // Data came from .aux file
    mp->spheroid = arcgisSpheroidName2spheroid(arcgisProjParms.proSpheroid.sphereName);
  }
  else {
    if (datum != UNKNOWN_DATUM) {
      mp->spheroid = datum_spheroid (datum);
    } // else leave it at initial value
  }

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

  mp->datum = datum; // NOTE: This is not actually written to the .meta file

  asfPrintStatus("\nGathering image statistics...\n");
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
  mp->param.utm.lat0 = 0.0;
  mp->param.utm.lon0 = utm_zone_to_central_meridian(mp->param.utm.zone);
  mp->hem = mg->center_latitude > 0.0 ? 'N' : 'S';

  if (msar)
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

  double lat, lon;
  proj_to_latlon(&proj, mp->startX, mp->startY, 0.0,
                 &lat, &lon, &dummy_var);
  ml->lat_start_near_range = R2D*lat; //mp->startX;
  ml->lon_start_near_range = R2D*lon; //mp->startY;

  proj_to_latlon(&proj, mp->startX + mp->perX * width, mp->startY, 0.0,
                 &lat, &lon, &dummy_var);
  ml->lat_start_far_range = R2D*lat; //mp->startX + mp->perX * width;
  ml->lon_start_far_range = R2D*lon; //mp->startY;

  proj_to_latlon(&proj, mp->startX, mp->startY + mp->perY * height, 0.0,
                 &lat, &lon, &dummy_var);
  ml->lat_end_near_range = R2D*lat; //mp->startX;
  ml->lon_end_near_range = R2D*lon; //mp->startY + mp->perY * height;

  proj_to_latlon(&proj, mp->startX + mp->perX * width, mp->startY + mp->perY * height, 0.0,
                 &lat, &lon, &dummy_var);
  ml->lat_end_far_range = R2D*lat; //mp->startX + mp->perX * width;
  ml->lon_end_far_range = R2D*lon; //mp->startY + mp->perY * height;

  asfPrintStatus("\nWriting new '.meta' and '.img' files...\n");
  int return_code = write_meta_and_img (outBaseName, meta_out, image);
  asfRequire (return_code == 0,
	      "Failed to write new '.meta' and '.img' files.\n");

  // We're now done with the data and metadata.
  GTIFFree(input_gtif);
  XTIFFClose(input_tiff);
  meta_free (meta_out);
  float_image_free (image);

  // We must be done with the citation string too :)
  FREE (citation);
}

/***** Returns a numeric value that represents the type of projection *****/
/*     coordinate system exists in the ArcGIS geotiff file, according     */
/*     to the ArcGIS geotiff metadata (.aux) file contents                */
/*                                                                        */
/*     Albers Equal Area Conic == 3  (ALBERS in proj.h)                   */
/*     Lambert Azimuthal Equal Area == 11  (LAMAZ in proj.h)              */
/*     Lambert Conformal Conic == 4  (LAMCC in proj.h)                    */
/*     Polar Stereographic == 6  (PS in proj.h)                           */
/*     Universal Transverse Mercator == 1  (UTM in proj.h)                */
/*                                                                        */
short getArcgisProjType(const char *auxFile) {
  short nodeFound;
  short projType;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  //arcgisEimg_MapInformation_t arcgisEimg_MapInformation;
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  unsigned long proType;
  unsigned long proNumber;
  int i, j;
  FILE *fp;

  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    for (j=0; j<MAX_EHFA_ITEMS_PER_OBJECT; j++) {
      ddObjects[i].ddItems[j].number = 0;
      ddObjects[i].ddItems[j].indirectData = '\0';
      strcpy(ddObjects[i].ddItems[j].name, "");
      ddObjects[i].ddItems[j].dataType = '\0';
      strcpy(ddObjects[i].ddItems[j].definedTypeName, "");
      ddObjects[i].ddItems[j].enumNames = NULL;
      ddObjects[i].ddItems[j].numEnums = 0;
      strcpy(ddObjects[i].ddItems[j].prevTypeName, "");
      ddObjects[i].ddItems[j].nestedItems = NULL;
      ddObjects[i].ddItems[j].numNestedItems = 0;
    }
    ddObjects[i].numItems = 0;
    strcpy(ddObjects[i].objStr, "");
    strcpy(ddObjects[i].objName, "");
    ddObjects[i].prev = NULL;
    ddObjects[i].next = NULL;
  }

  fp = fopen(auxFile, "r");
  asfRequire(fp != NULL,
             "Error opening input ArcGIS metadata (.aux) file.\n");

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp((char*)hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "ArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "ArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  projType = DHFA_UNKNOWN_PROJECTION;
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_PROPARAMETERS, &foundNode); // do a get, but via a search
  if (nodeFound) {
    /* NOTE: proType is an index meaning either "EPRJ_INTERNAL" or     */
    /* "EPRJ_EXTERNAL" in an enumerated type.  The proNumber name      */
    /* however is the projection type, by numerical ID, that we want.  */
    /* NOTE: I used the Eprj_ProParameters data element naming here... */
    DHFAGetIntegerValFromOffset(fp, foundNode.data, (long*)&proType, _EMIF_T_ENUM);
    DHFAGetIntegerVal(fp, (long*)&proNumber, _EMIF_T_LONG);
    projType = (short)proNumber;
  }

  // If the Eprj_ProParameters data node was nonexistent, then the
  // Eimg_MapInformation node usually is ...try to find the projection
  // type from Eimg_MapInformation instead
  if (!nodeFound) {
    nodeFound = FindNode (fp, &rootNode, EIMG_MAPINFORMATION, &foundNode); // do a get, but via a search
    if (nodeFound) {
      // TODO: readArcgisEimg_MapInformation remains UNTESTED.
      // So far, I have not been able to locate an aux file
      // that has the Eimg_MapInformation data node in it.
      //
      // ...Just allow the projType to remain 'unknown'
      /*readArcgisEimg_MapInformation (fp, foundNode.data,
      &arcgisEimg_MapInformation);*/
    }
  }

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    if (strlen(ddObjects[i].objName) > 0) {
      freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
    }
  }
  fclose(fp);

  return projType;
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


/***** GetAuxHeader()                                          *****/
/*                                                                 */
/* GetAuxHeader() reads the HFA file header at offset 0x00 in the  */
/* file.  This header contains a pointer to the 'data header'      */
/* which in turn contains pointers to the data dictionary and the  */
/* root of the data tree stored in the file.                       */
/*                                                                 */
void GetAuxHeader(FILE *fp, _Ehfa_HeaderTag* hdr)
{
  char* pTmpUlong;
  char* pTmpUchar;

  /* Allocate temporary variables */
  pTmpUlong = MALLOC(EMIF_T_ULONG_LEN);
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Go to beginning of file */
  fseek(fp, 0L, SEEK_SET);

  /* Read file header label tag */
  DHFAGetString(fp, strlen(EHFA_HEADER_TAG)+1, hdr->label);

  /* Read offset to data header */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  hdr->headerPtr = *((unsigned long*) pTmpUlong);

  /* Clean up */
  FREE(pTmpUlong);
  FREE(pTmpUchar);
}

/***** GetDataHeader()                                         *****/
/*                                                                 */
/* GetDataHeader() reads data access and data dictionary info from */
/* the 'data header', including pointers (offsets) to free data    */
/* storage area within the file and the root entry of the tree of  */
/* data elements                                                   */
/*                                                                 */
void GetDataHeader(FILE *fp, _Ehfa_File* dhdr, _Ehfa_HeaderTag* hdr)
{
  char* pTmpUlong;
  char* pTmpLong;
  char* pTmpSint;

  /* Allocate temporary variables */
  pTmpUlong = MALLOC(EMIF_T_ULONG_LEN);
  pTmpLong = MALLOC(EMIF_T_LONG_LEN);
  pTmpSint = MALLOC(EMIF_T_SHORT_LEN);

  /* Go to data header offset */
  fseek(fp, hdr->headerPtr, SEEK_SET);

  /* Read version number of ERDAS MIF file, should be '1' */
  DHFAfread(pTmpLong, EMIF_T_LONG_LEN, fp);
  dhdr->version = *((long*)pTmpLong);

  /* Read offset to free data locations list */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  dhdr->freeList = *((unsigned long*) pTmpUlong);

  /* Read offset to root entry in data tree */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  dhdr->rootEntryPtr = *((unsigned long*) pTmpUlong);

  /* Read length of header portion of data nodes */
  DHFAfread(pTmpSint, EMIF_T_SHORT_LEN, fp);
  dhdr->entryHeaderLength = *((short int*) pTmpSint);

  /* Read offset to data dictionary at end of file */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  dhdr->dictionaryPtr = *((unsigned long*) pTmpUlong);

  /* Clean up */
  FREE(pTmpUlong);
  FREE(pTmpLong);
  FREE(pTmpSint);
}

/* DHFAswab() mimics swab() by copying 'numBytes' from the array 'from'   */
/* to the array pointed to by 'to', but rather than just exchanging even  */
/* and odd adjacent bytes it reverses the entire stream of bytes during   */
/* the copy from 'from' to 'to'.  This makes the function work for any    */
/* length of binary value read from a file, not just 2-byte values.       */
/* NOTE: This function does nothing when 'numBytes' is negative, but      */
/* unlike swab(), this function DOES work on odd-length ('numBytes' is an */
/* odd number) values.                                                    */
/*                                                                        */
void DHFAswab(char *from, char *to, unsigned int numBytes)
{
  int i, j;

  for (i=0, j=numBytes-1;
       numBytes>0 && i<numBytes;
       i++, j--)
  {
    to[i] = from[j];
  }
}

/* DHFAfread() mimics fread() but reads one element 'size' bytes          */
/* long from the stream pointed to by 'stream', storing them at the       */
/* location given by 'ptr'.  In addition, DHFAfread() determines if the   */
/* local machine is little-endian or big-endian and swabs the bytes if    */
/* necessary (see notes.)                                                 */
/*                                                                        */
/* NOTE: The ERDAS MIF HFA file format is always in little-endian byte    */
/* order and the bytes should therefore be swabbed upon read.  The        */
/* standard fread() however, will swab() bytes already if the local       */
/* machine is little-endian.  Therefore we need to be careful to only     */
/* swab() the bytes read from the file in the case that the local machine */
/* is a big-endian machine (no automatic swabbing by fread())             */
/*                                                                        */
void DHFAfread(char *ptr, size_t size, FILE *fp)
{
  char *pTmp[2];
  char failed = 0;
  int bytesRead;
  int i;

  /* Allocate temporary storage */
  for (i=0; i<2; i++) {
    pTmp[i] = MALLOC(size);
  }

  /* Read the bytes from the file */
  bytesRead = fread(pTmp[0], 1, size, fp);
  if (bytesRead < size) {
    failed = 1;
  }

  /* swab() bytes on big-endian machines since fread() didn't */
  if (!failed) {
    if (local_machine_is_little_endian() == 0) {
      DHFAswab(pTmp[0], pTmp[1], size);
      for (i=0; i<size; i++) ptr[i] = (pTmp[1])[i];
    }
    else {
      for (i=0; i<size; i++) ptr[i] = (pTmp[0])[i];
    }
  }
  else {
    *ptr = 0; /* Default to a zero value if the read failed */
  }

  /* Clean up */
  for (i=0; i<2; i++) {
    FREE(pTmp[i]);
  }
}

/***** GetNode()                                               *****/
/*                                                                 */
/* GetNode(), given an offset to the node, reads one data node     */
/* in the data tree stored in the HFA file.  The data node         */
/* includes type info and pointers (file offsets) to related nodes */
/* in the file.  The actual data is separately accessed via the    */
/* the data pointer (offset) and type information derived from the */
/* node's type info and a look-up in the data dictionary.          */
/*                                                                 */
void GetNode(FILE *fp, unsigned long nodeOffset, _Ehfa_Entry *nodeEntry)
{
  char* pTmpUlong;
  char* pTmpLong;
  unsigned char* pTmpUchar;

  /* Allocate temporary variables */
  pTmpUlong = MALLOC(EMIF_T_ULONG_LEN);
  pTmpLong = MALLOC(EMIF_T_LONG_LEN);
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Go to node offset */
  fseek(fp, nodeOffset, SEEK_SET);

  /* Read 'next child' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->next = *((unsigned long*)pTmpUlong);

  /* Read 'previous child' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->prev = *((unsigned long*)pTmpUlong);

  /* Read 'parent node' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->parent = *((unsigned long*)pTmpUlong);

  /* Read 'child' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->child = *((unsigned long*)pTmpUlong);

  /* Read 'data record' pointer (file offset) */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->data = *((unsigned long*)pTmpUlong);

  /* Read 'data record size' (number of bytes in data record) */
  DHFAfread(pTmpLong, EMIF_T_LONG_LEN, fp);
  nodeEntry->dataSize = *((long*)pTmpLong);

  /* Read 'node name' string */
  DHFAGetString(fp, MAX_EHFA_ENTRY_NAMESTRING_LEN,
                (unsigned char*)nodeEntry->name);

  /* Read 'data type' string */
  DHFAGetString(fp, MAX_EHFA_ENTRY_TYPESTRING_LEN,
                (unsigned char*)nodeEntry->type);

  /* Read 'time node last modified' */
  DHFAfread(pTmpUlong, EMIF_T_ULONG_LEN, fp);
  nodeEntry->modTime = *((long*)pTmpUlong);

  /* Clean up */
  FREE(pTmpUlong);
  FREE(pTmpLong);
  FREE(pTmpUchar);
}

/***** DHFAGetStringFromOffset()                               *****/
/*                                                                 */
/* DHFAGetStringFromOffset() - a utility function.  This function  */
/* reads a fixed-length ASCII string (not stopping due to the      */
/* presence or lack of a '\0\ character) from any file given a     */
/* file offset to where the string resides.                        */
/*                                                                 */
void DHFAGetStringFromOffset(FILE *fp, unsigned char strOffset,
                             unsigned int maxSize, unsigned char *str)
{
  /* Go to node offset */
  fseek(fp, strOffset, SEEK_SET);

  /* Read the string from the file */
  DHFAGetString(fp, maxSize, str);
}

/***** DHFAGetString()                                         *****/
/*                                                                 */
/* DHFAGetString() - a utility function.  This function reads a    */
/* fixed-length ASCII string (not stopping due to the presence or  */
/* lack of a '\0\ character) from any file starting at the current */
/* file position associated with a FILE *fp.                       */
/*                                                                 */
void DHFAGetString(FILE *fp, unsigned int maxSize, unsigned char *str)
{
  unsigned char* pTmpUchar; /* Initialize to anything but '\0' */
  int i;

  /* Allocate temporary variables */
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Read the string from the file (up to and including any '\0's)   */
  /* NOTE: ASCII strings in ERDAS MIF HFA files are fixed length,    */
  /* so do not terminate the loop upon finding a '\0'.  It is up     */
  /* to the writer of the file to make sure that nulls are           */
  /* inserted after the last valid character if they choose to,      */
  /* otherwise it is up to the reader to properly terminate a string */
  /* read from the file.                                             */
  for (i = 0; i < maxSize; i++) {
    fread(pTmpUchar, 1, EMIF_T_UCHAR_LEN, fp);
    if (feof(fp))
      break;
    str[i] = (unsigned char) *pTmpUchar;
  }
  str[maxSize-1] = '\0'; /* Terminate the string (to be safe) */

  /* Clean up */
  FREE(pTmpUchar);
}

/***** DHFAGetIntegerValFromOffset()                           *****/
/*                                                                 */
void DHFAGetIntegerValFromOffset(FILE *fp, unsigned long offset,
                              long *val, char type)
{
  /* Go to file offset and read the WORD value */
  fseek(fp, offset, SEEK_SET);
  DHFAGetIntegerVal(fp, val, type);
}

/***** DHFAGetIntegerVal()                                     *****/
/*                                                                 */
void DHFAGetIntegerVal(FILE *fp, long *val, char type)
{
  char* pTmpWord;

  /* Just in case the definitions change, use the type lengths */
  /* for the fread() etc.                                      */
  switch (type) {
    case _EMIF_T_UCHAR:
      pTmpWord = MALLOC(EMIF_T_UCHAR_LEN);
      DHFAfread(pTmpWord, EMIF_T_UCHAR_LEN, fp);
      *val = *((unsigned char*) pTmpWord);
      break;
    case _EMIF_T_CHAR:
      pTmpWord = MALLOC(EMIF_T_CHAR_LEN);
      DHFAfread(pTmpWord, EMIF_T_CHAR_LEN, fp);
      *val = *((char*) pTmpWord);
      break;
    case _EMIF_T_ENUM:
      pTmpWord = MALLOC(EMIF_T_ENUM_LEN);
      DHFAfread(pTmpWord, EMIF_T_ENUM_LEN, fp);
      *val = *((unsigned short*) pTmpWord);
      break;
    case _EMIF_T_USHORT:
      pTmpWord = MALLOC(EMIF_T_USHORT_LEN);
      DHFAfread(pTmpWord, EMIF_T_USHORT_LEN, fp);
      *val = *((unsigned short*) pTmpWord);
      break;
    case _EMIF_T_SHORT:
      pTmpWord = MALLOC(EMIF_T_SHORT_LEN);
      DHFAfread(pTmpWord, EMIF_T_SHORT_LEN, fp);
      *val = *((short*) pTmpWord);
      break;
    case _EMIF_T_TIME:
      pTmpWord = MALLOC(EMIF_T_ULONG_LEN);
      DHFAfread(pTmpWord, EMIF_T_ULONG_LEN, fp);
      *val = *((unsigned long*) pTmpWord);
      break;
    case _EMIF_T_ULONG:
      pTmpWord = MALLOC(EMIF_T_ULONG_LEN);
      DHFAfread(pTmpWord, EMIF_T_ULONG_LEN, fp);
      *val = *((unsigned long*) pTmpWord);
      break;
    case _EMIF_T_LONG:
      pTmpWord = MALLOC(EMIF_T_LONG_LEN);
      DHFAfread(pTmpWord, EMIF_T_LONG_LEN, fp);
      *val = *((long*) pTmpWord);
      break;
    default:
      *val = 0;
      break;
  }

  /* Clean up */
  FREE(pTmpWord);
}

/***** DHFAGetDoubleValFromOffset()                            *****/
/*                                                                 */
void DHFAGetDoubleValFromOffset(FILE *fp, unsigned long offset, double *val)
{
  /* Go to file offset and read the WORD value */
  fseek(fp, offset, SEEK_SET);
  DHFAGetDoubleVal(fp, val);
}

/***** DHFAGetDoubleVal()                                      *****/
/*                                                                 */
void DHFAGetDoubleVal(FILE *fp, double *val)
{
  char* pTmpVal;

  /* Just in case the definitions change, use the type lengths */
  /* for the fread() etc.                                      */
  pTmpVal = MALLOC(EMIF_T_DOUBLE_LEN);
  DHFAfread(pTmpVal, EMIF_T_DOUBLE_LEN, fp);
  *val = *((double*) pTmpVal);
  FREE(pTmpVal);
}

/***** DHFAGetFloatValFromOffset()                            *****/
/*                                                                 */
void DHFAGetFloatValFromOffset(FILE *fp, unsigned long offset, float *val)
{
  /* Go to file offset and read the WORD value */
  fseek(fp, offset, SEEK_SET);
  DHFAGetFloatVal(fp, val);
}

/***** DHFAGetFloatVal()                                      *****/
/*                                                                 */
void DHFAGetFloatVal(FILE *fp, float *val)
{
  char* pTmpVal;

  /* Just in case the definitions change, use the type lengths */
  /* for the fread() etc.                                      */
  pTmpVal = MALLOC(EMIF_T_DOUBLE_LEN);
  DHFAfread(pTmpVal, EMIF_T_FLOAT_LEN, fp);
  *val = *((float*) pTmpVal);
  FREE(pTmpVal);
}

void DHFAGetStringValFromOffset(FILE *fp, unsigned long offset,
                                unsigned long strLen, char *str)
{
  if (strLen > 0) {
    fseek(fp, offset, SEEK_SET);
    DHFAGetStringVal(fp, strLen, str);
  }
  else {
    str = NULL;
  }
}

void DHFAGetStringVal(FILE *fp, unsigned long strLen, char *str)
{
  unsigned long i;
  char *pTmpStr;

  if (strLen > 0) {
    pTmpStr = MALLOC(strLen);
    for (i=0; i<strLen; i++) {
      fread(&pTmpStr[i], 1, 1, fp);
    }
    strncpy(str, pTmpStr, strLen);
    str[strLen] = '\0';
  }
  else {
    str = NULL;
  }
  if (pTmpStr != NULL)
    FREE(pTmpStr);
}

/***** GetDataDictionary()                                     *****/
/*                                                                 */
/* GetDataDictionary() reads the MIF HFA type data dictionary      */
/* (an ASCII string w/o nulls) from an HFA file given a file       */
/* offset.                                                         */
/*                                                                 */
void GetDataDictionary(FILE *fp, unsigned long ddOffset,char **dd)
{
  unsigned char* pTmpUchar;
  unsigned long endPos;
  unsigned long ddLen;
  int level;

  /* Allocate temporary variables */
  pTmpUchar = MALLOC(EMIF_T_UCHAR_LEN);

  /* Count the number of bytes in the data dictionary */
  /* NOTE: The data dictionary exists at the end of the file but   */
  /* is of unknown length.  It starts at ddOffset and ends at EOF. */
  fseek(fp,0, SEEK_END);
  endPos = ftell(fp);
  ddLen = endPos - ddOffset;

  /* Allocate the data dictionary and read the data dictionary from */
  /* the file into it.                                              */
  *dd = MALLOC(1 + sizeof(unsigned char) * ddLen);
  level = 0; // Level of nesting in {} pairs
//  fseek(fp, ddOffset, SEEK_SET);

  /* Read the dictionary (a string) */
  DHFAGetStringValFromOffset(fp, ddOffset, ddLen, *dd);
  char *substr = strstr(*dd, ",.");
  if (substr != NULL && substr != *dd) {
    substr++;
    substr++;
    *substr = '\0';
  }
  else {
    asfPrintWarning("ArcGIS metadata (.aux) file's data dictionary may be\n"
        "corrupt.  Dictionary is not properly terminated.\n");
    (*dd)[ddLen] = '\0';
  }

  /* Clean up */
  FREE(pTmpUchar);
}

/***** ParseDictionary()                                           *****/
/*                                                                     */
/* ParseDictionary(...) parses the data dictionary (ASCII string)      */
/* from the MIF HFA file into a linked list of ddObjects, and a linked */
/* list of ddItems within each ddObject.                               */
/* NOTE: For convenience here, the linked list of ddObjects is         */
/* allocated as an array (with prev/next ptrs maintained), but all     */
/* code NOT in this function will use the prev/next ptrs to traverse   */
/* the list, e.g. while searching for parameters and freeing memory on */
/* the way out the door (main())                                       */
/*                                                                     */
void ParseDictionary(char *dd, ddObject ddObjects[], int lim)
{
  int numObjects;
  int i;

  /***** Parse the dictionary to produce an array of un-parsed object */
  /* strings.  Example: if the dd is                                  */
  /*                                                                  */
  /*  Example:  If the dd (a character string) is the following,      */
  /*                                                                  */
  /*     "{2:litem1,1:citem2,}obj1,{7:litem3,99:citem4,}obj2,."       */
  /*                                                                  */
  /*     then the goal is to produce an array of objects ready for    */
  /*     parsing items out of, like these:                            */
  /*                                                                  */
  /*     charArray[0].objName = "obj1"                                */
  /*     charArray[0].objStr  = "7:litem3,99:citem4,"                 */
  /*            <etc>                                                 */
  /*                                                                  */
  ParseDictionaryToObjectStrs (dd, ddObjects, &numObjects, lim);

  /***** Parse the data dictionary (string) objects into lists of *****/
  /* items.                                                           */
  /*                                                                  */
  for (i=0; i<numObjects; i++) {
    Parse_ObjectString_to_Items (ddObjects[i].objStr, ddObjects[i].ddItems,
                                 &(ddObjects[i].numItems));
  }
}

/* ParseDictionaryToObjectStrs() tokenizes the objects out of the long */
/* data dictionary string into objects (combination of an item list    */
/* and a data type name)                                               */
void ParseDictionaryToObjectStrs (char *dd, ddObject ddObjects[],
                                  int *count, int lim)
{
  ddObject tmpObj;
  char *tdd;
  BOOL foundObj;

  /* Parse out the first object string from the data dictionary */
  tdd = dd;
  *count = 0;
  do {
    foundObj = getObjectToken(&tdd, &tmpObj);

    if (foundObj) {
      strcpy(ddObjects[*count].objStr, tmpObj.objStr);
      strcpy(ddObjects[*count].objName, tmpObj.objName);
      ddObjects[*count].prev = NULL;
      ddObjects[*count].next = NULL;

      if (*count > 0) {
        ddObjects[*count].prev = &ddObjects[*count - 1];
        ddObjects[*count - 1].next = &ddObjects[*count];
      }

      (*count)++;

      /* NOTE: Not allowing *count to equal lim exactly allows some */
      /* buffer in ddObjects for extraneous characters that may     */
      /* result from nested item descriptions (this adds additional */
      /* '{' and '}' characters to the stream)                      */
      asfRequire(*count < lim,
                  "ERROR: Infile contains too many data type objects"
                      " in the data dictionary\n");
    }
    else {
      strcpy(ddObjects[*count].objStr, "");
      strcpy(ddObjects[*count].objName, "");
    }
  } while (foundObj);
}

BOOL getObjectToken(char **tdd, ddObject *tmpObj)
{
  BOOL rtn = 0;
  char *pcTmp;
  char *pcTmp2;
  char *pcTmp3;
  int level;
  int charCount;

  /* Find and copy the object (string) token */
  pcTmp = strchr(*tdd, OPEN_BRACE);
  if (pcTmp != NULL && *pcTmp == OPEN_BRACE) {
    level = 1;
    charCount = 0;
    pcTmp++; /* Move just beyond '{' */
    pcTmp2 = pcTmp;
    while (level > 0 && *pcTmp2 != '\0') {
      switch (*pcTmp2) {
        case OPEN_BRACE:
          level++;
          if (level > 1) {
            charCount++;
          }
          break;
        case CLOSE_BRACE:
          if (level > 1) {
            charCount++;
          }
          level--;
          break;
        default:
          charCount++;
          break;
      }
      if (level > 0 && *pcTmp2 == '\0') {
        asfRequire(0,
                   "getObjectToken() ERROR: Invalid data dictionary record in file\n");
      }
      pcTmp2++;
    }
    /* Save the object token (string) */
    strncpy(tmpObj->objStr, pcTmp, charCount);
    tmpObj->objStr[charCount] = '\0';

    /*** Find the copy the object token's name (data type) ***/
    /* Send the pointer back ready to find a new object token */
    *tdd = strchr(pcTmp2, ',');
    (*tdd)++;

    /*** Find the copy the object token's name (data type) ***/
    pcTmp = strtok_r(pcTmp2, ",", &pcTmp3);
    asfRequire(pcTmp != NULL,
               "getObjectToken() ERROR: Invalid data dictionary record in file\n");
    strcpy(tmpObj->objName, pcTmp);

    rtn = 1;
  }

  return rtn;
}

/* Given a raw object string from the file's data dictionary, */
/* parse out the list of items contained therein ...note that */
/* the object is like a struct, the items are like the        */
/* struct's members, and that the file contains many such     */
/* struct-type data type definitions                          */
void Parse_ObjectString_to_Items (char objString[], ddItem *items, int *numItems)
{
  BOOL tooManyItems = 0;
  BOOL missingName;
  BOOL nestedItemFound;
  char *pcTmp = (char *)objString;
  char *pcTmp2;
  char *pcTmp3;
  char tmpStr[MAX_EHFA_OBJECTSTRING_LEN];
  int itemNo = 0;
  int numChars;
  int i;
  ddItem *item; /* Convenience pointer */
  ddObject tmpObj;

  asfRequire(pcTmp != NULL && *pcTmp != '\0',
             "Parse_ObjectString_to_Items() ERROR: Empty object string\n");

  /* For each item in the object... */
  while (*pcTmp != '\0' && itemNo < MAX_EHFA_ITEMS_PER_OBJECT) {
    item = &(items[itemNo]); /* convenience ptr */

    /* Init prev/next and fields that may remain unused */
    item->numEnums = 0;
    strcpy(item->prevTypeName, "");
    strcpy(item->definedTypeName, "");
    item->nestedItems = NULL;
    item->indirectData = ' ';

    /* Parse out the number of items for this item type */
    while (!isdigit(*pcTmp) && *pcTmp != '\0') pcTmp++;
    pcTmp2 = pcTmp;
    numChars = 0;
    /*   ...Normally the following stops on ':' */
    while (isdigit(*pcTmp2) && *pcTmp2 != '\0') {
      numChars++;
      pcTmp2++;
    }
    strncpy(tmpStr, pcTmp, numChars);
    tmpStr[numChars] = '\0';
    item->number = atoi(tmpStr); /* Zero (0) is a valid result */
    pcTmp = pcTmp2;
    pcTmp++; /* Point at first char past the ':' */

    /* Check for 'indirect data type' and store if necessary */
    if (*pcTmp == '*' || *pcTmp == 'p') {
      item->indirectData = *pcTmp;
      pcTmp++; /* Move to char just past the '*' or 'p' */
    }

    /* Store the data type indicator (a char) */
    asfRequire(validDataType(*pcTmp) != 0,
               "ERROR: Invalid or unsupported data type found in data dictionary\n");
    item->dataType = *pcTmp;
    pcTmp++;
    /* pcTmp now either points at the name of the type, the number of enums in an */
    /* enum type, left brace (for nested, 'defined', types) or the name of a      */
    /* previously-defined type                                                    */

    /* Perform datatype-specific parsing if necessary including recursive call to */
    /* Parse_ObjectString_to_Items() if necessary for nested types                */
    switch (item->dataType)
    {
      case _EMIF_T_ENUM:
        /* Parse out the number of items for this item type */
        while (!isdigit(*pcTmp) && *pcTmp != '\0') pcTmp++;
        pcTmp2 = pcTmp; /* pcTmp and pcTmp2 point at num of enums */
        numChars = 0;
        /*   ...Normally the following stops on ':' */
        while (isdigit(*pcTmp2) && *pcTmp2 != '\0') {
          numChars++;
          pcTmp2++;
        }
        strncpy(tmpStr, pcTmp, numChars);
        tmpStr[numChars] = '\0';
        item->numEnums = atoi(tmpStr); /* Must be integer and grt zero */
        asfRequire(item->numEnums > 0,
                   "ERROR: Found enum type with no members in data dictionary\n");
        pcTmp += numChars + 1; /* Now points at first char of first enum string */
        /* Parse out the enum strings (names of enum types) */
        item->enumNames = (char **)MALLOC(item->numEnums * sizeof(char *));
        asfRequire(item->enumNames != NULL,
                   "Parse_ObjectString_to_Items() ERROR: Memory allocation error\n");
        for (i=0; i < item->numEnums; i++) {
          item->enumNames[i] =
              (char *)MALLOC(MAX_EHFA_ITEMSTRING_LEN * sizeof(char));
          asfRequire(item->enumNames[i] != NULL,
                     "Parse_ObjectString_to_Items() ERROR: Memory allocation error\n");
        }
        strcpy(tmpStr, pcTmp); /* copy remainder of object string for strtok_r() */
        pcTmp2 = strtok_r(tmpStr, ",", &pcTmp3);
        asfRequire(pcTmp2 != NULL && pcTmp2 > 0,
                   "ERROR: Found empty enum element name in data dictionary\n");
        strcpy(item->enumNames[0], pcTmp2);
        numChars = strlen(item->enumNames[0]);
        for (i=1; i < item->numEnums; i++) {
          pcTmp2 = strtok_r(NULL, ",", &pcTmp3);
          asfRequire(pcTmp2 != NULL && pcTmp2 > 0,
                     "ERROR: Found empty enum element name in data dictionary\n");
          strcpy(item->enumNames[i], pcTmp2);
          numChars += strlen(item->enumNames[i]);
        }
        pcTmp += numChars + i; /* Now points at data type name following last enum str */
        break;
      case _EMIF_T_PREDEFINED:
        strcpy(tmpStr, pcTmp); /* copy remainder of object string for strtok_r() */
        pcTmp2 = strtok_r(tmpStr, ",", &pcTmp3); /* get name of predefined type */
        asfRequire(pcTmp2 != NULL && pcTmp2 > 0,
                   "ERROR: Found empty predefined type name in data dictionary\n");
        strcpy(item->prevTypeName, pcTmp2);
        numChars = strlen(item->prevTypeName);
        pcTmp += numChars + 1; /* Now points at data type name following predef data type name */
        break;
      case _EMIF_T_DEFINED:
        /* Allocate an array to hold the nested items */
        item->nestedItems = MALLOC(MAX_EHFA_NESTEDITEMS_PER_ITEM * sizeof(ddItem));
        asfRequire(item->nestedItems != NULL,
                   "ERROR: Parse_ObjectString_to_Items() memory allocation error\n");
        /* The following borrows the getObjectToken() function since a nested item */
        /* has the same format ({...}...,) as an object string.  Upon return, the  */
        /* item string is contained in tmpObj.objStr                               */
        nestedItemFound = getObjectToken(&pcTmp, &tmpObj);
        if (nestedItemFound) {
          /* Recursive call to Parse_ObjectString_to_Items() to populate  */
          /* the nested item with the item list contained within it.      */
          Parse_ObjectString_to_Items(tmpObj.objStr, item->nestedItems,
                                      &item->numNestedItems);
          strcpy(item->definedTypeName, tmpObj.objName);
        }
        break;
        default: /* Other data types do not need further parsing */
          break;
    }

    /* Parse out & save the datatype name */
    numChars = strlen(pcTmp);
    if (*pcTmp != '\0' && strlen(pcTmp) > 0) {
      strcpy(item->name, strtok_r(pcTmp, DELIM_TYPENAME, &pcTmp2));
      pcTmp += strlen(item->name) + 1; /* Move to next item or '\0' */
      missingName = 0;
    }
    else {
      strcpy(item->name, MISSING_ASCII_DATA);
      missingName = 1;
    }
    if (missingName) {
      asfPrintWarning("ERROR: Parse_ObjectString_to_Items() found missing data type name in\n"
          "item description.  Original object string:\n\n\"%s\"\n",
      objString);
    }

    /* If too many items for allocated storage, then vaMoose */
    itemNo++;
    tooManyItems = (itemNo >= MAX_EHFA_ITEMS_PER_OBJECT) ? 1 : 0;
    if (tooManyItems) {
      asfPrintWarning("Parse_ObjectString_to_Items() found too many items in data\n"
          "dictionary object.  Original object string:\n\n\"%s\"\n",
      objString);
    }
    asfRequire(tooManyItems == 0,
               "ERROR: Too many items in data dictionary object\n");
  }
  *numItems = itemNo;
}

/* Return nonzero if data type character is valid according to the */
/* ERDAS MIF HFA file standard                                     */
int validDataType(char dataType)
{
  switch (dataType) {
    case _EMIF_T_U1:
    case _EMIF_T_U2:
    case _EMIF_T_U4:
    case _EMIF_T_UCHAR:
    case _EMIF_T_CHAR:
    case _EMIF_T_ENUM:
    case _EMIF_T_USHORT:
    case _EMIF_T_SHORT:
    case _EMIF_T_TIME:
    case _EMIF_T_ULONG:
    case _EMIF_T_LONG:
    case _EMIF_T_FLOAT:
    case _EMIF_T_DOUBLE:
    case _EMIF_T_COMPLEX:
    case _EMIF_T_DCOMPLEX:
    case _EMIF_T_BASEDATA:
    case _EMIF_T_PREDEFINED:
    case _EMIF_T_DEFINED:
      return 1;
      break;
    default:
      return 0;
      break;
  }
}

/***** PrintDictionary()                                           *****/
/*                                                                     */
/* PrintDictionary(...) dumps the data dictionary (linked list) to     */
/* stdout in a more understandable human-readable form (prettified)    */
/*                                                                     */
void PrintDictionary(ddObject *ddObjects, char *dd)
{
  int i;
  ddObject *obj;

  printf("\nDATA DICTIONARY (raw, unparsed):"
      "\n====================================");
  printf("\n%s\n", dd);

  printf("\nDATA DICTIONARY (parsed):"
      "\n=============================");
  obj = ddObjects;
  i=0;
  while (obj) {
    printf("\n-------------------\n    OBJECT NO: %d\n", i+1);
    printf("  OBJECT NAME: \"%s\"\n", obj->objName);
    printf("Number of Items: %d\n", obj->numItems);
    PrintItems(obj->ddItems, obj->numItems, 1);
    obj = obj->next;
    i++;
  }
}

void PrintItems(ddItem *items, int numItems, int tabLevel)
{
  int i, j; /* loop vars */
  char tabs[TABSTRING_LEN]; /* leading indent */
  char type[32]; /* Item type name string */

  /* Build leading whitespace string for indent */
  strcpy(tabs, "");
  strncat(tabs, TABSTRING, (tabLevel * TAB_LEN > TABSTRING_LEN - 2) ?
      TABSTRING_LEN - 2 : tabLevel * TAB_LEN);
  for (i=0; i<numItems; i++) {
    /* Assign linguistic term to data type */
    switch (items[i].dataType) {
      case _EMIF_T_U1:
        strcpy(type, EMIF_T_U1_STR);
        break;
      case _EMIF_T_U2:
        strcpy(type, EMIF_T_U2_STR);
        break;
      case _EMIF_T_U4:
        strcpy(type, EMIF_T_U4_STR);
        break;
      case _EMIF_T_UCHAR:
        strcpy(type, EMIF_T_UCHAR_STR);
        break;
      case _EMIF_T_CHAR:
        strcpy(type, EMIF_T_CHAR_STR);
        break;
      case _EMIF_T_ENUM:
        strcpy(type, EMIF_T_ENUM_STR);
        break;
      case _EMIF_T_USHORT:
        strcpy(type, EMIF_T_USHORT_STR);
        break;
      case _EMIF_T_SHORT:
        strcpy(type, EMIF_T_SHORT_STR);
        break;
      case _EMIF_T_TIME:
        strcpy(type, EMIF_T_TIME_STR);
        break;
      case _EMIF_T_ULONG:
        strcpy(type, EMIF_T_ULONG_STR);
        break;
      case _EMIF_T_LONG:
        strcpy(type, EMIF_T_LONG_STR);
        break;
      case _EMIF_T_FLOAT:
        strcpy(type, EMIF_T_FLOAT_STR);
        break;
      case _EMIF_T_DOUBLE:
        strcpy(type, EMIF_T_DOUBLE_STR);
        break;
      case _EMIF_T_COMPLEX:
        strcpy(type, EMIF_T_COMPLEX_STR);
        break;
      case _EMIF_T_DCOMPLEX:
        strcpy(type, EMIF_T_DCOMPLEX_STR);
        break;
      case _EMIF_T_BASEDATA:
        strcpy(type, EMIF_T_BASEDATA_STR);
        break;
      case _EMIF_T_PREDEFINED:
        strcpy(type, EMIF_T_PREDEFINED_STR);
        break;
      case _EMIF_T_DEFINED:
        strcpy(type, EMIF_T_DEFINED_STR);
        break;
      default:
        strcpy(type, "Undefined type");
        break;
    }

    /* Dump to stdout */
    printf("\n%sITEM DESCRIPTION:\n", tabs);
    printf("%s  name: \"%s\"\n", tabs, items[i].name);
    printf("%s  number: %d\n", tabs, items[i].number);
    if (items[i].indirectData == '*' || items[i].indirectData == 'p') {
      printf("%s  indirection type: '%c'\n", tabs, items[i].indirectData);
    }
    else {
      printf("%s  indirection type: none\n", tabs);
    }
    printf("%s  data type: '%c' (%s)\n", tabs, items[i].dataType, type);
    if (items[i].dataType == _EMIF_T_DEFINED) {
      printf("%s  defined type name: \"%s\"\n", tabs, items[i].definedTypeName);
    }
    else{
      printf("%s  defined type name: none\n", tabs);
    }
    printf("%s  number of enums: %d\n", tabs, items[i].numEnums);
    for (j=0; j<items[i].numEnums; j++){
      printf("%s      \"%s\"\n", tabs, items[i].enumNames[j]);
    }
    printf("%s  number of nested items: %d\n", tabs, items[i].numNestedItems);
    if (items[i].numNestedItems > 0) {
      printf("%s  NESTED ITEMS:\n", tabs);
      PrintItems(items[i].nestedItems, items[i].numNestedItems, tabLevel + 2);
    }
    else{
      printf("%s  NESTED ITEMS: none\n", tabs);
    }
    if (items[i].dataType == _EMIF_T_PREDEFINED) {
      printf("%s  previous type defn: \"%s\"\n", tabs, items[i].prevTypeName);
    }
    else{
      printf("%s  previous type defn: none\n", tabs);
    }
  }
  printf("\n");
}

/***** traverseNodes()                                                *****/
/*                                                                        */
void traverseNodes(FILE *fp, _Ehfa_Entry *node, unsigned long nodeOffset, BOOL dumpFlag)
{
  _Ehfa_Entry newNode;

  if (dumpFlag) {
    printDataNode(node, nodeOffset);
  }
  if (node->child) {
    GetNode(fp, node->child, &newNode);
    asfRequire(newNode.prev == (unsigned long) 0,
               "ERROR: traverseNodes() found a child with non-NULL prev pointer\n");
    traverseNodes(fp, &newNode, node->child, dumpFlag);
  }
  if (node->next) {
    GetNode(fp, node->next, &newNode);
    traverseNodes(fp, &newNode, node->next, dumpFlag);
  }
}

void freeItems(ddItem *items, int numItems)
{
  int i;
  for (i=0; i<numItems; i++) {
    freeOneItem(&items[i]);
  }
}

void freeOneItem(ddItem *item)
{
  int i;
  ddItem *_nestedItems;

  /* NOTE: The array of objects declared in main has no malloc()'d            */
  /* memory but each object contains an array of ddItems.  ddItems have no    */
  /* malloc()'d memory unless the type includes enums or nested items.        */
  /* Arrays of enum strings are malloc()'d as are arrays of ddItems           */
  /* for the nested items if applicable. For the case of nested items, note   */
  /* that freeOneItem() is called recursively for each item in order to       */
  /* handle the situation where items are nested in items to some unknown     */
  /* level.                                                                   */
  /*                                                                          */

  /* Free the enums if they exist */
  if (item->dataType == _EMIF_T_ENUM && item->numEnums > 0) {
    for (i=0; i<item->numEnums; i++) {
      FREE(item->enumNames[i]);
    }
    FREE(item->enumNames);
  }

  /* Free the nested types if they exist */
  if (item->dataType == _EMIF_T_DEFINED && item->numNestedItems > 0) {
    _nestedItems = (ddItem *)item->nestedItems;
    for (i=0; i<item->numNestedItems; i++) {
      freeOneItem(&_nestedItems[i]);
    }
    FREE(item->nestedItems);
  }
}

void printDataNode(_Ehfa_Entry *node, unsigned long nodeOffset)
{
  if (strncmp(node->name, "root", 4) != 0) {
    printf("\n    DATA NODE (at offset 0x%04x):\n", (unsigned int) nodeOffset);
    printf("        name: \"%s\"\n", node->name);
    printf("        type: \"%s\"\n", node->type);
    printf("        bytes of data: %ld\n", node->dataSize);
    printf("        data offset: 0x%04x (%ldd)\n", (unsigned int)node->data, node->data);
    printf("        time stamp: %ld\n", node->modTime);
    printf("        parent: 0x%04x (%ldd)\n", (unsigned int)node->parent, node->parent);
    printf("        child: 0x%04x (%ldd)\n", (unsigned int)node->child, node->child);
    printf("        prev: 0x%04x (%ldd)\n", (unsigned int)node->prev, node->prev);
    printf("        next: 0x%04x (%ldd)\n", (unsigned int)node->next, node->next);
  }
}

/***** FindNode()                                                   *****/
/* Performs an in-file recursive pre-order tree traversal to find a     */
/* known type of object, ex: a type "Eprj_ProParameters" data object    */
/*                                                                      */
short FindNode (FILE *fp, _Ehfa_Entry *node, char *type,
                _Ehfa_Entry *foundNode)
{
  short nodeFound = 0;
  _Ehfa_Entry newNode;

  /* Pre-order check on searched-for node */
  /* ...FOUND... STOP LOOKING...          */
  if (strncmp(node->type, type, strlen(type)) == 0) {
    strcpy(foundNode->name, node->name);
    strcpy(foundNode->type, node->type);
    foundNode->dataSize = node->dataSize;
    foundNode->modTime = node->modTime;
    foundNode->data = node->data;
    foundNode->parent = node->parent;
    foundNode->child = node->child;
    foundNode->next = node->next;
    foundNode->prev = node->prev;

    nodeFound = 1;
  }

  /* If not found yet and a child node exists, look further... */
  if (node->child && !nodeFound) {
    GetNode(fp, node->child, &newNode);
    asfRequire(newNode.prev == (unsigned long) 0,
               "FindNode() found a child with non-NULL prev pointer\n");
    nodeFound = FindNode(fp, &newNode, type, foundNode);
  }

  /* If not found yet and a next node exists, look further... */
  if (node->next && !nodeFound) {
    GetNode(fp, node->next, &newNode);
    nodeFound = FindNode(fp, &newNode, type, foundNode);
  }

  /* ...NOT FOUND and NO MORE NODES... Nullify all...       */
  if (!nodeFound) {
    strcpy(foundNode->name, "");
    strcpy(foundNode->type, "");
    foundNode->dataSize = (long)0;
    foundNode->modTime = (unsigned long)0;
    foundNode->data = (unsigned long)0;
    foundNode->parent = (unsigned long)0;
    foundNode->child = (unsigned long)0;
    foundNode->next = (unsigned long)0;
    foundNode->prev = (unsigned long)0;
  }

  return nodeFound;
}

void getArcgisProjParameters(char *infile, arcgisProjParms_t *proParms)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  long offset;
  int i;
  unsigned long strLen;
  unsigned long nElements;

  char    sphereName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  double  a,
          b,
          eSquared,
          radius;
  long    proNumber;
  long    proZone;
  double  proParams[ARCGIS_NUM_PROJDPARAMS];
  unsigned long proType;
  char    proExeName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char    proName[MAX_EHFA_ENTRY_NAMESTRING_LEN];

  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "Error opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  proNumber = (unsigned long)MAGIC_UNSET_INT;
  proZone = (unsigned long)MAGIC_UNSET_INT;
  for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++){
    proParams[i] = MAGIC_UNSET_DOUBLE;
  }
  proType = 0L; // Can't use MAGIC_UNSET_INT because it's a negative number
  strcpy(proExeName, MAGIC_UNSET_STRING);
  strcpy(proName, MAGIC_UNSET_STRING);
  strcpy(sphereName, MAGIC_UNSET_STRING);
  a = MAGIC_UNSET_DOUBLE;
  b = MAGIC_UNSET_DOUBLE;
  eSquared = MAGIC_UNSET_DOUBLE;
  radius = MAGIC_UNSET_DOUBLE;

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp((char*)hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "ArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "ArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_PROPARAMETERS, &foundNode); // do a get, but via a search
  if (nodeFound) {
    // Get proType (enum idx, 0 == 'EPRJ_INTERNAL' and 1 == 'EPRJ_EXTERNAL'
    DHFAGetIntegerValFromOffset(fp, foundNode.data, (long*)&proType, _EMIF_T_ENUM);

    // Get proNumber, e.g. 4 => Lambert Conformal Conic
    DHFAGetIntegerVal(fp, &proNumber, _EMIF_T_LONG);

    // Get proExeName, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    // NOTE: proExeName is the name of an executable that can convert
    // the file between the two proType types listed above, if it exists
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-p
    if (offset > 0) {
      fseek(fp, offset, SEEK_SET);
      DHFAGetIntegerVal(fp, (long*)&strLen, _EMIF_T_ULONG);
      if (strLen > 0) {
        DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
        if (offset > 0) {
          fseek(fp, offset, SEEK_SET);
          DHFAGetStringVal(fp, strLen, proExeName);
        }
      }
    }

    // Get proName, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    // NOTE: proName is the textual name of the projection type, e.g.
    // "Albers Conical Equal Area" or other
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-p
    if (offset > 0) {
      fseek(fp, offset, SEEK_SET);
      DHFAGetIntegerVal(fp, (long*)&strLen, _EMIF_T_ULONG);
      if (strLen > 0) {
        DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
        if (offset > 0) {
          fseek(fp, offset, SEEK_SET);
          DHFAGetStringVal(fp, strLen, proName);
        }
      }
    }

    // Get the proZone (only applies to UTM, but always exists in
    // the file)
    DHFAGetIntegerVal(fp, &proZone, _EMIF_T_LONG);

    // Get the proParams, the array of projection parameters (always
    // exists in the file), DOUBLE-p
    DHFAGetIntegerVal(fp, (long*)&nElements, _EMIF_T_ULONG); // Get num of elements in array
    if (nElements > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Get offset to array
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        for (i=0; i<nElements && i<ARCGIS_NUM_PROJDPARAMS; i++) {
          DHFAGetDoubleVal(fp, &proParams[i]);
        }
      }
    }

    // Get proSpheroid data from file (CHAR-*)
    DHFAGetIntegerVal(fp, (long*)&nElements, _EMIF_T_ULONG); // Get number of spheroids (should be 1)
    if (nElements > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Get offset to spheroid name
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetIntegerVal(fp, (long*)&strLen, _EMIF_T_ULONG); // Get length of spheroid name
        if (strLen > 0) {
          DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
          if (offset > 0) {
            fseek(fp, offset, SEEK_SET);
            DHFAGetStringVal(fp, strLen, sphereName); // Finally get the name!
          }
        }
      }
    }
    DHFAGetDoubleVal(fp, &a);
    DHFAGetDoubleVal(fp, &b);
    DHFAGetDoubleVal(fp, &eSquared);
    DHFAGetDoubleVal(fp, &radius);
  }

  // Populate return struct
  proParms->proNumber = proNumber;
  proParms->proZone = proZone;
  for (i=0; i<ARCGIS_NUM_PROJDPARAMS; i++){
    proParms->proParams[i] = proParams[i];
  }
  proParms->proType = (unsigned short) proType;
  strcpy(proParms->proExeName, proExeName);
  strcpy(proParms->proName, proName);
  strcpy(proParms->proSpheroid.sphereName, sphereName);
  proParms->proSpheroid.a = a;
  proParms->proSpheroid.b = b;
  proParms->proSpheroid.eSquared = eSquared;
  proParms->proSpheroid.radius = radius;

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    if (strlen(ddObjects[i].objName) > 0) {
      freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
    }
  }
  fclose(fp);
}

spheroid_type_t arcgisSpheroidName2spheroid(char *sphereName)
{
  spheroid_type_t rtnVal;

  if (
      strncmp(sphereName,
              ARCGIS_BESSEL_SPHEROID,
              strlen(ARCGIS_BESSEL_SPHEROID)) == 0
     )
  {
    rtnVal = BESSEL_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_CLARKE1866_SPHEROID,
                   strlen(ARCGIS_CLARKE1866_SPHEROID)) == 0
          )
  {
    rtnVal = CLARKE1866_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_CLARKE1880_SPHEROID,
                   strlen(ARCGIS_CLARKE1880_SPHEROID)) == 0
          )
  {
    rtnVal = CLARKE1880_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_GEM6_SPHEROID,
                   strlen(ARCGIS_GEM6_SPHEROID)) == 0
          )
  {
    rtnVal = GEM6_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_GEM10C_SPHEROID,
                   strlen(ARCGIS_GEM10C_SPHEROID)) == 0
          )
  {
    rtnVal = GEM10C_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_GRS1980_SPHEROID,
                   strlen(ARCGIS_GRS1980_SPHEROID)) == 0
          )
  {
    rtnVal = GRS1980_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_INTERNATIONAL1924_SPHEROID,
                   strlen(ARCGIS_INTERNATIONAL1924_SPHEROID)) == 0
          )
  {
    rtnVal = INTERNATIONAL1924_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_INTERNATIONAL1967_SPHEROID,
                   strlen(ARCGIS_INTERNATIONAL1967_SPHEROID)) == 0
          )
  {
    rtnVal = INTERNATIONAL1967_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_WGS72_SPHEROID,
                   strlen(ARCGIS_WGS72_SPHEROID)) == 0
          )
  {
    rtnVal = WGS72_SPHEROID;
  }
  else if (
           strncmp(sphereName,
                   ARCGIS_WGS84_SPHEROID,
                   strlen(ARCGIS_WGS84_SPHEROID)) == 0
          )
  {
    rtnVal = WGS84_SPHEROID;
  }

  return rtnVal;
}

void getArcgisDatumParameters(char *infile, arcgisDatumParms_t *datumParms)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  long offset;
  int i;
  unsigned long strLen;
  unsigned long nElements;

  char          datumname[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  unsigned long type;
  double        params[ARCGIS_NUM_DATUMDPARAMS];
  char          gridname[MAX_EHFA_ENTRY_NAMESTRING_LEN];

  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "Error opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  strcpy(datumname, MAGIC_UNSET_STRING);
  type = 0L;
  for (i=0; i<ARCGIS_NUM_DATUMDPARAMS; i++){
    params[i] = MAGIC_UNSET_DOUBLE;
  }
  strcpy(gridname, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp((char*)hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "ArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "ArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_DATUM, &foundNode); // do a get, but via a search
  if (nodeFound) {
    // Get datumname, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    DHFAGetIntegerValFromOffset(fp, foundNode.data, (long*)&strLen, _EMIF_T_ULONG);
    if (strLen > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetStringVal(fp, strLen, datumname);
      }
    }

    // Get type (enum idx, 0 == 'EPRJ_DATUM_PARAMETRIC',
    // 1 == 'EPRJ_DATUM_GRID', and 2 == 'EPRJ_DATUM_REGRESSION'
    DHFAGetIntegerVal(fp, (long*)&type, _EMIF_T_ENUM);

    // Get the datum double params, the array of datum parameters (always
    // exists in the file), DOUBLE-p
    DHFAGetIntegerVal(fp, (long*)&nElements, _EMIF_T_ULONG); // Get num of elements in array
    if (nElements > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Get offset to array
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        for (i=0; i<nElements && i<ARCGIS_NUM_DATUMDPARAMS; i++) {
          DHFAGetDoubleVal(fp, &params[i]);
        }
      }
    }

    // Get gridname, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-p
    if (offset > 0) {
      fseek(fp, offset, SEEK_SET);
      DHFAGetIntegerVal(fp, (long*)&strLen, _EMIF_T_ULONG);
      if (strLen > 0) {
        DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
        if (offset > 0) {
          fseek(fp, offset, SEEK_SET);
          DHFAGetStringVal(fp, strLen, gridname);
        }
      }
    }
  }

  // Populate return struct
  strcpy(datumParms->datumname, datumname);
  datumParms->type = (unsigned short)type;
  for (i=0; i<ARCGIS_NUM_DATUMDPARAMS; i++){
    datumParms->params[i] = params[i];
  }
  strcpy(datumParms->gridname, gridname);

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    if (strlen(ddObjects[i].objName) > 0) {
      freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
    }
  }
  fclose(fp);
}

void getArcgisMapInfo(char *infile, arcgisMapInfo_t *arcgisMapInfo)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  long offset;
  int i;
  unsigned long strLen;
  unsigned long nElements;

  char                proName[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  arcgisCoordinate_t  upperLeftCenter; // x, y
  arcgisCoordinate_t  lowerRightCenter; // x, y
  arcgisSize_t        pixelSize; // width, height
  char                units[MAX_EHFA_ENTRY_NAMESTRING_LEN];

  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "Error opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  strcpy(proName, MAGIC_UNSET_STRING);
  upperLeftCenter.x = MAGIC_UNSET_DOUBLE;
  upperLeftCenter.y = MAGIC_UNSET_DOUBLE;
  lowerRightCenter.x = MAGIC_UNSET_DOUBLE;
  lowerRightCenter.y = MAGIC_UNSET_DOUBLE;
  pixelSize.width = MAGIC_UNSET_DOUBLE;
  pixelSize.height = MAGIC_UNSET_DOUBLE;
  strcpy(units, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp((char*)hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "ArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "ArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EPRJ_MAPINFO, &foundNode); // do a get, but via a search
  if (nodeFound) {
    // Get proName, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    DHFAGetIntegerValFromOffset(fp, foundNode.data, (long*)&strLen, _EMIF_T_ULONG);
    if (strLen > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to str itself
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetStringVal(fp, strLen, proName);
      }
    }

    // Get upperLeftCenter coordinates
    // ...read number of Eprj_Coordinate elements and offset to first one,
    // then read the doubles.  No need to loop here since the Eprj_MapInfo
    // by default only defines a single upperLeftCenter coordinate
    DHFAGetIntegerVal(fp, (long*)&nElements, _EMIF_T_ULONG);
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
    fseek(fp, offset, SEEK_SET);
    DHFAGetDoubleVal(fp, &upperLeftCenter.x);
    DHFAGetDoubleVal(fp, &upperLeftCenter.y);

    // Get lowerRightCenter coordinates
    DHFAGetIntegerVal(fp, (long*)&nElements, _EMIF_T_ULONG);
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
    fseek(fp, offset, SEEK_SET);
    DHFAGetDoubleVal(fp, &lowerRightCenter.x);
    DHFAGetDoubleVal(fp, &lowerRightCenter.y);

    // Get pixelSize
    DHFAGetIntegerVal(fp, (long*)&nElements, _EMIF_T_ULONG);
    DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG);
    fseek(fp, offset, SEEK_SET);
    DHFAGetDoubleVal(fp, &pixelSize.width);
    DHFAGetDoubleVal(fp, &pixelSize.height);

    // Get units string, first val is a ushort string length and
    // if greater than zero, immediately followed by an offset to
    // the string of characters (otherwise followed by next data item)
    // NOTE: This is a CHAR-* not a CHAR-p, so the first element is
    // the number of characters (followed by an offset to the string)
    DHFAGetIntegerVal(fp, (long*)&strLen, _EMIF_T_ULONG); // Length of units str
    if (strLen > 0) {
      DHFAGetIntegerVal(fp, &offset, _EMIF_T_ULONG); // Offset to CHAR-*
      if (offset > 0) {
        fseek(fp, offset, SEEK_SET);
        DHFAGetStringVal(fp, strLen, units);
      }
    }
  }

  // Populate return struct
  strcpy(arcgisMapInfo->proName, proName);
  arcgisMapInfo->upperLeftCenter.x = upperLeftCenter.x;
  arcgisMapInfo->upperLeftCenter.y = upperLeftCenter.y;
  arcgisMapInfo->lowerRightCenter.x = lowerRightCenter.x;
  arcgisMapInfo->lowerRightCenter.y = lowerRightCenter.y;
  arcgisMapInfo->pixelSize.width = pixelSize.width;
  arcgisMapInfo->pixelSize.height = pixelSize.height;
  strcpy(arcgisMapInfo->units, units);

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    if (strlen(ddObjects[i].objName) > 0) {
      freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
    }
  }
  fclose(fp);
}

void getArcgisEimg_MapInformation (char *infile,
                 arcgisEimg_MapInformation_t *arcgisEimg_MapInformation)
{
  short nodeFound;
  char *dictionary; /* Data dictionary from HFA file */
  int i;
  _Ehfa_HeaderTag hdr; /* File header from offset 0x00 */
  _Ehfa_File dhdr; /* Data header, points to data dictionary and root node */
  _Ehfa_Entry rootNode; /* Root node from embedded data tree */
  _Ehfa_Entry foundNode; /* Data node for desired data */
  ddObject ddObjects[MAX_EHFA_OBJECTS_PER_DICTIONARY]; /* data dictionary objects */
  FILE *fp;

  fp = fopen(infile, "r");
  asfRequire(fp != NULL,
             "Error opening input ArcGIS metadata (.aux) file.\n");

  // Populate values to be read from the file with initial values
  strcpy(arcgisEimg_MapInformation->projection, MAGIC_UNSET_STRING);
  strcpy(arcgisEimg_MapInformation->units, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  GetAuxHeader(fp, &hdr);
  asfRequire(strncmp((char*)hdr.label, "EHFA_HEADER_TAG", 15) == 0,
             "ArcGIS metadata (.aux) file invalid\n");
  GetDataHeader(fp, &dhdr, &hdr);
  asfRequire(dhdr.version == 1,
             "ArcGIS metadata (.aux) file invalid\n");
  /* NOTE: GetDataDictionary() dynamically allocates 'dictionary' with MALLOC() */
  GetDataDictionary(fp, dhdr.dictionaryPtr, &dictionary);
  ParseDictionary(dictionary, ddObjects, MAX_EHFA_OBJECTS_PER_DICTIONARY);

  /* Get root data node and traverse tree to find projection parameters, */
  /* then get projection type to determine parameter list to grab from   */
  /* the file                                                            */
  GetNode(fp, dhdr.rootEntryPtr, &rootNode); // do a get, given an offset
  nodeFound = FindNode (fp, &rootNode, EIMG_MAPINFORMATION, &foundNode); // do a get, but via a search
  if (nodeFound) {
    readArcgisEimg_MapInformation (fp, foundNode.data, arcgisEimg_MapInformation);
  }

  /***** Clean up memory allocations and close the file *****/
  /*                                                        */
  if (dictionary != NULL) {
    FREE(dictionary);
  }
  for (i=0; i<MAX_EHFA_OBJECTS_PER_DICTIONARY; i++) {
    if (strlen(ddObjects[i].objName) > 0) {
      freeItems(ddObjects[i].ddItems, ddObjects[i].numItems);
    }
  }
  fclose(fp);
}

void readArcgisEimg_MapInformation (FILE *fp, unsigned long offset,
                                    arcgisEimg_MapInformation_t *arcgisEimg_MapInformation)
{
  // TODO:  This code is UNTESTED ...so far, I have not been able
  // to find an HFA file that contains a Eimg_MapInformation node
  // in it
  long strOffset;
  unsigned long strLen;
  char  projection[MAX_EHFA_ENTRY_NAMESTRING_LEN];
  char  units[MAX_EHFA_ENTRY_NAMESTRING_LEN];


  // Populate values to be read from the file with initial values
  strcpy(projection, MAGIC_UNSET_STRING);
  strcpy(units, MAGIC_UNSET_STRING);

  /***** Parse header and data dictionary *****/
  /*                                          */
  fseek(fp, offset, SEEK_SET);
  // Get projection string, first val is a ushort string length and
  // if greater than zero, immediately followed by an offset to
  // the string of characters (otherwise followed by next data item)
  DHFAGetIntegerValFromOffset(fp, offset, (long*)&strLen, _EMIF_T_ULONG);
  if (strLen > 0) {
    DHFAGetIntegerVal(fp, &strOffset, _EMIF_T_ULONG); // Offset to str itself
    if (strOffset > 0) {
      fseek(fp, strOffset, SEEK_SET);
      DHFAGetStringVal(fp, strLen, projection);
    }
  }

  // Get units string, first val is a ushort string length and
  // if greater than zero, immediately followed by an offset to
  // the string of characters (otherwise followed by next data item)
  // NOTE: This is a CHAR-* not a CHAR-p, so the first element is
  // the number of characters (followed by an offset to the string)
  DHFAGetIntegerVal(fp, (long*)&strLen, _EMIF_T_ULONG); // Length of units str
  if (strLen > 0) {
    DHFAGetIntegerVal(fp, &strOffset, _EMIF_T_ULONG); // Offset to CHAR-*
    if (strOffset > 0) {
      fseek(fp, strOffset, SEEK_SET);
      DHFAGetStringVal(fp, strLen, units);
    }
  }

  // Populate return struct
  strcpy(arcgisEimg_MapInformation->projection, projection);
  strcpy(arcgisEimg_MapInformation->units, units);
}
