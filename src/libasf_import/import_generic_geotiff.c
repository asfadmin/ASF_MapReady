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
#include "geotiff_support.h"
#include "import_generic_geotiff.h"

#define BAD_VALUE_SCAN_ON

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define IMPORT_GENERIC_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, IMPORT_GENERIC_FLOAT_MICRON))

#define DEFAULT_UTM_SCALE_FACTOR     0.9996

#define DEFAULT_SCALE_FACTOR         1.0
#define UNKNOWN_PROJECTION_TYPE       -1

#define NAD27_DATUM_STR   "NAD27"
#define NAD83_DATUM_STR   "NAD83"
#define HARN_DATUM_STR    "HARN"
#define WGS84_DATUM_STR   "WGS 84"
#define HUGHES_DATUM_STR  "HUGHES"

#define USER_DEFINED_PCS             32767
#define BAND_NAME_LENGTH  12

// If you change the BAND_ID_STRING here, make sure you make an identical
// change in the export library ...the defs must match
#define BAND_ID_STRING "Color Channel (Band) Contents in RGBA+ order"

spheroid_type_t SpheroidName2spheroid(char *sphereName);
void check_projection_parameters(meta_projection *mp);
int  band_float_image_write(FloatImage *oim, meta_parameters *meta_out,
                            const char *outBaseName, int num_bands, int *ignore);
int  band_byte_image_write(UInt8Image *oim_b, meta_parameters *meta_out,
                           const char *outBaseName, int num_bands, int *ignore);
int  get_bands_from_citation(int *num_bands, char **band_str, int *empty, char *citation);

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
  short model_type;
  short raster_type;
  short linear_units;
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
  ret = get_tiff_data_config(input_tiff,
                             &data_type, // ASF datatype, e.g. BYTE or REAL32 etc
                             &num_bands); // Initial number of bands
  if (ret != 0) {
    asfPrintError("Unsupported TIFF type found or required TIFF tags are missing\n"
        "in TIFF File %s\n"
        "TIFFs must contain UINT, INT, or IEEEFP data, be in chunky (RGBRGB...)\n"
        "format rather than having color planes stored separately, and contain\n"
        "no more than %d samples per pixel.\n", inFileName, MAX_BANDS);
  }
  asfPrintStatus("\n   Found %d-banded Generic GeoTIFF with %s type data\n"
      "        (Note: Empty or missing bands will be ignored)\n",
                 num_bands,
                 (data_type == BYTE) ? "8-BIT BYTE" :
                     (data_type == INTEGER16) ? "16-BIT INTEGER" :
                          (data_type == REAL32) ? "32-BIT FLOAT" : "UNKNOWN");
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
      asfPrintStatus("\nCitation: MISSING\n\n");
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
    // the model_type would be ModelTypeGeographic ...but oh well.
    asfPrintError("Only map-projected images using linear units are supported.\n"
        "Geographic (lat/long) images are not.\n");
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
        asfRequire(read_count == 1 && proj_coords_trans != UNKNOWN_PROJECTION_TYPE,
                  "Unable to determine type of projection coordinate system in GeoTIFF file\n");

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
        // FIXME: Hughes datum support... need it!
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
                "file.  Projection parameters will be incomplete.\n");
            break;
        }
      }
    } // End of if UTM else OTHER projection type
  } // End of reading projection parameters from geotiff ...if it existed
  else {
    asfPrintWarning("Projection parameters missing in the GeoTIFF\n"
                "file.  Projection parameters will be incomplete.\n");
  }

  /***** CONVERT TIFF TO BYTE or FLOAT IMAGE *****/
  /*                                             */
  asfPrintStatus("\nConverting input TIFF image into %d-banded %s image...\n\n",
                 num_bands, (data_type == BYTE) ? "byte" : "float");
  UInt8Image *oim_b = NULL;
  FloatImage *oim = NULL;
  if (data_type == BYTE) {
    // Create byte image
    if (num_bands == 1) {
      // Single-band byte image
      oim_b = tiff_to_byte_image(input_tiff);
    }
    else {
      // Multi-band byte image
      oim_b = tiff_to_band_byte_image(input_tiff, num_bands);
    }
    if (oim_b == NULL) {
      asfPrintError("Could not create byte image\n");
    }
  }
  else {
    // Create float image
    if (num_bands == 1) {
      // Single-band float image
      oim = tiff_to_float_image(input_tiff);
    }
    else {
      // Multi-band float image
      oim = tiff_to_band_float_image(input_tiff, num_bands);
    }
    if (oim == NULL) {
      asfPrintError("Could not create float image\n");
    }
  }

  // Get the raster width and height of the image.
  uint32 width;
  uint32 height;
  if (data_type == BYTE) {
    width = oim_b->size_x;
    height = oim_b->size_y / num_bands;
  }
  else {
    width = oim->size_x;
    height = oim->size_y / num_bands;
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

  if (datum != UNKNOWN_DATUM) {
    mp->spheroid = datum_spheroid(mp->datum);
    spheroid_axes_lengths (mp->spheroid, &mg->re_major, &mg->re_minor);
  }
  else {
    mg->re_major = MAGIC_UNSET_DOUBLE;
    mg->re_minor = MAGIC_UNSET_DOUBLE;
  }

  mp->startX = (0.0 - raster_tp_x) * mg->x_pixel_size + tp_lon;
  mp->startY = (0.0 - raster_tp_y) * (-mg->y_pixel_size) + tp_lat;
  mp->perX = mg->x_pixel_size;
  mp->perY = -mg->y_pixel_size;

  // These fields should be the same as the ones in the general block.
  mp->re_major = mg->re_major;
  mp->re_minor = mg->re_minor;

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
      asfPercentMeter((double)ii / (double)(oim->size_y));
      for ( jj = 0 ; jj < oim->size_x ; jj++ ) {
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
  }
#endif

  asfPrintStatus("\nGathering image statistics (per available band)...\n");
  int *ignore;
  float min, max;
  float mean, standard_deviation;
  meta_stats *band_stats[MAX_BANDS]; // min, max, mean, rmse, std_deviation, mask: all doubles
  int ii;
  for (ii=0; ii<MAX_BANDS; ii++) {
    band_stats[ii] = meta_stats_init();
  }
  ignore = CALLOC(MAX_BANDS, sizeof(int)); // Contains '1' if a band is to be ignored (empty band)
  if (data_type == BYTE) {
    uint8 b_min, b_max;
    double b_mean, b_standard_deviation;

    // FIXME: We should not calc statistics on an entire multi-band image at once since
    // each band (radar polarization, color, etc) will be different.  The metadata structure
    // needs to be modified so that multiple stats blocks can exist, similar to satellite
    // state vectors, and then write the band stats into the individual blocks...
    // ...but for now, since we've already been committing this sin, keep on calculating
    // the stats for the whole image (sigh.)
    //uint8_image_statistics (oim_b, &b_min, &b_max, &b_mean, &b_standard_deviation,
                            //0, UINT8_IMAGE_DEFAULT_MASK);
    for (ii=0; ii<num_bands; ii++) {
      int ret;
      ret = uint8_image_band_statistics(oim_b, band_stats[ii],
                                        mg->line_count, ii,
                                        0, UINT8_IMAGE_DEFAULT_MASK);
      if (ret != 0) {
        // Band data is blank, e.g. no variation ...all pixels the same
        ignore[ii] = 1;
      }
    }

    min = band_stats[0]->min; //(float)b_min;
    max = band_stats[0]->max; //(float)b_max;
    mean = band_stats[0]->mean; //(float)b_mean;
    standard_deviation = band_stats[0]->std_deviation; //(float)b_standard_deviation;
  }
  else {
    // FIXME: We should not calc statistics on an entire multi-band image at once since
    // each band (radar polarization, color, etc) will be different.  The metadata structure
    // needs to be modified so that multiple stats blocks can exist, similar to satellite
    // state vectors, and then write the band stats into the individual blocks...
    // ...but for now, since we've already been committing this sin, keep on calculating
    // the stats for the whole image (sigh.)
    //float_image_statistics (oim, &min, &max, &mean, &standard_deviation,
                            //FLOAT_IMAGE_DEFAULT_MASK);
    for (ii=0; ii<num_bands; ii++) {
      int ret;
      ret = float_image_band_statistics(oim, band_stats[ii],
                                        mg->line_count, ii,
                                        FLOAT_IMAGE_DEFAULT_MASK);
      if (ret != 0) {
        // Band data is blank, e.g. no variation ...all pixels the same
        ignore[ii] = 1;
      }
    }
    min = band_stats[0]->min;
    max = band_stats[0]->max;
    mean = band_stats[0]->mean;
    standard_deviation = band_stats[0]->std_deviation; //(float)b_standard_deviation;
  }
  mp->height = mean; // Overrides the default value of 0.0 set earlier
  ms->min = min;
  ms->max = max;

  // Fill out the number of bands and the band names
  strcpy(mg->bands, "");
  mg->band_count = num_bands;
  int *empty = (int*)CALLOC(num_bands, sizeof(int)); // Defaults to 'no empty bands'
  char *band_str;
  band_str = (char*)MALLOC(25*sizeof(char)); // '25' is the array length of mg->bands (see asf_meta.h) ...yes, I know.
  int num_found_bands;
  int found_bands = get_bands_from_citation(&num_found_bands, &band_str, empty, citation);
  if (num_found_bands < 1) {
    asfPrintWarning("No ASF-exported band names found in GeoTIFF citation tag.\n"
       "Band names will be assigned in numerical order.\n");
  }
  if ( found_bands > 0 && num_bands == num_found_bands) {
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
    asfPrintError("GeoTIFF file must contain at least one color channel (band)\n");
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

  ms->mean = mean;
  // The root mean square error and standard deviation are very close
  // by definition when the number of samples is large, there seems to
  // be some confusion about the definitions of one relative to the
  // other, and I don't think its worth agonizing about the best thing
  // to do.
  ms->rmse = standard_deviation;
  ms->std_deviation = standard_deviation;

  if (data_type == BYTE) {
    ms->mask = (float)UINT8_IMAGE_DEFAULT_MASK;
  }
  else {
    ms->mask = FLOAT_IMAGE_DEFAULT_MASK;
  }

  // Set up the location block
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

  // Write the Metadata file
  meta_write(meta_out, outBaseName);

  // Write the binary file
  if (data_type == BYTE) {
    int ret;
    ret = band_byte_image_write(oim_b, meta_out, outBaseName, num_bands, ignore);
    if (ret != 0) {
      asfPrintError("Unable to write binary byte image...\n");
    }
  }
  else {
    int ret;
    ret = band_float_image_write(oim, meta_out, outBaseName, num_bands, ignore);
    if (ret != 0) {
      asfPrintError("Unable to write binary float image...\n");
    }
  }
  //int return_code = write_meta_and_img (outBaseName, meta_out, oim);
  //asfRequire (return_code == 0,
  //	      "Failed to write new '.meta' and '.img' files.\n");

  // We're now done with the data and metadata.
  GTIFFree(input_gtif);
  XTIFFClose(input_tiff);
  meta_free (meta_out);
  for (ii=0; ii<MAX_BANDS; ii++) {
    FREE(band_stats[ii]);
  }
  FREE(ignore);
  if (data_type == BYTE) {
    uint8_image_free(oim_b);
  }
  else {
    float_image_free(oim);
  }

  // We must be done with the citation string too :)
  FREE (citation);
}

// Checking routine for projection parameter input.
void check_projection_parameters(meta_projection *mp)
{
  project_parameters_t *pp = &mp->param;

// FIXME: Hughes datum stuff commented out for now ...until Hughes is implemented in the trunk
//  asfRequire(!(datum == HUGHES_DATUM && projection_type != POLAR_STEREOGRAPHIC),
//               "Hughes ellipsoid is only supported for polar stereographic projections.\n");

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

  s = strstr(citation, BAND_ID_STRING);
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









































