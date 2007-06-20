#include "asf_geocode.h"
#include "libasf_proj.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "proj_api.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static const double DEFAULT_POLAR_STERO_NORTH_CENTRAL_MERIDIAN = -45;
static const double DEFAULT_POLAR_STERO_SOUTH_CENTRAL_MERIDIAN = -90;

static const double DEFAULT_POLAR_STERO_NORTH_STANDARD_PARALLEL = 70;
static const double DEFAULT_POLAR_STERO_SOUTH_STANDARD_PARALLEL = -70;

double zone_to_central_meridian (int zone);

project_parameters_t * get_geocode_options(int *argc, char **argv[],
                                           projection_type_t * proj_type,
                                           double *height, double *pixel_size,
                                           datum_type_t *datum,
                                           resample_method_t *resample_method,
                                           int *override_checks,
                                           char *band_id)
{
  /* projection parameters obtained from the command line */
  project_parameters_t * pps;

  /* TRUE if we did a write-proj-file */
  int did_write_proj_file = FALSE;

  /* must pull out logfile first, so we can log projection parsing errors */
  parse_log_options(argc, argv);

  /* get the projection params out of the cmd line & remove from cmd line */
  pps = parse_projection_options(argc, argv, proj_type,
                                 &did_write_proj_file);

  if (pps)
  {
    /* "other" options include: 'height', 'pixel-size', 'force'
        and 'resample_method'.  */
    parse_other_options(argc, argv, height, pixel_size, datum,
                        resample_method, override_checks, band_id);

    /* here the semantics of the projection parameters are applied */
    sanity_check(*proj_type, pps);
  }
  else {
    asfPrintWarning("Required projection parameters are missing.\n");
  }

  /* Exit now if no input/output files are specified, and the
     "write-proj-file" option was specified */
  if (did_write_proj_file && *argc == 1) {
    asfPrintWarning("No input files.\n");
  }

  return pps;
}

static double
local_earth_radius (spheroid_type_t spheroid, double geodetic_lat)
{
  double a, b;
  spheroid_axes_lengths (spheroid, &a, &b);

  double e2 = 1 - (pow (b, 2.0) / pow (a, 2.0));

  // Geocentric latitude.
  double gcl = atan (tan (geodetic_lat) * (1 - e2));

  return (a * b) / sqrt (pow (b * cos (gcl), 2.0) + pow (a * sin (gcl), 2.0));
}

// Return the arc length of an arc covering angle at geodetic_lat on
// spheroid, assuming a constant radius of curvature approximation
// over the arc.
static double
arc_length_at_latitude (spheroid_type_t spheroid, double geodetic_lat,
                        double angle)
{
  assert (angle < 2 * M_PI);

  double er = local_earth_radius (spheroid, geodetic_lat);

  return angle * er;
}

int calc_utm_zone(double lon)
{
  return((int)(((lon + 180.0) / 6.0) + 1.0));
}

static void verify_valid_latitude(double lat)
{
  if (ISNAN(lat))
    return;

  if (lat > 90 || lat < -90)
  {
    asfPrintWarning("Invalid Latitude: %f\n", lat);
    lat = NAN;
  }
}

static void verify_valid_longitude(double lon)
{
  if (ISNAN(lon))
    return;

  if (lon > 360 || lon < -360)
  {
    asfPrintWarning("Invalid Longitude: %f\n", lon);
    lon = NAN;
  }
}

void sanity_check(projection_type_t pt, project_parameters_t * pps)
{
  switch (pt) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      if (pps->utm.zone != MAGIC_UNSET_INT) {
        if ((abs(pps->utm.zone) < 1) || (abs(pps->utm.zone) > 60)) {
          asfPrintError("Illegal zone number: %d\n", pps->utm.zone);
        }
      }
      verify_valid_latitude(pps->utm.lat0);
      verify_valid_longitude(pps->utm.lon0);
      break;
    case POLAR_STEREOGRAPHIC:
      verify_valid_latitude(pps->ps.slat);
      verify_valid_longitude(pps->ps.slon);
      break;
    case ALBERS_EQUAL_AREA:
      verify_valid_latitude(pps->albers.std_parallel1);
      verify_valid_latitude(pps->albers.std_parallel2);
      verify_valid_latitude(pps->albers.orig_latitude);
      verify_valid_longitude(pps->albers.center_meridian);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      verify_valid_latitude(pps->lamaz.center_lat);
      verify_valid_longitude(pps->lamaz.center_lon);
      break;
    case LAMBERT_CONFORMAL_CONIC:
      verify_valid_latitude(pps->lamcc.plat1);
      verify_valid_latitude(pps->lamcc.plat2);
      verify_valid_latitude(pps->lamcc.lat0);
      verify_valid_longitude(pps->lamcc.lon0);
      break;
    default:
      asfPrintError("sanity_check: illegal projection type!");
  }
}

void apply_defaults(projection_type_t pt, project_parameters_t * pps,
                                        meta_parameters * meta, double * average_height,
                                        double * pixel_size)
{
  if ( ISNAN (*average_height) )
    *average_height = 0.0;

  if ( ISNAN (*pixel_size) ) {
    // If the input image is pseudoprojected, we use the arc
    // length between pixels at the image center as the
    // approximate pixel size.  There are comments relating to
    // pixel size in the import_usgs_seamless.c file that should
    // change if this approach is changed.
    if ( meta->projection != NULL &&
         meta->projection->type == LAT_LONG_PSEUDO_PROJECTION ) {
      *pixel_size = arc_length_at_latitude (meta->projection->spheroid,
                                            meta->general->center_latitude,
                                            meta->general->y_pixel_size * D2R);
    }
    else {
      *pixel_size = meta->general->x_pixel_size;
    }
  }

  switch (pt) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      // set the center_longitude and center_latitude if necessary
      if (ISNAN(meta->general->center_longitude) ||
          ISNAN(meta->general->center_latitude)
         ) {
        double lat, lon;
        meta_get_latLon(meta, meta->general->line_count/2,
                        meta->general->sample_count/2, 0,
                        &lat, &lon);
        meta->general->center_longitude = lon;
        meta->general->center_latitude = lat;
      }

      /* set the zone based on the specified longitude */
      if (pps->utm.zone == MAGIC_UNSET_INT) {
        // The only sane thing to do is refuse to operate if we
        // get a longitude that puts us on a zone boundry --
        // such an argument is essentially ambiguous, and the
        // user must resolve it.
        if ( meta->general->center_longitude / 6.0 -
             floor (meta->general->center_longitude / 6.0) == 0.0 )
        {
          asfPrintError ("Center of Scene Longitude (%.6f) lies on a UTM\n"
              "zone boundry, (i.e. is ambiguous as to which UTM zone\n"
              "should be used for geocoding.)  Please select which zone\n"
              "you prefer (via user interface or .proj definition file.)\n",
          meta->general->center_longitude);
        }
        pps->utm.zone = calc_utm_zone(meta->general->center_longitude);
      }

      // Set the latitude/longitude of origin
      if (ISNAN(pps->utm.lon0)) {
        pps->utm.lon0 = zone_to_central_meridian(pps->utm.zone);
      }
      if (ISNAN(pps->utm.lat0)) {
        pps->utm.lat0 = 0;
      }

      /* false easting & false northing are fixed for utm */
      pps->utm.false_northing = meta->general->center_latitude >= 0 ? 0 : 10000000;
      pps->utm.false_easting = 500000;
      pps->utm.scale_factor = 0.9996;
      break;

    case POLAR_STEREOGRAPHIC:
      /* SMMI standard values */
      if (ISNAN(pps->ps.slon))
        pps->ps.slon = pps->ps.is_north_pole ?
                         DEFAULT_POLAR_STERO_NORTH_CENTRAL_MERIDIAN :
                         DEFAULT_POLAR_STERO_SOUTH_CENTRAL_MERIDIAN;

      /* default standard parallels are +/- 70 */
      if (ISNAN(pps->ps.slat))
        pps->ps.slat = pps->ps.is_north_pole ?
                         DEFAULT_POLAR_STERO_NORTH_STANDARD_PARALLEL :
                         DEFAULT_POLAR_STERO_SOUTH_STANDARD_PARALLEL;

      if (ISNAN(pps->ps.false_easting))
        pps->ps.false_easting = 0;
      if (ISNAN(pps->ps.false_northing))
        pps->ps.false_northing = 0;
      break;

    case ALBERS_EQUAL_AREA:
      if (ISNAN(pps->albers.false_easting))
        pps->albers.false_easting = 0;
      if (ISNAN(pps->albers.false_northing))
        pps->albers.false_northing = 0;

      if (ISNAN(pps->albers.orig_latitude))
        pps->albers.orig_latitude = meta->general->center_latitude;
      if (ISNAN(pps->albers.center_meridian))
        pps->albers.center_meridian = meta->general->center_longitude;
      break;

    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      if (ISNAN(pps->lamaz.false_easting))
        pps->lamaz.false_easting = 0;
      if (ISNAN(pps->lamaz.false_northing))
        pps->lamaz.false_northing = 0;

      if (ISNAN(pps->lamaz.center_lat))
        pps->lamaz.center_lat = meta->general->center_latitude;
      if (ISNAN(pps->lamaz.center_lon))
        pps->lamaz.center_lon = meta->general->center_longitude;
      break;

    case LAMBERT_CONFORMAL_CONIC:
      if (ISNAN(pps->lamcc.false_easting))
        pps->lamcc.false_easting = 0;
      if (ISNAN(pps->lamcc.false_northing))
        pps->lamcc.false_northing = 0;

      if (ISNAN(pps->lamcc.lat0))
        pps->lamcc.lat0 = meta->general->center_latitude;
      if (ISNAN(pps->lamcc.lon0))
        pps->lamcc.lon0 = meta->general->center_longitude;
      break;

    default:
      asfPrintError("apply_defaults: illegal projection type!");
  }
}

double zone_to_central_meridian (int zone)
{
  return -177.0 + (zone - 1) * 6.0;
}

const char *geocode_projection_options_help()
{
    return
"     Projection Parameter Options  \n"
"     ============================\n"
"\n"
"     All these options take arguments, unless otherwise noted.  Groups\n"
"     of options which appear together on a single line seperated by\n"
"     commas are aliases for the same parameter (they are intended to\n"
"     aid recognition, since there is much confusion of map projection\n"
"     terminology).\n"
"\n"
"     -p, -projection: Projection\n"
"          Projection to use.  Argument must be one of the following:\n"
"               utm    - Universal Transverse Mercator\n"
"               ps     - Polar stereographic\n"
"               lamcc  - Lambert conformal conic\n"
"               lamaz  - Lambert azimuthal equal area\n"
"               albers - Albers conical equal area\n"
"\n"
"       UTM\n"
"       ---\n"
"          --zone                      : Zone (optional)\n"
"\n"
"          If a zone is not specified, it will be determined from the\n"
"          scene's metadata.\n\n"
"\n"
"       POLAR STEREO\n"
"       ------------\n"
"          --first-standard-parallel    : Latitude of True Scale\n"
"          --central-meridian           : Longitude of Central Meridian\n"
"          --north-pole                 : Center on North Pole (no argument)\n"
"          --south-pole                 : Center on South Pole (no argument)\n"
"          --false-easting              : False Easting (optional)\n"
"          --false-northing             : False Northing (optional)\n"
"\n"
"       LAMBERT CONFORMAL CONIC\n"
"       -----------------------\n"
"          --first-standard-parallel   : First Standard Parallel\n"
"          --second-standard-parallel  : Second Standard Parallel\n"
"          --latitude-of-origin        : Latitude at projection\"s origin\n"
"          --central-meridian          : Central Meridian\n"
"          --false-easting             : False Easting (optional)\n"
"          --false-northing            : False Northing (optional)\n"
"\n"
"          You may omit the origin (the image center will be used as the\n"
"          origin), however the standard parallels are required.\n"
"\n"
"       LAMBERT AZIMUTHAL EQUAL AREA\n"
"       ----------------------------\n"
"          --latitude-of-origin        : Latitude at center of projection\n"
"          --central-meridian          : Longitude at center of projection\n"
"          --false-easting             : False Easting (optional)\n"
"          --false-northing            : False Northing (optional)\n"
"\n"
"          You may omit the point of tangency (the image center will be\n"
"          used).\n"
"\n"
"       ALBERS CONICAL EQUAL AREA\n"
"       -------------------------\n"
"          --first-standard-parallel   : First Standard Parallel\n"
"          --second-standard-parallel  : Second Standard Parallel\n"
"          --latitude-of-origin        : Latitude of projection\"s origin\n"
"          --central-meridian          : Central Meridian\n"
"          --false-easting             : False Easting (optional)\n"
"          --false-northing            : False Northing (optional)\n"
"\n"
"          You may omit the origin (the image center will be used as the\n"
"          origin), however the standard parallels are required.\n"
"\n"
"        Projection Option Abbreviations\n"
"        -------------------------------\n"
"          The following are accepted as abbreviations for the above options:\n"
"\n"
"          --first-standard-parallel   : -plat1\n"
"          --second-standard-parallel  : -plat2\n"
"          --central-meridian          : -lon0\n"
"          --latitude-of-origin        : -lat0\n"
"          --north-pole                : -n\n"
"          --south-pole                : -s\n"
"          --zone                      : -z\n"
"\n"
"          For example, instead of:\n"
"            --projection albers --first-standard-parallel 55 \n"
"              --second-standard-parallel 65 --central-meridian -154\n"
"              --latitude-of-origin 50\n"
"\n"
"          you may use:\n"
"            -p albers -plat1 55 -plat2 65 -lon0 -154 -lat0 50\n"
"\n"
"     Using a Projection Parameters File\n"
"     ==================================\n"
"\n"
"     -write-proj-file <file>\n"
"          Save the specified projection information to a file with\n"
"          the given name.  The file may be used for subsequent projections\n"
"          with --read-proj-file.\n"
"\n"
"     -read-proj-file <file>\n"
"          Read projection information from the given file.  The format of\n"
"          the file must match what is used with --write-proj-file.\n"
"          This option may not be used together with any other\n"
"          projection options.\n"
"\n"
"          You cannot specify a projection (with --projection), and\n"
"          --read-proj-file at the same time.\n"
"\n"
"     Other Options\n"
"     =============\n"
"\n"
"     -height <height> (-h)\n"
"          Assume that terrain in the image is <height> meters above\n"
"          the reference GEM6 ellipsoid.  Optimal geolocation accuracy\n"
"          will then be achieved for pixels on terrain at this height.\n"
"          The geolocation of terrain at other height will be off by\n"
"          about the height difference between the terrain and\n"
"          <height>, assuming a satellite look angle 45 degrees from\n"
"          horizontal.\n"
"\n"
"     -resample-method <method>\n"
"          Specifies which interpolation method to use when resampling\n"
"          images into projection geometry.  Available choices are:\n"
"            nearest_neighbor\n"
"            bilinear\n"
"            bicubic\n"
"\n"
"     -datum <datum> \n"
"          Specifies the datum that is used when projecting.  The datum\n"
"          applies to the target coordinate system.  Supported Datums:\n"
"            NAD27  (North American Datum 1927) (Clarke 1866)\n"
"            NAD83  (North American Datum 1983) (GRS 1980)\n"
"            WGS84  (World Geodetic System 1984) (default).\n"
"          NOTE: When specifying a datum together with a UTM projection\n"
"          type, note the following valid zone numbers which may be combined\n"
"          with each datum type:\n"
"            NAD27  Valid zones are 2 through 22\n"
"            NAD83  Valid zones are 2 through 23\n"
"            WGS84  Valid zones are 1 through 60\n"
"\n"
"     -pixel_size <pixel spacing> (-ps, -pix)\n"
"          Specifies the pixel spacing of the geocoded image.  By default,\n"
"          the pixel size of the first input image is used.\n"
"\n"
"     -band <band_id | all>\n"
"          If the image file contains multiple bands (channels), then\n"
"          geocode the band identified by 'band_id' (only) into a single-band\n"
"          ASF-format file.  If 'all' is specified rather than a band_id, then\n"
"          geocode all available bands into a single multi-band ASF-format file.\n"
"          In this case, all bands will be geocoded using the same set of\n"
"          projection parameters, and the parameters in the resulting metadata\n"
"          file apply to all.  Default is '-band all'.\n"
"\n"
"     -background <background fill value>\n"
"          Value to use for pixels that fall outside of the scene.  By default,\n"
"          the outside is filled with zeroes.\n"\
"\n"
"     -force (-f)\n"
"          Override the built-in projection sanity checks.  Normally,\n"
"          the program will abort with an error if it detects that a\n"
"          scene lies in an area where the selected projection is\n"
"          going to give poor results.  However, you may still wish\n"
"          to do the projection anyway (such as when you will mosaic\n"
"          the result a number of other scenes and wish to have them\n"
"          all in the same projection), the -force option can be used\n"
"          in these situations.\n"
    ;
}
