#include "asf.h"
#include "asf_meta.h"
#include "asf_export.h"
#include "spheroids.h"
#include "typlim.h"
#include <ctype.h>


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
  double s_semi_major = 0.0;
  double s_semi_minor = 0.0;

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
