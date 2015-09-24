#include "libasf_proj.h"
#include "asf.h"
#include "asf_nan.h"

#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include "proj_api.h"
#include "spheroids.h"

#define DEFAULT_AVERAGE_HEIGHT 0.0;

// Do not change the BAND_ID_STRING.  It will break ingest of legacy TIFFs 
// exported with this string in their citation strings.  If you are changing 
// the citation string to have some _other_ identifying string, then use a 
// _new_ definition rather than replace what is in this one.
// Then, make sure that your code supports both the legacy string and the new 
// string. Also see the export library string definitions (which MUST match 
// these here) and for associated code that needs to reflect the changes you 
// are making here.
#define BAND_ID_STRING "Color Channel (Band) Contents in RGBA+ order";

/* WGS84 is hard-coded on the lat/lon side for now! */
static const char *latlon_description = "+proj=latlong +datum=WGS84";

#ifndef linux
#ifndef darwin
#ifndef win32
static double
round (double arg)
{
    return floor (arg + 0.5);
}
#endif /* #ifndef win32 */
#endif /* #ifndef darwin */
#endif /* #ifndef linux */

#ifdef linux
/* Some missing prototypes */
int putenv(char *);
int setenv(const char *, const char *, int);
#endif

static void set_proj_lib_path(datum_type_t datum)
{
    static int set_path_already = FALSE;
    if (datum == NAD27_DATUM) {
        if (!set_path_already) {
            // point to the grid shift files
            char proj_putenv[255];
            sprintf(proj_putenv, "PROJ_LIB=%s/proj", get_asf_share_dir());
            putenv(proj_putenv);
            //sprintf(proj_putenv, "%s/proj", get_asf_share_dir());
            //setenv("PROJ_LIB", proj_putenv, TRUE);
            set_path_already = TRUE;
        }
    }
}

static const char * datum_str(datum_type_t datum)
{
    set_proj_lib_path(datum);

    switch (datum)
    {
    case NAD27_DATUM:    /* North American Datum 1927 (Clarke 1866) */
        return "NAD27";
    case NAD83_DATUM:    /* North American Datum 1983 (GRS 1980)    */
        return "NAD83";
    case HUGHES_DATUM:
        return "HUGHES";
    case ED50_DATUM:
      return "ED50";
    case ITRF97_DATUM:
      return "ITRF97";
    case SAD69_DATUM:
      return "SAD69";
    default:
      return "UNKNOWN";
    case WGS84_DATUM:    /* World Geodetic System 1984 (WGS84)      */
        return "WGS84";
    }
}

// Returns TRUE if we have grid shift files available for the given point,
// and returns FALSE if not.  If this returns FALSE for any point in a
// scene, the NAD27 datum shouldn't be used.
int test_nad27(double lat, double lon)
{
    projPJ ll_proj, utm_proj;
    ll_proj = pj_init_plus(latlon_description);

    char desc[255];
    int zone = utm_zone(lon);
    sprintf(desc, "+proj=utm +zone=%d +datum=NAD27", zone);
    utm_proj = pj_init_plus(desc);
/*
    double *px, *py, *pz;
    px = MALLOC(sizeof(double));
    py = MALLOC(sizeof(double));
    pz = MALLOC(sizeof(double));
*/
    double px[1], py[1], pz[1];
    py[0] = lat*D2R;
    px[0] = lon*D2R;
    pz[0] = 0;

    pj_transform (ll_proj, utm_proj, 1, 1, px, py, pz);

    int ret = TRUE;
    if (pj_errno == -38) // -38 indicates error with the grid shift files
    {
        ret = FALSE;
    }
    else if (pj_errno != 0) // some other error (pj errors are negative,
    {                       // system errors are positive)
        asfPrintError("libproj Error: %s (test_nad27)\n", 
		      pj_strerrno(pj_errno));
    }

    pj_free(ll_proj);
    pj_free(utm_proj);

    return ret;
}

static double sHeight = DEFAULT_AVERAGE_HEIGHT;
void project_set_avg_height(double h)
{
    sHeight = h;
}

static int height_was_set()
{
    return !ISNAN(sHeight);
}

static double get_avg_height()
{
    if (height_was_set())
  return sHeight;
    else
  return DEFAULT_AVERAGE_HEIGHT;
}

static int project_worker_arr(const char * projection_description,
                              double *lat, double *lon, double *height,
                              double **projected_x, double **projected_y,
                              double **projected_z, long length)
{
  projPJ geographic_projection, output_projection;
  int i, ok = TRUE;

  // This section is a bit confusing.  The interfaces to the single
  // point functions allow the user to a pass ASF_PROJ_NO_HEIGHT value
  // if they don't care about height.  The array functions allow the
  // user to pass NULL for height.  Both interfaces end up calling
  // this routine, which has to detect what has happened.  The single
  // point functions require NULL to be passed to as the third return
  // value pointer, to ensure that the user isn't confused about what
  // will be returned.  The array functions require that NULL be
  // passed as the return array pointer.  We need to allocate some
  // space for z values for temporary use in these cases, which is
  // also a bit tricky.

  // Flag true iff we "don't care about heights" (or a willing to use
  // the set average height), which happens if the user passes
  // ASF_PROJ_NO_HEIGHT for *height or NULL for height.
  int dcah = 0;

  if ( height == NULL ) {
    assert (projected_z == NULL);
    dcah = 1;
  }
  else if ( *height == ASF_PROJ_NO_HEIGHT ) {
    assert (*projected_z == NULL);
    dcah = 1;
  }

  // Flag true iff argument projected_z was not NULL on function entry.
  int projected_z_not_null = 0;
  if ( projected_z != NULL ) {
    projected_z_not_null = 1;
  }

  if ( dcah ) {
    // User should either pre-allocate both, or want us to allocate
    // both -- to preallocate one but not the other is weird and
    // probably a mistake.
    if ( !(*projected_x) || !(*projected_y) ) {
      assert (!(*projected_x) && !(*projected_y));

      *projected_x = (double *) MALLOC (sizeof(double) * length);
      *projected_y = (double *) MALLOC (sizeof(double) * length);
    }

    if ( !projected_z_not_null ) {
      projected_z = (double **) MALLOC (sizeof (double *));
    }
    *projected_z = (double *) MALLOC (sizeof(double) * length);
  }

  else {
    if (!(*projected_x) || !(*projected_y) || !(*projected_z)) {
      /* user should either pre-allocate all, or want us to allocate
   all -- to preallocate some but not the all is weird and
   probably a mistake */
      assert(!(*projected_x) && !(*projected_y) && !(*projected_z));

      *projected_x = (double *) MALLOC (sizeof(double) * length);
      *projected_y = (double *) MALLOC (sizeof(double) * length);
      *projected_z = (double *) MALLOC (sizeof(double) * length);
    }
  }

  double *px = *projected_x;
  double *py = *projected_y;
  double *pz = *projected_z;

  assert(px);
  assert(py);
  assert(pz);

  for (i = 0; i < length; ++i)
  {
    px[i] = lon[i];
    py[i] = lat[i];

    // If user passed height values, use those, otherwise, use the
    // average height or the default average height.
    if ( !dcah ) {
      pz[i] = height[i];
    }
    else {
      if ( height_was_set () ) {
  pz[i] = get_avg_height ();
      }
      else {
  pz[i] = 0.0;
      }
    }
  }

  //printf("proj: +from %s +to %s\n",
  //       latlon_description, projection_description);

  geographic_projection = pj_init_plus (latlon_description);

  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s (initializing geographic projection)\n",
		    pj_strerrno(pj_errno));
      ok = FALSE;
  }

  if (ok)
  {
      assert (geographic_projection != NULL);

      output_projection = pj_init_plus (projection_description);

      if (pj_errno != 0)
      {
	printf("proj: %s\n", projection_description);
    asfPrintError("libproj Error: %s (initializing output projection)\n", 
		  pj_strerrno(pj_errno));
    ok = FALSE;
      }

      if (ok)
      {
    assert (output_projection != NULL);

    pj_transform (geographic_projection, output_projection, length, 1,
      px, py, pz);

    if (pj_errno != 0)
    {
        asfPrintWarning("libproj error: %s (projection transformation)\n", 
			pj_strerrno(pj_errno));
        ok = FALSE;
    }

    pj_free(output_projection);
      }

      pj_free(geographic_projection);
  }

  // Free memory temporarily allocated for height values that we don't
  // really care about.
  if ( dcah ) {
    FREE (*projected_z);
    if ( !projected_z_not_null ) {
      FREE (projected_z);
    }
  }

  return ok;
}

static int
project_worker_arr_inv(const char * projection_description,
                       double *x, double *y, double *z,
                       double **lat, double **lon, double **height,
                       long length)
{
  projPJ geographic_projection, output_projection;
  int i, ok = TRUE;

  // Same issue here as above.  Because both single and array
  // functions ultimately call this routine, and we allow the user to
  // specify that they don't care about heights by passing certain
  // arguments, things get complicated.  See above.

  // Flag true iff we "don't care about heights" (or a willing to use
  // the set average height), which happens if the user passes
  // ASF_PROJ_NO_HEIGHT for *z or NULL for z.
  int dcah = 0;

  if ( z == NULL ) {
    assert (height == NULL);
    dcah = 1;
  }
  else if ( *z == ASF_PROJ_NO_HEIGHT ) {
    assert (*height == NULL);
    dcah = 1;
  }

  // Flag true iff argument height was not NULL on function entry.
  int height_not_null = 0;
  if ( height != NULL ) {
    height_not_null = 1;
  }

  if ( dcah ) {
    // User should either pre-allocate both, or want us to allocate
    // both -- to preallocate one but not the other is weird and
    // probably a mistake.
    if ( !(*lat) || !(*lon) ) {
      assert (!(*lat) && !(*lon));

      *lat = (double *) MALLOC (sizeof(double) * length);
      *lon = (double *) MALLOC (sizeof(double) * length);
    }

    if ( !height_not_null ) {
      height = (double **) MALLOC (sizeof (double *));
    }
    *height = (double *) MALLOC (sizeof(double) * length);
  }

  else {
    if (!(*lat) || !(*lon) || !(*height)) {
      /* user should either pre-allocate all, or want us to allocate
   all -- to preallocate some but not the all is weird and
   probably a mistake */
      assert(!(*lat) && !(*lon) && !(*height));

      *lat = (double *) MALLOC (sizeof(double) * length);
      *lon = (double *) MALLOC (sizeof(double) * length);
      *height = (double *) MALLOC (sizeof(double) * length);
    }
  }

  double *plat = *lat;
  double *plon = *lon;
  double *pheight = *height;

  assert(plat);
  assert(plon);
  assert(pheight);

  for (i = 0; i < length; ++i)
  {
    plat[i] = y[i];
    plon[i] = x[i];

    // If user passed z values, use those, otherwise, use the
    // average height or the default average height.
    if ( !dcah ) {
      pheight[i] = z[i];
    }
    else {
      if ( height_was_set () ) {
  pheight[i] = get_avg_height ();
      }
      else {
  pheight[i] = 0.0;
      }
    }
  }

  //printf("proj: +from %s +to %s\n",
  //       projection_description, latlon_description);

  geographic_projection = pj_init_plus ( latlon_description );

  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s (initializing inverse geographic "
		    "projection)\n", pj_strerrno(pj_errno));
      ok = FALSE;
  }

  if (ok)
  {
      assert (geographic_projection != NULL);

      output_projection = pj_init_plus (projection_description);

      if (pj_errno != 0)
      {
    asfPrintError("libproj Error: %s\n (initializing inverse output "
		  "projection)\n", pj_strerrno(pj_errno));
    ok = FALSE;
      }

      if (ok)
      {
    assert (output_projection != NULL);

    pj_transform (output_projection, geographic_projection, length, 1,
      plon, plat, pheight);

    if (pj_errno != 0)
    {
        asfPrintWarning("libproj error: %s (inverse projection transformation)"
			"\n", pj_strerrno(pj_errno));
        ok = FALSE;
    }

    pj_free(output_projection);
      }

      pj_free(geographic_projection);
  }

  // Free memory temporarily allocated for height values that we don't
  // really care about.
  if ( dcah ) {
    FREE (*height);
    if ( !height_not_null ) {
      FREE (height);
    }
  }

  return ok;
}

/****************************************************************************
 Universal Transverse Mercator (UTM)
****************************************************************************/
static double utm_nudge(double lon_0)
{
  double tiny_value;

  /* Nudge cases which are marginal in terms of which utm zone they
     fall in towards zero a bit.  The proj documentation tells us we
     should avoid the marginal cases. */
  tiny_value = 0.00001;    /* Random small number of degrees. */
  if ( fabs(round(lon_0 / 6.0) - lon_0 / 6) < tiny_value ) {
    if ( lon_0 > 0 ) {
      lon_0 -= tiny_value;
    }
    else {
      lon_0 += tiny_value;
    }
  }

  return lon_0;
}

char *utm_projection_description(project_parameters_t *pps, datum_type_t datum)
{
  static char utm_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM || datum == NAD27_DATUM || datum == NAD83_DATUM) {
    if (pps->utm.zone == MAGIC_UNSET_INT)
    {
      sprintf(utm_projection_description,
        "+proj=utm +lon_0=%f +datum=%s +over",
        utm_nudge(pps->utm.lon0), datum_str(datum));
    } else {
      sprintf(utm_projection_description,
        "+proj=utm +zone=%d %s+datum=%s +over",
        pps->utm.zone, pps->utm.false_northing == 10000000 ? "+south " : "",
          datum_str(datum));
    }
  }
  else if (datum == ITRF97_DATUM) {
    if (pps->utm.zone == MAGIC_UNSET_INT) {
      sprintf(utm_projection_description,
              "+proj=utm +lon_0=%f +a=%f +rf=%f +over",
              utm_nudge(pps->utm.lon0 * RAD_TO_DEG),
              (float)INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SEMIMAJOR,
              (float)INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_INV_FLATTENING);
    } else {
      sprintf(utm_projection_description,
              "+proj=utm +zone=%d %s+a=%f +rf=%f +over",
              pps->utm.zone, pps->utm.false_northing == 10000000 ? "+south " : "",
              (float)INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SEMIMAJOR,
              (float)INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_INV_FLATTENING);
    }
  }
  else if (datum == HUGHES_DATUM) {
    if (pps->utm.zone == MAGIC_UNSET_INT) {
      sprintf(utm_projection_description,
              "+proj=utm +lon_0=%f +a=%f +rf=%f +over",
              utm_nudge(pps->utm.lon0 * RAD_TO_DEG),
              (float)HUGHES_SEMIMAJOR,
              (float)HUGHES_INV_FLATTENING);
    } else {
      sprintf(utm_projection_description,
              "+proj=utm +zone=%d %s+a=%f +rf=%f +over",
              pps->utm.zone, pps->utm.false_northing == 10000000 ? "+south " : "",
              (float)HUGHES_SEMIMAJOR,
              (float)HUGHES_INV_FLATTENING);
    }
  }

  return utm_projection_description;
}

int
project_utm (project_parameters_t * pps, double lat, double lon, double height,
       double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(utm_projection_description(pps, datum),
                              &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_utm_arr (project_parameters_t * pps,
     double *lat, double *lon, double *height,
     double **projected_x, double **projected_y,
     double **projected_z, long length,
         datum_type_t datum)
{
  return project_worker_arr(
      utm_projection_description(pps, datum),
      lat, lon, height, projected_x, projected_y, projected_z, length);
}

int
project_utm_inv (project_parameters_t * pps,
     double x, double y, double z,  double *lat, double *lon,
     double *height, datum_type_t datum)
{
  return project_worker_arr_inv(
      utm_projection_description(pps, datum),
      &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_utm_arr_inv (project_parameters_t * pps,
         double *x, double *y, double *z,
         double **lat, double **lon, double **height,
         long length, datum_type_t datum)
{
  return project_worker_arr_inv(
      utm_projection_description(pps, datum),
      x, y, z, lat, lon, height, length);
}

/****************************************************************************
 Polar Sterographic (PS)
****************************************************************************/
char *ps_projection_desc(project_parameters_t *pps, datum_type_t datum)
{
  static char ps_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM || datum == NAD27_DATUM || datum == NAD83_DATUM ||
    datum == ITRF97_DATUM) {
    sprintf(ps_projection_description,
      "+proj=stere +lat_0=%s +lat_ts=%f +lon_0=%f "
        "+k_0=%f +datum=%s +over",
      pps->ps.is_north_pole ? "90" : "-90",
      pps->ps.slat,
      pps->ps.slon,
      1.0 /* pps->ps.scale_factor */,
      datum_str(datum));
  }
  else if (datum == HUGHES_DATUM) { 
    sprintf(ps_projection_description,
            "+proj=stere +lat_0=%s +lat_ts=%f +lon_0=%f "
                "+k_0=%f +a=%f +rf=%f +over",
            pps->ps.is_north_pole ? "90" : "-90",
            pps->ps.slat,
            pps->ps.slon,
            1.0 /* pps->ps.scale_factor */,
            (float)HUGHES_SEMIMAJOR,
            (float)HUGHES_INV_FLATTENING);
  }

  return ps_projection_description;
}

int
project_ps(project_parameters_t * pps, double lat, double lon, double height,
           double *x, double *y, double *z, datum_type_t datum)
{
  return project_worker_arr(ps_projection_desc(pps, datum), &lat, &lon,
                &height, &x, &y, &z, 1);
}

int
project_ps_arr(project_parameters_t * pps,
         double *lat, double *lon, double *height,
         double **projected_x, double **projected_y,
         double **projected_z, long length, datum_type_t datum)
{
  return project_worker_arr(ps_projection_desc(pps, datum), lat, lon, height,
          projected_x, projected_y, projected_z, length);
}

int
project_ps_inv(project_parameters_t * pps, double x, double y, double z,
         double *lat, double *lon, double *height, datum_type_t datum)
{
    return project_worker_arr_inv(ps_projection_desc(pps, datum),
                  &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_ps_arr_inv(project_parameters_t * pps,
       double *x, double *y, double *z,
       double **lat, double **lon, double **height,
       long length, datum_type_t datum)
{
    return project_worker_arr_inv(ps_projection_desc(pps, datum),
          x, y, z, lat, lon, height, length);
}

/****************************************************************************
 Lambert Azimuthal Equal Area
****************************************************************************/
char *lamaz_projection_desc(project_parameters_t *pps, datum_type_t datum)
{
  static char lamaz_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM || datum == NAD27_DATUM || datum == NAD83_DATUM) {
    sprintf(lamaz_projection_description,
	    "+proj=laea +lat_0=%f +lon_0=%f +datum=%s +over",
	    pps->lamaz.center_lat,
	    pps->lamaz.center_lon,
	    datum_str(datum));
  }
  else if (datum == HUGHES_DATUM) {
    sprintf(lamaz_projection_description,
	    "+proj=laea +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
	    pps->lamaz.center_lat,
	    pps->lamaz.center_lon,
            (float)HUGHES_SEMIMAJOR,
            (float)HUGHES_INV_FLATTENING);
  }
  else if (datum == ED50_DATUM) {
    sprintf(lamaz_projection_description,
	    "+proj=laea +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
	    pps->lamaz.center_lat,
	    pps->lamaz.center_lon,
            (float)INTERNATIONAL1924_SEMIMAJOR,
            (float)INTERNATIONAL1924_INV_FLATTENING);
  }
  else if (datum == SAD69_DATUM) {
    sprintf(lamaz_projection_description,
	    "+proj=laea +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
	    pps->lamaz.center_lat,
	    pps->lamaz.center_lon,
            (float)GRS1980_SEMIMAJOR,
            (float)GRS1980_INV_FLATTENING);
  }

  return lamaz_projection_description;
}

int
project_lamaz(project_parameters_t * pps,
        double lat, double lon, double height,
        double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(lamaz_projection_desc(pps, datum),
                  &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_lamaz_arr(project_parameters_t *pps,
      double *lat, double *lon, double *height,
      double **projected_x, double **projected_y,
      double **projected_z, long length, datum_type_t datum)
{
    return project_worker_arr(lamaz_projection_desc(pps, datum), lat, lon,
                  height, projected_x, projected_y, projected_z, length);
}

int
project_lamaz_inv(project_parameters_t *pps, double x, double y, double z,
      double *lat, double *lon, double *height, datum_type_t datum)
{
  return project_worker_arr_inv(lamaz_projection_desc(pps, datum),
                &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_lamaz_arr_inv(project_parameters_t *pps,
          double *x, double *y, double *z,
          double **lat, double **lon, double **height,
          long length, datum_type_t datum)
{
  return project_worker_arr_inv(lamaz_projection_desc(pps, datum), x, y, z,
        lat, lon, height, length);
}

/****************************************************************************
 Lambert Conformal Conic
****************************************************************************/
char *lamcc_projection_desc(project_parameters_t *pps, datum_type_t datum)
{
  static char lamcc_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM || datum == NAD27_DATUM || datum == NAD83_DATUM) {
    sprintf(lamcc_projection_description,
        "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +datum=%s +over",
        pps->lamcc.plat1,
        pps->lamcc.plat2,
        pps->lamcc.lat0,
        pps->lamcc.lon0,
        datum_str(datum));
  }
  else if (datum == HUGHES_DATUM) {
    sprintf(lamcc_projection_description,
        "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
        pps->lamcc.plat1,
        pps->lamcc.plat2,
        pps->lamcc.lat0,
        pps->lamcc.lon0,
            (float)HUGHES_SEMIMAJOR,
            (float)HUGHES_INV_FLATTENING);
  }
  else if (datum == ED50_DATUM) {
    sprintf(lamcc_projection_description,
        "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
        pps->lamcc.plat1,
        pps->lamcc.plat2,
        pps->lamcc.lat0,
        pps->lamcc.lon0,
            (float)INTERNATIONAL1924_SEMIMAJOR,
            (float)INTERNATIONAL1924_INV_FLATTENING);
  }
  else if (datum == SAD69_DATUM) {
    sprintf(lamcc_projection_description,
        "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
        pps->lamcc.plat1,
        pps->lamcc.plat2,
        pps->lamcc.lat0,
        pps->lamcc.lon0,
            (float)GRS1980_SEMIMAJOR,
            (float)GRS1980_INV_FLATTENING);
  }

  return lamcc_projection_description;
}

int
project_lamcc(project_parameters_t * pps,
        double lat, double lon, double height,
        double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(lamcc_projection_desc(pps, datum),
                  &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_lamcc_arr(project_parameters_t *pps,
      double *lat, double *lon, double *height,
      double **projected_x, double **projected_y,
      double **projected_z, long length, datum_type_t datum)
{
  return project_worker_arr(lamcc_projection_desc(pps, datum), lat, lon,
                height, projected_x, projected_y, projected_z, length);
}

int
project_lamcc_inv(project_parameters_t *pps, double x, double y, double z,
      double *lat, double *lon, double *height, datum_type_t datum)
{
  return project_worker_arr_inv(lamcc_projection_desc(pps, datum),
                &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_lamcc_arr_inv(project_parameters_t *pps,
          double *x, double *y, double *z,
          double **lat, double **lon, double **height,
          long length, datum_type_t datum)
{
  return project_worker_arr_inv(lamcc_projection_desc(pps, datum),
                x, y, z, lat, lon, height, length);
}

// Mercator
char *mer_projection_desc(project_parameters_t *pps, datum_type_t datum)
{
  static char mer_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM) {
    sprintf(mer_projection_description,
	    "+proj=merc +lat_ts=%f +lon_0=%f +x_0=%f +y_0=%f +datum=%s +over",
	    pps->mer.standard_parallel,
	    pps->mer.central_meridian,
	    pps->mer.false_easting,
	    pps->mer.false_northing,
	    datum_str(datum));
  }
  else if (datum == ITRF97_DATUM) {
    sprintf(mer_projection_description,
	    "+proj=merc +lat_ts=%f +lon_0=%f +x_0=%f +y_0=%f +a=%f +rf=%f +over",
	    pps->mer.standard_parallel,
	    pps->mer.central_meridian,
	    pps->mer.false_easting,
	    pps->mer.false_northing,
            (float)GRS1980_SEMIMAJOR,
            (float)GRS1980_INV_FLATTENING);
  }
  else
    asfPrintError("Datum (%s) not supported with this Mercator projection!\n",
		  datum_str(datum));

  return mer_projection_description;
}

int
project_mer(project_parameters_t *pps,
	    double lat, double lon, double height,
	    double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(mer_projection_desc(pps, datum),
            &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_mer_arr(project_parameters_t *pps,
		double *lat, double *lon, double *height,
		double **projected_x, double **projected_y,
		double **projected_z, long length, datum_type_t datum)
{
  long ii; 
  int negative=FALSE, positive=FALSE;
  for (ii=0; ii<length; ii++) {
    if (lon[ii] < 0.0)
      negative = TRUE;
    else if (lon[ii] > 0.0)
      positive = TRUE;
  }
  if (negative && positive)
    asfPrintError("Projection problem: Image crosses the dateline.\n"
		  "Mercator projection does not handle this case well.\n");
  return project_worker_arr(mer_projection_desc(pps, datum),
                lat, lon, height, projected_x, projected_y, projected_z,
                length);
}

int
project_mer_inv(project_parameters_t *pps, double x, double y, double z,
		double *lat, double *lon, double *height, datum_type_t datum)
{
    return project_worker_arr_inv(mer_projection_desc(pps, datum),
                  &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_mer_arr_inv(project_parameters_t *pps,
           double *x, double *y, double *z,
           double **lat, double **lon, double **height,
           long length, datum_type_t datum)
{
    return project_worker_arr_inv(mer_projection_desc(pps, datum),
                  x, y, z, lat, lon, height, length);
}

// Sinusoidal
char *sin_projection_desc(project_parameters_t *pps)
{
  static char sin_projection_description[128];

  /* Establish description of output projection. */
  sprintf(sin_projection_description,
	  "+proj=sinu +lon_0=%f +x_0=%f +y_0=%f +a=%f +b=%f +units=m +over",
	  pps->sin.longitude_center,
	  pps->sin.false_easting,
	  pps->sin.false_northing,
	  pps->sin.sphere,
	  pps->sin.sphere);

  return sin_projection_description;
}

int
project_sin(project_parameters_t *pps,
	    double lat, double lon, double height,
	    double *x, double *y, double *z, datum_type_t datum)
{
  return project_worker_arr(sin_projection_desc(pps),
			    &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_sin_arr(project_parameters_t *pps,
		double *lat, double *lon, double *height,
		double **projected_x, double **projected_y,
		double **projected_z, long length, datum_type_t datum)
{
  long ii; 
  int negative=FALSE, positive=FALSE;
  for (ii=0; ii<length; ii++) {
    if (lon[ii] < 0.0)
      negative = TRUE;
    else if (lon[ii] > 0.0)
      positive = TRUE;
  }
  if (negative && positive)
    asfPrintError("Projection problem: Image crosses the dateline.\n"
		  "Sinusoidal projection does not handle this case well.\n");
  return project_worker_arr(sin_projection_desc(pps),
			    lat, lon, height, projected_x, projected_y, 
			    projected_z, length);
}

int
project_sin_inv(project_parameters_t *pps, double x, double y, double z,
		double *lat, double *lon, double *height, datum_type_t datum)
{
  return project_worker_arr_inv(sin_projection_desc(pps),
				&x, &y, &z, &lat, &lon, &height, 1);
}

int
project_sin_arr_inv(project_parameters_t *pps,
		    double *x, double *y, double *z,
		    double **lat, double **lon, double **height,
		    long length, datum_type_t datum)
{
  return project_worker_arr_inv(sin_projection_desc(pps),
				x, y, z, lat, lon, height, length);
}

// Equirectangular
char *eqr_projection_desc(project_parameters_t *pps, datum_type_t datum)
{
  static char eqr_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM) {
    sprintf(eqr_projection_description,
	    "+proj=eqc +lat_ts=%f +lon_0=%f +x_0=%f +y_0=%f +datum=%s +over",
	    pps->eqr.orig_latitude,
	    pps->eqr.central_meridian,
	    pps->eqr.false_easting,
	    pps->eqr.false_northing,
	    datum_str(datum));
  }
  else if (datum == ITRF97_DATUM) {
    sprintf(eqr_projection_description,
	    "+proj=eqc +lat_ts=%f +lon_0=%f +x_0=%f +y_0=%f +a=%f +rf=%f +over",
	    pps->eqr.orig_latitude,
	    pps->eqr.central_meridian,
	    pps->eqr.false_easting,
	    pps->eqr.false_northing,
            (float)GRS1980_SEMIMAJOR,
            (float)GRS1980_INV_FLATTENING);
  }
  else
    asfPrintError("Datum (%s) not supported for Equirectangular projection!\n",
		  datum_str(datum));

  return eqr_projection_description;
}

int
project_eqr(project_parameters_t *pps,
	    double lat, double lon, double height,
	    double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(eqr_projection_desc(pps, datum),
            &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_eqr_arr(project_parameters_t *pps,
		double *lat, double *lon, double *height,
		double **projected_x, double **projected_y,
		double **projected_z, long length, datum_type_t datum)
{
  return project_worker_arr(eqr_projection_desc(pps, datum),
                lat, lon, height, projected_x, projected_y, projected_z,
                length);
}

int
project_eqr_inv(project_parameters_t *pps, double x, double y, double z,
		double *lat, double *lon, double *height, datum_type_t datum)
{
    return project_worker_arr_inv(eqr_projection_desc(pps, datum),
                  &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_eqr_arr_inv(project_parameters_t *pps,
		    double *x, double *y, double *z,
		    double **lat, double **lon, double **height,
		    long length, datum_type_t datum)
{
    return project_worker_arr_inv(eqr_projection_desc(pps, datum),
                  x, y, z, lat, lon, height, length);
}

// Equidistant
char *eqc_projection_desc(project_parameters_t *pps, datum_type_t datum)
{
  static char eqc_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM) {
    sprintf(eqc_projection_description,
	    "+proj=eqc +lat_ts=%f +lon_0=%f +datum=%s +over",
	    pps->eqc.orig_latitude,
	    pps->eqc.central_meridian,
	    datum_str(datum));
  }
  else
    asfPrintError("Datum (%s) not supported for Equidistant projection!\n",
		  datum_str(datum));

  return eqc_projection_description;
}

int
project_eqc(project_parameters_t *pps,
	    double lat, double lon, double height,
	    double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(eqc_projection_desc(pps, datum),
            &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_eqc_arr(project_parameters_t *pps,
		double *lat, double *lon, double *height,
		double **projected_x, double **projected_y,
		double **projected_z, long length, datum_type_t datum)
{
  long ii; 
  int negative=FALSE, positive=FALSE;
  for (ii=0; ii<length; ii++) {
    if (lon[ii] < 0.0)
      negative = TRUE;
    else if (lon[ii] > 0.0)
      positive = TRUE;
  }
  if (negative && positive)
    asfPrintError("Projection problem: Image crosses the dateline.\n"
		  "Equidistant projection does not handle this case well."
		  "\n");
  return project_worker_arr(eqc_projection_desc(pps, datum),
                lat, lon, height, projected_x, projected_y, projected_z,
                length);
}

int
project_eqc_inv(project_parameters_t *pps, double x, double y, double z,
		double *lat, double *lon, double *height, datum_type_t datum)
{
    return project_worker_arr_inv(eqc_projection_desc(pps, datum),
                  &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_eqc_arr_inv(project_parameters_t *pps,
		    double *x, double *y, double *z,
		    double **lat, double **lon, double **height,
		    long length, datum_type_t datum)
{
    return project_worker_arr_inv(eqc_projection_desc(pps, datum),
                  x, y, z, lat, lon, height, length);
}

// EASE grid - Global
char *ease_global_projection_desc(project_parameters_t *pps)
{
  static char ease_global_projection_description[128];

  /* Establish description of output projection. */
  sprintf(ease_global_projection_description,
	  "+proj=cea +lon_0=%f +lat_ts=%f +x_0=%f +y_0=%f +a=%f +b=%f +units=m +over",
	  pps->cea.central_meridian,
	  pps->cea.standard_parallel,
	  pps->cea.false_easting,
	  pps->cea.false_northing,
	  pps->cea.sphere,
	  pps->cea.sphere);

  return ease_global_projection_description;
}

int
project_ease_global(project_parameters_t *pps,
		    double lat, double lon, double height,
		    double *x, double *y, double *z)
{
  return project_worker_arr(ease_global_projection_desc(pps),
			    &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_ease_global_arr(project_parameters_t *pps,
			double *lat, double *lon, double *height,
			double **projected_x, double **projected_y,
			double **projected_z, long length)
{
  return project_worker_arr(ease_global_projection_desc(pps),
			    lat, lon, height, projected_x, projected_y, 
			    projected_z, length);
}

int
project_ease_global_inv(project_parameters_t *pps, double x, double y, double z,
		double *lat, double *lon, double *height)
{
  return project_worker_arr_inv(ease_global_projection_desc(pps),
				&x, &y, &z, &lat, &lon, &height, 1);
}

int
project_ease_global_arr_inv(project_parameters_t *pps,
			    double *x, double *y, double *z,
			    double **lat, double **lon, double **height,
			    long length)
{
  return project_worker_arr_inv(ease_global_projection_desc(pps),
				x, y, z, lat, lon, height, length);
}

/****************************************************************************
  Albers Equal-Area Conic
****************************************************************************/
char * albers_projection_desc(project_parameters_t * pps,
			      datum_type_t datum)
{
  static char albers_projection_description[128];

  /* Establish description of output projection. */
  if (datum == WGS84_DATUM || datum == NAD27_DATUM || datum == NAD83_DATUM) {
    sprintf(albers_projection_description,
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +datum=%s +over",
        pps->albers.std_parallel1,
        pps->albers.std_parallel2,
        pps->albers.orig_latitude,
        pps->albers.center_meridian,
        datum_str(datum));
  }
  else if (datum == HUGHES_DATUM) {
    sprintf(albers_projection_description,
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
        pps->albers.std_parallel1,
        pps->albers.std_parallel2,
        pps->albers.orig_latitude,
        pps->albers.center_meridian,
            (float)HUGHES_SEMIMAJOR,
            (float)HUGHES_INV_FLATTENING);
  }
  else if (datum == SAD69_DATUM) {
    sprintf(albers_projection_description,
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
        pps->albers.std_parallel1,
        pps->albers.std_parallel2,
        pps->albers.orig_latitude,
        pps->albers.center_meridian,
            (float)GRS1967_SEMIMAJOR,
            (float)GRS1967_INV_FLATTENING);
  }
  else if (datum == ED50_DATUM) {
    sprintf(albers_projection_description,
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +a=%f +rf=%f +over",
        pps->albers.std_parallel1,
        pps->albers.std_parallel2,
        pps->albers.orig_latitude,
        pps->albers.center_meridian,
            (float)INTERNATIONAL1924_SEMIMAJOR,
            (float)INTERNATIONAL1924_INV_FLATTENING);
  } else {
    sprintf(albers_projection_description,
        "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +over",
        pps->albers.std_parallel1,
        pps->albers.std_parallel2,
        pps->albers.orig_latitude,
        pps->albers.center_meridian);
  }

  return albers_projection_description;
}

int
project_albers(project_parameters_t *pps,
         double lat, double lon, double height,
         double *x, double *y, double *z, datum_type_t datum)
{
    return project_worker_arr(albers_projection_desc(pps, datum),
            &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_albers_arr(project_parameters_t *pps,
       double *lat, double *lon, double *height,
       double **projected_x, double **projected_y,
       double **projected_z, long length, datum_type_t datum)
{
  return project_worker_arr(albers_projection_desc(pps, datum),
                lat, lon, height, projected_x, projected_y, projected_z,
                length);
}

int
project_albers_inv(project_parameters_t *pps, double x, double y, double z,
       double *lat, double *lon, double *height, datum_type_t datum)
{
    return project_worker_arr_inv(albers_projection_desc(pps, datum),
                  &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_albers_arr_inv(project_parameters_t *pps,
           double *x, double *y, double *z,
           double **lat, double **lon, double **height,
           long length, datum_type_t datum)
{
    return project_worker_arr_inv(albers_projection_desc(pps, datum),
                  x, y, z, lat, lon, height, length);
}

/******************************************************************************
  Pseudo Projection
******************************************************************************/

static char * pseudo_projection_description(datum_type_t datum)
{
  static char pseudo_projection_description[128];

  asfRequire(datum != HUGHES_DATUM,
             "Using a Hughes-1980 ellipsoid with a pseudo lat/long "
             "projection\nis not supported.\n");
  sprintf(pseudo_projection_description, "+proj=latlong +datum=%s +over",
          datum_str(datum));

  return pseudo_projection_description;
}

int
project_pseudo (project_parameters_t *pps, double lat, double lon,
    double height, double *x, double *y, double *z, datum_type_t datum)
{
  return project_worker_arr(pseudo_projection_description (datum),
          &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_pseudo_inv (project_parameters_t *pps, double x, double y,
        double z, double *lat, double *lon, double *height, datum_type_t datum)
{
  return project_worker_arr_inv(pseudo_projection_description (datum),
                &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_pseudo_arr (project_parameters_t *pps, double *lat, double *lon,
        double *height, double **x, double **y, double **z,
        long length, datum_type_t datum)
{
  return project_worker_arr(pseudo_projection_description (datum),
          lat, lon, height, x, y, z, length);
}

int project_pseudo_arr_inv (project_parameters_t *pps, double *x, double *y,
          double *z, double **lat, double **lon,
          double **height, long length, datum_type_t datum)
{
  return project_worker_arr_inv(pseudo_projection_description (datum),
        x, y, z, lat, lon, height, length);
}

// a generally useful function
int utm_zone(double lon)
{
    double lon_nudged = utm_nudge(lon);
    return (int) ((lon_nudged + 180.0) / 6.0 + 1.0);
}

int crosses_dateline(double *lon, int start, int end)
{
  int ii, dateline = FALSE;
  double min = 999;
  double max = -999;
  for (ii=start; ii<end; ii++) {
    if (lon[ii] > max)
      max = lon[ii];
    if (lon[ii] < min)
      min = lon[ii];
  }
  if (fabs(max - min) > 180)
    dateline = TRUE;

  return dateline;
}
