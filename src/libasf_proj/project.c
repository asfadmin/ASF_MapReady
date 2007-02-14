#include "projects.h"
#include "libasf_proj.h"
#include "asf.h"
#include "jpl_proj.h"
#include "asf_nan.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "proj_api.h"
#include "spheroids.h"

#define DEFAULT_AVERAGE_HEIGHT 0.0;

#define SQR(A)	((A)*(A))
#define ecc2 (sqrt(1.0 - (proj->re_minor*proj->re_minor)/(proj->re_major*proj->re_major)) \
            * sqrt(1.0 - (proj->re_minor*proj->re_minor)/(proj->re_major*proj->re_major)))

/* WGS84 is hard-coded on the lat/lon side for now! */
static const char *latlon_description = "+proj=latlong +datum=WGS84";

#ifndef linux
#ifndef win32
static double
round (double arg)
{
  return floor (arg + 0.5);
}
#endif /* #ifndef win32 */
#endif /* #ifndef linux */

// Fill in major and minor with the axes lenghts of spheroid.  This is
// a cut-and-paste implementation of a function defined in asf_meta.h,
// that we can't actually use here because we don't actually link
// against that library (only include the header).
static void
project_spheroid_axes_lengths (spheroid_type_t spheroid, double *major,
			       double *minor)
{
  switch ( spheroid ) {
  case BESSEL_SPHEROID:
    *major = BESSEL_SEMIMAJOR;
    *minor = *major - (1.0 / BESSEL_INV_FLATTENING) * *major;
    break;
  case CLARKE1866_SPHEROID:
    *major = CLARKE1866_SEMIMAJOR;
    *minor = *major - (1.0 / CLARKE1866_INV_FLATTENING) * *major;
    break;
  case CLARKE1880_SPHEROID:
    *major = CLARKE1880_SEMIMAJOR;
    *minor = *major - (1.0 / CLARKE1880_INV_FLATTENING) * *major;
    break;
  case GEM6_SPHEROID:
    *major = GEM6_SEMIMAJOR;
    *minor = *major - (1.0 / GEM6_INV_FLATTENING) * *major;
    break;
  case GEM10C_SPHEROID:
    *major = GEM10C_SEMIMAJOR;
    *minor = *major - (1.0 / GEM10C_INV_FLATTENING) * *major;
    break;
  case GRS1980_SPHEROID:
    *major = GRS1980_SEMIMAJOR;
    *minor = *major - (1.0 / GRS1980_INV_FLATTENING) * *major;
    break;
  case INTERNATIONAL1924_SPHEROID:
    *major = INTERNATIONAL1924_SEMIMAJOR;
    *minor = *major - (1.0 / INTERNATIONAL1924_INV_FLATTENING) * *major;
    break;
  case INTERNATIONAL1967_SPHEROID:
    *major = INTERNATIONAL1967_SEMIMAJOR;
    *minor = *major - (1.0 / INTERNATIONAL1967_INV_FLATTENING) * *major;
    break;
  case WGS72_SPHEROID:
    *major = WGS72_SEMIMAJOR;
    *minor = *major - (1.0 / WGS72_INV_FLATTENING) * *major;
    break;
  case WGS84_SPHEROID:
    *major = WGS84_SEMIMAJOR;
    *minor = *major - (1.0 / WGS84_INV_FLATTENING) * *major;
    break;
  default:
    assert (0);
    break;
  }
}

static void set_proj_lib_path(datum_type_t datum)
{
    static int set_path_already = FALSE;
    if (!set_path_already) {
        if (datum == NAD27_DATUM) {
            // point to the grid shift files
            char proj_putenv[255];
            sprintf(proj_putenv, "PROJ_LIB=%s/proj", get_asf_share_dir());
            putenv(proj_putenv);
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
    default:
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
    px[0] = lat*D2R;
    py[0] = lon*D2R;
    pz[0] = 0;

    pj_transform (ll_proj, utm_proj, 1, 1, px, py, pz);

    int ret = TRUE;
    if (pj_errno == -38) // -38 indicates error with the grid shift files
    {
        ret = FALSE;
    }
    else if (pj_errno > 0) // some other error
    {
        asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
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

  geographic_projection = pj_init_plus (latlon_description);

  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      ok = FALSE;
  }
  
  if (ok)
  {
      assert (geographic_projection != NULL);
      
      output_projection = pj_init_plus (projection_description);
      
      if (pj_errno != 0)
      {
	  asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
	  ok = FALSE;
      }
  
      if (ok)
      {
	  assert (output_projection != NULL);
	  
	  pj_transform (geographic_projection, output_projection, length, 1, 
			px, py, pz);
	  
	  if (pj_errno != 0)
	  {
	      asfPrintWarning("libproj error: %s\n", pj_strerrno(pj_errno));
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

  geographic_projection = pj_init_plus ( latlon_description );

  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      ok = FALSE;
  }
  
  if (ok)
  {
      assert (geographic_projection != NULL);
  
      output_projection = pj_init_plus (projection_description);
      
      if (pj_errno != 0)
      {
	  asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
	  ok = FALSE;
      }
      
      if (ok)
      {
	  assert (output_projection != NULL);
	  
	  pj_transform (output_projection, geographic_projection, length, 1, 
			plon, plat, pheight);
	  	  
	  if (pj_errno != 0)
	  {
	      asfPrintWarning("libproj error: %s\n", pj_strerrno(pj_errno));
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

static const char * 
utm_projection_description(project_parameters_t * pps, datum_type_t datum)
{
  static char utm_projection_description[128];

  /* Establish description of output projection. */
  if (pps->utm.zone == MAGIC_UNSET_INT)
  {
      sprintf(utm_projection_description,
	      "+proj=utm +lon_0=%f +datum=%s",
	      utm_nudge(pps->utm.lon0 * RAD_TO_DEG), datum_str(datum));
  }
  else
  {
      sprintf(utm_projection_description,
	      "+proj=utm +zone=%d %s+datum=%s",
	      pps->utm.zone, pps->utm.lat0 < 0 ? "+south " : "", 
          datum_str(datum));
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
static const char * 
ps_projection_desc(project_parameters_t * pps, datum_type_t datum)
{
  static char ps_projection_description[128];

  /* Establish description of output projection. */
  sprintf(ps_projection_description,
	  "+proj=stere +lat_0=%s +lat_ts=%f +lon_0=%f "
      "+k_0=%f +datum=%s",
	  pps->ps.is_north_pole ? "90" : "-90",
	  pps->ps.slat * RAD_TO_DEG,
	  pps->ps.slon * RAD_TO_DEG,
	  1.0 /* pps->ps.scale_factor */,
	  datum_str(datum));

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
static char * lamaz_projection_desc(project_parameters_t * pps,
                                    datum_type_t datum)
{
  static char lamaz_projection_description[128];

  /* Establish description of output projection. */
  sprintf(lamaz_projection_description,
	  "+proj=laea +lat_0=%f +lon_0=%f +datum=%s",
	  pps->lamaz.center_lat * RAD_TO_DEG,
	  pps->lamaz.center_lon * RAD_TO_DEG,
	  datum_str(datum));

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
static char * lamcc_projection_desc(project_parameters_t * pps,
                                    datum_type_t datum)
{
  static char lamcc_projection_description[128];

  /* Establish description of output projection. */
  sprintf(lamcc_projection_description,
	  "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +datum=%s",
	  pps->lamcc.plat1 * RAD_TO_DEG,
	  pps->lamcc.plat2 * RAD_TO_DEG,
	  pps->lamcc.lat0 * RAD_TO_DEG,
	  pps->lamcc.lon0 * RAD_TO_DEG,
	  datum_str(datum));

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

/****************************************************************************
  Albers Equal-Area Conic
****************************************************************************/
static char * albers_projection_desc(project_parameters_t * pps,
                                     datum_type_t datum)
{
  static char albers_projection_description[128];

  /* Establish description of output projection. */
  sprintf(albers_projection_description,
	  "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +datum=%s",
	  pps->albers.std_parallel1 * RAD_TO_DEG,
	  pps->albers.std_parallel2 * RAD_TO_DEG,
	  pps->albers.orig_latitude * RAD_TO_DEG,
	  pps->albers.center_meridian * RAD_TO_DEG,
	  datum_str(datum));

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

  sprintf(pseudo_projection_description, "+proj=latlong +datum=%s",
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

/*Convert projection units (meters) to geodetic latitude and longitude (degrees).*/
void proj_to_latlon(meta_projection *proj, double x, double y, double z,
		    double *lat, double *lon, double *height)
{
  if (proj==NULL)
    bail("NULL projection parameter structure passed to proj_to_latlon!\n");

  switch(proj->type)
    {
    case ALBERS_EQUAL_AREA:
      project_albers_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_lamaz_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum);
      break;
    case LAMBERT_CONFORMAL_CONIC: 
      project_lamcc_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum); 
      break;
    case POLAR_STEREOGRAPHIC: 
      project_ps_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum); 
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR: 
      project_utm_inv(&(proj->param), x, y, z, lat, lon, height, proj->datum); 
      break;
    case SCANSAR_PROJECTION: 
      asfPrintError("'proj_to_latlon' not defined for SCANSAR_PROJECTION.\n"
		    "Use 'scan_latlon' instead.\n");
      break;
    case LAT_LONG_PSEUDO_PROJECTION: 
      *lat = y; 
      *lon = x; 
      break;
    default:
      printf("Unrecognized map projection '%c' passed to proj_to_latlon!\n",
	     proj->type);
      exit(1);
    }
}

void rotate_z(vector *v,double theta)
{
	double xNew,yNew;
	
	xNew = v->x*cosd(theta)+v->y*sind(theta);
	yNew = -v->x*sind(theta)+v->y*cosd(theta);
	v->x = xNew; v->y = yNew;
}

void rotate_y(vector *v,double theta)
{
	double xNew,zNew;
	
	zNew = v->z*cosd(theta)+v->x*sind(theta);
	xNew = -v->z*sind(theta)+v->x*cosd(theta);
	v->x = xNew; v->z = zNew;
}

void scan_to_latlon(meta_parameters *meta,
		    double x, double y, double z,
		    double *lat_d, double *lon, double *height)
{
	double qlat, qlon;
	double lat,radius;
	vector pos;
        meta_projection *proj = meta->projection;

        if (z != 0.0) {
            // height correction applies directly to y (range direction)
            double line, samp;
            line = (y-proj->startY)/proj->perY - meta->general->start_line;
            samp = (x-proj->startX)/proj->perX - meta->general->start_sample;
            double sr = meta_get_slant(meta,line,samp);
            double er = proj->param.atct.rlocal;
            double ht = meta_get_sat_height(meta,line,samp);
            double incid = PI-acos((SQR(sr) + SQR(er) - SQR(ht))/(2.0*sr*er));
            x += z*tan(PI/2-incid);
        }

	if (meta->sar->look_direction=='R')
		qlat = -x/proj->param.atct.rlocal; /* Right looking sar */
	else
		qlat =  x/proj->param.atct.rlocal; /* Left looking sar */
	qlon = y/(proj->param.atct.rlocal*cos(qlat));
	
	sph2cart(proj->param.atct.rlocal, qlat, qlon, &pos);
	
	rotate_z(&pos,-proj->param.atct.alpha3);
	rotate_y(&pos,-proj->param.atct.alpha2);
	rotate_z(&pos,-proj->param.atct.alpha1);
	
	cart2sph(pos,&radius,&lat,lon);
	*lon *= R2D;
	lat *= R2D;
	*lat_d = atand(tand(lat) / (1-ecc2));
        *height = z; // FIXME
}


void ll_ac(meta_projection *proj, char look_dir, double lat_d, double lon, double *c1, double *c2)
{
	double qlat, qlon;
	double lat,radius;
	vector pos;
	
	lat= atand(tand(lat_d)*(1 - ecc2));
	sph2cart(proj->param.atct.rlocal,lat*D2R,lon*D2R,&pos);
	
	rotate_z(&pos,proj->param.atct.alpha1);
	rotate_y(&pos,proj->param.atct.alpha2);
	rotate_z(&pos,proj->param.atct.alpha3);
	
	cart2sph(pos,&radius,&qlat,&qlon);
	
	*c1 = qlon*proj->param.atct.rlocal*cos(qlat);
	if (look_dir=='R')
		*c2 = -1.0*qlat*proj->param.atct.rlocal;  /* right looking */
	else
		*c2 = qlat * proj->param.atct.rlocal;   /* left looking */
}


/*Convert projection units (meters) from geodetic latitude and longitude (degrees).*/
void latlon_to_proj(meta_projection *proj, char look_dir,
		    double lat, double lon, double height,
		    double *x, double *y, double *z)
{
  if (proj==NULL)
    bail("NULL projection parameter structure passed to ll_to_proj!\n");

  switch (proj->type)
    {
    case SCANSAR_PROJECTION: 
      ll_ac(proj, look_dir, lat, lon, y, x); 
      break;
    case ALBERS_EQUAL_AREA:
      project_albers(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_lamaz(&(proj->param), lat, lon, height, x, y, z, proj->datum);
      break;
    case LAMBERT_CONFORMAL_CONIC: 
      project_lamcc(&(proj->param), lat, lon, height, x, y, z, proj->datum); 
      break;
    case POLAR_STEREOGRAPHIC: 
      project_ps(&(proj->param), lat, lon, height, x, y, z, proj->datum); 
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR: 
      project_utm(&(proj->param), lat, lon, height, x, y, z, proj->datum); 
      break;
    default:
      printf("Unrecognized map projection '%c' passed to latlon_to_proj!\n",
	     proj->type);
      exit(1);
    }
}

int utm_zone(double lon)
{
    double lon_nudged = utm_nudge(lon);
    return (int) ((lon_nudged + 180.0) / 6.0 + 1.0);
}

void fill_in_utm(double lat, double lon, project_parameters_t *pps)
{
  pps->utm.zone = utm_zone(lon);
  pps->utm.scale_factor = 0.9996;
  pps->utm.lon0 = (double) (pps->utm.zone - 1) * 6.0 - 177.0;
  pps->utm.lat0 = 0.0;
  pps->utm.false_easting = 500000.0;
  if (lat > 0.0)
    pps->utm.false_northing = 0.0;
  else
    pps->utm.false_northing = 10000000.0;
}

static void latLon2proj_imp(double lat, double lon, double elev, 
                            char *projFile, double *projX, double *projY)
{
  project_parameters_t pps;
  projection_type_t proj_type;
  meta_projection *meta_proj;
  double projZ;

  if (projFile)
  {
      // Read projection file
      read_proj_file(projFile, &pps, &proj_type);
      
      // Report the conversion type
      switch(proj_type) 
      {
          case ALBERS_EQUAL_AREA:
              printf("Lat/Lon to Albers Equal Area\n\n");
              break;
          case LAMBERT_AZIMUTHAL_EQUAL_AREA:
              printf("Lat/Lon to Lambert Azimuthal Equal Area\n\n");
              break;
          case LAMBERT_CONFORMAL_CONIC:
              printf("Lat/Lon to Lambert Conformal Conic\n\n");
              break;
          case POLAR_STEREOGRAPHIC:
              printf("Lat/Lon to Polar Stereographic\n\n");
              break;
          case UNIVERSAL_TRANSVERSE_MERCATOR:
              printf("Lat/Lon to UTM\n\n");
              break;
          case STATE_PLANE:
              // Not implemented.
              assert (0);
              break;
          case SCANSAR_PROJECTION:
              // Not implemented.
              assert (0);
              break;
          case LAT_LONG_PSEUDO_PROJECTION:
              // Not implemented.
              assert (0);
              break;
          default:
              assert (0);		// Shouldn't be here.
              break;
      }
  }
  else
  {
      proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;
  }

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM;

  if (proj_type == UNIVERSAL_TRANSVERSE_MERCATOR)
    fill_in_utm(lat, lon, &meta_proj->param);
  else
    meta_proj->param = pps;
  latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, elev, projX, projY, &projZ);
}

void latLon2UTM(double lat, double lon, double elev,
                double *projX, double *projY)
{
    latLon2proj_imp(lat, lon, elev, NULL, projX, projY);
}

void latLon2UTM_zone(double lat, double lon, double elev, int zone,
                double *projX, double *projY)
{
  project_parameters_t pps;
  meta_projection *meta_proj;
  double projZ;

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = UNIVERSAL_TRANSVERSE_MERCATOR;
  meta_proj->datum = WGS84_DATUM;
  pps.utm.zone = zone;
  pps.utm.scale_factor = 0.9996;
  pps.utm.lon0 = (double) (zone - 1) * 6.0 - 177.0;
  pps.utm.lat0 = 0.0;
  pps.utm.false_easting = 500000.0;
  pps.utm.false_northing = lat>0 ? 0.0 : 10000000.0;
  meta_proj->param = pps;
  latlon_to_proj(meta_proj, 'R', lat*D2R, lon*D2R, elev, projX, projY, &projZ);
}

void latLon2proj(double lat, double lon, double elev, char *projFile, 
		 double *projX, double *projY)
{
    asfRequire(projFile != NULL, "A projection file is required.\n");
    latLon2proj_imp(lat, lon, elev, projFile, projX, projY);
}

void UTM2latLon(double projX, double projY, double elev, int zone,
                double *lat, double *lon)
{
  project_parameters_t pps;
  projection_type_t proj_type;
  meta_projection *meta_proj;
  double h;

  proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;

  pps.utm.zone = zone;
  pps.utm.scale_factor = 0.9996;
  pps.utm.lon0 = (double) (zone - 1) * 6.0 - 177.0;
  pps.utm.lat0 = 0.0;
  pps.utm.false_easting = 500000.0;
  pps.utm.false_northing = lat>0 ? 0.0 : 10000000.0;

  // Initialize meta_projection block
  meta_proj = meta_projection_init();
  meta_proj->type = proj_type;
  meta_proj->datum = WGS84_DATUM; // assumed...
  meta_proj->param = pps;
  
  proj_to_latlon(meta_proj, projX, projY, elev, lat, lon, &h);

  *lat *= R2D;
  *lon *= R2D;
}

void to_radians(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
            if (!ISNAN(pps->utm.lon0))
                pps->utm.lon0 *= DEG_TO_RAD;
            if (!ISNAN(pps->utm.lat0))
                pps->utm.lat0 *= DEG_TO_RAD;
            
            break;
            
	case POLAR_STEREOGRAPHIC:
            if (!ISNAN(pps->ps.slon))
                pps->ps.slon *= DEG_TO_RAD;
            if (!ISNAN(pps->ps.slat))
                pps->ps.slat *= DEG_TO_RAD;
            
            break;
            
	case ALBERS_EQUAL_AREA:
            if (!ISNAN(pps->albers.center_meridian))
                pps->albers.center_meridian *= DEG_TO_RAD;
            if (!ISNAN(pps->albers.orig_latitude))
                pps->albers.orig_latitude *= DEG_TO_RAD;
            if (!ISNAN(pps->albers.std_parallel1))
                pps->albers.std_parallel1 *= DEG_TO_RAD;
            if (!ISNAN(pps->albers.std_parallel2))
                pps->albers.std_parallel2 *= DEG_TO_RAD;
            
            break;
            
	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
            if (!ISNAN(pps->lamaz.center_lat))
                pps->lamaz.center_lat *= DEG_TO_RAD;
            if (!ISNAN(pps->lamaz.center_lon))
                pps->lamaz.center_lon *= DEG_TO_RAD;
            
            break;
            
	case LAMBERT_CONFORMAL_CONIC:
            if (!ISNAN(pps->lamcc.plat1))
                pps->lamcc.plat1 *= DEG_TO_RAD;
            if (!ISNAN(pps->lamcc.plat2))
                pps->lamcc.plat2 *= DEG_TO_RAD;
            if (!ISNAN(pps->lamcc.lat0))
                pps->lamcc.lat0 *= DEG_TO_RAD;
            if (!ISNAN(pps->lamcc.lon0))
                pps->lamcc.lon0 *= DEG_TO_RAD;
            
            break;
            
	default:
            asfPrintError("Image file is not map-projected.  Use asf_geocode or\n"
                          "Geocode tab to geocode the image file before proceeding.\n");
    }
}

void to_degrees(projection_type_t pt, project_parameters_t * pps)
{
    switch (pt)
    {
	case UNIVERSAL_TRANSVERSE_MERCATOR:
            if (!ISNAN(pps->utm.lon0))
                pps->utm.lon0 *= RAD_TO_DEG;
            if (!ISNAN(pps->utm.lat0))
                pps->utm.lat0 *= RAD_TO_DEG;
            
            break;
            
	case POLAR_STEREOGRAPHIC:
            if (!ISNAN(pps->ps.slon))
                pps->ps.slon *= RAD_TO_DEG;
            if (!ISNAN(pps->ps.slat))
                pps->ps.slat *= RAD_TO_DEG;

            break;
            
	case ALBERS_EQUAL_AREA:
            if (!ISNAN(pps->albers.center_meridian))
                pps->albers.center_meridian *= RAD_TO_DEG;
            if (!ISNAN(pps->albers.orig_latitude))
                pps->albers.orig_latitude *= RAD_TO_DEG;
            if (!ISNAN(pps->albers.std_parallel1))
                pps->albers.std_parallel1 *= RAD_TO_DEG;
            if (!ISNAN(pps->albers.std_parallel2))
                pps->albers.std_parallel2 *= RAD_TO_DEG;
            
            break;
            
	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
            if (!ISNAN(pps->lamaz.center_lat))
                pps->lamaz.center_lat *= RAD_TO_DEG;
            if (!ISNAN(pps->lamaz.center_lon))
                pps->lamaz.center_lon *= RAD_TO_DEG;
            
            break;
            
	case LAMBERT_CONFORMAL_CONIC:
            if (!ISNAN(pps->lamcc.plat1))
                pps->lamcc.plat1 *= RAD_TO_DEG;
            if (!ISNAN(pps->lamcc.plat2))
                pps->lamcc.plat2 *= RAD_TO_DEG;
            if (!ISNAN(pps->lamcc.lat0))
                pps->lamcc.lat0 *= RAD_TO_DEG;
            if (!ISNAN(pps->lamcc.lon0))
                pps->lamcc.lon0 *= RAD_TO_DEG;
            
            break;
            
	default:
            asfPrintError("Image file is not map-projected.  Use asf_geocode or\n"
                          "Geocode tab to geocode the image file before proceeding.\n");
    }
}
