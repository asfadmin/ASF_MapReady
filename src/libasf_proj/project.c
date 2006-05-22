#include "projects.h"
#include "libasf_proj.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_reporting.h"

#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "proj_api.h"
#include "spheroids.h"

#define DEFAULT_AVERAGE_HEIGHT 0.0;

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

// The default as advertised in the interface.
static datum_type_t input_spheroid = GEM6_SPHEROID;
void project_set_input_spheroid(spheroid_type_t spheroid)
{
  input_spheroid = spheroid;
}

static datum_type_t sDatum = MAGIC_UNSET_INT;
void project_set_datum(datum_type_t datum)
{
    sDatum = datum;
}
static datum_type_t datum_type(void)
{
  if ( sDatum == MAGIC_UNSET_INT ) {
    return WGS84_DATUM;
  }
  else {
    return sDatum;
  }
}

static const char * datum(project_parameters_t * pps)
{
    if (sDatum == MAGIC_UNSET_INT)
	return "WGS84";

    switch (sDatum)
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

/*
  We aren't really using this function... just a basic test from
  right out of the libproj manual, to get the ball rolling.  Only
  used in the test driver as a sanity check
*/

int project_poly(double phi_deg, double lam_deg, double * xx, double *yy)
{
    char * args[] = { "proj=poly", "ellps=clrk66", "lon_0=90W" };

    projPJ ref;
    projUV idata;
    projUV odata;

    ref = pj_init(3, args);
    if (pj_errno != 0)
	asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
    if (!ref)
	asfPrintError("libproj Error: null ref returned from pj_init\n");

    idata.v = phi_deg * DEG_TO_RAD;
    idata.u = lam_deg * DEG_TO_RAD;

    odata = pj_fwd(idata, ref);
    if (pj_errno != 0)
	asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));

    *xx = odata.u;
    *yy = odata.v;

    pj_free(ref);
    if (pj_errno != 0)
	asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));

    return 1;
}

static int project_worker_arr(char * projection_description,
			      double *lat, double *lon, double *height,
			      double **projected_x, double **projected_y,
			      double **projected_z, long length)
{
  projPJ geographic_projection, output_projection;
  int i, ok = TRUE;
  char latlon_projection[256];

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

  double spheroid_a, spheroid_b;
  project_spheroid_axes_lengths (input_spheroid, &spheroid_a, &spheroid_b);

  sprintf(latlon_projection, "+proj=latlong +a=%lf +b=%lf", spheroid_a,
	  spheroid_b);

  geographic_projection = pj_init_plus (latlon_projection);
  
/*
  printf("projection description: %s %s\n", latlon_projection,
  	 projection_description);
*/

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
	      asfPrintWarning("libproj Error: %s\n", pj_strerrno(pj_errno));
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

static int project_worker_arr_inv(char * projection_description,
				  double *x, double *y, double *z,
				  double **lat, double **lon, double **height,
				  long length)
{
  projPJ geographic_projection, output_projection;
  int i, ok = TRUE;
  char latlon_projection[256];

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
    plat[i] = x[i];
    plon[i] = y[i];
    
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

  double spheroid_a, spheroid_b;
  project_spheroid_axes_lengths (input_spheroid, &spheroid_a, &spheroid_b);

  sprintf(latlon_projection, "+proj=latlong +a=%lf +b=%lf", spheroid_a,
	  spheroid_b);

  geographic_projection = pj_init_plus ( latlon_projection );
  
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
			plat, plon, pheight);
	  	  
	  if (pj_errno != 0)
	  {
	      asfPrintWarning("libproj Error: %s\n", pj_strerrno(pj_errno));
	      ok = FALSE;
	  }
	  
	  if (ok)
	  {	  
	      for (i = 0; i < length; ++i)
	      {
		  double t = plat[i];
		  plat[i] = plon[i];
		  plon[i] = t;
	      }
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

static char * utm_projection_description(project_parameters_t * pps)
{
  static char utm_wgs84_projection_description[128];

  /* Establish description of output projection. */
  if (pps->utm.zone == MAGIC_UNSET_INT)
  {
      sprintf(utm_wgs84_projection_description,
	      "+proj=utm +lon_0=%f +datum=%s",
	      utm_nudge(pps->utm.lon0 * RAD_TO_DEG), datum(pps));
  }
  else
  {
      sprintf(utm_wgs84_projection_description,
	      "+proj=utm +zone=%d %s+datum=%s",
	      pps->utm.zone, pps->utm.lat0 < 0 ? "+south " : "", datum(pps));
  }

  return utm_wgs84_projection_description;
}

int
project_utm (project_parameters_t * pps, double lat, double lon, double height,
	     double *x, double *y, double *z)
{
  return project_worker_arr(utm_projection_description(pps),
			    &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_utm_arr (project_parameters_t * pps, 
		 double *lat, double *lon, double *height,
		 double **projected_x, double **projected_y, 
		 double **projected_z, long length)
{
  return project_worker_arr(
      utm_projection_description(pps),
      lat, lon, height, projected_x, projected_y, projected_z, length);
}

int
project_utm_inv (project_parameters_t * pps,
		 double x, double y, double z,  double *lat, double *lon,
		 double *height)
{
  return project_worker_arr_inv(
      utm_projection_description(pps),
      &x, &y, &z, &lat, &lon, &height, 1);
}

int
project_utm_arr_inv (project_parameters_t * pps,
		     double *x, double *y, double *z,
		     double **lat, double **lon, double **height,
		     long length)
{
  return project_worker_arr_inv(
      utm_projection_description(pps),
      x, y, z, lat, lon, height, length);
}

/****************************************************************************
 Polar Sterographic (PS)
****************************************************************************/
static char * ps_projection_desc(project_parameters_t * pps)
{
  static char ps_wgs84_projection_description[128];

  /* Establish description of output projection. */
  sprintf(ps_wgs84_projection_description,
	  "+proj=stere +lat_0=%s +lat_ts=%f +lon_0=%f "
	  "+x_0=%f +y_0=%f +k_0=%f +datum=%s",
	  pps->ps.is_north_pole ? "90" : "-90",
	  pps->ps.slat * RAD_TO_DEG,
	  pps->ps.slon * RAD_TO_DEG,
	  pps->ps.false_easting, pps->ps.false_northing,
	  1.0 /* pps->ps.scale_factor */,
	  datum(pps));

  return ps_wgs84_projection_description;
}

int
project_ps(project_parameters_t * pps,
	   double lat, double lon, double height,
	   double *x, double *y, double *z)
{
  return project_worker_arr(ps_projection_desc(pps), &lat, &lon, &height,
			    &x, &y, &z, 1);
}

int
project_ps_arr(project_parameters_t * pps,
	       double *lat, double *lon, double *height,
	       double **projected_x, double **projected_y,
	       double **projected_z, long length)
{
  return project_worker_arr(ps_projection_desc(pps), lat, lon, height, 
			    projected_x, projected_y, projected_z, length);
}

int
project_ps_inv(project_parameters_t * pps, double x, double y, double z,
	       double *lat, double *lon, double *height)
{
    return project_worker_arr_inv(ps_projection_desc(pps), &x, &y, &z, 
				  &lat, &lon, &height, 1);
}

int
project_ps_arr_inv(project_parameters_t * pps,
		   double *x, double *y, double *z,
		   double **lat, double **lon, double **height,
		   long length)
{
    return project_worker_arr_inv(ps_projection_desc(pps), 
				  x, y, z, lat, lon, height, length);
}

/****************************************************************************
 Lambert Azimuthal Equal Area
****************************************************************************/
static char * lamaz_projection_desc(project_parameters_t * pps)
{
  static char lamaz_projection_description[128];

  /* Establish description of output projection. */
  sprintf(lamaz_projection_description,
	  "+proj=laea +lat_0=%f +lon_0=%f +x_0=%f +y_0=%f +datum=%s",
	  pps->lamaz.center_lat * RAD_TO_DEG,
	  pps->lamaz.center_lon * RAD_TO_DEG,
	  pps->lamaz.false_easting,
	  pps->lamaz.false_northing,
	  datum(pps));

  return lamaz_projection_description;
}

int
project_lamaz(project_parameters_t * pps,
	      double lat, double lon, double height,
	      double *x, double *y, double *z)
{
    return project_worker_arr(lamaz_projection_desc(pps), &lat, &lon, &height,
			      &x, &y, &z, 1);
}

int
project_lamaz_arr(project_parameters_t *pps,
		  double *lat, double *lon, double *height, 
		  double **projected_x, double **projected_y,
		  double **projected_z, long length)
{
    return project_worker_arr(lamaz_projection_desc(pps), lat, lon, height,
			      projected_x, projected_y, projected_z, length);
}

int
project_lamaz_inv(project_parameters_t *pps, double x, double y, double z,
		  double *lat, double *lon, double *height)
{
  return project_worker_arr_inv(lamaz_projection_desc(pps), &x, &y, &z, 
				&lat, &lon, &height, 1);
}

int
project_lamaz_arr_inv(project_parameters_t *pps,
		      double *x, double *y, double *z, 
		      double **lat, double **lon, double **height,
		      long length)
{
  return project_worker_arr_inv(lamaz_projection_desc(pps), x, y, z,
				lat, lon, height, length);
}

/****************************************************************************
 Lambert Conformal Conic
****************************************************************************/
static char * lamcc_projection_desc(project_parameters_t * pps)
{
  static char lamcc_projection_description[128];

  /* Establish description of output projection. */
  sprintf(lamcc_projection_description,
	  "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f "
	  "+x_0=%f +y_0=%f +datum=%s",
	  pps->lamcc.plat1 * RAD_TO_DEG,
	  pps->lamcc.plat2 * RAD_TO_DEG,
	  pps->lamcc.lat0 * RAD_TO_DEG,
	  pps->lamcc.lon0 * RAD_TO_DEG,
	  pps->lamcc.false_easting,
	  pps->lamcc.false_northing,
	  datum(pps));

  return lamcc_projection_description;
}

int
project_lamcc(project_parameters_t * pps,
	      double lat, double lon, double height,
	      double *x, double *y, double *z)
{
    return project_worker_arr(lamcc_projection_desc(pps), &lat, &lon, &height,
			      &x, &y, &z, 1);
}

int
project_lamcc_arr(project_parameters_t *pps,
		  double *lat, double *lon, double *height,
		  double **projected_x, double **projected_y,
		  double **projected_z, long length)
{
  return project_worker_arr(lamcc_projection_desc(pps), lat, lon, height,
			    projected_x, projected_y, projected_z, length);
}

int
project_lamcc_inv(project_parameters_t *pps, double x, double y, double z,
		  double *lat, double *lon, double *height)
{
  return project_worker_arr_inv(lamcc_projection_desc(pps), &x, &y, &z,
				&lat, &lon, &height, 1);
}

int
project_lamcc_arr_inv(project_parameters_t *pps,
		      double *x, double *y, double *z,
		      double **lat, double **lon, double **height, 
		      long length)
{
  return project_worker_arr_inv(lamcc_projection_desc(pps), x, y, z,
				lat, lon, height, length);
}

/****************************************************************************
  Albers Equal-Area Conic
****************************************************************************/
static char * albers_projection_desc(project_parameters_t * pps)
{
  static char albers_projection_description[128];

  /* Establish description of output projection. */
  sprintf(albers_projection_description,
	  "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f "
	  "+x_0=%f +y_0=%f +datum=%s",
	  pps->albers.std_parallel1 * RAD_TO_DEG,
	  pps->albers.std_parallel2 * RAD_TO_DEG,
	  pps->albers.orig_latitude * RAD_TO_DEG,
	  pps->albers.center_meridian * RAD_TO_DEG,
	  pps->albers.false_easting,
	  pps->albers.false_northing,
	  datum(pps));

  return albers_projection_description;
}

int
project_albers(project_parameters_t *pps,
	       double lat, double lon, double height,
	       double *x, double *y, double *z)
{
    return project_worker_arr(albers_projection_desc(pps),
			      &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_albers_arr(project_parameters_t *pps,
		   double *lat, double *lon, double *height,
		   double **projected_x, double **projected_y,
		   double **projected_z, long length)
{
  return project_worker_arr(albers_projection_desc(pps), lat, lon, height,
			    projected_x, projected_y, projected_z, length);
}

int
project_albers_inv(project_parameters_t *pps,
		   double x, double y, double z,
		   double *lat, double *lon, double *height)
{
    return project_worker_arr_inv(albers_projection_desc(pps), &x, &y, &z,
				  &lat, &lon, &height, 1);
}

int
project_albers_arr_inv(project_parameters_t *pps,
		       double *x, double *y, double *z,
		       double **lat, double **lon, double **height,
		       long length)
{
    return project_worker_arr_inv(albers_projection_desc(pps), x, y, z,
				  lat, lon, height, length);
}

/******************************************************************************
  Pseudo Projection
******************************************************************************/

static char * pseudo_projection_description(void)
{
  static char pseudo_projection_description[128];

  // Spheroid semimajor and semiminor axes associated with current datum.
  double spheroid_a, spheroid_b;
  project_spheroid_axes_lengths (datum_spheroid (datum_type ()), &spheroid_a,
				 &spheroid_b);


  sprintf(pseudo_projection_description,
	  "+proj=latlong +a=%lf +b=%lf", spheroid_a, spheroid_b);

  return pseudo_projection_description;
}

int
project_pseudo (project_parameters_t *pps, double lat, double lon,
		double height, double *x, double *y, double *z)
{
  return project_worker_arr(pseudo_projection_description (),
			    &lat, &lon, &height, &x, &y, &z, 1);
}

int
project_pseudo_inv (project_parameters_t *pps, double x, double y,
		    double z, double *lat, double *lon, double *height)
{
  return project_worker_arr_inv(pseudo_projection_description (), &x, &y, &z,
				&lat, &lon, &height, 1);
}

int
project_pseudo_arr (project_parameters_t *pps, double *lat, double *lon,
		    double *height, double **x, double **y, double **z,
		    long length)
{
  return project_worker_arr(pseudo_projection_description (),
			    lat, lon, height, x, y, z, length);
}

int project_pseudo_arr_inv (project_parameters_t *pps, double *x, double *y,
			    double *z, double **lat, double **lon,
			    double **height, long length)
{
  return project_worker_arr_inv(pseudo_projection_description (),
				x, y, z, lat, lon, height, length);
}

/*Convert projection units (meters) to geodetic latitude and longitude (degrees).*/
void proj_to_latlon(meta_projection *proj, char look_dir,
		    double x, double y, double z,
		    double *lat, double *lon, double *height)
{
  if (proj==NULL)
    bail("NULL projection parameter structure passed to proj_to_ll!\n");

  project_set_datum(proj->datum);

  switch(proj->type)
    {
    case SCANSAR_PROJECTION: 
      ac_ll(proj,look_dir, y, x, lat, lon); 
      break;
    case ALBERS_EQUAL_AREA:
      project_albers_inv(&(proj->param), x, y, z, lat, lon, height);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_lamaz_inv(&(proj->param), x, y, z, lat, lon, height);
      break;
    case LAMBERT_CONFORMAL_CONIC: 
      project_lamcc_inv(&(proj->param), x, y, z, lat, lon, height); 
      break;
    case POLAR_STEREOGRAPHIC: 
      project_ps_inv(&(proj->param), x, y, z, lat, lon, height); 
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR: 
      project_utm_inv(&(proj->param), x, y, z, lat, lon, height); 
      break;
    default:
      printf("Unrecognized map projection '%c' passed to proj_to_latlon!\n",
	     proj->type);
      exit(1);
    }
}



/*Convert projection units (meters) from geodetic latitude and longitude (degrees).*/
void latlon_to_proj(meta_projection *proj, char look_dir,
		    double lat, double lon, double height,
		    double *x, double *y, double *z)
{
  if (proj==NULL)
    bail("NULL projection parameter structure passed to ll_to_proj!\n");

  project_set_datum(proj->datum);

  switch (proj->type)
    {
    case SCANSAR_PROJECTION: 
      ll_ac(proj, look_dir, lat, lon, y, x); 
      break;
    case ALBERS_EQUAL_AREA:
      project_albers(&(proj->param), lat, lon, height, x, y, z);
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_lamaz(&(proj->param), lat, lon, height, x, y, z);
      break;
    case LAMBERT_CONFORMAL_CONIC: 
      project_lamcc(&(proj->param), lat, lon, height, x, y, z); 
      break;
    case POLAR_STEREOGRAPHIC: 
      project_ps(&(proj->param), lat, lon, height, x, y, z); 
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR: 
      project_utm(&(proj->param), lat, lon, height, x, y, z); 
      break;
    default:
      printf("Unrecognized map projection '%c' passed to latlon_to_proj!\n",
	     proj->type);
      exit(1);
    }
}
