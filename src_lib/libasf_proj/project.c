#include "libasf_proj.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_reporting.h"

#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "projects.h"
#include "proj_api.h"

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

static datum_type_t sDatum = MAGIC_UNSET_INT;
void project_set_datum(datum_type_t datum)
{
    sDatum = datum;
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
			      double *lat, double *lon,
			      double **projected_x, double **projected_y,
			      long length)
{
  double * tmp3;
  projPJ geographic_projection, output_projection;
  int i, ok = TRUE;

  if (!(*projected_x))
  {
      /* user should either pre-allocate both, or want us to allocate
	 both -- to preallocate one and not the other is weird and
	 probably a mistake */
      assert(!(*projected_y));

      *projected_x = (double *) MALLOC (sizeof(double) * length);
      *projected_y = (double *) MALLOC (sizeof(double) * length);
  }

  double *px = *projected_x;
  double *py = *projected_y;

  assert(px);
  assert(py);

  for (i = 0; i < length; ++i)
  {
      px[i] = lon[i];
      py[i] = lat[i];
  }

  geographic_projection
      = pj_init_plus ("+proj=latlong +a=6378136.3 +b=6356751.600563");
  
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
	  
	  tmp3 = (double *) MALLOC(sizeof(double) * length);

	  if (height_was_set())
	  {
	      double height = get_avg_height();
	      for (i = 0; i < length; ++i)
		  tmp3[i] = height;
	  }
	  else
	  {
	      memset(tmp3, 0, sizeof(double) * length);
	  }

	  pj_transform (geographic_projection, output_projection, length, 1, 
			px, py, tmp3);
	  
	  if (pj_errno != 0)
	  {
	      asfPrintWarning("libproj Error: %s\n", pj_strerrno(pj_errno));
	      ok = FALSE;
	  }
	  
	  FREE(tmp3);
	  pj_free(output_projection);
      }
      
      pj_free(geographic_projection);
  }

  return ok;
}

static int project_worker_arr_inv(char * projection_description,
				  double *x, double *y,
				  double **lat, double **lon,
				  long length)
{
  double * tmp3;
  projPJ geographic_projection, output_projection;
  int i, ok = TRUE;

  if (!(*lat))
  {
      /* user should either pre-allocate both, or want us to allocate
	 both -- to preallocate one and not the other is weird and
	 probably a mistake */
      assert(!(*lat));

      *lat = (double *) MALLOC (sizeof(double) * length);
      *lon = (double *) MALLOC (sizeof(double) * length);
  }

  double *plat = *lat;
  double *plon = *lon;

  assert(plat);
  assert(plon);

  for (i = 0; i < length; ++i)
  {
      plat[i] = x[i];
      plon[i] = y[i];
  }

  geographic_projection
      = pj_init_plus ("+proj=latlong +a=6378136.3 +b=6356751.600563");
  
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
	  
	  tmp3 = (double *) MALLOC(sizeof(double) * length);

	  if (height_was_set())
	  {
	      double height = get_avg_height();
	      for (i = 0; i < length; ++i)
		  tmp3[i] = height;
	  }
	  else
	  {
	      memset(tmp3, 0, sizeof(double) * length);
	  }

	  pj_transform (output_projection, geographic_projection, length, 1, 
			plat, plon, tmp3);
	  	  
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

	  FREE(tmp3);
	  pj_free(output_projection);
      }

      pj_free(geographic_projection);
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
  sprintf(utm_wgs84_projection_description,
	  "+proj=utm +lon_0=%f +datum=%s",
	  utm_nudge(pps->utm.lon0 * RAD_TO_DEG), datum(pps));

  return utm_wgs84_projection_description;
}

int
project_utm (project_parameters_t * pps, 
	     double lat, double lon, double *x, double *y)
{
  return project_worker_arr(utm_projection_description(pps),
			    &lat, &lon, &x, &y, 1);
}

int
project_utm_arr (project_parameters_t * pps, 
		 double *lat, double *lon, 
		 double **projected_x, double **projected_y,
		 long length)
{
  return project_worker_arr(
      utm_projection_description(pps),
      lat, lon, projected_x, projected_y, length);
}

int
project_utm_inv (project_parameters_t * pps,
		 double x, double y,  double *lat, double *lon)
{
  return project_worker_arr_inv(
      utm_projection_description(pps),
      &x, &y, &lat, &lon, 1);
}

int
project_utm_arr_inv (project_parameters_t * pps,
		     double *x, double *y,
		     double **lat, double **lon,
		     long length)
{
  return project_worker_arr_inv(
      utm_projection_description(pps),
      x, y, lat, lon, length);
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
	   double lat, double lon,
	   double *x, double *y)
{
    return project_worker_arr(ps_projection_desc(pps), &lat, &lon, &x, &y, 1);
}

int
project_ps_arr(project_parameters_t * pps,
	       double *lat, double *lon,
	       double **projected_x, double **projected_y,
	       long length)
{
    return project_worker_arr(ps_projection_desc(pps), lat, lon,
			      projected_x, projected_y, length);
}

int
project_ps_inv(project_parameters_t * pps,
	       double x, double y, double *lat, double *lon)
{
    return project_worker_arr_inv(
	ps_projection_desc(pps), &x, &y, &lat, &lon, 1);
}

int
project_ps_arr_inv(project_parameters_t * pps,
		   double *x, double *y,
		   double **lat, double **lon,
		   long length)
{
    return project_worker_arr_inv(ps_projection_desc(pps), 
				  x, y, lat, lon, length);
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
	      double lat, double lon, double *x, double *y)
{
    return project_worker_arr(
	lamaz_projection_desc(pps), &lat, &lon, &x, &y, 1);
}

int
project_lamaz_arr(project_parameters_t * pps,
		  double * lat, double * lon,
		  double ** projected_x, double ** projected_y, 
		  long length)
{
    return project_worker_arr(
	lamaz_projection_desc(pps), lat, lon, projected_x, projected_y, 
	length);
}

int
project_lamaz_inv(project_parameters_t * pps,
		  double x, double y, double *lat, double *lon)
{
    return project_worker_arr(
	lamaz_projection_desc(pps), &x, &y, &lat, &lon, 1);
}

int
project_lamaz_arr_inv(project_parameters_t * pps,
		      double * x, double * y,
		      double ** lat, double ** lon, 
		      long length)
{
    return project_worker_arr_inv(
	lamaz_projection_desc(pps), x, y, lat, lon, length);
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
	      double lat, double lon, double *x, double *y)
{
    return project_worker_arr(
	lamcc_projection_desc(pps), &lat, &lon, &x, &y, 1);
}

int
project_lamcc_arr(project_parameters_t * pps,
		  double * lat, double * lon,
		  double ** projected_x, double ** projected_y, 
		  long length)
{
    return project_worker_arr(
	lamcc_projection_desc(pps), lat, lon, projected_x, projected_y, 
	length);
}

int
project_lamcc_inv(project_parameters_t * pps,
		  double x, double y, double *lat, double *lon)
{
    return project_worker_arr(
	lamcc_projection_desc(pps), &x, &y, &lat, &lon, 1);
}

int
project_lamcc_arr_inv(project_parameters_t * pps,
		      double * x, double * y,
		      double ** lat, double ** lon, 
		      long length)
{
    return project_worker_arr_inv(
	lamcc_projection_desc(pps), x, y, lat, lon, length);
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
project_albers(project_parameters_t * pps,
	       double lat, double lon, double *x, double *y)
{
    return project_worker_arr(
	albers_projection_desc(pps), &lat, &lon, &x, &y, 1);
}

int
project_albers_arr(project_parameters_t * pps,
		  double * lat, double * lon,
		  double ** projected_x, double ** projected_y, 
		  long length)
{
    return project_worker_arr(
	albers_projection_desc(pps), lat, lon, projected_x, projected_y, 
	length);
}

int
project_albers_inv(project_parameters_t * pps,
		  double x, double y, double *lat, double *lon)
{
    return project_worker_arr(
	albers_projection_desc(pps), &x, &y, &lat, &lon, 1);
}

int
project_albers_arr_inv(project_parameters_t * pps,
		      double * x, double * y,
		      double ** lat, double ** lon, 
		      long length)
{
    return project_worker_arr_inv(
	albers_projection_desc(pps), x, y, lat, lon, length);
}
