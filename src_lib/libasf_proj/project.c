#include "project.h"
#include "asf.h"
#include "asf_reporting.h"

#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "projects.h"
#include "proj_api.h"

static double round(double d)
{
    return floor(d + 0.5);
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

static int project_worker(char * projection_description,
			  double lat, double lon, 
			  double *x, double *y)
{
  double tmp1 = lon, tmp2 = lat, tmp3 = 0;
  projPJ geographic_projection, output_projection;

  *x = HUGE_VAL;
  *y = HUGE_VAL;

  /* Establish description of input pseudo-projection. */
  geographic_projection
      = pj_init_plus ("+proj=latlong +a=6378136.3 +b=6356751.600563");

  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      return FALSE;
  }

  assert (geographic_projection != NULL);

  output_projection = pj_init_plus (projection_description);
  
  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      return FALSE;
  }

  assert (output_projection != NULL);

  pj_transform (geographic_projection, output_projection, 1, 1, &tmp1,
                &tmp2, &tmp3);

  if (pj_errno != 0)
  {
      asfPrintWarning("libproj Error: %s\n", pj_strerrno(pj_errno));
      return FALSE;
  }

  *x = tmp1;
  *y = tmp2;
  
  /* Height difference from small datum change is expected to be minimal.  
   */
  assert (tmp3 < 5.0);

  pj_free(geographic_projection);
  pj_free(output_projection);

  return TRUE;
}

/*
  lon_0: Logitude of natural origin
*/

int
project_utm (double lon_0, double lat, double lon, double *x, double *y)
{
  char utm_wgs84_projection_description[128];
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

  /* Establish description of output projection. */
  sprintf(utm_wgs84_projection_description,
	  "+proj=utm +lon_0=%f +datum=WGS84",
	  lon_0);

  return project_worker(utm_wgs84_projection_description, lat, lon, x, y);
}

/*
  lat_ts : Latitude at natural origin
  lon_0 : Longitude at natural origin
  is_north_pole: 1 if projecting from North Pole, 0 if South Pole
*/

int
project_ps (double lat_ts, double lon_0, int is_north_pole,
	    double lat, double lon,
	    double *x, double *y)
{
  char ps_wgs84_projection_description[128];

  /* Establish description of output projection. */
  sprintf(ps_wgs84_projection_description,
	  "+proj=stere +lat_0=%s +lat_ts=%f +lon_0=%f +datum=WGS84",
	  is_north_pole ? "90" : "-90", lat_ts, lon_0);

  return  project_worker(ps_wgs84_projection_description, lat, lon, x, y);
}

int
project_ps_s (proj_ps * ps,
	      int is_north_pole,
	      double lat, double lon,
	      double *x, double *y)
{
    return project_ps(ps->slat, ps->slon, is_north_pole, lat, lon, x, y);
}

/*
  lat_0 : Latitude at projection center
  lon_0 : Longitude at projection center
*/

int
project_lamaz(double lat_0, double lon_0, double lat, double lon, 
	      double *x, double *y)
{
  char lamaz_projection_description[128];

  /* Establish description of output projection. */
  sprintf(lamaz_projection_description,
	  "+proj=laea +lat_0=%f +lon_0=%f +datum=WGS84",
	  lat_0, lon_0);

  return project_worker(lamaz_projection_description, lat, lon, x, y);
}

int
project_lamaz_s(proj_lamaz * lamaz,
		double lat, double lon, 
		double *x, double *y)
{
    return project_lamaz(lamaz->center_lat, lamaz->center_lon,
			 lat, lon, x, y);
}

/*
  lat_1 : First standard parallel
  lat_2 : Second standard parallel
  lat_0 : Latitude of false origin
  lon_0 : Longitude of false origin
*/

int
project_lamcc(double lat_1, double lat_2, double lat_0, double lon_0, 
	      double lat, double lon, double *x, double *y)
{
  char lcc_wgs84_projection_description[128];

  /* Establish description of output projection. */
  sprintf(lcc_wgs84_projection_description,
	  "+proj=lcc +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +datum=WGS84",
	  lat_1, lat_2, lat_0, lon_0);

  return project_worker(lcc_wgs84_projection_description, lat, lon, x, y);
}

int
project_lamcc_s(proj_lamcc * lamcc,
		double lat, double lon, double *x, double *y)
{
    return project_lamcc(lamcc->plat1, lamcc->plat2, lamcc->lat0, lamcc->lon0,
			 lat, lon, x, y);
}

/*
  lat_1 : Latitude of first standard parallel
  lat_2 : Latitude of second standard parallel
  lat_0 : Latitude at false origin
  lon_0 : Longitude at false origin
 */

int
project_albers(double lat_1, double lat_2, double lat_0, double lon_0,
	       double lat, double lon, double *x, double *y)
{
  char aea_projection_description[128];

  /* Establish description of output projection. */
  sprintf(aea_projection_description,
	  "+proj=aea +lat_1=%f +lat_2=%f +lat_0=%f +lon_0=%f +datum=WGS84",
	  lat_1, lat_2, lat_0, lon_0);

  return project_worker(aea_projection_description, lat, lon, x, y);
}

int
project_albers_s(proj_albers * alb,
		 double lat, double lon, double *x, double *y)
{
    return project_albers(
	  alb->std_parallel1,
	  alb->std_parallel2,
	  alb->orig_latitude,
	  alb->center_meridian,
	  lat, lon, x, y);
}
