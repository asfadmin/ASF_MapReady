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

int
project_utm (double lon_0, double lat, double lon, double *x, double *y)
{
  double tmp1 = lon, tmp2 = lat, tmp3 = 0, tiny_value;
  char * utm_wgs84_projection_description;
  projPJ geographic_projection, utm_wgs84_projection;

  *x = HUGE_VAL;
  *y = HUGE_VAL;

  /* Establish description of input pseudo-projection. */
  geographic_projection
      = pj_init_plus ("+proj=latlong +a=6378136.3 +b=6356751.600563");
      // = pj_init_plus ("+proj=latlong +ellps=WGS84");
      // = pj_init_plus ("+proj=latlong");
  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      return FALSE;
  }

  assert (geographic_projection != NULL);

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
  utm_wgs84_projection_description =
      (char *) malloc( sizeof(char) * 64 );

  sprintf(utm_wgs84_projection_description,
	  "+proj=utm +lon_0=%f +datum=WGS84",
//	  "+proj=utm +lon_0=%f",
	  lon_0);

  utm_wgs84_projection
    = pj_init_plus (utm_wgs84_projection_description);
  
  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      return FALSE;
  }

  assert (utm_wgs84_projection != NULL);

  pj_transform (geographic_projection, utm_wgs84_projection, 1, 1, &tmp1,
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

  free(utm_wgs84_projection_description);
  pj_free(geographic_projection);
  pj_free(utm_wgs84_projection);

  return TRUE;    
}

int
project_ps (double lat_0, double lon_0, double lat, double lon, 
	    double *x, double *y)
{
  double tmp1 = lon, tmp2 = lat, tmp3 = 0;
  char * ps_wgs84_projection_description;
  projPJ geographic_projection, ps_wgs84_projection;

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

  /* Establish description of output projection. */
  ps_wgs84_projection_description =
      (char *) malloc( sizeof(char) * 128 );

  sprintf(ps_wgs84_projection_description,
	  "+proj=stere +lat_0=90 +lat_ts=%f +lon_0=%f +datum=WGS84",
	  lat_0, lon_0);

  ps_wgs84_projection
    = pj_init_plus (ps_wgs84_projection_description);
  
  if (pj_errno != 0)
  {
      asfPrintError("libproj Error: %s\n", pj_strerrno(pj_errno));
      return FALSE;
  }

  assert (ps_wgs84_projection != NULL);

  pj_transform (geographic_projection, ps_wgs84_projection, 1, 1, &tmp1,
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

  free(ps_wgs84_projection_description);
  pj_free(geographic_projection);
  pj_free(ps_wgs84_projection);

  return TRUE;    
}

