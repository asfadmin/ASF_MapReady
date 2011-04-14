#include "libasf_proj.h"
#include "asf_meta.h"
#include "projects.h"
#include "proj_api.h"

#define DELTA 1e-05
#define EPSILON 1e-12

static const char *spheroid_str(spheroid_type_t spheroid)
{
  if (spheroid ==  BESSEL_SPHEROID)
    return "bessel";
  else if (spheroid == CLARKE1866_SPHEROID)
    return "clrk66";
  else if (spheroid == CLARKE1880_SPHEROID)
    return "clrk80";
  else if (spheroid == GRS1967_SPHEROID)
    return "GRS67";
  else if (spheroid == GRS1980_SPHEROID)
    return "GRS80";
  else if (spheroid == INTERNATIONAL1924_SPHEROID)
    return "intl";
  else if (spheroid == INTERNATIONAL1967_SPHEROID)
    return "new_intl";
  else if (spheroid == WGS66_SPHEROID)
    return "WGS66";
  else if (spheroid == WGS72_SPHEROID)
    return "WGS72";
  else if (spheroid == WGS84_SPHEROID)
    return "WGS84";
  else
    return "unknown";
}

void map_distortions(meta_projection *proj, double lat, double lon, 
		     distortion_t *distortion)
{
  double dx_dlat, dy_dlat, dx_dlon, dy_dlon;
  char proj_description[255], spheroid_desc[25];

  // Put projection description together
  if (proj->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
    sprintf(proj_description, "%s",
    	    utm_projection_description(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == POLAR_STEREOGRAPHIC) {
    sprintf(proj_description, "%s",
    	    ps_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == ALBERS_EQUAL_AREA) {
    sprintf(proj_description, "%s",
	    albers_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == LAMBERT_CONFORMAL_CONIC) {
    sprintf(proj_description, "%s",
    	    lamcc_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
    sprintf(proj_description, "%s",
    	    lamaz_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == MERCATOR) {
    sprintf(proj_description, "%s",
    	    mer_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == EQUI_RECTANGULAR) {
    sprintf(proj_description, "%s",
    	    eqr_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }
  else if (proj->type == EQUIDISTANT) {
    sprintf(proj_description, "%s",
    	    eqc_projection_desc(&(proj->param), proj->datum));
    if (strcmp_case(spheroid_str(proj->spheroid), "unknown") != 0) {
      sprintf(spheroid_desc, " +ellsp=%s", spheroid_str(proj->spheroid));
      strcat(proj_description, spheroid_desc);
    }
  }

  // Initialize transformation
  projLP lp;
  projPJ pj = pj_init_plus(proj_description);
  pj->a = 1.0;
  pj->x0 = pj->y0 = 0.0;
  double es = pj->es;
  double one_es = pj->one_es;

  // Check lat/lon input
  double t = fabs(lat) - PI/2;
  if (fabs(t) <= EPSILON)
    lat = lat < 0.0 ? -PI/2 : PI/2;

  // Calculate the derivatives
  lp.v = lat+DELTA;
  lp.u = lon+DELTA;
  projXY xy = pj_fwd(lp, pj);
  dx_dlon = xy.u;
  dy_dlon = -xy.v;
  dx_dlat = -xy.u;
  dy_dlat = xy.v;
  lp.v = lat-DELTA;
  lp.u = lon+DELTA;
  xy = pj_fwd(lp, pj);
  dx_dlon += xy.u;
  dy_dlon -= xy.v;
  dx_dlat += xy.u;
  dy_dlat -= xy.v;
  lp.v = lat-DELTA;
  lp.u = lon-DELTA;
  xy = pj_fwd(lp, pj);
  dx_dlon -= xy.u;
  dy_dlon += xy.v;
  dx_dlat += xy.u;
  dy_dlat -= xy.v;
  lp.v = lat+DELTA;
  lp.u = lon-DELTA;
  xy = pj_fwd(lp, pj);
  dx_dlon -= xy.u;
  dy_dlon += xy.v;
  dx_dlat -= xy.u;
  dy_dlat += xy.v;
  dx_dlon /= 4*DELTA;
  dy_dlon /= 4*DELTA;
  dx_dlat /= 4*DELTA;
  dy_dlat /= 4*DELTA;

  // Meridian scale factor
  double h = hypot(dx_dlat, dy_dlat);
  t = sin(lat);
  t = 1.0 - es*t*t;
  double n = sqrt(t);
  h *= t*n/one_es;

  // Parallel scale factor
  double k = hypot(dx_dlon, dy_dlon) / cos(lat) * n;

  // Areal scale factor
  double r = sin(lat);
  r = 1.0 - es*r*r;
  r = r*r/one_es;
  double s = (dy_dlat*dx_dlon - dx_dlat*dy_dlon) * r / cos(lat);

  // Meridian/Parallel angle
  double theta = aasin(s/(h*k));

  // Tissot indicatrix (semimajor and semiminor axes)
  /*
  double a_ = sqrt(h*h + k*k + 2*h*k*sin(theta));
  double b_ = sqrt(h*h + k*k - 2*h*k*sin(theta));
  double a = (a_ + b_)/2.0;
  double b = (a_ - b_)/2.0;
  */
  t = k*k + h*h;
  double a = sqrt(t + 2.0*s);
  t = (t = t - 2.0*s) <= 0.0 ? 0.0 : sqrt(t);
  double b = 0.5*(a - t);
  a = 0.5*(a + t);

  // Angular distortion
  double omega = 2.0 * asin((a-b)/(a+b))*R2D;

  // Fill in structure
  distortion->h = h;
  distortion->k = k;
  distortion->s = s;
  distortion->omega = omega;
  distortion->a = a;
  distortion->b = b;
}
