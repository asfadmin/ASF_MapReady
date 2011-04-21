#include "asf_meta.h"

// NOTE: Because of the degrees-to-radians conversions, this function is ONLY 
// for meta_getlatlon()...
void copy_proj_parms(meta_projection *dest, meta_projection *src)
{
  dest->type = src->type;
  dest->startX = src->startX;
  dest->startY = src->startY;
  dest->perX = src->perX;
  dest->perY = src->perY;
  strcpy (dest->units, src->units);
  dest->hem = src->hem;
  dest->spheroid = src->spheroid;
  dest->re_major = src->re_major;
  dest->re_minor = src->re_minor;
  dest->datum = src->datum;
  dest->height = src->height;

  switch (src->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      dest->param.utm.zone = src->param.utm.zone;
      dest->param.utm.lat0 = src->param.utm.lat0;
      dest->param.utm.lon0 = src->param.utm.lon0;
      dest->param.utm.false_easting = src->param.utm.false_easting;
      dest->param.utm.false_northing = src->param.utm.false_northing;
      dest->param.utm.scale_factor = src->param.utm.scale_factor;
      break;
    case POLAR_STEREOGRAPHIC:
      dest->param.ps.slat = src->param.ps.slat;
      dest->param.ps.slon = src->param.ps.slon;
      dest->param.ps.is_north_pole = src->param.ps.is_north_pole;
      dest->param.ps.false_easting = src->param.ps.false_easting;
      dest->param.ps.false_northing = src->param.ps.false_northing;
      break;
    case ALBERS_EQUAL_AREA:
      dest->param.albers.std_parallel1 = src->param.albers.std_parallel1;
      dest->param.albers.std_parallel2 = src->param.albers.std_parallel2;
      dest->param.albers.center_meridian = src->param.albers.center_meridian;
      dest->param.albers.orig_latitude = src->param.albers.orig_latitude;
      dest->param.albers.false_easting = src->param.albers.false_easting;
      dest->param.albers.false_northing = src->param.albers.false_northing;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      dest->param.lamcc.plat1 = src->param.lamcc.plat1;
      dest->param.lamcc.plat2 = src->param.lamcc.plat2;
      dest->param.lamcc.lat0 = src->param.lamcc.lat0;
      dest->param.lamcc.lon0 = src->param.lamcc.lon0;
      dest->param.lamcc.false_easting = src->param.lamcc.false_easting;
      dest->param.lamcc.false_northing = src->param.lamcc.false_northing;
      dest->param.lamcc.scale_factor = src->param.lamcc.scale_factor;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      dest->param.lamaz.center_lon = src->param.lamaz.center_lon;
      dest->param.lamaz.center_lat = src->param.lamaz.center_lat;
      dest->param.lamaz.false_easting = src->param.lamaz.false_easting;
      dest->param.lamaz.false_northing = src->param.lamaz.false_northing;
      break;
    case EQUI_RECTANGULAR:
      dest->param.eqr.orig_latitude = src->param.eqr.orig_latitude;
      dest->param.eqr.central_meridian = src->param.eqr.central_meridian;
      dest->param.eqr.false_easting = src->param.eqr.false_easting;
      dest->param.eqr.false_northing = src->param.eqr.false_northing;
      break;
    case EQUIDISTANT:
      dest->param.eqc.orig_latitude = src->param.eqc.orig_latitude;
      dest->param.eqc.central_meridian = src->param.eqc.central_meridian;
      break;
    case MERCATOR:
      dest->param.mer.orig_latitude = src->param.mer.orig_latitude;
      dest->param.mer.central_meridian = src->param.mer.central_meridian;
      dest->param.mer.standard_parallel = src->param.mer.standard_parallel;
      dest->param.mer.scale_factor = src->param.mer.scale_factor;
      dest->param.mer.false_easting = src->param.mer.false_easting;
      dest->param.mer.false_northing = src->param.mer.false_northing;
      break;
    case SINUSOIDAL:
      dest->param.sin.longitude_center = src->param.sin.longitude_center;
      dest->param.sin.false_easting = src->param.sin.false_easting;
      dest->param.sin.false_northing = src->param.sin.false_northing;
      dest->param.sin.sphere = src->param.sin.sphere;
      break;
    case STATE_PLANE:
    case LAT_LONG_PSEUDO_PROJECTION:
      break;
    default:
      asfPrintError("Unsupported projection type found.\n");
      break;
  }
}

