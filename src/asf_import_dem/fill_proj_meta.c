
#include <asf.h>
#include <libasf_proj.h>
#include "asf_import_dem.h"


// Taken from geocode_options.c in asf_geocode
static const double DEFAULT_POLAR_STERO_NORTH_CENTRAL_MERIDIAN = -45;
static const double DEFAULT_POLAR_STERO_SOUTH_CENTRAL_MERIDIAN = -90;
static const double DEFAULT_POLAR_STERO_NORTH_STANDARD_PARALLEL = 70;
static const double DEFAULT_POLAR_STERO_SOUTH_STANDARD_PARALLEL = -70;


// in geocode_options.c in asf_geocode
int calc_utm_zone(double lon);


void fill_proj_meta(projection_type_t pt, project_parameters_t *outProjPrms,
                    seamless_meta_t *smeta, PROJECT_T project,
                    double *average_height, double *pixel_size)
{
  asfRequire(strcmp(seamless_meta_get_units(smeta),"DD")==0,
             "%s is only programmed to deal with DEMs in decimal degrees"
             ", exiting.\n",
             __func__);

  int ncols = seamless_meta_get_ncols(smeta);
  int nrows = seamless_meta_get_nrows(smeta);
  double yllcorner = seamless_meta_get_yllcorner(smeta);
  double xllcorner = seamless_meta_get_xllcorner(smeta);
  double cellsize = seamless_meta_get_cellsize(smeta);

  // Just calculate this once right here
  double center_lat = yllcorner + (nrows*cellsize / 2);
  double center_lon = xllcorner + (ncols*cellsize / 2);

  switch (pt)
  {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    if (ISNAN(outProjPrms->utm.lon0))
      outProjPrms->utm.lon0 = center_lon;
    if (ISNAN(outProjPrms->utm.lat0))
      outProjPrms->utm.lat0 = center_lat;
    if (outProjPrms->utm.zone == MAGIC_UNSET_INT) {
      if ( outProjPrms->utm.lon0 / 6.0 - floor (outProjPrms->utm.lon0 / 6.0)
           == 0.0 ) {
        asfPrintError ("Longitude %.6f lies on a UTM zone boundry, "
                       "(i.e. is ambiguous as to which UTM zone "
                       "should be used for geocoding)\n",
                       outProjPrms->utm.lon0);
      }
      outProjPrms->utm.zone = calc_utm_zone(outProjPrms->utm.lon0);
    }
    outProjPrms->utm.false_northing = outProjPrms->utm.lat0 >= 0?0:10000000;
    outProjPrms->utm.false_easting = 500000;
    outProjPrms->utm.scale_factor = 0.9996;
    break;

  case POLAR_STEREOGRAPHIC:
    if (ISNAN(outProjPrms->ps.slon))
      outProjPrms->ps.slon = outProjPrms->ps.is_north_pole ?
        DEFAULT_POLAR_STERO_NORTH_CENTRAL_MERIDIAN :
        DEFAULT_POLAR_STERO_SOUTH_CENTRAL_MERIDIAN;
    if (ISNAN(outProjPrms->ps.slat))
      outProjPrms->ps.slat = outProjPrms->ps.is_north_pole ?
        DEFAULT_POLAR_STERO_NORTH_STANDARD_PARALLEL :
        DEFAULT_POLAR_STERO_SOUTH_STANDARD_PARALLEL;
    if (ISNAN(outProjPrms->ps.false_easting))
      outProjPrms->ps.false_easting = 0;
    if (ISNAN(outProjPrms->ps.false_northing))
       outProjPrms->ps.false_northing = 0;
    break;

  case ALBERS_EQUAL_AREA:
      if (ISNAN(outProjPrms->albers.false_easting))
        outProjPrms->albers.false_easting = 0;
      if (ISNAN(outProjPrms->albers.false_northing))
        outProjPrms->albers.false_northing = 0;
      if (ISNAN(outProjPrms->albers.orig_latitude))
        outProjPrms->albers.orig_latitude = center_lat;
      if (ISNAN(outProjPrms->albers.center_meridian))
        outProjPrms->albers.center_meridian = center_lon;
      break;

  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    if (ISNAN(outProjPrms->lamaz.false_easting))
      outProjPrms->lamaz.false_easting = 0;
    if (ISNAN(outProjPrms->lamaz.false_northing))
      outProjPrms->lamaz.false_northing = 0;
    if (ISNAN(outProjPrms->lamaz.center_lat))
      outProjPrms->lamaz.center_lat = center_lat;
    if (ISNAN(outProjPrms->lamaz.center_lon))
      outProjPrms->lamaz.center_lon = center_lon;
    break;

  case LAMBERT_CONFORMAL_CONIC:
    if (ISNAN(outProjPrms->lamcc.false_easting))
      outProjPrms->lamcc.false_easting = 0;
    if (ISNAN(outProjPrms->lamcc.false_northing))
      outProjPrms->lamcc.false_northing = 0;
    if (ISNAN(outProjPrms->lamcc.lat0))
      outProjPrms->lamcc.lat0 = center_lat;
    if (ISNAN(outProjPrms->lamcc.lon0))
      outProjPrms->lamcc.lon0 = center_lon;
      break;

  default:
      asfPrintError("%s: illegal projection type!", __func__);
  }


  if (ISNAN(*average_height))
    *average_height = 0.0;

  if (ISNAN(*pixel_size)) {
    double upper_lat = (yllcorner + cellsize * nrows) * DEG_TO_RAD;
    double lower_lat = yllcorner * DEG_TO_RAD;
    double left_lon = xllcorner * DEG_TO_RAD;
    double right_lon = (xllcorner + cellsize * ncols) * DEG_TO_RAD;
    double left_x, right_x, upper_y, lower_y;
    project (outProjPrms, upper_lat, right_lon, &right_x, &upper_y);
    project (outProjPrms, lower_lat, left_lon, &left_x, &lower_y);
    double x_pixel_size = (right_x - left_x) / (double)ncols;
    double y_pixel_size = (upper_y - lower_y) / (double)nrows;
    asfRequire ( (x_pixel_size-y_pixel_size)<0.0001,
                 "Pixel size not square, but it needs to be!\n");
    *pixel_size = x_pixel_size;
  }


}
