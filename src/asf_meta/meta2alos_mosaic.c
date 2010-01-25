#include "asf_meta.h"
#include "dateUtil.h"
#include "alos_mosaic.h"
#include "asf_nan.h"

meta_parameters* alos_mosaic2meta(alos_mosaic_header *alos)
{
  meta_parameters *meta;
  double lat, lon, height, startX, startY, z, centerX, centerY;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  strcpy(meta->general->basename, alos->image_file_id);
  strcpy(meta->general->sensor, alos->mission);
  strcpy(meta->general->sensor_name, alos->sensor);
  strncpy(meta->general->mode, alos->path_mode, 3);
  strcpy(meta->general->processor, alos->processor_name);
  meta->general->data_type = REAL32;
  meta->general->image_data_type = MOSAIC;
  meta->general->radiometry = r_AMP;
  strcpy(meta->general->acquisition_date, alos->acquisition);
  if (strncmp_case(alos->orbit_direction, "AS", 2) == 0)
    meta->general->orbit_direction = 'A';
  else if (strncmp_case(alos->orbit_direction, "DE", 2) == 0)
    meta->general->orbit_direction = 'D';
  meta->general->band_count = 1;
  strcpy(meta->general->bands, "MOSAIC");
  meta->general->line_count = alos->line_count;
  meta->general->sample_count = alos->sample_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = alos->pixel_spacing;
  meta->general->y_pixel_size = alos->pixel_spacing;
  meta->general->re_major = alos->semimajor*1000;
  meta->general->re_minor = alos->semiminor*1000;

  // Projection block
  if (strcmp_case(alos->map_projection, "MER") == 0 ||
      strcmp_case(alos->map_projection, "EQR") == 0 ||
      strcmp_case(alos->map_projection, "UTM") == 0 ||
      strcmp_case(alos->map_projection, "PS") == 0 ||
      strcmp_case(alos->map_projection, "LCC") == 0) {
    meta->projection = meta_projection_init();
    // Need to derive the startX and startY from lat/lon corners. That 
    // requires the rest of the parameters to be known first.
    meta->projection->perX = alos->range_spacing;
    meta->projection->perY = -alos->azimuth_spacing;
    strcpy(meta->projection->units, "meters");
    if (strcmp_case(alos->hemisphere, "NORTH") == 0)
      meta->projection->hem = 'N';
    else if (strcmp_case(alos->hemisphere, "SOUTH") == 0)
      meta->projection->hem = 'S';
    else {
      if (alos->corner1_lat >= 0.0)
	meta->projection->hem = 'N';
      else
	meta->projection->hem = 'S';
    }
    meta->projection->re_major = alos->semimajor*1000;
    meta->projection->re_minor = alos->semiminor*1000;
    meta->projection->height = 0.0;
    // Not bothering about any other datums and spheroids
    // Always the same - presumably
    if (strcmp_case(alos->datum, "ITRF97") == 0)
      meta->projection->datum = ITRF97_DATUM;
    if (strcmp_case(alos->ellipsoid, "GRS80") == 0)
      meta->projection->spheroid = GRS1980_SPHEROID;
    // Universal Transverse Mercator
    if (strcmp_case(alos->map_projection, "UTM") == 0) {
      meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      meta->projection->param.utm.zone = alos->utm_zone;
      meta->projection->param.utm.false_easting = 0.0;
      meta->projection->param.utm.false_northing = 0.0;
      meta->projection->param.utm.lat0 = alos->orig_lat;
      meta->projection->param.utm.lon0 = alos->orig_lon;
      meta->projection->param.utm.scale_factor = 0.99996;
    }
    // Polar Stereographic
    else if (strcmp_case(alos->map_projection, "PS") == 0) {
      meta->projection->type = POLAR_STEREOGRAPHIC;
      meta->projection->param.ps.slat = alos->ps_ref_lat;
      meta->projection->param.ps.slon = alos->ps_ref_lon;
      if (meta->projection->param.ps.slat > 0.0)
	meta->projection->param.ps.is_north_pole = TRUE;
      else
	meta->projection->param.ps.is_north_pole = FALSE;
      meta->projection->param.ps.false_northing = 0.0;
      meta->projection->param.ps.false_easting = 0.0;
    }
    // Lambert Conformal Conic
    else if (strcmp_case(alos->map_projection, "LCC") == 0) {
      meta->projection->type = LAMBERT_CONFORMAL_CONIC;
      meta->projection->param.lamcc.plat1 = alos->lcc_ref_lat1;
      meta->projection->param.lamcc.plat2 = alos->lcc_ref_lat2;
      meta->projection->param.lamcc.lat0 = 0.0;
      meta->projection->param.lamcc.lon0 = 0.0;
      meta->projection->param.lamcc.false_easting = 0.0;
      meta->projection->param.lamcc.false_northing = 0.0;
      meta->projection->param.lamcc.scale_factor = 1.0;
    }
    else if (strcmp_case(alos->map_projection, "MER") == 0) {
      meta->projection->type = MERCATOR;
      meta->projection->param.mer.standard_parallel = alos->lcc_ref_lat1;
      meta->projection->param.mer.central_meridian = 0.0;
      meta->projection->param.mer.orig_latitude = 0.0;
      meta->projection->param.mer.false_easting = 0.0;
      meta->projection->param.mer.false_northing = 0.0;
    }
    else if (strcmp_case(alos->map_projection, "EQR") == 0) {
      meta->projection->type = EQUI_RECTANGULAR;
      meta->projection->param.eqr.central_meridian = 0.0;
      meta->projection->param.eqr.orig_latitude = 0.0;
      meta->projection->param.eqr.false_easting = 0.0;
      meta->projection->param.eqr.false_northing = 0.0;
    }
    // Now we have everything to calculate startX and startY
    lat = alos->corner1_lat * D2R;
    lon = alos->corner1_lon * D2R;
    latlon_to_proj(meta->projection, 'R', lat, lon, 0.0, &startX, &startY, &z);
    meta->projection->startX = startX;
    meta->projection->startY = startY;

    // Determine center lat/lon from projection block
    centerX = startX + meta->general->sample_count/2 * meta->projection->perX;
    centerY = startY + meta->general->line_count/2 * meta->projection->perY;
    proj_to_latlon(meta->projection, centerX, centerY, 0.0, 
		   &lat, &lon, &height);
    meta->general->center_latitude = lat * R2D; 
    meta->general->center_longitude = lon * R2D;
  }

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = alos->corner1_lat;
  meta->location->lon_start_near_range = alos->corner1_lon;
  meta->location->lat_start_far_range = alos->corner2_lat;
  meta->location->lon_start_far_range = alos->corner2_lon;
  meta->location->lat_end_near_range = alos->corner4_lat;
  meta->location->lon_end_near_range = alos->corner4_lon;
  meta->location->lat_end_far_range = alos->corner3_lat;
  meta->location->lon_end_far_range = alos->corner3_lon;

  // Calibration block
  meta->calibration = meta_calibration_init();
  meta->calibration->type = alos_cal;
  meta->calibration->alos = 
    (alos_cal_params *) MALLOC(sizeof(alos_cal_params));
  meta->calibration->alos->cf_hh = alos->cal_factor;
    
  return meta;
}
