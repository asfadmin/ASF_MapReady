#include "asf_meta.h"

meta_parameters *gamma_dem2meta(char *demFile, char *demPar)
{
  FILE *fp = FOPEN(demPar, "rt");
  meta_parameters *meta = raw_init();
  meta->general->data_type = REAL32; // INTEGER16 ?
  meta->general->image_data_type = DEM;
  meta->general->radiometry = r_AMP;
  meta->general->band_count = 1;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->no_data = -1;
  meta->projection = meta_projection_init();
  strcpy(meta->projection->units, "m");


  char line[255];
  while (fgets(line, 255, fp) != NULL) {
    if (strstr(line, "DEM_projection") && strstr(line, "UTM"))
      meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
    else if (strstr(line, "DEM_projection") && strstr(line, "PS"))
    	meta->projection->type = POLAR_STEREOGRAPHIC;
    if (strstr(line, "width:"))
      sscanf(line, "width: %d", &meta->general->sample_count);
    if (strstr(line, "nlines:"))
      sscanf(line, "nlines: %d", &meta->general->line_count);
    if (strstr(line, "corner_north:"))
      sscanf(line, "corner_north: %lf m", &meta->projection->startY);
    if (strstr(line, "corner_east:"))
      sscanf(line, "corner_east: %lf m", &meta->projection->startX);
    if (strstr(line, "post_north:")) {
      sscanf(line, "post_north: %lf m", &meta->projection->perY);
      meta->general->x_pixel_size = fabs(meta->projection->perY);
    }
    if (strstr(line, "post_east:")) {
      sscanf(line, "post_east: %lf m", &meta->projection->perX);
      meta->general->y_pixel_size = meta->projection->perX;
    }
    if (strstr(line, "ellipsoid_name: WGS 84"))
      meta->projection->spheroid = WGS84_SPHEROID;
    if (strstr(line, "datum_name: WGS 1984"))
      meta->projection->datum = WGS84_DATUM;
    if (strstr(line, "projection_zone:"))
      sscanf(line, "projection_zone: %d", &meta->projection->param.utm.zone);
    if (strstr(line, "false_easting:"))
      sscanf(line, "false_easting: %lf",
        &meta->projection->param.utm.false_easting);
    if (strstr(line, "false_northing:"))
      sscanf(line, "false_northing: %lf",
        &meta->projection->param.utm.false_northing);
    if (strstr(line, "projection_k0:"))
      sscanf(line, "projection_k0: %lf",
        &meta->projection->param.utm.scale_factor);
    if (strstr(line, "center_longitude:"))
      sscanf(line, "center_longitude: %lf", &meta->projection->param.utm.lon0);
    if (strstr(line, "center_latitude:"))
      sscanf(line, "center_latitude: %lf", &meta->projection->param.utm.lat0);
		if (strstr(line, "PS_secant_lat:")) {
			sscanf(line, "PS_secant_lat: %lf decimal degrees",
				&meta->projection->param.ps.slat);
			if (meta->projection->param.ps.slat > 0)
				meta->projection->param.ps.is_north_pole = 1;
			else
				meta->projection->param.ps.is_north_pole = 0;
		}
		if (strstr(line, "PS_central_meridian:"))
			sscanf(line, "PS_central_meridian: %lf decimal degrees",
				&meta->projection->param.ps.slon);
  }
  spheroid_axes_lengths(meta->projection->spheroid,
    &meta->projection->re_major, &meta->projection->re_minor);
  double lat, lon;
  meta_get_latLon(meta, meta->general->line_count/2,
    meta->general->sample_count/2, 0.0, &lat, &lon);
  if (lat > 0.0)
    meta->projection->hem = 'N';
  else
    meta->projection->hem = 'S';

  return meta;
}