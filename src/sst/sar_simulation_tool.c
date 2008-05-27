#include "asf_simulation.h"

#define VERSION 1.0

#ifndef SQR
# define SQR(x) ((x)*(x))
#endif

static void
get_target_position(stateVector *st, double look, double yaw,
                    double sr, vector *targPos)
{
  vector relPos = vecNew(sin(yaw), -sin(look)*cos(yaw), -cos(look)*cos(yaw));

  // relPos unit vector points from s/c to targPos.
  // Rotate into earth axes
  vector look_matrix[3];
  look_matrix[2] = st->pos;
  vecNormalize(&look_matrix[2]);
  vector v = st->vel;
  vecNormalize(&v);
  vecCross(st->pos,v,&look_matrix[1]);
  vecCross(look_matrix[2],v,&look_matrix[1]);
  vecCross(look_matrix[1],look_matrix[2],&look_matrix[0]);
  vecMul(look_matrix,relPos,&relPos);

  // scale relPos so it reaches from s/c to targPos
  vecScale(&relPos,sr);
  vecAdd(relPos,st->pos,targPos);
}

static int
get_target_latlon(stateVector *st, double look, double *tlat, double *tlon)
{

  // satellite height, from the state vector
  double ht = vecMagnitude(st->pos);

  // earth radius, calculate from WGS84 values, and approx latitude
  double re = 6378137.0;
  double rp = 6356752.31414;
  double lat = asin(st->pos.z/ht);
  double er = re*rp/sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));

  // calculate slant range by law of cosines
  // (where we know two sides, but not the angle between them)
  double D = er*er - ht*ht*sin(look)*sin(look);
  if (D < 0)
    return 0; // can't see the Earth from here
  double sr1 = ht*cos(look) + sqrt(D);
  double sr2 = ht*cos(look) - sqrt(D);
  double sr = sr1>sr2 ? sr2 : sr1;

  // target position, first in inertial coords, then convert to lat/lon
  vector target;
  get_target_position(st, look, 0, sr, &target);

  double glat; // geocentric
  cart2sph(target,&er,&glat,tlon);

  // tlon should be -180,180
  // tlat needs to be geodetic
  // both should be degrees
  *tlon *= R2D;
  if (*tlon < -180.) *tlon += 360.;

  *tlat = R2D*atan(tan(glat)*re*re/(rp*rp));

  return 1;
}

static void calc_geometry(stateVector st, double look_angle, int size, 
			  double pixel_size,double *sat_height,
			  double *earth_rad, double *sr_first)
{
  double re = 6378137.000; //[m] - semimajor axis WGS84
  double rp = 6356752.314; //[m] - semiminor axis WGS84
  double phi = look_angle*D2R;

  
  double ht = sqrt(st.pos.x*st.pos.x + st.pos.y*st.pos.y + st.pos.z*st.pos.z);
  double lat = asin(st.pos.z / ht);
  double er = (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  double D = er*er - ht*ht*sin(phi)*sin(phi);
  double sr1 = ht*cos(phi) + sqrt(D);
  double sr2 = ht*cos(phi) - sqrt(D);
  double sr = sr1>sr2 ? sr2 : sr1;

  *sat_height = ht;
  *earth_rad = er;
  *sr_first = sr;
}

void sar_simulation_tool(char *demFile, satellite_t *sat, sat_t *tle)
{
  meta_parameters *metaSAR, *metaDEM;
  char orbit_direction;
  double satellite_height, orbital_inclination, look_angle;
  double pixel_size, acquisition_length, swath_velocity, latitude, longitude;
  double earth_radius, slant_range_first_pixel;
  double range_time_per_pixel, azimuth_time_per_pixel, orbital_velocity;
  double lat, lon;
  int ii, demHeight;
  int dem_grid_size = 20;
  int clean_files = TRUE;
  double cutoff = -900.0; // [m] - any height below this is considered hole
  int size; // [pixels] - size of the output image
  double re = 6378137.0; //[m] - semimajor axis WGS84
  double rp = 6356752.314; //[m] - semiminor axis WGS84
  const double g = 9.81; // [m/s^2]
  double inc = 5.0; // [s] increment for propagation

  /*
  printf("satellite: %s, sensor: %s, beam mode: %s. orbit direction: %s\n",
	 sat->satellite, sat->sensor, sat->beam_mode, sat->orbit_direction);
  printf("wavelength: %.5lf, look angle: %.4lf, pixel size: %.2lf, "
	 "pixel count: %d\n", sat->wavelength, sat->look_angle, 
	 sat->pixel_size, sat->pixel_count);
  */

  // Get essential parameters from DEM
  metaDEM = meta_read(demFile);
  latitude = metaDEM->general->center_latitude;
  longitude = metaDEM->general->center_longitude;

  // Read parameters out of satellite structure
  orbital_inclination = tle->dec;
  look_angle = sat->look_angle;
  size = sat->pixel_count;
  if (strcmp(sat->orbit_direction, "ascending")==0)
    orbit_direction = 'A';
  else if (strcmp(sat->orbit_direction, "descending")==0)
    orbit_direction = 'D';
  pixel_size = sat->pixel_size;

  // Generate a temporary directory for the output
  char *output_dir = (char *) MALLOC(sizeof(char)*512);
  strcpy(output_dir, "sst-");
  strcat(output_dir, time_stamp_dir());
  create_clean_dir(output_dir);
  chdir(output_dir);
  char *demBaseName = (char *) MALLOC(sizeof(char)*512);
  demBaseName = get_basename(demFile);

  // Propagate TLE
  double tle_time =
    time_to_secs(tle->tle.epoch_year, tle->tle.epoch_day, tle->tle.epoch_fod);
  double time = tle_time;
  double end_time = tle_time + 8640000; // something big
  stateVector st = tle_propagate(tle, time-inc);
  double old_lat = tle->ssplat;

  // Propagate with initial increment
  printf("Propagating TLE  ...\n");
  while (time < end_time) {
    st = tle_propagate(tle, time);
    char dir = tle->ssplat > old_lat ? 'A' : 'D';

    // First we need to have the correct orbit direction.
    if (dir == orbit_direction) {

      get_target_latlon(&st, sat->look_angle*D2R, &lat, &lon);

      // Second we need to be in the ballpark vicinity
      if (fabs(lat - latitude) < 0.5 && fabs(lon - longitude) < 0.5)
	break;
    }
    time += inc;
    old_lat = tle->ssplat;
  }

  // Decrease increment
  inc = 0.05;
  while (time < end_time) {
    st = tle_propagate(tle, time);
    get_target_latlon(&st, sat->look_angle*D2R, &lat, &lon);
    if (fabs(lat - latitude) < 0.01)
      break;
    time += inc;
    old_lat = tle->ssplat;
  }

  // Keep propagating with our smallest increment until latitude converges
  inc = 0.001;
  double dLat = 10.0;
  while (time < end_time) {
    st = tle_propagate(tle, time);
    get_target_latlon(&st, sat->look_angle*D2R, &lat, &lon);
    if (fabs(lat - latitude) < dLat)
      dLat = fabs(lat - latitude);
    else {
      time -= inc;
      st = tle_propagate(tle, time);
      break;
    }

    time += inc;
    old_lat = tle->ssplat;
  }

  // Generate the necessary metadata for artifical SAR image in slant range
  calc_geometry(st, sat->look_angle, sat->pixel_count, sat->pixel_size, 
		&satellite_height, &earth_radius, &slant_range_first_pixel);
  orbital_velocity = sqrt(g * earth_radius * earth_radius / satellite_height);
  swath_velocity = orbital_velocity * earth_radius / satellite_height;
  azimuth_time_per_pixel = pixel_size / swath_velocity;
  acquisition_length = azimuth_time_per_pixel * size;
  // FIXME: Close but not quite right
  range_time_per_pixel = 2 * pixel_size / speedOfLight;

  // Fill in metadata fields
  metaSAR = raw_init();
  sprintf(metaSAR->general->basename, "%s", metaDEM->general->basename);
  sprintf(metaSAR->general->sensor, "%s", sat->satellite);
  sprintf(metaSAR->general->sensor_name, "%s", sat->sensor);
  sprintf(metaSAR->general->mode, "%s", sat->beam_mode);
  sprintf(metaSAR->general->processor, "SAR simulation");
  metaSAR->general->data_type = REAL32;
  metaSAR->general->image_data_type = SIMULATED_IMAGE;
  metaSAR->general->radiometry = r_AMP;
  sprintf(metaSAR->general->system, "%s", meta_get_system());
  metaSAR->general->line_count = size;
  metaSAR->general->sample_count = size;
  metaSAR->general->orbit_direction = orbit_direction;
  metaSAR->general->frame = 
    asf_frame_calc(sat->satellite, (int)(latitude+0.5), orbit_direction);
  metaSAR->general->start_line = 0;
  metaSAR->general->start_sample = 0;
  metaSAR->general->x_pixel_size = pixel_size;
  metaSAR->general->y_pixel_size = pixel_size;
  metaSAR->general->re_major = re;
  metaSAR->general->re_minor = rp;
  metaSAR->sar = meta_sar_init();
  metaSAR->sar->image_type = 'G';
  metaSAR->sar->look_direction = 'R';
  // FIXME: hardwired for ERS for the moment
  metaSAR->sar->look_count = 5;
  metaSAR->sar->multilook = 1;
  metaSAR->sar->deskewed = 1;
  metaSAR->sar->original_line_count = size;
  metaSAR->sar->original_sample_count = size;
  metaSAR->sar->line_increment = 1;
  metaSAR->sar->sample_increment = 1;
  metaSAR->sar->range_time_per_pixel = range_time_per_pixel;
  metaSAR->sar->azimuth_time_per_pixel = 
    orbit_direction == 'A' ? azimuth_time_per_pixel : -azimuth_time_per_pixel;
  metaSAR->sar->wavelength = sat->wavelength;
  metaSAR->sar->earth_radius = earth_radius;
  metaSAR->sar->satellite_height = satellite_height;
  metaSAR->sar->slant_range_first_pixel = slant_range_first_pixel;
  metaSAR->sar->slant_shift = 0.0;
  if (orbit_direction == 'A')
    metaSAR->sar->time_shift = 0.0;
  else
    metaSAR->sar->time_shift = acquisition_length;
  for (ii=0; ii<3; ii++) {
    metaSAR->sar->range_doppler_coefficients[ii] = 0.0;
    metaSAR->sar->azimuth_doppler_coefficients[ii] = 0.0;
  }
  metaSAR->state_vectors = meta_state_vectors_init(3);
  julian_date jd;
  jd.year = tle->tle.epoch_year;
  jd.jd = tle->tle.epoch_day;
  ymd_date ymd;
  date_jd2ymd(&jd, &ymd);
  hms_time hms;
  date_sec2hms(tle->tle.epoch_fod*86400, &hms);
  if (orbit_direction == 'A')
    add_time(time-tle_time-acquisition_length, &ymd, &hms);
  else
    add_time(time-tle_time+acquisition_length, &ymd, &hms);
  date_ymd2jd(&ymd, &jd);
  metaSAR->state_vectors->year = jd.year;
  metaSAR->state_vectors->julDay = jd.jd;
  metaSAR->state_vectors->second = date_hms2sec(&hms);
  metaSAR->state_vectors->vector_count = 3;
  gei2fixed(&st, 0.0);
  metaSAR->state_vectors->vecs[0].vec = 
    propagate(st, time, time-acquisition_length/2);
  metaSAR->state_vectors->vecs[0].time = 0.0;
  metaSAR->state_vectors->vecs[1].vec = st; 
  metaSAR->state_vectors->vecs[1].time = acquisition_length/2;
  metaSAR->state_vectors->vecs[2].vec =
    propagate(st, time, time+acquisition_length/2);
  metaSAR->state_vectors->vecs[2].time = acquisition_length;
  meta_get_corner_coords(metaSAR);
  meta_get_latLon(metaSAR, size/2, size/2, 0.0, &lat, &lon);
  metaSAR->general->center_latitude = lat;
  metaSAR->general->center_longitude = lon;
  char *srFile = (char *) MALLOC(sizeof(char)*512);
  sprintf(srFile, "%s_sar.img", demBaseName);
  meta_write(metaSAR, srFile);

  // Interpolate over holes in the DEM. No need to generate any artifacts 
  // in the simulated SAR image because of that.
  asfPrintStatus("\nInterpolating holes in DEM (%s) ...\n", demFile);
  char *smoothedDem = (char *) MALLOC(sizeof(char)*512);
  sprintf(smoothedDem, "%s_smooth.img", demBaseName);
  interp_dem_holes_file(demFile, smoothedDem, cutoff, FALSE);

  // Cut out the DEM based on the artifical SAR geometry
  char *demClipped = (char *) MALLOC(sizeof(char)*512);
  sprintf(demClipped, "%s_clip.img", demBaseName);
  clip_dem(metaSAR, srFile, smoothedDem, demClipped, "DEM",
	   NULL, NULL, NULL, output_dir, dem_grid_size, clean_files,
	   &demHeight);

  // Calculate the simulated amplitude image in slant range
  asfPrintStatus("\nSimulating amplitude image in slant range geometry ...\n");
  char *demSlant = (char *) MALLOC(sizeof(char)*512);
  sprintf(demSlant, "%s_slant.img", demBaseName);
  char *demSimAmp = (char *) MALLOC(sizeof(char)*512);
  sprintf(demSimAmp, "%s_sim.img", demBaseName);
  reskew_dem(srFile, demClipped, demSlant, demSimAmp, NULL);

  // Resample simulated SAR image to DEM pixel size
  char *resampSim = (char *) MALLOC(sizeof(char)*512);
  sprintf(resampSim, "%s_resamp", demBaseName);
  resample_to_square_pixsiz(demSimAmp, resampSim, 
			    metaDEM->general->x_pixel_size);

  // Convert simulated SAR image into ground range
  char *geoSim = (char *)  MALLOC(sizeof(char)*512);
  sprintf(geoSim, "%s_geo", demBaseName);
  asf_geocode_utm(RESAMPLE_BILINEAR, 0.0, WGS84_DATUM, 
		  metaDEM->general->x_pixel_size, NULL, 
		  resampSim, geoSim, 0.0);

  // Export the ground range simulated SAR image to common graphics format
  char *exportSim = (char *) MALLOC(sizeof(char)*512);
  sprintf(exportSim, "../%s_sim.tif", demBaseName);
  asf_export(GEOTIFF, SIGMA, geoSim, exportSim);

  // Clean up
  chdir("../");
  //remove_dir(output_dir);
  meta_free(metaDEM);
  meta_free(metaSAR);
  FREE(output_dir);
  FREE(smoothedDem);
  FREE(demClipped);
  FREE(srFile);
  FREE(demSlant);
  FREE(demSimAmp);
  FREE(geoSim);
  FREE(exportSim);
}
