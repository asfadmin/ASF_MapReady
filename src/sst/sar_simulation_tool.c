#include "asf_simulation.h"

#define VERSION 1.0

static char *outputName(const char *dir, const char *base, const char *suffix)
{
  int dirlen = strlen(dir);
  int baselen = strlen(base);
  int len = dirlen > baselen ? dirlen : baselen;
  
  char *fil = MALLOC(sizeof(char)*(dirlen+baselen+strlen(suffix)+10));
  char *dirTmp = MALLOC(sizeof(char) * (len+10));
  char *fileTmp = MALLOC(sizeof(char) * (len+10));
  
  split_dir_and_file(dir, dirTmp, fileTmp);
  strcpy(fil, dirTmp);
  
  split_dir_and_file(base, dirTmp, fileTmp);
  strcat(fil, fileTmp);
  
  char *ret = appendToBasename(fil, suffix);
  
  free(fil);
  free(dirTmp);
  free(fileTmp);
  
  return ret;
}

void read_tle(char *tleFile, char *satellite, tle_t *tle)
{
  FILE *fp;
  char line[512], *s;

  s = (char *) MALLOC(sizeof(char)*25);
  fp = FOPEN(tleFile, "r");
  while (fgets(line, 512, fp)) {
    if (strncmp(line, satellite, strlen(satellite))==0) {

      fgets(line, 512, fp);

      // Epoch year
      strncpy(s, &line[18], 3);
      s[2] = 0;
      tle->year = atoi(s);
      if (tle->year > 90)
	tle->year += 1900;
      else
	tle->year += 2000;
      
      // Epoch day
      strncpy(s, &line[20], 13);
      s[12] = 0;
      tle->day = atoi(s);

      // Epoch time
      tle->time = (atof(s) - tle->day) * 86400;

      fgets(line, 512, fp);
      
      // Orbital inclination
      strncpy(s, &line[8], 9);
      s[8] = 0;
      tle->inclination = atof(s);

      // Right ascension of the ascending node
      strncpy(s, &line[17], 9);
      s[8] = 0;
      tle->cap_omega = atof(s);

      // Eccentricity
      sprintf(s, "0.");
      strncpy(&s[2], &line[26], 8);
      s[9] = 0;
      tle->eccentricity = atof(s);

      // Argument of the perigee
      strncpy(s, &line[34], 9);
      s[8] = 0;
      tle->omega = atof(s);

      // Mean anomaly
      strncpy(s, &line[43], 9);
      s[8] = 0;
      tle->mean_anomaly = atof(s);

      // Mean motion
      strncpy(s, &line[52], 12);
      s[11] = 0;
      tle->mean_motion = atof(s);
      
      // Revolution number at epoch
      strncpy(s, &line[63], 6);
      s[5] = 0;
      tle->rev_at_epoch = atoi(s);
    }
  }
  
  // Clean up
  FCLOSE(fp);
  FREE(s);
}

void read_satellite_config(char *satelliteFile, char *satellite, char *beam_mode, 
			   satellite_t *sat)
{
  FILE *fp;
  char line[512], *s;

  s = (char *) MALLOC(sizeof(char)*25);
  sprintf(s, "[%s %s]", satellite, beam_mode);
  fp = FOPEN(satelliteFile, "r");

  while (fgets(line, 512, fp)) {
    if (strncmp(line, s, strlen(s))==0) {
      fgets(line, 512, fp);
      sscanf(line, "look angle = %lf", &sat->look_angle);
      fgets(line, 512, fp);
      sscanf(line, "pixel size = %lf", &sat->pixel_size);
      fgets(line, 512, fp);
      sscanf(line, "orbital inclination = %lf", &sat->orbital_inclination);
      fgets(line, 512, fp);
      sscanf(line, "wavelength = %lf", &sat->wavelength);
    }
  }

  // Clean up
  FCLOSE(fp);
  FREE(s);
}

static double
kepler_equation (double mean_anomaly, double eccentricity)
{
  double tol1 = 1e-5, tol2 = 1e-8, ea, sin_ea, cos_ea, dea, a;
  double ma = mean_anomaly*D2R, ecc = eccentricity;
  double abm = fabs (mean_anomaly*D2R);
  int k = 0;

  ea = abm + ecc * sin(abm) / (1 - sin(abm + ecc) + sin(abm));

  do {
    sin_ea = sin(ea);
    cos_ea = cos(ea);
    k++;
    if (k>10) break;

    do {
      dea = (abm + ecc * sin_ea - ea) / (1 - ecc * cos_ea
            + (0.5 * (abm + ecc * sin_ea - ea) * ecc * sin_ea)
            / (1 - ecc * cos_ea));
      ea += dea;
      if ( fabs(dea) > tol1 ) break;
      a = 1 - 0.5 * dea *dea;
      sin_ea = a * sin_ea + dea * cos_ea;
      cos_ea = a * cos_ea - dea * sin_ea;
    } while ( fabs(dea) > tol2 );
  } while ( fabs(dea) > tol2 );

  if ( ma < 0.0 ) ea = -ea;

  return ea*R2D;
}

static void
keplerian_to_cartesian (keplerian_orbit_t *ko, cartesian_orbit_t *co)
{
  double td[8];
  double radius;

  td[0]=cos(ko->cap_omega)*cos(ko->omega)-sin(ko->cap_omega)*sin(ko->omega)
    *cos(ko->i);
  td[1]=sin(ko->cap_omega)*cos(ko->omega)+cos(ko->cap_omega)*sin(ko->omega)
    *cos(ko->i);
  td[2]=sin(ko->omega)*sin(ko->i);
  td[3]=-cos(ko->cap_omega)*sin(ko->omega)-sin(ko->cap_omega)*cos(ko->omega)
    *cos(ko->i);
  td[4]=-sin(ko->cap_omega)*sin(ko->omega)+cos(ko->cap_omega)*cos(ko->omega)
    *cos(ko->i);
  td[5]=cos(ko->omega)*sin(ko->i);
  td[6]=sqrt(1 - pow(ko->e, 2)) * ko->a;
  td[7]=sqrt(GM / pow(ko->a, 3));

  radius = ko->a * (1 - ko->e * cos(ko->ea));

  co->x = ko->a * td[0] * (cos(ko->ea) - ko->e) + td[6] * td[3] * sin(ko->ea);
  co->y = ko->a * td[1] * (cos(ko->ea) - ko->e) + td[6] * td[4] * sin(ko->ea);
  co->z = ko->a * td[2] * (cos(ko->ea) - ko->e) + td[6] * td[5] * sin(ko->ea);
  co->vx = ko->a * td[7]
    * (td[6] * cos(ko->ea) * td[3] - ko->a * sin(ko->ea) * td[0]) / radius;
  co->vy = ko->a * td[7]
    * (td[6] * cos(ko->ea) * td[4] - ko->a * sin(ko->ea) * td[1]) / radius;
  co->vz = ko->a * td[7]
    * (td[6] * cos(ko->ea) * td[5] - ko->a * sin(ko->ea) * td[2]) / radius;
}

void sar_simulation_tool(char *demFile, satellite_t *sat, tle_t *tle)
{
  keplerian_orbit_t *ko;
  cartesian_orbit_t *co;
  meta_parameters *metaSAR, *metaDEM;
  stateVector vec, st;
  char *srFile, *smoothedDem, *demClipped, *demSlant, *demSimAmp;
  char *output_dir, orbit_direction;
  double satellite_height, orbital_inclination, look_angle;
  double delta=0.1, delta_lat, old_lat;
  double period, velocity, time, radius, angle, wavelength, lat, lon;
  double pixel_size, acquisition_length, swath_velocity, latitude, longitude;
  double earth_radius, slant_range_first_pixel, slant_range_center_pixel;
  int demHeight;
  int dem_grid_size = 20;
  int clean_files = TRUE;
  double azimuth_time_per_pixel = 0.002; // [s] - realistic, yet simple
  double cutoff = -900.0; // [m] - any height below this is considered hole
  int size = 7500; // [pixels] - size of the output image, want that fixed
  double re = 6378144.0; //[m] - semimajor axis WGS84
  double rp = 6356754.9; //[m] - semiminor axis WGS84

  // Read parameters out of satellite structure
  orbital_inclination = sat->orbital_inclination;
  look_angle = sat->look_angle;;
  if (strcmp(sat->orbit_direction, "ascending")==0)
    orbit_direction = 'A';
  else if (strcmp(sat->orbit_direction, "descending")==0)
    orbit_direction = 'D';
  pixel_size = sat->pixel_size;
  wavelength = sat->wavelength;

  /*
  // Generate a temporary directory for the output
  output_dir = (char *) MALLOC(sizeof(char)*512);
  strcpy(output_dir, demFile);
  strcat(output_dir, "-");
  strcat(output_dir, time_stamp_dir());
  create_clean_dir(output_dir);
  */

  // Keplerian elements
  period = 86400 / tle->mean_motion;
  velocity = pow(2*M_PI*GM/period, 1.0/3.0);
  ko = (keplerian_orbit_t *) MALLOC(sizeof(keplerian_orbit_t));
  ko->a = GM/velocity/velocity;
  ko->e = tle->eccentricity;
  ko->i = sat->orbital_inclination;
  ko->cap_omega = tle->cap_omega;
  ko->omega = 90.0;
  ko->ea = kepler_equation(tle->mean_anomaly, tle->eccentricity);

  // Cartesian coordinates
  co = (cartesian_orbit_t *) MALLOC(sizeof(cartesian_orbit_t));
  keplerian_to_cartesian(ko, co);

  // Fill in the initial state vector in structure
  vec.pos.x = co->x;
  vec.pos.y = co->y;
  vec.pos.z = co->z;
  vec.vel.x = co->vx;
  vec.vel.y = co->vy;
  vec.vel.z = co->vz;

  // Get essential parameters from DEM
  metaDEM = meta_read(demFile);
  latitude = metaDEM->general->center_latitude;
  longitude = metaDEM->general->center_longitude;
  printf("DEM - center lat: %.4lf, center lon: %.4lf\n", latitude, longitude);

  // Calculate slant range
  satellite_height = sqrt(co->x*co->x + co->y*co->y + co->z*co->z);
  lat = asin(co->z / satellite_height);
  earth_radius = (re*rp) / 
    sqrt(rp*rp*cos(lat)*cos(lat) + re*re*sin(lat)*sin(lat));
  angle = asin(satellite_height/earth_radius*sin(look_angle*D2R))*R2D 
    - look_angle;
  slant_range_center_pixel = 
    sqrt(satellite_height*satellite_height + earth_radius*earth_radius -
	 2*satellite_height*earth_radius*cos(angle*D2R));
  angle -= size/2 * pixel_size / earth_radius * R2D;
  slant_range_first_pixel = 
    sqrt(satellite_height*satellite_height + earth_radius*earth_radius -
	 2*satellite_height*earth_radius*cos(angle*D2R));

  // Generate the necessary metadata for artifical SAR image in slant range
  swath_velocity = pixel_size / azimuth_time_per_pixel;
  acquisition_length = azimuth_time_per_pixel * size;

  // Fill in metadata fields
  metaSAR = raw_init();
  metaSAR->general->line_count = size;
  metaSAR->general->sample_count = size;
  metaSAR->general->orbit_direction = orbit_direction;
  metaSAR->general->x_pixel_size = pixel_size;
  metaSAR->general->y_pixel_size = pixel_size;
  metaSAR->general->start_line = 0;
  metaSAR->general->start_sample = 0;
  metaSAR->sar = meta_sar_init();
  metaSAR->sar->image_type = 'S';
  metaSAR->sar->look_direction = 'R';
  metaSAR->sar->deskewed = 1;
  metaSAR->sar->azimuth_time_per_pixel = azimuth_time_per_pixel;
  metaSAR->sar->earth_radius = earth_radius;
  metaSAR->sar->satellite_height = satellite_height;
  metaSAR->sar->slant_range_first_pixel = slant_range_first_pixel;
  metaSAR->sar->time_shift = 0.0;
  metaSAR->sar->slant_shift = 0.0;
  metaSAR->sar->wavelength = wavelength;
  metaSAR->state_vectors = meta_state_vectors_init(3);
  metaSAR->state_vectors->year = tle->year;
  metaSAR->state_vectors->julDay = tle->day;
  metaSAR->state_vectors->second = tle->time;
  metaSAR->state_vectors->vector_count = 3;
  metaSAR->state_vectors->vecs[0].vec = vec;
  metaSAR->state_vectors->vecs[0].time = 0.0;
  metaSAR->state_vectors->vecs[1].vec = 
    propagate(vec, tle->time, tle->time+acquisition_length/2);
  metaSAR->state_vectors->vecs[1].time = acquisition_length/2;
  metaSAR->state_vectors->vecs[2].vec = 
    propagate(vec, tle->time, tle->time+acquisition_length);
  metaSAR->state_vectors->vecs[2].time = acquisition_length;

  meta_get_latLon(metaSAR, size/2, size/2, 0.0, &lat, &lon);
  printf("lat: %.4lf, lon: %.4lf\n", lat, lon);
  old_lat = lat;

  printf("period: %.6lf\n", period);

  while (1) {
    metaSAR->state_vectors->vecs[0].vec = 
      propagate(vec, tle->time, tle->time+delta);
    metaSAR->state_vectors->vecs[1].vec = 
      propagate(vec, tle->time, tle->time+delta+acquisition_length/2);
    metaSAR->state_vectors->vecs[2].vec = 
      propagate(vec, tle->time, tle->time+delta+acquisition_length);
    metaSAR->state_vectors->second = tle->time+delta;
    meta_get_latLon(metaSAR, size/2, size/2, 0.0, &lat, &lon);
    delta_lat = lat - old_lat;
    old_lat = lat;
    delta += 0.1;
    if (fabs(latitude-lat) < 0.005) {
      printf("lat: %.4lf, lon: %.4lf\n", lat, lon);
      break;
    }
  }

  while (1) {
    metaSAR->state_vectors->vecs[0].vec = 
      propagate(vec, tle->time, tle->time+delta);
    metaSAR->state_vectors->vecs[1].vec = 
      propagate(vec, tle->time, tle->time+delta+acquisition_length/2);
    metaSAR->state_vectors->vecs[2].vec = 
      propagate(vec, tle->time, tle->time+delta+acquisition_length);
    metaSAR->state_vectors->second = tle->time+delta;
    meta_get_latLon(metaSAR, size/2, size/2, 0.0, &lat, &lon);
    delta_lat = lat - old_lat;
    old_lat = lat;
    delta += period;
    printf("time: %.3lf, lat: %.4lf, lon: %.4lf\n", 
	   metaSAR->state_vectors->second, lat, lon);
    if (fabs(longitude-lon) < 0.5)
      break;
  }

  if (delta_lat > 0.0)
    printf("ascending node\n");
  else
    printf("descending node\n");

  exit(0);

  // Interpolate over holes in the DEM. No need to generate any artifacts 
  // in the simulated SAR image because of that.
  asfPrintStatus("Interpolating holes in DEM (%s)\n", demFile);
  smoothedDem = outputName(output_dir, demFile, "_smooth");
  interp_dem_holes_file(demFile, smoothedDem, cutoff, FALSE);
  demFile = smoothedDem;

  // Cut out the DEM based on the artifical SAR geometry
  demClipped = outputName(output_dir, demFile, "_clip");
  clip_dem(metaSAR, srFile, demFile, demClipped, "DEM",
	   NULL, NULL, NULL, output_dir, dem_grid_size, clean_files,
	   &demHeight);
  srFile = outputName(output_dir, demFile, "_sar");
  meta_write(metaSAR, srFile);
  meta_free(metaSAR);  

  // Calculate the simulated amplitude image in slant range
  demSlant = outputName(output_dir, demFile, "_slant");
  demSimAmp = outputName(output_dir, demFile, "_sim");
  reskew_dem(srFile, demClipped, demSlant, demSimAmp, NULL);

  // Convert simulated SAR image into ground range

  // Export the ground range simulated SAR image to common graphics format

  // Clean up
  //remove_dir(output_dir);
  FREE(output_dir);
}
