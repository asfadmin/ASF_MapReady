#include "asf.h"
#include "shapefil.h"
#include "asf_vector.h"
#include "terrasar.h"

// Some of the functionality here is used to convert CEOS leader files as well
// as metadata into the various. When comparing the results from the two
// sources you will find a few differences.
//
// The following parameters can vary depending on the ingest parameters:
// data_type
// image_data_type
// bands
// earth_radius_pp
//
// All these are not deal with in particular within 'meta_init_ceos' but in
// 'import_ceos' instead.

// Convert metadata to generic csv file
int terrasar2csv(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  meta_parameters *meta;
  terrasar_meta *terrasar;
  dbf_header_t *dbf;
  int ii, nCols;
  char *header = (char *) MALLOC(sizeof(char)*4096);
  char *line = (char *) MALLOC(sizeof(char)*4096);

  // Read configuration file
  read_header_config("META", &dbf, &nCols);

  // Read metadata file
  terrasar = read_terrasar_meta(inFile);
  meta = terrasar2meta(terrasar);

  // Open output file
  fp = FOPEN(outFile, "w");
  strcpy(header, "");

  // Write header line
  for (ii=0; ii<nCols; ii++) {
    // General block
    if (strcmp(dbf[ii].header, "meta.general.basename") == 0 &&
        dbf[ii].visible)
      strcat(header, "Basename,");
    else if (strcmp(dbf[ii].header, "meta.general.sensor") == 0 &&
             dbf[ii].visible)
      strcat(header, "Sensor,");
    else if (strcmp(dbf[ii].header, "meta.general.sensor_name") == 0 &&
             dbf[ii].visible)
      strcat(header, "Name,");
    else if (strcmp(dbf[ii].header, "meta.general.mode") == 0 &&
             dbf[ii].visible)
      strcat(header, "Mode,");
    else if (strcmp(dbf[ii].header, "meta.general.processor") == 0 &&
             dbf[ii].visible)
      strcat(header, "Processor,");
    else if (strcmp(dbf[ii].header, "meta.general.data_type") == 0 &&
             dbf[ii].visible)
      strcat(header, "Data_type,");
    else if (strcmp(dbf[ii].header, "meta.general.image_data_type") == 0 &&
             dbf[ii].visible)
      strcat(header, "Img_data_t,");
    else if (strcmp(dbf[ii].header, "meta.general.radiometry") == 0 &&
             dbf[ii].visible)
      strcat(header, "Radiometry,");
    else if (strcmp(dbf[ii].header, "meta.general.acquisition_date") == 0 &&
             dbf[ii].visible)
      strcat(header, "Acq_date,");
    else if (strcmp(dbf[ii].header, "meta.general.orbit") == 0 &&
             dbf[ii].visible)
      strcat(header, "Orbit,");
    else if (strcmp(dbf[ii].header, "meta.general.orbit_direction") == 0 &&
             dbf[ii].visible)
      strcat(header, "Direction,");
    else if (strcmp(dbf[ii].header, "meta.general.frame") == 0 &&
             dbf[ii].visible)
      strcat(header, "Frame,");
    else if (strcmp(dbf[ii].header, "meta.general.band_count") == 0 &&
             dbf[ii].visible)
      strcat(header, "Band_count,");
    else if (strcmp(dbf[ii].header, "meta.general.bands") == 0 &&
             dbf[ii].visible)
      strcat(header, "Bands,");
    else if (strcmp(dbf[ii].header, "meta.general.line_count") == 0 &&
             dbf[ii].visible)
      strcat(header, "Lines,");
    else if (strcmp(dbf[ii].header, "meta.general.sample_count") == 0 &&
             dbf[ii].visible)
      strcat(header, "Samples,");
    else if (strcmp(dbf[ii].header, "meta.general.start_line") == 0 &&
             dbf[ii].visible)
      strcat(header, "Start_line,");
    else if (strcmp(dbf[ii].header, "meta.general.start_sample") == 0 &&
             dbf[ii].visible)
      strcat(header, "Start_sample,");
    else if (strcmp(dbf[ii].header, "meta.general.x_pixel_size") == 0 &&
             dbf[ii].visible)
      strcat(header, "X_pix_size,");
    else if (strcmp(dbf[ii].header, "meta.general.y_pixel_size") == 0 &&
             dbf[ii].visible)
      strcat(header, "Y_pix_size,");
    else if (strcmp(dbf[ii].header, "meta.general.center_latitude") == 0 &&
             dbf[ii].visible)
      strcat(header, "Center_lat,");
    else if (strcmp(dbf[ii].header, "meta.general.center_longitude") == 0 &&
             dbf[ii].visible)
      strcat(header, "Center_lon,");
    else if (strcmp(dbf[ii].header, "meta.general.re_major") == 0 &&
             dbf[ii].visible)
      strcat(header, "RE_major,");
    else if (strcmp(dbf[ii].header, "meta.general.re_minor") == 0 &&
             dbf[ii].visible)
      strcat(header, "RE major,");
    else if (strcmp(dbf[ii].header, "meta.general.bit_error_rate") == 0 &&
             dbf[ii].visible)
      strcat(header, "BER,");
    else if (strcmp(dbf[ii].header, "meta.general.missing_lines") == 0 &&
             dbf[ii].visible)
      strcat(header, "Miss_lines,");
    else if (strcmp(dbf[ii].header, "meta.general.no_data") == 0 &&
             dbf[ii].visible)
      strcat(header, "No_data,");
    // SAR block
    if (meta->sar) {
      int kk;
      char str[12];
      if (strcmp(dbf[ii].header, "meta.sar.image_type") == 0 &&
          dbf[ii].visible)
        strcat(header, "Image_type,");
      else if (strcmp(dbf[ii].header, "meta.sar.look_direction") == 0 &&
               dbf[ii].visible)
        strcat(header, "Look_dir,");
      else if (strcmp(dbf[ii].header, "meta.sar.look_count") == 0 &&
               dbf[ii].visible)
        strcat(header, "Looks,");
      else if (strcmp(dbf[ii].header, "meta.sar.deskewed") == 0 &&
               dbf[ii].visible)
        strcat(header, "Deskewed,");
      else if (strcmp(dbf[ii].header, "meta.sar.original_line_count") == 0 &&
               dbf[ii].visible)
        strcat(header, "Org_lines,");
      else if (strcmp(dbf[ii].header, "meta.sar.original_sample_count") == 0 &&
               dbf[ii].visible)
        strcat(header, "Org_samples,");
      else if (strcmp(dbf[ii].header, "meta.sar.line_increment") == 0 &&
               dbf[ii].visible)
        strcat(header, "Line_inc,");
      else if (strcmp(dbf[ii].header, "meta.sar.sample_increment") == 0 &&
               dbf[ii].visible)
        strcat(header, "Sample_inc,");
      else if (strcmp(dbf[ii].header, "meta.sar.range_time_per_pixel") == 0 &&
               dbf[ii].visible)
    strcat(header, "Rng_t_pix,");
      else if (strcmp(dbf[ii].header, "meta.sar.azimuth_time_per_pixel") == 0
               && dbf[ii].visible)
        strcat(header, "Az_t_pix,");
      else if (strcmp(dbf[ii].header, "meta.sar.slant_range_first_pixel") == 0
               && dbf[ii].visible)
        strcat(header, "Slant_range,");
      else if (strcmp(dbf[ii].header, "meta.sar.slant_shift") == 0 &&
               dbf[ii].visible)
        strcat(header, "Slant_shift,");
      else if (strcmp(dbf[ii].header, "meta.sar.time_shift") == 0 &&
               dbf[ii].visible)
        strcat(header, "Time_shift,");
      else if (strcmp(dbf[ii].header, "meta.sar.wavelength") == 0 &&
               dbf[ii].visible)
        strcat(header, "Wavelength,");
      else if (strcmp(dbf[ii].header, "meta.sar.prf") == 0 &&
               dbf[ii].visible)
        strcat(header, "PRF,");
      else if (strcmp(dbf[ii].header, "meta.sar.earth_radius") == 0 &&
               dbf[ii].visible)
        strcat(header, "Earth_rad,");
      else if (strcmp(dbf[ii].header, "meta.sar.earth_radius_pp") == 0 &&
               dbf[ii].visible)
        strcat(header, "ER_pp,");
      else if (strcmp(dbf[ii].header, "meta.sar.satellite_height") == 0 &&
               dbf[ii].visible)
        strcat(header, "Sat_height,");
      else if (strcmp(dbf[ii].header, "meta.sar.satellite_binary_time") == 0 &&
               dbf[ii].visible)
        strcat(header, "Sat_bin_t,");
      else if (strcmp(dbf[ii].header, "meta.sar.satellite_clock_time") == 0 &&
               dbf[ii].visible)
        strcat(header, "Sat_clock_t,");
      else if (strcmp(dbf[ii].header,
                      "meta.sar.range_doppler_coefficients") == 0 &&
               dbf[ii].visible) {
        strcat(header, "Rng_dop_1,");
        strcat(header, "Rng_dop_2,");
        strcat(header, "Rng_dop_3,");
      }
      else if (strcmp(dbf[ii].header,
                      "meta.sar.azimuth_doppler_coefficients") == 0 &&
               dbf[ii].visible) {
        strcat(header, "Az_dop_1,");
        strcat(header, "Az_dop_2,");
        strcat(header, "Az_dop_3,");
      }
      else if (strcmp(dbf[ii].header,
              "meta.sar.azimuth_processing_bandwidth") == 0 &&
           dbf[ii].visible)
        strcat(header, "Az_proc_bw,");
      else if (strcmp(dbf[ii].header, "meta.sar.chirp_rate") == 0 &&
               dbf[ii].visible)
        strcat(header, "Chirp_rate,");
      else if (strcmp(dbf[ii].header, "meta.sar.pulse_duration") == 0 &&
               dbf[ii].visible)
        strcat(header, "Pulse_dur,");
      else if (strcmp(dbf[ii].header, "meta.sar.range_sampling_rate") == 0 &&
               dbf[ii].visible)
        strcat(header, "Rng_samp_r,");
      else if (strcmp(dbf[ii].header, "meta.sar.polarization") == 0 &&
               dbf[ii].visible)
        strcat(header, "Polarization,");
      else if (strcmp(dbf[ii].header, "meta.sar.multilook") == 0 &&
               dbf[ii].visible)
        strcat(header, "Multilook,");
      else if (strcmp(dbf[ii].header, "meta.sar.pitch") == 0 &&
               dbf[ii].visible)
        strcat(header, "Pitch,");
      else if (strcmp(dbf[ii].header, "meta.sar.roll") == 0 &&
               dbf[ii].visible)
        strcat(header, "Roll,");
      else if (strcmp(dbf[ii].header, "meta.sar.yaw") == 0 &&
               dbf[ii].visible)
        strcat(header, "Yaw,");
      else if (strcmp(dbf[ii].header, "meta.sar.incid_a") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<6; kk++) {
          sprintf(str, "Incid_a[%d],", kk+1);
          strcat(header, str);
        }
      }
    }
    // Optical block
    if (meta->optical) {
      if (strcmp(dbf[ii].header, "meta.optical.pointing_direction") == 0 &&
          dbf[ii].visible)
        strcat(header, "Point_dir,");
      else if (strcmp(dbf[ii].header, "meta.optical.off_nadir_angle") == 0 &&
               dbf[ii].visible)
        strcat(header, "Off_nadir,");
      else if (strcmp(dbf[ii].header, "meta.optical.correction_level") == 0 &&
               dbf[ii].visible)
        strcat(header, "Corr_level,");
      else if (strcmp(dbf[ii].header, "meta.optical.cloud_percentage") == 0 &&
               dbf[ii].visible)
        strcat(header, "Cloud_perc,");
      else if (strcmp(dbf[ii].header, "meta.optical.sun_azimuth_angle") == 0 &&
               dbf[ii].visible)
        strcat(header, "Sun_az_ang,");
      else if (strcmp(dbf[ii].header, "meta.optical.sun_elevation_angle") == 0
               && dbf[ii].visible)
        strcat(header, "Sun_elev,");
    }
    // Thermal block
    if (meta->thermal) {
      if (strcmp(dbf[ii].header, "meta.thermal.band_gain") == 0 &&
          dbf[ii].visible)
        strcat(header, "Band_gain,");
      else if (strcmp(dbf[ii].header, "meta.thermal.band_gain_change") == 0 &&
               dbf[ii].visible)
        strcat(header, "Gain_change,");
      else if (strcmp(dbf[ii].header, "meta.thermal.day") == 0 &&
               dbf[ii].visible)
    strcat(header, "Day,");
    }
    // Transform block
    if (meta->transform) {
      int kk;
      char str[12];
      int n = meta->transform->parameter_count;
      if (strcmp(dbf[ii].header, "meta.transform.parameter_count") == 0 &&
          dbf[ii].visible)
        strcat(header, "Parameters,");
      else if (strcmp(dbf[ii].header, "meta.transform.x") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(str, "X[%d],", kk+1);
          strcat(header, str);
        }
      }
      else if (strcmp(dbf[ii].header, "meta.transform.y") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(str, "Y[%d],", kk+1);
          strcat(header, str);
        }
      }
      else if (strcmp(dbf[ii].header, "meta.transform.l") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(str, "L[%d],", kk+1);
          strcat(header, str);
        }
      }
      else if (strcmp(dbf[ii].header, "meta.transform.s") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(str, "S[%d],", kk+1);
          strcat(header, str);
        }
      }
    }
    // AirSAR block
    if (meta->airsar) {
      if (strcmp(dbf[ii].header, "meta.airsar.scale_factor") == 0 &&
          dbf[ii].visible)
        strcat(header, "Scale,");
      else if (strcmp(dbf[ii].header, "meta.airsar.gps_altitude") == 0 &&
           dbf[ii].visible)
        strcat(header, "GPS_height,");
      else if (strcmp(dbf[ii].header, "meta.airsar.lat_peg_point") == 0 &&
               dbf[ii].visible)
        strcat(header, "Lat_peg_pt,");
      else if (strcmp(dbf[ii].header, "meta.airsar.lon_peg_point") == 0 &&
               dbf[ii].visible)
        strcat(header, "Lon_peg_pt,");
      else if (strcmp(dbf[ii].header, "meta.airsar.head_peg_point") == 0 &&
               dbf[ii].visible)
        strcat(header, "Head_pegpt,");
      else if (strcmp(dbf[ii].header, "meta.airsar.along_track_offset") == 0 &&
               dbf[ii].visible)
        strcat(header, "AT_offset,");
      else if (strcmp(dbf[ii].header, "meta.airsar.cross_track_offset") == 0 &&
               dbf[ii].visible)
        strcat(header, "CT_offset,");
    }
    // Projection block
    if (meta->projection) {
      if (strcmp(dbf[ii].header, "meta.projection.type") == 0 &&
          dbf[ii].visible)
        strcat(header, "Proc_type,");
      else if (strcmp(dbf[ii].header, "meta.projection.startX") == 0 &&
               dbf[ii].visible)
        strcat(header, "Start_x,");
      else if (strcmp(dbf[ii].header, "meta.projection.startY") == 0 &&
               dbf[ii].visible)
        strcat(header, "Start_y,");
      else if (strcmp(dbf[ii].header, "meta.projection.perX") == 0 &&
               dbf[ii].visible)
        strcat(header, "Per_x,");
      else if (strcmp(dbf[ii].header, "meta.projection.perY") == 0 &&
               dbf[ii].visible)
        strcat(header, "Per_y,");
      else if (strcmp(dbf[ii].header, "meta.projection.units") == 0 &&
               dbf[ii].visible)
        strcat(header, "Units,");
      else if (strcmp(dbf[ii].header, "meta.projection.hem") == 0 &&
               dbf[ii].visible)
        strcat(header, "Hemisphere,");
      else if (strcmp(dbf[ii].header, "meta.projection.spheroid") == 0 &&
               dbf[ii].visible)
        strcat(header, "Spheroid,");
      else if (strcmp(dbf[ii].header, "meta.projection.re_major") == 0 &&
               dbf[ii].visible)
        strcat(header, "Proc_major,");
      else if (strcmp(dbf[ii].header, "meta.projection.re_minor") == 0 &&
               dbf[ii].visible)
        strcat(header, "Proc_minor,");
      else if (strcmp(dbf[ii].header, "meta.projection.datum") == 0 &&
               dbf[ii].visible)
        strcat(header, "Datum,");
      else if (strcmp(dbf[ii].header, "meta.projection.height") == 0 &&
               dbf[ii].visible)
        strcat(header, "Proc_ht,");
      if (meta->projection->type == ALBERS_EQUAL_AREA) {
        if (strcmp(dbf[ii].header,
                   "meta.projection.param.albers.std_parallel1") == 0 &&
            dbf[ii].visible)
          strcat(header, "Std_par_1,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.std_parallel2") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Std_par_2,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.center_meridian") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Center_mer,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.orig_latitude") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Orig_lat,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.false_easting") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_east,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.false_northing") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Flse_north,");
      }
      else if (meta->projection->type == SCANSAR_PROJECTION) {
        if (strcmp(dbf[ii].header, "meta.projection.param.atct.rlocal") == 0 &&
            dbf[ii].visible)
          strcat(header, "R_local,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.atct.alpha1") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Alpha_1,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.atct.alpha2") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Alpha_2,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.atct.alpha3") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Alphar_3,");
      }
      else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
        if (strcmp(dbf[ii].header,
                   "meta.projection.param.lamaz.center_lat") == 0 &&
            dbf[ii].visible)
          strcat(header, "Center_lat,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamaz.center_lon") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Center_lon,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamaz.false_easting") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_east,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamaz.false_northing") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Flse_north,");
      }
      else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC) {
        if (strcmp(dbf[ii].header, "meta.projection.param.lamcc.plat1") == 0 &&
            dbf[ii].visible)
          strcat(header, "Std_par_1,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.plat2") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Std_par_2,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.lat0") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Orig_lat,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.lon0") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Center_mer,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.false_easting") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_east,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.false_northing") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Flse_north,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.scale_factor") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Proc_scale,");
      }
      else if (meta->projection->type == POLAR_STEREOGRAPHIC) {
        if (strcmp(dbf[ii].header, "meta.projection.param.ps.slat") == 0 &&
            dbf[ii].visible)
          strcat(header, "Std_par,");
        else if (strcmp(dbf[ii].header, "meta.projection.param.ps.slon") == 0
                 && dbf[ii].visible)
          strcat(header, "Center_mer,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.ps.false_easting") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_east,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.ps.false_northing") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_north,");
      }
      else if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
        if (strcmp(dbf[ii].header, "meta.projection.param.utm.zone") == 0 &&
            dbf[ii].visible)
          strcat(header, "Zone,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.false_easting") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_east,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.false_northing") == 0 &&
                 dbf[ii].visible)
          strcat(header, "False_north,");
        else if (strcmp(dbf[ii].header, "meta.projection.param.utm.lat0") == 0
                 && dbf[ii].visible)
          strcat(header, "Proc_lat,");
        else if (strcmp(dbf[ii].header, "meta.projection.param.utm.lon0") == 0
                 && dbf[ii].visible)
          strcat(header, "Proc_lon,");
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.scale_factor") == 0 &&
                 dbf[ii].visible)
          strcat(header, "Proc_scale,");
      }
      else if (meta->projection->type == STATE_PLANE) {
        if (strcmp(dbf[ii].header, "meta.projection.param.state.zone") == 0 &&
            dbf[ii].visible)
          strcat(header, "Zone,");
      }
    }
    // Statistics block
    if (meta->stats) {
      int kk;
      int n = meta->stats->band_count;
      char str[12];
      if (strcmp(dbf[ii].header, "meta.stats.band_count") == 0 &&
          dbf[ii].visible)
        strcat(header, "Stat_bands,");
      for (kk=0; kk<n; kk++) {
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.band_id") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "Band_ID_%d,", kk+1);
          strcat(header, str);
        }
        else if (strcmp(dbf[ii].header, "meta.stats.band_stats.min") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "Minimum_%d,", kk+1);
          strcat(header, str);
        }
        else if (strcmp(dbf[ii].header, "meta.stats.band_stats.max") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "Maximum_%d,", kk+1);
          strcat(header, str);
        }
        else if (strcmp(dbf[ii].header, "meta.stats.band_stats.mean") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "Mean_%d,", kk+1);
          strcat(header, str);
        }
        else if (strcmp(dbf[ii].header, "meta.stats.band_stats.rmse") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "RMSE_%d,", kk+1);
          strcat(header, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.stats.band_stats.std_deviation") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "Std_dev_%d", kk+1);
          strcat(header, str);
        }
        else if (strcmp(dbf[ii].header, "meta.stats.band_stats.mask") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "Mask_%d,", kk+1);
          strcat(header, str);
        }
      }
    }
    // State vector block
    if (meta->state_vectors) {
      int kk;
      int n = meta->state_vectors->vector_count;
      char str[12];
      if (strcmp(dbf[ii].header, "meta.state.year") == 0 && dbf[ii].visible)
        strcat(header, "Year,");
      else if (strcmp(dbf[ii].header, "meta.state.julDay") == 0 &&
               dbf[ii].visible)
        strcat(header, "Julian_day,");
      else if (strcmp(dbf[ii].header, "meta.state.second") == 0 &&
               dbf[ii].visible)
        strcat(header, "Second,");
      else if (strcmp(dbf[ii].header, "meta.state.vector_count") == 0 &&
               dbf[ii].visible)
        strcat(header, "Num_stVec,");
      else if (strcmp(dbf[ii].header, "meta.state.vectors") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(str, "Time_%d,", kk+1);
          strcat(header, str);
          sprintf(str, "Pos_x_%d,", kk+1);
          strcat(header, str);
          sprintf(str, "Pos_y_%d,", kk+1);
          strcat(header, str);
          sprintf(str, "Pos_z_%d,", kk+1);
          strcat(header, str);
          sprintf(str, "Vel_x_%d,", kk+1);
          strcat(header, str);
          sprintf(str, "Vel_y_%d,", kk+1);
          strcat(header, str);
          sprintf(str, "Vel_z_%d,", kk+1);
          strcat(header, str);
        }
      }
    }
    // Location block
    if (meta->location) {
      if (strcmp(dbf[ii].header, "meta.location.lat_start_near_range") == 0 &&
          dbf[ii].visible)
        strcat(header, "Lat_1,");
      else if (strcmp(dbf[ii].header,
                      "meta.location.lon_start_near_range") == 0 &&
               dbf[ii].visible)
        strcat(header, "Lon_1,");
      else if (strcmp(dbf[ii].header,
                      "meta.location.lat_start_far_range") == 0 &&
               dbf[ii].visible)
        strcat(header, "Lat_2,");
      else if (strcmp(dbf[ii].header,
                      "meta.location.lon_start_far_range") == 0 &&
               dbf[ii].visible)
        strcat(header, "Lon_2,");
      else if (strcmp(dbf[ii].header, "meta.location.lat_end_near_range") == 0
               && dbf[ii].visible)
        strcat(header, "Lat_3,");
      else if (strcmp(dbf[ii].header, "meta.location.lon_end_near_range") == 0
               && dbf[ii].visible)
        strcat(header, "Lon_3,");
      else if (strcmp(dbf[ii].header, "meta.location.lat_end_far_range") == 0
               && dbf[ii].visible)
        strcat(header, "Lat_4,");
      else if (strcmp(dbf[ii].header, "meta.location.lon_end_far_range") == 0
               && dbf[ii].visible)
        strcat(header, "Lon_4,");
    }
  }
  strip_end_whitesp_inplace(header);
  fprintf(fp, "%s\n", header);

  char str[255];

  // Write line
  for (ii=0; ii<nCols; ii++) {
    // General block
    if (strcmp(dbf[ii].header, "meta.general.basename") == 0 &&
        dbf[ii].visible) {
      sprintf(str, "%s,", meta->general->basename);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.sensor") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", meta->general->sensor);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.sensor_name") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", meta->general->sensor_name);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.mode") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", meta->general->mode);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.processor") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", meta->general->processor);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.data_type") == 0 &&
             dbf[ii].visible) {
      char data_type[25];
      if (meta->general->data_type == BYTE)
        strcpy(data_type, "BYTE");
      else if (meta->general->data_type == INTEGER16)
        strcpy(data_type, "INTEGER16");
      else if (meta->general->data_type == INTEGER32)
        strcpy(data_type, "INTEGER32");
      else if (meta->general->data_type == REAL32)
        strcpy(data_type, "REAL32");
      else if (meta->general->data_type == REAL64)
        strcpy(data_type, "REAL64");
      else if (meta->general->data_type == COMPLEX_BYTE)
        strcpy(data_type, "COMPLEX_BYTE");
      else if (meta->general->data_type == COMPLEX_INTEGER16)
        strcpy(data_type, "COMPLEX_INTEGER16");
      else if (meta->general->data_type == COMPLEX_INTEGER32)
        strcpy(data_type, "COMPLEX_INTEGER32");
      else if (meta->general->data_type == COMPLEX_REAL32)
        strcpy(data_type, "COMPLEX_REAL32");
      else if (meta->general->data_type == COMPLEX_REAL64)
        strcpy(data_type, "COMPLEX_REAL64");
      sprintf(str, "%s,", data_type);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.image_data_type") == 0 &&
             dbf[ii].visible) {
      char image_data_type[25];
      if (meta->general->image_data_type == RAW_IMAGE)
        strcpy(image_data_type, "RAW_IMAGE");
      else if (meta->general->image_data_type == COMPLEX_IMAGE)
        strcpy(image_data_type, "COMPLEX_IMAGE");
      else if (meta->general->image_data_type == AMPLITUDE_IMAGE)
        strcpy(image_data_type, "AMPLITUDE_IMAGE");
      else if (meta->general->image_data_type == PHASE_IMAGE)
        strcpy(image_data_type, "PHASE_IMAGE");
      else if (meta->general->image_data_type == INTERFEROGRAM)
        strcpy(image_data_type, "INTERFEROGRAM");
      else if (meta->general->image_data_type == COHERENCE_IMAGE)
        strcpy(image_data_type, "COHERENCE_IMAGE");
      else if (meta->general->image_data_type == POLARIMETRIC_IMAGE)
	strcpy(image_data_type, "POLARIMETRIC_IMAGE");
      else if (meta->general->image_data_type == POLARIMETRIC_SEGMENTATION)
        strcpy(image_data_type, "POLARIMETRIC_SEGMENTATION");
      else if (meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
        strcpy(image_data_type, "POLARIMETRIC_DECOMPOSITION");
      else if (meta->general->image_data_type == POLARIMETRIC_PARAMETER)
        strcpy(image_data_type, "POLARIMETRIC_PARAMETER");
      else if (meta->general->image_data_type == POLARIMETRIC_C2_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C2_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_C3_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C3_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_C4_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C4_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_T3_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_T3_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_T4_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_T4_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_STOKES_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_STOKES_MATRIX");
      else if (meta->general->image_data_type == LUT_IMAGE)
        strcpy(image_data_type, "LUT_IMAGE");
      else if (meta->general->image_data_type == ELEVATION)
        strcpy(image_data_type, "ELEVATION");
      else if (meta->general->image_data_type == DEM)
        strcpy(image_data_type, "DEM");
      else if (meta->general->image_data_type == IMAGE)
        strcpy(image_data_type, "IMAGE");
      else if (meta->general->image_data_type == MASK)
        strcpy(image_data_type, "MASK");
      sprintf(str, "%s,", image_data_type);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.radiometry") == 0 &&
             dbf[ii].visible) {
      char radiometry[20];
      if (meta->general->radiometry == r_AMP)
        strcpy(radiometry, "AMPLITUDE");
      else if (meta->general->radiometry == r_SIGMA)
        strcpy(radiometry, "SIGMA");
      else if (meta->general->radiometry == r_BETA)
        strcpy(radiometry, "BETA");
      else if (meta->general->radiometry == r_GAMMA)
        strcpy(radiometry, "GAMMA");
      else if (meta->general->radiometry == r_SIGMA_DB)
        strcpy(radiometry, "SIGMA_DB");
      else if (meta->general->radiometry == r_BETA_DB)
        strcpy(radiometry, "BETA_DB");
      else if (meta->general->radiometry == r_GAMMA_DB)
        strcpy(radiometry, "GAMMA_DB");
      else if (meta->general->radiometry == r_POWER)
        strcpy(radiometry, "POWER");
      sprintf(str, "%s,", radiometry);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.acquisition_date") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "\"%s\",", meta->general->acquisition_date);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.orbit") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->orbit);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.orbit_direction") == 0 &&
             dbf[ii].visible) {
      if (meta->general->orbit_direction == 'A')
        strcpy(str, "Ascending,");
      else
    strcpy(str, "Descending,");
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.frame") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->frame);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.band_count") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->band_count);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.bands") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", meta->general->bands);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.line_count") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->line_count);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.sample_count") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->sample_count);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.start_line") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->start_line);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.start_sample") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->start_sample);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.x_pixel_size") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->x_pixel_size));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.y_pixel_size") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->y_pixel_size));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.center_latitude") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->center_latitude));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.center_longitude") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->center_longitude));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.re_major") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->re_major));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.re_minor") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->re_minor));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.bit_error_rate") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->bit_error_rate));
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.missing_lines") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%d,", meta->general->missing_lines);
      strcat(line, str);
    }
    else if (strcmp(dbf[ii].header, "meta.general.no_data") == 0 &&
             dbf[ii].visible) {
      sprintf(str, "%s,", lf(meta->general->no_data));
      strcat(line, str);
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // SAR block
    if (meta->sar) {
      if (strcmp(dbf[ii].header, "meta.sar.image_type") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%c,", meta->sar->image_type);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.look_direction") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%c,", meta->sar->look_direction);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.look_count") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->sar->look_count);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.deskewed") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->sar->deskewed);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.original_line_count") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->sar->original_line_count);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.original_sample_count") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->sar->original_sample_count);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.line_increment") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->line_increment));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.sample_increment") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->sample_increment));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.range_time_per_pixel") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->range_time_per_pixel));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.azimuth_time_per_pixel") == 0
               && dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->azimuth_time_per_pixel));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.slant_shift") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->slant_shift));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.time_shift") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->time_shift));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.slant_range_first_pixel") == 0
               && dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->slant_range_first_pixel));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.wavelength") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->wavelength));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.prf") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->prf));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.earth_radius") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->earth_radius));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.earth_radius_pp") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->earth_radius_pp));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.satellite_height") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->satellite_height));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.satellite_binary_time") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", meta->sar->satellite_binary_time);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.satellite_clock_time") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", meta->sar->satellite_clock_time);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.sar.range_doppler_coefficients") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->range_doppler_coefficients[0]));
        strcat(line, str);
        
        sprintf(str, "%s,", lf(meta->sar->range_doppler_coefficients[1]));
        strcat(line, str);
        
        sprintf(str, "%s,", lf(meta->sar->range_doppler_coefficients[2]));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.sar.azimuth_doppler_coefficients") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->azimuth_doppler_coefficients[0]));
        strcat(line, str);
        
        sprintf(str, "%s,", lf(meta->sar->azimuth_doppler_coefficients[1]));
        strcat(line, str);
        
        sprintf(str, "%s,", lf(meta->sar->azimuth_doppler_coefficients[2]));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.sar.azimuth_processing_bandwidth") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->azimuth_processing_bandwidth));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.chirp_rate") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->chirp_rate));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.pulse_duration") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->pulse_duration));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.range_sampling_rate") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->range_sampling_rate));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.polarization") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", meta->sar->polarization);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.multilook") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->sar->multilook);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.pitch") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->pitch));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.roll") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->roll));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.sar.yaw") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->sar->yaw));
        strcat(line, str);
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Optical block
    if (meta->optical) {
      if (strcmp(dbf[ii].header, "meta.optical.pointing_direction") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%s,", meta->optical->pointing_direction);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.optical.off_nadir_angle") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->optical->off_nadir_angle));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.optical.correction_level") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", meta->optical->correction_level);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.optical.cloud_percentage") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->optical->cloud_percentage));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.optical.sun_azimuth_angle") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->optical->sun_azimuth_angle));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.optical.sun_elevation_angle") == 0
               && dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->optical->sun_elevation_angle));
        strcat(line, str);
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Thermal block
    if (meta->thermal) {
      if (strcmp(dbf[ii].header, "meta.thermal.band_gain") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->thermal->band_gain));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.thermal.band_gain_change") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->thermal->band_gain_change));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.thermal.day") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->thermal->day);
        strcat(line, str);
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Transform block
    if (meta->transform) {
      int kk;
      if (strcmp(dbf[ii].header, "meta.transform.parameter_count") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%d,", meta->transform->parameter_count);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.transform.x") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          sprintf(str, "%s,", lf(meta->transform->x[kk]));
          strcat(line, str);
        }
      }
      else if (strcmp(dbf[ii].header, "meta.transform.y") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          sprintf(str, "%s,", lf(meta->transform->y[kk]));
          strcat(line, str);
        }
      }
      else if (strcmp(dbf[ii].header, "meta.transform.l") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          sprintf(str, "%s,", lf(meta->transform->l[kk]));
          strcat(line, str);
        }
      }
      else if (strcmp(dbf[ii].header, "meta.transform.s") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          sprintf(str, "%s,", lf(meta->transform->s[kk]));
          strcat(line, str);
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // AirSAR block
    if (meta->airsar) {
      if (strcmp(dbf[ii].header, "meta.airsar.scale_factor") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->scale_factor));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.airsar.gps_altitude") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->gps_altitude));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.airsar.lat_peg_point") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->lat_peg_point));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.airsar.lon_peg_point") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->lon_peg_point));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.airsar.head_peg_point") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->head_peg_point));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.airsar.along_track_offset") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->along_track_offset));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.airsar.cross_track_offset") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->airsar->cross_track_offset));
        strcat(line, str);
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Projection block
    if (meta->projection) {
      if (strcmp(dbf[ii].header, "meta.projection.type") == 0 &&
          dbf[ii].visible) {
        char type[50];
        if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR)
          strcpy(type, "UNIVERSAL TRANSVERSE MERCATOR");
        else if (meta->projection->type == POLAR_STEREOGRAPHIC)
          strcpy(type, "POLAR STEREOGRAPHIC");
        else if (meta->projection->type == ALBERS_EQUAL_AREA)
          strcpy(type, "ALBERS EQUAL AREA");
        else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC)
          strcpy(type, "LAMBERT CONFORMAL CONIC");
        else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
          strcpy(type, "LAMBERT AZIMUTHAL EQUAL AREA");
        else if (meta->projection->type == STATE_PLANE)
          strcpy(type, "STATE PLANE");
        else if (meta->projection->type == SCANSAR_PROJECTION)
          strcpy(type, "SCANSAR PROJECTION");
        else if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION)
          strcpy(type, "LAT LONG PSEUDO PROJECTION");
        else if (meta->projection->type == UNKNOWN_PROJECTION)
          strcpy(type, "UNKNOWN PROJECTION");
        sprintf(str, "%s,", type);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.startX") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->startX));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.startY") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->startY));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.perX") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->perX));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.perY") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->perY));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.units") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", meta->projection->units);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.hem") == 0 &&
               dbf[ii].visible) {
        if (meta->projection->hem == 'N')
          strcpy(str, "North,");
        else if (meta->projection->hem == 'S')
          strcpy(str, "South,");
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.spheroid") == 0 &&
               dbf[ii].visible) {
        char spheroid[25];
        if (meta->projection->spheroid == BESSEL_SPHEROID)
          strcpy(spheroid, "BESSEL");
        else if (meta->projection->spheroid == CLARKE1866_SPHEROID)
          strcpy(spheroid, "CLARKE1866");
        else if (meta->projection->spheroid == CLARKE1880_SPHEROID)
          strcpy(spheroid, "CLARKE1880");
        else if (meta->projection->spheroid == GEM6_SPHEROID)
          strcpy(spheroid, "GEM6");
        else if (meta->projection->spheroid == GEM10C_SPHEROID)
          strcpy(spheroid, "GEM10C");
        else if (meta->projection->spheroid == GRS1980_SPHEROID)
          strcpy(spheroid, "GRS1980");
        else if (meta->projection->spheroid == INTERNATIONAL1924_SPHEROID)
          strcpy(spheroid, "INTERNATIONAL1924");
        else if (meta->projection->spheroid == INTERNATIONAL1967_SPHEROID)
          strcpy(spheroid, "INTERNATIONAL1967");
        else if (meta->projection->spheroid == WGS72_SPHEROID)
          strcpy(spheroid, "WGS72");
        else if (meta->projection->spheroid == WGS84_SPHEROID)
          strcpy(spheroid, "WGS84");
        else if (meta->projection->spheroid == HUGHES_SPHEROID)
          strcpy(spheroid, "HUGHES");
        else
          strcpy(spheroid, "UNKNOWN");
        sprintf(str, "%s,", spheroid);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.re_major") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->re_major));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.re_minor") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->re_minor));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.datum") == 0 &&
               dbf[ii].visible) {
        char datum[25];
        if (meta->projection->datum == EGM96_DATUM)
          strcpy(datum, "EGM96");
        else if (meta->projection->datum == ED50_DATUM)
          strcpy(datum, "ED50");
        else if (meta->projection->datum == ETRF89_DATUM)
          strcpy(datum, "ETRF89");
        else if (meta->projection->datum == ETRS89_DATUM)
          strcpy(datum, "ETRS89");
        else if (meta->projection->datum == ITRF97_DATUM)
          strcpy(datum, "ITRF97");
        else if (meta->projection->datum == NAD27_DATUM)
          strcpy(datum, "NAD27");
        else if (meta->projection->datum == NAD83_DATUM)
          strcpy(datum, "NAD83");
        else if (meta->projection->datum == WGS72_DATUM)
          strcpy(datum, "WGS72");
        else if (meta->projection->datum == WGS84_DATUM)
          strcpy(datum, "WGS84");
        else if (meta->projection->datum == HUGHES_DATUM)
          strcpy(datum, "HUGHES");
        else
          strcpy(datum, "UNKNOWN");
        sprintf(str, "%s,", datum);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.projection.height") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->projection->height));
        strcat(line, str);
      }
      if (meta->projection->type == ALBERS_EQUAL_AREA) {
        if (strcmp(dbf[ii].header,
                   "meta.projection.param.albers.std_parallel1") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.albers.std_parallel1));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.std_parallel2") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.albers.std_parallel2));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.center_meridian") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.albers.center_meridian));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.orig_latitude") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.albers.orig_latitude));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.false_easting") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.albers.false_easting));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.albers.false_northing") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.albers.false_northing));
          strcat(line, str);
        }
      }
      else if (meta->projection->type == SCANSAR_PROJECTION) {
        if (strcmp(dbf[ii].header, "meta.projection.param.atct.rlocal") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.atct.rlocal));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.atct.alpha1") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.atct.alpha1));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.atct.alpha2") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.atct.alpha2));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.atct.alpha3") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.atct.alpha3));
          strcat(line, str);
        }
      }
      else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
        if (strcmp(dbf[ii].header,
                   "meta.projection.param.lamaz.center_lat") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamaz.center_lat));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamaz.center_lon") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamaz.center_lon));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamaz.false_easting") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamaz.false_easting));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamaz.false_northing") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.lamaz.false_northing));
          strcat(line, str);
        }
      }
      else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC) {
        if (strcmp(dbf[ii].header, "meta.projection.param.lamcc.plat1") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamcc.plat1));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.plat2") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamcc.plat2));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.lat0") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamcc.lat0));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.lon0") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamcc.lon0));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.false_easting") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamcc.false_easting));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.false_northing") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,",
          lf(meta->projection->param.lamcc.false_northing));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.lamcc.scale_factor") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.lamcc.scale_factor));
          strcat(line, str);
        }
      }
      else if (meta->projection->type == POLAR_STEREOGRAPHIC) {
        if (strcmp(dbf[ii].header, "meta.projection.param.ps.slat") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.ps.slat));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.ps.slon") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.ps.slon));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.ps.false_easting") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.ps.false_easting));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.ps.false_northing") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.ps.false_northing));
          strcat(line, str);
        }
      }
      else if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
        if (strcmp(dbf[ii].header, "meta.projection.param.utm.zone") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%d,", meta->projection->param.utm.zone);
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.false_easting") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.utm.false_easting));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.false_northing") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.utm.false_northing));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.lat0") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.utm.lat0));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.lon0") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.utm.lon0));
          strcat(line, str);
        }
        else if (strcmp(dbf[ii].header,
                        "meta.projection.param.utm.scale_factor") == 0 &&
                 dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->projection->param.utm.scale_factor));
          strcat(line, str);
        }
      }
      else if (meta->projection->type == STATE_PLANE) {
        if (strcmp(dbf[ii].header, "meta.projection.param.state.zone") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%d,", meta->projection->param.state.zone);
          strcat(line, str);
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Stats block
    if (meta->stats) {
      int kk;
      if (strcmp(dbf[ii].header, "meta.stats.band_count") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%d,", meta->stats->band_count);
        strcat(line, str);
      }
      for (kk=0; kk<meta->stats->band_count; kk++) {
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.band_id") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", meta->stats->band_stats[kk].band_id);
          strcat(line, str);
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.min") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->stats->band_stats[kk].min));
          strcat(line, str);
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.max") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->stats->band_stats[kk].max));
          strcat(line, str);
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.mean") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->stats->band_stats[kk].mean));
          strcat(line, str);
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.rmse") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->stats->band_stats[kk].rmse));
          strcat(line, str);
        }
        if (strcmp(dbf[ii].header,
                   "meta.stats.band_stats.std_deviation") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->stats->band_stats[kk].std_deviation));
          strcat(line, str);
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.mask") == 0 &&
            dbf[ii].visible) {
          sprintf(str, "%s,", lf(meta->stats->band_stats[kk].mask));
          strcat(line, str);
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // State vector block
    if (meta->state_vectors) {
      int kk;
      int n = meta->state_vectors->vector_count;
      if (strcmp(dbf[ii].header, "meta.state.year") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%d,", meta->state_vectors->year);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.state.julDay") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->state_vectors->julDay);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.state.second") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->state_vectors->second));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.state.vector_count") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%d,", meta->state_vectors->vector_count);
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header, "meta.state.vectors") == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].time));
          strcat(line, str);
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].vec.pos.x));
          strcat(line, str);
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].vec.pos.y));
          strcat(line, str);
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].vec.pos.z));
          strcat(line, str);
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].vec.vel.x));
          strcat(line, str);
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].vec.vel.y));
          strcat(line, str);
          sprintf(str, "%s,", lf(meta->state_vectors->vecs[kk].vec.vel.z));
          strcat(line, str);
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Location block
    if (meta->location) {
      if (strcmp(dbf[ii].header, "meta.location.lat_start_near_range") == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lat_start_near_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lon_start_near_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lon_start_near_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lat_start_far_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lat_start_far_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lon_start_far_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lon_start_far_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lat_end_near_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lat_end_near_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lon_end_near_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lon_end_near_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lat_end_far_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lat_end_far_range));
        strcat(line, str);
      }
      else if (strcmp(dbf[ii].header,
                      "meta.location.lon_end_far_range") == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%s,", lf(meta->location->lon_end_far_range));
        strcat(line, str);
      }
    }
  }
  strip_end_whitesp_inplace(line);
  fprintf(fp, "%s\n", line);
  FCLOSE(fp);

  // Clean up
  FREE(header);
  FREE(line);
  meta_free(meta);
  FREE(terrasar);

  return 1;
}


// Convert metadata to kml file
int terrasar2kml(char *inFile, char *outFile, int listFlag)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta;
  terrasar_meta *terrasar;
  char *line = (char *) MALLOC(sizeof(char)*1024);

  if (listFlag) {
    fpIn = FOPEN(inFile, "r");
    fpOut = FOPEN(outFile, "w");
    kml_header(fpOut);
    while (fgets(line, 1024, fpIn)) {
      strip_end_whitesp_inplace(line);
      asfPrintStatus("File: %s\n\n", line);
      terrasar = terrasar2meta(line);
      meta = terrasar2meta(terrasar);
      kml_entry(fpOut, meta, meta->general->basename);
      meta_free(meta);
      FREE(terrasar);
    }
    kml_footer(fpOut);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
  }
  else {
    terrasar = read_terrasar_meta(inFile);
    meta = terrasar2meta(terrasar);
    fpOut = FOPEN(outFile, "w");
    kml_header(fpOut);
    kml_entry(fpOut, meta, meta->general->basename);
    kml_footer(fpOut);
    FCLOSE(fpOut);
    meta_free(meta);
    FREE(terrasar);
  }

  return 1;
}



// Convert metadata to shapefile
static int convert_terrasar2shape(char *inFile, DBFHandle dbase, 
				  SHPHandle shape, int n)
{
  dbf_header_t *dbf;
  meta_parameters *meta;
  terrasar_meta *terrasar;
  double lat[5], lon[5];
  int ii, field=0, nCols;

  // Read metadata
  terrasar = read_terrasar_meta(inFile);
  meta = terrasar2meta(terrasar);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  // Determine corner coordinates
  if (meta->location) {
    lat[0] = meta->location->lat_start_near_range;
    lon[0] = meta->location->lon_start_near_range;
    lat[3] = meta->location->lat_start_far_range;
    lon[3] = meta->location->lon_start_far_range;
    lat[1] = meta->location->lat_end_near_range;
    lon[1] = meta->location->lon_end_near_range;
    lat[2] = meta->location->lat_end_far_range;
    lon[2] = meta->location->lon_end_far_range;
  }
  else {
    meta_get_latLon(meta, 0, 0, 0.0, &lat[0], &lon[0]);
    meta_get_latLon(meta, 0, ns, 0.0, &lat[1], &lon[1]);
    meta_get_latLon(meta, nl, ns, 0.0, &lat[2], &lon[2]);
    meta_get_latLon(meta, nl, 0, 0.0, &lat[3], &lon[3]);
  }
  lat[4] = lat[0];
  lon[4] = lon[0];

  // Read configuration file
  read_header_config("META", &dbf, &nCols);

  // Write information into database file
  for (ii=0; ii<nCols; ii++) {
    // General block
    if (strncmp(dbf[ii].header, "meta.general.basename", 21) == 0 &&
    dbf[ii].visible) {
      char *str = (char *)
        MALLOC(sizeof(char)*strlen(meta->general->basename));
      strcpy(str, meta->general->basename);
      DBFWriteStringAttribute(dbase, n, field, str);
      FREE(str);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.sensor", 19) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->sensor);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.sensor_name", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->sensor_name);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.mode", 17) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->mode);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.processor", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->processor);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.data_type", 22) == 0 &&
             dbf[ii].visible) {
      char data_type[25];
      if (meta->general->data_type == BYTE)
        strcpy(data_type, "BYTE");
      else if (meta->general->data_type == INTEGER16)
        strcpy(data_type, "INTEGER16");
      else if (meta->general->data_type == INTEGER32)
        strcpy(data_type, "INTEGER32");
      else if (meta->general->data_type == REAL32)
        strcpy(data_type, "REAL32");
      else if (meta->general->data_type == REAL64)
        strcpy(data_type, "REAL64");
      else if (meta->general->data_type == COMPLEX_BYTE)
        strcpy(data_type, "COMPLEX_BYTE");
      else if (meta->general->data_type == COMPLEX_INTEGER16)
        strcpy(data_type, "COMPLEX_INTEGER16");
      else if (meta->general->data_type == COMPLEX_INTEGER32)
        strcpy(data_type, "COMPLEX_INTEGER32");
      else if (meta->general->data_type == COMPLEX_REAL32)
        strcpy(data_type, "COMPLEX_REAL32");
      else if (meta->general->data_type == COMPLEX_REAL64)
        strcpy(data_type, "COMPLEX_REAL64");
      DBFWriteStringAttribute(dbase, n, field, data_type);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.image_data_type", 27) == 0 
	     && dbf[ii].visible) {
      char image_data_type[25];
      if (meta->general->image_data_type == RAW_IMAGE)
        strcpy(image_data_type, "RAW_IMAGE");
      else if (meta->general->image_data_type == COMPLEX_IMAGE)
        strcpy(image_data_type, "COMPLEX_IMAGE");
      else if (meta->general->image_data_type == AMPLITUDE_IMAGE)
        strcpy(image_data_type, "AMPLITUDE_IMAGE");
      else if (meta->general->image_data_type == PHASE_IMAGE)
        strcpy(image_data_type, "PHASE_IMAGE");
      else if (meta->general->image_data_type == INTERFEROGRAM)
        strcpy(image_data_type, "INTERFEROGRAM");
      else if (meta->general->image_data_type == COHERENCE_IMAGE)
        strcpy(image_data_type, "COHERENCE_IMAGE");
      else if (meta->general->image_data_type == POLARIMETRIC_IMAGE)
	strcpy(image_data_type, "POLARIMETRIC_IMAGE");
      else if (meta->general->image_data_type == POLARIMETRIC_SEGMENTATION)
        strcpy(image_data_type, "POLARIMETRIC_SEGMENTATION");
      else if (meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
        strcpy(image_data_type, "POLARIMETRIC_DECOMPOSITION");
      else if (meta->general->image_data_type == POLARIMETRIC_PARAMETER)
        strcpy(image_data_type, "POLARIMETRIC_PARAMETER");
      else if (meta->general->image_data_type == POLARIMETRIC_C2_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C2_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_C3_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C3_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_C4_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C4_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_T3_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_T3_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_T4_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_T4_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_STOKES_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_STOKES_MATRIX");
      else if (meta->general->image_data_type == LUT_IMAGE)
        strcpy(image_data_type, "LUT_IMAGE");
      else if (meta->general->image_data_type == ELEVATION)
        strcpy(image_data_type, "ELEVATION");
      else if (meta->general->image_data_type == DEM)
        strcpy(image_data_type, "DEM");
      else if (meta->general->image_data_type == IMAGE)
        strcpy(image_data_type, "IMAGE");
      else if (meta->general->image_data_type == MASK)
        strcpy(image_data_type, "MASK");
      DBFWriteStringAttribute(dbase, n, field, image_data_type);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.radiometry", 22) == 0 &&
             dbf[ii].visible) {
      char radiometry[20];
      if (meta->general->radiometry == r_AMP)
        strcpy(radiometry, "AMPLITUDE");
      else if (meta->general->radiometry == r_SIGMA)
        strcpy(radiometry, "SIGMA");
      else if (meta->general->radiometry == r_BETA)
        strcpy(radiometry, "BETA");
      else if (meta->general->radiometry == r_GAMMA)
        strcpy(radiometry, "GAMMA");
      else if (meta->general->radiometry == r_SIGMA_DB)
        strcpy(radiometry, "SIGMA_DB");
      else if (meta->general->radiometry == r_BETA_DB)
        strcpy(radiometry, "BETA_DB");
      else if (meta->general->radiometry == r_GAMMA_DB)
        strcpy(radiometry, "GAMMA_DB");
      else if (meta->general->radiometry == r_POWER)
        strcpy(radiometry, "POWER");
      DBFWriteStringAttribute(dbase, n, field, radiometry);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.acquisition_date", 29) == 0 
	     && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field,
                  meta->general->acquisition_date);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.orbit_direction", 27) == 0 
	     && dbf[ii].visible) {
      char orbit_direction[15];
      if (meta->general->orbit_direction == 'A')
    strcpy(orbit_direction, "Ascending");
      else
       strcpy(orbit_direction, "Descending");
      DBFWriteStringAttribute(dbase, n, field, orbit_direction);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.orbit", 17) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->orbit);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.frame", 18) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->frame);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.band_count", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->band_count);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.bands", 17) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->bands);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.line_count", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->line_count);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.sample_count", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->sample_count);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.start_line", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->start_line);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.start_sample", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->start_sample);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.x_pixel_size", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->x_pixel_size);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.y_pixel_size", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->y_pixel_size);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.center_latitude", 27) == 0 
	     && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->center_latitude);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.center_longitude", 28) == 0 
	     && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field,
                  meta->general->center_longitude);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.re_major", 20) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->re_major);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.re_minor", 20) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->re_minor);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.bit_error_rate", 26) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->bit_error_rate);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.missing_lines", 25) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->missing_lines);
      field++;
    }
    else if (strncmp(dbf[ii].header, "meta.general.no_data", 19) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->no_data);
      field++;
    }
  }
  for (ii=0; ii<nCols; ii++) {

    // SAR block
    if (meta->sar) {
      int kk;
      char str[12];
      if (strncmp(dbf[ii].header, "meta.sar.image_type", 19) == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%c", meta->sar->image_type);
        DBFWriteStringAttribute(dbase, n, field, str);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.look_direction", 23) == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%c", meta->sar->look_direction);
        DBFWriteStringAttribute(dbase, n, field, str);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.look_count", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->sar->look_count);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.deskewed", 17) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->sar->deskewed);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.original_line_count", 28) == 0
	       && dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                                 meta->sar->original_line_count);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.sar.original_sample_count", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                                 meta->sar->original_sample_count);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.line_increment", 23) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->line_increment);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.sample_increment", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->sample_increment);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.sar.range_time_per_pixel", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_time_per_pixel);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.sar.azimuth_time_per_pixel", 31) == 0 && 
	       dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_time_per_pixel);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.slant_shift", 20) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->slant_shift);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.time_shift", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->time_shift);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.sar.slant_range_first_pixel", 32) == 0 && 
	       dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->slant_range_first_pixel);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.wavelength", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->wavelength);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.prf", 12) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->prf);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.earth_radius", 21) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->earth_radius);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.earth_radius_pp", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->earth_radius_pp);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.satellite_height", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->satellite_height);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.sar.satellite_binary_time", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->sar->satellite_binary_time);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.sar.satellite_clock_time", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->sar->satellite_clock_time);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.sar.range_doppler_coefficients", 35) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_doppler_coefficients[0]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_doppler_coefficients[1]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_doppler_coefficients[2]);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.sar.azimuth_doppler_coefficients", 37) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_doppler_coefficients[0]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_doppler_coefficients[1]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_doppler_coefficients[2]);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.sar.azimuth_processing_bandwidth", 37) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_processing_bandwidth);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.chirp_rate", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->chirp_rate);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.pulse_duration", 23) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->pulse_duration);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.range_sampling_rate", 28) == 0
	       && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_sampling_rate);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.polarization", 21) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field, meta->sar->polarization);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.multilook", 18) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->sar->multilook);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.pitch", 14) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->pitch);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.roll", 13) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->roll);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.yaw", 12) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->yaw);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.sar.incid_a", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<6; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->sar->incid_a[kk]);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Optical block
    if (meta->optical) {
      if (strncmp(dbf[ii].header, "meta.optical.pointing_direction", 31) == 0 
	  && dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->optical->pointing_direction);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.optical.off_nadir_angle", 28) == 0
	       && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->off_nadir_angle);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.optical.correction_level", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->optical->correction_level);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.optical.cloud_percentage", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->cloud_percentage);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.optical.sun_azimuth_angle", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->sun_azimuth_angle);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.optical.sun_elevation_angle", 32) == 0
               && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->sun_elevation_angle);
        field++;
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Thermal block
    if (meta->thermal) {
      if (strncmp(dbf[ii].header, "meta.thermal.band_gain", 22) == 0 &&
          dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->thermal->band_gain);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.thermal.band_gain_change", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->thermal->band_gain_change);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.thermal.day", 16) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->thermal->day);
        field++;
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Transform block
    if (meta->transform) {
      int kk;
      if (strncmp(dbf[ii].header, "meta.transform.parameter_count", 30) == 0 &&
          dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                 meta->transform->parameter_count);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.transform.x", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field, meta->transform->x[kk]);
          field++;
        }
      }
      else if (strncmp(dbf[ii].header, "meta.transform.y", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field, meta->transform->y[kk]);
          field++;
        }
      }
      else if (strncmp(dbf[ii].header, "meta.transform.l", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field, meta->transform->l[kk]);
          field++;
        }
      }
      else if (strncmp(dbf[ii].header, "meta.transform.s", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<meta->transform->parameter_count; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field, meta->transform->s[kk]);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // AirSAR block
    if (meta->airsar) {
      if (strncmp(dbf[ii].header, "meta.airsar.scale_factor", 24) == 0 &&
          dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->scale_factor);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.airsar.gps_altitude", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->gps_altitude);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.airsar.lat_peg_point", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->lat_peg_point);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.airsar.lon_peg_point", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->lon_peg_point);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.airsar.head_peg_point", 26) == 0 
	       && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->head_peg_point);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.airsar.along_track_offset", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->airsar->along_track_offset);
        field++;
      }
      else if (strncmp(dbf[ii].header, 
		       "meta.airsar.cross_track_offset", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->airsar->cross_track_offset);
        field++;
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Projection block
    if (meta->projection) {
      if (strncmp(dbf[ii].header, "meta.projection.type", 20) == 0 &&
      dbf[ii].visible) {
    char type[50];
        if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR)
          strcpy(type, "UNIVERSAL TRANSVERSE MERCATOR");
        else if (meta->projection->type == POLAR_STEREOGRAPHIC)
          strcpy(type, "POLAR STEREOGRAPHIC");
        else if (meta->projection->type == ALBERS_EQUAL_AREA)
          strcpy(type, "ALBERS EQUAL AREA");
        else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC)
          strcpy(type, "LAMBERT CONFORMAL CONIC");
        else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
          strcpy(type, "LAMBERT AZIMUTHAL EQUAL AREA");
        else if (meta->projection->type == STATE_PLANE)
          strcpy(type, "STATE PLANE");
        else if (meta->projection->type == SCANSAR_PROJECTION)
          strcpy(type, "SCANSAR PROJECTION");
        else if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION)
          strcpy(type, "LAT LONG PSEUDO PROJECTION");
        else if (meta->projection->type == UNKNOWN_PROJECTION)
          strcpy(type, "UNKNOWN PROJECTION");
        DBFWriteStringAttribute(dbase, n, field, type);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.startX", 22) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->startX);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.startY", 22) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->startY);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.perX", 20) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->perX);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.perY", 20) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->perY);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.units", 21) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field, meta->projection->units);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.hem", 19) == 0 &&
               dbf[ii].visible) {
        char hemisphere[15];
        if (meta->projection->hem == 'N')
          strcpy(hemisphere, "North");
        else if (meta->projection->hem == 'S')
          strcpy(hemisphere, "South");
        DBFWriteStringAttribute(dbase, n, field, hemisphere);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.spheroid", 24) == 0 &&
               dbf[ii].visible) {
        char spheroid[25];
        if (meta->projection->spheroid == BESSEL_SPHEROID)
          strcpy(spheroid, "BESSEL");
        else if (meta->projection->spheroid == CLARKE1866_SPHEROID)
          strcpy(spheroid, "CLARKE1866");
        else if (meta->projection->spheroid == CLARKE1880_SPHEROID)
          strcpy(spheroid, "CLARKE1880");
        else if (meta->projection->spheroid == GEM6_SPHEROID)
          strcpy(spheroid, "GEM6");
        else if (meta->projection->spheroid == GEM10C_SPHEROID)
          strcpy(spheroid, "GEM10C");
        else if (meta->projection->spheroid == GRS1980_SPHEROID)
          strcpy(spheroid, "GRS1980");
        else if (meta->projection->spheroid == INTERNATIONAL1924_SPHEROID)
          strcpy(spheroid, "INTERNATIONAL1924");
        else if (meta->projection->spheroid == INTERNATIONAL1967_SPHEROID)
          strcpy(spheroid, "INTERNATIONAL1967");
        else if (meta->projection->spheroid == WGS72_SPHEROID)
          strcpy(spheroid, "WGS72");
        else if (meta->projection->spheroid == WGS84_SPHEROID)
          strcpy(spheroid, "WGS84");
        else if (meta->projection->spheroid == HUGHES_SPHEROID)
          strcpy(spheroid, "HUGHES");
        else
          strcpy(spheroid, "UNKNOWN");
        DBFWriteStringAttribute(dbase, n, field, spheroid);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.re_major", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->re_major);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.re_minor", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->re_minor);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.datum", 21) == 0 &&
               dbf[ii].visible) {
        char datum[25];
        if (meta->projection->datum == EGM96_DATUM)
          strcpy(datum, "EGM96");
        else if (meta->projection->datum == ED50_DATUM)
          strcpy(datum, "ED50");
        else if (meta->projection->datum == ETRF89_DATUM)
          strcpy(datum, "ETRF89");
        else if (meta->projection->datum == ETRS89_DATUM)
          strcpy(datum, "ETRS89");
        else if (meta->projection->datum == ITRF97_DATUM)
          strcpy(datum, "ITRF97");
        else if (meta->projection->datum == NAD27_DATUM)
          strcpy(datum, "NAD27");
        else if (meta->projection->datum == NAD83_DATUM)
          strcpy(datum, "NAD83");
        else if (meta->projection->datum == WGS72_DATUM)
          strcpy(datum, "WGS72");
        else if (meta->projection->datum == WGS84_DATUM)
          strcpy(datum, "WGS84");
        else if (meta->projection->datum == HUGHES_DATUM)
          strcpy(datum, "HUGHES");
        else
          strcpy(datum, "UNKNOWN");
        DBFWriteStringAttribute(dbase, n, field, datum);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.projection.height",22) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->height);
        field++;
      }
      if (meta->projection->type == ALBERS_EQUAL_AREA) {
        if (strncmp(dbf[ii].header,
		    "meta.projection.param.albers.std_parallel1", 42) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.std_parallel1);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.albers.std_parallel2", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.std_parallel2);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.albers.center_meridian", 
			 44) == 0 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.center_meridian);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.albers.orig_latitude", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.orig_latitude);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.albers.false_easting", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.albers.false_northing", 
			 43) == 0 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.false_northing);
          field++;
        }
      }
      else if (meta->projection->type == SCANSAR_PROJECTION) {
        if (strncmp(dbf[ii].header, 
		    "meta.projection.param.atct.rlocal", 33) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.rlocal);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.atct.alpha1", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.alpha1);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.atct.alpha2", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.alpha2);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.atct.alpha3", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.alpha3);
          field++;
        }
      }
      else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
        if (strncmp(dbf[ii].header,
		    "meta.projection.param.lamaz.center_lat", 38) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.center_lat);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamaz.center_lon", 38) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.center_lon);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamaz.false_easting", 41) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamaz.false_northing", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.false_northing);
          field++;
        }
      }
      else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC) {
        if (strncmp(dbf[ii].header, 
		    "meta.projection.param.lamcc.plat1", 33) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.plat1);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamcc.plat2", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.plat2);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamcc.lat0", 32) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.lat0);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamcc.lon0", 32) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.lon0);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamcc.false_easting", 41) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamcc.false_northing", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.false_northing);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.lamcc.scale_factor", 40) == 0 
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.scale_factor);
          field++;
        }
      }
      else if (meta->projection->type == POLAR_STEREOGRAPHIC) {
        if (strncmp(dbf[ii].header, "meta.projection.param.ps.slat", 29) == 0 
	    && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.slat);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.ps.slon", 29) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.slon);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.ps.false_easting", 38) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.ps.false_northing", 39) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.false_northing);
          field++;
        }
      }
      else if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
        if (strncmp(dbf[ii].header, "meta.projection.param.utm.zone", 30) == 0 
	    && dbf[ii].visible) {
          DBFWriteIntegerAttribute(dbase, n, field,
                                   meta->projection->param.utm.zone);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.utm.false_easting", 39) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.utm.false_northing", 40) == 0 
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.false_northing);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.utm.lat0", 30) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.lat0);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.utm.lon0", 30) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.lon0);
          field++;
        }
        else if (strncmp(dbf[ii].header,
			 "meta.projection.param.utm.scale_factor", 38) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.scale_factor);
          field++;
        }
      }
      else if (meta->projection->type == STATE_PLANE) {
        if (strncmp(dbf[ii].header, 
		    "meta.projection.param.state.zone", 32) == 0 &&
            dbf[ii].visible) {
          DBFWriteIntegerAttribute(dbase, n, field,
                                   meta->projection->param.state.zone);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Stats block
    if (meta->stats) {
      int kk;
      if (strcmp(dbf[ii].header, "meta.stats.band_count") == 0 &&
          dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->stats->band_count);
        field++;
      }
      for (kk=0; kk<meta->stats->band_count; kk++) {
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.band_id") == 0 &&
            dbf[ii].visible) {
          DBFWriteStringAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].band_id);
          field++;
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.min") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].min);
          field++;
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.max") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].max);
          field++;
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.mean") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].mean);
          field++;
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.rmse") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].rmse);
          field++;
        }
        if (strcmp(dbf[ii].header,
                   "meta.stats.band_stats.std_deviation") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].std_deviation);
          field++;
        }
        if (strcmp(dbf[ii].header, "meta.stats.band_stats.mask") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].mask);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // State vector block
    if (meta->state_vectors) {
      int kk;
      int n = meta->state_vectors->vector_count;
      if (strncmp(dbf[ii].header, "meta.state.year", 15) == 0 &&
          dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->state_vectors->year);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.state.julDay", 17) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->state_vectors->julDay);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.state.second", 17) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->state_vectors->second);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.state.vector_count", 23) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                                 meta->state_vectors->vector_count);
        field++;
      }
      else if (strncmp(dbf[ii].header, "meta.state.vectors", 18) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].time);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.pos.x);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.pos.y);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.pos.z);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.vel.x);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.vel.y);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.vel.z);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Location block
    if (meta->location) {
      if (strncmp(dbf[ii].header, 
		  "meta.location.lat_start_near_range", 34) == 0 &&
          dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->location->lat_start_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lon_start_near_range", 34) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_start_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lat_start_far_range", 33) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lat_start_far_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lon_start_far_range", 33) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_start_far_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lat_end_near_range", 32) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lat_end_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lon_end_near_range", 32) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_end_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lat_end_far_range", 31) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lat_end_far_range);
        field++;
      }
      else if (strncmp(dbf[ii].header,
		       "meta.location.lon_end_far_range", 31) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_end_far_range);
        field++;
      }
    }
  }

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Clean up
  meta_free(meta);

  return 1;
}

// Convert metadata to shapefile
int terrasar2shape(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  meta_parameters *meta;
  terrasar_meta *terrasar;
  char line[1024], metaFile[1024];
  int n=0;

  // Initialize the shape file.
  if (listFlag) {
    fp = FOPEN(inFile, "r");
    fgets(line, 1024, fp);
    strip_end_whitesp_inplace(line);
    strcpy(metaFile, line);
    FCLOSE(fp);
  }
  else
    strcpy(metaFile, inFile);
  terrasar = read_terrasar_meta(metaFile);
  meta = terrasar2meta(terrasar);
  shape_meta_init(outFile, meta);
  open_shape(outFile, &dbase, &shape);
  meta_free(meta);
  FREE(terrasar);
  
  if (listFlag) {
    fp = FOPEN(inFile, "r");
    while (fgets(line, 1024, fp)) {
      strip_end_whitesp_inplace(line);
      convert_terrasar2shape(line, dbase, shape, n);
      n++;
    }
    FCLOSE(fp);
  }
  else
    convert_terrasar2shape(inFile, dbase, shape, 0);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return 1;
}

// Convert leader file to metadata - just a simple read and write actually
int write_terrasar2meta(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  meta_parameters *meta;
  terrasar_meta *terrasar;
  char *line = (char *) MALLOC(sizeof(char)*1024);
  int n=0;

  if (listFlag) {
    fp = FOPEN(inFile, "r");
    while (fgets(line, 1024, fp)) {
      strip_end_whitesp_inplace(line);
      terrasar = read_terrasar_meta(inFile);
      meta = terrasar2meta(terrasar);
      meta_write(meta, outFile);
      FREE(terrasar);
      n++;
    }
    FCLOSE(fp);
  }
  else {
    terrasar = read_terrasar_meta(inFile);
    meta = terrasar2meta(terrasar);
    meta_write(meta, outFile);
    FREE(terrasar);
  }

  return 1;
}
