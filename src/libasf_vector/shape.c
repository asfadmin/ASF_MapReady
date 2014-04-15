#include "shapefil.h"
#include "spheroids.h"
#include "asf_vector.h"
#include "libasf_proj.h"
#include "asf.h"

void shapefile_init(char *inFile, char *format, meta_parameters *meta)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape = NULL;

  // Read configuration file
  dbf_header_t *dbf;
  int ii, nCols;
  char shape_type[25];
  if (!read_header_config(format, &dbf, &nCols, shape_type))
    asfPrintError("Could not find format (%s) information\n", format);
  if (strcmp_case(shape_type, "UNKNOWN") == 0)
    asfPrintError("Unknown shape type! Needs to be either 'POLYGON' or 'POINT'!"
      "\n");

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  if (meta) {
    for (ii=0; ii<nCols; ii++) {

      // General block
      if (meta->general) {
        if (strcmp_case(dbf[ii].meta, "meta.general.basename") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.sensor") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.sensor_name") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.mode") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, 
          "meta.general.receiving_station") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.processor") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.data_type") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.image_data_type") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.radiometry") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, 
          "meta.general.acquisition_date") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.orbit") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.orbit_direction") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.frame") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.band_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.bands") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.line_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.sample_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.start_line") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.start_sample") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.x_pixel_size") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.y_pixel_size") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.center_latitude") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.center_longitude") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.re_major") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.re_minor") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.bit_error_rate") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.missing_lines") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        else if (strcmp_case(dbf[ii].meta, "meta.general.no_data") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
      }
      // SAR block
      if (meta->sar) {
        if (strcmp_case(dbf[ii].meta, "meta.sar.image_type") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.look_direction") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.azimuth_look_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.range_look_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.deskewed") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.original_line_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.original_sample_count") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.line_increment") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.sample_increment") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.range_time_per_pixel") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.azimuth_time_per_pixel") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.slant_shift") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.time_shift") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.slant_range_first_pixel") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.wavelength") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.prf") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.earth_radius") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.earth_radius_pp") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.satellite_height") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.satellite_binary_time") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.satellite_clock_time") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.range_doppler_coefficients[0]") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.range_doppler_coefficients[1]") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.range_doppler_coefficients[2]") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.azimuth_doppler_coefficients[0]") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.azimuth_doppler_coefficients[1]") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.azimuth_doppler_coefficients[2]") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.sar.azimuth_processing_bandwidth") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.chirp_rate") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.pulse_duration") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.range_sampling_rate") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.polarization") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.multilook") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' integer field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.pitch") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.roll") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.sar.yaw") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
      }
      // Optical block
      if (meta->optical) {
        if (strcmp_case(dbf[ii].meta, "meta.optical.pointing_direction") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.optical.off_nadir_angle") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.optical.correction_level") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.optical.cloud_percentage") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.optical.sun_azimuth_angle") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.optical.sun_elevation_angle") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
      }
      // AirSAR block
      if (meta->airsar) {
        if (strcmp_case(dbf[ii].meta, "meta.airsar.scale_factor") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.airsar.gps_altitude") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.airsar.lat_peg_point") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.airsar.lon_peg_point") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.airsar.head_peg_point") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.airsar.along_track_offset") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.airsar.cross_track_offset") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
      }
      // UAVSAR block
      if (meta->uavsar) {
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.id") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.scale_factor") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.gps_altitude") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.lat_peg_point") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.lon_peg_point") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.head_peg_point") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.along_track_offset") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.uavsar.cross_track_offset") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
      }
      // Map projection block
      if (meta->projection && meta->projection->type != SCANSAR_PROJECTION) {
        if (strcmp_case(dbf[ii].meta, "meta.projection") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' string field to database file\n",
              dbf[ii].meta);
        }
      }
      // Location block
      if (meta->location) {
        if (strcmp_case(dbf[ii].meta, 
          "meta.location.lat_start_near_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.location.lon_start_near_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.location.lat_start_far_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.location.lon_start_far_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.location.lat_end_near_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, 
          "meta.location.lon_end_near_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.location.lat_end_far_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
        if (strcmp_case(dbf[ii].meta, "meta.location.lon_end_far_range") == 0) {
          if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
            dbf[ii].decimals) == -1)
            asfPrintError("Could not add '%s' double field to database file\n",
              dbf[ii].meta);
        }
      }   
    }
  }
  else {
    // Add fields to database
    for (ii=0; ii<nCols; ii++) {
      if (dbf[ii].format == DBF_STRING) {
        if (DBFAddField(dbase, dbf[ii].shape, FTString, dbf[ii].length, 
          dbf[ii].decimals) == -1)
          asfPrintError("Could not add '%s' string field to database file\n",
            dbf[ii].meta);
      }
      else if (dbf[ii].format == DBF_DOUBLE) {
        if (DBFAddField(dbase, dbf[ii].shape, FTDouble, dbf[ii].length, 
          dbf[ii].decimals) == -1)
          asfPrintError("Could not add '%s' double field to database file\n",
            dbf[ii].meta);
      }
      else if (dbf[ii].format == DBF_INTEGER) {
        if (DBFAddField(dbase, dbf[ii].shape, FTInteger, dbf[ii].length, 
          dbf[ii].decimals) == -1)
          asfPrintError("Could not add '%s' integer field to database file\n",
            dbf[ii].meta);
      }
    }
  }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  if (strcmp_case(shape_type, "POINT") == 0)
    shape = SHPCreate(inFile, SHPT_POINT);
  else if (strcmp_case(shape_type, "POLYGON") == 0)
    shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

// Initialize internal format such as RGPS and MULTIMATCH
void shape_init(char *inFile, format_type_t format)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape;

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+5));
  sprintf(dbaseFile, "%s.dbf", inFile);
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  if (format == RGPS) {
    if (DBFAddField(dbase, "Cell_ID", FTInteger, 6, 0) == -1)
      asfPrintError("Could not add 'Cell_ID' field to database file\n");
    if (DBFAddField(dbase, "Vertices", FTInteger, 2, 0) == -1)
      asfPrintError("Could not add 'Vertices' field to database file\n");
    if (DBFAddField(dbase, "Date", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'Date' field to database file\n");
    if (DBFAddField(dbase, "SrcImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'SrcImage' field to database file\n");
    if (DBFAddField(dbase, "TrgImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'TrgImage' field to database file\n");
    if (DBFAddField(dbase, "Stream", FTString, 3, 0) == -1)
      asfPrintError("Could not add 'Stream' field to database file\n");
    if (DBFAddField(dbase, "Area", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Area' field to database file\n");
    if (DBFAddField(dbase, "MY_ice", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'MY_ice' field to database file\n");
    if (DBFAddField(dbase, "OpenWater", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'OpenWater' field to database file\n");
    if (DBFAddField(dbase, "IncidAngle", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'IncidAngle' field to database file\n");
    if (DBFAddField(dbase, "Cell_x", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Cell_x' field to database file\n");
    if (DBFAddField(dbase, "Cell_y", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Cell_y' field to database file\n");
    if (DBFAddField(dbase, "dudx", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dudx' field to database file\n");
    if (DBFAddField(dbase, "dudy", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dudy' field to database file\n");
    if (DBFAddField(dbase, "dvdx", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dvdx' field to database file\n");
    if (DBFAddField(dbase, "dvdy", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dvdy' field to database file\n");
    if (DBFAddField(dbase, "dtp", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'dtp' field to database file\n");
    if (DBFAddField(dbase, "Temp", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Temp' field to database file\n");
    if (DBFAddField(dbase, "u_wind", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'u_wind' field to database file\n");
    if (DBFAddField(dbase, "v_wind", FTDouble, 12, 6) == -1)
      asfPrintError("Could not add 'v_wind' field to database file\n");
  }
  else if (format == RGPS_GRID) {
    if (DBFAddField(dbase, "Grid_ID", FTInteger, 6, 0) == -1)
      asfPrintError("Could not add 'Grid_ID' field to database file\n");
    if (DBFAddField(dbase, "Date", FTString, 12, 0) == -1)
      asfPrintError("Could not add 'Date' field to database file\n");
    if (DBFAddField(dbase, "Day", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Grid_x' field to database file\n");
    if (DBFAddField(dbase, "Grid_x", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Grid_x' field to database file\n");
    if (DBFAddField(dbase, "Grid_y", FTDouble, 12, 3) == -1)
      asfPrintError("Could not add 'Grid_y' field to database file\n");
    if (DBFAddField(dbase, "SrcImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'SrcImage' field to database file\n");
    if (DBFAddField(dbase, "TrgImage", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'TrgImage' field to database file\n");
    if (DBFAddField(dbase, "Stream", FTString, 3, 0) == -1)
      asfPrintError("Could not add 'Stream' field to database file\n");
    if (DBFAddField(dbase, "Quality", FTInteger, 3, 0) == -1)
      asfPrintError("Could not add 'Quality' field to database file\n");
  }
  else if (format == RGPS_WEATHER) {
    if (DBFAddField(dbase, "Date", FTString, 12, 0) == -1)
      asfPrintError("Could not add 'Date' field to database file\n");
    if (DBFAddField(dbase, "Lat", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Lat' field to database file\n");
    if (DBFAddField(dbase, "Lon", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Lon' field to database file\n");
    if (DBFAddField(dbase, "Direction", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Direction' field to database file\n");
    if (DBFAddField(dbase, "Speed", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'Speed' field to database file\n");
    if (DBFAddField(dbase, "Temp", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'Temp' field to database file\n");
    if (DBFAddField(dbase, "Pressure", FTDouble, 6, 1) == -1)
      asfPrintError("Could not add 'Pressure' field to database file\n");
  }
  else if (format == MULTIMATCH) {
    if (DBFAddField(dbase, "Ref_x", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_x' field to database file\n");
    if (DBFAddField(dbase, "Ref_y", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_y' field to database file\n");
    if (DBFAddField(dbase, "Ref_z", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_z' field to database file\n");
    if (DBFAddField(dbase, "Search_x", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_x' field to database file\n");
    if (DBFAddField(dbase, "Search_y", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_y' field to database file\n");
    if (DBFAddField(dbase, "Search_z", FTDouble, 9, 2) == -1)
      asfPrintError("Could not add 'Ref_z' field to database file\n");
    if (DBFAddField(dbase, "dx", FTDouble, 7, 2) == -1)
      asfPrintError("Could not add 'dx' field to database file\n");
    if (DBFAddField(dbase, "dy", FTDouble, 7, 2) == -1)
      asfPrintError("Could not add 'dy' field to database file\n");
    if (DBFAddField(dbase, "dh", FTDouble, 7, 3) == -1)
      asfPrintError("Could not add 'dh' field to database file\n");
    if (DBFAddField(dbase, "Direction", FTDouble, 9, 4) == -1)
      asfPrintError("Could not add 'Direction' field to database file\n");
    if (DBFAddField(dbase, "Speed", FTDouble, 6, 1) == -1)
      asfPrintError("Could not add 'Speed' field to database file\n");
  }
  else if (format == GRANULE_COUNT) {
    if (DBFAddField(dbase, "FRAMES", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAMES' field to database file\n");
  }
  else if (format == GRANULE_LIST) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "FRAMES", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAMES' field to database file\n");
  }
  else if (format == GRANULE_DETAILS_A3) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "OFF_NADIR", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'OFF_NADIR' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "DATE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'DATE' field to database file\n");
    if (DBFAddField(dbase, "NSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "NSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LON field to database file\n");
    if (DBFAddField(dbase, "FSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "FSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LON field to database file\n");
    if (DBFAddField(dbase, "N_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LAT field to database file\n");
    if (DBFAddField(dbase, "N_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LON field to database file\n");
    if (DBFAddField(dbase, "F_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LAT field to database file\n");
    if (DBFAddField(dbase, "F_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LON field to database file\n");
  }
  else if (format == GRANULE_DETAILS) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "DATE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'DATE' field to database file\n");
    if (DBFAddField(dbase, "NSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "NSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LON field to database file\n");
    if (DBFAddField(dbase, "FSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "FSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LON field to database file\n");
    if (DBFAddField(dbase, "N_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LAT field to database file\n");
    if (DBFAddField(dbase, "N_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LON field to database file\n");
    if (DBFAddField(dbase, "F_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LAT field to database file\n");
    if (DBFAddField(dbase, "F_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LON field to database file\n");
  }
  else if (format == FOOT_PRINT) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "FRAME_CNT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'FRAME_CNT' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "OFF_NADIR", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'OFF_NADIR' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "DATE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'DATE' field to database file\n");
    if (DBFAddField(dbase, "ORBIT_DIR", FTString, 15, 0) == -1)
      asfPrintError("Could not add 'ORBIT_DIR' field to database file\n");
    if (DBFAddField(dbase, "PATH", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'PATH' field to database file\n");
    if (DBFAddField(dbase, "CENTER_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add 'CENTER_LAT' field to database file\n");
    if (DBFAddField(dbase, "CENTER_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add 'CENTER_LON' field to database file\n");
    if (DBFAddField(dbase, "NSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "NSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add NSTART_LON field to database file\n");
    if (DBFAddField(dbase, "FSTART_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LAT field to database file\n");
    if (DBFAddField(dbase, "FSTART_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add FSTART_LON field to database file\n");
    if (DBFAddField(dbase, "N_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LAT field to database file\n");
    if (DBFAddField(dbase, "N_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add N_END_LON field to database file\n");
    if (DBFAddField(dbase, "F_END_LAT", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LAT field to database file\n");
    if (DBFAddField(dbase, "F_END_LON", FTDouble, 10, 4) == -1)
      asfPrintError("Could not add F_END_LON field to database file\n");
  }
  else if (format == GRANULE) {
    if (DBFAddField(dbase, "STACK_ID", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'STACK_ID' field to database file\n");
    if (DBFAddField(dbase, "GRANULE", FTString, 20, 0) == -1)
      asfPrintError("Could not add 'GRANULE' field to database file\n");
    if (DBFAddField(dbase, "SATELLITE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'SATELLITE' field to database file\n");
    if (DBFAddField(dbase, "BEAM_MODE", FTString, 5, 0) == -1)
      asfPrintError("Could not add 'BEAM_MODE' field to database file\n");
    if (DBFAddField(dbase, "OFF_NADIR", FTDouble, 5, 1) == -1)
      asfPrintError("Could not add 'OFF_NADIR' field to database file\n");
    if (DBFAddField(dbase, "ORBIT", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'ORBIT' field to database file\n");
    if (DBFAddField(dbase, "FRAME", FTInteger, 4, 0) == -1)
      asfPrintError("Could not add 'FRAME' field to database file\n");
    if (DBFAddField(dbase, "ACQ_DATE", FTString, 25, 0) == -1)
      asfPrintError("Could not add 'ACQ_DATE' field to database file\n");
    if (DBFAddField(dbase, "ORBIT_DIR", FTString, 15, 0) == -1)
      asfPrintError("Could not add 'ORBIT_DIR' field to database file\n");
    if (DBFAddField(dbase, "PATH", FTInteger, 5, 0) == -1)
      asfPrintError("Could not add 'PATH' field to database file\n");
    if (DBFAddField(dbase, "TERRAIN", FTInteger, 2, 0) == -1)
      asfPrintError("Could not add 'TERRAIN' field to database file\n");
    if (DBFAddField(dbase, "INSAR", FTInteger, 2, 0) == -1)
      asfPrintError("Could not add 'INSAR' field to database file\n");
  }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  if (format == POINT || format == RGPS_GRID || format == RGPS_WEATHER ||
      format == MULTIMATCH)
    shape = SHPCreate(inFile, SHPT_POINT);
  else
    shape = SHPCreate(inFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);

  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

void open_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape)
{
  char *dbaseFile;

  if (dbase) {
    // Open database for adding values
    dbaseFile = appendExt(inFile, ".dbf");
    *dbase = DBFOpen(dbaseFile, "r+b");
    if (*dbase == NULL)
      asfPrintError("Could not open database file '%s'\n", dbaseFile);
    FREE(dbaseFile);
  }

  if (shape) {
    // Create a copy of the name to use for opening
    char tmpInFile[1024];
    strcpy(tmpInFile, inFile);
    char *ext = findExt(inFile);
    if (!ext) {
      // KLUDGE ALERT!  SHPOpen() below replaces the file extension in inFile
      // by searching from the end of the filename in reverse for a '.'
      // character, then appends .shx and .shp to the two filenames that it
      // produces ...Unfortunately, this results in truncated ALOS basenames
      // in the output files AND we don't own the shape library.
      
      // So, since SHPOpen() always wants to strip off an extension, we
      // add an extension for it to strip in cases where one isn't already
      // there.  This will solve the ALOS naming problem.
      sprintf(tmpInFile, "%s.dummy", inFile);
    }

    // Open shapefile for adding values
    *shape = SHPOpen(tmpInFile, "r+b");
    if (*shape == NULL)
      asfPrintError("Could not open shapefile '%s'\n", inFile);
  }
}

void write_shape_attributes(DBFHandle dbase, int nAttr, int n,
  dbf_header_t *dbf)
{
  int ii;

  // Write fields into the database
  for (ii=0; ii<nAttr; ii++) {
    if (dbf[ii].format == DBF_STRING)
      DBFWriteStringAttribute(dbase, n, ii, dbf[ii].sValue);
    else if (dbf[ii].format == DBF_INTEGER)
      DBFWriteIntegerAttribute(dbase, n, ii, dbf[ii].nValue);
    else if (dbf[ii].format == DBF_DOUBLE)
      DBFWriteDoubleAttribute(dbase, n, ii, dbf[ii].fValue);
  }
} 

void write_shape_object(SHPHandle shape, int nCoords, double *lat, double *lon)
{
  // Check whether we need to split up the polygon
  if (crosses_dateline(lon, 0, nCoords)) {
    int *start = (int *) MALLOC(sizeof(int)*2);
    double *mLat = (double *) MALLOC(sizeof(double)*(nCoords+5));
    double *mLon = (double *) MALLOC(sizeof(double)*(nCoords+5));

    split_polygon(lat, lon, nCoords, start, mLat, mLon);
    
    SHPObject *shapeObject=NULL;
    shapeObject = SHPCreateObject(SHPT_POLYGON, -1, 2, start, NULL, nCoords+5, 
      mLon, mLat, NULL, NULL);
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);

    FREE(mLat);
    FREE(mLon);
    FREE(start);
  }
  else {
    SHPObject *shapeObject=NULL;
    shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, nCoords, lon, lat, NULL);
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);
  }
}

void close_shape(DBFHandle dbase, SHPHandle shape)
{
  // Close database
  DBFClose(dbase);

  // Close shapefile
  SHPClose(shape);

  return;
}

static void write_dbase_field_to_kml(DBFHandle dbase, int record,
                     int field, FILE *fp)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, field, fieldName,
                  &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, field);
      fprintf(fp, "<strong>%s</strong>: %s <br>\n",
          fieldName, sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, field);
      fprintf(fp, "<strong>%s</strong>: %d <br>\n",
          fieldName, nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, field);
      sprintf(str, "<strong>%%s</strong>: %%%d.%df <br>\n",
          nWidth, nDecimals);
      fprintf(fp, str, fieldName, fValue);
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
}

static void write_name_field_to_kml(DBFHandle dbase, int record, FILE *fp)
{
  DBFFieldType dbaseType;
  char fieldName[25], str[50];
  int nWidth, nDecimals, nValue;
  double fValue;
  const char *sValue;

  dbaseType = DBFGetFieldInfo(dbase, 0, fieldName, &nWidth, &nDecimals);
  switch (dbaseType)
    {
    case FTString:
      sValue = DBFReadStringAttribute(dbase, record, 0);
      fprintf(fp, "<name>%s</name>\n", sValue);
      break;
    case FTInteger:
      nValue = DBFReadIntegerAttribute(dbase, record, 0);
      fprintf(fp, "<name>%d</name>\n", nValue);
      break;
    case FTDouble:
      fValue = DBFReadDoubleAttribute(dbase, record, 0);
      sprintf(str, "<name>%%%d.%df</name>\n", nWidth, nDecimals);
      fprintf(fp, str, fValue);
      break;
    case FTLogical:
    case FTInvalid:
      break;
    }
}

// Convert shape to kml file
int shape2kml(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, kk, ll, nEntities, nVertices, nParts, *part;
  int nFields, pointType;
  double *lat, *lon, *height, *min, *max, clat, clon;

  // Open shapefile
  open_shape(inFile, &dbase, &shape);

  // Open kmlfile
  fp = FOPEN(outFile, "w");
  kml_header(fp);

  // Extract the vital information out of the shapefile
  min = (double *) MALLOC(sizeof(double)*4);
  max = (double *) MALLOC(sizeof(double)*4);
  SHPGetInfo(shape, &nEntities, &pointType, min, max);
  switch (pointType)
    {
    // 2D shape types
    case SHPT_POLYGON:
    case SHPT_ARC:
    case SHPT_POINT:
      break;
    case SHPT_MULTIPOINT:
      asfPrintError("Conversion does not support shape type 'Multipoint'\n");
      break;
    // 3D shape types
    case SHPT_POLYGONZ:
    case SHPT_ARCZ:
    case SHPT_POINTZ:
      break;
    case SHPT_MULTIPOINTZ:
      asfPrintError("Conversion does not support shape type 'Multipoint'\n");
      break;
    // 2D + measure types
    case SHPT_POINTM:
      asfPrintError("Conversion does not support shape type 'PointM'\n");
      break;
    case SHPT_ARCM:
      asfPrintError("Conversion does not support shape type 'ArcM'\n");
      break;
    case SHPT_POLYGONM:
      asfPrintError("Conversion does not support shape type 'PolygonM'\n");
      break;
    case SHPT_MULTIPOINTM:
      asfPrintError("Conversion does not support shape type 'MultipointM'\n");
      break;
    case SHPT_MULTIPATCH:
      asfPrintError("Conversion does not support shape type 'Multipatch'\n");
    }
  // Ball park center for <LookAt> position  - does not have to be accurate
  clat = min[1] + (max[1]-min[1])/2;
  clon = min[0] + (max[0]-min[0])/2;

  for (ii=0; ii<nEntities; ii++) {

    // Read lat/lon from shape object
    shapeObject = SHPReadObject(shape, ii);
    nVertices = shapeObject->nVertices;
    lat = (double *) MALLOC(sizeof(double)*(nVertices+1));
    lon = (double *) MALLOC(sizeof(double)*(nVertices+1));
    height = (double *) MALLOC(sizeof(double)*(nVertices+1));
    for (kk=0; kk<nVertices; kk++) {
      lat[kk] = shapeObject->padfY[kk];
      lon[kk] = shapeObject->padfX[kk];
      if (shapeObject->padfZ)
	height[kk] = shapeObject->padfZ[kk];
    }
    lat[nVertices] = lat[0];
    lon[nVertices] = lon[0];
    height[nVertices] = height[0];
    nParts = shapeObject->nParts;
    part = (int *) MALLOC(sizeof(int)*(nParts+1));
    for (kk=0; kk<nParts; kk++)
      part[kk] = shapeObject->panPartStart[kk];
    part[nParts] = nVertices;
    SHPDestroyObject(shapeObject);
    if (nParts == 0)
      nParts++;

    // Extract the attributes out of the database file
    nFields = DBFGetFieldCount(dbase);

    for (ll=0; ll<nParts; ll++) {

      // Write information in kml file
      fprintf(fp, "<Placemark>\n");
      fprintf(fp, "<description><![CDATA[\n");
      fprintf(fp, "<!-- Format: SHAPE (generated by %s) -->\n", version_string("convert2vector"));
      for (kk=1; kk<nFields; kk++)
	write_dbase_field_to_kml(dbase, ii, kk, fp);
      fprintf(fp, "]]></description>\n");
      write_name_field_to_kml(dbase, ii, fp);
      fprintf(fp, "<LookAt>\n");
      fprintf(fp, "<longitude>%9.4f</longitude>\n", clon);
      fprintf(fp, "<latitude>%9.4f</latitude>\n", clat);
      fprintf(fp, "<range>400000</range>\n");
      fprintf(fp, "</LookAt>\n");
      if (pointType == SHPT_POINT) {
	fprintf(fp, "<Point>\n");
	fprintf(fp, "<coordinates>%.12f,%.12f,4000</coordinates>",
		lon[0], lat[0]);
	fprintf(fp, "</Point>\n");
      }
      else if (pointType == SHPT_POLYGON) {
	write_kml_style_keys(fp);
	fprintf(fp, "<Polygon>\n");
	fprintf(fp, "<outerBoundaryIs>\n");
	fprintf(fp, "<LinearRing>\n");
	fprintf(fp, "<coordinates>\n");
	for (kk=part[ll]; kk<part[ll+1]; kk++)
	  fprintf(fp, "%.6f,%.6f,4000\n", lon[kk], lat[kk]);
	fprintf(fp, "</coordinates>\n");
	fprintf(fp, "</LinearRing>\n");
	fprintf(fp, "</outerBoundaryIs>\n");
	fprintf(fp, "</Polygon>\n");
      }
      else if (pointType == SHPT_POINTZ) {
	fprintf(fp, "<Point>\n");
	fprintf(fp, "<coordinates>%.6f,%.6f,%.3f</coordinates>",
		lon[0], lat[0], height[0]);
	fprintf(fp, "</Point>\n");
      }
      else if (pointType == SHPT_ARCZ) {
	fprintf(fp, "<styleUrl>#yellowLineGreenPoly</styleUrl>\n");
	fprintf(fp, "<LineString>\n");
        fprintf(fp, "<extrude>1</extrude>\n");
        fprintf(fp, "<tessellate>1</tessellate>\n");
        fprintf(fp, "<altitudeMode>%s</altitudeMode>\n", altitude_mode());
        fprintf(fp, "<coordinates>\n");
 	for (kk=part[ll]; kk<part[ll+1]; kk++)
	  fprintf(fp, "%.6f,%.6f,%.3f\n", lon[kk], lat[kk], height[kk]);
	fprintf(fp, "</coordinates>\n");
	fprintf(fp, "</LineString>\n");
      }
      else if (pointType == SHPT_POLYGONZ) {
	write_kml_style_keys(fp);
	fprintf(fp, "<Polygon>\n");
	fprintf(fp, "<outerBoundaryIs>\n");
	fprintf(fp, "<LinearRing>\n");
	fprintf(fp, "<coordinates>\n");
	for (kk=part[ll]; kk<part[ll+1]; kk++)
	  fprintf(fp, "%.6f,%.6f,%.3f\n", lon[kk], lat[kk], height[kk]);
	fprintf(fp, "</coordinates>\n");
	fprintf(fp, "</LinearRing>\n");
	fprintf(fp, "</outerBoundaryIs>\n");
	fprintf(fp, "</Polygon>\n");
      }
      fprintf(fp, "</Placemark>\n");
    }
  }

  // Close shapefile
  close_shape(dbase, shape);

  // Clean up
  kml_footer(fp);
  FCLOSE(fp);

  return 1;
}

void shape2latlon(char *infile, double **latArray, double **lonArray, 
  int **startArray, int *nPoly, int *nCoords)
{
  DBFHandle dbase;
  SHPHandle shape;
  SHPObject *shapeObject;
  int ii, nEntities, nParts, nVertices, pointType;

  open_shape(infile, &dbase, &shape);
  SHPGetInfo(shape, &nEntities, &pointType, NULL, NULL);
  if (pointType != SHPT_POLYGON)
    asfPrintError("Only polygon shapefiles supported for subsetting!\n");
  shapeObject = SHPReadObject(shape, 0);
  nVertices = shapeObject->nVertices;
  nParts = shapeObject->nParts;
  int *start = (int *) MALLOC(sizeof(int)*(nParts+1));
  for (ii=0; ii<nParts; ii++)
    start[ii] = shapeObject->panPartStart[ii];
  start[nParts] = nVertices;
  double *lat = (double *) MALLOC(sizeof(double)*nVertices);
  double *lon = (double *) MALLOC(sizeof(double)*nVertices);
  for (ii=0; ii<nVertices; ii++) {
    lat[ii] = shapeObject->padfY[ii];
    lon[ii] = shapeObject->padfX[ii];
  }
  SHPDestroyObject(shapeObject);
  close_shape(dbase, shape);
  
  *latArray = lat;
  *lonArray = lon;
  *startArray = start;
  *nPoly = nParts;
  *nCoords = nVertices;
}

// Convert to shapefile
int convert2shape(char *inFile, char *outFile, char *format, int list)
{
  DBFHandle dbase;
  SHPHandle shape;
  dbf_header_t *dbf = NULL;
  char line[1024];
  int n = 0, nAttr = 0, nCoords = 0;
  double *lat = NULL, *lon = NULL;
  meta_parameters *meta = NULL;
  
  if (list) {
    FILE *fp = FOPEN(inFile, "r");
    if (strcmp_case(format, "META") != 0) {
      shapefile_init(outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
    }
    while (fgets(line, 1024, fp)) {
      strip_end_whitesp_inplace(line);
      if (strcmp_case(format, "META") == 0) {
        meta = meta2vector(line, &dbf, &nAttr, &lat, &lon, &nCoords);
        if (n == 0) {
          shapefile_init(outFile, format, meta);
          open_shape(outFile, &dbase, &shape);
        }
        write_shape_attributes(dbase, nAttr, n, dbf);
        write_shape_object(shape, nCoords, lat, lon);
      }      
      else if (strcmp_case(format, "SMAP") == 0) {
        smap2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
        write_shape_attributes(dbase, nAttr, n, dbf);
        write_shape_object(shape, nCoords, lat, lon);
      }
      else if (strcmp_case(format, "GEOTIFF") == 0) {
        geotiff2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
        write_shape_attributes(dbase, nAttr, n, dbf);
        write_shape_object(shape, nCoords, lat, lon);
      }
      n++;
    }
    FCLOSE(fp);
    close_shape(dbase, shape);
    if (meta && meta->projection) {
      write_asf2esri_proj(meta, NULL, outFile);
      meta_free(meta);
    }
    else
      write_esri_proj_file(outFile);
  }
  else {
    if (strcmp_case(format, "META") == 0) {
      meta = meta2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
      shapefile_init(outFile, format, meta);
      meta_free(meta);
      open_shape(outFile, &dbase, &shape);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
    else if (strcmp_case(format, "SMAP") == 0) {
      smap2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
      shapefile_init(outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
    else if (strcmp_case(format, "GEOTIFF") == 0) {
      geotiff2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
      shapefile_init(outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
  }
  FREE(lat);
  FREE(lon);

  return TRUE;
}
