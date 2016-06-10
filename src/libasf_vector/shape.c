#include "shapefil.h"
#include "spheroids.h"
#include "asf_vector.h"
#include "libasf_proj.h"
#include "asf.h"

void shapefile_init(char *inFile, char *outFile, char *format, 
  meta_parameters *meta)
{
  char *dbaseFile;
  DBFHandle dbase;
  SHPHandle shape = NULL;
  
  // Read configuration file
  dbf_header_t *dbf;
  int ii, nCols;
  char shape_type[25];
  if (strcmp_case(format, "CSV") == 0) {
    char line[1024], **cols;
    FILE *fpIn = FOPEN(inFile, "r");
    fgets(line, 1024, fpIn); // header line
    FCLOSE(fpIn);
    chomp(line);
    split_into_array(line, ',', &nCols, &cols);
    dbf = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*nCols);
    for (ii=0; ii<nCols; ii++) {
      dbf[ii].shape = STRDUP(cols[ii]);
      dbf[ii].format = DBF_STRING;
      dbf[ii].length = 100;
      dbf[ii].decimals = 0;
    }
    FREE(cols);
    strcpy(shape_type, "polygon");
  }
  else {
    if (!read_header_config(format, &dbf, &nCols, shape_type))
      asfPrintError("Could not find format (%s) information\n", format);
    if (strcmp_case(shape_type, "UNKNOWN") == 0)
      asfPrintError("Unknown shape type! Needs to be either 'POLYGON', 'LINE' "
        "or 'POINT'!\n");
    if (strcmp_case(format, "POINT") == 0 ||
      strcmp_case(format, "POLYGON") == 0)
      nCols = 1;
  }

  // Open database for initialization
  dbaseFile = (char *) MALLOC(sizeof(char)*(strlen(outFile)+5));
  sprintf(dbaseFile, "%s.dbf", outFile);
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
    shape = SHPCreate(outFile, SHPT_POINT);
  else if (strcmp_case(shape_type, "LINE") == 0)
    shape = SHPCreate(outFile, SHPT_ARC);
  else if (strcmp_case(shape_type, "POLYGON") == 0)
    shape = SHPCreate(outFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", outFile);

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

  if (format == FOOT_PRINT) {
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

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  if (format == POINT || format == MULTIMATCH)
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

void write_shape_object_ext(SHPHandle shape, int nCoords, double *lat, 
  double *lon, int nosplit)
{
  // Check whether we need to split up the polygon
  if (crosses_dateline(lon, 0, nCoords) && !nosplit) {
    int *start = (int *) MALLOC(sizeof(int)*2);
    double *mLat = (double *) MALLOC(sizeof(double)*(nCoords+5));
    double *mLon = (double *) MALLOC(sizeof(double)*(nCoords+5));

    split_polygon(lat, lon, nCoords, start, mLat, mLon, 60);
    
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

void write_shape_object_dateline(SHPHandle shape, int nCoords, double *lat,
  double *lon, double tolerance)
{
  // Check whether we need to split up the polygon
  if (crosses_dateline(lon, 0, nCoords) && tolerance > 0) {
    int *start = (int *) MALLOC(sizeof(int)*2);
    double *mLat = (double *) MALLOC(sizeof(double)*(nCoords+5));
    double *mLon = (double *) MALLOC(sizeof(double)*(nCoords+5));
  
    split_polygon(lat, lon, nCoords, start, mLat, mLon, tolerance);
  
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

void write_shape_object(SHPHandle shape, int nCoords, double *lat, double *lon)
{
  write_shape_object_ext(shape, nCoords, lat, lon, FALSE);
}

void close_shape(DBFHandle dbase, SHPHandle shape)
{
  // Close database
  DBFClose(dbase);

  // Close shapefile
  SHPClose(shape);

  return;
}

int point2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  char line[1024], **coords;
  int nCols;

  // Initialize the database file
  shapefile_init(NULL, outFile, "POINT", NULL);
  open_shape(outFile, &dbase, &shape);

  // Read the CSV file and keep writing location information
  char *id;
  double lat, lon;
  int n = 0;
  FILE *fp = FOPEN(inFile, "r");
  fgets(line, 1024, fp);
  while (fgets(line, 1024, fp) != NULL) {
    chomp(line);
    split_into_array(line, ',', &nCols, &coords);
    id = STRDUP(coords[0]);
    lat = atof(coords[1]);
    lon = atof(coords[2]);
    FREE(coords);
    DBFWriteStringAttribute(dbase, n, 0, id);
    SHPObject *shapeObject = NULL;
    shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);
    n++;
  }
  FCLOSE(fp);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return TRUE;
}

int latlon2shape(char *inFile, char *outFile)
{
  DBFHandle dbase;
  SHPHandle shape;
  dbf_header_t *dbf;
  char line[8192], id[255], shape_type[25], **coords;
  int ii, idx, n = 0, start = 0, nCols, nCoords;

  // Read CSV file
  FILE *ifp = FOPEN(inFile, "r");

  // Initialize the database file
  shapefile_init(NULL, outFile, "LATLON", NULL);
  open_shape(outFile, &dbase, &shape);
  read_header_config("LATLON", &dbf, &nCols, shape_type);

  while (fgets(line, 8192, ifp) != NULL) {
    chomp(line);
    split_into_array(line, ',', &nCols, &coords);
    nCoords = nCols/2 + 1;
    if (nCols % 2 != 0) {
      start = 1;
      strcpy(id, coords[0]);
    }
    else
      sprintf(id, "%d", n);
    printf("Found %d coordinate pairs\n", nCoords-1);
    double *lat = (double *) MALLOC(sizeof(double)*nCoords);
    double *lon = (double *) MALLOC(sizeof(double)*nCoords);
    for (ii=start; ii<nCols; ii+=2) {
      idx = ii/2;
      lat[idx] = atof(coords[ii]);
      lon[idx] = atof(coords[ii+1]);
    }
    lat[idx+1] = lat[0];
    lon[idx+1] = lon[0];
    dbf[ii].sValue = STRDUP(id);
    write_shape_attributes(dbase, nCoords, n, dbf);
    write_shape_object(shape, nCoords, lat, lon);
    FREE(lat);
    FREE(lon);
    FREE(coords);
    n++; 
  }

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  FCLOSE(ifp);

  return TRUE;
}

void csv2shape(char *inFile, char *format, char *outFile, c2v_config *cfg)
{  
  DBFHandle dbase = NULL;
  SHPHandle shape = NULL;

  // Read header file
  FILE *fpIn;
  dbf_header_t *header;
  char shape_type[25], **cols, str[10], line[1024];
  int ii, kk, nCols;
  if (strcmp_case(format, "CSV") == 0) {
    fpIn = FOPEN(inFile, "r");
    fgets(line, 1024, fpIn); // header line
    chomp(line);
    split_into_array(line, ',', &nCols, &cols);
    header = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*nCols);
    strcpy(shape_type, "polygon");
    for (ii=0; ii<nCols; ii++) {
      header[ii].meta = STRDUP(cols[ii]);
      header[ii].shape = STRDUP(cols[ii]);
      header[ii].format = DBF_STRING;
      header[ii].length = 50;
      header[ii].decimals = 0;
      header[ii].definition = STRDUP(cols[ii]);
      header[ii].column = ii; 
    }
    FREE(cols);
  }
  else
    read_header_config(format, &header, &nCols, shape_type);
  
  if (strcmp_case(shape_type, "POINT") == 0) {

    // We only need check whether a LAT/LON pair is in the file
    int nColumns, nLat = 0, nLon = 0;
    fpIn = FOPEN(inFile, "r");
    fgets(line, 1024, fpIn); // header line
    if (cfg->debug)
      asfPrintStatus("%s", line);
    chomp(line);
    split_into_array(line, ',', &nColumns, &cols);
  
    for (ii=0; ii<nCols; ii++) {

      // Assign the column we need to read from
      for (kk=0; kk<nColumns; kk++) {
        if (strcmp_case(header[ii].meta, cols[kk]) == 0)
          header[ii].column = kk;
      }
    
      // Checking for LAT/LON columns
      if (strcmp_case(header[ii].shape, "LAT") == 0)
        nLat++;
      if (strcmp_case(header[ii].shape, "LON") == 0)
        nLon++;
    }
    if (nLat != 1 || nLon != 1)
      asfPrintError("Found %d latitude and %d longitude columns!\n", 
        nLat, nLon);

    // Initialize shapefile
    shapefile_init(inFile, outFile, format, NULL);
    open_shape(outFile, &dbase, &shape);

    // Read point information
    double lat, lon; 
    int column, n = 0;
    while (fgets(line, 1024, fpIn)) {
      if (cfg->debug)
        asfPrintStatus("%s", line);
      chomp(line);
      split_into_array(line, ',', &nColumns, &cols);
      for (ii=0; ii<nCols; ii++) {
        column = header[ii].column;
        if (header[ii].format == DBF_STRING)
          header[ii].sValue = STRDUP(cols[column]);
        else if (header[ii].format == DBF_INTEGER)
          header[ii].nValue = atoi(cols[column]);
        else if (header[ii].format == DBF_DOUBLE)
          header[ii].fValue = atof(cols[column]);
        
        // Assign lat/lon coordinates
        if (strcmp_case(header[ii].shape, "LAT") == 0)
          lat = atof(cols[column]);
        if (strcmp_case(header[ii].shape, "LON") == 0)
          lon = atof(cols[column]);
      }
      FREE(cols);
      write_shape_attributes(dbase, nCols, n, header);
      SHPObject *shapeObject = NULL;
      shapeObject = SHPCreateSimpleObject(SHPT_POINT, 1, &lon, &lat, NULL);
      SHPWriteObject(shape, -1, shapeObject);
      SHPDestroyObject(shapeObject);
      n++;
    }
  }
  else if (strcmp_case(shape_type, "LINE") == 0 ||
    strcmp_case(shape_type, "POLYGON") == 0) {
  
    // Figure out how many vertices we got
    int nColumns, nVertices = 0, nLat = 0, nLon = 0;
    fpIn = FOPEN(inFile, "r");
    fgets(line, 1024, fpIn); // header line
    chomp(line);
    split_into_array(line, ',', &nColumns, &cols);
  
    for (ii=0; ii<nCols; ii++) {
  
      // Assign the column we need to read from
      for (kk=0; kk<nColumns; kk++) {
        if (strcmp_case(header[ii].meta, cols[kk]) == 0)
          header[ii].column = kk;
      }
    
      // Assuming that we don't have more than 12 vertices
      for (kk=1; kk<13; kk++) {
        sprintf(str, "LAT%d", kk);
        if (strcmp_case(header[ii].shape, str) == 0)
          nLat++;
        sprintf(str, "LON%d", kk);
        if (strcmp_case(header[ii].shape, str) == 0)
          nLon++;
      }

      // Alternative column names - only things that we already know
      if (strcmp_case(header[ii].shape, "NEAR START LAT") == 0 ||
        strcmp_case(header[ii].shape, "NEAR_START_LAT") == 0)
        nLat++;
      else if (strcmp_case(header[ii].shape, "NEAR START LON") == 0 ||
        strcmp_case(header[ii].shape, "NEAR_START_LON") == 0)
        nLon++;
      else if (strcmp_case(header[ii].shape, "FAR START LAT") == 0 ||
        strcmp_case(header[ii].shape, "FAR_START_LAT") == 0)
        nLat++;
      else if (strcmp_case(header[ii].shape, "FAR START LON") == 0 ||
        strcmp_case(header[ii].shape, "FAR_START_LON") == 0)
        nLon++;
      else if (strcmp_case(header[ii].shape, "NEAR END LAT") == 0 ||
        strcmp_case(header[ii].shape, "NEAR_END_LAT") == 0)
        nLat++;
      else if (strcmp_case(header[ii].shape, "NEAR END LON") == 0 ||
        strcmp_case(header[ii].shape, "NEAR_END_LON") == 0)
        nLon++;
      else if (strcmp_case(header[ii].shape, "FAR END LAT") == 0 ||
        strcmp_case(header[ii].shape, "FAR_END_LAT") == 0)
        nLat++;
      else if (strcmp_case(header[ii].shape, "FAR END LON") == 0 ||
        strcmp_case(header[ii].shape, "FAR_END_LON") == 0)
        nLon++;
    }
    if (nLat != nLon)
      asfPrintError("Found %d latitude and %d longitude columns.\n"
        "Can't convert this information properly!\n", nLat, nLon);
    else {
      nVertices = nLat;
      asfPrintStatus("Found %d vertices of a %s.\n", nVertices, shape_type);
    }

    // Initialize shapefile
    shapefile_init(inFile, outFile, format, NULL);
    open_shape(outFile, &dbase, &shape);
  
    // Read polygon information
    double *lat = (double *) MALLOC(sizeof(double)*(nVertices+1));
    double *lon = (double *) MALLOC(sizeof(double)*(nVertices+1)); 
    int column, n = 0;
    while (fgets(line, 1024, fpIn)) {
      chomp(line);
      split_into_array(line, ',', &nColumns, &cols);
      for (ii=0; ii<nCols; ii++) {
        column = header[ii].column;
        if (header[ii].format == DBF_STRING)
          header[ii].sValue = STRDUP(cols[column]);
        else if (header[ii].format == DBF_INTEGER)
          header[ii].nValue = atoi(cols[column]);
        else if (header[ii].format == DBF_DOUBLE)
          header[ii].fValue = atof(cols[column]);
        
        // Standard LAT/LON columns - mentioned in the documentation
        for (kk=0; kk<nVertices; kk++) {
          sprintf(str, "LAT%d", kk+1);
          if (strcmp_case(header[ii].shape, str) == 0)
            lat[kk] = atof(cols[column]);
          sprintf(str, "LON%d", kk+1);
          if (strcmp_case(header[ii].shape, str) == 0)
            lon[kk] = atof(cols[column]);
        }
      
        // Alternative column names - only things that we already know
        if (strcmp_case(header[ii].shape, "NEAR START LAT") == 0 ||
          strcmp_case(header[ii].shape, "NEAR_START_LAT") == 0)
          lat[0] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "NEAR START LON") == 0 ||
          strcmp_case(header[ii].shape, "NEAR_START_LON") == 0)
          lon[0] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "FAR START LAT") == 0 ||
          strcmp_case(header[ii].shape, "FAR_START_LAT") == 0)
          lat[1] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "FAR START LON") == 0 ||
          strcmp_case(header[ii].shape, "FAR_START_LON") == 0)
          lon[1] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "NEAR END LAT") == 0 ||
          strcmp_case(header[ii].shape, "NEAR_END_LAT") == 0)
          lat[3] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "NEAR END LON") == 0 ||
          strcmp_case(header[ii].shape, "NEAR_END_LON") == 0)
          lon[3] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "FAR END LAT") == 0 ||
          strcmp_case(header[ii].shape, "FAR_END_LAT") == 0)
          lat[2] = atof(cols[column]);
        else if (strcmp_case(header[ii].shape, "FAR END LON") == 0 ||
          strcmp_case(header[ii].shape, "FAR_END_LON") == 0)
          lon[2] = atof(cols[column]);
      }
      lat[nVertices] = lat[0];
      lon[nVertices] = lon[0];
      FREE(cols);
      write_shape_attributes(dbase, nCols, n, header);
      if (strcmp_case(shape_type, "POLYGON") == 0)
        write_shape_object_dateline(shape, nVertices+1, lat, lon, 
          cfg->wrapdateline);
      else {
        SHPObject *shapeObject = NULL;
        shapeObject = SHPCreateSimpleObject(SHPT_ARC, nVertices, lon, lat,
          NULL);
        SHPWriteObject(shape, -1, shapeObject);
        SHPDestroyObject(shapeObject);
      }
      n++;
    }
  }

  // Close shapefile
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);      
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
int convert2shape(char *inFile, char *outFile, char *format, int list,
  c2v_config *cfg)
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
      shapefile_init(NULL, outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
    }
    while (fgets(line, 1024, fp)) {
      chomp(line);
      if (strcmp_case(format, "META") == 0) {
        meta = meta2vector(line, &dbf, &nAttr, &lat, &lon, &nCoords);
        if (n == 0) {
          shapefile_init(NULL, outFile, format, meta);
          open_shape(outFile, &dbase, &shape);
        }
        write_shape_attributes(dbase, nAttr, n, dbf);
        write_shape_object(shape, nCoords, lat, lon);
      }      
      else if (strcmp_case(format, "SMAP") == 0) {
        smap2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
        write_shape_attributes(dbase, nAttr, n, dbf);
        write_shape_object_ext(shape, nCoords, lat, lon, cfg->nosplit);
      }
      else if (strcmp_case(format, "GEOTIFF") == 0) {
        geotiff2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
        write_shape_attributes(dbase, nAttr, n, dbf);
        write_shape_object(shape, nCoords, lat, lon);
      }
      else
        asfPrintError("List option for %s format not supported\n", format);
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
      shapefile_init(NULL, outFile, format, meta);
      meta_free(meta);
      open_shape(outFile, &dbase, &shape);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
    else if (strcmp_case(format, "SMAP") == 0) {
      shapefile_init(NULL, outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
      smap2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
    else if (strcmp_case(format, "GEOTIFF") == 0) {
      shapefile_init(NULL, outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
      geotiff2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
    else if (strcmp_case(format, "POLYGON") == 0) {
      shapefile_init(NULL, outFile, format, meta);
      open_shape(outFile, &dbase, &shape);
      polygon2vector(inFile, &dbf, &nAttr, &lat, &lon, &nCoords);
      write_shape_attributes(dbase, nAttr, 0, dbf);
      write_shape_object(shape, nCoords, lat, lon);
      close_shape(dbase, shape);
      write_esri_proj_file(outFile);
    }
    else // must be some CSV type file
      csv2shape(inFile, format, outFile, cfg);
  }
  FREE(lat);
  FREE(lon);

  return TRUE;
}
