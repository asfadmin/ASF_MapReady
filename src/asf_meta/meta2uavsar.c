#include <ctype.h>
#include "asf_meta.h"
#include "dateUtil.h"
#include "uavsar.h"
#include "asf_nan.h"

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

int parse_annotation_line(char *line, char *key, char *value)
{
  char *ks = line, *ke = line, *vs = line, *ve = line, *b = line;

  // Move forward past the whitespace to the beginning of the key
  while(isspace(*b)) ks = ++b;

  if(*b == ';' || *b == '\0') {
    // This line has nothing interesting, return nothing
    *key = *value = '\0';
    return 1;
  }

  // Advance to a delimiter; either an equals sign, a '(' (to denote the 
  // beginning of the unit specification) or the end of the line
  while(*b != '(' && *b != '\0' && *b != '=') ++b; 
  
  if(*b == '\0') return 0; // Unexpected end-of-line

  ke = b - 1;
  while(isspace(*ke)) --ke; // Back up until we hit the end of the key

  while(*b != '\0' && *b != '=') b++; // Advance to the equals sign

  if(*b == '\0') return 0; // Unexpected end-of-line

  b++;
  // Move forward past the whitespace to the beginning of the value
  while(isspace(*b) && *b != '\0') b++; 

  if(*b == '\0') {
    // This key has no value
    vs=ve=b;  
  }
  else {
    vs = b++;
    // Move forward to the end of the line or the beginning of a comment
    while(*b != ';' && *b != '\0') ++b; 

    ve = b - 1;
    while(isspace(*ve)) --ve; // Back up until we hit the end of the key
  }

  strncpy(key, ks, ke - ks + 1);
  key[ke-ks+1] = '\0';
  strncpy(value, vs, ve - vs + 1);
  value[ve-vs+1] = '\0';

  return 1;
}

uavsar_polsar *
read_uavsar_polsar_params(const char *dataFile, uavsar_type_t type)
{
  uavsar_polsar *params = (uavsar_polsar *) MALLOC(sizeof(uavsar_polsar));

  // Determine ID
  char *dirName = (char *) MALLOC(sizeof(char) * 1024);
  char *fileName = (char *) MALLOC(sizeof(char) * 1024);
  split_dir_and_file(dataFile, dirName, fileName);
  sprintf(params->id, "%s", stripExt(fileName));

  // Read annotation file
  char line[MAX_LINE], key[MAX_LINE], value[MAX_LINE];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, MAX_LINE, fp)) {
    if(!parse_annotation_line(line, key, value)) {
      asfPrintWarning("Unable to parse line in annotation file: %s", line);
      continue;
    }
    if (!strcmp(key, ""))
      continue;
    if (!strcmp(key, "Site Description"))
      strcpy(params->site, value);
    if (!strcmp(key, "Acquisition Mode"))
      strcpy(params->acquisition_mode, value);
    if (type == POLSAR_SLC) {
      params->type = POLSAR_SLC;
      if (!strcmp(key, "slc_mag.set_rows"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "slc_mag.set_cols"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "slc_mag.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "slc_mag.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "slc_mag.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "slc_mag.row_mult"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "slc_mag.col_mult"))
        params->range_pixel_spacing = atof(value);
      else if (!strcmp(key, "slc_mag.val_size"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "slc_mag.val_frmt"))
        strcpy(params->value_format, value);
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
      if (!strcmp(key, "SLC Data Units"))
        strcpy(params->data_units, value);
    }
    else if (type == POLSAR_MLC) {
      params->type = POLSAR_MLC;
      if (!strcmp(key, "mlc_pwr.set_rows"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "mlc_pwr.set_cols"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "mlc_pwr.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "mlc_pwr.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "mlc_pwr.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "mlc_pwr.row_mult"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "mlc_pwr.col_mult"))
        params->range_pixel_spacing = atof(value);
      else if (!strcmp(key, "mlc_pwr.val_size"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "mlc_pwr.val_frmt"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Number of Range Looks in MLC"))
        params->range_look_count = atoi(value);
      else if (!strcmp(key, "Number of Azimuth Looks in MLC"))
        params->azimuth_look_count = atoi(value);
      else if (!strcmp(key, "MLC Data Units"))
        strcpy(params->data_units, value);
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
    }
    else if (type == POLSAR_DAT) {
      params->type = POLSAR_DAT;
      if (!strcmp(key, "dat.set_rows"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "dat.set_cols"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "dat.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "dat.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "dat.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "dat.row_mult"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "dat.col_mult"))
        params->range_pixel_spacing = atof(value);
      else if (!strcmp(key, "dat.val_size"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "dat.val_frmt"))
        strcpy(params->value_format, value);
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
      strcpy(params->data_units, MAGIC_UNSET_STRING);
    }
    else if (type == POLSAR_GRD) {
      params->type = POLSAR_GRD;
      if (!strcmp(key, "grd_pwr.set_rows"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "grd_pwr.set_cols"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "grd_pwr.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "grd_pwr.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "grd_pwr.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "grd_pwr.row_mult"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "grd_pwr.col_mult"))
        params->range_pixel_spacing = atof(value);
      else if (!strcmp(key, "grd_pwr.val_size"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "grd_pwr.val_frmt"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Number of Range Looks in MLC"))
        params->range_look_count = atoi(value);
      else if (!strcmp(key, "Number of Azimuth Looks in MLC"))
        params->azimuth_look_count = atoi(value);
      else if (!strcmp(key, "GRD Data Units"))
        strcpy(params->data_units, value);
    }
    else if (type == POLSAR_HGT) {
      params->type = POLSAR_HGT;
      if (!strcmp(key, "hgt.set_rows"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "hgt.set_cols"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "hgt.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "hgt.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "hgt.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "hgt.row_mult"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "hgt.col_mult"))
        params->range_pixel_spacing = atof(value);
      else if (!strcmp(key, "hgt.val_size"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "hgt.val_frmt"))
        strcpy(params->value_format, value);
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
      if (!strcmp(key, "HGT Data Units"))
        strcpy(params->data_units, value);
    }
    if (!strcmp(key, "set_hddr"))
      params->header_bytes = atoi(value);
    else if (!strcmp(key, "set_tail"))
      params->tail_bytes = atoi(value);
    else if (!strcmp(key, "set_plat"))
      params->lat_peg_point = atof(value);
    else if (!strcmp(key, "set_plon"))
      params->lon_peg_point = atof(value);
    else if (!strcmp(key, "set_phdg"))
      params->head_peg_point = atof(value);
    else if (!strcmp(key, "val_endi"))
      strcpy(params->endianess, value);
    else if (!strcmp(key, "val_mult"))
      params->data_scale = atof(value);
    else if (!strcmp(key, "val_addr"))
      params->data_shift = atof(value);
    else if (!strcmp(key, "val_minv"))
      params->min_value = atof(value);
    else if (!strcmp(key, "val_maxv"))
      params->max_value = atof(value);
    else if (!strcmp(key, "Center Wavelength"))
      params->wavelength = atof(value);
    else if (!strcmp(key, "Ellipsoid Semi-major Axis"))
      params->semi_major = atof(value);
    else if (!strcmp(key, "Ellipsoid Eccentricity Squared"))
      params->eccentricity = atof(value);
    else if (!strcmp(key, "Look Direction"))
      strcpy(params->look_direction, value);
    else if (!strcmp(key, "Range Spacing per Bin"))
      params->range_spacing = atof(value);
    else if (!strcmp(key, "Azimuth Spacing"))
      params->azimuth_spacing = atof(value);
    else if (!strcmp(key, "Image Starting Range"))
      params->slant_range_first_pixel = atof(value);
    else if (!strcmp(key, "Global Average Yaw"))
      params->yaw = atof(value);
    else if (!strcmp(key, "Global Average Pitch"))
      params->pitch = atof(value);
    else if (!strcmp(key, "Global Average Roll"))
      params->roll = atof(value);
    else if (!strcmp(key, "Global Average ESA"))
      params->steering_angle = atof(value);
    else if (!strcmp(key, "Global Average Altitude"))
      params->altitude = atof(value);
    else if (!strcmp(key, "Average GPS Altitude"))
      params->altitude = atof(value);
    else if (!strcmp(key, "Global Average Terrain Height"))
      params->terrain_height = atof(value);
    else if (!strcmp(key, "Global Average Squint Angle"))
      params->squint_angle = atof(value);
    else if (!strcmp(key, "Pulse Length"))
      params->pulse_length = atof(value);
    else if (!strcmp(key, "Bandwidth"))
      params->bandwidth = atof(value);
    else if (!strcmp(key, "Approximate Upper Left Latitude"))
      params->lat_upper_left = atof(value);
    else if (!strcmp(key, "Approximate Upper Left Longitude"))
      params->lon_upper_left = atof(value);
    else if (!strcmp(key, "Approximate Upper Right Latitude"))
      params->lat_upper_right = atof(value);
    else if (!strcmp(key, "Approximate Upper Right Longitude"))
      params->lon_upper_right = atof(value);
    else if (!strcmp(key, "Approximate Lower Left Latitude"))
      params->lat_lower_left = atof(value);
    else if (!strcmp(key, "Approximate Lower Left Longitude"))
      params->lon_lower_left = atof(value);
    else if (!strcmp(key, "Approximate Lower Right Latitude"))
      params->lat_lower_right = atof(value);
    else if (!strcmp(key, "Approximate Lower Right Longitude"))
      params->lon_lower_right = atof(value);
    else if (!strcmp(key, "Date of Acquisition"))
      strcpy(params->acquisition_date, value);
    else if (!strcmp(key, "Processor Version Number"))
      strcpy(params->processor, value);
  }
  FCLOSE(fp);

  return params;
}

uavsar_insar *
read_uavsar_insar_params(const char *dataFile, uavsar_type_t type)
{
  uavsar_insar *params = (uavsar_insar *) MALLOC(sizeof(uavsar_insar));
  char time1[50]="", time2[50]="";

  // Determine ID
  char *dirName = (char *) MALLOC(sizeof(char) * 1024);
  char *fileName = (char *) MALLOC(sizeof(char) * 1024);
  split_dir_and_file(dataFile, dirName, fileName);
  sprintf(params->id, "%s", stripExt(fileName));

  // Read annotation file
  char line[MAX_LINE], key[MAX_LINE], value[MAX_LINE];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, MAX_LINE, fp)) {
    if(!parse_annotation_line(line, key, value)) {
      asfPrintWarning("Unable to parse line in annotation file: %s", line);
      continue;
    }
    if(!strcmp(key, ""))
      continue;
    if (!strcmp(key, "Site Description"))
      strcpy(params->site, value);
    if (!strcmp(key, "Processing Mode"))
      strcpy(params->processing_mode, value);
    if (!strcmp(key, "Polarization"))
      strcpy(params->polarization, value);
    if (!strcmp(key, "Number of Looks in Range"))
      params->range_look_count = atoi(value);
    if (!strcmp(key, "Number of Looks in Azimuth"))
      params->azimuth_look_count = atoi(value);
    if (type == INSAR_INT) {
      params->type = INSAR_INT;
      if (!strcmp(key, "Interferogram Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Interferogram Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Interferogram Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_UNW) {
      params->type = INSAR_UNW;
      if (!strcmp(key, "Unwrapped Phase Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Unwrapped Phase Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Unwrapped Phase Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_COR) {
      params->type = INSAR_COR;
      if (!strcmp(key, "Correlation Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Correlation Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Correlation Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_AMP) {
      params->type = INSAR_AMP;
      if (!strcmp(key, "Amplitude Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Amplitude Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Amplitude Units"))
        strcpy(params->data_units, value);
    }
    if (type >= INSAR_AMP && type <= INSAR_COR) {
      if (!strcmp(key, "Slant Range Data Azimuth Lines"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "Slant Range Data Range Samples"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "slt.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "slt.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "slt.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "Slant Range Data at Near Range")) {
        params->slant_range_first_pixel = atof(value);
        params->slant_range_first_pixel /= 1000.0;
      }
      else if (!strcmp(key, "Slant Range Data Azimuth Spacing"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "Slant Range Data Range Spacing"))
        params->range_pixel_spacing = atof(value);
    }
    else if (type >= INSAR_AMP_GRD && type <= INSAR_HGT_GRD) {
      if (!strcmp(key, "Ground Range Data Latitude Lines"))
        params->row_count = atoi(value);
      else if (!strcmp(key, "Ground Range Data Longitude Samples"))
        params->column_count = atoi(value);
      else if (!strcmp(key, "grd.set_proj"))
        strcpy(params->projection, value);
      else if (!strcmp(key, "grd.row_addr"))
        params->along_track_offset = atof(value);
      else if (!strcmp(key, "grd.col_addr"))
        params->cross_track_offset = atof(value);
      else if (!strcmp(key, "Ground Range Data Latitude Spacing"))
        params->azimuth_pixel_spacing = atof(value);
      else if (!strcmp(key, "Ground Range Data Longitude Spacing"))
        params->range_pixel_spacing = atof(value);
    }
    if (type == INSAR_INT_GRD) {
      params->type = INSAR_INT_GRD;
      if (!strcmp(key, "Interferogram Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Interferogram Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Interferogram Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_UNW_GRD) {
      params->type = INSAR_UNW_GRD;
      if (!strcmp(key, "Unwrapped Phase Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Unwrapped Phase Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Unwrapped Phase Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_COR_GRD) {
      params->type = INSAR_COR_GRD;
      if (!strcmp(key, "Correlation Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Correlation Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Correlation Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_AMP_GRD) {
      params->type = INSAR_AMP_GRD;
      if (!strcmp(key, "Amplitude Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "Amplitude Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "Amplitude Units"))
        strcpy(params->data_units, value);
    }
    else if (type == INSAR_HGT_GRD) {
      params->type = INSAR_HGT_GRD;
      if (!strcmp(key, "DEM Bytes Per Pixel"))
        params->bytes_per_pixel = atoi(value);
      else if (!strcmp(key, "DEM Pixel Format"))
        strcpy(params->value_format, value);
      else if (!strcmp(key, "DEM Units"))
        strcpy(params->data_units, value);
    }
    if (!strcmp(key, "set_hddr"))
      params->header_bytes = atoi(value);
    else if (!strcmp(key, "set_tail"))
      params->tail_bytes = atoi(value);
    else if (!strcmp(key, "set_plat"))
      params->lat_peg_point = atof(value);
    else if (!strcmp(key, "set_plon"))
      params->lon_peg_point = atof(value);
    else if (!strcmp(key, "set_phdg"))
      params->head_peg_point = atof(value);
    else if (!strcmp(key, "val_endi"))
      strcpy(params->endianess, value);
    else if (!strcmp(key, "val_mult"))
      params->data_scale = atof(value);
    else if (!strcmp(key, "val_addr"))
      params->data_shift = atof(value);
    else if (!strcmp(key, "val_minv"))
      params->min_value = atof(value);
    else if (!strcmp(key, "val_maxv"))
      params->max_value = atof(value);
    else if (!strcmp(key, "Center Wavelength"))
      params->wavelength = atof(value);
    else if (!strcmp(key, "Ellipsoid Semi-major Axis"))
      params->semi_major = atof(value);
    else if (!strcmp(key, "Ellipsoid Eccentricity Squared"))
      params->eccentricity = atof(value);
    else if (!strcmp(key, "Look Direction"))
      strcpy(params->look_direction, value);
    else if (!strcmp(key, "Range Spacing per Bin"))
      params->range_spacing = atof(value);
    else if (!strcmp(key, "Azimuth Spacing"))
      params->azimuth_spacing = atof(value);
    else if (!strcmp(key, "Image Starting Range"))
      params->slant_range_first_pixel = atof(value);
    else if (!strcmp(key, "Global Average Yaw"))
      params->yaw = atof(value);
    else if (!strcmp(key, "Global Average Pitch"))
      params->pitch = atof(value);
    else if (!strcmp(key, "Global Average Roll"))
      params->roll = atof(value);
    else if (!strcmp(key, "Global Average ESA"))
      params->steering_angle = atof(value);
    else if (!strcmp(key, "Global Average Altitude"))
      params->altitude = atof(value);
    else if (!strcmp(key, "Average GPS Altitude"))
      params->altitude = atof(value);
    else if (!strcmp(key, "Global Average Terrain Height"))
      params->terrain_height = atof(value);
    else if (!strcmp(key, "Global Average Squint Angle"))
      params->squint_angle = atof(value);
    else if (!strcmp(key, "Pulse Length"))
      params->pulse_length = atof(value);
    else if (!strcmp(key, "Bandwidth"))
      params->bandwidth = atof(value);
    else if (!strcmp(key, "Approximate Upper Left Latitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lat_upper_left = MAGIC_UNSET_DOUBLE;
      else
        params->lat_upper_left = atof(value);
    }
    else if (!strcmp(key, "Approximate Upper Left Longitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lon_upper_left = MAGIC_UNSET_DOUBLE;
      else
        params->lon_upper_left = atof(value);
    }
    else if (!strcmp(key, "Approximate Upper Right Latitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lat_upper_right = MAGIC_UNSET_DOUBLE;
      else
        params->lat_upper_right = atof(value);
    }
    else if (!strcmp(key, "Approximate Upper Right Longitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lon_upper_right = MAGIC_UNSET_DOUBLE;
      else
        params->lon_upper_right = atof(value);
    }
    else if (!strcmp(key, "Approximate Lower Left Latitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lat_lower_left = MAGIC_UNSET_DOUBLE;
      else
        params->lat_lower_left = atof(value);
    }
    else if (!strcmp(key, "Approximate Lower Left Longitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lon_lower_left = MAGIC_UNSET_DOUBLE;
      else
        params->lon_lower_left = atof(value);
    }
    else if (!strcmp(key, "Approximate Lower Right Latitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lat_lower_right = MAGIC_UNSET_DOUBLE;
      else
        params->lat_lower_right = atof(value);
    }
    else if (!strcmp(key, "Approximate Lower Right Longitude")) {
      if (strcmp_case(value, "N/A") == 0)
        params->lon_lower_right = MAGIC_UNSET_DOUBLE;
      else
        params->lon_lower_right = atof(value);
    }
    else if (!strcmp(key, "Time of Acquisition for Pass 1"))
      strcpy(time1, value);
    else if (!strcmp(key, "Time of Acquisition for Pass 2"))
      strcpy(time2, value);
    else if (!strcmp(key, "Processor Version Number"))
      strcpy(params->processor, value);
  }
  FCLOSE(fp);

  sprintf(params->acquisition_date, "%s, %s", time1, time2);

  return params;
}

meta_parameters* uavsar_polsar2meta(uavsar_polsar *params)
{
  meta_parameters *meta;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  sprintf(meta->general->basename, "%s", params->site);
  sprintf(meta->general->sensor, "UAVSAR");
  strcpy(meta->general->sensor_name, "PolSAR");
  strcpy(meta->general->processor, "JPL version ");
  strcat(meta->general->processor, params->processor);
  if (params->type == POLSAR_SLC) {
    strcpy(meta->general->mode, "SLC");
    meta->general->image_data_type = POLARIMETRIC_S2_MATRIX;
  }
  else if (params->type == POLSAR_MLC) {
    strcpy(meta->general->mode, "MLC");
    meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
  }
  else if (params->type == POLSAR_DAT) {
    strcpy(meta->general->mode, "DAT");
    meta->general->image_data_type = POLARIMETRIC_STOKES_MATRIX;
  }
  else if (params->type == POLSAR_GRD) {
    strcpy(meta->general->mode, "GRD");
    meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
    meta->general->no_data = 0;
  }
  else if (params->type == POLSAR_HGT) {
    strcpy(meta->general->mode, "HGT");
    meta->general->image_data_type = DEM;
  }
  else
    strcpy(meta->general->mode, "unknown");
  meta->general->radiometry = r_GAMMA;
  meta->general->data_type = REAL32;
  strcpy(meta->general->acquisition_date, params->acquisition_date);
  meta->general->band_count = 1;
  meta->general->line_count = params->row_count;
  meta->general->sample_count = params->column_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = params->range_pixel_spacing;
  meta->general->y_pixel_size = fabs(params->azimuth_pixel_spacing);
  meta->general->re_major = params->semi_major;
  double re = params->semi_major;
  double ecc2 = params->eccentricity;
  double rp = sqrt((1-ecc2)*re*re);
  meta->general->re_minor = rp;
  // no information on bit error rate, missing lines and no data

  // SAR block
  meta->sar = meta_sar_init();
  strcpy(meta->sar->polarization, "QUAD-POL");
  if (strcmp_case(params->projection, "SCX") == 0)
    meta->sar->image_type = 'S';
  else if (strcmp_case(params->projection, "EQA") == 0)
    meta->sar->image_type = 'P';
  if (strcmp_case(params->look_direction, "Left") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(params->look_direction, "Right") == 0)
    meta->sar->look_direction = 'R';
  if (params->type == POLSAR_SLC) {
    meta->sar->azimuth_look_count = 1;
    meta->sar->range_look_count = 1;
    meta->sar->multilook = 0;
  }
  else {
    // FIXME: Update once range look count is introduced to metadata structure
    meta->sar->azimuth_look_count = params->azimuth_look_count;
    meta->sar->range_look_count = params->range_look_count;
    meta->sar->multilook = 1;
  }
  meta->sar->deskewed = 1;
  meta->sar->original_line_count = params->row_count;
  meta->sar->original_sample_count = params->column_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  // no information on azimuth and range time per pixel
  meta->sar->time_shift = 0.0;
  meta->sar->slant_shift = 0.0;
  meta->sar->slant_range_first_pixel = params->slant_range_first_pixel * 1000.0;
  meta->sar->wavelength = params->wavelength / 100.0;
  // no information on pulse repetition frequency
  double tan_lat = params->lat_peg_point*D2R;
  meta->sar->earth_radius = rp*sqrt(1 + tan_lat*tan_lat) /
    sqrt(rp*rp/(re*re) + tan_lat*tan_lat);
  // no information on satellite height
  // no time information
  // no Doppler information
  meta->sar->yaw = params->yaw;
  meta->sar->pitch = params->pitch;
  meta->sar->roll = params->roll;
  meta->sar->azimuth_processing_bandwidth = params->bandwidth;
  // no chirp rate information
  meta->sar->pulse_duration = params->pulse_length / 1000.0;
  // no information on range sampling rate
  // FIXME: check polarizations for interferometric/polarimetric data
  // FIXME: multilook flag depends on data type
  
  // UAVSAR block
  meta->uavsar = meta_uavsar_init();
  strcpy(meta->uavsar->id, params->id);
  meta->uavsar->scale_factor = 1.0;
  meta->uavsar->gps_altitude = params->altitude;
  meta->uavsar->lat_peg_point = params->lat_peg_point;
  meta->uavsar->lon_peg_point = params->lon_peg_point;
  meta->uavsar->head_peg_point = params->head_peg_point;
  meta->uavsar->along_track_offset = params->along_track_offset;
  meta->uavsar->cross_track_offset = params->cross_track_offset;

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = params->lat_upper_left;
  meta->location->lon_start_near_range = params->lon_upper_left;
  meta->location->lat_start_far_range = params->lat_upper_right;
  meta->location->lon_start_far_range = params->lon_upper_right;
  meta->location->lat_end_near_range = params->lat_lower_left;
  meta->location->lon_end_near_range = params->lon_lower_left;
  meta->location->lat_end_far_range = params->lat_lower_right;
  meta->location->lon_end_far_range = params->lon_lower_right;

  // Projection block
  if (params->type == POLSAR_GRD || params->type == POLSAR_HGT) {
    meta->projection = meta_projection_init();
    meta->projection->type = LAT_LONG_PSEUDO_PROJECTION;
    strcpy (meta->projection->units, "degrees");
    if (params->along_track_offset >= 0.0)
      meta->projection->hem = 'N';
    else
      meta->projection->hem = 'S';
    meta->projection->re_major = meta->general->re_major;
    meta->projection->re_minor = meta->general->re_minor;
    meta->projection->height = 0.0;
    meta->projection->spheroid = WGS84_SPHEROID;
    meta->projection->datum = WGS84_DATUM;

    // Coordinate reference: Point
    // UAVSAR lat/lon projected data is calculated from center
    // pixel.  Thus we have to add in a half-pixel shift.
    meta->projection->startX = 
      params->cross_track_offset - params->range_pixel_spacing / 2.0;
    meta->projection->startY = 
      params->along_track_offset - params->azimuth_pixel_spacing / 2.0;
    meta->projection->perX = params->range_pixel_spacing;
    meta->projection->perY = params->azimuth_pixel_spacing;

    meta->general->center_latitude = params->along_track_offset + 
      params->azimuth_pixel_spacing * params->row_count / 2.0;
    meta->general->center_longitude = params->cross_track_offset +
      params->range_pixel_spacing * params->column_count / 2.0;
  }
  else {
    meta_get_latLon(meta, meta->general->line_count/2, meta->general->sample_count/2, 0,
                    &meta->general->center_latitude, &meta->general->center_longitude);
  }

  return meta;
}

meta_parameters* uavsar_insar2meta(uavsar_insar *params)
{
  meta_parameters *meta;

  // Allocate memory for metadata structure
  meta = raw_init();
  
  // General block
  sprintf(meta->general->basename, "%s", params->site);
  sprintf(meta->general->sensor, "UAVSAR");
  strcpy(meta->general->sensor_name, "InSAR");
  strcpy(meta->general->processor, "JPL version ");
  strcat(meta->general->processor, params->processor);
  if (params->type == INSAR_AMP || params->type == INSAR_AMP_GRD) {
    if (params->type == INSAR_AMP) 
      strcpy(meta->general->mode, "AMP");
    else if (params->type == INSAR_AMP_GRD) 
      strcpy(meta->general->mode, "AMP_GRD");
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  }
  else if (params->type == INSAR_INT || params->type == INSAR_INT_GRD) {
    if (params->type == INSAR_INT)
      strcpy(meta->general->mode, "INT");
    else if (params->type == INSAR_INT_GRD)
      strcpy(meta->general->mode, "INT_GRD");
    meta->general->image_data_type = INTERFEROGRAM;
  }
  else if (params->type == INSAR_UNW || params->type == INSAR_UNW_GRD) {
    if (params->type == INSAR_UNW)
      strcpy(meta->general->mode, "UNW");
    else if (params->type == INSAR_UNW_GRD)
      strcpy(meta->general->mode, "UNW_GRD");
    meta->general->image_data_type = UNWRAPPED_PHASE;
  }
  else if (params->type == INSAR_COR || params->type == INSAR_COR_GRD) {
    if (params->type == INSAR_COR)
      strcpy(meta->general->mode, "COR");
    else if (params->type == INSAR_COR_GRD)
      strcpy(meta->general->mode, "COR_GRD");
    meta->general->image_data_type = COHERENCE_IMAGE;
  }
  else if (params->type == INSAR_HGT_GRD) {
    strcpy(meta->general->mode, "HGT_GRD");
    meta->general->image_data_type = DEM;
  }
  else
    strcpy(meta->general->mode, "unknown");
  meta->general->data_type = REAL32;
  strcpy(meta->general->acquisition_date, params->acquisition_date);
  meta->general->band_count = 1;
  meta->general->line_count = params->row_count;
  meta->general->sample_count = params->column_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = params->range_pixel_spacing;
  meta->general->y_pixel_size = fabs(params->azimuth_pixel_spacing);
  meta->general->re_major = params->semi_major;
  double a = params->semi_major;
  double ecc2 = params->eccentricity;
  meta->general->re_minor = sqrt((1-ecc2)*a*a);
  // no information on bit error rate, missing lines and no data
  
  // SAR block
  meta->sar = meta_sar_init();
  strcpy(meta->sar->polarization, params->polarization);
  if (strcmp_case(params->projection, "SCX") == 0)
    meta->sar->image_type = 'S';
  else if (strcmp_case(params->projection, "EQA") == 0)
    meta->sar->image_type = 'P';
  if (strcmp_case(params->look_direction, "Left") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(params->look_direction, "Right") == 0)
    meta->sar->look_direction = 'R';
  meta->sar->azimuth_look_count = params->azimuth_look_count;
  meta->sar->range_look_count = params->range_look_count;
  meta->sar->multilook = 1;
  meta->sar->deskewed = 1;
  meta->sar->original_line_count = params->row_count;
  meta->sar->original_sample_count = params->column_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  // no information on azimuth and range time per pixel
  meta->sar->time_shift = 0.0;
  meta->sar->slant_shift = 0.0;
  meta->sar->slant_range_first_pixel = params->slant_range_first_pixel * 1000.0;
  meta->sar->wavelength = params->wavelength / 100.0;
  // no information on pulse repetition frequency
  // no information on earth radius and satellite height
  // no time information
  // no Doppler information
  meta->sar->yaw = params->yaw;
  meta->sar->pitch = params->pitch;
  meta->sar->roll = params->roll;
  meta->sar->azimuth_processing_bandwidth = params->bandwidth;
  // no chirp rate information
  meta->sar->pulse_duration = params->pulse_length / 1000.0;
  // no information on range sampling rate
  // FIXME: check polarizations for interferometric/polarimetric data
  // FIXME: multilook flag depends on data type
  
  // UAVSAR block
  meta->uavsar = meta_uavsar_init();
  strcpy(meta->uavsar->id, params->id);
  meta->uavsar->scale_factor = 1.0;
  meta->uavsar->gps_altitude = params->altitude;
  meta->uavsar->lat_peg_point = params->lat_peg_point;
  meta->uavsar->lon_peg_point = params->lon_peg_point;
  meta->uavsar->head_peg_point = params->head_peg_point;
  meta->uavsar->along_track_offset = params->along_track_offset;
  meta->uavsar->cross_track_offset = params->cross_track_offset;

  // Projection block
  if (params->type >= INSAR_AMP_GRD && params->type <= INSAR_HGT_GRD) {
    meta->projection = meta_projection_init();
    meta->projection->type = LAT_LONG_PSEUDO_PROJECTION;
    strcpy (meta->projection->units, "degrees");
    if (params->along_track_offset >= 0.0)
      meta->projection->hem = 'N';
    else
      meta->projection->hem = 'S';
    meta->projection->re_major = meta->general->re_major;
    meta->projection->re_minor = meta->general->re_minor;
    meta->projection->height = 0.0;
    meta->projection->spheroid = WGS84_SPHEROID;
    meta->projection->datum = WGS84_DATUM;

    // FIXME: Check that the following is correct - needs some data
    // Coordinate reference: Point
    // UAVSAR lat/lon projected data is calculated from center
    // pixel.  Thus we have to add in a half-pixel shift.
    meta->projection->startX = 
      params->cross_track_offset - params->range_pixel_spacing / 2.0;
    meta->projection->startY = 
      params->along_track_offset - params->azimuth_pixel_spacing / 2.0;
    meta->projection->perX = params->range_pixel_spacing;
    meta->projection->perY = params->azimuth_pixel_spacing;

    meta->general->center_latitude = params->along_track_offset + 
      params->azimuth_pixel_spacing * params->row_count / 2.0;
    meta->general->center_longitude = params->cross_track_offset +
      params->range_pixel_spacing * params->column_count / 2.0;
  }

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = params->lat_upper_left;
  meta->location->lon_start_near_range = params->lon_upper_left;
  meta->location->lat_start_far_range = params->lat_upper_right;
  meta->location->lon_start_far_range = params->lon_upper_right;
  meta->location->lat_end_near_range = params->lat_lower_left;
  meta->location->lon_end_near_range = params->lon_lower_left;
  meta->location->lat_end_far_range = params->lat_lower_right;
  meta->location->lon_end_far_range = params->lon_lower_right;

  return meta;
}
