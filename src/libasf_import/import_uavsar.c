#include "uavsar.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include <ctype.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

#define SQR(x) (x*x)
#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))
#define MAX_LINE 512

char **get_uavsar_products(const char *data_type, char *type, int *num_product)
{  
  char *rest, *token;
  int ii, product_count;
  char *tmp = (char *) MALLOC(sizeof(char)*60);
  strcpy(tmp, data_type);

  if (strcmp_case(type, "POLSAR") == 0) {
    product_count = 5;
    if (strcmp_case(tmp, "ALL") == 0)
      sprintf(tmp, "SLC,MLC,DAT,GRD,HGT");
  }
  else if (strcmp_case(type, "INSAR") == 0) {
    product_count = 9;
    if (strcmp_case(tmp, "ALL") == 0)
      sprintf(tmp, "AMP,INT,UNW,COR,AMP_GRD,INT_GRD,UNW_GRD,COR_GRD,HGT_GRD");
  }

  char **product = (char **) MALLOC(sizeof(char*) * product_count);
  for (ii = 0; ii < product_count; ii++) {
    product[ii] = (char *) MALLOC(sizeof(char)*10);
    strcpy(product[ii], "");
  }

  ii = 0;
  while ((token = strtok_r(tmp, ",", &rest))) {
    strcpy(product[ii], token);
    tmp = rest;
    ii++;
  }

  *num_product = ii;

  return product;
}

int parse_annotation_line(char *line, char *key, char *value)
{
  char *ks = line, *ke = line, *vs = line, *ve = line, *b = line;

  while(isspace(*b)) ks = ++b; // Move forward past the whitespace to the beginning of the key

  if(*b == ';' || *b == '\0') {
    // This line has nothing interesting, return nothing
    *key = *value = '\0';
    return 1;
  }

  while(*b != '(' && *b != '\0' && *b != '=') ++b; // Advance to a delimiter; either an equals sign, a '(' (to denote the beginning of the unit specification) or the end of the line
  
  if(*b == '\0') return 0; // Unexpected end-of-line

  ke = b - 1;
  while(isspace(*ke)) --ke; // Back up until we hit the end of the key

  while(*b != '\0' && *b != '=') b++; // Advance to the equals sign

  if(*b == '\0') return 0; // Unexpected end-of-line

  b++;
  while(isspace(*b) && *b != '\0') b++; // Move forward past the whitespace to the beginning of the value

  if(*b == '\0') return 0; // Unexpected end-of-line

  vs = b++;
  while(*b != ';' && *b != '\0') ++b; // Move forward to the end of the line or the beginning of a comment

  ve = b - 1;
  while(isspace(*ve)) --ve; // Back up until we hit the end of the key

  strncpy(key, ks, ke - ks + 1);
  key[ke-ks+1] = '\0';
  strncpy(value, vs, ve - vs + 1);
  value[ve-vs+1] = '\0';

  return 1;
}

int check_file(const char *path, char *line, char **fileName)
{
  char *p, *r;
  p = strchr(line, '=');
  char *file = (char *) MALLOC(sizeof(char)*255);
  strcpy(file, p+1);
  r = strchr(file, ';');
  if (r)
    r[0] = '\0';
  *fileName = MALLOC(sizeof(char)*(strlen(path) + strlen(file) + 32));
  strcpy(*fileName, path);
  char *trimmed_file = trim_spaces(file);
  strcat(*fileName, trimmed_file);
  FREE(file);
  FREE(trimmed_file);
  p = strchr(line, ';');
  long long size;
  sscanf(p+12, "%lld", &size);
  if (fileExists(*fileName) && fileSize(*fileName) == size)
    return TRUE;
  else
    return FALSE;
}

void
get_uavsar_file_names(const char *dataFile, uavsar_type_t type,
                      char ***pDataName, char ***pElement,
                      int **pDataType, int *nBands)
{
  int ii, slc = 0, mlc = 0, dat = 0, grd = 0, hgt = 0;
  int igram = 0, unw = 0, cor = 0, amp = 0;
  int igram_grd = 0, unw_grd = 0, cor_grd = 0, amp_grd = 0, hgt_grd = 0;

  char *file = (char *) MALLOC(sizeof(char) * 1024);
  char **dataName = (char **) MALLOC(6 * sizeof(char *));
  char **element = (char **) MALLOC(6 * sizeof(char *));
  int *dataType = (int *) MALLOC(6 * sizeof(int));
  for (ii = 0; ii < 6; ii++) {
    dataName[ii] = (char *) MALLOC(sizeof(char) * 512);
    element[ii] = (char *) MALLOC(sizeof(char) * 10);
  }
  *pDataName = dataName;
  *pElement = element;
  *pDataType = dataType;
  *nBands = 0;

  char *path = get_dirname(dataFile);

  char line[MAX_LINE], key[MAX_LINE], value[MAX_LINE];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, MAX_LINE, fp)) {
    if(!parse_annotation_line(line, key, value)) {
      asfPrintWarning("Unable to parse line in annotation file: %s", line);
      continue;
    }
    if (!strcmp(key, ""))
      continue;
    if (type == POLSAR_SLC) {
      if (!strcmp(key, "slcHH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "HH");
          dataType[slc] = 1;
          slc++;
        }
      }
      else if (!strcmp(key, "slcHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "HV");
          dataType[slc] = 1;
          slc++;
        }
      }
      else if (!strcmp(key, "slcVH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "VH");
          dataType[slc] = 1;
          slc++;
        }
      }
      else if (!strcmp(key, "slcVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[slc], file);
          strcpy(element[slc], "VV");
          dataType[slc] = 1;
          slc++;
        }
      }
      *nBands = slc;
    }
    else if (type == POLSAR_MLC) {
      if (!strcmp(key, "mlcHHHH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C11");
          dataType[mlc] = 0;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHVHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C22");
          dataType[mlc] = 0;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcVVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C33");
          dataType[mlc] = 0;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHHHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C12");
          dataType[mlc] = 1;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHHVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C13");
          dataType[mlc] = 1;
          mlc++;
        }
      }
      else if (!strcmp(key, "mlcHVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[mlc], file);
          strcpy(element[mlc], "C23");
          dataType[mlc] = 1;
          mlc++;
        }
      }
      *nBands = mlc;
    }
    else if (type == POLSAR_DAT) {
      if (!strcmp(key, "dat")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[dat], file);
          dat++;
        }
      }
      *nBands = dat;
    }
    else if (type == POLSAR_GRD) {
      if (!strcmp(key, "grdHHHH")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C11");
          dataType[grd] = 0;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHVHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C22");
          dataType[grd] = 0;
          grd++;
        }
      }
      else if (!strcmp(key, "grdVVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C33");
          dataType[grd] = 0;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHHHV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C12");
          dataType[grd] = 1;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHHVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C13");
          dataType[grd] = 1;
          grd++;
        }
      }
      else if (!strcmp(key, "grdHVVV")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[grd], file);
          strcpy(element[grd], "C23");
          dataType[grd] = 1;
          grd++;
        }
      }
      *nBands = grd;
    }
    else if (type == POLSAR_HGT) {
      if (!strcmp(key, "hgt")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[hgt], file);
          strcpy(element[hgt], "HH");
          dataType[hgt] = 0;
          hgt++;
        }
      }
      *nBands = hgt;
    }
    else if (type == INSAR_AMP) {
      if (!strcmp(key, "Slant Range Amplitude of Pass 1")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp], file);
          strcpy(element[amp], "HH");
          dataType[amp] = 0;
          amp++;
        }
      }
      else if (!strcmp(key, "Slant Range Amplitude of Pass 2")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp], file);
          strcpy(element[amp], "HH");
          dataType[amp] = 0;
          amp++;
        }
      }
      *nBands = amp;
    }
    else if (type == INSAR_AMP_GRD) {
      if (!strcmp(key, "Ground Range Amplitude of Pass 1")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp_grd], file);
          strcpy(element[amp_grd], "HH");
          dataType[amp_grd] = 0;
          amp_grd++;
        }
      }
      else if (!strcmp(key, "Ground Range Amplitude of Pass 2")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[amp_grd], file);
          strcpy(element[amp_grd], "HH");
          dataType[amp_grd] = 0;
          amp_grd++;
        }
      }
      *nBands = amp_grd;
    }
    else if (type == INSAR_INT) {
      if (!strcmp(key, "Slant Range Interferogram")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[igram], file);
          strcpy(element[igram], "HH");
          dataType[igram] = 1;
          igram++;
        }
      }
      *nBands = igram;
    }
    else if (type == INSAR_INT_GRD) {
      if (!strcmp(key, "Ground Range Interferogram")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[igram_grd], file);
          strcpy(element[igram_grd], "HH");
          dataType[igram_grd] = 1;
          igram_grd++;
        }
      }
      *nBands = igram_grd;
    }
    else if (type == INSAR_UNW) {
      if (!strcmp(key, "Slant Range Unwrapped Phase")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[unw], file);
          strcpy(element[unw], "HH");
          dataType[unw] = 0;
          unw++;
        }
      }
      *nBands = unw;
    }
    else if (type == INSAR_UNW_GRD) {
      if (!strcmp(key, "Ground Range Unwrapped Phase")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[unw_grd], file);
          strcpy(element[unw_grd], "HH");
          dataType[unw_grd] = 0;
          unw_grd++;
        }
      }
      *nBands = unw_grd;
    }
    else if (type == INSAR_COR) {
      if (!strcmp(key, "Slant Range Correlation")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[cor], file);
          strcpy(element[cor], "HH");
          dataType[cor] = 0;
          cor++;
        }
      }
      *nBands = cor;
    }
    else if (type == INSAR_COR_GRD) {
      if (!strcmp(key, "Ground Range Correlation")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[cor_grd], file);
          strcpy(element[cor_grd], "HH");
          dataType[cor_grd] = 0;
          cor_grd++;
        }
      }
      *nBands = cor_grd;
    }
    else if (type == INSAR_HGT_GRD) {
      if (!strcmp(key, "DEM Used in Ground Projection")) {
        if (check_file(path, line, &file)) {
          strcpy(dataName[hgt_grd], file);
          strcpy(element[hgt_grd], "HH");
          dataType[hgt_grd] = 0;
          hgt_grd++;
        }
      }
      *nBands = hgt_grd;
    }
  }

  FCLOSE(fp);
  FREE(file);
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
    else if (!strcmp(key, "Steering Angle"))
      params->steering_angle = atof(value);
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
  char time1[50], time2[50];

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
    else if (!strcmp(key, "Steering Angle"))
      params->steering_angle = atof(value);
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

int sign(char byteBuf)
{
  if (byteBuf < 0)
    return -1;
  else
    return 1;
}

char *check_data_type(const char *inFileName)
{
  char line[MAX_LINE], key[MAX_LINE], value[MAX_LINE];
  char *type = (char *) MALLOC(sizeof(char)*25);
  FILE *fp = FOPEN(inFileName, "r");
  while (fgets(line, MAX_LINE, fp)) {
    if(!parse_annotation_line(line, key, value)) {
      asfPrintWarning("Unable to parse line in annotation file: %s", line);
      continue;
    }
    if (!strcmp(key, ""))
      continue;
    if (!strcmp(key, "Acquisition Mode"))
      strcpy(type, value);
    else if (!strcmp(key, "Processing Mode"))
      strcpy(type, value);
    else if (strcmp_case(type, "RPI") == 0)
      sprintf(type, "InSAR");
  }
  FCLOSE(fp);

  return type;
}

void import_uavsar(const char *inFileName, int line, int sample, int width,
		   int height, radiometry_t radiometry,
		   const char *data_type, const char *outBaseName) {

  // UAVSAR comes in two flavors: InSAR and PolSAR
  // Things look similar to AirSAR data, just organized a little different.
  // There does not seem to be a consistent identifier in the annotation file,
  // that would allow us to easily identify the data set as UAVSAR. No mention
  // of UAVSAR whatsoever.

  // The data can come in a large variety of flavors (single look versus multi-
  // look, derived magnitude and phase, etc.). I assume we can take anything or
  // nothing from this menu. The different files have different dimensions,
  // so we will need to generate several output images to accommodate that.

  // PolSAR data
  // slc - Single look complex slant range data 
  // mlc - Multilook cross product slant range data
  // dat - Compressed Stokes matrix of multilooked data
  // grd - Ground range projected (equi-rectangular) and multilooked data
  // hgt - Digital elevation model projected in projection
  // slc_mag and slc_phase - 8 bytes per pixel derived from slc
  // mlc_mag and mlc_phase - 8 bytes per pixel derived from mlc

  // InSAR data
  // int - Slant range interferogram
  // unw - Slant range unwrapped phase
  // cor - Slant range correlation
  // amp - Slant range amplitudes
  // int_grd - Ground range interferogram
  // unw_grd - Ground range unwrapped phase
  // cor_grd - Ground range correlation
  // amp_grd - Ground range amplitudes
  // hgt_grd - Digital elevation model in ground projection

  FILE *fpIn, *fpOut;
  int ii, kk, ll, nn, pp, nBands, ns, *dataType, product_count;
  int multi = FALSE;
  float *floatAmp, *floatPhase, *floatAmpBuf, *amp, re, im;
  float *floatComplexReal, *floatComplexImag;
  float *floatComplexBuf;
  char **dataName, **element, **product, tmp[50];
  char *type;
  char *outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
  uavsar_polsar *polsar_params;
  uavsar_insar *insar_params;
  meta_parameters *metaIn, *metaOut;

  type = check_data_type(inFileName);
  asfPrintStatus("   Data type: %s\n", type);
  product = get_uavsar_products(data_type, type, &product_count);
  if (product_count > 1)
    multi = TRUE;

  for (pp = 0; pp < product_count; pp++) {

    // InSAR data
    // Ground range interferogram
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "INT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range interferogram does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaIn->general->sample_count;
      nn = 0;
      floatAmp = (float *) CALLOC(ns, sizeof(float));
      floatPhase = (float *) CALLOC(ns, sizeof(float));
      floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      metaOut->general->band_count = 2;
      if (multi)
	outName = appendToBasename(outBaseName, "_int_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range interferogram:\n");
      fpOut = FOPEN(outName, "wb");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      sprintf(metaOut->general->bands, "INTERFEROGRAM_AMP,INTERFEROGRAM_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	metaIn->general->sample_count = 2*ns;
	get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	for (kk=0; kk<ns; kk++) {
	  re = floatComplexBuf[kk*2];
	  im = floatComplexBuf[kk*2+1];
	  ieee_big32(re);
	  ieee_big32(im);
	  floatAmp[kk] = hypot(re,im);
	  floatPhase[kk] = atan2_check(im,re);
	}
	put_band_float_line(fpOut, metaOut, 0, ii, floatAmp);
	put_band_float_line(fpOut, metaOut, 1, ii, floatPhase);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmp);
      FREE(floatPhase);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }
    
    // Ground range unwrapped phase
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "UNW_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range unwrapped phase does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_unw_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range unwrapped phase:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "UNWRAPPED_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
      asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Ground range correlation image
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "COR_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range correlation image does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      if (multi)
	outName = appendToBasename(outBaseName, "_cor_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range correlation image:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "COHERENCE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Ground range amplitude images
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range amplitude images does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      metaOut->general->band_count = 2;
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_amp_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "AMP1,AMP2");
      asfPrintStatus("\nGround range amplitude images:\n");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	fpIn = FOPEN(dataName[nn], "rb");
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	  for (kk=0; kk<metaIn->general->sample_count; kk++)
	    ieee_big32(floatAmpBuf[kk]);
	  put_band_float_line(fpOut, metaOut, nn, ii, floatAmpBuf);
	  asfLineMeter(ii, metaIn->general->line_count);
	}
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Ground range digital elevation model
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "HGT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_HGT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range digital elevation model does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_HGT_GRD);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_hgt_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "HEIGHT");
      asfPrintStatus("\nGround range digital elevation model:\n");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	fpIn = FOPEN(dataName[nn], "rb");
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	  for (kk=0; kk<metaIn->general->sample_count; kk++)
	    ieee_big32(floatAmpBuf[kk]);
	  put_band_float_line(fpOut, metaOut, nn, ii, floatAmpBuf);
	  asfLineMeter(ii, metaIn->general->line_count);
	}
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }
    
    // Slant range interferogram
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "INT") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range interferogram does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaIn->general->sample_count;
      nn = 0;
      floatAmp = (float *) CALLOC(ns, sizeof(float));
      floatPhase = (float *) CALLOC(ns, sizeof(float));
      floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_int.img");
      else
	outName = appendExt(outBaseName, ".img");
      metaOut->general->band_count = 2;
      if (multi)
	outName = appendToBasename(outBaseName, "_int.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nSlant range interferogram:\n");
      fpOut = FOPEN(outName, "wb");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      sprintf(metaOut->general->bands, "INTERFEROGRAM_AMP,INTERFEROGRAM_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	metaIn->general->sample_count = 2*ns;
	get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	for (kk=0; kk<ns; kk++) {
	  re = floatComplexBuf[kk*2];
	  im = floatComplexBuf[kk*2+1];
	  ieee_big32(re);
	  ieee_big32(im);
	  floatAmp[kk] = hypot(re,im);
	  floatPhase[kk] = atan2_check(im,re);
	}
	put_band_float_line(fpOut, metaOut, 0, ii, floatAmp);
	put_band_float_line(fpOut, metaOut, 1, ii, floatPhase);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmp);
      FREE(floatPhase);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }
    
    // Slant range unwrapped phase
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "UNW") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range unwrapped phase does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_unw.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nSlant range unwrapped phase:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "UNWRAPPED_PHASE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Slant range correlation image
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "COR") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range correlation image does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_cor.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nSlant range correlation image:\n");
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "COHERENCE");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // Slant range amplitude images
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range amplitude images does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP);
      metaIn = uavsar_insar2meta(insar_params);
      metaOut = uavsar_insar2meta(insar_params);
      metaOut->general->band_count = 2;
      ns = metaOut->general->sample_count;
      nn = 0;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_amp.img");
      else
	outName = appendExt(outBaseName, ".img");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "AMP1,AMP2");
      asfPrintStatus("\nSlant range amplitude images:\n");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	fpIn = FOPEN(dataName[nn], "rb");
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	  for (kk=0; kk<metaIn->general->sample_count; kk++)
	    ieee_big32(floatAmpBuf[kk]);
	  put_band_float_line(fpOut, metaOut, nn, ii, floatAmpBuf);
	  asfLineMeter(ii, metaIn->general->line_count);
	}
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(insar_params);
    }    
    
    // PolSAR data
    // Single look complex data
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "SLC") == 0) {
      asfPrintWarning("Ingest of SLC data is currently not supported!\n");
      /*
      get_uavsar_file_names(inFileName, POLSAR_SLC, &dataName, &element,
			    &dataType, &nBands);
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_SLC);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      if (multi)
	outName = appendToBasename(outBaseName, "_slc.img");
      else
	outName = appendExt(outBaseName, ".img");
      for (ii=0; ii<nBands; ii++)
	printf("file: %s\n", dataName[ii]);
      //meta_write(metaOut, outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
      */
    }
    
    // Multilooked data
    if (strcmp_case(type, "PolSAR") == 0  &&
	strcmp_case(product[pp], "MLC") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_MLC, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range multilooked data do not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_MLC);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      ns = metaIn->general->sample_count;
      amp = (float *) MALLOC(sizeof(float)*ns);
      floatAmp = (float *) MALLOC(sizeof(float)*ns);
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      floatComplexReal = (float *) MALLOC(sizeof(float)*ns);
      floatComplexImag = (float *) MALLOC(sizeof(float)*ns);
      floatComplexBuf = (float *) MALLOC(sizeof(float)*2*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      metaOut->general->band_count = ll = 1;
      if (multi)
	outName = appendToBasename(outBaseName, "_mlc.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nMultilooked data:\n");
      fpOut = FOPEN(outName, "wb");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	if (dataType[nn] == 0)
	  metaOut->general->band_count += 1;
	else
	  metaOut->general->band_count += 2;
	fpIn = FOPEN(dataName[nn], "rb");
	if (nn == 0)
	  sprintf(metaOut->general->bands, "AMP,%s", element[0]);
	else {
	  if (dataType[nn])
	    sprintf(tmp, ",%s_real,%s_imag", element[nn], element[nn]);
	  else
	    sprintf(tmp, ",%s", element[nn]);
	  strcat(metaOut->general->bands, tmp);
	}
	for (ii=0; ii<metaIn->general->line_count; ii++) {
	  if (dataType[nn]) {
	    metaIn->general->sample_count = 2*ns;
	    get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	    for (kk=0; kk<ns; kk++) {
	      floatComplexReal[kk] = floatComplexBuf[kk*2];
	      floatComplexImag[kk] = floatComplexBuf[kk*2+1];
	      ieee_big32(floatComplexReal[kk]);
	      ieee_big32(floatComplexImag[kk]);
	    }
	    put_band_float_line(fpOut, metaOut, ll, ii, floatComplexReal);
	    put_band_float_line(fpOut, metaOut, ll+1, ii, floatComplexImag);
	  }
	  else {
	    metaIn->general->sample_count = ns;
	    get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	    if (nn == 0) {
	      for (kk=0; kk<ns; kk++) {
		ieee_big32(floatAmpBuf[kk]);
		floatAmp[kk] = sqrt(floatAmpBuf[kk]);
	      }
	    }
	    else {
	      for (kk=0; kk<ns; kk++) 
		ieee_big32(floatAmpBuf[kk]);
	    }
	    put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	  }
	  asfLineMeter(ii, metaIn->general->line_count);      
	}
	if (dataType[nn])
	  ll += 2;
	else
	  ll++;
	FCLOSE(fpIn);
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(amp);
      FREE(floatAmp);
      FREE(floatAmpBuf);
      FREE(floatComplexReal);
      FREE(floatComplexImag);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
    }    
    
    // Compressed Stokes matrix
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "DAT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_DAT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Slant range Stokes matrix does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_DAT);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_dat.img");
      else
	outName = appendExt(outBaseName, ".img");
      metaOut->general->band_count = 9;
      asfPrintStatus("\nCompressed Stokes matrix:\n");
      char *filename = get_filename(dataName[0]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      if (radiometry == r_AMP)
	strcpy(metaOut->general->bands,
	       "AMP,AMP_HH,PHASE_HH,AMP_HV,PHASE_HV,AMP_VH,PHASE_VH,"	\
	       "AMP_VV,PHASE_VV");
      else if (radiometry == r_SIGMA)
	strcpy(metaOut->general->bands,
	       "AMP,SIGMA-AMP-HH,SIGMA-PHASE-HH,SIGMA-AMP-HV,SIGMA-PHASE-HV," \
	       "SIGMA-AMP-VH,SIGMA-PHASE-VH,SIGMA-AMP-VV,SIGMA-PHASE-VV");
      else if (radiometry == r_SIGMA_DB)
	strcpy(metaOut->general->bands,
	       "AMP,SIGMA_DB-AMP-HH,SIGMA_DB-PHASE-HH,SIGMA_DB-AMP-HV,"	\
	       "SIGMA_DB-PHASE-HV,SIGMA_DB-AMP-VH,SIGMA_DB-PHASE-VH,"	\
	       "SIGMA_DB-AMP-VV,SIGMA_DB-PHASE-VV");
      int ns = metaOut->general->sample_count;
      float total_power, ysca, amp, phase;
      complexFloat cpx;
      float *power = (float *) MALLOC(sizeof(float)*ns);
      float *shh_amp = (float *) MALLOC(sizeof(float)*ns);
      float *shh_phase = (float *) MALLOC(sizeof(float)*ns);
      float *shv_amp = (float *) MALLOC(sizeof(float)*ns);
      float *shv_phase = (float *) MALLOC(sizeof(float)*ns);
      float *svh_amp = (float *) MALLOC(sizeof(float)*ns);
      float *svh_phase = (float *) MALLOC(sizeof(float)*ns);
      float *svv_amp = (float *) MALLOC(sizeof(float)*ns);
      float *svv_phase = (float *) MALLOC(sizeof(float)*ns);
      char *byteBuf = (char *) MALLOC(sizeof(char)*10);
      fpIn = FOPEN(dataName[0], "rb");
      fpOut = FOPEN(outName, "wb");
      for (ii=0; ii<metaOut->general->line_count; ii++) {
	for (kk=0; kk<metaOut->general->sample_count; kk++) {
	  FREAD(byteBuf, sizeof(char), 10, fpIn);
          //float m11, m12, m13, m14, m22, m23, m24, m33, m34, m44;
	  // Scale is always 1.0 according to Bruce Chapman
	  //m11 = ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
	  //m12 = (float)byteBuf[2] * m11 / 127.0;
	  //m13 = sign(byteBuf[3]) * SQR((float)byteBuf[3] / 127.0) * m11;
	  //m14 = sign(byteBuf[4]) * SQR((float)byteBuf[4] / 127.0) * m11;
	  //m23 = sign(byteBuf[5]) * SQR((float)byteBuf[5] / 127.0) * m11;
	  //m24 = sign(byteBuf[6]) * SQR((float)byteBuf[6] / 127.0) * m11;
	  //m33 = (float)byteBuf[7] * m11 / 127.0;
	  //m34 = (float)byteBuf[8] * m11 / 127.0;
	  //m44 = (float)byteBuf[9] * m11 / 127.0;
	  //m22 = 1 - m33 -m44;
	  total_power =
	    ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
	  ysca = 2.0 * sqrt(total_power);
	  power[kk] = sqrt(total_power);
	  cpx.real = (float)byteBuf[2] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[3] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  if (radiometry == r_AMP) {
	    shh_amp[kk] = amp;
	    shh_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA) {
	    shh_amp[kk] = amp*amp;
	    shh_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA_DB) {
	    shh_amp[kk] = amp;
	    shh_phase[kk] = phase;
	  }
	  cpx.real = (float)byteBuf[4] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[5] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  if (radiometry == r_AMP) {
	    shv_amp[kk] = amp;
	    shv_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA) {
	    shv_amp[kk] = amp*amp;
	    shv_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA_DB) {
	    shv_amp[kk] = amp;
	    shv_phase[kk] = phase;
	  }
	  cpx.real = (float)byteBuf[6] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[7] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  if (radiometry == r_AMP) {
	    svh_amp[kk] = amp;
	    svh_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA) {
	    svh_amp[kk] = amp*amp;
	    svh_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA_DB) {
	    svh_amp[kk] = amp;
	    svh_phase[kk] = phase;
	  }
	  cpx.real = (float)byteBuf[8] * ysca / 127.0;
	  cpx.imag = (float)byteBuf[9] * ysca / 127.0;
	  amp = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	  phase = atan2(cpx.imag, cpx.real);
	  if (radiometry == r_AMP) {
	    svv_amp[kk] = amp;
	    svv_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA) {
	    svv_amp[kk] = amp*amp;
	    svv_phase[kk] = phase;
	  }
	  else if (radiometry == r_SIGMA_DB) {
	    svv_amp[kk] = amp;
	    svv_phase[kk] = phase;
	  }
	}
	put_band_float_line(fpOut, metaOut, 0, ii, power);
	put_band_float_line(fpOut, metaOut, 1, ii, shh_amp);
	put_band_float_line(fpOut, metaOut, 2, ii, shh_phase);
	put_band_float_line(fpOut, metaOut, 3, ii, shv_amp);
	put_band_float_line(fpOut, metaOut, 4, ii, shv_phase);
	put_band_float_line(fpOut, metaOut, 5, ii, svh_amp);
	put_band_float_line(fpOut, metaOut, 6, ii, svh_phase);
	put_band_float_line(fpOut, metaOut, 7, ii, svv_amp);
	put_band_float_line(fpOut, metaOut, 8, ii, svv_phase);
	asfLineMeter(ii, metaOut->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(power);
      FREE(shh_amp);
      FREE(shh_phase);
      FREE(shv_amp);
      FREE(shv_phase);
      FREE(svh_amp);
      FREE(svh_phase);
      FREE(svv_amp);
      FREE(svv_phase);
      meta_write(metaOut, outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(outName);
      FREE(polsar_params);
    }
    
    // Ground range projected data
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "GRD") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Ground range projected data do not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_GRD);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      ns = metaIn->general->sample_count;
      floatAmpBuf = (float *) CALLOC(ns, sizeof(float));
      floatComplexReal = (float *) CALLOC(ns, sizeof(float));
      floatComplexImag = (float *) CALLOC(ns, sizeof(float));
      floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      metaOut->general->band_count = ll = 0;
      if (multi)
	outName = appendToBasename(outBaseName, "_grd.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nGround range projected data:\n");
      fpOut = FOPEN(outName, "wb");
      for (nn=0; nn<nBands; nn++) {
        char *filename = get_filename(dataName[nn]);
        asfPrintStatus("Ingesting %s ...\n", filename);
        FREE(filename);
	if (dataType[nn])
	  metaOut->general->band_count += 2;
	else
	  metaOut->general->band_count += 1;
	fpIn = FOPEN(dataName[nn], "rb");
	if (nn == 0)
	  sprintf(metaOut->general->bands, "%s", element[0]);
	else {
	  if (dataType[nn])
	    sprintf(tmp, ",%s_real,%s_imag", element[nn], element[nn]);
	  else
	    sprintf(tmp, ",%s", element[nn]);
	  strcat(metaOut->general->bands, tmp);
	}
	if (dataType[nn]) {
	  for (ii=0; ii<metaIn->general->line_count; ii++) {
	    metaIn->general->sample_count = 2*ns;
	    get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	    for (kk=0; kk<ns; kk++) {
	      floatComplexReal[kk] = floatComplexBuf[kk*2];
	      floatComplexImag[kk] = floatComplexBuf[kk*2+1];
	      ieee_big32(floatComplexReal[kk]);
	      ieee_big32(floatComplexImag[kk]);
	    }
	    put_band_float_line(fpOut, metaOut, ll, ii, floatComplexReal);
	    put_band_float_line(fpOut, metaOut, ll+1, ii, floatComplexImag);
	    asfLineMeter(ii, metaIn->general->line_count);
	  }
	}
	else {
	  for (ii=0; ii<metaIn->general->line_count; ii++) {
	    metaIn->general->sample_count = ns;
	    get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	    for (kk=0; kk<ns; kk++)
	      ieee_big32(floatAmpBuf[kk]);
	    put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	    asfLineMeter(ii, metaIn->general->line_count);      
	  }
	}
	FCLOSE(fpIn);
	if (dataType[nn])
	  ll += 2;
	else
	  ll++;
      }
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(floatComplexReal);
      FREE(floatComplexImag);
      FREE(floatComplexBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
    }
    
    // Digital elevation model
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "HGT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_HGT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands) {
	asfPrintWarning("Digital elevation model does not exist. "
			"Will skip the ingest.\n");
	continue;
      }
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_HGT);
      metaIn = uavsar_polsar2meta(polsar_params);
      metaOut = uavsar_polsar2meta(polsar_params);
      ns = metaOut->general->sample_count;
      floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
      outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
      if (multi)
	outName = appendToBasename(outBaseName, "_hgt.img");
      else
	outName = appendExt(outBaseName, ".img");
      asfPrintStatus("\nDigital elevation model:\n");
      nn=0;
      char *filename = get_filename(dataName[nn]);
      asfPrintStatus("Ingesting %s ...\n", filename);
      FREE(filename);
      fpIn = FOPEN(dataName[nn], "rb");
      fpOut = FOPEN(outName, "wb");
      strcpy(metaOut->general->bands, "HEIGHT");
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  ieee_big32(floatAmpBuf[kk]);
	put_float_line(fpOut, metaOut, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);
      }
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      meta_write(metaOut, outName);
      FREE(floatAmpBuf);
      FREE(outName);
      meta_free(metaIn);
      meta_free(metaOut);
      FREE(polsar_params);
    }    
  }
  if (strcmp_case(type, "PolSAR") == 0)
    product_count = 5;
  else if (strcmp_case(type, "InSAR") == 0)
    product_count = 9;
  for (pp=0; pp<product_count; pp++)
    FREE(product[pp]);
  FREE(product);
  FREE(type);
}

void read_meta_uavsar(const char *inFileName, const char *outBaseName) 
{
  int pp, product_count, *dataType, nBands;
  char *type = check_data_type(inFileName);
  char **product = get_uavsar_products("ALL", type, &product_count);
  char **dataName, **element;
  char *outName = (char *) MALLOC(sizeof(char)*255);
  uavsar_polsar *polsar_params;
  uavsar_insar *insar_params;
  meta_parameters *meta;
  
  for (pp = 0; pp < product_count; pp++) {

    // InSAR
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "INT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_int_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "UNW_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_unw_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "COR_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_cor_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_amp_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "HGT_GRD") == 0) {
      get_uavsar_file_names(inFileName, INSAR_HGT_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_HGT_GRD);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_hgt_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "INT") == 0) {
      get_uavsar_file_names(inFileName, INSAR_INT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_INT);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_int.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "UNW") == 0) {
      get_uavsar_file_names(inFileName, INSAR_UNW, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_UNW);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_unw.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 && 
	strcmp_case(product[pp], "COR") == 0) {
      get_uavsar_file_names(inFileName, INSAR_COR, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_COR);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_cor.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "InSAR") == 0 &&
	strcmp_case(product[pp], "AMP") == 0) {
      get_uavsar_file_names(inFileName, INSAR_AMP, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      insar_params = 
	read_uavsar_insar_params(inFileName, INSAR_AMP);
      meta = uavsar_insar2meta(insar_params);
      sprintf(outName, "%s_amp.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    // PolSAR
    if (strcmp_case(type, "PolSAR") == 0  &&
	strcmp_case(product[pp], "MLC") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_MLC, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_MLC);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_mlc.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "DAT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_DAT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_DAT);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_dat.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "GRD") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_GRD, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_GRD);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_grd.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
    if (strcmp_case(type, "PolSAR") == 0 &&
	strcmp_case(product[pp], "HGT") == 0) {
      get_uavsar_file_names(inFileName, POLSAR_HGT, &dataName, &element,
			    &dataType, &nBands);
      if (!nBands)
	continue;
      polsar_params = 
	read_uavsar_polsar_params(inFileName, POLSAR_HGT);
      meta = uavsar_polsar2meta(polsar_params);
      sprintf(outName, "%s_hgt.xml", outBaseName);
      meta_write_xml(meta, outName);
      FREE(outName);
      meta_free(meta);
      FREE(insar_params);
      FREE(dataType);
    }
  }
  for (pp=0; pp<product_count; pp++)
    FREE(product[pp]);
  FREE(product);
  FREE(type);
}
