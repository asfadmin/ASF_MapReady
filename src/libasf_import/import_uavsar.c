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

static char *get_uavsar(char *buf, char *str)
{
  char *p, *q, *r, *value;

  value = (char *) CALLOC(60,sizeof(char));
  p = strstr(buf, str);
  if (p) {
    q = strchr(p, '=');
    strncpy(value, q+1, 60);
    r = strchr(value, ';');
    if (r)
      r[0] = '\0';
    value = trim_spaces(value);
    //printf("%s: %s\n", str, value);
  }
  else
    strcpy(value, "");

  return value;
}

static int check_file(char *line, char **fileName)
{
  char *p;
  p = strchr(line, '=');
  char *file = (char *) CALLOC(1, sizeof(char)*255);
  strncpy(file, p+1, 65);
  *fileName = trim_spaces(file);
  FREE(file);
  p = strchr(line, ';');
  long long size;
  sscanf(p+12, "%lld", &size);
  if (fileExists(*fileName) && fileSize(*fileName) == size)
    return TRUE;
  else
    return FALSE;
}

static void get_uavsar_file_names(const char *dataFile, uavsar_type_t type, 
				  char ***pDataName, char ***pElement,
				  int **pDataType, int *nBands)
{
  char *file = (char *) MALLOC(sizeof(char)*1024);
  int ii, slc=0, mlc=0, dat=0, grd=0, hgt=0;
  int igram=0, unw=0, cor=0, amp=0;
  int igram_grd=0, unw_grd=0, cor_grd=0, amp_grd=0, hgt_grd=0;

  char **dataName = (char **) MALLOC(6*sizeof(char *));
  char **element = (char **) MALLOC(6*sizeof(char *));
  int *dataType = (int *) MALLOC(6*sizeof(int));
  for (ii=0; ii<6; ii++) {
    dataName[ii] = (char *) MALLOC(sizeof(char)*512);
    element[ii] = (char *) MALLOC(sizeof(char)*10);
  }
  *pDataName = dataName;
  *pElement = element;
  *pDataType = dataType;
  *nBands = 0;

  char line[255];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, 255, fp)) {
    if (type == POLSAR_SLC) {
      if (strstr(line, "slcHH   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(element[slc], "HH");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      else if (strstr(line, "slcHV   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(element[slc], "HV");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      else if (strstr(line, "slcVH   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(element[slc], "VH");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      else if (strstr(line, "slcVV   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(element[slc], "VV");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      *nBands = slc;
    }
    else if (type == POLSAR_MLC) {
      if (strstr(line, "mlcHHHH =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(element[mlc], "C11");
	  dataType[mlc] = 0;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHVHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(element[mlc], "C22");
	  dataType[mlc] = 0;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcVVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(element[mlc], "C33");
	  dataType[mlc] = 0;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHHHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(element[mlc], "C12");
	  dataType[mlc] = 1;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHHVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(element[mlc], "C13");
	  dataType[mlc] = 1;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(element[mlc], "C23");
	  dataType[mlc] = 1;
	  mlc++;
	}
      }
      *nBands = mlc;
    }
    else if (type == POLSAR_DAT) {
      if (strstr(line, "dat     =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[dat], file);
	  dat++;
	}
      }
      *nBands = dat;
    }
    else if (type == POLSAR_GRD) {
      if (strstr(line, "grdHHHH =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(element[grd], "C11");
	  dataType[grd] = 0;
	  grd++;
	}
      }
      else if (strstr(line, "grdHVHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(element[grd], "C22");
	  dataType[grd] = 0;
	  grd++;
	}
      }
      else if (strstr(line, "grdVVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(element[grd], "C33");
	  dataType[grd] = 0;
	  grd++;
	}
      }
      else if (strstr(line, "grdHHHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(element[grd], "C12");
	  dataType[grd] = 1;
	  grd++;
	}
      }
      else if (strstr(line, "grdHHVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(element[grd], "C13");
	  dataType[grd] = 1;
	  grd++;
	}
      }
      else if (strstr(line, "grdHVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(element[grd], "C23");
	  dataType[grd] = 1;
	  grd++;
	}
      }
      *nBands = grd;
    }
    else if (type == POLSAR_HGT) {
      if (strstr(line, "hgt     =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[hgt], file);
	  strcpy(element[hgt], "HH");
	  dataType[hgt] = 0;
	  hgt++;
	}
      }
      *nBands = hgt;
    }
    else if (type == INSAR_AMP) {
      if (strstr(line, "Slant Range Amplitude of Pass 1          =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[amp], file);
	  strcpy(element[amp], "HH");
	  dataType[amp] = 0;
	  amp++;
	}
      }
      else if (strstr(line, "Slant Range Amplitude of Pass 2          =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[amp], file);
	  strcpy(element[amp], "HH");
	  dataType[amp] = 0;
	  amp++;
	}
      }
      *nBands = amp;
    }
    else if (type == INSAR_AMP_GRD) {
      if (strstr(line, "Ground Range Amplitude of Pass 1         =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[amp_grd], file);
	  strcpy(element[amp_grd], "HH");
	  dataType[amp_grd] = 0;
	  amp_grd++;
	}
      }
      else if (strstr(line, "Ground Range Amplitude of Pass 2         =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[amp_grd], file);
	  strcpy(element[amp_grd], "HH");
	  dataType[amp_grd] = 0;
	  amp_grd++;
	}
      }
      *nBands = amp_grd;
    }
    else if (type == INSAR_INT) {
      if (strstr(line, "Slant Range Interferogram                =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[igram], file);
	  strcpy(element[igram], "HH");
	  dataType[igram] = 1;
	  igram++;
	}
      }
      *nBands = igram;
    }
    else if (type == INSAR_INT_GRD) {
      if (strstr(line, "Ground Range Interferogram               =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[igram_grd], file);
	  strcpy(element[igram_grd], "HH");
	  dataType[igram_grd] = 1;
	  igram_grd++;
	}
      }
      *nBands = igram_grd;
    }
    else if (type == INSAR_UNW) {
      if (strstr(line, "Slant Range Unwrapped Phase              =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[unw], file);
	  strcpy(element[unw], "HH");
	  dataType[unw] = 0;
	  unw++;	  
	}
      }
      *nBands = unw;
    }
    else if (type == INSAR_UNW_GRD) {
      if (strstr(line, "Ground Range Unwrapped Phase             =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[unw_grd], file);
	  strcpy(element[unw_grd], "HH");
	  dataType[unw_grd] = 0;
	  unw_grd++;
	}
      }
      *nBands = unw_grd;
    }
    else if (type == INSAR_COR) {
      if (strstr(line, "Slant Range Correlation                  =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[cor], file);
	  strcpy(element[cor], "HH");
	  dataType[cor] = 0;
	  cor++;	  
	}
      }
      *nBands = cor;
    }
    else if (type == INSAR_COR_GRD) {
      if (strstr(line, "Ground Range Correlation                 =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[cor_grd], file);
	  strcpy(element[cor_grd], "HH");
	  dataType[cor_grd] = 0;
	  cor_grd++;
	}
      }
      *nBands = cor_grd;
    }
    else if (type == INSAR_HGT_GRD) {
      if (strstr(line, "DEM Used in Ground Projection            =")) {
	if (check_file(line, &file)) {
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

uavsar_polsar *read_uavsar_polsar_params(const char *dataFile, 
					 uavsar_type_t type)
{
  uavsar_polsar *params = (uavsar_polsar *) MALLOC(sizeof(uavsar_polsar));

  // Read annotation file
  char line[255];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, 255, fp)) {
    if (strstr(line, "Site Description"))
      strcpy(params->site, get_uavsar(line, "Site Description"));
    if (strstr(line, "Acquisition Mode"))
      strcpy(params->acquisition_mode, get_uavsar(line, "Acquisition Mode"));
    if (type == POLSAR_SLC) {
      params->type = POLSAR_SLC;
      if (strstr(line, "slc_mag.set_rows"))
	params->row_count = atoi(get_uavsar(line, "slc_mag.set_rows"));
      else if (strstr(line, "slc_mag.set_cols"))
	params->column_count = atoi(get_uavsar(line, "slc_mag.set_cols"));
      else if (strstr(line, "slc_mag.set_proj"))
	strcpy(params->projection, get_uavsar(line, "slc_mag.set_proj"));
      else if (strstr(line, "slc_mag.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "slc_mag.row_addr"));
      else if (strstr(line, "slc_mag.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "slc_mag.col_addr"));
      else if (strstr(line, "slc_mag.row_mult"))
	params->azimuth_pixel_spacing = 
	  atof(get_uavsar(line, "slc_mag.row_mult"));
      else if (strstr(line, "slc_mag.col_mult"))
	params->range_pixel_spacing = 
	  atof(get_uavsar(line, "slc_mag.col_mult"));
      else if (strstr(line, "slc_mag.val_size"))
	params->bytes_per_pixel = atoi(get_uavsar(line, "slc_mag.val_size"));
      else if (strstr(line, "slc_mag.val_frmt"))
	strcpy(params->value_format, get_uavsar(line, "slc_mag.val_frmt"));
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
      if (strstr(line, "SLC Data Units"))
	strcpy(params->data_units, get_uavsar(line, "SLC Data Units"));
    }
    else if (type == POLSAR_MLC) {
      params->type = POLSAR_MLC;
      if (strstr(line, "mlc_pwr.set_rows"))
	params->row_count = atoi(get_uavsar(line, "mlc_pwr.set_rows"));
      else if (strstr(line, "mlc_pwr.set_cols"))
	params->column_count = atoi(get_uavsar(line, "mlc_pwr.set_cols"));
      else if (strstr(line, "mlc_pwr.set_proj"))
	strcpy(params->projection, get_uavsar(line, "mlc_pwr.set_proj"));
      else if (strstr(line, "mlc_pwr.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "mlc_pwr.row_addr"));
      else if (strstr(line, "mlc_pwr.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "mlc_pwr.col_addr"));
      else if (strstr(line, "mlc_pwr.row_mult"))
	params->azimuth_pixel_spacing = 
	  atof(get_uavsar(line, "mlc_pwr.row_mult"));
      else if (strstr(line, "mlc_pwr.col_mult"))
	params->range_pixel_spacing = 
	  atof(get_uavsar(line, "mlc_pwr.col_mult"));
      else if (strstr(line, "mlc_pwr.val_size"))
	params->bytes_per_pixel = atoi(get_uavsar(line, "mlc_pwr.val_size"));
      else if (strstr(line, "mlc_pwr.val_frmt"))
	strcpy(params->value_format, get_uavsar(line, "mlc_pwr.val_frmt"));
      else if (strstr(line, "Number of Range Looks in MLC"))
	params->range_look_count = 
	  atoi(get_uavsar(line, "Number of Range Looks in MLC"));
      else if (strstr(line, "Number of Range Looks in MLC"))
	params->azimuth_look_count =
	  atoi(get_uavsar(line, "Number of Azimuth Looks in MLC"));
      else if (strstr(line, "MLC Data Units"))
	strcpy(params->data_units, get_uavsar(line, "MLC Data Units"));
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
    }
    else if (type == POLSAR_DAT) {
      params->type = POLSAR_DAT;
      if (strstr(line, "dat.set_rows"))
	params->row_count = atoi(get_uavsar(line, "dat.set_rows"));
      else if (strstr(line, "dat.set_cols"))
	params->column_count = atoi(get_uavsar(line, "dat.set_cols"));
      else if (strstr(line, "dat.set_proj"))
	strcpy(params->projection, get_uavsar(line, "dat.set_proj"));
      else if (strstr(line, "dat.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "dat.row_addr"));
      else if (strstr(line, "dat.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "dat.col_addr"));
      else if (strstr(line, "dat.row_mult"))
	params->azimuth_pixel_spacing = atof(get_uavsar(line, "dat.row_mult"));
      else if (strstr(line, "dat.col_mult"))
	params->range_pixel_spacing = atof(get_uavsar(line, "dat.col_mult"));
      else if (strstr(line, "dat.val_size"))
	params->bytes_per_pixel = atoi(get_uavsar(line, "dat.val_size"));
      else if (strstr(line, "dat.val_frmt"))
	strcpy(params->value_format, get_uavsar(line, "dat.val_frmt"));
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
      strcpy(params->data_units, MAGIC_UNSET_STRING);
    }
    else if (type == POLSAR_GRD) {
      params->type = POLSAR_GRD;
      if (strstr(line, "grd_pwr.set_rows"))
	params->row_count = atoi(get_uavsar(line, "grd_pwr.set_rows"));
      else if (strstr(line, "grd_pwr.set_cols"))
	params->column_count = atoi(get_uavsar(line, "grd_pwr.set_cols"));
      else if (strstr(line, "grd_pwr.set_proj"))
	strcpy(params->projection, get_uavsar(line, "grd_pwr.set_proj"));
      else if (strstr(line, "grd_pwr.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "grd_pwr.row_addr"));
      else if (strstr(line, "grd_pwr.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "grd_pwr.col_addr"));
      else if (strstr(line, "grd_pwr.row_mult"))
	params->azimuth_pixel_spacing = 
	  atof(get_uavsar(line, "grd_pwr.row_mult"));
      else if (strstr(line, "grd_pwr.col_mult"))
	params->range_pixel_spacing = 
	  atof(get_uavsar(line, "grd_pwr.col_mult"));
      else if (strstr(line, "grd_pwr.val_size"))
	params->bytes_per_pixel = atoi(get_uavsar(line, "grd_pwr.val_size"));
      else if (strstr(line, "grd_pwr.val_frmt"))
	strcpy(params->value_format, get_uavsar(line, "grd_pwr.val_frmt"));
      else if (strstr(line, "Number of Range Looks in MLC"))
	params->range_look_count = 
	  atoi(get_uavsar(line, "Number of Range Looks in MLC"));
      else if (strstr(line, "Number of Azimuth Looks in MLC"))
	params->azimuth_look_count =
	  atoi(get_uavsar(line, "Number of Azimuth Looks in MLC"));
      else if (strstr(line, "GRD Data Units"))
	strcpy(params->data_units, get_uavsar(line, "GRD Data Units"));
    }
    else if (type == POLSAR_HGT) {
      params->type = POLSAR_HGT;
      if (strstr(line, "hgt.set_rows"))
	params->row_count = atoi(get_uavsar(line, "hgt.set_rows"));
      else if (strstr(line, "hgt.set_cols"))
	params->column_count = atoi(get_uavsar(line, "hgt.set_cols"));
      else if (strstr(line, "hgt.set_proj"))
	strcpy(params->projection, get_uavsar(line, "hgt.set_proj"));
      else if (strstr(line, "hgt.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "hgt.row_addr"));
      else if (strstr(line, "hgt.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "hgt.col_addr"));
      else if (strstr(line, "hgt.row_mult"))
	params->azimuth_pixel_spacing = atof(get_uavsar(line, "hgt.row_mult"));
      else if (strstr(line, "hgt.col_mult"))
	params->range_pixel_spacing = atof(get_uavsar(line, "hgt.col_mult"));
      else if (strstr(line, "hgt.val_size"))
	params->bytes_per_pixel = atoi(get_uavsar(line, "hgt.val_size"));
      else if (strstr(line, "hgt.val_frmt"))
	strcpy(params->value_format, get_uavsar(line, "hgt.val_frmt"));
      params->range_look_count = 1;
      params->azimuth_look_count = 1;
      if (strstr(line, "HGT Data Units"))
	strcpy(params->data_units, get_uavsar(line, "HGT Data Units"));
    }
    if (strstr(line, "set_hddr"))
      params->header_bytes = atoi(get_uavsar(line, "set_hddr"));
    else if (strstr(line, "set_tail"))
      params->tail_bytes = atoi(get_uavsar(line, "set_tail"));
    else if (strstr(line, "set_plat"))
      params->lat_peg_point = atof(get_uavsar(line, "set_plat"));
    else if (strstr(line, "set_plon"))
      params->lon_peg_point = atof(get_uavsar(line, "set_plon"));
    else if (strstr(line, "set_phdg"))
      params->head_peg_point = atof(get_uavsar(line, "set_phdg"));
    else if (strstr(line, "val_endi"))
      strcpy(params->endianess, get_uavsar(line, "val_endi"));
    else if (strstr(line, "val_mult"))
      params->data_scale = atof(get_uavsar(line, "val_mult"));
    else if (strstr(line, "val_addr"))
      params->data_shift = atof(get_uavsar(line, "val_addr"));
    else if (strstr(line, "val_minv"))
      params->min_value = atof(get_uavsar(line, "val_minv"));
    else if (strstr(line, "val_maxv"))
      params->max_value = atof(get_uavsar(line, "val_maxv"));
    else if (strstr(line, "Center Wavelength"))
      params->wavelength = atof(get_uavsar(line, "Center Wavelength"));
    else if (strstr(line, "Ellipsoid Semi-major Axis"))
      params->semi_major = atof(get_uavsar(line, "Ellipsoid Semi-major Axis"));
    else if (strstr(line, "Ellipsoid Eccentricity Squared"))
      params->eccentricity = 
	atof(get_uavsar(line, "Ellipsoid Eccentricity Squared"));
    else if (strstr(line, "Look Direction"))
      strcpy(params->look_direction, get_uavsar(line, "Look Direction"));
    else if (strstr(line, "Range Spacing per Bin"))
      params->range_spacing = atof(get_uavsar(line, "Range Spacing per Bin"));
    else if (strstr(line, "Azimuth Spacing"))
      params->azimuth_spacing = atof(get_uavsar(line, "Azimuth Spacing"));
    else if (strstr(line, "Image Starting Range"))
      params->slant_range_first_pixel = 
	atof(get_uavsar(line, "Image Starting Range"));
    else if (strstr(line, "Global Average Yaw"))
      params->yaw = atof(get_uavsar(line, "Global Average Yaw"));
    else if (strstr(line, "Global Average Pitch"))
      params->pitch = atof(get_uavsar(line, "Global Average Pitch"));
    else if (strstr(line, "Global Average Roll"))
      params->roll = atof(get_uavsar(line, "Global Average Roll"));
    else if (strstr(line, "Global Average Altitude"))
      params->altitude = atof(get_uavsar(line, "Global Average Altitude"));
    else if (strstr(line, "Average GPS Altitude"))
      params->altitude = atof(get_uavsar(line, "Average GPS Altitude"));
    else if (strstr(line, "Global Average Terrain Height"))
      params->terrain_height = 
	atof(get_uavsar(line, "Global Average Terrain Height"));
    else if (strstr(line, "Global Average Squint Angle"))
      params->squint_angle = 
	atof(get_uavsar(line, "Global Average Squint Angle"));
    else if (strstr(line, "Pulse Length"))
      params->pulse_length = atof(get_uavsar(line, "Pulse Length"));
    else if (strstr(line, "Steering Angle (90 is Boresite)"))
      params->steering_angle = 
	atof(get_uavsar(line, "Steering Angle (90 is Boresite)"));
    else if (strstr(line, "Bandwidth"))
      params->bandwidth = atof(get_uavsar(line, "Bandwidth"));
    else if (strstr(line, "Approximate Upper Left Latitude"))
      params->lat_upper_left = 
	atof(get_uavsar(line, "Approximate Upper Left Latitude"));
    else if (strstr(line, "Approximate Upper Left Longitude"))
	params->lon_upper_left = 
	  atof(get_uavsar(line, "Approximate Upper Left Longitude"));
    else if (strstr(line, "Approximate Upper Right Latitude"))
      params->lat_upper_right = 
	atof(get_uavsar(line, "Approximate Upper Right Latitude"));
    else if (strstr(line, "Approximate Upper Right Longitude"))
	params->lon_upper_right = 
	  atof(get_uavsar(line, "Approximate Upper Right Longitude"));
    else if (strstr(line, "Approximate Lower Left Latitude"))
      params->lat_lower_left = 
	atof(get_uavsar(line, "Approximate Lower Left Latitude"));
    else if (strstr(line, "Approximate Lower Left Longitude"))
	params->lon_lower_left = 
	  atof(get_uavsar(line, "Approximate Lower Left Longitude"));
    else if (strstr(line, "Approximate Lower Right Latitude"))
      params->lat_lower_right = 
	atof(get_uavsar(line, "Approximate Lower Right Latitude"));
    else if (strstr(line, "Approximate Lower Right Longitude"))
	params->lon_lower_right = 
	  atof(get_uavsar(line, "Approximate Lower Right Longitude"));
    else if (strstr(line, "Date of Acquisition"))
      strcpy(params->acquisition_date, get_uavsar(line, "Date of Acquisition"));
    else if (strstr(line, "Processor Version Number"))
      strcpy(params->processor, get_uavsar(line, "Processor Version Number"));
  }
  FCLOSE(fp);

  return params;
}

uavsar_insar *read_uavsar_insar_params(const char *dataFile, 
				       uavsar_type_t type)
{
  uavsar_insar *params = (uavsar_insar *) MALLOC(sizeof(uavsar_insar));
  // Read annotation file
  char line[255];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, 255, fp)) {
    if (strstr(line, "Site Description"))
      strcpy(params->site, get_uavsar(line, "Site Description"));
    if (strstr(line, "Processing Mode"))
      strcpy(params->processing_mode, get_uavsar(line, "Processing Mode"));
    if (type == INSAR_INT) {
      params->type = INSAR_INT;
      if (strstr(line, "Interferogram Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Interferogram Bytes Per Pixel"));
      else if (strstr(line, "Interferogram Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Interferogram Pixel Format"));
      else if (strstr(line, "Interferogram Units"))
	strcpy(params->data_units, get_uavsar(line, "Interferogram Units"));
    }
    else if (type == INSAR_UNW) {
      params->type = INSAR_UNW;
      if (strstr(line, "Unwrapped Phase Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Unwrapped Phase Bytes Per Pixel"));
      else if (strstr(line, "Unwrapped Phase Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Unwrapped Phase Pixel Format"));
      else if (strstr(line, "Unwrapped Phase Units"))
	strcpy(params->data_units, get_uavsar(line, "Unwrapped Phase Units"));
    }
    else if (type == INSAR_COR) {
      params->type = INSAR_COR;
      if (strstr(line, "Correlation Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Correlation Bytes Per Pixel"));
      else if (strstr(line, "Correlation Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Correlation Pixel Format"));
      else if (strstr(line, "Correlation Units"))
	strcpy(params->data_units, get_uavsar(line, "Correlation Units"));
    }
    else if (type == INSAR_AMP) {
      params->type = INSAR_AMP;
      if (strstr(line, "Amplitude Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Amplitude Bytes Per Pixel"));
      else if (strstr(line, "Amplitude Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Amplitude Pixel Format"));
      else if (strstr(line, "Amplitude Units"))
	strcpy(params->data_units, get_uavsar(line, "Amplitude Units"));
    }
    if (type >= INSAR_AMP && type <=INSAR_COR) {
      if (strstr(line, "Slant Range Data Azimuth Lines"))
	params->row_count = 
	  atoi(get_uavsar(line, "Slant Range Data Azimuth Lines"));
      else if (strstr(line, "Slant Range Data Range Samples"))
	params->column_count = 
	  atoi(get_uavsar(line, "Slant Range Data Range Samples"));
      else if (strstr(line, "slt.set_proj"))
	strcpy(params->projection, get_uavsar(line, "slt.set_proj"));
      else if (strstr(line, "slt.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "slt.row_addr"));
      else if (strstr(line, "slt.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "slt.col_addr"));
      else if (strstr(line, "Slant Range Data at Near Range")) {
	params->slant_range_first_pixel = 
	  atof(get_uavsar(line, "Slant Range Data at Near Range"));
	params->slant_range_first_pixel /= 1000.0;
      }
      else if (strstr(line, "Slant Range Data Azimuth Spacing"))
	params->azimuth_pixel_spacing = 
	  atof(get_uavsar(line, "Slant Range Data Azimuth Spacing"));
      else if (strstr(line, "Slant Range Data Range Spacing"))
	params->range_pixel_spacing = 
	  atof(get_uavsar(line, "Slant Range Data Range Spacing"));
      else if (strstr(line, "Number of Looks in Range"))
	params->range_look_count = 
	  atoi(get_uavsar(line, "Number of Looks in Range"));
      else if (strstr(line, "Number of Looks in Azimuth"))
	params->azimuth_look_count = 
	  atoi(get_uavsar(line, "Number of Looks in Azimuth"));
    }
    else if (type >= INSAR_AMP_GRD && type <=INSAR_HGT_GRD) { 
      if (strstr(line, "Ground Range Data Latitude Lines"))
	params->row_count = 
	  atoi(get_uavsar(line, "Ground Range Data Latitude Lines"));
      else if (strstr(line, "Ground Range Data Longitude Samples"))
	params->column_count = 
	  atoi(get_uavsar(line, "Ground Range Data Longitude Samples"));
      else if (strstr(line, "grd.set_proj"))
	strcpy(params->projection, get_uavsar(line, "grd.set_proj"));
      else if (strstr(line, "grd.row_addr"))
	params->along_track_offset = atof(get_uavsar(line, "grd.row_addr"));
      else if (strstr(line, "grd.col_addr"))
	params->cross_track_offset = atof(get_uavsar(line, "grd.col_addr"));
      else if (strstr(line, "Number of Looks in Range"))
	params->range_look_count = 
	  atoi(get_uavsar(line, "Number of Looks in Range"));
      else if (strstr(line, "Number of Looks in Azimuth"))
	params->azimuth_look_count = 
	  atoi(get_uavsar(line, "Number of Looks in Azimuth"));
    }
    if (type == INSAR_INT_GRD) {
      params->type = INSAR_INT_GRD;
      if (strstr(line, "Interferogram Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Interferogram Bytes Per Pixel"));
      else if (strstr(line, "Interferogram Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Interferogram Pixel Format"));
      else if (strstr(line, "Interferogram Units"))
	strcpy(params->data_units, get_uavsar(line, "Interferogram Units"));
    }
    else if (type == INSAR_UNW_GRD) {
      params->type = INSAR_UNW_GRD;
      if (strstr(line, "Unwrapped Phase Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Unwrapped Phase Bytes Per Pixel"));
      else if (strstr(line, "Unwrapped Phase Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Unwrapped Phase Pixel Format"));
      else if (strstr(line, "Unwrapped Phase Units"))
	strcpy(params->data_units, get_uavsar(line, "Unwrapped Phase Units"));
    }
    else if (type == INSAR_COR_GRD) {
      params->type = INSAR_COR_GRD;
      if (strstr(line, "Correlation Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Correlation Bytes Per Pixel"));
      else if (strstr(line, "Correlation Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Correlation Pixel Format"));
      else if (strstr(line, "Correlation Units"))
	strcpy(params->data_units, get_uavsar(line, "Correlation Units"));
    }
    else if (type == INSAR_AMP_GRD) {
      params->type = INSAR_AMP_GRD;
      if (strstr(line, "Amplitude Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "Amplitude Bytes Per Pixel"));
      else if (strstr(line, "Amplitude Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "Amplitude Pixel Format"));
      else if (strstr(line, "Amplitude Units"))
	strcpy(params->data_units, get_uavsar(line, "Amplitude Units"));
    }
    else if (type == INSAR_HGT_GRD) {
      params->type = INSAR_HGT_GRD;
      if (strstr(line, "DEM Bytes Per Pixel"))
	params->bytes_per_pixel = 
	  atoi(get_uavsar(line, "DEM Bytes Per Pixel"));
      else if (strstr(line, "DEM Pixel Format"))
	strcpy(params->value_format, 
	       get_uavsar(line, "DEM Pixel Format"));
      else if (strstr(line, "DEM Units"))
	strcpy(params->data_units, get_uavsar(line, "DEM Units"));
    }
    if (strstr(line, "set_hddr"))
      params->header_bytes = atoi(get_uavsar(line, "set_hddr"));
    else if (strstr(line, "set_tail"))
      params->tail_bytes = atoi(get_uavsar(line, "set_tail"));
    else if (strstr(line, "set_plat"))
      params->lat_peg_point = atof(get_uavsar(line, "set_plat"));
    else if (strstr(line, "set_plon"))
      params->lon_peg_point = atof(get_uavsar(line, "set_plon"));
    else if (strstr(line, "set_phdg"))
      params->head_peg_point = atof(get_uavsar(line, "set_phdg"));
    else if (strstr(line, "val_endi"))
      strcpy(params->endianess, get_uavsar(line, "val_endi"));
    else if (strstr(line, "val_mult"))
      params->data_scale = atof(get_uavsar(line, "val_mult"));
    else if (strstr(line, "val_addr"))
      params->data_shift = atof(get_uavsar(line, "val_addr"));
    else if (strstr(line, "val_minv"))
      params->min_value = atof(get_uavsar(line, "val_minv"));
    else if (strstr(line, "val_maxv"))
      params->max_value = atof(get_uavsar(line, "val_maxv"));
    else if (strstr(line, "Center Wavelength"))
      params->wavelength = atof(get_uavsar(line, "Center Wavelength"));
    else if (strstr(line, "Ellipsoid Semi-major Axis"))
      params->semi_major = atof(get_uavsar(line, "Ellipsoid Semi-major Axis"));
    else if (strstr(line, "Ellipsoid Eccentricity Squared"))
      params->eccentricity = 
	atof(get_uavsar(line, "Ellipsoid Eccentricity Squared"));
    else if (strstr(line, "Look Direction"))
      strcpy(params->look_direction, get_uavsar(line, "Look Direction"));
    else if (strstr(line, "Range Spacing per Bin"))
      params->range_spacing = atof(get_uavsar(line, "Range Spacing per Bin"));
    else if (strstr(line, "Azimuth Spacing"))
      params->azimuth_spacing = atof(get_uavsar(line, "Azimuth Spacing"));
    else if (strstr(line, "Image Starting Range"))
      params->slant_range_first_pixel = 
	atof(get_uavsar(line, "Image Starting Range"));
    else if (strstr(line, "Global Average Yaw"))
      params->yaw = atof(get_uavsar(line, "Global Average Yaw"));
    else if (strstr(line, "Global Average Pitch"))
      params->pitch = atof(get_uavsar(line, "Global Average Pitch"));
    else if (strstr(line, "Global Average Roll"))
      params->roll = atof(get_uavsar(line, "Global Average Roll"));
    else if (strstr(line, "Global Average Altitude"))
      params->altitude = atof(get_uavsar(line, "Global Average Altitude"));
    else if (strstr(line, "Global Average Terrain Height"))
      params->terrain_height = 
	atof(get_uavsar(line, "Global Average Terrain Height"));
    else if (strstr(line, "Global Average Squint Angle"))
      params->squint_angle = 
	atof(get_uavsar(line, "Global Average Squint Angle"));
    else if (strstr(line, "Pulse Length"))
      params->pulse_length = atof(get_uavsar(line, "Pulse Length"));
    else if (strstr(line, "Steering Angle (90 is Boresite)"))
      params->steering_angle = 
	atof(get_uavsar(line, "Steering Angle (90 is Boresite)"));
    else if (strstr(line, "Bandwidth"))
      params->bandwidth = atof(get_uavsar(line, "Bandwidth"));
    else if (strstr(line, "Approximate Upper Left Latitude"))
      params->lat_upper_left = 
	atof(get_uavsar(line, "Approximate Upper Left Latitude"));
    else if (strstr(line, "Approximate Upper Left Longitude"))
	params->lon_upper_left = 
	  atof(get_uavsar(line, "Approximate Upper Left Longitude"));
    else if (strstr(line, "Approximate Upper Right Latitude"))
      params->lat_upper_right = 
	atof(get_uavsar(line, "Approximate Upper Right Latitude"));
    else if (strstr(line, "Approximate Upper Right Longitude"))
	params->lon_upper_right = 
	  atof(get_uavsar(line, "Approximate Upper Right Longitude"));
    else if (strstr(line, "Approximate Lower Left Latitude"))
      params->lat_lower_left = 
	atof(get_uavsar(line, "Approximate Lower Left Latitude"));
    else if (strstr(line, "Approximate Lower Left Longitude"))
	params->lon_lower_left = 
	  atof(get_uavsar(line, "Approximate Lower Left Longitude"));
    else if (strstr(line, "Approximate Lower Right Latitude"))
      params->lat_lower_right = 
	atof(get_uavsar(line, "Approximate Lower Right Latitude"));
    else if (strstr(line, "Approximate Lower Right Longitude"))
	params->lon_lower_right = 
	  atof(get_uavsar(line, "Approximate Lower Right Longitude"));
    else if (strstr(line, "Date of Acquisition"))
      strcpy(params->acquisition_date, get_uavsar(line, "Date of Acquisition"));
    else if (strstr(line, "Processor Version Number"))
      strcpy(params->processor, get_uavsar(line, "Processor Version Number"));
  }
  FCLOSE(fp);

  return params;
}

static int sign(char byteBuf)
{
  if (byteBuf < 0)
    return -1;
  else
    return 1;
}

static void check_data_type(const char *inFileName, char *type)
{
  char line[255];
  FILE *fp = FOPEN(inFileName, "r");
  while (fgets(line, 255, fp)) {
    if (strstr(line, "Acquisition Mode"))
      strcpy(type, get_uavsar(line, "Acquisition Mode"));
    else if (strstr(line, "Processing Mode"))
      strcpy(type, get_uavsar(line, "Processing Mode"));
    if (strcmp_case(type, "RPI") == 0)
      sprintf(type, "InSAR");
  }
  FCLOSE(fp);
}

void import_uavsar(const char *inFileName, radiometry_t radiometry,
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
  int ii, kk, ll, nn, nBands, ns, nl, *dataType;
  long long offset;
  float *floatAmp, *floatPhase, *floatAmpBuf, *amp, re, im;
  float *floatComplexReal, *floatComplexImag;
  float *floatComplexBuf;
  char **dataName, **element, tmp[50], type[10];
  char *outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
  uavsar_polsar *polsar_params;
  uavsar_insar *insar_params;
  meta_parameters *metaIn, *metaOut;

  check_data_type(inFileName, type);
  asfPrintStatus("   Data type: %s\n", type);

  // InSAR data
  // Slant range interferogram
  if (strcmp_case(data_type, "INT") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_INT, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_INT);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaIn->general->sample_count;
    nl = metaIn->general->line_count;
    nn = 0;
    floatAmp = (float *) CALLOC(ns, sizeof(float));
    floatPhase = (float *) CALLOC(ns, sizeof(float));
    floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    metaOut->general->band_count = 2;
    if (strcmp_case(data_type, "INT") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_int.img");
    asfPrintStatus("\nSlant range interferogram:\n");
    fpOut = FOPEN(outName, "wb");
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "UNW") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_UNW, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_UNW);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "UNW") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_unw.img");
    asfPrintStatus("\nSlant range unwrapped phase:\n");
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "COR") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_COR, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_COR);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "COR") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_cor.img");
    asfPrintStatus("\nSlant range correlation image:\n");
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "AMP") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_AMP, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_AMP);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    metaOut->general->band_count = 2;
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "AMP") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_amp.img");
    fpOut = FOPEN(outName, "wb");
    strcpy(metaOut->general->bands, "AMP1,AMP2");
    asfPrintStatus("\nSlant range amplitude images:\n");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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

  // Ground range interferogram
  if (strcmp_case(data_type, "INT_GRD") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_INT_GRD, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_INT_GRD);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaIn->general->sample_count;
    nl = metaIn->general->line_count;
    nn = 0;
    floatAmp = (float *) CALLOC(ns, sizeof(float));
    floatPhase = (float *) CALLOC(ns, sizeof(float));
    floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    metaOut->general->band_count = 2;
    if (strcmp_case(data_type, "INT_GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_int_grd.img");
    asfPrintStatus("\nGround range interferogram:\n");
    fpOut = FOPEN(outName, "wb");
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "UNW_GRD") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_UNW_GRD, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_UNW_GRD);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "UNW_GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_unw_grd.img");
    asfPrintStatus("\nGround range unwrapped phase:\n");
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "COR_GRD") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_COR_GRD, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_COR_GRD);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    if (strcmp_case(data_type, "COR_GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_cor_grd.img");
    asfPrintStatus("\nGround range correlation image:\n");
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
    FREE(floatAmpBuf);
    FREE(outName);
    meta_write(metaOut, outName);
    meta_free(metaIn);
    meta_free(metaOut);
    FREE(insar_params);
  }    

  // Ground range amplitude images
  if (strcmp_case(data_type, "AMP_GRD") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_AMP_GRD, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_AMP_GRD);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    metaOut->general->band_count = 2;
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "AMP_GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_amp_grd.img");
    fpOut = FOPEN(outName, "wb");
    strcpy(metaOut->general->bands, "AMP1,AMP2");
    asfPrintStatus("\nGround range amplitude images:\n");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "HGT_GRD") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "InSAR") == 0)) {
    get_uavsar_file_names(inFileName, INSAR_HGT_GRD, &dataName, &element,
			  &dataType, &nBands);
    insar_params = 
      read_uavsar_insar_params(inFileName, INSAR_HGT_GRD);
    metaIn = uavsar_insar2meta(insar_params);
    metaOut = uavsar_insar2meta(insar_params);
    ns = metaOut->general->sample_count;
    nn = 0;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "HGT_GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_hgt_grd.img");
    fpOut = FOPEN(outName, "wb");
    strcpy(metaOut->general->bands, "HEIGHT");
    asfPrintStatus("\nGround range digital elevation model:\n");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "SLC") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "PolSAR") == 0)) {
    asfPrintWarning("Ingest of SLC data is currently not supported!\n");
    get_uavsar_file_names(inFileName, POLSAR_SLC, &dataName, &element,
			  &dataType, &nBands);
    polsar_params = 
      read_uavsar_polsar_params(inFileName, POLSAR_SLC);
    metaIn = uavsar_polsar2meta(polsar_params);
    metaOut = uavsar_polsar2meta(polsar_params);
    if (strcmp_case(data_type, "SLC") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_slc.img");
    for (ii=0; ii<nBands; ii++)
      printf("file: %s\n", dataName[ii]);
    //meta_write(metaOut, outName);
    meta_free(metaIn);
    meta_free(metaOut);
    FREE(polsar_params);
  }

  // Multilooked data
  if (strcmp_case(data_type, "MLC") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "PolSAR") == 0)) {
    get_uavsar_file_names(inFileName, POLSAR_MLC, &dataName, &element,
			  &dataType, &nBands);
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
    if (strcmp_case(data_type, "MLC") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_mlc.img");
    asfPrintStatus("\nMultilooked data:\n");
    fpOut = FOPEN(outName, "wb");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "DAT") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "PolSAR") == 0)) {
    get_uavsar_file_names(inFileName, POLSAR_DAT, &dataName, &element,
			  &dataType, &nBands);
    polsar_params = 
      read_uavsar_polsar_params(inFileName, POLSAR_DAT);
    metaIn = uavsar_polsar2meta(polsar_params);
    metaOut = uavsar_polsar2meta(polsar_params);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "DAT") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_dat.img");
    metaOut->general->band_count = 9;
    asfPrintStatus("\nCompressed Stokes matrix:\n");
    asfPrintStatus("Ingesting %s ...\n", dataName[0]);
    if (radiometry == r_AMP)
      strcpy(metaOut->general->bands,
	     "AMP,AMP_HH,PHASE_HH,AMP_HV,PHASE_HV,AMP_VH,PHASE_VH,"\
	     "AMP_VV,PHASE_VV");
    else if (radiometry == r_SIGMA)
      strcpy(metaOut->general->bands,
	     "AMP,SIGMA-AMP-HH,SIGMA-PHASE-HH,SIGMA-AMP-HV,SIGMA-PHASE-HV,"\
	     "SIGMA-AMP-VH,SIGMA-PHASE-VH,SIGMA-AMP-VV,SIGMA-PHASE-VV");
    else if (radiometry == r_SIGMA_DB)
      strcpy(metaOut->general->bands,
	     "AMP,SIGMA_DB-AMP-HH,SIGMA_DB-PHASE-HH,SIGMA_DB-AMP-HV,"\
	     "SIGMA_DB-PHASE-HV,SIGMA_DB-AMP-VH,SIGMA_DB-PHASE-VH,"\
	     "SIGMA_DB-AMP-VV,SIGMA_DB-PHASE-VV");
    int ns = metaOut->general->sample_count;
    float total_power, ysca, amp, phase;
    float m11, m12, m13, m14, m22, m23, m24, m33, m34, m44;
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
	// Scale is always 1.0 according to Bruce Chapman
	m11 = ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
	m12 = (float)byteBuf[2] * m11 / 127.0;
	m13 = sign(byteBuf[3]) * SQR((float)byteBuf[3] / 127.0) * m11;
	m14 = sign(byteBuf[4]) * SQR((float)byteBuf[4] / 127.0) * m11;
	m23 = sign(byteBuf[5]) * SQR((float)byteBuf[5] / 127.0) * m11;
	m24 = sign(byteBuf[6]) * SQR((float)byteBuf[6] / 127.0) * m11;
	m33 = (float)byteBuf[7] * m11 / 127.0;
	m34 = (float)byteBuf[8] * m11 / 127.0;
	m44 = (float)byteBuf[9] * m11 / 127.0;
	m22 = 1 - m33 -m44;
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
    FREE(outName);
    meta_write(metaOut, outName);
    meta_free(metaIn);
    meta_free(metaOut);
    FREE(polsar_params);
  }

  // Ground range projected data
  if (strcmp_case(data_type, "GRD") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "PolSAR") == 0)) {
    get_uavsar_file_names(inFileName, POLSAR_GRD, &dataName, &element,
			  &dataType, &nBands);
    polsar_params = 
      read_uavsar_polsar_params(inFileName, POLSAR_GRD);
    metaIn = uavsar_polsar2meta(polsar_params);
    metaOut = uavsar_polsar2meta(polsar_params);
    ns = metaIn->general->sample_count;
    nl = metaIn->general->line_count;
    floatAmpBuf = (float *) CALLOC(ns, sizeof(float));
    floatComplexReal = (float *) CALLOC(ns, sizeof(float));
    floatComplexImag = (float *) CALLOC(ns, sizeof(float));
    floatComplexBuf = (float *) CALLOC(2*ns, sizeof(float));
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    metaOut->general->band_count = ll = 0;
    if (strcmp_case(data_type, "GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_grd.img");
    asfPrintStatus("\nGround range projected data:\n");
    fpOut = FOPEN(outName, "wb");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
  if (strcmp_case(data_type, "HGT") == 0 ||
      (strcmp_case(data_type, "ALL") == 0 && 
       strcmp_case(type, "PolSAR") == 0)) {
    get_uavsar_file_names(inFileName, POLSAR_HGT, &dataName, &element,
			  &dataType, &nBands);
    polsar_params = 
      read_uavsar_polsar_params(inFileName, POLSAR_HGT);
    metaIn = uavsar_polsar2meta(polsar_params);
    metaOut = uavsar_polsar2meta(polsar_params);
    ns = metaOut->general->sample_count;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+15));
    if (strcmp_case(data_type, "HGT") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_hgt.img");
    asfPrintStatus("\nDigital elevation model:\n");
    nn=0;
    asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
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
