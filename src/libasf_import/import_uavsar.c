#include "uavsar.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include <ctype.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

#define SQR(x) (x*x)

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
  char *file = (char *) MALLOC(sizeof(char)*255);
  strncpy(file, p+1, 60);
  *fileName = trim_spaces(file);
  FREE(file);
  p = strchr(line, ';');
  int size;
  sscanf(p+12, "%d", &size);
  if (fileExists(*fileName) && fileSize(*fileName) == size)
    return TRUE;
  else
    return FALSE;
}

static void get_uavsar_file_names(const char *dataFile, uavsar_type_t type, 
				  char ***pDataName, char ***pPolarization,
				  int **pDataType, int *nBands)
{
  char *file = (char *) MALLOC(sizeof(char)*1024);
  int ii, slc=0, mlc=0, dat=0, grd=0, hgt=0; 

  char **dataName = (char **) MALLOC(6*sizeof(char *));
  char **polarization = (char **) MALLOC(6*sizeof(char *));
  int *dataType = (int *) MALLOC(6*sizeof(int));
  for (ii=0; ii<6; ii++) {
    dataName[ii] = (char *) MALLOC(sizeof(char)*512);
    polarization[ii] = (char *) MALLOC(sizeof(char)*10);
  }
  *pDataName = dataName;
  *pPolarization = polarization;
  *pDataType = dataType;
  *nBands = 0;

  char line[255];
  FILE *fp = FOPEN(dataFile, "r");
  while (fgets(line, 255, fp)) {
    if (type == POLSAR_SLC) {
      if (strstr(line, "slcHH   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(polarization[slc], "HH");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      else if (strstr(line, "slcHV   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(polarization[slc], "HV");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      else if (strstr(line, "slcVH   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(polarization[slc], "VH");
	  dataType[slc] = 1;
	  slc++;
	}
      }
      else if (strstr(line, "slcVV   =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[slc], file);
	  strcpy(polarization[slc], "VV");
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
	  strcpy(polarization[mlc], "HHHH");
	  dataType[mlc] = 0;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHVHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(polarization[mlc], "HVHV");
	  dataType[mlc] = 0;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcVVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(polarization[mlc], "VVVV");
	  dataType[mlc] = 0;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHHHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(polarization[mlc], "HHHV");
	  dataType[mlc] = 1;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHHVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(polarization[mlc], "HHVV");
	  dataType[mlc] = 1;
	  mlc++;
	}
      }
      else if (strstr(line, "mlcHVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[mlc], file);
	  strcpy(polarization[mlc], "HVVV");
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
	  strcpy(polarization[grd], "HHHH");
	  dataType[grd] = 0;
	  grd++;
	}
      }
      else if (strstr(line, "grdHVHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(polarization[grd], "HVHV");
	  dataType[grd] = 0;
	  grd++;
	}
      }
      else if (strstr(line, "grdVVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(polarization[grd], "VVVV");
	  dataType[grd] = 0;
	  grd++;
	}
      }
      else if (strstr(line, "grdHHHV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(polarization[grd], "HHHV");
	  dataType[grd] = 1;
	  grd++;
	}
      }
      else if (strstr(line, "grdHHVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(polarization[grd], "HHVV");
	  dataType[grd] = 1;
	  grd++;
	}
      }
      else if (strstr(line, "grdHVVV =")) {
	if (check_file(line, &file)) {
	  strcpy(dataName[grd], file);
	  strcpy(polarization[grd], "HVVV");
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
	  strcpy(polarization[hgt], "HHHH");
	  dataType[hgt] = 0;
	  hgt++;
	}
      }
      *nBands = hgt;
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

void import_uavsar(const char *inFileName, radiometry_t radiometry,
		   const char *data_type, const char *outBaseName) {

  // UAVSAR comes in two flavors: InSAR and PolSAR
  // Things look similar to AirSAR data, just organized a little different.
  // There does not seem to be a consistent identifier in the annotation file,
  // that would allow us to easily identify the data set as UAVSAR. No mention
  // of UAVSAR whatsoever.

  // The data can come in a large variety of flavors (single look versus multi-
  // look, derived magnitude and phase, etc.). I assume we can anything or
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

  FILE *fpIn, *fpOut;
  int ii, kk, ll, nn, nBands, ns, *dataType;
  float *floatAmpBuf, *floatPhaseBuf, *floatComplexBuf;
  complexFloat cpx;
  char **dataName, **polarization, tmp[50];
  char *outName = (char *) MALLOC(sizeof(char)*(strlen(outBaseName)+5));
  uavsar_polsar *params;
  meta_parameters *metaIn, *metaOut;

  // Single look complex data
  if (strcmp_case(data_type, "SLC") == 0 ||
      strcmp_case(data_type, "ALL") == 0) {
    get_uavsar_file_names(inFileName, POLSAR_SLC, &dataName, &polarization,
			  &dataType, &nBands);
    params = 
      read_uavsar_polsar_params(inFileName, POLSAR_SLC);
    metaIn = uavsar_polsar2meta(params);
    metaOut = uavsar_polsar2meta(params);
    if (strcmp_case(data_type, "SLC") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_slc.img");
    for (ii=0; ii<nBands; ii++)
      printf("file: %s\n", dataName[ii]);
    meta_write(metaOut, outName);
    meta_free(metaIn);
    meta_free(metaOut);
  }

  // Multilooked data
  if (strcmp_case(data_type, "MLC") == 0 ||
      strcmp_case(data_type, "ALL") == 0) {
    get_uavsar_file_names(inFileName, POLSAR_MLC, &dataName, &polarization,
			  &dataType, &nBands);
    params = 
      read_uavsar_polsar_params(inFileName, POLSAR_MLC);
    metaIn = uavsar_polsar2meta(params);
    metaOut = uavsar_polsar2meta(params);
    ns = metaOut->general->sample_count;
    floatAmpBuf = (float *) MALLOC(sizeof(float)*ns);
    floatPhaseBuf = (float *) MALLOC(sizeof(float)*ns);
    floatComplexBuf = (float *) MALLOC(sizeof(float)*ns*2);
    metaOut->general->band_count = ll = 0;
    if (strcmp_case(data_type, "MLC") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_mlc.img");
    asfPrintStatus("\nMultilooked data:\n");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
      if (dataType[nn] == 0)
	metaOut->general->band_count += 1;
      else
	metaOut->general->band_count += 2;
      fpIn = FOPEN(dataName[nn], "rb");
      if (nn == 0) {
	fpOut = FOPEN(outName, "wb");
	if (dataType[nn])
	  sprintf(metaOut->general->bands, "%s-AMP,%s-PHASE", polarization[nn],
		  polarization[nn]);
	else
	  sprintf(metaOut->general->bands, "%s", polarization[nn]);
      }
      else {
	fpOut = FOPEN(outName, "ab");
	if (dataType[nn])
	  sprintf(tmp, ",%s-AMP,%s-PHASE", polarization[nn], polarization[nn]);
	else
	  sprintf(tmp, ",%s", polarization[nn]);
	strcat(metaOut->general->bands, tmp);
      }
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	if (dataType[nn]) {
	  metaIn->general->sample_count = ns*2;
	  get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	}
	else {
	  metaIn->general->sample_count = ns;
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	}
	for (kk=0; kk<ns; kk++) {
	  if (dataType[nn]) {
	    ieee_big32(floatComplexBuf[kk*2]);
	    ieee_big32(floatComplexBuf[kk*2+1]);
	    cpx.real = floatComplexBuf[kk*2];
	    cpx.imag = floatComplexBuf[kk*2+1];
	    floatAmpBuf[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	    floatPhaseBuf[kk] = atan2(cpx.imag, cpx.real);
	  }
	  else
	    ieee_big32(floatAmpBuf[kk]);
	}
	if (dataType[nn]) {
	  put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	  put_band_float_line(fpOut, metaOut, ll+1, ii, floatPhaseBuf);
	}
	else 
	  put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);      
      }
      if (dataType[nn])
	ll += 2;
      else
	ll++;
      FCLOSE(fpIn);
      FCLOSE(fpOut);
    }
    meta_write(metaOut, outName);
    FREE(floatAmpBuf);
    FREE(floatPhaseBuf);
    FREE(floatComplexBuf);
    meta_free(metaIn);
    meta_free(metaOut);
  }    

  // Compressed Stokes matrix
  if (strcmp_case(data_type, "DAT") == 0 ||
      strcmp_case(data_type, "ALL") == 0) {
    get_uavsar_file_names(inFileName, POLSAR_DAT, &dataName, &polarization,
			  &dataType, &nBands);
    params = 
      read_uavsar_polsar_params(inFileName, POLSAR_DAT);
    metaIn = uavsar_polsar2meta(params);
    metaOut = uavsar_polsar2meta(params);
    if (strcmp_case(data_type, "DAT") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_dat.img");
    metaOut->general->band_count = ll = 0;
    asfPrintStatus("\nCompress Stokes matrix:\n");
    for (ii=0; ii<nBands; ii++)
      printf("file: %s\n", dataName[ii]);
    meta_write(metaOut, outName);
  }

  // Ground range projected data
  if (strcmp_case(data_type, "GRD") == 0 ||
      strcmp_case(data_type, "ALL") == 0) {
    get_uavsar_file_names(inFileName, POLSAR_GRD, &dataName, &polarization,
			  &dataType, &nBands);
    params = 
      read_uavsar_polsar_params(inFileName, POLSAR_GRD);
    metaIn = uavsar_polsar2meta(params);
    metaOut = uavsar_polsar2meta(params);
    metaOut->general->band_count = ll = 0;
    if (strcmp_case(data_type, "GRD") == 0)
      outName = appendExt(outBaseName, ".img");
    else if (strcmp_case(data_type, "ALL") == 0)
      outName = appendToBasename(outBaseName, "_grd.img");
    asfPrintStatus("\nGround range projected data:\n");
    for (nn=0; nn<nBands; nn++) {
      asfPrintStatus("Ingesting %s ...\n", dataName[nn]);
      if (dataType[nn] == 0)
	metaOut->general->band_count += 1;
      else
	metaOut->general->band_count += 2;
      fpIn = FOPEN(dataName[nn], "rb");
      if (nn == 0) {
	fpOut = FOPEN(outName, "wb");
	if (dataType[nn])
	  sprintf(metaOut->general->bands, "%s-AMP,%s-PHASE", polarization[nn],
		  polarization[nn]);
	else
	  sprintf(metaOut->general->bands, "%s", polarization[nn]);
      }
      else {
	fpOut = FOPEN(outName, "ab");
	if (dataType[nn])
	  sprintf(tmp, ",%s-AMP,%s-PHASE", polarization[nn], polarization[nn]);
	else
	  sprintf(tmp, ",%s", polarization[nn]);
	strcat(metaOut->general->bands, tmp);
      }
      for (ii=0; ii<metaIn->general->line_count; ii++) {
	if (dataType[nn]) {
	  metaIn->general->sample_count = ns*2;
	  get_float_line(fpIn, metaIn, ii, floatComplexBuf);
	}
	else {
	  metaIn->general->sample_count = ns;
	  get_float_line(fpIn, metaIn, ii, floatAmpBuf);
	}
	for (kk=0; kk<ns; kk++) {
	  if (dataType[nn]) {
	    ieee_big32(floatComplexBuf[kk*2]);
	    ieee_big32(floatComplexBuf[kk*2+1]);
	    cpx.real = floatComplexBuf[kk*2];
	    cpx.imag = floatComplexBuf[kk*2+1];
	    floatAmpBuf[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
	    floatPhaseBuf[kk] = atan2(cpx.imag, cpx.real);
	  }
	  else
	    ieee_big32(floatAmpBuf[kk]);
	}
	if (dataType[nn]) {
	  put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	  put_band_float_line(fpOut, metaOut, ll+1, ii, floatPhaseBuf);
	}
	else 
	  put_band_float_line(fpOut, metaOut, ll, ii, floatAmpBuf);
	asfLineMeter(ii, metaIn->general->line_count);      
      }
      if (dataType[nn])
	ll += 2;
      else
	ll++;
      FCLOSE(fpIn);
      FCLOSE(fpOut);
    }
    meta_write(metaOut, outName);
    meta_free(metaIn);
    meta_free(metaOut);
  }

  // Digital elevation model
  if (strcmp_case(data_type, "HGT") == 0 ||
      strcmp_case(data_type, "ALL") == 0) {
    get_uavsar_file_names(inFileName, POLSAR_HGT, &dataName, &polarization,
			  &dataType, &nBands);
    params = 
      read_uavsar_polsar_params(inFileName, POLSAR_HGT);
    metaIn = uavsar_polsar2meta(params);
    metaOut = uavsar_polsar2meta(params);
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
    meta_free(metaIn);
    meta_free(metaOut);
  }    

  // InSAR data
  // We currently don't have InSAR in the archive. This is a placeholder
  // for later.

  FREE(outName);
}
