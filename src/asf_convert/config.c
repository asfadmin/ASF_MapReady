#include "asf.h"
#include "asf_convert.h"
#include "asf_reporting.h"
#include <ctype.h>

int warnFlag = FALSE;

int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

char *read_param(char *line)
{
  int i, k;
  char *value=(char *)MALLOC(sizeof(char)*255);

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k-1);
  return value;
}

char *read_str(char *line, char *block, char *param)
{
  char tmp[255], *start;
  char *value=(char *)MALLOC(sizeof(char)*255);

  start = strchr(line, '<');
  if (start) {
    asfPrintWarning("Invalid value for parameter '%s' in block [%s]\n",
                    param, block);
    warnFlag = TRUE;
    strcpy(value, "");
    return value;
  }
  else {
    start = strchr(line, '=');
    sscanf(start, "%s %s", tmp, value);
    return value;
  }
}

int read_int(char *line, char *block, char *param)
{
  char *tmp=NULL;
  int value;

  tmp = read_str(line, block, param);
  if (!sscanf(tmp, "%i", &value)) {
    asfPrintWarning("Invalid value for parameter '%s' in block [%s]\n",
                    param, block);
    warnFlag = TRUE;
    return 0;
  }
  else return value;
}

double read_double(char *line, char *block, char *param)
{
  char *tmp=NULL;
  double value;

  tmp = read_str(line, block, param);
  if (!sscanf(tmp, "%lf", &value)) {
    asfPrintWarning("Invalid value for parameter '%s' in block [%s]\n",
                    param, block);
    warnFlag = TRUE;
    return 0.0;
  }
  else return value;
}

char *str2upper(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;

  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';

  return out;
}

int init_config(char *configFile)
{
  FILE *fConfig;

  fConfig = FOPEN(configFile, "w");

  fprintf(fConfig, "asf_convert configuration file\n\n");
  fprintf(fConfig, "[General]\n");
  fprintf(fConfig,
          "input data file = < name of input data file (with extension) >\n");
  fprintf(fConfig, "input metadata file = < name of input metadata file "
          "(with extension) >\n");
  fprintf(fConfig, "input format = < CEOS | ASF >\n");
  fprintf(fConfig, "data type = < amplitude | power | sigma | gamma | "
          "beta >\n");
  fprintf(fConfig, "output file = < basename of the output file >\n");
  fprintf(fConfig, "output format = < ASF | GEOTIFF | JPEG | ENVI | ESRI "
          "| PPM | PNG >\n");
  fprintf(fConfig, "resampling = < flag for subsampling: 0 | 1 >\n");
  fprintf(fConfig, "browse image = < flag for subsampling to browse image size: "
          "0 | 1 >\n");
  fprintf(fConfig, "geocoding = < flag for geocoding: 0 | 1 >\n");
  fprintf(fConfig, "batch mode = < flag for batch mode processing: 0 | 1 >\n");
  fprintf(fConfig, "batch file = < batch file name >\n");
  fprintf(fConfig, "log file = < log file name >\n\n");

  FCLOSE(fConfig);

  asfPrintStatus("   Initialized basic configuration file\n\n");

  return(0);
}

int check_geocode_flag(char *configFile)
{
  FILE *fConfig;
  int geocode_flag=0;
  char line[255], *test;

  test=(char *)MALLOC(sizeof(char)*255);
  fConfig = FOPEN(configFile, "r");
  while (fgets(line, 255, fConfig) != NULL) {
    test = read_param(line);
    if (strncmp(test, "geocoding", 9)==0)
      if (strncmp(line, "geocoding = 1", 13)==0)
        geocode_flag = 1;
    if (strncmp(line, "[Geocoding]", 11)==0 && geocode_flag)
      return (0);
  }
  if (geocode_flag) return(1);
  else return(0);
}

int check_resample_flag(char *configFile)
{
  FILE *fConfig;
  int resample_flag=0;
  char line[255], *test;

  test=(char *)MALLOC(sizeof(char)*255);
  fConfig = FOPEN(configFile, "r");
  while (fgets(line, 255, fConfig) != NULL) {
    test = read_param(line);
    if (strncmp(test, "resampling", 10)==0)
      if (strncmp(line, "resampling = 1", 14)==0)
        resample_flag = 1;
    if (strncmp(line, "[Resampling]", 12)==0 && resample_flag)
      return(0);
  }
  if (resample_flag) return(1);
  else return(0);
}

int init_projection_config(char *configFile)
{
  FILE *fConfig;

  fConfig = FOPEN(configFile, "a");

  fprintf(fConfig, "[Geocoding]\n");
  fprintf(fConfig, "projection = < UTM | Polar Sterographic | "
          "Albers Conic Equal Area | Lambert Conformal Conic >\n");
  fprintf(fConfig, "pixel spacing = < pixel spacing of geocoded image >\n");
  fprintf(fConfig, "height = < average height of the data (default: 0) >\n");
  fprintf(fConfig, "background fill = < pixel value for the background >\n\n");
  fprintf(fConfig, "[UTM]\n");
  fprintf(fConfig, "datum = < datum code >\n");
  fprintf(fConfig, "zone number = < UTM zone number >\n");
  fprintf(fConfig, "units = < meters | degrees >\n\n");
  fprintf(fConfig, "[Polar Stereographic]\n");
  fprintf(fConfig, "datum = < datum code >\n");
  fprintf(fConfig, "center latitude = < decimal degrees >\n");
  fprintf(fConfig, "center longitude = < decimal degrees >\n");
  fprintf(fConfig, "units = < meters | degrees >\n\n");
  fprintf(fConfig, "[Albers Conic Equal Area]\n");
  fprintf(fConfig, "datum = < datum code >\n");
  fprintf(fConfig, "first standard parallel = < decimal degrees >\n");
  fprintf(fConfig, "second standard parallel = < decimal degrees >\n");
  fprintf(fConfig, "central meridian = < decimal degrees >");
  fprintf(fConfig, "latitude of origin = < decimal degrees >\n");
  fprintf(fConfig, "units = < meters | degrees >\n\n");
  /* Temporarily out until we fix geocode *
  fprintf(fConfig, "[Lambert Azimuthal Equal Area]\n");
  fprintf(fConfig, "datum = < datum code >\n");
  fprintf(fConfig, "center latitude = < decimal degrees >\n");
  fprintf(fConfig, "center longitude = < decimal degrees >\n");
  fprintf(fConfig, "units = < meters | degrees >\n\n");
  ******************************************************/
  fprintf(fConfig, "[Lambert Conformal Conic]\n");
  fprintf(fConfig, "datum = < datum code >\n");
  fprintf(fConfig, "first standard parallel = < decimal degrees >\n");
  fprintf(fConfig, "second standard parallel = < decimal degrees >\n");
  fprintf(fConfig, "central meridian = < decimal degrees >\n");
  fprintf(fConfig, "latitude of origin = < decimal degrees >\n");
  fprintf(fConfig, "units = < meters | degrees >\n\n");

  FCLOSE(fConfig);

  printf("   Initialized extended geocoding configuration file\n\n");

  return(0);
}

int init_resample_config(char *configFile)
{
  FILE *fConfig;

  fConfig = FOPEN(configFile, "a");

  fprintf(fConfig, "[Resampling]\n");
  fprintf(fConfig, "subsampling factor = < factor for reducing the output file "
          "dimensions >\n\n");

  FCLOSE(fConfig);

  printf("   Initialized extended resampling configuration file\n\n");

  return(0);
}

s_config *init_cfg(void)
{
#define newStruct(type) (type *)MALLOC(sizeof(type))

  /* Create structure */
  s_config *cfg = newStruct(s_config);
  cfg->general = newStruct(s_general);
  cfg->geocoding = newStruct(s_geocoding);
  cfg->resampling = newStruct(s_resampling);
  cfg->polar = newStruct(s_azimuthal);
  cfg->utm = newStruct(s_utm);
  cfg->albers = newStruct(s_conic);
  cfg->lambert_az = newStruct(s_azimuthal);
  cfg->lambert_cc = newStruct(s_conic);

  /* initialize structure */
  strcpy(cfg->comment, "asf_convert configuration file");

  cfg->general->in_data_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->in_data_name, "");
  cfg->general->in_meta_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->in_meta_name, "");
  cfg->general->in_format = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->general->in_format, "CEOS");
  cfg->general->data_type = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->general->data_type, "amplitude");
  cfg->general->out_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->out_name, "");
  cfg->general->out_format = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->out_format, "JPEG");
  cfg->general->resample = 0;
  cfg->general->browse = 0;
  cfg->general->geocoding = 0;
  cfg->general->batch = 0;
  cfg->general->batchFile = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->batchFile, "");
  cfg->general->logFile = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->logFile, "");

  cfg->geocoding->projection = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->geocoding->projection, "UTM");
  cfg->geocoding->background = 0;
  cfg->geocoding->pixel = 12.5;
  cfg->geocoding->height = 0.0;

  cfg->resampling->kernel = 3;

  cfg->polar->datum = 0;
  cfg->polar->center_lon = 0.0;
  cfg->polar->center_lat = 0.0;
  cfg->polar->units = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->polar->units, "");

  cfg->utm->datum = 0;
  cfg->utm->zone = 0;

  cfg->utm->units = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->utm->units, "");

  cfg->albers->datum = 0;
  cfg->albers->first_parallel = 0.0;
  cfg->albers->second_parallel = 0.0;
  cfg->albers->center_meridian = 0.0;
  cfg->albers->orig_latitude = 0.0;
  cfg->albers->units = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->albers->units, "");

  cfg->lambert_az->datum = 0;
  cfg->lambert_az->center_lat = 0.0;
  cfg->lambert_az->center_lon = 0.0;
  cfg->lambert_az->units = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->lambert_az->units, "");

  cfg->lambert_cc->datum = 0;
  cfg->lambert_cc->first_parallel = 0.0;
  cfg->lambert_cc->second_parallel = 0.0;
  cfg->lambert_cc->center_meridian = 0.0;
  cfg->lambert_cc->orig_latitude = 0.0;
  cfg->lambert_cc->units = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->lambert_cc->units, "");

  return cfg;
}

s_config *init_fill_config(char *configFile)
{
  FILE *fConfig;
  s_config *cfg;
  int i;
  char line[255], block[50];
  char *test=(char *)MALLOC(sizeof(char)*255);

  cfg = init_cfg();

  fConfig = FOPEN(configFile, "r");
  i=0;
  while (fgets(line, 255, fConfig) != NULL) {
    if (i==0) strcpy(cfg->comment, line);
    i++;

    if (strncmp(line, "[General]", 9)==0) {
      strcpy(block, "General");
      test = read_param(line);
      if (strncmp(test, "input data file", 15)==0)
        cfg->general->in_data_name = read_str(line, block, "input data file");
      if (strncmp(test, "input metadata file", 19)==0)
        cfg->general->in_meta_name = read_str(line, block, "input metadata file");
      if (strncmp(test, "input format", 12)==0)
        cfg->general->in_format = read_str(line, block, "input format");
      if (strncmp(test, "data type", 9)==0)
        cfg->general->data_type = read_str(line, block, "data type");
      if (strncmp(test, "output file", 11)==0)
        cfg->general->out_name = read_str(line, block, "output file");
      if (strncmp(test, "output format", 13)==0)
        cfg->general->out_format = read_str(line, block, "output format");
      if (strncmp(test, "resampling", 10)==0)
        cfg->general->resample = read_int(line, block, "resampling");
      if (strncmp(test, "browse image", 12)==0)
        cfg->general->browse = read_int(line, block, "browse image");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, block, "geocoding");
      if (strncmp(test, "log file", 8)==0)
        cfg->general->logFile = read_str(line, block, "log file");
    }
  }
  FCLOSE(fConfig);

  return cfg;
}

s_config *read_config(char *configFile)
{
  FILE *fConfig;
  s_config *cfg=NULL;
  char line[255], block[50];
  char *test=(char *)MALLOC(sizeof(char)*255);

  cfg = init_fill_config(configFile);
  if (cfg == NULL) check_return(1, "Creating configuration structure.\n");
  fConfig = FOPEN(configFile, "r");
  while (fgets(line, 255, fConfig) != NULL) {

    if (strncmp(line, "[General]", 9)==0) strcpy(block, "General");
    if (strncmp(block, "General", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "input data file", 15)==0)
        cfg->general->in_data_name = read_str(line, block, "input data file");
      if (strncmp(test, "input metadata file", 19)==0)
        cfg->general->in_meta_name = read_str(line, block, "input metadata file");
      if (strncmp(test, "input format", 12)==0)
        cfg->general->in_format = read_str(line, block, "input format");
      if (strncmp(test, "data type", 9)==0)
        cfg->general->data_type = read_str(line, block, "data type");
      if (strncmp(test, "output file", 11)==0)
        cfg->general->out_name = read_str(line, block, "output file");
      if (strncmp(test, "output format", 13)==0)
        cfg->general->out_format = read_str(line, block, "output format");
      if (strncmp(test, "resampling", 10)==0)
        cfg->general->resample = read_int(line, block, "resampling");
      if (strncmp(test, "browse image", 12)==0)
        cfg->general->browse = read_int(line, block, "browse image");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, block, "geocoding");
      if (strncmp(test, "batch mode", 10)==0)
        cfg->general->batch = read_int(line, block, "batch mode");
      if (strncmp(test, "batch file", 10)==0)
        cfg->general->batchFile = read_str(line, block, "batch file");
      if (strncmp(test, "log file", 8)==0)
        cfg->general->logFile = read_str(line, block, "log file");
    }

    if (strncmp(line, "[Geocoding]", 11)==0) strcpy(block, "Geocoding");
    if (strncmp(block, "Geocoding", 9)==0) {
      test = read_param(line);
      if (strncmp(test, "projection", 10)==0)
        cfg->geocoding->projection = read_str(line, block, "projection");
      if (strncmp(test, "background fill", 15)==0)
        cfg->geocoding->background = read_int(line, block, "background fill");
      if (strncmp(test, "pixel spacing", 13)==0)
        cfg->geocoding->pixel = read_double(line, block, "pixel spacing");
      if (strncmp(test, "height", 6)==0)
        cfg->geocoding->height = read_double(line, block, "height");
    }

    if (strncmp(line, "[Polar Stereographic]", 21)==0)
      strcpy(block, "Polar Stereographic");
    if (strncmp(block, "Polar Stereographic", 19)==0) {
      test = read_param(line);
      if (strncmp(test, "datum", 5)==0)
        cfg->polar->datum = read_int(line, block, "datum");
      if (strncmp(test, "center latitude", 16)==0)
        cfg->polar->center_lat = read_double(line, block, "center latitude");
      if (strncmp(test, "center longitude", 17)==0)
        cfg->polar->center_lon = read_double(line, block, "center longitude");
      if (strncmp(test, "units", 5)==0)
        cfg->polar->units = read_str(line, block, "units");
    }

    if (strncmp(line, "[UTM]", 5)==0) strcpy(block, "UTM");
    if (strncmp(block, "UTM", 3)==0) {
      test = read_param(line);
      if (strncmp(test, "datum", 5)==0)
        cfg->utm->datum = read_int(line, block, "datum");
      if (strncmp(test, "zone number", 11)==0)
        cfg->utm->zone = read_int(line, block, "zone number");
      if (strncmp(test, "units", 5)==0)
        cfg->utm->units = read_str(line, block, "units");
    }

    if (strncmp(line, "[Albers Conic Equal Area]", 25)==0)
      strcpy(block, "Albers Conic Equal Area");
    if (strncmp(block, "Albers Conic Equal Area", 23)==0) {
      test = read_param(line);
      if (strncmp(test, "datum", 5)==0)
        cfg->albers->datum = read_int(line, block, "datum");
      if (strncmp(test, "first standard parallel", 23)==0)
        cfg->albers->first_parallel =
          read_double(line, block, "first standard parallel");
      if (strncmp(test, "second standard parallel", 24)==0)
        cfg->albers->second_parallel =
          read_double(line, block, "second standard parallel");
      if (strncmp(test, "central meridian", 16)==0)
        cfg->albers->center_meridian = read_double(line, block, "central meridian");
      if (strncmp(test, "latitude of origin", 18)==0)
        cfg->albers->orig_latitude = read_double(line, block, "latitude of origin");
      if (strncmp(test, "units", 5)==0)
        cfg->polar->units = read_str(line, block, "units");
    }

    if (strncmp(line, "[Lambert Azimuthal Equal Area]", 30)==0)
      strcpy(block, "Lambert Azimuthal Equal Area");
    if (strncmp(block, "Lambert Azimuthal Equal Area", 28)==0) {
      test = read_param(line);
      if (strncmp(test, "datum", 5)==0)
        cfg->lambert_az->datum = read_int(line, block, "datum");
      if (strncmp(test, "center latitude", 15)==0)
        cfg->lambert_az->center_lat = read_double(line, block, "center latitude");
      if (strncmp(test, "center longitude", 16)==0)
        cfg->lambert_az->center_lon = read_double(line, block, "center longitude");
      if (strncmp(test, "units", 5)==0)
        cfg->lambert_az->units = read_str(line, block, "units");
    }

    if (strncmp(line, "[Lambert Conformal Conic]", 25)==0)
      strcpy(block, "Lambert Conformal Conic");
    if (strncmp(block, "Lambert Conformal Conic", 23)==0) {
      test = read_param(line);
      if (strncmp(test, "datum", 5)==0)
        cfg->lambert_cc->datum = read_int(line, block, "datum");
      if (strncmp(test, "first standard parallel", 23)==0)
        cfg->lambert_cc->first_parallel =
          read_double(line, block, "first standard parallel");
      if (strncmp(test, "second standard parallel", 24)==0)
        cfg->lambert_cc->second_parallel =
          read_double(line, block, "second standard parallel");
      if (strncmp(test, "central meridian", 16)==0)
        cfg->lambert_cc->center_meridian =
          read_double(line, block, "central meridian");
      if (strncmp(test, "latitude of origin", 18)==0)
        cfg->lambert_cc->orig_latitude =
          read_double(line, block, "latitude of origin");
      if (strncmp(test, "units", 5)==0)
        cfg->lambert_cc->units = read_str(line, block, "units");
    }

    if (strncmp(line, "[Resampling]", 12)==0) strcpy(block, "Resampling");
    if (strncmp(block, "Resampling", 10)==0) {
      test = read_param(line);
      if (strncmp(test, "subsampling factor", 18)==0)
        cfg->resampling->kernel = read_int(line, block, "subsampling factor");
    }
  }

  FCLOSE(fConfig);
  if (warnFlag)
    check_return(1, "Invalid values in configuration file.\n");

  return cfg;
}

int write_config(char *configFile, s_config *cfg)
{
  FILE *fConfig;

  if (cfg == NULL)
    check_return(1, "No configuration structure available for writing.\n");
  fConfig = FOPEN(configFile, "w");

  fprintf(fConfig, "%s\n", cfg->comment);
  fprintf(fConfig, "[General]\n");
  fprintf(fConfig, "input data file = %s\n", cfg->general->in_data_name);
  fprintf(fConfig, "input metadata file = %s\n", cfg->general->in_meta_name);
  fprintf(fConfig, "input format = %s\n", cfg->general->in_format);
  fprintf(fConfig, "data type = %s\n", cfg->general->data_type);
  fprintf(fConfig, "output file = %s\n", cfg->general->out_name);
  fprintf(fConfig, "output format = %s\n", cfg->general->out_format);
  fprintf(fConfig, "resampling = %d\n", cfg->general->resample);
  fprintf(fConfig, "browse image = %d\n", cfg->general->browse);
  fprintf(fConfig, "geocoding = %d\n", cfg->general->geocoding);
  fprintf(fConfig, "batch mode = %d\n", cfg->general->batch);
  fprintf(fConfig, "batch file = %s\n", cfg->general->batchFile);
  fprintf(fConfig, "log file = %s\n\n", cfg->general->logFile);
  if (cfg->general->geocoding) {
    fprintf(fConfig, "[Geocoding]\n");
    fprintf(fConfig, "projection = %s\n", cfg->geocoding->projection);
    fprintf(fConfig, "background fill = %i\n", cfg->geocoding->background);
    fprintf(fConfig, "pixel spacing = %.2f\n", cfg->geocoding->pixel);
    fprintf(fConfig, "height = %.3f\n\n", cfg->geocoding->height);
    if (strncmp(str2upper(cfg->geocoding->projection), "POLAR", 5)==0) {
      fprintf(fConfig, "[Polar Stereographic]\n");
      fprintf(fConfig, "datum = %i\n", cfg->polar->datum);
      fprintf(fConfig, "center latitude = %.6f\n", cfg->polar->center_lat);
      fprintf(fConfig, "center longitude = %.6f\n", cfg->polar->center_lon);
      fprintf(fConfig, "units = %s\n\n", cfg->polar->units);
    }
    else if (strncmp(str2upper(cfg->geocoding->projection), "UTM", 3)==0) {
      fprintf(fConfig, "[UTM]\n");
      fprintf(fConfig, "datum = %i\n", cfg->utm->datum);
      fprintf(fConfig, "zone number = %i\n", cfg->utm->zone);
      fprintf(fConfig, "units = %s\n\n", cfg->utm->units);
    }
    else if (strncmp(str2upper(cfg->geocoding->projection), "ALBERS", 6)==0) {
      fprintf(fConfig, "[Albers Conic Equal Area]\n");
      fprintf(fConfig, "datum = %i\n", cfg->albers->datum);
      fprintf(fConfig, "first standard parallel = %.6f\n",
              cfg->albers->first_parallel);
      fprintf(fConfig, "second standard parallel = %.6f\n",
              cfg->albers->second_parallel);
      fprintf(fConfig, "central meridian = %.6f\n",
              cfg->albers->center_meridian);
      fprintf(fConfig, "latitude of origin = %.6f\n",
              cfg->albers->orig_latitude);
      fprintf(fConfig, "units = %s\n\n", cfg->albers->units);
    }
    else if (strncmp(str2upper(cfg->geocoding->projection), "LAMBERT_AZ", 10)==0) {
      fprintf(fConfig, "[Lambert Azimuthal Equal Area]\n");
      fprintf(fConfig, "datum = %i\n", cfg->lambert_az->datum);
      fprintf(fConfig, "center latitude = %.6f\n", cfg->lambert_az->center_lat);
      fprintf(fConfig, "center longitude = %.6f\n", cfg->lambert_az->center_lon);
      fprintf(fConfig, "units = %s\n\n", cfg->lambert_az->units);
    }
    else if (strncmp(str2upper(cfg->geocoding->projection), "LAMBERT_CC", 10)==0) {
      fprintf(fConfig, "[Lambert Conformal Conic]\n");
      fprintf(fConfig, "datum = %i\n", cfg->lambert_cc->datum);
      fprintf(fConfig, "first standard parallel = %.6f\n",
              cfg->lambert_cc->first_parallel);
      fprintf(fConfig, "second standard parallel = %.6f\n",
              cfg->lambert_cc->second_parallel);
      fprintf(fConfig, "central meridian = %.6f\n",
              cfg->lambert_cc->center_meridian);
      fprintf(fConfig, "latitude of origin = %.6f\n",
              cfg->lambert_cc->orig_latitude);
      fprintf(fConfig, "units = %s\n\n", cfg->lambert_cc->units);
    }
  }
  if (cfg->general->resample) {
    fprintf(fConfig, "[Resampling]\n");
    fprintf(fConfig, "subsampling factor = %i\n", cfg->resampling->kernel);
  }

  FCLOSE(fConfig);

  return(0);
}
