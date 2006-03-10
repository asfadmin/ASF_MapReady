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

char *read_str(char *line, char *param)
{
  char tmp[255], *start;
  char *value=(char *)MALLOC(sizeof(char)*255);

  start = strchr(line, '=');
  sscanf(start, "%s %s", tmp, value);
  return value;
}

int read_int(char *line, char *param)
{
  char *tmp=NULL;
  int value;

  tmp = read_str(line, param);
  sscanf(tmp, "%i", &value);

  return value;
}

double read_double(char *line, char *param)
{
  char *tmp=NULL;
  double value;

  tmp = read_str(line, param);
  sscanf(tmp, "%lf", &value);

  return value;
}

int init_config(char *configFile)
{
  FILE *fConfig;

  fConfig = FOPEN(configFile, "w");

  fprintf(fConfig, "asf_convert configuration file\n\n");
  fprintf(fConfig, "[General]\n");
  fprintf(fConfig, "input file = < basename of input file >\n");
  fprintf(fConfig, "output file = < basename of output file >\n");
  fprintf(fConfig, "import = < 0 | 1 >\n");
  fprintf(fConfig, "geocoding = < 0 | 1 >\n");
  fprintf(fConfig, "export = < 0 | 1 >\n");
  fprintf(fConfig, "batch file = < name of batch file >\n");
  fprintf(fConfig, "status = new\n\n");

  FCLOSE(fConfig);

  asfPrintStatus("   Initialized basic configuration file\n\n");

  return(0);
}

convert_config *init_fill_config(char *configFile)
{
#define newStruct(type) (type *)MALLOC(sizeof(type))

  FILE *fConfig, *fDefaults;
  char line[255], params[25];
  char *test=(char *)MALLOC(sizeof(char)*255);
  int i;
  
  // Create structure
  convert_config *cfg = newStruct(convert_config);
  cfg->general = newStruct(s_general);
  cfg->import = newStruct(s_import);
  cfg->geocoding = newStruct(s_geocoding);
  cfg->export = newStruct(s_export);

  // Initialize structure
  strcpy(cfg->comment, "asf_convert configuration file");

  cfg->general->in_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->in_name, "");
  cfg->general->out_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->out_name, "");
  cfg->general->import = 0;
  cfg->general->geocoding = 0;
  cfg->general->export = 0;
  cfg->general->batchFile = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->batchFile, "");
  cfg->general->defaults = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->defaults, "");
  cfg->general->intermediates = 0;

  cfg->import->format = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->import->format, "CEOS");
  cfg->import->radiometry = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->import->radiometry, "AMPLITUDE_IMAGE");
  cfg->import->lut = (char *)MALLOC(sizeof(char)*25);
  cfg->import->lut = "";
  cfg->import->lat_begin = -99.0;
  cfg->import->lat_end = -99.0;
  cfg->import->prc = (char *)MALLOC(sizeof(char)*25);
  cfg->import->prc = "";

  cfg->geocoding->projection = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->geocoding->projection, "");
  cfg->geocoding->pixel = 100;
  cfg->geocoding->height = 0.0;
  cfg->geocoding->datum = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->geocoding->datum, "WGS84");
  cfg->geocoding->resampling = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->geocoding->resampling, "BILINEAR");
  cfg->geocoding->force = 0;

  cfg->export->format = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->export->format, "GEOTIFF");
  cfg->export->byte = (char *)MALLOC(sizeof(char)*25);
  cfg->export->byte = "";

  // Check for a default values file
  fConfig = FOPEN(configFile, "r");
  i=0;
  while (fgets(line, 255, fConfig) != NULL) {
    if (i==0) strcpy(cfg->comment, line);
    i++;

    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "default values", 14)==0) 
	cfg->general->defaults = read_str(line, "default values");
    }
  }
  FCLOSE(fConfig);

  // Read default values file if there is one
  if (strcmp(cfg->general->defaults, "") != 0) {
    if (!fileExists(cfg->general->defaults)) 
      check_return(1, "default values file does not exist");
    fDefaults = FOPEN(cfg->general->defaults, "r");
    while (fgets(line, 255, fDefaults) != NULL) {
      test = read_param(line);
      if (strncmp(test, "import", 6)==0)
        cfg->general->import = read_int(line, "import");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, "geocoding");
      if (strncmp(test, "export", 6)==0)
        cfg->general->export = read_int(line, "export");
      if (strncmp(test, "intermediates", 13)==0)
	cfg->general->intermediates = read_int(line, "intermediates");
      if (strncmp(test, "input format", 12)==0)
        cfg->import->format = read_str(line, "input format");
      if (strncmp(test, "radiometry", 10)==0)
        cfg->import->radiometry = read_str(line, "radiometry");
      if (strncmp(test, "look up table", 13)==0)
        cfg->import->lut = read_str(line, "look up table");
      if (strncmp(test, "projection", 10)==0)
        cfg->geocoding->projection = read_str(line, "projection");
      if (strncmp(test, "pixel spacing", 13)==0)
        cfg->geocoding->pixel = read_double(line, "pixel spacing");
      if (strncmp(test, "height", 6)==0)
        cfg->geocoding->height = read_double(line, "height");
      if (strncmp(test, "datum", 5)==0)
        cfg->geocoding->datum = read_str(line, "datum");
      if (strncmp(test, "resampling", 10)==0)
        cfg->geocoding->resampling = read_str(line, "resampling");
      if (strncmp(test, "force", 5)==0)
        cfg->geocoding->force = read_int(line, "force");
      if (strncmp(test, "output format", 13)==0)
        cfg->export->format = read_str(line, "output format");
      if (strncmp(test, "byte conversion", 15)==0)
        cfg->export->byte = read_str(line, "byte conversion");
    }
  }

  // Read in parameters
  fConfig = FOPEN(configFile, "r");
  i=0;
  while (fgets(line, 255, fConfig) != NULL) {
    if (i==0) strcpy(cfg->comment, line);
    i++;

    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "input file", 10)==0)
        cfg->general->in_name = read_str(line, "input file");
      if (strncmp(test, "output file", 11)==0)
        cfg->general->out_name = read_str(line, "output file");
      if (strncmp(test, "import", 6)==0)
        cfg->general->import = read_int(line, "import");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, "geocoding");
      if (strncmp(test, "export", 6)==0)
        cfg->general->export = read_int(line, "export");
      if (strncmp(test, "default values", 14)==0)
        cfg->general->defaults = read_str(line, "default values");
      if (strncmp(test, "intermediates", 13)==0)
        cfg->general->export = read_int(line, "intermediates");
      if (strncmp(test, "batch file", 10)==0)
        cfg->general->batchFile = read_str(line, "batch file");
    }
  }
  FCLOSE(fConfig);

  return cfg;
}

convert_config *read_config(char *configFile, int cFlag)
{
  FILE *fConfig;
  convert_config *cfg=NULL;
  char line[255], params[50];
  char *test=(char *)MALLOC(sizeof(char)*255);

  cfg = init_fill_config(configFile);
  if (cfg == NULL) check_return(1, "Creating configuration structure.\n");
  fConfig = FOPEN(configFile, "r");
  while (fgets(line, 255, fConfig) != NULL) {

    if (strncmp(line, "[General]", 9)==0) strcpy(params, "General");
    if (strncmp(params, "General", 7)==0) {
      test = read_param(line);
      if (strncmp(test, "input file", 10)==0)
        cfg->general->in_name = read_str(line, "input file");
      if (strncmp(test, "output file", 11)==0)
        cfg->general->out_name = read_str(line, "output file");
      if (strncmp(test, "import", 6)==0)
        cfg->general->import = read_int(line, "import");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, "geocoding");
      if (strncmp(test, "export", 6)==0)
        cfg->general->export = read_int(line, "export");
      if (strncmp(test, "default values", 14)==0)
        cfg->general->defaults = read_str(line, "default values");
      if (strncmp(test, "intermediates", 13)==0)
        cfg->general->intermediates = read_int(line, "intermediates");
      if (strncmp(test, "batch file", 10)==0)
        cfg->general->batchFile = read_str(line, "batch file");
    }

    if (strncmp(line, "[Import]", 8)==0) strcpy(params, "Import");
    if (strncmp(params, "Import", 6)==0) {
      test = read_param(line);
      if (strncmp(test, "format", 6)==0)
        cfg->import->format = read_str(line, "format");
      if (strncmp(test, "radiometry", 10)==0)
        cfg->import->radiometry = read_str(line, "radiometry");
      if (strncmp(test, "look up table", 13)==0)
        cfg->import->lut = read_str(line, "look up table");
      if (strncmp(test, "lat begin", 9)==0)
        cfg->import->lat_begin = read_double(line, "lat begin");
      if (strncmp(test, "lat end", 7)==0)
        cfg->import->lat_end = read_double(line, "lat end");
      if (strncmp(test, "precise", 7)==0)
        cfg->import->prc = read_str(line, "precise");
    }

    if (strncmp(line, "[Geocoding]", 11)==0) strcpy(params, "Geocoding");
    if (strncmp(params, "Geocoding", 9)==0) {
      test = read_param(line);
      if (strncmp(test, "projection", 10)==0)
        cfg->geocoding->projection = read_str(line, "projection");
      if (strncmp(test, "pixel spacing", 13)==0)
        cfg->geocoding->pixel = read_double(line, "pixel spacing");
      if (strncmp(test, "height", 6)==0)
        cfg->geocoding->height = read_double(line, "height");
      if (strncmp(test, "datum", 5)==0)
        cfg->geocoding->datum = read_str(line, "datum");
      if (strncmp(test, "resampling", 10)==0)
        cfg->geocoding->resampling = read_str(line, "resampling");
      if (strncmp(test, "force", 5)==0)
        cfg->geocoding->force = read_int(line, "force");
    }

    if (strncmp(line, "[Export]", 8)==0) strcpy(params, "Export");
    if (strncmp(params, "Export", 6)==0) {
      test = read_param(line);
      if (strncmp(test, "format", 6)==0)
        cfg->export->format = read_str(line, "format");
      if (strncmp(test, "byte conversion", 15)==0)
        cfg->export->byte = read_str(line, "byte conversion");
    }
  }

  FCLOSE(fConfig);

  return cfg;
}

int write_config(char *configFile, convert_config *cfg)
{
  FILE *fConfig;

  if (cfg == NULL)
    check_return(1, "No configuration structure to write.\n");
  fConfig = FOPEN(configFile, "w");

  if (strcmp(cfg->general->batchFile, "") == 0) {
    fprintf(fConfig, "%s\n", cfg->comment);
    
    fprintf(fConfig, "[General]\n");
    fprintf(fConfig, "input file = %s\n", cfg->general->in_name);
    fprintf(fConfig, "output file = %s\n", cfg->general->out_name);
    fprintf(fConfig, "import = %i\n", cfg->general->import);
    fprintf(fConfig, "geocoding = %i\n", cfg->general->geocoding);
    fprintf(fConfig, "export = %i\n", cfg->general->export);
    fprintf(fConfig, "default values = %s\n", cfg->general->defaults);
    fprintf(fConfig, "intermediates = %i\n\n", cfg->general->intermediates);
    
    if (cfg->general->import) {
      fprintf(fConfig, "[Import]\n");
      fprintf(fConfig, "format = %s\n", cfg->import->format);
      fprintf(fConfig, "radiometry = %s\n", cfg->import->radiometry);
      fprintf(fConfig, "look up table = %s\n", cfg->import->lut);
      fprintf(fConfig, "lat begin = %.2f\n", cfg->import->lat_begin);
      fprintf(fConfig, "lat end = %.2f\n", cfg->import->lat_end);
      fprintf(fConfig, "precise = %s\n\n", cfg->import->prc);
    }
    
    if (cfg->general->geocoding) {
      fprintf(fConfig, "[Geocoding]\n");
      fprintf(fConfig, "projection = %s\n", cfg->geocoding->projection);
      fprintf(fConfig, "pixel spacing = %.2f\n", cfg->geocoding->pixel);
      fprintf(fConfig, "height = %.1f\n", cfg->geocoding->height);
      fprintf(fConfig, "datum = %s\n", cfg->geocoding->datum);
      fprintf(fConfig, "resampling = %s\n", cfg->geocoding->resampling);
      fprintf(fConfig, "force = %i\n\n", cfg->geocoding->force);
    }
    
    if (cfg->general->export) {
      fprintf(fConfig, "[Export]\n");
      fprintf(fConfig, "format = %s\n", cfg->export->format);
      fprintf(fConfig, "byte conversion = %s\n\n", cfg->export->byte);
    }
  }
  else {
    fprintf(fConfig, "%s\n", cfg->comment);
    
    fprintf(fConfig, "[General]\n");
    fprintf(fConfig, "default values = %s\n", cfg->general->defaults);
    fprintf(fConfig, "batch file = %s\n", cfg->general->batchFile);
  }

  FCLOSE(fConfig);

  return(0);
}
