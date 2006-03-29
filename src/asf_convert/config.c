#include "asf.h"
#include "asf_convert.h"
#include "asf_reporting.h"
#include <ctype.h>

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

  if ( fileExists(configFile) ) {
    asfPrintError("Cannot create file, %s, because it already exists.\n",
                  configFile);
  }
  fConfig = FOPEN(configFile, "w");

  fprintf(fConfig, "asf_convert configuration file\n\n");

  fprintf(fConfig, "[General]\n\n");
  fprintf(fConfig, "# This parameter looks for the basename of the input file\n\n");
  fprintf(fConfig, "input file = \n\n");
  fprintf(fConfig, "# This parameter looks for the basename of the output file\n\n");
  fprintf(fConfig, "output file = \n\n");
  fprintf(fConfig, "# The import flag indicates whether the data needs to be run through\n"
	  "# 'asf_import' (1 for running it, 0 for leaving out the import step).\n"
	  "# For example, setting the import switch to zero assumes that all the data \n"
	  "# is already in the ASF internal format.\n");
  fprintf(fConfig, "# Running asf_convert with the -create option and the import flag\n"
	  "# switched on will generate an [Import] section where you can define further\n"
	  "# parameters.\n\n");
  fprintf(fConfig, "import = 1\n\n");
  fprintf(fConfig, "# The geocoding flag indicates whether the data needs to be run through\n"
	  "# 'asf_geocode' (1 for running it, 0 for leaving out the geocoding step).\n");
  fprintf(fConfig, "# Running asf_convert with the -create option and the geocoding flag\n"
	  "# switched on will generate an [Geocoding] section where you can define further\n"
	  "# parameters.\n\n");
  fprintf(fConfig, "geocoding = 1\n\n");
  fprintf(fConfig, "# The export flag indicates whether the data needs to be run through\n"
	  "# 'asf_export' (1 for running it, 0 for leaving out the export step).\n");
  fprintf(fConfig, "# Running asf_convert with the -create option and the export flag\n"
	  "# switched on will generate an [Export] section where you can define further\n"
	  "# parameters.\n\n");
  fprintf(fConfig, "export = 1\n\n");
  fprintf(fConfig, "# The default values file is used to define the user's preferred parameter\n"
	  "# settings. In most cases, you will work on a study where your area of interest is\n"
	  "# geographically well defined. You want the data for the entire project in the same\n"
	  "# projection, with the same pixel spacing and the same output format.\n");
  fprintf(fConfig, "# A sample of a default values file can be located in %s.\n\n", 
	  get_asf_share_dir());
  fprintf(fConfig, "default values = \n\n");
  fprintf(fConfig, "# The intermediates flag indicates whether the intermediate processing\n"
	  "# results are kept (1 for keeping them, 0 for deleting them at the end of the\n"
	  "# processing).\n\n");    
  fprintf(fConfig, "intermediates = 0\n\n");
  fprintf(fConfig, "# The quiet flag determines how much information is reported by the\n"
	  "# individual tools (1 for keeping reporting to a minimum, 0 for maximum reporting\n\n");
  fprintf(fConfig, "quiet = 1\n\n");
  fprintf(fConfig, "# This parameter looks for the location of the batch file\n");
  fprintf(fConfig, "# asf_convert can be used in a batch mode to run a large number of data\n"
	  "# sets through the processing flow with the same processing parameters.\n\n");
  fprintf(fConfig, "batch file = \n\n");
  fprintf(fConfig, "# A prefix can be added to the outfile name to avoid overwriting\n"
	  "# files (e.g. when running the same data sets through the processing flow\n"
	  "# with different map projection parameters\n\n");
  fprintf(fConfig, "prefix = \n\n");
  fprintf(fConfig, "# A suffix can be added to the outfile name to avoid overwriting\n"
	  "# files (e.g when running the same data sets through the processing flow\n"
	  "# with different map projection parameters\n\n");
  fprintf(fConfig, "suffix = \n\n");
  
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
  cfg->image_stats = newStruct(s_image_stats);
  cfg->geocoding = newStruct(s_geocoding);
  cfg->export = newStruct(s_export);

  // Initialize structure
  strcpy(cfg->comment, "asf_convert configuration file");

  cfg->general->in_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->in_name, "");
  cfg->general->out_name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->out_name, "");
  cfg->general->import = 0;
  cfg->general->image_stats = 0;
  cfg->general->geocoding = 0;
  cfg->general->export = 0;
  cfg->general->batchFile = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->batchFile, "");
  cfg->general->defaults = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->defaults, "");
  cfg->general->prefix = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->prefix, "");
  cfg->general->suffix = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->suffix, "");
  cfg->general->intermediates = 0;
  cfg->general->quiet = 1;

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

  cfg->image_stats->values = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->image_stats->values, "LOOK");
  cfg->image_stats->bins = -99;
  cfg->image_stats->interval = -99.9;

  cfg->geocoding->projection = (char *)MALLOC(sizeof(char)*255);
  sprintf(cfg->geocoding->projection, "%s/projections/utm/utm.proj", get_asf_share_dir());
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
  cfg->export->byte = "SIGMA";

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
      check_return(1, "Default values file does not exist\n");
    fDefaults = FOPEN(cfg->general->defaults, "r");
    while (fgets(line, 255, fDefaults) != NULL) {
      test = read_param(line);
      // General
      if (strncmp(test, "import", 6)==0)
        cfg->general->import = read_int(line, "import");
      if (strncmp(test, "image stats", 11)==0)
	cfg->general->image_stats = read_int(line, "image stats");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, "geocoding");
      if (strncmp(test, "export", 6)==0)
        cfg->general->export = read_int(line, "export");
      if (strncmp(test, "intermediates", 13)==0)
	cfg->general->intermediates = read_int(line, "intermediates");
      if (strncmp(test, "quiet", 5)==0)
	cfg->general->quiet = read_int(line, "quiet");
      if (strncmp(test, "prefix", 6)==0)
	cfg->general->prefix = read_str(line, "prefix");
      if (strncmp(test, "suffix", 6)==0)
	cfg->general->suffix = read_str(line, "suffix");
      // Import
      if (strncmp(test, "input format", 12)==0)
        cfg->import->format = read_str(line, "input format");
      if (strncmp(test, "radiometry", 10)==0)
        cfg->import->radiometry = read_str(line, "radiometry");
      if (strncmp(test, "look up table", 13)==0)
        cfg->import->lut = read_str(line, "look up table");
      // Image stats
      if (strncmp(test, "stats values", 12)==0)
	cfg->image_stats->values = read_str(line, "stats values");
      // Geocoding
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
      // Export
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
      if (strncmp(test, "image stats", 11)==0)
	cfg->general->image_stats = read_int(line, "image stats");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, "geocoding");
      if (strncmp(test, "export", 6)==0)
        cfg->general->export = read_int(line, "export");
      if (strncmp(test, "default values", 14)==0)
        cfg->general->defaults = read_str(line, "default values");
      if (strncmp(test, "intermediates", 13)==0)
        cfg->general->intermediates = read_int(line, "intermediates");
      if (strncmp(test, "quiet", 13)==0)
        cfg->general->quiet = read_int(line, "quiet");
      if (strncmp(test, "batch file", 10)==0)
        cfg->general->batchFile = read_str(line, "batch file");
      if (strncmp(test, "prefix", 6)==0)
        cfg->general->prefix = read_str(line, "prefix");
      if (strncmp(test, "suffix", 6)==0)
        cfg->general->suffix = read_str(line, "suffix");
    }
  }
  FCLOSE(fConfig);

  return cfg;
}

convert_config *read_config(char *configFile)
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
      if (strncmp(test, "image stats", 11)==0)
	cfg->general->image_stats = read_int(line, "image stats");
      if (strncmp(test, "geocoding", 9)==0)
        cfg->general->geocoding = read_int(line, "geocoding");
      if (strncmp(test, "export", 6)==0)
        cfg->general->export = read_int(line, "export");
      if (strncmp(test, "default values", 14)==0)
        cfg->general->defaults = read_str(line, "default values");
      if (strncmp(test, "intermediates", 13)==0)
        cfg->general->intermediates = read_int(line, "intermediates");
      if (strncmp(test, "quiet", 5)==0)
        cfg->general->quiet = read_int(line, "quiet");
      if (strncmp(test, "batch file", 10)==0)
        cfg->general->batchFile = read_str(line, "batch file");
      if (strncmp(test, "prefix", 6)==0)
        cfg->general->prefix = read_str(line, "prefix");
      if (strncmp(test, "suffix", 6)==0)
        cfg->general->suffix = read_str(line, "suffix");
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

    if (strncmp(line, "[Image stats]", 13)==0) strcpy(params, "Image stats");
    if (strncmp(params, "Image stats", 11)==0) {
      test = read_param(line);
      if (strncmp(test, "values", 6)==0)
        cfg->image_stats->values = read_str(line, "values");
      if (strncmp(test, "bins", 4)==0)
	cfg->image_stats->bins = read_int(line, "bins");
      if (strncmp(test, "interval", 8)==0)
	cfg->image_stats->interval = read_double(line, "interval");
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
    fprintf(fConfig, "\n# This parameter looks for the basename of the input file\n\n");
    fprintf(fConfig, "input file = %s\n", cfg->general->in_name);
    fprintf(fConfig, "\n# This parameter looks for the basename of the output file\n\n");
    fprintf(fConfig, "output file = %s\n", cfg->general->out_name);
    fprintf(fConfig, "\n# The import flag indicates whether the data needs to be run through\n"
	    "# 'asf_import' (1 for running it, 0 for leaving out the import step).\n"
	    "# For example, setting the import switch to zero assumes that all the data \n"
	    "# is already in the ASF internal format.\n");
    fprintf(fConfig, "# Running asf_convert with the -create option and the import flag\n"
	    "# switched on will generate an [Import] section where you can define further\n"
	    "# parameters.\n\n");
    fprintf(fConfig, "import = %i\n", cfg->general->import);
    if (cfg->general->image_stats) {
      fprintf(fConfig, "\n# The image stats flag indicates whether the data needs to be run\n"
	      "# through 'image_stats' (1 for running it, 0 for level out the image stats\n"
	      "# step).\n\n");
      fprintf(fConfig, "image stats = %i\n", cfg->general->image_stats);
    }
    fprintf(fConfig, "\n# The geocoding flag indicates whether the data needs to be run through\n"
	    "# 'asf_geocode' (1 for running it, 0 for leaving out the geocoding step).\n");
    fprintf(fConfig, "# Running asf_convert with the -create option and the geocoding flag\n"
	    "# switched on will generate an [Geocoding] section where you can define further\n"
	    "# parameters.\n\n");
    fprintf(fConfig, "geocoding = %i\n", cfg->general->geocoding);
    fprintf(fConfig, "\n# The export flag indicates whether the data needs to be run through\n"
	    "# 'asf_export' (1 for running it, 0 for leaving out the export step).\n");
    fprintf(fConfig, "# Running asf_convert with the -create option and the export flag\n"
	    "# switched on will generate an [Export] section where you can define further\n"
	    "# parameters.\n\n");
    fprintf(fConfig, "export = %i\n", cfg->general->export);
    fprintf(fConfig, "\n# The default values file is used to define the user's preferred parameter\n"
	    "# settings. In most cases, you will work on a study where your area of interest is\n"
	    "# geographically well defined. You want the data for the entire project in the same\n"
	    "# projection, with the same pixel spacing and the same output format.\n\n");
    fprintf(fConfig, "# A sample of a default values file can be located in %s.\n\n", 
	    get_asf_share_dir());
    fprintf(fConfig, "default values = %s\n", cfg->general->defaults);
    fprintf(fConfig, "\n# The intermediates flag indicates whether the intermediate processing\n"
	    "# results are kept (1 for keeping them, 0 for deleting them at the end of the\n"
	    "# processing).\n\n");    
    fprintf(fConfig, "intermediates = %i\n\n", cfg->general->intermediates);

    if (cfg->general->import) {
      fprintf(fConfig, "\n[Import]\n");
      fprintf(fConfig, "\n# The recognized import formats are: ASF, CEOS and STF.\n"
	      "# Defining ASF, being the internal format, as the import format is\n"
	      "# just another way of actually skipping the import step.\n\n");
      fprintf(fConfig, "format = %s\n", cfg->import->format);
      fprintf(fConfig, "\n# The radiometry can be one of the following: AMPLITUDE_IMAGE,\n"
	      "# POWER_IMAGE, SIGMA_IMAGE, GAMMA_IMAGE and BETA_IMAGE.\n"
	      "# The amplitude image is the regularly processed SAR image. The power image\n"
	      "# represents the magnitude (square of the amplitude) of the SAR image.\n"
	      "# The sigma, gamma and beta image are different representations of calibrated\n"
	      "# SAR images. Their values are in power scale.\n\n");
      fprintf(fConfig, "radiometry = %s\n", cfg->import->radiometry);
      fprintf(fConfig, "\n# The look up table option is primarily used by the Canadian Ice\n"
	      "# Service (CIS) and scales the amplitude values in range direction. The file\n"
	      "# parsed in to the import tool is expected to have two columns, the first one\n"
	      "# indicating the look angle with the corresponding scale factor as the second\n"
	      "# column.\n\n");
      fprintf(fConfig, "look up table = %s\n", cfg->import->lut);
      fprintf(fConfig, "\n# The latitude constraints (lat begin and lat end) can only be used\n"
	      "# when importing level zero swath data (STF). This is the most convenient way\n"
	      "# to cut a subset out of a long image swath.\n\n");
      fprintf(fConfig, "lat begin = %.2f\n", cfg->import->lat_begin);
      fprintf(fConfig, "lat end = %.2f\n", cfg->import->lat_end);
      fprintf(fConfig, "\n# The precise option, currently under development, will allow the use\n"
	      "# of ERS precision state vector from DLR as a replacement of the restituted\n"
	      "# state vectors that are provided from the European Space Agency. The parameter\n"
	      "# required here defines the location of the precision state vectors.\n\n");
      fprintf(fConfig, "precise = %s\n\n", cfg->import->prc);
    }

    if (cfg->general->image_stats) {
      fprintf(fConfig, "\n[Image stats]\n\n");
      fprintf(fConfig, "values = %s\n", cfg->image_stats->values);
      fprintf(fConfig, "bins = %i\n", cfg->image_stats->bins);
      fprintf(fConfig, "interval = %.2lf\n", cfg->image_stats->interval);
    }

    if (cfg->general->geocoding) {
      fprintf(fConfig, "\n[Geocoding]\n");
      fprintf(fConfig, "\n# The geocoding tool currently supports five different map projections:\n"
	      "# Universal Transverse Mercator (UTM), Polar Stereographic, Albers Equal Area\n"
	      "# Conic, Lambert Conformal Conic and Lambert Azimuthal Equal Area.\n"
	      "# For all these map projections a large number of projection parameter files\n"
	      "# have been predefined for various parts of the world.\n");
      fprintf(fConfig, "# The projection parameter files are located in %s/projections.\n\n", 
	      get_asf_share_dir());
      fprintf(fConfig, "projection = %s\n", cfg->geocoding->projection);
      fprintf(fConfig, "\n# The pixel spacing determines the pixel size used for the resulting\n"
	      "# geocoded image and, therefore, the size of the output image.\n\n");
      fprintf(fConfig, "pixel spacing = %.2f\n", cfg->geocoding->pixel);
      fprintf(fConfig, "\n# An average height can be defined for the image that is taken into\n"
	      "#  account and adjusted for during the geocoding process.\n\n");
      fprintf(fConfig, "height = %.1f\n", cfg->geocoding->height);
      fprintf(fConfig, "\n# A vertical datum can be defined for geocoded image. WGS84 is the\n"
	      "# only currently supported datum. However, NAD27 and NAD83 are planned to be\n"
	      "# appropriate alternatives.\n\n");
      fprintf(fConfig, "datum = %s\n", cfg->geocoding->datum);
      fprintf(fConfig, "\n# Three different resampling methods have been implemented as part\n"
	      "# of the geocoding: NEAREST NEIGHBOR, BILINEAR and BICUBIC. The bilinear\n"
	      "#  resampling method is the default.\n\n");
      fprintf(fConfig, "resampling = %s\n", cfg->geocoding->resampling);
      fprintf(fConfig, "\n# In order to ensure the proper use of projection parameter files,\n"
	      "# we have implemented a number of checks that verify whether the map\n"
	      "# projection parameters are reasonable for the area that is covered by the\n"
	      "# data. For example, applying a projection parameter file that is defined for\n"
	      "# South America for a data set that is covering Alaska would lead to huge\n"
	      "# distortions. These checks can be overwritten by setting the force option.\n\n");
      fprintf(fConfig, "force = %i\n\n", cfg->geocoding->force);
    }

    if (cfg->general->export) {
      fprintf(fConfig, "\n[Export]\n");
      fprintf(fConfig, "\n# The following format are considered valid format: ASF, TIFF, GEOTIFF\n"
	      "# JPEG and PPM.\n" 
	      "# In the same way as for the import block, ASF as an export option results in\n"
	      "# skipping the export step entirely. All other format, with the exception of\n"
	      "# GeoTIFF, require the scaling of the internal ASF format from floating point\n"
	      "# to byte. The GeoTIFF supports byte as well as floating point data.\n\n");
      fprintf(fConfig, "format = %s\n", cfg->export->format);
      fprintf(fConfig, "\n# The byte conversion options are SIGMA, MINMAX, TRUNCATE or\n"
	      "# HISTOGRAM_EQUALIZE. They scale the floating point values to byte values.\n\n");
      fprintf(fConfig, "byte conversion = %s\n\n", cfg->export->byte);
    }
  }
  else {
    fprintf(fConfig, "%s\n", cfg->comment);

    fprintf(fConfig, "[General]\n\n");
    fprintf(fConfig, "# The default values file is used to define the user's preferred\n"
	    "# parameter settings.\n\n");
    fprintf(fConfig, "default values = %s\n\n", cfg->general->defaults);
    fprintf(fConfig, "# asf_convert has a batch mode to run a large number of data sets\n"
	    "# through the processing flow with the same processing parameters\n\n");
    fprintf(fConfig, "batch file = %s\n\n", cfg->general->batchFile);
    fprintf(fConfig, "# A prefix can be added to the outfile name to avoid overwriting\n"
	    "# files (e.g. when running the same data sets through the processing flow\n"
	    "# with different map projection parameters\n\n");
    fprintf(fConfig, "prefix = %s\n\n", cfg->general->prefix);
    fprintf(fConfig, "# A suffix can be added to the outfile name to avoid overwriting\n"
	    "# files (e.g when running the same data sets through the processing flow\n"
	    "# with different map projection parameters\n\n");
    fprintf(fConfig, "suffix = %s\n\n", cfg->general->suffix);
  }

  FCLOSE(fConfig);

  return(0);
}
