#include "asf.h"
#include "ips.h"


static char * new_blank_str(void)
{
    char *ret = MALLOC(sizeof(char)*MAX_STRING_LEN);
    strcpy(ret, "");
    return ret;
}

static char * new_str(const char *val)
{
    char *ret = MALLOC(sizeof(char)*MAX_STRING_LEN);
    strcpy(ret, val);
    return ret;
}

static int strindex(char s[], char t[])
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

static char *read_param(char *line)
{
  int i, k;
  char *value=new_blank_str();
  
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k-1);
  return value;
}

// assume line is of length < 255
static void read_str(char *dest, char *line, char *param)
{
  char *start = strchr(line, '=');
  if (start)
    sscanf(start+1, "%s", dest);
  else
    strcpy(dest, "");
}

static int read_int(char *line, char *param)
{
  char tmp[255];
  int value;
  
  read_str(tmp, line, param);
  sscanf(tmp, "%i", &value);
  
  return value;
}

static double read_double(char *line, char *param)
{
  char tmp[255];
  double value;
  
  read_str(tmp, line, param);
  sscanf(tmp, "%lf", &value);
  
  return value;
}

int init_config(char *configFile)
{
  FILE *fConfig;
  
  fConfig = FOPEN(configFile, "w");
  
  fprintf(fConfig, "ips configuration file\n\n");
  
  // [General] section
  fprintf(fConfig, "[General]\n");
  // mode
  fprintf(fConfig, "\n# The interferometric processing system 'ips' can be run in two\n"
	  "# different modes. The main mode is DEM for the generation of digital elevation\n"
	  "# models. The DINSAR mode for differential interferometry is still under\n"
	  "# development.\n\n");
  fprintf(fConfig, "mode = DEM\n\n");
  // reference DEM
  fprintf(fConfig, "# This parameter looks for the location of the reference DEM file\n"
          "# The reference DEM is used in various parts of the SAR interferometric\n" 	        
	  "# processing flow, mostly prominently for the phase unwrapping.\n\n");
  fprintf(fConfig, "reference dem = \n\n");
  // output basename
  fprintf(fConfig, "# The ips saves a large number of intermediate and final results.\n"
          "# All the files relevant for further analysis will start with this basename\n\n");
  fprintf(fConfig, "base name = \n\n");
  // data type
  fprintf(fConfig, "# The ips handles three different data types. The most flexible type is\n"
          "# the level zero Sky Telemetry Format (STF). This swath data type allows for\n"
	  "# variable area sizes that are processed. The second data type is RAW for CEOS\n"
	  "# level zero data. The third supported data type is single look complex data"
	  "(SLC)\n\n");
  fprintf(fConfig, "data type = STF\n\n");
  // deskew flag
  fprintf(fConfig, "# The deskew flag indicates whether the raw data is SAR processed in\n"
          "# in zero Doppler geometry or not (1 for deskewing, 0 for regular processing)\n\n");
  fprintf(fConfig, "deskew = 0\n\n");
  // Doppler processing
  fprintf(fConfig, "# For the SAR processing, two different schemes for chosing the Doppler\n"
          "# values have been considered. Currently only the processing to the 'average'\n"
	  "# Doppler values of the image pair is used. The alternative approach that uses\n"
	  "# 'updated' Doppler values has not been implemented.\n\n");
  fprintf(fConfig, "doppler = average\n\n");
  // latitude constraints
  fprintf(fConfig, "# For effectively using swath data the user can define latitude\n"
	  "# constraints to select a subset of the swath data (-99 indicates that no\n"
          "# latitude constraint is chosen).\n\n");
  fprintf(fConfig, "lat begin = -99.0\n");
  fprintf(fConfig, "lat end = -99.0\n\n");
  // coregistration
  fprintf(fConfig, "# Matching up the first and last patches of an image pair leads to the\n"
          "# best results. For this approach use the 'PATCH' option. Once this method fails\n"
	  "# you can use the 'FRAME' option to match up master and slave image in its\n"
	  "# entirety.\n\n");
  fprintf(fConfig, "coregistration = PATCH\n\n");
  // maximum offset
  fprintf(fConfig, "# This parameter defines the maximum allowed pixel offset in range\n"
          "# or azimuth after the initial co-registration has been performed. Three pixels\n"
	  "# is an empirical value that worked in most cases.\n\n");
  fprintf(fConfig, "maximum offset = 3\n\n");
  /* Keep the mask related stuff out until it is actually implemented
  fprintf(fConfig, "correlation mask = < flag for using a mask for co-registration: "
	  "0 | 1 (not implemented yet) >\n");
  fprintf(fConfig, "mask file = < mask file location >\n");
  */
  // default values file
  fprintf(fConfig, "# The default values file is used to define the user's preferred\n"
	  "# parameter settings. In most cases, you will work on a study where your area\n"
          "# of interest is geographically well defined. You want the data for the entire\n"
	  "# project in the same projection, with the same pixel spacing and the same\n"
          "# output format.\n");
  fprintf(fConfig, "# A sample of a default values file can be located in %s/ips.\n\n",
          get_asf_share_dir());
  fprintf(fConfig, "default values = \n\n");
  // test mode
  fprintf(fConfig, "# The test mode is for internal use only (1 for test mode on, 0 for\n"
          "# test mode off).\n\n");
  fprintf(fConfig, "test mode = 0\n\n");
  // short configuration file flag
  fprintf(fConfig, "# The short configuration file flag allows the experienced user to\n"
	  "# generate configuration files without the verbose comments that explain all\n"
	  "# entries for the parameters in the configuration file (1 for a configuration\n"
	  "# without comments, 0 for a configuration file with verbose comments)\n\n");
  fprintf(fConfig, "short configuration file = 0\n\n");
  // general status
  fprintf(fConfig, "# The general status field indicates the progress of the processing.\n"
          "# The status 'new' indicates that the configuration has only been initialized\n"
	  "# but not run yet. For each new run the status needs to be set back to 'new'\n"
	  "# before running a data set again. Once the processing starts the status changes\n"
	  "# to 'processing'. When the processing is complete it is changed to 'success'\n\n");
  fprintf(fConfig, "status = new\n\n\n");

  // [Master] section
  fprintf(fConfig, "[Master image]\n");
  fprintf(fConfig, "\n# This parameter gives the path of the master image data.\n\n");
  fprintf(fConfig, "path = \n");
  fprintf(fConfig, "\n# This parameter gives the name of the master data file.\n"
          "# Swath data has usually an extension .000, whereas CEOS data has an extension\n"
          "# .D\n\n");
  fprintf(fConfig, "data file = \n");
  fprintf(fConfig, "\n# This parameter gives the name of the master metadata file.\n"
          "# Swath data has usually an extension .par, whereas CEOS data has an extension\n"
          "# .D\n\n");		  
  fprintf(fConfig, "metadata file = \n\n\n");
	  
  // [Slave] section
  fprintf(fConfig, "[Slave image]\n");
  fprintf(fConfig, "\n# This parameter gives the path of the slave image data.\n\n");
  fprintf(fConfig, "path = \n");
  fprintf(fConfig, "\n# This parameter gives the name of the slave data file.\n"
          "# Swath data has usually an extension .000, whereas CEOS data has an extension\n"
          "# .D\n");
  fprintf(fConfig, "data file = \n");
  fprintf(fConfig, "\n# This parameter gives the name of the slave metadata file.\n"
          "# Swath data has usually an extension .par, whereas CEOS data has an extension\n"
          "# .D\n\n");		  
  fprintf(fConfig, "metadata file = \n\n\n");
  
  FCLOSE(fConfig);
  
  printf("   Initialized basic configuration file\n\n");
  
  return(0);
}

#define newStruct(type) (type *)MALLOC(sizeof(type))

dem_config *create_config_with_defaults()
{
  /* Create structure */
  dem_config *cfg = newStruct(dem_config);
  cfg->general = newStruct(s_general);
  cfg->master = newStruct(s_image);
  cfg->slave = newStruct(s_image);
  cfg->ingest = newStruct(s_ingest);
  cfg->doppler = newStruct(s_status);
  cfg->coreg_p1 = newStruct(s_coreg);
  cfg->coreg_pL = newStruct(s_coreg);
  cfg->doppler_per_patch = newStruct(s_status);
  cfg->ardop_master = newStruct(s_ardop);
  cfg->ardop_slave = newStruct(s_ardop);
  cfg->cpx_autofilter = newStruct(s_status);
  cfg->coreg_slave = newStruct(s_coreg);
  cfg->igram_coh = newStruct(s_igram_coh);
  cfg->offset_match = newStruct(s_offset);
  cfg->sim_phase = newStruct(s_sim_phase);
  cfg->dinsar = newStruct(s_dinsar);
  cfg->deramp_ml = newStruct(s_status);
  cfg->unwrap = newStruct(s_unwrap);
  cfg->refine = newStruct(s_refine);
  cfg->elevation = newStruct(s_elev);
  cfg->ground_range = newStruct(s_status);
  cfg->geocode = newStruct(s_geocode);
  cfg->export = newStruct(s_export);
  
  /* initialize structure */
  strcpy(cfg->comment, "create_dem: configuration file");
  
  cfg->general->mode = new_blank_str();
  cfg->general->dem = new_blank_str();
  cfg->general->def_val = new_blank_str();
  cfg->general->base = new_blank_str();
  cfg->general->data_type = new_blank_str();
  cfg->general->deskew = 0;
  cfg->general->doppler = new_blank_str();
  cfg->general->lat_begin = -99;
  cfg->general->lat_end = 99;
  cfg->general->coreg = new_blank_str();
  cfg->general->max_off = 3;
  cfg->general->mflag = 0;
  cfg->general->mask = new_blank_str();
  cfg->general->test = 0;
  cfg->general->short_config = 0;
  cfg->general->status = new_str("new");
  
  cfg->master->path = new_blank_str();
  cfg->master->data = new_blank_str();
  cfg->master->meta = new_blank_str();
  
  cfg->slave->path = new_blank_str();
  cfg->slave->data = new_blank_str();
  cfg->slave->meta = new_blank_str();
  
  cfg->ingest->prc_master = new_blank_str();
  cfg->ingest->prc_slave = new_blank_str();
  cfg->ingest->prcflag = 0;
  cfg->ingest->status = new_str("new");
  
  cfg->doppler->status = new_str("new");
  
  cfg->coreg_p1->patches = 1;
  cfg->coreg_p1->start_master = 0;
  cfg->coreg_p1->start_slave = 0;
  cfg->coreg_p1->grid = 20;
  cfg->coreg_p1->fft = 1;
  cfg->coreg_p1->off_az = 0;
  cfg->coreg_p1->off_rng = 0;
  cfg->coreg_p1->status = new_str("new");
  
  cfg->coreg_pL->patches = 1;
  cfg->coreg_pL->start_master = 0;
  cfg->coreg_pL->start_slave = 0;
  cfg->coreg_pL->grid = 20;
  cfg->coreg_pL->fft = 1;
  cfg->coreg_pL->off_az = 0;
  cfg->coreg_pL->off_rng = 0;
  cfg->coreg_pL->status = new_str("new");
  
  cfg->doppler_per_patch->status = new_str("new");
  
  cfg->ardop_master->start_offset = 0;
  cfg->ardop_master->end_offset = 0;
  cfg->ardop_master->patches = 1;
  cfg->ardop_master->power = 1;
  cfg->ardop_master->power_img = new_blank_str();
  cfg->ardop_master->status = new_str("new");
  
  cfg->ardop_slave->start_offset = 0;
  cfg->ardop_slave->end_offset = 0;
  cfg->ardop_slave->patches = 1;
  cfg->ardop_slave->power = 1;
  cfg->ardop_slave->power_img = new_blank_str();
  cfg->ardop_slave->status = new_str("new");
  
  cfg->cpx_autofilter->status = new_str("new");
  
  cfg->coreg_slave->grid = 20;
  cfg->coreg_slave->fft = 1;
  cfg->coreg_slave->sinc = 0;
  cfg->coreg_slave->warp = 0;
  cfg->coreg_slave->status = new_str("new");
  
  cfg->igram_coh->igram = new_blank_str();
  cfg->igram_coh->coh = new_blank_str();
  cfg->igram_coh->min = 0.3;
  cfg->igram_coh->ml = 1;
  cfg->igram_coh->status = new_str("new");
  
  cfg->offset_match->max = 1.0;
  cfg->offset_match->status = new_str("new");
  
  cfg->sim_phase->seeds = new_blank_str();
  cfg->sim_phase->status = new_str("new");
  
  cfg->dinsar->igram = new_blank_str();
  cfg->dinsar->status = new_str("new");
  
  cfg->deramp_ml->status = new_str("new");
  
  cfg->unwrap->algorithm = new_str("escher");
  cfg->unwrap->flattening = 1;
  cfg->unwrap->procs = 8;
  cfg->unwrap->tiles_azimuth = 0;
  cfg->unwrap->tiles_range = 0;
  cfg->unwrap->overlap_azimuth = 400;
  cfg->unwrap->overlap_range = 400;
  cfg->unwrap->filter = 1.6;
  cfg->unwrap->qc = new_blank_str();
  cfg->unwrap->status = new_str("new");
  
  cfg->refine->iter = 0;
  cfg->refine->max = 15;
  cfg->refine->status = new_str("new");
  
  cfg->elevation->dem = new_blank_str();
  cfg->elevation->error = new_blank_str();
  cfg->elevation->status = new_str("new");
  
  cfg->ground_range->status = new_str("new");
  cfg->geocode->dem = new_blank_str();
  cfg->geocode->amp = new_blank_str();
  cfg->geocode->error = new_blank_str();
  cfg->geocode->coh = new_blank_str();
  cfg->geocode->name = new_str("utm");
  cfg->geocode->proj = new_blank_str();
  sprintf(cfg->geocode->proj, "%s/projections/utm/utm.proj", 
	  get_asf_share_dir());
  cfg->geocode->resample = new_str("bilinear");
  cfg->geocode->pixel_spacing = 20;
  cfg->geocode->status = new_str("new");
  
  cfg->export->format = new_str("geotiff");
  cfg->export->status = new_str("new");
  return cfg;
}

dem_config *init_fill_config(char *configFile)
{  
  FILE *fConfig, *fDefaults;
  char line[255], params[25];
  char *test=new_blank_str();
  int i;

  dem_config *cfg = create_config_with_defaults();

  fConfig = FOPEN(configFile, "r");
  i=0;
  while (fgets(line, 255, fConfig) != NULL) {
    if (i==0) strcpy(cfg->comment, line); 
    i++;
    
    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "default values", 14)==0) 
	 read_str(cfg->general->def_val, line, "default values");
    }
  }
  FCLOSE(fConfig);
  
  if (strcmp(cfg->general->def_val, "")!=0) {
    if (!fileExists(cfg->general->def_val)) 
      asfPrintError("default values file does not exist");
    fDefaults = FOPEN(cfg->general->def_val, "r");
    while (fgets(line, 255, fDefaults) != NULL) {
      test = read_param(line);
      if (strncmp(test, "mode", 4)==0) 
	read_str(cfg->general->mode, line, "mode"); 
      if (strncmp(test, "short configuration file", 24)==0) 
	cfg->general->short_config = read_int(line, "short configuration file");
      if (strncmp(test, "reference dem", 13)==0) 
	read_str(cfg->general->dem, line, "reference dem"); 
      if (strncmp(test, "data type", 9)==0) 
	read_str(cfg->general->data_type, line, "data type");
      if (strncmp(test, "coregistration", 14)==0) 
	read_str(cfg->general->coreg, line, "coregistration");
      if (strncmp(test, "doppler", 7)==0) 
	read_str(cfg->general->doppler, line, "doppler");
      if (strncmp(test, "deskew", 6)==0) 
	cfg->general->deskew = read_int(line, "deskew");
      if (strncmp(test, "maximum offset", 14)==0) 
	cfg->general->max_off = read_int(line, "maximum offset");
      if (strncmp(test, "precise master", 14)==0) 
	read_str(cfg->ingest->prc_master, line, "precise master"); 
      if (strncmp(test, "precise slave", 13)==0) 
	read_str(cfg->ingest->prc_slave, line, "precise slave");
      if (strncmp(test, "minimum coherence", 17)==0) 
	cfg->igram_coh->min = read_double(line, "minimum coherence");
      if (strncmp(test, "phase unwrapping", 16)==0) 
	read_str(cfg->unwrap->algorithm, line, "phase unwrapping");
      if (strncmp(test, "tiles per degree", 16)==0) 
	cfg->unwrap->tiles_per_degree = read_int(line, "tiles per degree");
      if (strncmp(test, "tile overlap", 12)==0) {
	cfg->unwrap->overlap_azimuth = read_int(line, "tile overlap");
	cfg->unwrap->overlap_range = cfg->unwrap->overlap_azimuth;
      }
      if (strncmp(test, "projection name", 15)==0) 
	read_str(cfg->geocode->name, line, "projection name");
      if (strncmp(test, "projection file", 15)==0) 
	read_str(cfg->geocode->proj, line, "projection file");
      if (strncmp(test, "resampling method", 17)==0)
	read_str(cfg->geocode->resample, line, "resampling method");
      if (strncmp(test, "pixel spacing", 13)==0) 
	cfg->geocode->pixel_spacing = read_int(line, "pixel spacing");
      free(test);
    }
    FCLOSE(fDefaults);
  }
  
  fConfig = FOPEN(configFile, "r");
  i=0;
  while (fgets(line, 255, fConfig) != NULL) {
    if (i==0) strcpy(cfg->comment, line); 
    i++;
    
    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "mode", 4)==0)
    read_str(cfg->general->mode, line, "mode"); 
      if (strncmp(test, "reference dem", 13)==0) 
    read_str(cfg->general->dem, line, "reference dem"); 
      if (strncmp(test, "base name", 9)==0) 
	read_str(cfg->general->base, line, "base name");
      if (strncmp(test, "data type", 9)==0) 
	read_str(cfg->general->data_type, line, "data_type");
      if (strncmp(test, "coregistration", 14)==0) 
	read_str(cfg->general->coreg, line, "coregistration");
      if (strncmp(test, "doppler", 7)==0) 
	read_str(cfg->general->doppler, line, "doppler");
      if (strncmp(test, "deskew", 6)==0) 
	cfg->general->deskew = read_int(line, "deskew");
      if (strncmp(test, "lat begin", 9)==0) 
	cfg->general->lat_begin = read_double(line, "lat_begin"); 
      if (strncmp(test, "lat end", 7)==0) 
	cfg->general->lat_end = read_double(line, "lat_end"); 
      if (strncmp(test, "coregistration", 14)==0) 
	read_str(cfg->general->coreg, line, "coregistration");
      if (strncmp(test, "maximum offset", 14)==0) 
	cfg->general->max_off = read_int(line, "maximum offset");
      if (strncmp(test, "correlation mask", 16)==0) 
	cfg->general->mflag = read_int(line, "correlation mask");
      if (strncmp(test, "mask file", 9)==0) 
	read_str(cfg->general->mask, line, "mask file");
      if (strncmp(test, "default values", 14)==0) 
	read_str(cfg->general->def_val, line, "default values");
      if (strncmp(test, "test mode", 9)==0) 
	cfg->general->test = read_int(line, "test mode");
      if (strncmp(test, "short configuration file", 24)==0) 
	cfg->general->short_config = read_int(line, "short configuration file");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->general->status, line, "status");
    }
    
    if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
    if (strcmp(params, "master image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) read_str(cfg->master->path, line, "path");
      if (strncmp(test, "data file", 9)==0) 
	read_str(cfg->master->data, line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	read_str(cfg->master->meta, line, "metadata file");
    }
    
    if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
    if (strcmp(params, "slave image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) read_str(cfg->slave->path, line, "path"); 
      if (strncmp(test, "data file", 9)==0) 
	read_str(cfg->slave->data, line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	read_str(cfg->slave->meta, line, "metadata file");
    }
    
  }
  FCLOSE(fConfig);
  
  return cfg;
}

dem_config *read_config(char *configFile, int createFlag)
{
  FILE *fConfig;
  dem_config *cfg=NULL;
  char line[255], params[25];
  char *test=new_blank_str();
  
  cfg = init_fill_config(configFile);
  if (cfg == NULL) asfPrintError("creating configuration structure");
  if (createFlag) return cfg;
  
  fConfig = FOPEN(configFile, "r");
  while (fgets(line, 255, fConfig) != NULL) {
    
    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "mode", 4)==0) read_str(cfg->general->mode, line, "mode"); 
      if (strncmp(test, "reference dem", 13)==0) 
	read_str(cfg->general->dem, line, "reference dem"); 
      if (strncmp(test, "base name", 9)==0) 
	read_str(cfg->general->base, line, "base name");
      if (strncmp(test, "data type", 9)==0) 
	read_str(cfg->general->data_type, line, "data type");
      if (strncmp(test, "deskew", 6)==0) 
	cfg->general->deskew = read_int(line, "deskew");
      if (strncmp(test, "doppler", 7)==0) 
	read_str(cfg->general->doppler, line, "doppler");
      if (strncmp(test, "lat begin", 9)==0) 
	cfg->general->lat_begin = read_double(line, "lat_begin"); 
      if (strncmp(test, "lat end", 7)==0) 
	cfg->general->lat_end = read_double(line, "lat_end"); 
      if (strncmp(test, "coregistration", 14)==0) 
	read_str(cfg->general->coreg, line, "coregistration");
      if (strncmp(test, "maximum offset", 14)==0) 
	cfg->general->max_off = read_int(line, "maximum offset");
      if (strncmp(test, "correlation mask", 16)==0) 
	cfg->general->mflag = read_int(line, "correlation mask");
      if (strncmp(test, "mask file", 9)==0) 
	read_str(cfg->general->mask, line, "mask file");
      if (strncmp(test, "default values", 14)==0) 
	read_str(cfg->general->def_val, line, "default values");
      if (strncmp(test, "test mode", 9)==0) 
	cfg->general->test = read_int(line, "test mode");
      if (strncmp(test, "short configuration file", 24)==0) 
	cfg->general->short_config = read_int(line, "short configuration file");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->general->status, line, "status");
    }
    
    if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
    if (strcmp(params, "master image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) read_str(cfg->master->path, line, "path");
      if (strncmp(test, "data file", 9)==0) 
	read_str(cfg->master->data, line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	read_str(cfg->master->meta, line, "metadata file");
    }
    
    if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
    if (strcmp(params, "slave image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) read_str(cfg->slave->path, line, "path"); 
      if (strncmp(test, "data file", 9)==0) 
	read_str(cfg->slave->data, line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	read_str(cfg->slave->meta, line, "metadata file");
    }
    
    if (strncmp(line, "[Ingest]", 8)==0) strcpy(params, "ingest");
    if (strcmp(params, "ingest")==0) {
      test = read_param(line);
      if (strncmp(test, "precise master", 14)==0) 
	read_str(cfg->ingest->prc_master, line, "precise master"); 
      if (strncmp(test, "precise slave", 13)==0) 
	read_str(cfg->ingest->prc_slave, line, "precise slave");
      if (strncmp(test, "precise orbits", 14)==0) 
	cfg->ingest->prcflag = read_int(line, "precise orbits");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->ingest->status, line, "status");
    }
    
    if (strncmp(line, "[Doppler]", 9)==0) strcpy(params, "doppler");
    if (strcmp(params, "doppler")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->doppler->status, line, "status");
    }
    
    if (strncmp(line, "[Coregister first patch]", 24)==0) strcpy(params, "coreg_p1");
    if (strcmp(params, "coreg_p1")==0) {
      test = read_param(line);
      if (strncmp(test, "patches", 7)==0) 
	cfg->coreg_p1->patches = read_int(line, "patches"); 
      if (strncmp(test, "start master", 12)==0) 
	cfg->coreg_p1->start_master = read_int(line, "start master"); 
      if (strncmp(test, "start slave", 11)==0) 
	cfg->coreg_p1->start_slave = read_int(line, "start slave"); 
      if (strncmp(test, "grid", 4)==0) cfg->coreg_p1->grid = read_int(line, "grid"); 
      if (strncmp(test, "fft", 3)==0) cfg->coreg_p1->fft = read_int(line, "fft"); 
      if (strncmp(test, "offset azimuth", 14)==0) 
	cfg->coreg_p1->off_az = read_int(line, "offset azimuth");
      if (strncmp(test, "offset range", 12)==0) 
	cfg->coreg_p1->off_rng = read_int(line, "offset azimuth");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->coreg_p1->status, line, "status"); 
    }	  
    
    if (strncmp(line, "[Coregister last patch]", 23)==0) strcpy(params, "coreg_pL");
    if (strcmp(params, "coreg_pL")==0) {
      test = read_param(line);
      if (strncmp(test, "patches", 7)==0) 
	cfg->coreg_pL->patches = read_int(line, "patches"); 
      if (strncmp(test, "start master", 12)==0) 
	cfg->coreg_pL->start_master = read_int(line, "start master"); 
      if (strncmp(test, "start slave", 11)==0) 
	cfg->coreg_pL->start_slave = read_int(line, "start slave"); 
      if (strncmp(test, "grid", 4)==0) cfg->coreg_pL->grid = read_int(line, "grid"); 
      if (strncmp(test, "fft", 3)==0) cfg->coreg_pL->fft = read_int(line, "fft"); 
      if (strncmp(test, "offset azimuth", 14)==0) 
	cfg->coreg_pL->off_az = read_int(line, "offset azimuth");
      if (strncmp(test, "offset range", 12)==0) 
	cfg->coreg_pL->off_rng = read_int(line, "offset azimuth");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->coreg_pL->status, line, "status"); 
    }	  
    
    if (strncmp(line, "[doppler_per_patch]", 19)==0) 
      strcpy(params, "doppler_per_patch");
    if (strcmp(params, "doppler_per_patch")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->doppler_per_patch->status, line, "status");
    }
    
    if (strncmp(line, "[ardop - Master image]", 22)==0) strcpy(params, "ardop_master");
    if (strcmp(params, "ardop_master")==0) {
      test = read_param(line);
      if (strncmp(test, "start offset", 12)==0) 
	cfg->ardop_master->start_offset = read_int(line, "start offset"); 
      if (strncmp(test, "end offset", 10)==0) 
	cfg->ardop_master->end_offset = read_int(line, "end offset"); 
      if (strncmp(test, "patches", 7)==0) 
	cfg->ardop_master->patches = read_int(line, "patches");
      if (strncmp(test, "power flag", 10)==0) 
	cfg->ardop_master->power = read_int(line, "power flag"); 
      if (strncmp(test, "power image", 11)==0) 
	read_str(cfg->ardop_master->power_img, line, "power image"); 
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->ardop_master->status, line, "status"); 
    }	  
    
    if (strncmp(line, "[ardop - Slave image]", 21)==0) strcpy(params, "ardop_slave");
    if (strcmp(params, "ardop_slave")==0) {
      test = read_param(line);
      if (strncmp(test, "start offset", 6)==0) 
	cfg->ardop_slave->start_offset = read_int(line, "start offset"); 
      if (strncmp(test, "end offset", 10)==0) 
	cfg->ardop_slave->end_offset = read_int(line, "end offset"); 
      if (strncmp(test, "patches", 7)==0) 
	cfg->ardop_slave->patches = read_int(line, "patches"); 
      if (strncmp(test, "power flag", 10)==0) 
	cfg->ardop_slave->power = read_int(line, "power flag"); 
      if (strncmp(test, "power image", 11)==0) 
	read_str(cfg->ardop_slave->power_img, line, "power image"); 
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->ardop_slave->status, line, "status"); 
    }	  
    
    if (strncmp(line, "[Coregister slave]", 16)==0) strcpy(params, "coreg_slave");
    if (strcmp(params, "coreg_slave")==0) {
      test = read_param(line);
      if (strncmp(test, "grid", 4)==0) 
	cfg->coreg_slave->grid = read_int(line, "grid"); 
      if (strncmp(test, "fft", 3)==0) 
	cfg->coreg_slave->fft = read_int(line, "fft"); 
      if (strncmp(test, "sinc", 4)==0) 
	cfg->coreg_slave->sinc = read_int(line, "sinc"); 
      if (strncmp(test, "warp", 4)==0) 
	cfg->coreg_slave->warp = read_int(line, "warp"); 
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->coreg_slave->status, line, "status"); 
    }	  
    
    if (strncmp(line, "[Interferogram/coherence]", 25)==0) 
      strcpy(params, "igram_coh");
    if (strcmp(params, "igram_coh")==0) {
      test = read_param(line);
      if (strncmp(test, "interferogram", 13)==0) 
	read_str(cfg->igram_coh->igram, line, "interferogram"); 
      if (strncmp(test, "coherence image", 15)==0) 
	read_str(cfg->igram_coh->coh, line, "coherence image");
      if (strncmp(test, "minimum coherence", 17)==0) 
	cfg->igram_coh->min = read_double(line, "minimum coherence");
      if (strncmp(test, "multilook", 9)==0) 
	cfg->igram_coh->ml = read_int(line, "multilook");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->igram_coh->status, line, "status");
    }
    
    if (strncmp(line, "[Offset matching]", 17)==0) strcpy(params, "offset_match");
    if (strcmp(params, "offset_match")==0) {
      test = read_param(line);
      if (strncmp(test, "max", 3)==0) 
	cfg->offset_match->max = read_double(line, "max");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->offset_match->status, line, "status");
    }
    
    if (strncmp(line, "[Simulated phase]", 17)==0) strcpy(params, "simulated phase");
    if (strcmp(params, "simulated phase")==0) {
      test = read_param(line);
      if (strncmp(test, "seeds", 5)==0) 
	read_str(cfg->sim_phase->seeds, line, "seeds");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->sim_phase->status, line, "status");
    }
    
    if (strncmp(line, "[Differential interferogram]", 28)==0) 
      strcpy(params, "differential interferogram");
    if (strcmp(params, "differential interferogram")==0) {
      test = read_param(line);
      if (strncmp(test, "igram", 5)==0) read_str(cfg->dinsar->igram, line, "igram");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->dinsar->status, line, "status");
    }
    
    if (strncmp(line, "[Deramp/multilook]", 18)==0) strcpy(params, "deramp_ml");
    if (strcmp(params, "deramp_ml")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->deramp_ml->status, line, "status");
    }
    
    if (strncmp(line, "[Phase unwrapping]", 18)==0) strcpy(params, "unwrap");
    if (strcmp(params, "unwrap")==0) {
      test = read_param(line);
      if (strncmp(test, "algorithm", 9)==0) 
	read_str(cfg->unwrap->algorithm, line, "algorithm");
      if (strncmp(test, "flattening", 10)==0) 
	cfg->unwrap->flattening = read_int(line, "flattening");
      if (strncmp(test, "processors", 10)==0) 
	cfg->unwrap->procs = read_int(line, "processors");
      if (strncmp(test, "tiles azimuth", 13)==0) 
	cfg->unwrap->tiles_azimuth = read_int(line, "tiles azimuth");
      if (strncmp(test, "tiles range", 11)==0) 
	cfg->unwrap->tiles_range = read_int(line, "tiles range");
      if (strncmp(test, "tiles per degree", 16)==0) 
	cfg->unwrap->tiles_per_degree = read_int(line, "tiles per degree");
      if (strncmp(test, "overlap azimuth", 15)==0) 
	cfg->unwrap->overlap_azimuth = read_int(line, "overlap azimuth");
      if (strncmp(test, "overlap range", 13)==0) 
	cfg->unwrap->overlap_range = read_int(line, "overlap range");
      if (strncmp(test, "filter", 6)==0) 
	cfg->unwrap->filter = read_double(line, "filter");
      if (strncmp(test, "quality control", 15)==0) 
	read_str(cfg->unwrap->qc, line, "quality control");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->unwrap->status, line, "status");
    }

    if (strncmp(line, "[Baseline refinement]", 21)==0) strcpy(params, "refine");
    if (strcmp(params, "refine")==0) {
      test = read_param(line);
      if (strncmp(test, "iter", 4)==0) 
	cfg->refine->iter = read_int(line, "iterations");
      if (strncmp(test, "max", 3)==0) 
	cfg->refine->max = read_int(line, "max iterations");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->refine->status, line, "status");
    }
    
    if (strncmp(line, "[Elevation]", 11)==0) strcpy(params, "elev");
    if (strcmp(params, "elev")==0) {
      test = read_param(line);
      if (strncmp(test, "dem", 3)==0) read_str(cfg->elevation->dem, line, "dem");
      if (strncmp(test, "error map", 9)==0) 
	read_str(cfg->elevation->error, line, "error map");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->elevation->status, line, "status");
    }
    
    if (strncmp(line, "[Ground range DEM]", 18)==0) strcpy(params, "final_dem");
    if (strcmp(params, "final_dem")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->ground_range->status, line, "status");   
    }
    
    if (strncmp(line, "[Geocoding]", 11)==0) strcpy(params, "geocoding");
    if (strcmp(params, "geocoding")==0) {
      test = read_param(line);
      if (strncmp(test, "dem", 3)==0) read_str(cfg->geocode->dem, line, "dem");
      if (strncmp(test, "error map", 9)==0) 
	read_str(cfg->geocode->error, line, "error map");
      if (strncmp(test, "amplitude", 9)==0) 
	read_str(cfg->geocode->amp, line, "amplitude");
      if (strncmp(test, "coherence", 9)==0) 
	read_str(cfg->geocode->coh, line, "coherence");
      if (strncmp(test, "projection name", 15)==0) 
	read_str(cfg->geocode->name, line, "projection name");
      if (strncmp(test, "projection file", 15)==0) 
	read_str(cfg->geocode->proj, line, "projection file");
      if (strncmp(test, "resampling method", 17)==0)
	read_str(cfg->geocode->resample, line, "resampling method");
      if (strncmp(test, "pixel spacing", 13)==0) 
	cfg->geocode->pixel_spacing = read_double(line, "pixel spacing");
      if (strncmp(test, "status", 6)==0) 
	read_str(cfg->geocode->status, line, "status");   
    }
    
    if (strncmp(line, "[Export]", 8)==0) strcpy(params, "export");
    if (strcmp(params, "export")==0) {
      test = read_param(line);
      if (strncmp(test, "format", 6)==0)
	read_str(cfg->export->format, line, "format");
      if (strncmp(test, "status", 6)==0)
	read_str(cfg->export->status, line, "status");
    }
  }
  FCLOSE(fConfig);
  
  return cfg;	
  
}

int write_config(char *configFile, dem_config *cfg)
{
  FILE *fConfig;
  int shortFlag=FALSE;
  
  if (cfg == NULL) 
    asfPrintError("no configuration structure to write");
  if (cfg->general->short_config)
    shortFlag = TRUE;
  fConfig = FOPEN(configFile, "w");
  
  fprintf(fConfig, "%s\n", cfg->comment);

  // [General] section
  fprintf(fConfig, "[General]\n");
  // mode
  if (!shortFlag)
    fprintf(fConfig, "\n# The interferometric processing system 'ips' can be run in two\n"
	    "# different modes. The main mode is DEM for the generation of digital elevation\n"
	    "# models. The DINSAR mode for differential interferometry is still under\n"
	    "# development.\n\n");
  fprintf(fConfig, "mode = %s\n", cfg->general->mode);
  // reference DEM
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter looks for the location of the reference DEM file\n"
            "# The reference DEM is used in various parts of the SAR interferometric\n"     
	    "# processing flow, mostly prominently for the phase unwrapping.\n\n");
  fprintf(fConfig, "reference dem = %s\n", cfg->general->dem);
  // output basename
  if (!shortFlag)
    fprintf(fConfig, "\n# The ips saves a large number of intermediate and final results.\n"
          "# All the files relevant for further analysis will start with this basename\n\n");
  fprintf(fConfig, "base name = %s\n", cfg->general->base);
  // data type
  if (!shortFlag)
    fprintf(fConfig, "\n# The ips handles three different data types. The most flexible type\n"
            "# is the level zero Sky Telemetry Format (STF). This swath data type allows for\n"
	    "# variable area sizes that are processed. The second data type is RAW for CEOS\n"
	    "# level zero data. The third supported data type is single look complex data"
	    "(SLC)\n\n");
  fprintf(fConfig, "data type = %s\n", cfg->general->data_type);
  // deskew flag
  if (!shortFlag)
    fprintf(fConfig, "\n# The deskew flag indicates whether the raw data is SAR processed in\n"
	    "# in zero Doppler geometry or not (1 for deskewing, 0 for regular processing)\n\n");
  fprintf(fConfig, "deskew = %d\n", cfg->general->deskew);
  // Doppler processing
  if (!shortFlag)
    fprintf(fConfig, "\n# For the SAR processing, two different schemes for chosing the\n"
            "# Doppler values have been considered. Currently only the processing to the\n"
            "# 'average' Doppler values of the image pair is used. The alternative approach\n"
            "# that uses 'updated' Doppler values has not been implemented.\n\n");
  fprintf(fConfig, "doppler = %s\n", cfg->general->doppler);
  // latitude constraints
  if (!shortFlag)
    fprintf(fConfig, "\n# For effectively using swath data the user can define latitude\n"
            "# constraints to select a subset of the swath data (-99 indicates that no\n"
            "# latitude constraint is chosen).\n\n");
    fprintf(fConfig, "lat begin = %.3f\n", cfg->general->lat_begin);
    fprintf(fConfig, "lat end = %.3f\n", cfg->general->lat_end);
  // coregistration
  if (!shortFlag)
    fprintf(fConfig, "\n# Matching up the first and last patches of an image pair leads to\n"
            "# the best results. For this approach use the 'PATCH' option. Once this method\n"
            "# fails you can use the 'FRAME' option to match up master and slave image in\n"
            "# its entirety.\n\n");
  fprintf(fConfig, "coregistration = %s\n", cfg->general->coreg);
  // maximum offset
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter defines the maximum allowed pixel offset in range\n"
            "# or azimuth after the initial co-registration has been performed. Three pixels\n"
	    "# is an empirical value that worked in most cases.\n\n");
  fprintf(fConfig, "maximum offset = %d\n", cfg->general->max_off);
  /* Keep the mask related stuff out until it is actually implemented
  fprintf(fConfig, "correlation mask = < flag for using a mask for co-registration: "
	  "0 | 1 (not implemented yet) >\n");
  fprintf(fConfig, "mask file = < mask file location >\n");
  */
  // default values file
  if (!shortFlag) {
    fprintf(fConfig, "\n# The default values file is used to define the user's preferred\n"
            "# parameter settings. In most cases, you will work on a study where your area\n"
            "# of interest is geographically well defined. You want the data for the entire\n"
            "# project in the same projection, with the same pixel spacing and the same\n"
            "# output format.\n");
    fprintf(fConfig, "# A sample of a default values file can be located in %s/ips.\n\n",
	    get_asf_share_dir());
  }
  fprintf(fConfig, "default values = %s\n", cfg->general->def_val);
  // test mode
  if (!shortFlag)
    fprintf(fConfig, "\n# The test mode is for internal use only (1 for test mode on, 0 for\n"
            "# test mode off).\n\n");
  fprintf(fConfig, "test mode = %d\n", cfg->general->test);
  // short configuration file flag
  if (!shortFlag)
    fprintf(fConfig, "\n# The short configuration file flag allows the experienced user to\n"
            "# generate configuration files without the verbose comments that explain all\n"
            "# entries for the parameters in the configuration file (1 for a configuration\n"
            "# without comments, 0 for a configuration file with verbose comments).\n\n");
  fprintf(fConfig, "short configuration file = %d\n", cfg->general->short_config);
  // general status
  if (!shortFlag)
    fprintf(fConfig, "\n# The general status field indicates the progress of the processing.\n"
            "# The status 'new' indicates that the configuration has only been initialized\n"
	    "# but not run yet. For each new run the status needs to be set back to 'new'\n"
	    "# before running a data set again. Once the processing starts the status changes\n"
	    "# to 'processing'. When the processing is complete it is changed to 'success'"
	    "\n\n");
  fprintf(fConfig, "status = %s\n\n\n", cfg->general->status);

  // [Master] section
  fprintf(fConfig, "[Master image]\n");
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter gives the path of the master image data.\n\n");
  fprintf(fConfig, "path = %s\n", cfg->master->path);
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter gives the name of the master data file.\n"
	    "# Swath data has usually an extension .000, whereas CEOS data has an extension.\n"
	    "# .D\n\n");
  fprintf(fConfig, "data file = %s\n", cfg->master->data);
  if (!shortFlag) 
    fprintf(fConfig, "\n# This parameter gives the name of the master metadata file.\n"
	    "# Swath data has usually an extension .par, whereas CEOS data has an extension.\n"
	    "# .L\n\n");		  
  fprintf(fConfig, "metadata file = %s\n\n\n", cfg->master->meta);
	  
  // [Slave] section
  fprintf(fConfig, "[Slave image]\n");
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter gives the path of the slave image data.\n\n");
  fprintf(fConfig, "path = %s\n", cfg->slave->path);
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter gives the name of the slave data file.\n"
            "# Swath data has usually an extension .000, whereas CEOS data has an extension.\n"
            "# .D\n\n");
  fprintf(fConfig, "data file = %s\n", cfg->slave->data);
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter gives the name of the slave metadata file.\n"
            "# Swath data has usually an extension .par, whereas CEOS data has an extension.\n"
            "# .D\n\n");		  
  fprintf(fConfig, "metadata file = %s\n\n\n", cfg->slave->meta);

  // SWATH data
  if (strncmp(cfg->general->data_type, "STF", 3)==0) {
    // [Ingest] section
    fprintf(fConfig, "[Ingest]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the location of the precision state\n"
	      "# vectors provided by the German Aerospace Center (DLR) for the master image."
	      "\n\n");
    fprintf(fConfig, "precise master = %s\n", cfg->ingest->prc_master);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the location of the precision state\n"
	      "# vectors provided by the German Aerospace Center (DLR) for the slave image."
	      "\n\n");
    fprintf(fConfig, "precise slave = %s\n", cfg->ingest->prc_slave);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines whether precision state vectors should be used\n"
	      "# or not (1 for using precision state vectors, 0 for not using precision\n"
	      "# state vectors). This funcionality is not fully implemented yet.\n\n");
    fprintf(fConfig, "precise orbits = %d\n", cfg->ingest->prcflag);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ingest->status);
    if (strncmp(cfg->general->doppler, "average", 7)==0) {
      // [Doppler] section
      fprintf(fConfig, "[Doppler]\n");
      if (!shortFlag)
        fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
                "# The status 'new' indicates that this processing step has not been\n"
		"# performed. When the processing is complete it is changed to 'success'\n"
		"# The processing flow can be interrupted by setting the status to 'stop'\n\n");
      fprintf(fConfig, "status = %s\n\n\n", cfg->doppler->status);
    }
    // [Coregister first patch] section
    fprintf(fConfig, "[Coregister first patch]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the number of patches that are used\n"
	      "# during the co-registration of the upper part of the images. Ideally the\n"
	      "# images correlate with one patch. At times, two patches might be required\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_p1->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the first patch of the master image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_p1->start_master);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the first patch of the slave image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_p1->start_slave);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter determines the number of pixels that define the\n"
              "# grid that is used for the FFT match.\n\n");	  
    fprintf(fConfig, "grid = %d\n", cfg->coreg_p1->grid);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines whether a complex FFT is used for the\n"
	      "# fine co-registration instead of the coherence (1 for complex FFT match,\n"
	      "# 0 for FFT match using coherence). Complex FFT matches usually lead to\n"
	      "# better matching results.\n\n");
    fprintf(fConfig, "fft = %d\n", cfg->coreg_p1->fft);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in azimuth direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_p1->off_az);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in range direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_p1->off_rng);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->coreg_p1->status);
    // [Coregister last patch] section
    fprintf(fConfig, "[Coregister last patch]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the number of patches that are used\n"
	      "# during the co-registration of the lower part of the images. Ideally the\n"
	      "# images correlate with one patch. At times, two patches might be required\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_pL->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the last patch of the master image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the last patch of the slave image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter determines the number of pixels that define the\n"
              "# grid that is used for the FFT match\n\n");	  
    fprintf(fConfig, "grid = %d\n", cfg->coreg_pL->grid);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines whether a complex FFT is used for the\n"
	      "# fine co-registration instead of the coherence (1 for complex FFT match,\n"
	      "# 0 for FFT match using coherence). Complex FFT matches usually lead to\n"
	      "# better matching results.\n\n");
    fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in azimuth direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_pL->off_az);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in range direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_pL->off_rng);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->coreg_pL->status);
    if (strncmp(cfg->general->doppler, "updated", 7)==0) {
      // [doppler_per_patch] section
      fprintf(fConfig, "[doppler_per_patch]\n");
      if (!shortFlag)
        fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
                "# The status 'new' indicates that this processing step has not been\n"
		"# performed. When the processing is complete it is changed to 'success'\n"
		"# The processing flow can be interrupted by setting the status to 'stop'\n\n");
      fprintf(fConfig, "status = %s\n\n\n", cfg->doppler_per_patch->status);
    }
    // [ardop - Master image] section
    fprintf(fConfig, "[ardop - Master image]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the start offset determined by the\n"
	      "# first patch co-registration for the master image.\n\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->ardop_master->start_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the end offset determined by the\n"
	      "# last patch co-registration for the master image.\n\n");
    fprintf(fConfig, "end offset = %ld\n", cfg->ardop_master->end_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates how many patches of data have been\n"
	      "# for the master image.\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->ardop_master->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines whether a power image is created while\n"
	      "# processing the master image (1 for generating a power image, 0 for not\n"
	      "# generating a power image).\n\n");
    fprintf(fConfig, "power flag = %d\n", cfg->ardop_master->power);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the file name of the master power image."
	      "\n\n");
    fprintf(fConfig, "power image = %s\n", cfg->ardop_master->power_img);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ardop_master->status);
    // [ardop - Slave image] section
    fprintf(fConfig, "[ardop - Slave image]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the start offset determined by the\n"
	      "# the first patch co-registration for the slave image.\n\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->ardop_slave->start_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the end offset determined by the\n"
	      "# the last patch co-registration for the slave image.\n\n");
    fprintf(fConfig, "end offset = %ld\n", cfg->ardop_slave->end_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates how many patches of data have been\n"
	      "# for the slave image.\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->ardop_slave->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines whether a power image is created while\n"
	      "# processing the slave image (1 for generating a power image, 0 for not\n"
	      "# generating a power image.\n\n");
    fprintf(fConfig, "power flag = %d\n", cfg->ardop_slave->power);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the file name of the slave power image."
	      "\n\n");
    fprintf(fConfig, "power image = %s\n", cfg->ardop_slave->power_img);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ardop_slave->status);
  }
  
  // CEOS level zero data
  if (strncmp(cfg->general->data_type, "RAW", 3)==0) {
    // [Ingest] section
    fprintf(fConfig, "[Ingest]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ingest->status);
    if (strncmp(cfg->general->doppler, "average", 7)==0) {
      // {Doppler] section
      fprintf(fConfig, "[Doppler]\n");
      if (!shortFlag)
        fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
                "# The status 'new' indicates that this processing step has not been\n"
		"# performed. When the processing is complete it is changed to 'success'\n"
		"# The processing flow can be interrupted by setting the status to 'stop'\n\n");
      fprintf(fConfig, "status = %s\n\n\n", cfg->doppler->status);
    }
    // [Coregister first patch] section
    fprintf(fConfig, "[Coregister first patch]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the number of patches that are used\n"
	      "# during the co-registration of the upper part of the images. Ideally the\n"
	      "# images correlate with one patch. At times, two patches might be required\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_p1->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the first patch of the master image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_p1->start_master);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the first patch of the slave image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_p1->start_slave);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter determines the number of pixels that define the\n"
              "# grid that is used for the FFT match\n\n");	  
    fprintf(fConfig, "grid = %d\n", cfg->coreg_p1->grid);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines whether a complex FFT is used for the\n"
	      "# fine co-registration instead of the coherence (1 for complex FFT match,\n"
	      "# 0 for FFT match using coherence). Complex FFT matches usually lead to\n"
	      "# better matching results.\n\n");
    fprintf(fConfig, "fft = %d\n", cfg->coreg_p1->fft);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in azimuth direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_p1->off_az);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in range direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_p1->off_rng);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->coreg_p1->status);
    // [Coregister last patch] section
    fprintf(fConfig, "[Coregister last patch]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the number of patches that are used\n"
	      "# during the co-registration of the lower part of the images. Ideally the\n"
	      "# images correlate with one patch. At times, two patches might be required\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_pL->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the last patch of the master image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates at which line number the processing\n"
	      "# of the last patch of the slave image is started. This can be changed \n"
	      "# when the initial co-registration does not succeed.\n\n");
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter determines the number of pixels that define the\n"
              "# grid that is used for the FFT match\n\n");	  
    fprintf(fConfig, "grid = %d\n", cfg->coreg_pL->grid);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines whether a complex FFT is used for the\n"
	      "# fine co-registration instead of the coherence (1 for complex FFT match,\n"
	      "# 0 for FFT match using coherence). Complex FFT matches usually lead to\n"
	      "# better matching results.\n\n");
    fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in azimuth direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_pL->off_az);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the pixel offset in range direction\n"
	      "# the matching algorithm determined.\n\n");
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_pL->off_rng);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->coreg_pL->status);
    if (strncmp(cfg->general->doppler, "updated", 7)==0) {
      fprintf(fConfig, "[doppler_per_patch]\n");
      fprintf(fConfig, "status = %s\n\n\n", cfg->doppler_per_patch->status);
    }
    fprintf(fConfig, "[ardop - Master image]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the start offset determined by the\n"
	      "# the first patch co-registration for the master image.\n\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->ardop_master->start_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the end offset determined by the\n"
	      "# the last patch co-registration for the master image.\n\n");
    fprintf(fConfig, "end offset = %ld\n", cfg->ardop_master->end_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates how many patches of data have been\n"
	      "# for the master image.\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->ardop_master->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines whether a power image is created while\n"
	      "# processing the master image (1 for generating a power image, 0 for not\n"
	      "# generating a power image).\n\n");
    fprintf(fConfig, "power flag = %d\n", cfg->ardop_master->power);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the file name of the master power image."
	      "\n\n");
    fprintf(fConfig, "power image = %s\n", cfg->ardop_master->power_img);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ardop_master->status);
    fprintf(fConfig, "[ardop - Slave image]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the start offset determined by the\n"
	      "# the first patch co-registration for the slave image.\n\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->ardop_slave->start_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates the end offset determined by the\n"
	      "# the last patch co-registration for the slave image.\n\n");
    fprintf(fConfig, "end offset = %ld\n", cfg->ardop_slave->end_offset);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter indicates how many patches of data have been\n"
	      "# for the slave image.\n\n");
    fprintf(fConfig, "patches = %d\n", cfg->ardop_slave->patches);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines whether a power image is created while\n"
	      "# processing the slave image (1 for generating a power image, 0 for not\n"
	      "# generating a power image.\n\n");
    fprintf(fConfig, "power flag = %d\n", cfg->ardop_slave->power);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the file name of the slave power image."
	      "\n\n");
    fprintf(fConfig, "power image = %s\n", cfg->ardop_slave->power_img);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ardop_slave->status);
  }
  
  // Single look complex (SLC) data
  if (strncmp(cfg->general->data_type, "SLC", 3)==0) {
    // [Ingest] section
    fprintf(fConfig, "[Ingest]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ingest->status);
    // [Coregister slave] section
    fprintf(fConfig, "[Coregister slave]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter determines the number of pixels that define the\n"
              "# grid that is used for the FFT match\n\n");	  
    fprintf(fConfig, "grid = %d\n", cfg->coreg_slave->grid);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines whether a complex FFT is used for the\n"
	      "# fine co-registration instead of the coherence (1 for complex FFT match,\n"
	      "# 0 for FFT match using coherence). Complex FFT matches usually lead to\n"
	      "# better matching results.\n\n");
    fprintf(fConfig, "fft = %d\n", cfg->coreg_slave->fft);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines the use of a sinc function in the interpolation\n"
	      "# during the remapping of the slave image into the geometry of the master\n"
	      "# image (1 for using the sinc function, 0 for not using it).\n\n");
    fprintf(fConfig, "sinc = %d\n", cfg->coreg_slave->sinc);
    if (!shortFlag)
      fprintf(fConfig, "\n# This flag defines the use of a warp function for the remapping\n"
	      "# of the slave image into the master image geometry (1 for using the warp\n"
	      "# function, 0 for not using it). The warp function generates a horizontal\n"
	      "# and vertical vector field that accomodate higher order distortions.\n\n");
    fprintf(fConfig, "warp = %d\n", cfg->coreg_slave->warp);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->coreg_slave->status);
  }
  
  // [Interferogram/coherence] section
  fprintf(fConfig, "[Interferogram/coherence]\n");
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter defines the file name of the interferogram\n\n");
  fprintf(fConfig, "interferogram = %s\n", cfg->igram_coh->igram);
  if (!shortFlag)
    fprintf(fConfig, "\n# This parameter defines the file name of the coherence image\n\n");
  fprintf(fConfig, "coherence image = %s\n", cfg->igram_coh->coh);
  if (!shortFlag)
    fprintf(fConfig, "\n# The minimum coherence level defines the threshold for the\n"
	    "# interferometric processing flow to interrupt the processing. In case the\n"
	    "# average of an image pair is below this threshold the ips automatically\n"
	    "# aborts any further processing. This way the low average coherence is used\n"
	    "# as an indicator of co-registration problems.\n\n");
  fprintf(fConfig, "minimum coherence = %.1f\n", cfg->igram_coh->min);
  if (!shortFlag)
    fprintf(fConfig, "\n# This indicates whether a multilooked version of the interferogram\n"
	    "# is stored (1 for generating a multilooked interferogram, 0 for not generating\n"
	    "# one).\n\n");
  fprintf(fConfig, "multilook = %d\n", cfg->igram_coh->ml);
  if (!shortFlag)
    fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
            "# The status 'new' indicates that this processing step has not been\n"
	    "# performed. When the processing is complete it is changed to 'success'\n"
	    "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
  fprintf(fConfig, "status = %s\n\n\n", cfg->igram_coh->status);
  // [Offset matching] section
  fprintf(fConfig, "[Offset matching]\n");
  if (!shortFlag)
    fprintf(fConfig, "\n# Maximum pixel offset allowed during matching with reference\n"
	    "# DEM.\n\n");
  fprintf(fConfig, "max = %.1lf\n", cfg->offset_match->max);
  if (!shortFlag)
    fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
            "# The status 'new' indicates that this processing step has not been\n"
	    "# performed. When the processing is complete it is changed to 'success'\n"
	    "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
  fprintf(fConfig, "status = %s\n\n\n", cfg->offset_match->status);
  // [Simulated phase] section
  fprintf(fConfig, "[Simulated phase]\n");
  if (!shortFlag)
    fprintf(fConfig, "\n# Name of the file containing seed points used in the phase\n"
            "# unwrapping process. Seed points are selected on a regular grid and represent\n"
	    "# points with minimum slope.\n\n");
  fprintf(fConfig, "seeds = %s\n", cfg->sim_phase->seeds);
  if (!shortFlag)
    fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
            "# The status 'new' indicates that this processing step has not been\n"
	    "# performed. When the processing is complete it is changed to 'success'\n"
	    "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
  fprintf(fConfig, "status = %s\n\n\n", cfg->sim_phase->status);
  
  if (strncmp(cfg->general->mode, "DINSAR", 6)==0) {
    // [Differential interferogram] section
    fprintf(fConfig, "[Differential interferogram]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the file name of the differential\n"
	      "# interferogram.\n\n");
    fprintf(fConfig, "igram = %s\n", cfg->dinsar->igram);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->dinsar->status);
  }
  
  if (strncmp(cfg->general->mode, "DEM", 3)==0) {
    // [Deramp/multilook] section
    fprintf(fConfig, "[Deramp/multilook]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->deramp_ml->status);
    // [Phase unwrapping] section
    fprintf(fConfig, "[Phase unwrapping]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# Name of the phase unwrapping algorithm used.\n"
	      "# Currently two phase unwrapping algorithms are supported. 'escher' is an\n"
	      "# implementation of Goldstein's branch cut algorithm. 'snaphu' has been\n"
	      "# developed and is distributed by Stanford University. It uses a minimum\n"
	      "# cost flow network.\n\n");
    fprintf(fConfig, "algorithm = %s\n", cfg->unwrap->algorithm);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameters defines whether a topographic phase based on\n"
	      "# an ellipsoidal approximation is subtracted from the phase before the\n"
	      "# phase unwrapping.\n\n");
    fprintf(fConfig, "flattening = %d\n", cfg->unwrap->flattening);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter sets the number of processors used for the\n"
	      "# phase unwrapping (only valid when using 'snaphu').\n\n");
    fprintf(fConfig, "processors = %d\n", cfg->unwrap->procs);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the number of tiles in azimuth direction\n"
	      "# used by the 'snaphu' phase unwrapping algorithm.\n\n");
    fprintf(fConfig, "tiles azimuth = %d\n", cfg->unwrap->tiles_azimuth);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the number of tiles in range direction\n"
	      "# used by the 'snaphu' phase unwrapping algorithm.\n\n");
    fprintf(fConfig, "tiles range = %d\n", cfg->unwrap->tiles_range);
    if (!shortFlag)
      fprintf(fConfig, "\n# Alternatively, the number of tiles used by 'snaphu' in azimuth\n"
	      "# direction can defined per degree.\n\n");
    fprintf(fConfig, "tiles per degree = %d\n", cfg->unwrap->tiles_per_degree);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the overlap between tiles in the azimuth\n"
	      "# direction (only valid when using 'snaphu').\n\n");
    fprintf(fConfig, "overlap azimuth = %d\n", cfg->unwrap->overlap_azimuth);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the overlap between tiles in the range\n"
	      "# direction (only valid when using 'snaphu').\n\n");
    fprintf(fConfig, "overlap range = %d\n", cfg->unwrap->overlap_range);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the weighting factor used for the\n"
	      "# phase filtering (default value: 1.6).\n\n");
    fprintf(fConfig, "filter = %.1f\n", cfg->unwrap->filter);
    if (!shortFlag)
      fprintf(fConfig, "\n# Name of the quality control file generated when using the\n"
	      "# snaphu phase unwrapping algorithm.\n\n");
    fprintf(fConfig, "quality control = %s\n", cfg->unwrap->qc);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->unwrap->status);
    // [Baseline refinement] section
    fprintf(fConfig, "[Baseline refinement]\n");
    if (!shortFlag)
      fprintf(fConfig, "# Number of iterations used in the baseline refinement.\n\n");
    fprintf(fConfig, "iterations = %d\n", cfg->refine->iter);
    if (!shortFlag)
      fprintf(fConfig, "# This parameter defines the maxiumum number of iterations allowed\n"
	      "# for the iterative determination of the interferometric baseline.\n\n");
    fprintf(fConfig, "max iterations = %d\n", cfg->refine->max);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->refine->status);
    // [Elevation] section
    fprintf(fConfig, "[Elevation]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# File name of the elevation model in slant range.\n\n");
    fprintf(fConfig, "dem = %s\n", cfg->elevation->dem);
    if (!shortFlag)
      fprintf(fConfig, "\n# File name of the error map generated in slant range.\n\n");
    fprintf(fConfig, "error map = %s\n", cfg->elevation->error);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->elevation->status);
    // [Ground range DEM] section
    fprintf(fConfig, "[Ground range DEM]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->ground_range->status);
    // [Geocoding] section
    fprintf(fConfig, "[Geocoding]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# File name of the geocoded digital elevation model.\n\n");
    fprintf(fConfig, "dem = %s\n", cfg->geocode->dem);
    if (!shortFlag)
      fprintf(fConfig, "\n# File name of the geocoded error map.\n\n");
    fprintf(fConfig, "error map = %s\n", cfg->geocode->error);
    if (!shortFlag)
      fprintf(fConfig, "\n# File name of the geocoded amplitude image.\n\n");
    fprintf(fConfig, "amplitude = %s\n", cfg->geocode->amp);
    if (!shortFlag)
      fprintf(fConfig, "\n# File name of the geocoded coherence image.\n\n");
    fprintf(fConfig, "coherence = %s\n", cfg->geocode->coh);
    if (!shortFlag)
      fprintf(fConfig, "\n# Name of the projection used for the geocoding.\n"
	      "# There are currently five projections supported: UTM, Polar Stereographic,\n"
	      "# Albers Conic Equal-Area, Lambert Conformal Conic and Lambert Azimuthal\n"
	      "# Equal-Area projection.\n\n");
    fprintf(fConfig, "projection name = %s\n", cfg->geocode->name);
    if (!shortFlag)
      fprintf(fConfig, "\n# Name of the projection parameter file.\n\n");
    fprintf(fConfig, "projection file = %s\n", cfg->geocode->proj);
    if (!shortFlag)
      fprintf(fConfig, "\n# Resampling method used for the geocoding of data.\n"
	      "# Currently three resampling method are supported: nearest neighbor,\n"
	      "# bilinear (default) and bicuc.\n\n");
    fprintf(fConfig, "resampling method = %s\n", cfg->geocode->resample);
    if (!shortFlag)
      fprintf(fConfig, "\n# This parameter defines the pixel spacing for the geocoded\n"
	      "# products.\n\n");
    fprintf(fConfig, "pixel spacing = %.1lf\n", cfg->geocode->pixel_spacing);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->geocode->status);
    // [Export] section
    fprintf(fConfig, "[Export]\n");
    if (!shortFlag)
      fprintf(fConfig, "\n# The name of the format all geocoded results are exported to.\n"
	      "# For using the geocoded results in any commerical image processing and GIS\n"
	      "# the 'geotiff' is the most reliable. For simple visualization 'jpeg' or\n"
	      "# 'tiff' do just fine.\n\n");
    fprintf(fConfig, "format = %s\n", cfg->export->format);
    if (!shortFlag)
      fprintf(fConfig, "\n# The status field indicates the progress of the processing.\n"
              "# The status 'new' indicates that this processing step has not been\n"
	      "# performed. When the processing is complete it is changed to 'success'\n"
	      "# The processing flow can be interrupted by setting the status to 'stop'\n\n");
    fprintf(fConfig, "status = %s\n\n\n", cfg->export->status);
  }
  
  FCLOSE(fConfig);
  
  return(0);
}

