#include "asf.h"
#include "ips.h"

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
  
  sprintf(value, "");
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
  
  fprintf(fConfig, "ips configuration file\n\n");
  fprintf(fConfig, "[General]\n");
  fprintf(fConfig, "mode = < DEM | DINSAR (under development) >\n");
  fprintf(fConfig, "reference dem = < reference DEM file location >\n");
  fprintf(fConfig, "base name = < basename of output files >\n");
  fprintf(fConfig, "data type = < STF | RAW | SLC >\n");
  fprintf(fConfig, "deskew = < flag for processing raw data to zero Doppler: "
	  "0 | 1 >\n");
  fprintf(fConfig, "doppler = < average | updated (not implemented yet) >\n");
  fprintf(fConfig, "lat begin = < begin of latitude constraint for swath data - "
	  "if not used: -99.0 >\n");
  fprintf(fConfig, "lat end = < end of latitude constraint for swath data - "
	  "if not used: -99.0 >\n");
  fprintf(fConfig, "coregistration = < PATCH | FRAME >\n");
  fprintf(fConfig, "maximum offset = < maximum offset allowed in initial "
	  "co-registration >\n");
  /* Keep the mask related stuff out until it is actually implemented
  fprintf(fConfig, "correlation mask = < flag for using a mask for co-registration: "
	  "0 | 1 (not implemented yet) >\n");
  fprintf(fConfig, "mask file = < mask file location >\n");
  */
  fprintf(fConfig, "default values = < default values file location >\n");
  fprintf(fConfig, "test mode = < test mode for internal use only - "
	  "default value: 0 >\n");
  fprintf(fConfig, "status = new\n\n");
  fprintf(fConfig, "[Master image]\n");
  fprintf(fConfig, "path = < path of master image data >\n");
  fprintf(fConfig, "data file = < name of the master data file: .000 or .D >\n");
  fprintf(fConfig, "metadata file = < name of the master metadata file: "
	  ".par or .L >\n\n");
  fprintf(fConfig, "[Slave image]\n");
  fprintf(fConfig, "path = < path of slave image data >\n");
  fprintf(fConfig, "data file = < name of the slave data file: .000 or .D >\n");
  fprintf(fConfig, "metadata file = < name of the slave metadata file: "
	  ".par or .L >\n\n");
  
  FCLOSE(fConfig);
  
  printf("   Initialized basic configuration file\n\n");
  
  return(0);
}

dem_config *init_fill_config(char *configFile)
{
#define newStruct(type) (type *)MALLOC(sizeof(type))
  
  FILE *fConfig, *fDefaults;
  char line[255], params[25];
  char *test=(char *)MALLOC(sizeof(char)*255);
  int i;
  
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
  cfg->aisp_master = newStruct(s_aisp);
  cfg->aisp_slave = newStruct(s_aisp);
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
  
  cfg->general->mode = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->mode, "");
  cfg->general->dem = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->dem, "");
  cfg->general->def_val = (char *)MALLOC(sizeof(char)*255);
  cfg->general->def_val = "";
  cfg->general->base = (char *)MALLOC(sizeof(char)*255);
  cfg->general->base = "";
  cfg->general->data_type = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->general->data_type, "");
  cfg->general->deskew = 0;
  cfg->general->doppler = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->general->doppler, "");
  cfg->general->lat_begin = -99;
  cfg->general->lat_end = 99;
  cfg->general->coreg = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->general->coreg, "PATCH");
  cfg->general->max_off = 3;
  cfg->general->mflag = 0;
  cfg->general->mask = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->general->mask, "");
  cfg->general->test = 0;
  cfg->general->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->general->status, "new");
  
  cfg->master->path = (char *)MALLOC(sizeof(char)*255);
  cfg->master->path = "";
  cfg->master->data = (char *)MALLOC(sizeof(char)*255);
  cfg->master->data = "";
  cfg->master->meta = (char *)MALLOC(sizeof(char)*255);
  cfg->master->meta = "";
  
  cfg->slave->path = (char *)MALLOC(sizeof(char)*255);
  cfg->slave->path = "";
  cfg->slave->data = (char *)MALLOC(sizeof(char)*255);
  cfg->slave->data = "";
  cfg->slave->meta = (char *)MALLOC(sizeof(char)*255);
  cfg->slave->meta = "";
  
  cfg->ingest->prc_master = (char *)MALLOC(sizeof(char)*255);
  cfg->ingest->prc_master = "";
  cfg->ingest->prc_slave = (char *)MALLOC(sizeof(char)*255);
  cfg->ingest->prc_slave = "";
  cfg->ingest->prcflag = 0;
  cfg->ingest->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->ingest->status, "new");
  
  cfg->doppler->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->doppler->status, "new");
  
  cfg->coreg_p1->patches = 1;
  cfg->coreg_p1->start_master = 0;
  cfg->coreg_p1->start_slave = 0;
  cfg->coreg_p1->grid = 20;
  cfg->coreg_p1->fft = 1;
  cfg->coreg_p1->off_az = 0;
  cfg->coreg_p1->off_rng = 0;
  cfg->coreg_p1->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->coreg_p1->status, "new");
  
  cfg->coreg_pL->patches = 1;
  cfg->coreg_pL->start_master = 0;
  cfg->coreg_pL->start_slave = 0;
  cfg->coreg_pL->grid = 20;
  cfg->coreg_pL->fft = 1;
  cfg->coreg_pL->off_az = 0;
  cfg->coreg_pL->off_rng = 0;
  cfg->coreg_pL->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->coreg_pL->status, "new");
  
  cfg->doppler_per_patch->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->doppler_per_patch->status, "new");
  
  cfg->aisp_master->start_offset = 0;
  cfg->aisp_master->end_offset = 0;
  cfg->aisp_master->patches = 1;
  cfg->aisp_master->power = 1;
  cfg->aisp_master->power_img = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->aisp_master->power_img, "");
  cfg->aisp_master->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->aisp_master->status, "new");
  
  cfg->aisp_slave->start_offset = 0;
  cfg->aisp_slave->end_offset = 0;
  cfg->aisp_slave->patches = 1;
  cfg->aisp_slave->power = 1;
  cfg->aisp_slave->power_img = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->aisp_slave->power_img, "");
  cfg->aisp_slave->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->aisp_slave->status, "new");
  
  cfg->cpx_autofilter->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->cpx_autofilter->status, "new");
  
  cfg->coreg_slave->grid = 20;
  cfg->coreg_slave->fft = 1;
  cfg->coreg_slave->sinc = 0;
  cfg->coreg_slave->warp = 0;
  cfg->coreg_slave->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->coreg_slave->status, "new");
  
  cfg->igram_coh->igram = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->igram_coh->igram, "");
  cfg->igram_coh->coh = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->igram_coh->coh, "");
  cfg->igram_coh->min = 0.3;
  cfg->igram_coh->ml = 1;
  cfg->igram_coh->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->igram_coh->status, "new");
  
  cfg->offset_match->max = 1.0;
  cfg->offset_match->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->offset_match->status, "new");
  
  cfg->sim_phase->seeds = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->sim_phase->seeds, "");
  cfg->sim_phase->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->sim_phase->status, "new");
  
  cfg->dinsar->igram = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->dinsar->igram, "");
  cfg->dinsar->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->dinsar->status, "new");
  
  cfg->deramp_ml->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->deramp_ml->status, "new");
  
  cfg->unwrap->algorithm = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->unwrap->algorithm, "escher");
  cfg->unwrap->flattening = 1;
  cfg->unwrap->procs = 8;
  cfg->unwrap->tiles_azimuth = 0;
  cfg->unwrap->tiles_range = 0;
  cfg->unwrap->overlap_azimuth = 400;
  cfg->unwrap->overlap_range = 400;
  cfg->unwrap->filter = 1.6;
  cfg->unwrap->qc = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->unwrap->qc, "");
  cfg->unwrap->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->unwrap->status, "new");
  
  cfg->refine->iter = 0;
  cfg->refine->max = 15;
  cfg->refine->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->refine->status, "new");
  
  cfg->elevation->dem = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->elevation->dem, "");
  cfg->elevation->error = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->elevation->error, "");
  cfg->elevation->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->elevation->status, "new");
  
  cfg->ground_range->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->ground_range->status, "new");
  
  cfg->geocode->dem = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->dem, "");
  cfg->geocode->amp = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->amp, "");
  cfg->geocode->error = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->error, "");
  cfg->geocode->coh = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->coh, "");
  cfg->geocode->name = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->name, "utm");
  cfg->geocode->proj = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->proj, "");
  cfg->geocode->resample = (char *)MALLOC(sizeof(char)*255);
  strcpy(cfg->geocode->resample, "bilinear");
  cfg->geocode->pixel_spacing = 100;
  cfg->geocode->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->geocode->status, "new");
  
  cfg->export->format = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->export->format, "geotiff");
  cfg->export->status = (char *)MALLOC(sizeof(char)*25);
  strcpy(cfg->export->status, "new");

  fConfig = FOPEN(configFile, "r");
  i=0;
  while (fgets(line, 255, fConfig) != NULL) {
    if (i==0) strcpy(cfg->comment, line); 
    i++;
    
    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "default values", 14)==0) 
	cfg->general->def_val = read_str(line, "default values");
    }
  }
  FCLOSE(fConfig);
  
  if (strcmp(cfg->general->def_val, "")!=0) {
    if (!fileExists(cfg->general->def_val)) 
      check_return(1, "default values file does not exist");
    fDefaults = FOPEN(cfg->general->def_val, "r");
    while (fgets(line, 255, fDefaults) != NULL) {
      test = read_param(line);
      if (strncmp(test, "mode", 4)==0) 
	cfg->general->mode = read_str(line, "mode"); 
      if (strncmp(test, "reference dem", 13)==0) 
	cfg->general->dem = read_str(line, "reference dem"); 
      if (strncmp(test, "data type", 9)==0) 
	cfg->general->data_type = read_str(line, "data type");
      if (strncmp(test, "coregistration", 14)==0) 
	cfg->general->coreg = read_str(line, "coregistration");
      if (strncmp(test, "doppler", 7)==0) 
	cfg->general->doppler = read_str(line, "doppler");
      if (strncmp(test, "deskew", 6)==0) 
	cfg->general->deskew = read_int(line, "deskew");
      if (strncmp(test, "maximum offset", 14)==0) 
	cfg->general->max_off = read_int(line, "maximum offset");
      if (strncmp(test, "precise master", 14)==0) 
	cfg->ingest->prc_master = read_str(line, "precise master"); 
      if (strncmp(test, "precise slave", 13)==0) 
	cfg->ingest->prc_slave = read_str(line, "precise slave");
      if (strncmp(test, "minimum coherence", 17)==0) 
	cfg->igram_coh->min = read_double(line, "minimum coherence");
      if (strncmp(test, "phase unwrapping", 16)==0) 
	cfg->unwrap->algorithm = read_str(line, "phase unwrapping");
      if (strncmp(test, "tiles per degree", 16)==0) 
	cfg->unwrap->tiles_per_degree = read_int(line, "tiles per degree");
      if (strncmp(test, "tile overlap", 12)==0) {
	cfg->unwrap->overlap_azimuth = read_int(line, "tile overlap");
	cfg->unwrap->overlap_range = cfg->unwrap->overlap_azimuth;
      }
      if (strncmp(test, "projection name", 15)==0) 
	cfg->geocode->name = read_str(line, "projection name");
      if (strncmp(test, "projection file", 15)==0) 
	cfg->geocode->proj = read_str(line, "projection file");
      if (strncmp(test, "resampling method", 17)==0)
	cfg->geocode->resample = read_str(line, "resampling method");
      if (strncmp(test, "pixel spacing", 13)==0) 
	cfg->geocode->pixel_spacing = read_int(line, "pixel spacing");
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
      if (strncmp(test, "mode", 4)==0) cfg->general->mode = read_str(line, "mode"); 
      if (strncmp(test, "reference dem", 13)==0) 
	cfg->general->dem = read_str(line, "reference dem"); 
      if (strncmp(test, "base name", 9)==0) 
	cfg->general->base = read_str(line, "base name");
      if (strncmp(test, "data type", 9)==0) 
	cfg->general->data_type = read_str(line, "data_type");
      if (strncmp(test, "coregistration", 14)==0) 
	cfg->general->coreg = read_str(line, "coregistration");
      if (strncmp(test, "doppler", 7)==0) 
	cfg->general->doppler = read_str(line, "doppler");
      if (strncmp(test, "deskew", 6)==0) 
	cfg->general->deskew = read_int(line, "deskew");
      if (strncmp(test, "lat begin", 9)==0) 
	cfg->general->lat_begin = read_double(line, "lat_begin"); 
      if (strncmp(test, "lat end", 7)==0) 
	cfg->general->lat_end = read_double(line, "lat_end"); 
      if (strncmp(test, "coregistration", 14)==0) 
	cfg->general->coreg = read_str(line, "coregistration");
      if (strncmp(test, "maximum offset", 14)==0) 
	cfg->general->max_off = read_int(line, "maximum offset");
      if (strncmp(test, "correlation mask", 16)==0) 
	cfg->general->mflag = read_int(line, "correlation mask");
      if (strncmp(test, "mask file", 9)==0) 
	cfg->general->mask = read_str(line, "mask file");
      if (strncmp(test, "default values", 14)==0) 
	cfg->general->def_val = read_str(line, "default values");
      if (strncmp(test, "test mode", 9)==0) 
	cfg->general->test = read_int(line, "test mode");
      if (strncmp(test, "status", 6)==0) 
	cfg->general->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
    if (strcmp(params, "master image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) cfg->master->path = read_str(line, "path");
      if (strncmp(test, "data file", 9)==0) 
	cfg->master->data = read_str(line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	cfg->master->meta = read_str(line, "metadata file");
    }
    
    if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
    if (strcmp(params, "slave image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) cfg->slave->path = read_str(line, "path"); 
      if (strncmp(test, "data file", 9)==0) 
	cfg->slave->data = read_str(line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	cfg->slave->meta = read_str(line, "metadata file");
    }
    
  }
  FCLOSE(fConfig);
  
  return cfg;
}

dem_config *read_config(char *configFile, int cFlag)
{
  FILE *fConfig;
  dem_config *cfg=NULL;
  char line[255], params[25];
  char *test=(char *)MALLOC(sizeof(char)*255);
  
  cfg = init_fill_config(configFile);
  if (cfg == NULL) check_return(1, "creating configuration structure");
  if (cFlag) return cfg;        
  
  fConfig = FOPEN(configFile, "r");
  while (fgets(line, 255, fConfig) != NULL) {
    
    if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
    if (strcmp(params, "general")==0) {
      test = read_param(line);
      if (strncmp(test, "mode", 4)==0) cfg->general->mode = read_str(line, "mode"); 
      if (strncmp(test, "reference dem", 13)==0) 
	cfg->general->dem = read_str(line, "reference dem"); 
      if (strncmp(test, "base name", 9)==0) 
	cfg->general->base = read_str(line, "base name");
      if (strncmp(test, "data type", 9)==0) 
	cfg->general->data_type = read_str(line, "data type");
      if (strncmp(test, "deskew", 6)==0) 
	cfg->general->deskew = read_int(line, "deskew");
      if (strncmp(test, "doppler", 7)==0) 
	cfg->general->doppler = read_str(line, "doppler");
      if (strncmp(test, "lat begin", 9)==0) 
	cfg->general->lat_begin = read_double(line, "lat_begin"); 
      if (strncmp(test, "lat end", 7)==0) 
	cfg->general->lat_end = read_double(line, "lat_end"); 
      if (strncmp(test, "coregistration", 14)==0) 
	cfg->general->coreg = read_str(line, "coregistration");
      if (strncmp(test, "maximum offset", 14)==0) 
	cfg->general->max_off = read_int(line, "maximum offset");
      if (strncmp(test, "correlation mask", 16)==0) 
	cfg->general->mflag = read_int(line, "correlation mask");
      if (strncmp(test, "mask file", 9)==0) 
	cfg->general->mask = read_str(line, "mask file");
      if (strncmp(test, "default values", 14)==0) 
	cfg->general->def_val = read_str(line, "default values");
      if (strncmp(test, "test mode", 9)==0) 
	cfg->general->test = read_int(line, "test mode");
      if (strncmp(test, "status", 6)==0) 
	cfg->general->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
    if (strcmp(params, "master image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) cfg->master->path = read_str(line, "path");
      if (strncmp(test, "data file", 9)==0) 
	cfg->master->data = read_str(line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	cfg->master->meta = read_str(line, "metadata file");
    }
    
    if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
    if (strcmp(params, "slave image")==0) {
      test = read_param(line);
      if (strncmp(test, "path", 4)==0) cfg->slave->path = read_str(line, "path"); 
      if (strncmp(test, "data file", 9)==0) 
	cfg->slave->data = read_str(line, "data file");
      if (strncmp(test, "metadata file", 13)==0) 
	cfg->slave->meta = read_str(line, "metadata file");
    }
    
    if (strncmp(line, "[Ingest]", 8)==0) strcpy(params, "ingest");
    if (strcmp(params, "ingest")==0) {
      test = read_param(line);
      if (strncmp(test, "precise master", 14)==0) 
	cfg->ingest->prc_master = read_str(line, "precise master"); 
      if (strncmp(test, "precise slave", 13)==0) 
	cfg->ingest->prc_slave = read_str(line, "precise slave");
      if (strncmp(test, "precise orbits", 14)==0) 
	cfg->ingest->prcflag = read_int(line, "precise orbits");
      if (strncmp(test, "status", 6)==0) 
	cfg->ingest->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Doppler]", 9)==0) strcpy(params, "doppler");
    if (strcmp(params, "doppler")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	cfg->doppler->status = read_str(line, "status");
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
	cfg->coreg_p1->status = read_str(line, "status"); 
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
	cfg->coreg_pL->status = read_str(line, "status"); 
    }	  
    
    if (strncmp(line, "[doppler_per_patch]", 19)==0) 
      strcpy(params, "doppler_per_patch");
    if (strcmp(params, "doppler_per_patch")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	cfg->doppler_per_patch->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[aisp - Master image]", 21)==0) strcpy(params, "aisp_master");
    if (strcmp(params, "aisp_master")==0) {
      test = read_param(line);
      if (strncmp(test, "start offset", 12)==0) 
	cfg->aisp_master->start_offset = read_int(line, "start offset"); 
      if (strncmp(test, "end offset", 10)==0) 
	cfg->aisp_master->end_offset = read_int(line, "end offset"); 
      if (strncmp(test, "patches", 7)==0) 
	cfg->aisp_master->patches = read_int(line, "patches");
      if (strncmp(test, "power flag", 10)==0) 
	cfg->aisp_master->power = read_int(line, "power flag"); 
      if (strncmp(test, "power image", 11)==0) 
	cfg->aisp_master->power_img = read_str(line, "power image"); 
      if (strncmp(test, "status", 6)==0) 
	cfg->aisp_master->status = read_str(line, "status"); 
    }	  
    
    if (strncmp(line, "[aisp - Slave image]", 20)==0) strcpy(params, "aisp_slave");
    if (strcmp(params, "aisp_slave")==0) {
      test = read_param(line);
      if (strncmp(test, "start offset", 6)==0) 
	cfg->aisp_slave->start_offset = read_int(line, "start offset"); 
      if (strncmp(test, "end offset", 10)==0) 
	cfg->aisp_slave->end_offset = read_int(line, "end offset"); 
      if (strncmp(test, "patches", 7)==0) 
	cfg->aisp_slave->patches = read_int(line, "patches"); 
      if (strncmp(test, "power flag", 10)==0) 
	cfg->aisp_slave->power = read_int(line, "power flag"); 
      if (strncmp(test, "power image", 11)==0) 
	cfg->aisp_slave->power_img = read_str(line, "power image"); 
      if (strncmp(test, "status", 6)==0) 
	cfg->aisp_slave->status = read_str(line, "status"); 
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
	cfg->coreg_slave->status = read_str(line, "status"); 
    }	  
    
    if (strncmp(line, "[Interferogram/coherence]", 25)==0) 
      strcpy(params, "igram_coh");
    if (strcmp(params, "igram_coh")==0) {
      test = read_param(line);
      if (strncmp(test, "interferogram", 13)==0) 
	cfg->igram_coh->igram = read_str(line, "interferogram"); 
      if (strncmp(test, "coherence image", 15)==0) 
	cfg->igram_coh->coh = read_str(line, "coherence image");
      if (strncmp(test, "minimum coherence", 17)==0) 
	cfg->igram_coh->min = read_double(line, "minimum coherence");
      if (strncmp(test, "multilook", 9)==0) 
	cfg->igram_coh->ml = read_int(line, "multilook");
      if (strncmp(test, "status", 6)==0) 
	cfg->igram_coh->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Offset matching]", 17)==0) strcpy(params, "offset_match");
    if (strcmp(params, "offset_match")==0) {
      test = read_param(line);
      if (strncmp(test, "max", 3)==0) 
	cfg->offset_match->max = read_double(line, "max");
      if (strncmp(test, "status", 6)==0) 
	cfg->offset_match->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Simulated phase]", 17)==0) strcpy(params, "simulated phase");
    if (strcmp(params, "simulated phase")==0) {
      test = read_param(line);
      if (strncmp(test, "seeds", 5)==0) 
	cfg->sim_phase->seeds = read_str(line, "seeds");
      if (strncmp(test, "status", 6)==0) 
	cfg->sim_phase->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Differential interferogram]", 28)==0) 
      strcpy(params, "differential interferogram");
    if (strcmp(params, "differential interferogram")==0) {
      test = read_param(line);
      if (strncmp(test, "igram", 5)==0) cfg->dinsar->igram = read_str(line, "igram");
      if (strncmp(test, "status", 6)==0) 
	cfg->dinsar->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Deramp/multilook]", 18)==0) strcpy(params, "deramp_ml");
    if (strcmp(params, "deramp_ml")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	cfg->deramp_ml->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Phase unwrapping]", 18)==0) strcpy(params, "unwrap");
    if (strcmp(params, "unwrap")==0) {
      test = read_param(line);
      if (strncmp(test, "algorithm", 9)==0) 
	cfg->unwrap->algorithm = read_str(line, "algorithm");
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
	cfg->unwrap->qc = read_str(line, "quality control");
      if (strncmp(test, "status", 6)==0) 
	cfg->unwrap->status = read_str(line, "status");
    }

    if (strncmp(line, "[Baseline refinement]", 21)==0) strcpy(params, "refine");
    if (strcmp(params, "refine")==0) {
      test = read_param(line);
      if (strncmp(test, "iter", 4)==0) 
	cfg->refine->iter = read_int(line, "iterations");
      if (strncmp(test, "max", 3)==0) 
	cfg->refine->max = read_int(line, "max iterations");
      if (strncmp(test, "status", 6)==0) 
	cfg->refine->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Elevation]", 11)==0) strcpy(params, "elev");
    if (strcmp(params, "elev")==0) {
      test = read_param(line);
      if (strncmp(test, "dem", 3)==0) cfg->elevation->dem = read_str(line, "dem");
      if (strncmp(test, "error map", 9)==0) 
	cfg->elevation->error = read_str(line, "error map");
      if (strncmp(test, "status", 6)==0) 
	cfg->elevation->status = read_str(line, "status");
    }
    
    if (strncmp(line, "[Ground range DEM]", 18)==0) strcpy(params, "final_dem");
    if (strcmp(params, "final_dem")==0) {
      test = read_param(line);
      if (strncmp(test, "status", 6)==0) 
	cfg->ground_range->status = read_str(line, "status");   
    }
    
    if (strncmp(line, "[Geocoding]", 11)==0) strcpy(params, "geocoding");
    if (strcmp(params, "geocoding")==0) {
      test = read_param(line);
      if (strncmp(test, "dem", 3)==0) cfg->geocode->dem = read_str(line, "dem");
      if (strncmp(test, "error map", 9)==0) 
	cfg->geocode->error = read_str(line, "error map");
      if (strncmp(test, "amplitude", 9)==0) 
	cfg->geocode->amp = read_str(line, "amplitude");
      if (strncmp(test, "coherence", 9)==0) 
	cfg->geocode->coh = read_str(line, "coherence");
      if (strncmp(test, "projection name", 15)==0) 
	cfg->geocode->name = read_str(line, "projection name");
      if (strncmp(test, "projection file", 15)==0) 
	cfg->geocode->proj = read_str(line, "projection file");
      if (strncmp(test, "resampling method", 17)==0)
	cfg->geocode->resample = read_str(line, "resampling method");
      if (strncmp(test, "pixel spacing", 13)==0) 
	cfg->geocode->pixel_spacing = read_double(line, "pixel spacing");
      if (strncmp(test, "status", 6)==0) 
	cfg->geocode->status = read_str(line, "status");   
    }
    
    if (strncmp(line, "[Export]", 8)==0) strcpy(params, "export");
    if (strcmp(params, "export")==0) {
      test = read_param(line);
      if (strncmp(test, "format", 6)==0)
	cfg->export->format = read_str(line, "format");
      if (strncmp(test, "status", 6)==0)
	cfg->export->status = read_str(line, "status");
    }
  }
  FCLOSE(fConfig);
  
  return cfg;	
  
}

int write_config(char *configFile, dem_config *cfg)
{
  FILE *fConfig;
  
  if (cfg == NULL) check_return(1, "not configuration structure to write");
  fConfig = FOPEN(configFile, "w");
  
  fprintf(fConfig, "%s\n", cfg->comment);
  fprintf(fConfig, "[General]\n");
  fprintf(fConfig, "mode = %s\n", cfg->general->mode);
  fprintf(fConfig, "reference dem = %s\n", cfg->general->dem);
  fprintf(fConfig, "base name = %s\n", cfg->general->base);
  fprintf(fConfig, "data type = %s\n", cfg->general->data_type);
  fprintf(fConfig, "deskew = %d\n", cfg->general->deskew);
  fprintf(fConfig, "doppler = %s\n", cfg->general->doppler);
  fprintf(fConfig, "lat begin = %.3f\n", cfg->general->lat_begin);
  fprintf(fConfig, "lat end = %.3f\n", cfg->general->lat_end);
  fprintf(fConfig, "coregistration = %s\n", cfg->general->coreg);
  fprintf(fConfig, "maximum offset = %d\n", cfg->general->max_off);
  /* Leave the mask related stuff out until it is actually implemented
  fprintf(fConfig, "correlation mask = %d\n", cfg->general->mflag);
  fprintf(fConfig, "mask file = %s\n", cfg->general->mask);
  */
  fprintf(fConfig, "default values = %s\n", cfg->general->def_val);
  fprintf(fConfig, "test mode = %d\n", cfg->general->test);
  fprintf(fConfig, "status = %s\n\n", cfg->general->status);
  fprintf(fConfig, "[Master image]\n");
  fprintf(fConfig, "path = %s\n", cfg->master->path);
  fprintf(fConfig, "data file = %s\n", cfg->master->data);
  fprintf(fConfig, "metadata file = %s\n\n", cfg->master->meta);
  fprintf(fConfig, "[Slave image]\n");
  fprintf(fConfig, "path = %s\n", cfg->slave->path);
  fprintf(fConfig, "data file = %s\n", cfg->slave->data);
  fprintf(fConfig, "metadata file = %s\n\n", cfg->slave->meta);
  if (strncmp(cfg->general->data_type, "STF", 3)==0) {
    fprintf(fConfig, "[Ingest]\n");
    fprintf(fConfig, "precise master = %s\n", cfg->ingest->prc_master);
    fprintf(fConfig, "precise slave = %s\n", cfg->ingest->prc_slave);
    fprintf(fConfig, "precise orbits = %d\n", cfg->ingest->prcflag);
    fprintf(fConfig, "status = %s\n\n", cfg->ingest->status);
    if (strncmp(cfg->general->doppler, "average", 7)==0) {
      fprintf(fConfig, "[Doppler]\n");
      fprintf(fConfig, "status = %s\n\n", cfg->doppler->status);
    }
    fprintf(fConfig, "[Coregister first patch]\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_p1->patches);
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_p1->start_master);
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_p1->start_slave);
    fprintf(fConfig, "grid = %ld\n", cfg->coreg_p1->grid);
    fprintf(fConfig, "fft = %d\n", cfg->coreg_p1->fft);
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_p1->off_az);
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_p1->off_rng);
    fprintf(fConfig, "status = %s\n\n", cfg->coreg_p1->status);
    fprintf(fConfig, "[Coregister last patch]\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_pL->patches);
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
    fprintf(fConfig, "grid = %ld\n", cfg->coreg_pL->grid);
    fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_pL->off_az);
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_pL->off_rng);
    fprintf(fConfig, "status = %s\n\n", cfg->coreg_pL->status);
    if (strncmp(cfg->general->doppler, "updated", 7)==0) {
      fprintf(fConfig, "[doppler_per_patch]\n");
      fprintf(fConfig, "status = %s\n\n", cfg->doppler_per_patch->status);
    }
    fprintf(fConfig, "[aisp - Master image]\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->aisp_master->start_offset);
    fprintf(fConfig, "end offset = %ld\n", cfg->aisp_master->end_offset);
    fprintf(fConfig, "patches = %d\n", cfg->aisp_master->patches);
    fprintf(fConfig, "power flag = %d\n", cfg->aisp_master->power);
    fprintf(fConfig, "power image = %s\n", cfg->aisp_master->power_img);
    fprintf(fConfig, "status = %s\n\n", cfg->aisp_master->status);
    fprintf(fConfig, "[aisp - Slave image]\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->aisp_slave->start_offset);
    fprintf(fConfig, "end offset = %ld\n", cfg->aisp_slave->end_offset);
    fprintf(fConfig, "patches = %d\n", cfg->aisp_slave->patches);
    fprintf(fConfig, "power flag = %d\n", cfg->aisp_slave->power);
    fprintf(fConfig, "power image = %s\n", cfg->aisp_slave->power_img);
    fprintf(fConfig, "status = %s\n\n", cfg->aisp_slave->status);
  }
  if (strncmp(cfg->general->data_type, "RAW", 3)==0) {
    fprintf(fConfig, "[Ingest]\n");
    fprintf(fConfig, "status = %s\n\n", cfg->ingest->status);
    if (strncmp(cfg->general->doppler, "average", 7)==0) {
      fprintf(fConfig, "[Doppler]\n");
      fprintf(fConfig, "status = %s\n\n", cfg->doppler->status);
    }
    fprintf(fConfig, "[Coregister first patch]\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_p1->patches);
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_p1->start_master);
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_p1->start_slave);
    fprintf(fConfig, "grid = %ld\n", cfg->coreg_p1->grid);
    fprintf(fConfig, "fft = %d\n", cfg->coreg_p1->fft);
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_p1->off_az);
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_p1->off_rng);
    fprintf(fConfig, "status = %s\n\n", cfg->coreg_p1->status);
    fprintf(fConfig, "[Coregister last patch]\n");
    fprintf(fConfig, "patches = %d\n", cfg->coreg_pL->patches);
    fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
    fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
    fprintf(fConfig, "grid = %ld\n", cfg->coreg_pL->grid);
    fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
    fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_pL->off_az);
    fprintf(fConfig, "offset range = %d\n", cfg->coreg_pL->off_rng);
    fprintf(fConfig, "status = %s\n\n", cfg->coreg_pL->status);
    if (strncmp(cfg->general->doppler, "updated", 7)==0) {
      fprintf(fConfig, "[doppler_per_patch]\n");
      fprintf(fConfig, "status = %s\n\n", cfg->doppler_per_patch->status);
    }
    fprintf(fConfig, "[aisp - Master image]\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->aisp_master->start_offset);
    fprintf(fConfig, "end offset = %ld\n", cfg->aisp_master->end_offset);
    fprintf(fConfig, "patches = %d\n", cfg->aisp_master->patches);
    fprintf(fConfig, "power flag = %d\n", cfg->aisp_master->power);
    fprintf(fConfig, "power image = %s\n", cfg->aisp_master->power_img);
    fprintf(fConfig, "status = %s\n\n", cfg->aisp_master->status);
    fprintf(fConfig, "[aisp - Slave image]\n");
    fprintf(fConfig, "start offset = %ld\n", cfg->aisp_slave->start_offset);
    fprintf(fConfig, "end offset = %ld\n", cfg->aisp_slave->end_offset);
    fprintf(fConfig, "patches = %d\n", cfg->aisp_slave->patches);
    fprintf(fConfig, "power flag = %d\n", cfg->aisp_slave->power);
    fprintf(fConfig, "power image = %s\n", cfg->aisp_slave->power_img);
    fprintf(fConfig, "status = %s\n\n", cfg->aisp_slave->status);
  }
  if (strncmp(cfg->general->data_type, "SLC", 3)==0) {
    fprintf(fConfig, "[Ingest]\n");
    fprintf(fConfig, "status = %s\n\n", cfg->ingest->status);
    fprintf(fConfig, "[Coregister slave]\n");
    fprintf(fConfig, "grid = %ld\n", cfg->coreg_slave->grid);
    fprintf(fConfig, "fft = %d\n", cfg->coreg_slave->fft);
    fprintf(fConfig, "sinc = %d\n", cfg->coreg_slave->sinc);
    fprintf(fConfig, "warp = %d\n", cfg->coreg_slave->warp);
    fprintf(fConfig, "status = %s\n\n", cfg->coreg_slave->status);
  }
  fprintf(fConfig, "[Interferogram/coherence]\n");
  fprintf(fConfig, "interferogram = %s\n", cfg->igram_coh->igram);
  fprintf(fConfig, "coherence image = %s\n", cfg->igram_coh->coh);
  fprintf(fConfig, "minimum coherence = %.1f\n", cfg->igram_coh->min);
  fprintf(fConfig, "multilook = %d\n", cfg->igram_coh->ml);
  fprintf(fConfig, "status = %s\n\n", cfg->igram_coh->status);
  fprintf(fConfig, "[Offset matching]\n");
  fprintf(fConfig, "max = %.1lf\n", cfg->offset_match->max);
  fprintf(fConfig, "status = %s\n\n", cfg->offset_match->status);
  fprintf(fConfig, "[Simulated phase]\n");
  fprintf(fConfig, "seeds = %s\n", cfg->sim_phase->seeds);
  fprintf(fConfig, "status = %s\n\n", cfg->sim_phase->status);
  if (strncmp(cfg->general->mode, "DINSAR", 6)==0) {
    fprintf(fConfig, "[Differential interferogram]\n");
    fprintf(fConfig, "igram = %s\n", cfg->dinsar->igram);
    fprintf(fConfig, "status = %s\n\n", cfg->dinsar->status);
  }
  if (strncmp(cfg->general->mode, "DEM", 3)==0) {
    fprintf(fConfig, "[Deramp/multilook]\n");
    fprintf(fConfig, "status = %s\n\n", cfg->deramp_ml->status);
    fprintf(fConfig, "[Phase unwrapping]\n");
    fprintf(fConfig, "algorithm = %s\n", cfg->unwrap->algorithm);
    fprintf(fConfig, "flattening = %d\n", cfg->unwrap->flattening);
    fprintf(fConfig, "processors = %d\n", cfg->unwrap->procs);
    fprintf(fConfig, "tiles azimuth = %d\n", cfg->unwrap->tiles_azimuth);
    fprintf(fConfig, "tiles range = %d\n", cfg->unwrap->tiles_range);
    fprintf(fConfig, "tiles per degree = %d\n", cfg->unwrap->tiles_per_degree);
    fprintf(fConfig, "overlap azimuth = %d\n", cfg->unwrap->overlap_azimuth);
    fprintf(fConfig, "overlap range = %d\n", cfg->unwrap->overlap_range);
    fprintf(fConfig, "filter = %.1f\n", cfg->unwrap->filter);
    fprintf(fConfig, "quality control = %s\n", cfg->unwrap->qc);
    fprintf(fConfig, "status = %s\n\n", cfg->unwrap->status);
    fprintf(fConfig, "[Baseline refinement]\n");
    fprintf(fConfig, "iterations = %d\n", cfg->refine->iter);
    fprintf(fConfig, "max iterations = %d\n", cfg->refine->max);
    fprintf(fConfig, "status = %s\n\n", cfg->refine->status);
    fprintf(fConfig, "[Elevation]\n");
    fprintf(fConfig, "dem = %s\n", cfg->elevation->dem);
    fprintf(fConfig, "error map = %s\n", cfg->elevation->error);
    fprintf(fConfig, "status = %s\n\n", cfg->elevation->status);
    fprintf(fConfig, "[Ground range DEM]\n");
    fprintf(fConfig, "status = %s\n\n", cfg->ground_range->status);
    fprintf(fConfig, "[Geocoding]\n");
    fprintf(fConfig, "dem = %s\n", cfg->geocode->dem);
    fprintf(fConfig, "error map = %s\n", cfg->geocode->error);
    fprintf(fConfig, "amplitude = %s\n", cfg->geocode->amp);
    fprintf(fConfig, "coherence = %s\n", cfg->geocode->coh);
    fprintf(fConfig, "projection name = %s\n", cfg->geocode->name);
    fprintf(fConfig, "projection file = %s\n", cfg->geocode->proj);
    fprintf(fConfig, "resampling method = %s\n", cfg->geocode->resample);
    fprintf(fConfig, "pixel spacing = %.1lf\n", cfg->geocode->pixel_spacing);
    fprintf(fConfig, "status = %s\n\n", cfg->geocode->status);
    fprintf(fConfig, "[Export]\n");
    fprintf(fConfig, "format = %s\n", cfg->export->format);
    fprintf(fConfig, "status = %s\n\n", cfg->export->status);
  }
  
  FCLOSE(fConfig);
  
  return(0);
}

