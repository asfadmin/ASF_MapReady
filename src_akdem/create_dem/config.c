#include "asf.h"
#include "create_dem.h"

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

dem_config *init_config(char *configFile)
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
	cfg->lz2raw = newStruct(s_lz2raw);
	cfg->ceos2raw = newStruct(s_status);
	cfg->trim_slc = newStruct(s_trim_slc);
	cfg->avg_in_dop = newStruct(s_status);
	cfg->aisp_master = newStruct(s_aisp);
	cfg->coreg_p1 = newStruct(s_coreg);
	cfg->coreg_pL = newStruct(s_coreg);
	cfg->aisp_slave = newStruct(s_aisp);
	cfg->cpx_autofilter = newStruct(s_status);
	cfg->coreg_slave = newStruct(s_coreg);
	cfg->igram_coh = newStruct(s_igram_coh);
	cfg->offset_match = newStruct(s_offset);
	cfg->sim_phase = newStruct(s_sim_phase);
	cfg->deramp_ml = newStruct(s_status);
	cfg->unwrap = newStruct(s_unwrap);
	cfg->refine = newStruct(s_refine);
	cfg->elevation = newStruct(s_elev);
	cfg->ground_range = newStruct(s_status);
	cfg->geocode = newStruct(s_geocode);

	/* initialize structure */
	strcpy(cfg->comment, "AKDEM project: configuration file");

	cfg->general->dem = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->general->dem, "");
	cfg->general->def_val = (char *)MALLOC(sizeof(char)*255);
	cfg->general->def_val = "";
	cfg->general->base = (char *)MALLOC(sizeof(char)*255);
	cfg->general->base = "";
	cfg->general->log = 1;
	cfg->general->quiet = 1;
	cfg->general->procs = 1;
	cfg->general->data_type = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->general->data_type, "");
	cfg->general->lat_begin = -99;
	cfg->general->lat_end = 99;
	cfg->general->coreg = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->general->coreg, "AUTOMATIC");
	cfg->general->max_off = 3;
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

	cfg->lz2raw->prc_master = (char *)MALLOC(sizeof(char)*255);
	cfg->lz2raw->prc_master = "";
	cfg->lz2raw->prc_slave = (char *)MALLOC(sizeof(char)*255);
	cfg->lz2raw->prc_slave = "";
	cfg->lz2raw->prcFlag = 1;
	cfg->lz2raw->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->lz2raw->status, "new");

	cfg->ceos2raw->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->ceos2raw->status, "new");

	cfg->trim_slc->line = 0;
	cfg->trim_slc->sample = 0;
	cfg->trim_slc->length = -99;
	cfg->trim_slc->width = -99;
	cfg->trim_slc->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->trim_slc->status, "new");

	cfg->avg_in_dop->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->avg_in_dop->status, "new");

	cfg->aisp_master->start_offset = 0;
	cfg->aisp_master->end_offset = 0;
	cfg->aisp_master->patches = 1;
	cfg->aisp_master->power = 1;
	cfg->aisp_master->power_img = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->aisp_master->power_img, "");
	cfg->aisp_master->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->aisp_master->status, "new");

	cfg->coreg_p1->start_master = 0;
	cfg->coreg_p1->start_slave = 0;
	cfg->coreg_p1->grid = 20;
	cfg->coreg_p1->fft = 1;
	cfg->coreg_p1->off_az = 0;
	cfg->coreg_p1->off_rng = 0;
	cfg->coreg_p1->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->coreg_p1->status, "new");

	cfg->coreg_pL->start_master = 0;
	cfg->coreg_pL->start_slave = 0;
	cfg->coreg_pL->grid = 20;
	cfg->coreg_pL->fft = 1;
	cfg->coreg_pL->off_az = 0;
	cfg->coreg_pL->off_rng = 0;
	cfg->coreg_pL->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->coreg_pL->status, "new");

	cfg->aisp_slave->start_offset = 0;
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

	cfg->deramp_ml->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->deramp_ml->status, "new");

	cfg->unwrap->algorithm = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->unwrap->algorithm, "");
	cfg->unwrap->flattening = 0;
	cfg->unwrap->tiles_azimuth = 0;
	cfg->unwrap->tiles_range = 0;
	cfg->unwrap->overlap_azimuth = 400;
	cfg->unwrap->overlap_range = 400;
	cfg->unwrap->filter = 1.6;
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
	cfg->geocode->proj = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->geocode->proj, "");
	cfg->geocode->key = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->geocode->key, "");
	cfg->geocode->pix_spacing = 100;
	cfg->geocode->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->geocode->status, "new");

	fConfig = FOPEN(configFile, "r");
	i=0;
        while (fgets(line, 255, fConfig) != NULL) {
	  if (i==0) strcpy(cfg->comment, line); 
	  i++;

	  if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
	  if (strcmp(params, "general")==0) {
	    test = read_param(line);
	    if (strncmp(test, "default values", 14)==0) cfg->general->def_val = read_str(line, "default values");
	  }
	}
        FCLOSE(fConfig);

	if (strcmp(cfg->general->def_val, "")!=0) {
	  if (!fileExists(cfg->general->def_val)) check_return(1, "default values file does not exist");
	  fDefaults = FOPEN(cfg->general->def_val, "r");
	  while (fgets(line, 255, fDefaults) != NULL) {
	    test = read_param(line);
	    if (strncmp(test, "reference dem", 13)==0) cfg->general->dem = read_str(line, "reference dem"); 
	    if (strncmp(test, "log file", 8)==0) cfg->general->log = read_int(line, "log file");
	    if (strncmp(test, "quiet", 5)==0) cfg->general->quiet = read_int(line, "quiet");
	    if (strncmp(test, "processors", 10)==0) cfg->general->procs = read_int(line, "processors");
            if (strncmp(test, "data type", 9)==0) cfg->general->data_type = read_str(line, "data type");
	    if (strncmp(test, "coregistration", 14)==0) cfg->general->coreg = read_str(line, "coregistration");
	    if (strncmp(test, "maximum offset", 14)==0) cfg->general->max_off = read_int(line, "maximum offset");
	    if (strncmp(test, "precise master", 14)==0) cfg->lz2raw->prc_master = read_str(line, "precise master"); 
	    if (strncmp(test, "precise slave", 13)==0) cfg->lz2raw->prc_slave = read_str(line, "precise slave");
	    if (strncmp(test, "minimum coherence", 17)==0) cfg->igram_coh->min = read_double(line, "minimum coherence");
	    if (strncmp(test, "phase unwrapping", 16)==0) cfg->unwrap->algorithm = read_str(line, "phase unwrapping");
	    if (strncmp(test, "tiles per degree", 16)==0) cfg->unwrap->tiles_per_degree = read_int(line, "tiles per degree");
	    if (strncmp(test, "tile overlap", 12)==0) {
	      cfg->unwrap->overlap_azimuth = read_int(line, "tile overlap");
	      cfg->unwrap->overlap_range = cfg->unwrap->overlap_azimuth;
	    }
            if (strncmp(test, "projection file", 15)==0) cfg->geocode->proj = read_str(line, "projection file");
            if (strncmp(test, "projection key", 14)==0) cfg->geocode->key = read_str(line, "projection key");
            if (strncmp(test, "pixel spacing", 13)==0) cfg->geocode->pix_spacing = read_int(line, "pixel spacing");
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
	    if (strncmp(test, "reference dem", 13)==0) cfg->general->dem = read_str(line, "reference dem"); 
	    if (strncmp(test, "base name", 9)==0) cfg->general->base = read_str(line, "base name");
	    if (strncmp(test, "log file", 8)==0) cfg->general->log = read_int(line, "log file");
	    if (strncmp(test, "quiet", 5)==0) cfg->general->quiet = read_int(line, "quiet");
	    if (strncmp(test, "processors", 10)==0) cfg->general->procs = read_int(line, "processors");
	    if (strncmp(test, "data type", 9)==0) cfg->general->data_type = read_str(line, "data_type");
	    if (strncmp(test, "lat begin", 9)==0) cfg->general->lat_begin = read_double(line, "lat_begin"); 
	    if (strncmp(test, "lat end", 7)==0) cfg->general->lat_end = read_double(line, "lat_end"); 
	    if (strncmp(test, "coregistration", 14)==0) cfg->general->coreg = read_str(line, "coregistration");
	    if (strncmp(test, "maximum offset", 14)==0) cfg->general->max_off = read_int(line, "maximum offset");
	    if (strncmp(test, "default values", 14)==0) cfg->general->def_val = read_str(line, "default values");
	    if (strncmp(test, "status", 6)==0) cfg->general->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
	  if (strcmp(params, "master image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->master->path = read_str(line, "path");
	    if (strncmp(test, "data file", 9)==0) cfg->master->data = read_str(line, "data file");
	    if (strncmp(test, "metadata file", 13)==0) cfg->master->meta = read_str(line, "metadata file");
	  }

	  if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
	  if (strcmp(params, "slave image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->slave->path = read_str(line, "path"); 
	    if (strncmp(test, "data file", 9)==0) cfg->slave->data = read_str(line, "data file");
	    if (strncmp(test, "metadata file", 13)==0) cfg->slave->meta = read_str(line, "metadata file");
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

	cfg = init_config(configFile);
	if (cfg == NULL) check_return(1, "creating configuration structure");
	if (cFlag) return cfg;        

	fConfig = FOPEN(configFile, "r");
        while (fgets(line, 255, fConfig) != NULL) {

	  if (strncmp(line, "[General]", 9)==0) strcpy(params, "general");
	  if (strcmp(params, "general")==0) {
	    test = read_param(line);
	    if (strncmp(test, "reference dem", 13)==0) cfg->general->dem = read_str(line, "reference dem"); 
	    if (strncmp(test, "base name", 9)==0) cfg->general->base = read_str(line, "base name");
	    if (strncmp(test, "log file", 8)==0) cfg->general->log = read_int(line, "log file");
	    if (strncmp(test, "quiet", 5)==0) cfg->general->quiet = read_int(line, "quiet");
	    if (strncmp(test, "processors", 10)==0) cfg->general->procs = read_int(line, "processors");
	    if (strncmp(test, "data type", 9)==0) cfg->general->data_type = read_str(line, "data type");
	    if (strncmp(test, "lat begin", 9)==0) cfg->general->lat_begin = read_double(line, "lat_begin"); 
	    if (strncmp(test, "lat end", 7)==0) cfg->general->lat_end = read_double(line, "lat_end"); 
	    if (strncmp(test, "coregistration", 14)==0) cfg->general->coreg = read_str(line, "coregistration");
	    if (strncmp(test, "maximum offset", 14)==0) cfg->general->max_off = read_int(line, "maximum offset");
	    if (strncmp(test, "default values", 14)==0) cfg->general->def_val = read_str(line, "default values");
	    if (strncmp(test, "status", 6)==0) cfg->general->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
	  if (strcmp(params, "master image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->master->path = read_str(line, "path");
	    if (strncmp(test, "data file", 9)==0) cfg->master->data = read_str(line, "data file");
	    if (strncmp(test, "metadata file", 13)==0) cfg->master->meta = read_str(line, "metadata file");
	  }

	  if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
	  if (strcmp(params, "slave image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->slave->path = read_str(line, "path"); 
	    if (strncmp(test, "data file", 9)==0) cfg->slave->data = read_str(line, "data file");
	    if (strncmp(test, "metadata file", 13)==0) cfg->slave->meta = read_str(line, "metadata file");
	  }

	  if (strncmp(line, "[lz2raw_flywheel]", 17)==0) strcpy(params, "lz2raw_flywheel");
	  if (strcmp(params, "lz2raw_flywheel")==0) {
	    test = read_param(line);
	    if (strncmp(test, "precise master", 14)==0) cfg->lz2raw->prc_master = read_str(line, "precise master"); 
	    if (strncmp(test, "precise slave", 13)==0) cfg->lz2raw->prc_slave = read_str(line, "precise slave");
	    if (strncmp(test, "precise orbits", 14)==0) cfg->lz2raw->prcFlag = read_int(line, "precise orbits");
	    if (strncmp(test, "status", 6)==0) cfg->lz2raw->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[ceos2raw]", 8)==0) strcpy(params, "ceos2raw");
	  if (strcmp(params, "ceos2raw")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->ceos2raw->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[trim_slc]", 8)==0) strcpy(params, "trim_slc");
	  if (strcmp(params, "trim_slc")==0) {
	    test = read_param(line);
	    if (strncmp(test, "start line", 10)==0) cfg->trim_slc->line = read_int(line, "start line"); 
	    if (strncmp(test, "start sample", 12)==0) cfg->trim_slc->sample = read_int(line, "start sample"); 
	    if (strncmp(test, "length", 6)==0) cfg->trim_slc->length = read_int(line, "length"); 
	    if (strncmp(test, "width", 5)==0) cfg->trim_slc->width = read_int(line, "width"); 
	    if (strncmp(test, "status", 6)==0) cfg->trim_slc->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[avg_in_dop]", 12)==0) strcpy(params, "avg_in_dop");
	  if (strcmp(params, "avg_in_dop")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->avg_in_dop->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[paisp - Master image]", 22)==0) strcpy(params, "aisp_master");
	  if (strcmp(params, "aisp_master")==0) {
	    test = read_param(line);
	    if (strncmp(test, "start offset", 12)==0) cfg->aisp_master->start_offset = read_int(line, "start offset"); 
	    if (strncmp(test, "end offset", 10)==0) cfg->aisp_master->end_offset = read_int(line, "end offset"); 
	    if (strncmp(test, "patches", 7)==0) cfg->aisp_master->patches = read_int(line, "patches"); 
	    if (strncmp(test, "power flag", 10)==0) cfg->aisp_master->power = read_int(line, "power flag"); 
	    if (strncmp(test, "power image", 11)==0) cfg->aisp_master->power_img = read_str(line, "power image"); 
	    if (strncmp(test, "status", 6)==0) cfg->aisp_master->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[Coregister first patch]", 24)==0) strcpy(params, "coreg_p1");
	  if (strcmp(params, "coreg_p1")==0) {
	    test = read_param(line);
	    if (strncmp(test, "start master", 12)==0) cfg->coreg_p1->start_master = read_int(line, "start master"); 
	    if (strncmp(test, "start slave", 11)==0) cfg->coreg_p1->start_slave = read_int(line, "start slave"); 
	    if (strncmp(test, "grid", 4)==0) cfg->coreg_p1->grid = read_int(line, "grid"); 
	    if (strncmp(test, "fft", 3)==0) cfg->coreg_p1->fft = read_int(line, "fft"); 
	    if (strncmp(test, "offset azimuth", 14)==0) cfg->coreg_p1->off_az = read_int(line, "offset azimuth");
	    if (strncmp(test, "offset range", 12)==0) cfg->coreg_p1->off_rng = read_int(line, "offset azimuth");
	    if (strncmp(test, "status", 6)==0) cfg->coreg_p1->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[Coregister last patch]", 23)==0) strcpy(params, "coreg_pL");
	  if (strcmp(params, "coreg_pL")==0) {
	    test = read_param(line);
	    if (strncmp(test, "start master", 12)==0) cfg->coreg_pL->start_master = read_int(line, "start master"); 
	    if (strncmp(test, "start slave", 11)==0) cfg->coreg_pL->start_slave = read_int(line, "start slave"); 
	    if (strncmp(test, "grid", 4)==0) cfg->coreg_pL->grid = read_int(line, "grid"); 
	    if (strncmp(test, "fft", 3)==0) cfg->coreg_pL->fft = read_int(line, "fft"); 
	    if (strncmp(test, "offset azimuth", 14)==0) cfg->coreg_pL->off_az = read_int(line, "offset azimuth");
	    if (strncmp(test, "offset range", 12)==0) cfg->coreg_pL->off_rng = read_int(line, "offset azimuth");
	    if (strncmp(test, "status", 6)==0) cfg->coreg_pL->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[paisp - Slave image]", 21)==0) strcpy(params, "aisp_slave");
	  if (strcmp(params, "aisp_slave")==0) {
	    test = read_param(line);
	    if (strncmp(test, "start offset", 6)==0) cfg->aisp_slave->start_offset = read_int(line, "start offset"); 
	    if (strncmp(test, "patches", 7)==0) cfg->aisp_slave->patches = read_int(line, "patches"); 
	    if (strncmp(test, "power flag", 10)==0) cfg->aisp_slave->power = read_int(line, "power flag"); 
	    if (strncmp(test, "power image", 11)==0) cfg->aisp_slave->power_img = read_str(line, "power image"); 
	    if (strncmp(test, "status", 6)==0) cfg->aisp_slave->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[cpx_autofilter]", 14)==0) strcpy(params, "cpx_autofilter");
	  if (strcmp(params, "cpx_autofilter")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->cpx_autofilter->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Coregister slave]", 16)==0) strcpy(params, "coreg_slave");
	  if (strcmp(params, "coreg_slave")==0) {
	    test = read_param(line);
	    if (strncmp(test, "grid", 4)==0) cfg->coreg_slave->grid = read_int(line, "grid"); 
	    if (strncmp(test, "fft", 3)==0) cfg->coreg_slave->fft = read_int(line, "fft"); 
	    if (strncmp(test, "sinc", 4)==0) cfg->coreg_slave->sinc = read_int(line, "sinc"); 
	    if (strncmp(test, "warp", 4)==0) cfg->coreg_slave->warp = read_int(line, "warp"); 
	    if (strncmp(test, "status", 6)==0) cfg->coreg_slave->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[Interferogram/coherence]", 25)==0) strcpy(params, "igram_coh");
	  if (strcmp(params, "igram_coh")==0) {
	    test = read_param(line);
	    if (strncmp(test, "interferogram", 13)==0) cfg->igram_coh->igram = read_str(line, "interferogram"); 
	    if (strncmp(test, "coherence image", 15)==0) cfg->igram_coh->coh = read_str(line, "coherence image");
	    if (strncmp(test, "minimum coherence", 17)==0) cfg->igram_coh->min = read_double(line, "minimum coherence");
	    if (strncmp(test, "ml", 2)==0) cfg->igram_coh->ml = read_int(line, "ml");
	    if (strncmp(test, "status", 6)==0) cfg->igram_coh->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Offset matching]", 17)==0) strcpy(params, "offset_match");
	  if (strcmp(params, "offset_match")==0) {
	    test = read_param(line);
	    if (strncmp(test, "max", 3)==0) cfg->offset_match->max = read_double(line, "max");
	    if (strncmp(test, "status", 6)==0) cfg->offset_match->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Simulated phase]", 17)==0) strcpy(params, "sim_phase");
	  if (strcmp(params, "sim_phase")==0) {
	    test = read_param(line);
	    if (strncmp(test, "seeds", 5)==0) cfg->sim_phase->seeds = read_str(line, "seeds");
	    if (strncmp(test, "status", 6)==0) cfg->sim_phase->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Deramp/multilook]", 18)==0) strcpy(params, "deramp_ml");
	  if (strcmp(params, "deramp_ml")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->deramp_ml->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Phase unwrapping]", 18)==0) strcpy(params, "unwrap");
	  if (strcmp(params, "unwrap")==0) {
	    test = read_param(line);
	    if (strncmp(test, "algorithm", 9)==0) cfg->unwrap->algorithm = read_str(line, "algorithm");
	    if (strncmp(test, "flattening", 10)==0) cfg->unwrap->flattening = read_int(line, "flattening");
	    if (strncmp(test, "tiles azimuth", 13)==0) cfg->unwrap->tiles_azimuth = read_int(line, "tiles azimuth");
	    if (strncmp(test, "tiles range", 11)==0) cfg->unwrap->tiles_range = read_int(line, "tiles range");
	    if (strncmp(test, "tiles per degree", 16)==0) cfg->unwrap->tiles_per_degree = read_int(line, "tiles per degree");
	    if (strncmp(test, "overlap azimuth", 15)==0) cfg->unwrap->overlap_azimuth = read_int(line, "overlap azimuth");
	    if (strncmp(test, "overlap range", 13)==0) cfg->unwrap->overlap_range = read_int(line, "overlap range");
	    if (strncmp(test, "filter", 6)==0) cfg->unwrap->filter = read_double(line, "filter");
	    if (strncmp(test, "status", 6)==0) cfg->unwrap->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Baseline refinement]", 21)==0) strcpy(params, "refine");
	  if (strcmp(params, "refine")==0) {
	    test = read_param(line);
	    if (strncmp(test, "iter", 4)==0) cfg->refine->iter = read_int(line, "iterations");
	    if (strncmp(test, "max", 3)==0) cfg->refine->max = read_int(line, "max iterations");
	    if (strncmp(test, "status", 6)==0) cfg->refine->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Elevation]", 11)==0) strcpy(params, "elev");
	  if (strcmp(params, "elev")==0) {
	    test = read_param(line);
            if (strncmp(test, "dem", 3)==0) cfg->elevation->dem = read_str(line, "dem");
            if (strncmp(test, "error map", 9)==0) cfg->elevation->error = read_str(line, "error map");
	    if (strncmp(test, "status", 6)==0) cfg->elevation->status = read_str(line, "status");
	  }

          if (strncmp(line, "[Ground range DEM]", 18)==0) strcpy(params, "final_dem");
          if (strcmp(params, "final_dem")==0) {
            test = read_param(line);
            if (strncmp(test, "status", 6)==0) cfg->ground_range->status = read_str(line, "status");   
          }
            
          if (strncmp(line, "[Geocoding]", 11)==0) strcpy(params, "geocoding");
          if (strcmp(params, "geocoding")==0) {
            test = read_param(line);
            if (strncmp(test, "dem", 3)==0) cfg->geocode->dem = read_str(line, "dem");
            if (strncmp(test, "error map", 9)==0) cfg->geocode->error = read_str(line, "error map");
            if (strncmp(test, "amplitude", 9)==0) cfg->geocode->amp = read_str(line, "amplitude");
            if (strncmp(test, "coherence", 9)==0) cfg->geocode->coh = read_str(line, "coherence");
            if (strncmp(test, "projection file", 15)==0) cfg->geocode->proj = read_str(line, "projection file");
            if (strncmp(test, "projection key", 14)==0) cfg->geocode->key = read_str(line, "projection key");
            if (strncmp(test, "pixel spacing", 13)==0) cfg->geocode->pix_spacing = read_int(line, "pixel spacing");
            if (strncmp(test, "status", 6)==0) cfg->geocode->status = read_str(line, "status");   
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
	fprintf(fConfig, "reference dem = %s\n", cfg->general->dem);
	fprintf(fConfig, "base name = %s\n", cfg->general->base);
	fprintf(fConfig, "log file = %i\n", cfg->general->log);
	fprintf(fConfig, "quiet = %i\n", cfg->general->quiet);
	fprintf(fConfig, "processors = %i\n", cfg->general->procs);
	fprintf(fConfig, "data type = %s\n", cfg->general->data_type);
	fprintf(fConfig, "lat begin = %.3f\n", cfg->general->lat_begin);
	fprintf(fConfig, "lat end = %.3f\n", cfg->general->lat_end);
	fprintf(fConfig, "coregistration = %s\n", cfg->general->coreg);
	fprintf(fConfig, "maximum offset = %d\n", cfg->general->max_off);
	fprintf(fConfig, "default values = %s\n", cfg->general->def_val);
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
	  fprintf(fConfig, "[lz2raw_flywheel]\n");
	  fprintf(fConfig, "precise master = %s\n", cfg->lz2raw->prc_master);
	  fprintf(fConfig, "precise slave = %s\n", cfg->lz2raw->prc_slave);
	  fprintf(fConfig, "precise orbits = %d\n", cfg->lz2raw->prcFlag);
	  fprintf(fConfig, "status = %s\n\n", cfg->lz2raw->status);
	  fprintf(fConfig, "[avg_in_dop]\n");
	  fprintf(fConfig, "status = %s\n\n", cfg->avg_in_dop->status);
	  fprintf(fConfig, "[paisp - Master image]\n");
	  fprintf(fConfig, "start offset = %ld\n", cfg->aisp_master->start_offset);
	  fprintf(fConfig, "end offset = %ld\n", cfg->aisp_master->end_offset);
	  fprintf(fConfig, "patches = %d\n", cfg->aisp_master->patches);
	  fprintf(fConfig, "power flag = %d\n", cfg->aisp_master->power);
	  fprintf(fConfig, "power image = %s\n", cfg->aisp_master->power_img);
	  fprintf(fConfig, "status = %s\n\n", cfg->aisp_master->status);
	  fprintf(fConfig, "[Coregister first patch]\n");
	  fprintf(fConfig, "start master = %ld\n", cfg->coreg_p1->start_master);
	  fprintf(fConfig, "start slave = %ld\n", cfg->coreg_p1->start_slave);
	  fprintf(fConfig, "grid = %ld\n", cfg->coreg_p1->grid);
	  fprintf(fConfig, "fft = %d\n", cfg->coreg_p1->fft);
	  fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_p1->off_az);
	  fprintf(fConfig, "offset range = %d\n", cfg->coreg_p1->off_rng);
	  fprintf(fConfig, "status = %s\n\n", cfg->coreg_p1->status);
	  fprintf(fConfig, "[Coregister last patch]\n");
	  fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
	  fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
	  fprintf(fConfig, "grid = %ld\n", cfg->coreg_pL->grid);
	  fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
	  fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_pL->off_az);
	  fprintf(fConfig, "offset range = %d\n", cfg->coreg_pL->off_rng);
	  fprintf(fConfig, "status = %s\n\n", cfg->coreg_pL->status);
	  fprintf(fConfig, "[paisp - Slave image]\n");
	  fprintf(fConfig, "start offset = %ld\n", cfg->aisp_slave->start_offset);
	  fprintf(fConfig, "patches = %d\n", cfg->aisp_slave->patches);
	  fprintf(fConfig, "power flag = %d\n", cfg->aisp_slave->power);
	  fprintf(fConfig, "power image = %s\n", cfg->aisp_slave->power_img);
	  fprintf(fConfig, "status = %s\n\n", cfg->aisp_slave->status);
	}
	if (strncmp(cfg->general->data_type, "RAW", 3)==0) {
	  fprintf(fConfig, "[ceos2raw]\n");
	  fprintf(fConfig, "status = %s\n\n", cfg->ceos2raw->status);
	  fprintf(fConfig, "[avg_in_dop]\n");
	  fprintf(fConfig, "status = %s\n\n", cfg->avg_in_dop->status);
	  fprintf(fConfig, "[paisp - Master image]\n");
	  fprintf(fConfig, "start offset = %ld\n", cfg->aisp_master->start_offset);
	  fprintf(fConfig, "end offset = %ld\n", cfg->aisp_master->end_offset);
	  fprintf(fConfig, "patches = %d\n", cfg->aisp_master->patches);
	  fprintf(fConfig, "power flag = %d\n", cfg->aisp_master->power);
	  fprintf(fConfig, "power image = %s\n", cfg->aisp_master->power_img);
	  fprintf(fConfig, "status = %s\n\n", cfg->aisp_master->status);
	  fprintf(fConfig, "[Coregister first patch]\n");
	  fprintf(fConfig, "start master = %ld\n", cfg->coreg_p1->start_master);
	  fprintf(fConfig, "start slave = %ld\n", cfg->coreg_p1->start_slave);
	  fprintf(fConfig, "grid = %ld\n", cfg->coreg_p1->grid);
	  fprintf(fConfig, "fft = %d\n", cfg->coreg_p1->fft);
	  fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_p1->off_az);
	  fprintf(fConfig, "offset range = %d\n", cfg->coreg_p1->off_rng);
	  fprintf(fConfig, "status = %s\n\n", cfg->coreg_p1->status);
	  fprintf(fConfig, "[Coregister last patch]\n");
	  fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
	  fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
	  fprintf(fConfig, "grid = %ld\n", cfg->coreg_pL->grid);
	  fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
	  fprintf(fConfig, "offset azimuth = %d\n", cfg->coreg_pL->off_az);
	  fprintf(fConfig, "offset range = %d\n", cfg->coreg_pL->off_rng);
	  fprintf(fConfig, "status = %s\n\n", cfg->coreg_pL->status);
	  fprintf(fConfig, "[paisp - Slave image]\n");
	  fprintf(fConfig, "start offset = %ld\n", cfg->aisp_slave->start_offset);
	  fprintf(fConfig, "patches = %d\n", cfg->aisp_slave->patches);
	  fprintf(fConfig, "power flag = %d\n", cfg->aisp_slave->power);
	  fprintf(fConfig, "power image = %s\n", cfg->aisp_slave->power_img);
	  fprintf(fConfig, "status = %s\n\n", cfg->aisp_slave->status);
	}
	if (strncmp(cfg->general->data_type, "SLC", 3)==0) {
	  fprintf(fConfig, "[trim_slc]\n");
	  fprintf(fConfig, "start line = %ld\n", cfg->trim_slc->line);
	  fprintf(fConfig, "start sample = %ld\n", cfg->trim_slc->sample);
	  fprintf(fConfig, "length = %ld\n", cfg->trim_slc->length);
	  fprintf(fConfig, "width = %ld\n", cfg->trim_slc->width);
	  fprintf(fConfig, "status = %s\n\n", cfg->trim_slc->status);
	  fprintf(fConfig, "[cpx_autofilter]\n");
	  fprintf(fConfig, "status = %s\n\n", cfg->cpx_autofilter->status);
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
	fprintf(fConfig, "ml = %d\n", cfg->igram_coh->ml);
	fprintf(fConfig, "status = %s\n\n", cfg->igram_coh->status);
	fprintf(fConfig, "[Offset matching]\n");
	fprintf(fConfig, "max = %.1lf\n", cfg->offset_match->max);
	fprintf(fConfig, "status = %s\n\n", cfg->offset_match->status);
	fprintf(fConfig, "[Simulated phase]\n");
	fprintf(fConfig, "seeds = %s\n", cfg->sim_phase->seeds);
	fprintf(fConfig, "status = %s\n\n", cfg->sim_phase->status);
	fprintf(fConfig, "[Deramp/multilook]\n");
	fprintf(fConfig, "status = %s\n\n", cfg->deramp_ml->status);
	fprintf(fConfig, "[Phase unwrapping]\n");
	fprintf(fConfig, "algorithm = %s\n", cfg->unwrap->algorithm);
	fprintf(fConfig, "flattening = %d\n", cfg->unwrap->flattening);
	fprintf(fConfig, "tiles azimuth = %d\n", cfg->unwrap->tiles_azimuth);
	fprintf(fConfig, "tiles range = %d\n", cfg->unwrap->tiles_range);
	fprintf(fConfig, "tiles per degree = %d\n", cfg->unwrap->tiles_per_degree);
	fprintf(fConfig, "overlap azimuth = %d\n", cfg->unwrap->overlap_azimuth);
	fprintf(fConfig, "overlap range = %d\n", cfg->unwrap->overlap_range);
	fprintf(fConfig, "filter = %.1f\n", cfg->unwrap->filter);
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
	fprintf(fConfig, "projection file = %s\n", cfg->geocode->proj);
	fprintf(fConfig, "projection key = %s\n", cfg->geocode->key);
	fprintf(fConfig, "pixel spacing = %d\n", cfg->geocode->pix_spacing);
	fprintf(fConfig, "status = %s\n\n", cfg->geocode->status);

	FCLOSE(fConfig);
 
	return(0);
}
