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
	cfg->fix_in_fromraw = newStruct(s_status);
	cfg->swath_offset = newStruct(s_status);
	cfg->water_mask = newStruct(s_water_mask);
	cfg->avg_in_dop = newStruct(s_status);
	cfg->aisp_master = newStruct(s_aisp);
	cfg->coreg_p1 = newStruct(s_coreg);
	cfg->coreg_pL = newStruct(s_coreg);
	cfg->aisp_slave = newStruct(s_aisp);
	cfg->igram_coh = newStruct(s_igram_coh);
	cfg->dem_sim = newStruct(s_status);
	cfg->offset_match = newStruct(s_offset_match);
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
	cfg->general->def_val = "/3dsar2/tlogan/default_values"; /* only for the AKDEM project - remove afterwards */
	cfg->general->base = (char *)MALLOC(sizeof(char)*255);
	cfg->general->base = "";
	cfg->general->log = 1;
	cfg->general->quiet = 1;
	cfg->general->procs = 1;
	cfg->general->lat_begin = -99;
	cfg->general->lat_end = -99;
	cfg->general->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->general->status, "new");

	cfg->master->path = (char *)MALLOC(sizeof(char)*255);
	cfg->master->path = "";
	cfg->master->data = (char *)MALLOC(sizeof(char)*255);
	cfg->master->data = "";
	cfg->master->par = (char *)MALLOC(sizeof(char)*255);
	cfg->master->par = "";

	cfg->slave->path = (char *)MALLOC(sizeof(char)*255);
	cfg->slave->path = "";
	cfg->slave->data = (char *)MALLOC(sizeof(char)*255);
	cfg->slave->data = "";
	cfg->slave->par = (char *)MALLOC(sizeof(char)*255);
	cfg->slave->par = "";

	cfg->lz2raw->prc_e1 = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->lz2raw->prc_e1, "");
	cfg->lz2raw->prc_e2 = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->lz2raw->prc_e2, "");
	cfg->lz2raw->prcFlag = 1;
	cfg->lz2raw->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->lz2raw->status, "new");

	cfg->fix_in_fromraw->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->fix_in_fromraw->status, "new");

	cfg->swath_offset->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->swath_offset->status, "new");

	cfg->water_mask->percent = -1.0;
	cfg->water_mask->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->water_mask->status, "new");

	cfg->avg_in_dop->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->avg_in_dop->status, "new");

	cfg->aisp_master->start_offset = -99;
	cfg->aisp_master->end_offset = -99;
	cfg->aisp_master->patches = 1;
	cfg->aisp_master->power = 1;
	cfg->aisp_master->power_img = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->aisp_master->power_img, "");
	cfg->aisp_master->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->aisp_master->status, "new");

	cfg->coreg_p1->start_master = -99;
	cfg->coreg_p1->start_slave = -99;
	cfg->coreg_p1->grid = 20;
	cfg->coreg_p1->fft = 1;
	cfg->coreg_p1->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->coreg_p1->status, "new");

	cfg->coreg_pL->start_master = -99;
	cfg->coreg_pL->start_slave = -99;
	cfg->coreg_pL->grid = 20;
	cfg->coreg_pL->fft = 1;
	cfg->coreg_pL->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->coreg_pL->status, "new");

	cfg->aisp_slave->start_offset = -99;
	cfg->aisp_slave->patches = 1;
	cfg->aisp_slave->power = 1;
	cfg->aisp_slave->power_img = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->aisp_slave->power_img, "");
	cfg->aisp_slave->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->aisp_slave->status, "new");

	cfg->igram_coh->igram = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->igram_coh->igram, "");
	cfg->igram_coh->coh = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->igram_coh->coh, "");
	cfg->igram_coh->min = 0.3;
	cfg->igram_coh->ml = 1;
	cfg->igram_coh->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->igram_coh->status, "new");

	cfg->dem_sim->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->dem_sim->status, "new");

	cfg->offset_match->offset = 1;
	cfg->offset_match->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->offset_match->status, "new");

	cfg->sim_phase->seeds = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->sim_phase->seeds, "");
	cfg->sim_phase->fft = 1;
	cfg->sim_phase->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->sim_phase->status, "new");

	cfg->deramp_ml->status = (char *)MALLOC(sizeof(char)*25);
	strcpy(cfg->deramp_ml->status, "new");

	cfg->unwrap->algorithm = (char *)MALLOC(sizeof(char)*255);
	strcpy(cfg->unwrap->algorithm, "");
	cfg->unwrap->flattening = 0;
	cfg->unwrap->tiles_azimuth = 3;
	cfg->unwrap->tiles_range = 3;
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
	    if (strncmp(test, "reference dem", 13)==0) cfg->general->dem = read_str(line, "reference dem"); 
	    if (strncmp(test, "base name", 9)==0) cfg->general->base = read_str(line, "base name");
	    if (strncmp(test, "log file", 8)==0) cfg->general->log = read_int(line, "log file");
	    if (strncmp(test, "quiet", 5)==0) cfg->general->quiet = read_int(line, "quiet");
	    if (strncmp(test, "processors", 10)==0) cfg->general->procs = read_int(line, "processors");
	    if (strncmp(test, "lat begin", 9)==0) cfg->general->lat_begin = read_double(line, "lat_begin"); 
	    if (strncmp(test, "lat end", 7)==0) cfg->general->lat_end = read_double(line, "lat_end"); 
	    if (strncmp(test, "default values", 14)==0) cfg->general->def_val = read_str(line, "default values");
	    if (strncmp(test, "status", 6)==0) cfg->general->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
	  if (strcmp(params, "master image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->master->path = read_str(line, "path");
	    if (strncmp(test, "data file", 9)==0) cfg->master->data = read_str(line, "data file");
	    if (strncmp(test, "par file", 8)==0) cfg->master->par = read_str(line, "par file");
	  }

	  if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
	  if (strcmp(params, "slave image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->slave->path = read_str(line, "path"); 
	    if (strncmp(test, "data file", 9)==0) cfg->slave->data = read_str(line, "data file");
	    if (strncmp(test, "par file", 8)==0) cfg->slave->par = read_str(line, "par file");
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
	    if (strncmp(test, "precise E1", 10)==0) cfg->lz2raw->prc_e1 = read_str(line, "precise E1"); 
	    if (strncmp(test, "precise E2", 10)==0) cfg->lz2raw->prc_e2 = read_str(line, "precise E2");
	    if (strncmp(test, "minimum coherence", 17)==0) cfg->igram_coh->min = read_double(line, "minimum coherence");
	    if (strncmp(test, "phase unwrapping", 16)==0) cfg->unwrap->algorithm = read_str(line, "phase unwrapping");
            if (strncmp(test, "projection file", 15)==0) cfg->geocode->proj = read_str(line, "projection file");
            if (strncmp(test, "projection key", 14)==0) cfg->geocode->key = read_str(line, "projection key");
            if (strncmp(test, "pixel spacing", 13)==0) cfg->geocode->pix_spacing = read_int(line, "pixel spacing");
	  }
	  FCLOSE(fDefaults);
	}

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
	    if (strncmp(test, "lat begin", 9)==0) cfg->general->lat_begin = read_double(line, "lat_begin"); 
	    if (strncmp(test, "lat end", 7)==0) cfg->general->lat_end = read_double(line, "lat_end"); 
	    if (strncmp(test, "default values", 14)==0) cfg->general->def_val = read_str(line, "default values");
	    if (strncmp(test, "status", 6)==0) cfg->general->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Master image]", 14)==0) strcpy(params, "master image");
	  if (strcmp(params, "master image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->master->path = read_str(line, "path");
	    if (strncmp(test, "data file", 9)==0) cfg->master->data = read_str(line, "data file");
	    if (strncmp(test, "par file", 8)==0) cfg->master->par = read_str(line, "par file");
	  }

	  if (strncmp(line, "[Slave image]", 13)==0) strcpy(params, "slave image");
	  if (strcmp(params, "slave image")==0) {
	    test = read_param(line);
	    if (strncmp(test, "path", 4)==0) cfg->slave->path = read_str(line, "path"); 
	    if (strncmp(test, "data file", 9)==0) cfg->slave->data = read_str(line, "data file");
	    if (strncmp(test, "par file", 8)==0) cfg->slave->par = read_str(line, "par file");
	  }

	  if (strncmp(line, "[lz2raw_flywheel]", 17)==0) strcpy(params, "lz2raw_flywheel");
	  if (strcmp(params, "lz2raw_flywheel")==0) {
	    test = read_param(line);
	    if (strncmp(test, "precise E1", 10)==0) cfg->lz2raw->prc_e1 = read_str(line, "precise E1"); 
	    if (strncmp(test, "precise E2", 10)==0) cfg->lz2raw->prc_e2 = read_str(line, "precise E2");
	    if (strncmp(test, "precise orbits", 14)==0) cfg->lz2raw->prcFlag = read_int(line, "precise orbits");
	    if (strncmp(test, "status", 6)==0) cfg->lz2raw->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[fix_in_fromraw]", 16)==0) strcpy(params, "fix_in_fromraw");
	  if (strcmp(params, "fix_in_fromraw")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->fix_in_fromraw->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[swath_offset]", 14)==0) strcpy(params, "swath_offset");
	  if (strcmp(params, "swath_offset")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->swath_offset->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[water_mask]", 12)==0) strcpy(params, "water_mask");
	  if (strcmp(params, "water_mask")==0) {
	    test = read_param(line);
	    if (strncmp(test, "percent", 7)==0) cfg->water_mask->percent = read_double(line, "percent"); 
	    if (strncmp(test, "status", 6)==0) cfg->water_mask->status = read_str(line, "status"); 
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
	    if (strncmp(test, "status", 6)==0) cfg->coreg_p1->status = read_str(line, "status"); 
	  }	  

	  if (strncmp(line, "[Coregister last patch]", 23)==0) strcpy(params, "coreg_pL");
	  if (strcmp(params, "coreg_pL")==0) {
	    test = read_param(line);
	    if (strncmp(test, "start master", 12)==0) cfg->coreg_pL->start_master = read_int(line, "start master"); 
	    if (strncmp(test, "start slave", 11)==0) cfg->coreg_pL->start_slave = read_int(line, "start slave"); 
	    if (strncmp(test, "grid", 4)==0) cfg->coreg_pL->grid = read_int(line, "grid"); 
	    if (strncmp(test, "fft", 3)==0) cfg->coreg_pL->fft = read_int(line, "fft"); 
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

	  if (strncmp(line, "[Interferogram/coherence]", 25)==0) strcpy(params, "igram_coh");
	  if (strcmp(params, "igram_coh")==0) {
	    test = read_param(line);
	    if (strncmp(test, "interferogram", 13)==0) cfg->igram_coh->igram = read_str(line, "interferogram"); 
	    if (strncmp(test, "coherence image", 15)==0) cfg->igram_coh->coh = read_str(line, "coherence image");
	    if (strncmp(test, "minimum coherence", 17)==0) cfg->igram_coh->min = read_double(line, "minimum coherence");
	    if (strncmp(test, "ml", 2)==0) cfg->igram_coh->ml = read_int(line, "ml");
	    if (strncmp(test, "status", 6)==0) cfg->igram_coh->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Subset DEM]", 10)==0) strcpy(params, "dem_sim");
	  if (strcmp(params, "dem_sim")==0) {
	    test = read_param(line);
	    if (strncmp(test, "status", 6)==0) cfg->dem_sim->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Pixel offset matching]", 23)==0) strcpy(params, "offset_match");
	  if (strcmp(params, "offset_match")==0) {
	    test = read_param(line);
	    if (strncmp(test, "offset", 6)==0) cfg->offset_match->offset = read_int(line, "offset");
	    if (strncmp(test, "status", 6)==0) cfg->offset_match->status = read_str(line, "status");
	  }

	  if (strncmp(line, "[Simulated phase]", 17)==0) strcpy(params, "sim_phase");
	  if (strcmp(params, "sim_phase")==0) {
	    test = read_param(line);
	    if (strncmp(test, "seeds", 5)==0) cfg->sim_phase->seeds = read_str(line, "seeds");
	    if (strncmp(test, "fft", 3)==0) cfg->sim_phase->fft = read_int(line, "fft");
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
	fprintf(fConfig, "lat begin = %.3f\n", cfg->general->lat_begin);
	fprintf(fConfig, "lat end = %.3f\n", cfg->general->lat_end);
	fprintf(fConfig, "default values = %s\n", cfg->general->def_val);
	fprintf(fConfig, "status = %s\n\n", cfg->general->status);
	fprintf(fConfig, "[Master image]\n");
	fprintf(fConfig, "path = %s\n", cfg->master->path);
	fprintf(fConfig, "data file = %s\n", cfg->master->data);
	fprintf(fConfig, "par file = %s\n\n", cfg->master->par);
	fprintf(fConfig, "[Slave image]\n");
	fprintf(fConfig, "path = %s\n", cfg->slave->path);
	fprintf(fConfig, "data file = %s\n", cfg->slave->data);
	fprintf(fConfig, "par file = %s\n\n", cfg->slave->par);
	fprintf(fConfig, "[lz2raw_flywheel]\n");
	fprintf(fConfig, "precise E1 = %s\n", cfg->lz2raw->prc_e1);
	fprintf(fConfig, "precise E2 = %s\n", cfg->lz2raw->prc_e2);
	fprintf(fConfig, "precise orbits = %d\n", cfg->lz2raw->prcFlag);
	fprintf(fConfig, "status = %s\n\n", cfg->lz2raw->status);
	fprintf(fConfig, "[fix_in_fromraw]\n");
	fprintf(fConfig, "status = %s\n\n", cfg->fix_in_fromraw->status);
	fprintf(fConfig, "[swath_offset]\n");
	fprintf(fConfig, "status = %s\n\n", cfg->swath_offset->status);
	fprintf(fConfig, "[water_mask]\n");
	fprintf(fConfig, "percent = %.1f\n", cfg->water_mask->percent);
	fprintf(fConfig, "status = %s\n\n", cfg->water_mask->status);
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
	fprintf(fConfig, "status = %s\n\n", cfg->coreg_p1->status);
	fprintf(fConfig, "[Coregister last patch]\n");
	fprintf(fConfig, "start master = %ld\n", cfg->coreg_pL->start_master);
	fprintf(fConfig, "start slave = %ld\n", cfg->coreg_pL->start_slave);
	fprintf(fConfig, "grid = %ld\n", cfg->coreg_pL->grid);
	fprintf(fConfig, "fft = %d\n", cfg->coreg_pL->fft);
	fprintf(fConfig, "status = %s\n\n", cfg->coreg_pL->status);
	fprintf(fConfig, "[paisp - Slave image]\n");
	fprintf(fConfig, "start offset = %ld\n", cfg->aisp_slave->start_offset);
	fprintf(fConfig, "patches = %d\n", cfg->aisp_slave->patches);
	fprintf(fConfig, "power flag = %d\n", cfg->aisp_slave->power);
	fprintf(fConfig, "power image = %s\n", cfg->aisp_slave->power_img);
	fprintf(fConfig, "status = %s\n\n", cfg->aisp_slave->status);
	fprintf(fConfig, "[Interferogram/coherence]\n");
	fprintf(fConfig, "interferogram = %s\n", cfg->igram_coh->igram);
	fprintf(fConfig, "coherence image = %s\n", cfg->igram_coh->coh);
	fprintf(fConfig, "minimum coherence = %.1f\n", cfg->igram_coh->min);
	fprintf(fConfig, "ml = %d\n", cfg->igram_coh->ml);
	fprintf(fConfig, "status = %s\n\n", cfg->igram_coh->status);
	fprintf(fConfig, "[Subset DEM]\n");
	fprintf(fConfig, "status = %s\n\n", cfg->dem_sim->status);
	fprintf(fConfig, "[Pixel offset matching]\n");
	fprintf(fConfig, "offset = %d\n", cfg->offset_match->offset);
	fprintf(fConfig, "status = %s\n\n", cfg->offset_match->status);
	fprintf(fConfig, "[Simulated phase]\n");
	fprintf(fConfig, "seeds = %s\n", cfg->sim_phase->seeds);
	fprintf(fConfig, "fft = %d\n", cfg->sim_phase->fft);
	fprintf(fConfig, "status = %s\n\n", cfg->sim_phase->status);
	fprintf(fConfig, "[Deramp/multilook]\n");
	fprintf(fConfig, "status = %s\n\n", cfg->deramp_ml->status);
	fprintf(fConfig, "[Phase unwrapping]\n");
	fprintf(fConfig, "algorithm = %s\n", cfg->unwrap->algorithm);
	fprintf(fConfig, "flattening = %d\n", cfg->unwrap->flattening);
	fprintf(fConfig, "tiles azimuth = %d\n", cfg->unwrap->tiles_azimuth);
	fprintf(fConfig, "tiles range = %d\n", cfg->unwrap->tiles_range);
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
	fprintf(fConfig, "projection file = %s\n", cfg->geocode->proj);
	fprintf(fConfig, "projection key = %s\n", cfg->geocode->key);
	fprintf(fConfig, "pixel spacing = %d\n", cfg->geocode->pix_spacing);
	fprintf(fConfig, "status = %s\n\n", cfg->geocode->status);

	FCLOSE(fConfig);
 
	return(0);
}
