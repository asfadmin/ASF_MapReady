/********************************************************************************
NAME:
	ips.c

SYNOPSIS:

DESCRIPTION:
	This program performs InSAR and DInSAR processing

FILE REFERENCES:
	NAME:		USAGE:
	---------------------------------------------------------------------
	.config		configuration file for all input files

PROGRAM HISTORY:
	VERS:   DATE:   AUTHOR:
	----------------------------------------------------------------------
	1.0	8/01	R. Gens, original development
	1.1	2/02	R. Gens, added ability to read CEOS raw and SLC data
	1.2	2/03	R. Gens, adapted processing flow for updated Doppler 
                                 processing
	1.3	3/03	R. Gens, included differential processing mode
	2.0	3/04	R. Gens, complete overhaul because of the new metadata
        2.1     6/05    R. Gens, getting stable version for summer course

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	There are no known bugs.

*********************************************************************************/

#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "ips.h"
#include "functions.h"
#include "proj.h"

#define VERSION 2.0

void usage(char *name)
{	
  printf("\n    Usage: ips [-c] <configuration file>\n\n");
  printf("           -c	creates only the configuration file and exits.\n\n");
  printf("    ips will run the complete SAR interferometric\n");
  printf(" processing chain, from ingesting the level zero STF data\n");
  printf(" until the creation and geocoding of the digital elevation model.\n");
  printf(" Alternatively, ips can run in differential processing and\n");
  printf(" produce a differential interferogram.\n");
  printf(" All the input parameters for the programs are read in from\n");
  printf(" a configuration file.\n\n");
  printf(" %1.2f, ASF InSAR Tools, 2001\n\n", VERSION);
  exit(1);
}

void check_return(int ret, char *msg)
{
  if (ret!=0) {
    sprintf(errbuf, "\n   ERROR: %s\n\n", msg);
    printErr(errbuf);
  }
}

int check_status(char *status)
{
  if (strncmp(status, "new", 3)==0) return 1;
  else if (strncmp(status, "stop", 4)==0) {
    printf("   Processing interrupted by user!\n\n");
    exit(0);
  }
  else return 0;
}

char *base2str(int baseNo, char *base)
{
  char *baseStr=(char *)MALLOC(sizeof(char)*50);
  
  if (baseNo<10) sprintf(baseStr,"%s.base.0%i", base, baseNo);
  else sprintf(baseStr,"%s.base.%i", base, baseNo);
  
  return baseStr;
}

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;
  
  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';
  
  return out;
}

main(int argc, char *argv[])
{
  
  dem_config *cfg;
  meta_parameters *meta=NULL;
  FILE *fCorr, *fCoh;
  char configFile[255], cmd[255], path[255], data[255], metadata[255], tmp[255];
  char options[255], metaFile[10];
  char *veryoldBase=NULL, *oldBase=NULL, *newBase=NULL;
  int i, delta, cFlag=0, datatype=0, off=1, nLooks, nl, ns;
  float xshift, yshift, avg;
  double lat1=0.0, lat2=0.0, lon;
  
  if (argc<2 || argc>3) usage("ips");
  if (strcmp(argv[1], "-c")==0) {
    cFlag = 1;
    strcpy(configFile, argv[2]);
  }
  else strcpy(configFile, argv[1]);
  
  if (argc == 3) printf("\nCommand line: ips -c %s", configFile);
  else printf("\nCommand line: ips %s", configFile);
  printf("\nDate: ");
  system("date");
  printf("Program: ips\n\n");
  
  logflag=quietflag=0;
  
  /* Read configuration file */
  if (!fileExists(configFile)) {
    check_return(init_config(configFile), 
		 "basic configuration file could not initialized");
    exit(0);
  }
  else cfg = read_config(configFile, cFlag);
  
  /* Setup log file */
  sprintf(logFile, "%s.log", cfg->general->base);
  if (strncmp(cfg->general->status, "new", 3)==0) fLog = FOPEN(logFile, "w");
  else fLog = FOPEN(logFile, "a");
  if (argc == 3) {
    sprintf(logbuf, "\nCommand line: ips -c %s\n", configFile); printLog(logbuf); }
  else {
    sprintf(logbuf, "\nCommand line: ips %s\n", configFile); printLog(logbuf); }
  sprintf(logbuf, "Program: ips\n\n"); printLog(logbuf);
  FCLOSE(fLog);
  
  /* Names for the results to keep */
  if (strncmp(cfg->general->status, "new", 3)==0) {
    sprintf(cfg->igram_coh->igram, "%s_igram", cfg->general->base);
    sprintf(cfg->igram_coh->coh, "coh.img", cfg->general->base);
    sprintf(cfg->aisp_master->power_img, "%s_a_pwr.img", cfg->general->base);
    sprintf(cfg->aisp_slave->power_img, "%s_b_pwr.img", cfg->general->base);
    sprintf(cfg->sim_phase->seeds, "%s.seeds", cfg->general->base);
    sprintf(cfg->dinsar->igram, "%s_digram.img", cfg->general->base);
    sprintf(cfg->unwrap->qc, "%s_qc.phase", cfg->general->base);
    sprintf(cfg->elevation->dem, "%s_ht.img", cfg->general->base);
    sprintf(cfg->elevation->error, "%s_err_ht.img", cfg->general->base);
    sprintf(cfg->geocode->dem, "%s_dem.img", cfg->general->base);
    sprintf(cfg->geocode->amp, "%s_amp.img", cfg->general->base);
    sprintf(cfg->geocode->error, "%s_error.img", cfg->general->base);
    sprintf(cfg->geocode->coh, "%s_coh.img", cfg->general->base);
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file"); 
  }
  
  /* Prepare processing */
  if (strcmp(cfg->master->data, cfg->slave->data)==0)
    check_return(1, "master and slave image data file have the same name");
  if (strcmp(cfg->master->meta, cfg->slave->meta)==0)
    check_return(1, "master and slave image meta data file have the same name");
  sscanf(cfg->master->path, "%s", path);
  sscanf(cfg->master->data, "%s", data);
  sscanf(cfg->master->meta, "%s", metadata);
  if (strncmp(cfg->general->status, "new", 3)==0 && !cFlag) {
    sprintf(tmp, "%s/%s", path, data);
    if (!fileExists(tmp)) check_return(1, "master image data file does not exist");
    sprintf(cmd, "ln -s %s/%s .", path, data); system(cmd);
    sprintf(tmp, "%s/%s", path, metadata);
    if (!fileExists(tmp)) 
      check_return(1, "master image metadata file does not exist");
    sprintf(cmd, "ln -s %s/%s .", path, metadata); system(cmd);
  }
  sscanf(cfg->slave->path, "%s", path);
  sscanf(cfg->slave->data, "%s", data);
  sscanf(cfg->slave->meta, "%s", metadata);
  if (strncmp(cfg->general->status, "new", 3)==0 && !cFlag) {
    sprintf(tmp, "%s/%s", path, data);
    if (!fileExists(tmp)) check_return(1, "slave image data file does not exist");
    sprintf(cmd, "ln -s %s/%s .", path, data); system(cmd);
    sprintf(tmp, "%s/%s", path, metadata);
    if (!fileExists(tmp)) 
      check_return(1, "slave image metadata file does not exist");
    sprintf(cmd, "ln -s %s/%s .", path, metadata); system(cmd);
    system("mkdir reg");
  } 
  
  /* Update configuration file */
  if (!cFlag) sprintf(cfg->general->status, "progress");
  check_return(write_config(configFile, cfg), "Could not update configuration file");
  
  if (cFlag) {
    printf("   Initialized complete configuration file\n\n");
    if (logflag) {
      fLog = FOPEN(logFile, "a");
      printLog("   Initialized complete configuration file\n\n");
      FCLOSE(fLog);
    } 
    exit(0);
  }
  
  /* Check the data type */
  printf("   Data type: %s\n   Processing mode: %s\n", 
	 cfg->general->data_type, cfg->general->mode);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf, "   Data type: %s\n   Processing mode: %s\n", 
	  cfg->general->data_type, cfg->general->mode);
  printLog(logbuf);
  FCLOSE(fLog);
  if (strncmp(uc(cfg->general->data_type), "STF", 3)==0) datatype = 0;
  if (strncmp(uc(cfg->general->data_type), "RAW", 3)==0) datatype = 1;
  if (strncmp(uc(cfg->general->data_type), "SLC", 3)==0) datatype = 2;
  
  /* Ingest the various data types: STF, RAW, or SLC */
  if (datatype==0) {
    
    /* Ingest of level zero STF data */
    if (check_status(cfg->stf2raw->status)) {
      if (cfg->general->lat_begin < -90.0 || cfg->general->lat_begin > 90.0) {
	cfg->general->lat_begin = -99.0;
	cfg->general->lat_end = 99.0;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      if (cfg->general->lat_end < -90.0 || cfg->general->lat_end > 90.0) {
	cfg->general->lat_begin = -99.0;
	cfg->general->lat_end = 99.0;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      if (!fileExists(cfg->master->data)) 
	check_return(1, "master image data file does not exist");
      if (!fileExists(cfg->master->meta)) 
	check_return(1, "master image metadata file does not exist"); 
      check_return(stf2raw(cfg->master->data, "a", 
			   cfg->stf2raw->prc_master, cfg->stf2raw->prcflag, 
			   cfg->general->lat_begin, cfg->general->lat_end), 
		   "ingesting master image(stf2raw)");
      if (!fileExists(cfg->slave->data)) 
	check_return(1, "slave image data file does not exist");
      if (!fileExists(cfg->slave->meta)) 
	check_return(1, "slave image metadata file does not exist"); 
      check_return(stf2raw(cfg->slave->data, "b", 
			   cfg->stf2raw->prc_slave, cfg->stf2raw->prcflag, 
			   cfg->general->lat_begin, cfg->general->lat_end), 
		   "ingesting slave image (stf2raw)");
      
      strcat(strcpy(metaFile,"a"),".meta");
      cfg->aisp_master->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      cfg->aisp_master->patches = 
	(int) ((cfg->aisp_master->end_offset-4096)/AISP_VALID_PATCH_LENGTH) + 2;
      strcat(strcpy(metaFile,"b"),".meta");
      cfg->aisp_slave->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      
      if (cfg->aisp_slave->end_offset > cfg->aisp_master->end_offset)
	cfg->aisp_slave->end_offset = cfg->aisp_master->end_offset;
      else
	cfg->aisp_master->end_offset = cfg->aisp_slave->end_offset;
      
      cfg->aisp_slave->patches = cfg->aisp_master->patches;
      
      sprintf(cfg->stf2raw->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
  }
  
  if (datatype==1) {
    
    /* Ingest of CEOS raw data */
    if (check_status(cfg->ceos2raw->status)) {
      if (!fileExists(cfg->master->data)) 
	check_return(1, "master image data file does not exist");
      if (!fileExists(cfg->master->meta)) 
	check_return(1, "master image metadata file does not exist");
      check_return(ceos2raw(cfg->master->data, "a"), 
		   "ingesting master image (ceos2raw)");
      if (!fileExists(cfg->slave->data)) 
	check_return(1, "slave image data file does not exist");
      if (!fileExists(cfg->slave->meta)) 
	check_return(1, "slave image metadata file does not exist");
      check_return(ceos2raw(cfg->slave->data, "b"), 
		   "ingesting slave image (ceos2raw)");
      
      /* Setting patches and offsets for processing */
      strcat(strcpy(metaFile,"a"),".meta");
      cfg->aisp_master->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      cfg->aisp_master->patches = 
	(int) ((cfg->aisp_master->end_offset-4096)/AISP_VALID_PATCH_LENGTH) + 2;
      strcat(strcpy(metaFile,"b"),".meta");
      cfg->aisp_slave->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      
      if (cfg->aisp_slave->end_offset > cfg->aisp_master->end_offset)
	cfg->aisp_slave->end_offset = cfg->aisp_master->end_offset;
      else
	cfg->aisp_master->end_offset = cfg->aisp_slave->end_offset;
      
      cfg->aisp_slave->patches = cfg->aisp_master->patches;
      
      sprintf(cfg->ceos2raw->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
  }
  
  if (datatype<2) {
    
    /* Calculate average Doppler */
    if (strncmp(cfg->general->doppler, "average", 7)==0) {
      if (check_status(cfg->avg_in_dop->status)) {
	check_return(avg_in_dop("a", "b", "reg/avedop"), 
		     "calculating the average Doppler (avg_in_dop)");
	system("cp reg/avedop a.dop");
	system("cp reg/avedop b.dop");
	sprintf(cfg->avg_in_dop->status, "success");
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file");
      }
    }
    
    /* Calculate updated Doppler */
    if (strncmp(cfg->general->doppler, "updated", 7)==0) {
      if (check_status(cfg->doppler_per_patch->status)) {
	check_return(doppler_per_patch(cfg->master->meta, cfg->slave->meta, 
				       "a.meta", "b.meta", 
				       "reg/deltas", "a.dop", "b.dop"), 
		     "calculating the updated Doppler (doppler_per_patch)");
	sprintf(cfg->doppler_per_patch->status, "success");
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file");
      }
    }
    
    /* Coregister whole image? */
    if (strncmp(cfg->general->coreg, "FRAME", 5)==0) {
      sprintf(cfg->coreg_p1->status, "skip");
      sprintf(cfg->coreg_pL->status, "skip");
    }
    
    /* Coregister first patch */
    if ((check_status(cfg->coreg_p1->status)) &&
	(strncmp(cfg->general->coreg, "PATCH", 5)==0)) {
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c a.dop");
      if (cfg->general->deskew == 1) sprintf(tmp, "-debug 1 -e 1");
      check_return(aisp(tmp, cfg->coreg_p1->start_master, cfg->coreg_p1->patches , 
			"a", "reg/a_p1"), 
		   "processing first patch of master image (aisp)");
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c b.dop");
      check_return(aisp(tmp, cfg->coreg_p1->start_slave, cfg->coreg_p1->patches , 
			"b", "reg/b_p1"), 
		   "processing first patch of slave image (aisp)");
      if (cfg->general->mflag) {
	strcat(strcpy(metaFile,"a"),".meta");
	nLooks = lzInt(metaFile, "sar.look_count:", NULL);
	strcat(strcpy(metaFile,"reg/a_p1"),".meta");
	nl = lzInt(metaFile, "general.line_count:", NULL);
	ns = lzInt(metaFile, "general.sample_count:", NULL);
	check_return(trim(cfg->general->mask, "reg/mask1", 
			  cfg->coreg_p1->start_master/nLooks, 0,
			  nl/nLooks, ns), "mask for first patch (trim)");
	check_return(coregister_coarse("reg/a_p1", "reg/b_p1", "reg/ctrl1", 
				       "reg/mask1"), 
		     "offset estimation first patch (coregister_coarse)");
      }
      else 
	check_return(coregister_coarse("reg/a_p1", "reg/b_p1", "reg/ctrl1", NULL), 
		     "offset estimation first patch (coregister_coarse)");
      if (cfg->coreg_p1->grid < 20 || cfg->coreg_p1->grid > 200) {
	printf("\n   WARNING: grid size out of range - "
	       "set to default value of 20\n\n");
	cfg->coreg_p1->grid = 20;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
    }
    if (check_status(cfg->coreg_p1->status)) {
      if (cfg->coreg_p1->fft < 0 || cfg->coreg_p1->fft > 1) {
	printf("\n   WARNING: FFT flag set to invalid value - "
	       "set to value of 1\n\n");
	cfg->coreg_p1->fft = 1;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      
      sprintf(cfg->coreg_p1->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
    /* Coregister last patch */
    if ((check_status(cfg->coreg_pL->status)) &&
	(strncmp(cfg->general->coreg, "PATCH", 5)==0)) {
      FILE *inFile;
      
      if ((cfg->coreg_pL->start_master==0) && (cfg->coreg_pL->start_slave==0)) {
	cfg->coreg_pL->start_master = 
	  cfg->aisp_master->end_offset - cfg->coreg_pL->patches*4096;
	cfg->coreg_pL->start_slave = 
	  cfg->aisp_slave->end_offset - cfg->coreg_pL->patches*4096;
      }
      
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c a.dop");
      if (cfg->general->deskew == 1) sprintf(tmp, "-debug 1 -e 1");
      check_return(aisp(tmp, cfg->coreg_pL->start_master, cfg->coreg_pL->patches , 
			"a", "reg/a_pL"), 
		   "processing last patch of master image (aisp)");
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c b.dop");
      check_return(aisp(tmp, cfg->coreg_pL->start_slave, cfg->coreg_pL->patches , 
			"b", "reg/b_pL"), 
		   "processing last patch of slave image (aisp)");
      
      if (cfg->general->mflag) {
	strcat(strcpy(metaFile,"a"),".meta");
	nLooks = lzInt(metaFile, "sar.look_count:", NULL);
	strcat(strcpy(metaFile,"reg/a_pL"),".meta");
	nl = lzInt(metaFile, "general.line_count:", NULL);
	ns = lzInt(metaFile, "general.sample_count:", NULL);
	check_return(trim(cfg->general->mask, "reg/maskL", 
			  cfg->coreg_pL->start_master/nLooks, 0, 
			  nl/nLooks, ns), "mask for last patch (trim)");
	check_return(coregister_coarse("reg/a_pL", "reg/b_pL", "reg/ctrlL", 
				       "reg/maskL"), 
		     "offset estimation last patch (coregister_coarse)");
      }
      else
	check_return(coregister_coarse("reg/a_pL", "reg/b_pL", "reg/ctrlL", NULL), 
		     "offset estimation last patch (coregister_coarse)");
      inFile = FOPEN("reg/ctrl1", "r");
      fscanf(inFile, "%d%d", &cfg->coreg_p1->off_rng, &cfg->coreg_p1->off_az);
      FCLOSE(inFile);
      inFile = FOPEN("reg/ctrlL", "r");
      fscanf(inFile, "%d%d", &cfg->coreg_pL->off_rng, &cfg->coreg_pL->off_az);
      FCLOSE(inFile);
      if (cfg->coreg_pL->grid < 20 || cfg->coreg_pL->grid > 200) {
	printf("\n   WARNING: grid size out of range - "
	       "set to default value of 20\n\n");
	cfg->coreg_pL->grid = 20;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
    }
    
    if ((check_status(cfg->coreg_pL->status)) &&
	(strncmp(cfg->general->coreg, "PATCH", 5)==0)) {
      if ((fabs(cfg->coreg_p1->off_rng - cfg->coreg_pL->off_rng) > 
	   cfg->general->max_off) ||
	  (fabs(cfg->coreg_p1->off_az - cfg->coreg_pL->off_az) > 
	   cfg->general->max_off)) {
	printf("   WARNING: estimated offset for first and last patch "
	       "differs more than %d pixels\n", cfg->general->max_off);
	printf("   Processing terminated to allow manual offset estimation\n\n");
	sprintf(cfg->coreg_p1->status, "new");
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file");
	exit(1);
      }
    }
    
    if (check_status(cfg->coreg_pL->status)) {
      if (cfg->coreg_pL->fft < 0 || cfg->coreg_pL->fft > 1) {
	printf("\n   WARNING: FFT flag set to invalid value - "
	       "set to value of 1\n\n");
	cfg->coreg_pL->fft = 1;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      if (cfg->general->mflag) {
	check_return(coregister_fine("reg/a_p1", "reg/b_p1", "reg/ctrl1", 
				     "reg/fico1", "reg/mask1.img", 
				     cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		     "fine coregistration first patch (coregister_fine)");
	check_return(coregister_fine("reg/a_pL", "reg/b_pL", "reg/ctrlL", 
				     "reg/ficoL", "reg/maskL.img",
				     cfg->coreg_pL->grid, cfg->coreg_pL->fft), 
		     "fine coregistration last patch (coregister_fine)");
      }
      else {
	check_return(coregister_fine("reg/a_p1", "reg/b_p1", "reg/ctrl1", 
				     "reg/fico1", NULL, 
				     cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		     "fine coregistration first patch (coregister_fine)");
	check_return(coregister_fine("reg/a_pL", "reg/b_pL", "reg/ctrlL", 
				     "reg/ficoL", NULL,
				     cfg->coreg_pL->grid, cfg->coreg_pL->fft), 
		     "fine coregistration last patch (coregister_fine)");
      }
      check_return(fit_line("reg/fico1", "reg/line1"), 
		   "fit regression line first patch (fit_line)");
      check_return(fit_line("reg/ficoL", "reg/lineL"), 
		   "fit regression line last patch (fit_line)");
      
      sprintf(cfg->coreg_pL->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    } 
    
    /* Calculate updated Doppler */
    if (strncmp(cfg->general->doppler, "updated", 7)==0) {
      if (check_status(cfg->doppler_per_patch->status)) {
	check_return(doppler_per_patch(cfg->master->meta, cfg->slave->meta, 
				       "a.meta", "b.meta", 
				       "reg/deltas", "a.dop", "b.dop"), 
		     "calculating the updated Doppler (doppler_per_patch)");
	sprintf(cfg->doppler_per_patch->status, "success");
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file");
      }
    }
    
    /* Processing the master image */
    if (check_status(cfg->aisp_master->status)) {
      if (cfg->aisp_master->power < 0 || cfg->aisp_master->power > 1) {
	printf("\n   WARNING: power flag set to invalid value - "
	       "set to value of 1\n\n");
	cfg->aisp_master->power = 1;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      if (cfg->general->deskew == 0) sprintf(options, "-debug 1 -c a.dop");
      if (cfg->general->deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (cfg->aisp_master->power == 1) strcat(options, " -power");
      
      cfg->aisp_master->start_offset = cfg->coreg_p1->start_master;
      check_return(aisp(options, cfg->aisp_master->start_offset, 
			cfg->aisp_master->patches, "a", "a"), 
		   "processing master image (aisp)");
      if (cfg->aisp_master->power == 1) {
	sprintf(cmd, "mv a_pwr.img %s_a_pwr.img", cfg->general->base); system(cmd);
	sprintf(cmd, "mv a_pwr.meta %s_a_pwr.meta", cfg->general->base); system(cmd);
      }
      sprintf(cmd, "cp a.meta %s.meta", cfg->general->base); system(cmd);
      
      sprintf(cfg->aisp_master->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
    /* Coregister slave image */
    if (check_status(cfg->aisp_slave->status)) {
      if (strncmp(cfg->general->coreg, "FRAME", 5)==0) { 
	/* match the whole slave to master */
	if (cfg->general->deskew == 0) sprintf(options, "-debug 1 -c b.dop");
	if (cfg->general->deskew == 1) sprintf(options, "-debug 1 -e 1");
	if (cfg->aisp_slave->power == 1) strcat(options, " -power");
	check_return(aisp(options, cfg->aisp_slave->start_offset, 
			  cfg->aisp_slave->patches, "b", "b"), 
		     "processing slave image (aisp)");
	if (cfg->general->mflag) 
	  check_return(coregister_fine("a", "b", "reg/ctrl", "reg/fico", 
				       cfg->general->mask,
				       cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "offset estimation slave image (coregister_fine)");
	else 
	  check_return(coregister_fine("a", "b", "reg/ctrl", "reg/fico", NULL, 
				       cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "offset estimation slave image (coregister_fine)");
	sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
	
	/* Taking care of the overlap */
	fCorr = FOPEN("reg/ctrl", "r");
	fscanf(fCorr, "%d", &delta);
	fscanf(fCorr, "%d", &delta);
	FCLOSE(fCorr);
	if (delta > 0) {
	  cfg->aisp_master->patches = (int) 
	    ((cfg->aisp_master->end_offset-4096-delta)/AISP_VALID_PATCH_LENGTH) + 1;
	  cfg->aisp_slave->patches = cfg->aisp_master->patches;
	  cfg->aisp_master->start_offset = delta;
	}
	else {
	  cfg->aisp_slave->patches = (int) 
	    ((cfg->aisp_slave->end_offset-4096+delta)/AISP_VALID_PATCH_LENGTH) + 1;
	  cfg->aisp_master->patches = cfg->aisp_slave->patches;
	  cfg->aisp_slave->start_offset = -delta;
	}
	check_return(aisp(options, cfg->aisp_master->start_offset, 
			  cfg->aisp_master->patches, "a", "a"), 
		     "processing master image (aisp)");
	check_return(aisp(options, cfg->aisp_slave->start_offset, 
			  cfg->aisp_slave->patches, "b", "b"), 
		     "processing slave image (aisp)");
	
	if (cfg->aisp_master->power == 1) {
	  sprintf(cmd, "mv a_pwr.img %s_a_pwr.img", cfg->general->base); 
	  system(cmd);
	  sprintf(cmd, "mv a_pwr.meta %s_a_pwr.meta", cfg->general->base); 
	  system(cmd);
	  sprintf(cmd, "mv b_pwr.img %s_b_pwr.img", cfg->general->base); 
	  system(cmd);
	  sprintf(cmd, "mv b_pwr.meta %s_b_pwr.meta", cfg->general->base); 
	  system(cmd);
	}
	
	if (cfg->general->mflag) {
	  check_return(coregister_coarse("a", "b", "reg/ctrl", cfg->general->mask), 
		       "offset estimation slave image (coregister_coarse)");
	  check_return(coregister_fine("a", "b", "reg/ctrl", "reg/fico", 
				       cfg->general->mask, 
				       cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "fine coregistration slave image (coregister_fine)");
	}
	else {
	  check_return(coregister_coarse("a", "b", "reg/ctrl", NULL), 
		       "offset estimation slave image (coregister_coarse)");
	  check_return(coregister_fine("a", "b", "reg/ctrl", "reg/fico", NULL, 
				       cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "fine coregistration slave image (coregister_fine)");
	}
	check_return(fit_plane("reg/fico", "reg/matrix", 0.8), 
		     "calculate transformation parameters (fit_plane)");
	sprintf(tmp, "-matrix reg/matrix -sameSize");
	check_return(remap("b.cpx", "b_corr.cpx", tmp), 
		     "resampling of slave image (remap)");		  
      }
      
      else {
	check_return(calc_deltas("reg/line1", "reg/lineL", 
				 cfg->coreg_pL->start_master - 
				 cfg->coreg_p1->start_master, "reg/deltas"), 
		     "conversion of regression coefficients (calc_deltas)");	
	
	delta = cfg->coreg_p1->start_master - cfg->aisp_master->start_offset;
	if(delta != 0)
	  {
	    FILE *inFile;
	    double a, b, c, d;
	    double e, f, g, h;
	    
	    inFile = FOPEN("reg/deltas", "r");
	    fscanf(inFile, "%lf%lf%lf%lf", &a, &b, &c, &d);
	    fscanf(inFile, "%lf%lf%lf%lf", &e, &f, &g, &h);
	    FCLOSE(inFile);
	    inFile = FOPEN("reg/deltas", "w");
	    fprintf(inFile, "%e %e %e %e\n",
		    (a-(delta*e)),(b-(delta*f)),(c-(delta*g)),(d-(delta*h)));
	    fprintf(inFile, "%e %e %e %e\n", e, f, g, h);
	    FCLOSE(inFile); 
	  }
	
	if (cfg->aisp_slave->power < 0 || cfg->aisp_slave->power > 1) {
	  printf("\n   WARNING: power flag set to invalid value - "
		 "set to value of 1\n\n");
	  cfg->aisp_slave->power = 1;
	  check_return(write_config(configFile, cfg), 
		       "Could not update configuration file"); 
	}
	if (cfg->general->deskew == 0) 
	  sprintf(options, "-o reg/deltas -debug 1 -c b.dop");
	if (cfg->general->deskew == 1) 
	  sprintf(options, "-o reg/deltas -debug 1 -e 1");
	if (cfg->aisp_slave->power == 1) 
	  strcat(options, " -power");
	
	cfg->aisp_slave->start_offset = cfg->coreg_p1->start_slave;
	check_return(aisp(options, cfg->aisp_slave->start_offset, 
			  cfg->aisp_slave->patches, "b", "b_corr"), 
		     "processing slave image (aisp)");
      }
      if (cfg->aisp_slave->power == 1) {
	sprintf(cmd, "mv b_corr_pwr.img %s_b_pwr.img", cfg->general->base); 
	system(cmd);
	sprintf(cmd, "mv b_corr_pwr.meta %s_b_pwr.meta", cfg->general->base); 
	system(cmd);
      }
      
      sprintf(cfg->aisp_slave->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
  }
  
  if (datatype==2) {
    
    /* Ingest CEOS SLC data */
    if (check_status(cfg->trim_slc->status)) {
      if (!fileExists(cfg->master->data)) 
	check_return(1, "master image data file does not exist");
      if (!fileExists(cfg->master->meta)) 
	check_return(1, "master image metadata file does not exist");
      if (!fileExists(cfg->slave->data)) 
	check_return(1, "slave image data file does not exist");
      if (!fileExists(cfg->slave->meta)) 
	check_return(1, "slave image metadata file does not exist");
      
      if ((cfg->trim_slc->length == -99) || (cfg->trim_slc->width == -99)) { 
	struct IOF_VFDR vfdr1, vfdr2;
	
	get_ifiledr(cfg->master->data, &vfdr1);
	get_ifiledr(cfg->slave->data, &vfdr2);
	cfg->trim_slc->length = 
	  (vfdr1.linedata < vfdr2.linedata) ? vfdr1.linedata : vfdr2.linedata;
	cfg->trim_slc->width = 
	  (vfdr1.datgroup < vfdr2.datgroup) ? vfdr1.datgroup : vfdr2.datgroup;
      }
      check_return(trim_slc(cfg->master->data, "a", cfg->trim_slc->line, 
			    cfg->trim_slc->sample,
			    cfg->trim_slc->length, cfg->trim_slc->width), 
		   "ingesting master image (trim_slc)");
      check_return(trim_slc(cfg->slave->data, "b", cfg->trim_slc->line, 
			    cfg->trim_slc->sample,
			    cfg->trim_slc->length, cfg->trim_slc->width), 
		   "ingesting slave image (trim_slc)");
      check_return(c2p("a", "a"), 
		   "converting complex master image into phase and amplitude (c2p)");
      meta = meta_init("a.meta");
      check_return(convert2byte("a.amp", "a_amp.img", meta->ifm->nLooks, 1), 
		   "creating byte amplitude master image (convert2byte)");
      check_return(c2p("b", "b"), 
		   "converting complex slave image into phase and amplitude (c2p)");
      meta = meta_init("b.meta");
      check_return(convert2byte("b.amp", "b_amp.img", meta->ifm->nLooks, 1), 
		   "creating byte amplitude slave image (convert2byte)");
      sprintf(cfg->trim_slc->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
    /* Coregister slave image */
    if (check_status(cfg->coreg_slave->status)) {
      if (cfg->general->mflag) {
	check_return(coregister_coarse("a", "b", "reg/ctrl", cfg->general->mask), 
		     "offset estimation (coregister_coarse)");
	check_return(coregister_fine("a", "b", "reg/ctrl", "reg/fico", 
				     cfg->general->mask, 
				     cfg->coreg_slave->grid, cfg->coreg_slave->fft), 
		     "fine coregistration slave image (coregister_fine)");
      }
      else {
	check_return(coregister_coarse("a", "b", "reg/ctrl", NULL), 
		     "offset estimation (coregister_coarse)");
	check_return(coregister_fine("a", "b", "reg/ctrl", "reg/fico", NULL, 
				     cfg->coreg_slave->grid, cfg->coreg_slave->fft), 
		     "fine coregistration slave image (coregister_fine)");
      }
      sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
      if (cfg->coreg_slave->warp == 1) {
	check_return(fit_warp("reg/fico", "b", "reg/warp"), 
		     "calculating offset grids (fit_warp)");
	sprintf(tmp, "-warp reg/warp -sameSize");
	if (cfg->coreg_slave->sinc) strcat(tmp, " -sinc");
	check_return(remap("b.cpx", "b_corr.cpx", tmp), 
		     "resampling of slave image (remap)");		  
      }
      else {
	check_return(fit_plane("reg/fico", "reg/matrix", 0.8), 
		     "calculating transformation parameters (fit_plane)");
	sprintf(tmp, "-matrix reg/matrix -sameSize");
	if (cfg->coreg_slave->sinc) strcat(tmp, " -sinc");
	check_return(remap("b.cpx", "b_corr.cpx", tmp), 
		     "resampling of slave image (remap)");		  
      }
      sprintf(cmd, "mv a_amp.img %s_a_amp.img", cfg->general->base); system(cmd);
      sprintf(cmd, "mv a_amp.meta %s_a_amp.meta", cfg->general->base); system(cmd);
      sprintf(cmd, "rm b_amp.img b_amp.meta"); system(cmd);
      check_return(c2p("b_corr", "b_corr"), 
		   "converting complex slave image into phase and amplitude (c2p)");
      sprintf(tmp, "%s_b_amp.img", cfg->general->base);
      check_return(convert2byte("b_corr.amp", tmp, meta->ifm->nLooks, 1), 
		   "creating byte amplitude slave image (convert2byte)");
      sprintf(cfg->coreg_slave->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
  }
  
  /* Calculate the interferogram and coherence */
  if (check_status(cfg->igram_coh->status)) {
    check_return(igram("a", "b_corr", cfg->igram_coh->igram), 
		 "interferogram generation (igram)");
    
    if (cfg->igram_coh->min < 0.0 || cfg->igram_coh->min > 1.0) {
      printf("\n   WARNING: minimum average coherence out of range - "
	     "set to value of 0.3\n\n");
      cfg->igram_coh->min = 0.3;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    check_return(coh("a", "b_corr", cfg->igram_coh->coh), 
		 "generating coherence image (coh)");
    if (fCoh = FOPEN(logFile, "r")) {
      while (fgets(tmp, 255, fCoh) != NULL) {
	if (strncmp(tmp, "   Average Coherence:", 21)==0) 
	  sscanf(tmp, "%s %s %f", tmp, tmp, &avg);
      }
      if (avg < cfg->igram_coh->min) {
	sprintf(tmp, "average coherence level below minimum of %.1f", 
		cfg->igram_coh->min);
	check_return(1, tmp);
      }
      FCLOSE(fCoh);
    }
    
    if (cfg->igram_coh->ml < 0 || cfg->igram_coh->ml > 1) {
      printf("\n   WARNING: multilook flag set to invalid value - "
	     "set to value of 1\n\n");
      cfg->igram_coh->ml = 1;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    if (cfg->igram_coh->ml == 1) {
      sprintf(tmp, "%s_ml", cfg->igram_coh->igram);
      check_return(multilook(cfg->igram_coh->igram, tmp, "a.meta"), 
		   "multilooking interferogram (multilook)");
    }
    
    sprintf(cfg->igram_coh->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  sprintf(cmd, "cp a.meta %s.meta", cfg->igram_coh->igram);
  system(cmd);
  
  /* Refine the offset */
  if (check_status(cfg->offset_match->status)) { 
    i = 0;
    while (off) {
      meta = meta_init("a.meta");
      sprintf(tmp, "%s.amp", cfg->igram_coh->igram);
      check_return(convert2byte(tmp, "sar_byte", meta->ifm->nLooks, 1), 
		   "creating byte amplitude image (convert2byte)");
      strcat(strcpy(metaFile,"sar_byte"),".meta");
      /*** Temporary fix: create a DDR file for sar_byte **
	   sprintf(cmd, "meta2ddr sar_byte tmp");
	   system(cmd);
	   sprintf(cmd, "mv tmp.ddr sar_byte.ddr");
	   system(cmd);
      ****************************************************/
      nl = lzInt(metaFile, "general.line_count:", NULL);
      ns = lzInt(metaFile, "general.sample_count:", NULL);
      /* if (!fileExists(cfg->general->dem)) check_return(1, 
	 "reference DEM file does not exist"); */
      check_return(create_dem_grid(cfg->general->dem, "sar_byte.img", "a.meta", 
				   "dem_grid"), 
		   "creating a grid (create_dem_grid)");
      check_return(fit_plane("dem_grid", "dem_plane", (double) 1.0), 
		   "transformation parameters for resampling (fit_plane)");
      sprintf(tmp, "-matrix dem_plane -translate 0 0 -width %d -height %d "
	      "-bilinear -float", ns+500, nl);
      check_return(remap(cfg->general->dem, "dem_big.dem", tmp),
		   "creating subset of reference DEM (remap)");
      /**** Needs change **************  
	    check_return(make_ddr("dem_big.dem", nl, ns+500, "float"), 
	    "creating DDR file for subset DEM (makeddr)"); 
      ********************/
      check_return(reskew_dem("dem_big.dem", "a.meta", "dem_slant.ht", 
			      "dem_sim.amp"), 
		   "transformation subset of reference DEM into slant range "
		   "(reskew_dem)");
      check_return(convert2byte("dem_sim.amp", "dem_simbyte.img", 1, 1), 
		   "creating simulated byte amplitude image (convert2byte)");
      check_return(trim("dem_simbyte.img", "dem_trimsim.img", 0, 100, nl, ns), 
		   "re-sizing simulated byte amplitude (trim)");
      check_return(trim("dem_slant.ht", "dem_lined.ht", 0, 100, nl, ns), 
		   "re-sizing slant range subset DEM (trim)");
      check_return(fftMatch("sar_byte", "dem_trimsim.img", "dem_corr"), 
		   "matching real and simulated amplitude (fftMatch)");
      
      fCorr = FOPEN("dem_corr", "r");
      fscanf(fCorr, "%f %f", &xshift, &yshift);
      FCLOSE(fCorr);
      sprintf(tmp, "cp dem_corr offset.%d", i);
      system(tmp);
      i++;
      if (fabs(xshift)<cfg->offset_match->max && 
	  fabs(yshift)<cfg->offset_match->max) off = 0;
      meta = meta_init("a.meta");
      meta->geo->timeShift -= yshift * meta->geo->azPixTime;
      meta->geo->slantShift -= xshift * meta->geo->xPix;
      meta_write(meta, "a.meta");
    }
    
    sprintf(cfg->offset_match->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Simulated phase image and seed points */
  if (check_status(cfg->sim_phase->status)) { 
    check_return(dem2phase("dem_lined.ht", "a.meta", 
			   base2str(0, cfg->general->base), "out_dem_phase.phase"), 
		 "creating simulated phase (dem2phase)");
    check_return(dem2seeds("dem_lined.ht", "sar_byte.img" , 
			   cfg->sim_phase->seeds, 0), 
		 "creating seed points (dem2seeds)");
    
    sprintf(cfg->sim_phase->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Calculate differential interferogram */
  
  if (strncmp(cfg->general->mode, "DINSAR", 6)==0) {
    sprintf(tmp, "\'(a-b)%%6.2831853-3.14159265\' %s_ml.phase out_dem_phase.phase", 
	    cfg->igram_coh->igram);
    check_return(raster_calc("dinsar.phase", tmp), 
		 "calculating differential interferogram (raster_calc)");
    sprintf(cfg->dinsar->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
    exit(0);
  }
  
  /* Deramping and multilooking interferogram */
  if (check_status(cfg->deramp_ml->status)) {
    check_return(deramp(cfg->igram_coh->igram, "a.meta", 
			base2str(0, cfg->general->base), "igramd", 0), 
		 "deramping interferogram (deramp)");
    check_return(multilook("igramd", "ml", "a.meta"), 
		 "multilooking interferogram (multilook)");
    
    sprintf(cfg->deramp_ml->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Phase unwrapping */
  if (check_status(cfg->unwrap->status)) {
    if (cfg->unwrap->flattening < 0 || cfg->unwrap->flattening > 1) {
      printf("\n   WARNING: flattening flag set to invalid value - "
	     "set to value of 0\n\n");
      cfg->unwrap->flattening = 0;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    if (cfg->unwrap->flattening==1) {
      check_return(raster_calc("ml_dem.phase", "\'(a-b)%6.2831853-3.14159265\' "
			       "ml.phase out_dem_phase.phase"),
		   "subtracting terrain induced phase (raster_calc)");
    }
    
    if (strncmp(cfg->unwrap->algorithm, "escher", 6)==0) {
      if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem.phase");
      else sprintf(tmp, "ml.phase");
      if (cfg->unwrap->filter < 0.0 || cfg->unwrap->filter > 3.0) {
	printf("\n   WARNING: phase filter value out of range - "
	       "set to value of 1.6\n\n");
	cfg->unwrap->filter = 1.6;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      if (cfg->unwrap->filter>0.0) {
	check_return(phase_filter(tmp, (double) cfg->unwrap->filter, 
				  "filtered_phase"), 
		     "phase filtering (phase_filter)");
	sprintf(tmp, "filtered_phase");
      }
      if (strcmp(tmp, "ml.phase")!=0) {
	check_return(zeroify(tmp, "ml.phase", "escher_in.phase"), 
		     "phase value cosmetics (zeroify)");
	sprintf(tmp, "escher_in.phase");
      }
      if (cfg->unwrap->flattening==1) {
	check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	check_return(raster_calc("unwrap.phase", "\'(a+b)*(a/a)*(b/b)\' "
				 "unwrap_dem.phase out_dem_phase.phase"),
		     "adding terrain induced phase back (raster_calc)");
      }
      else
	check_return(escher(tmp,"unwrap"), "phase unwrapping (escher)");
      
      check_return(deramp("unwrap", "a.meta", base2str(0, cfg->general->base), 
			  "unwrap_nod", 1), 
		   "reramping unwrapped phase (deramp)");
      
      system("ln -s unwrap.meta unwrap_dem.phase.mask.meta");
      system("ln -s unwrap.meta unwrap_dem.phase.meta");
      check_return(convert2ppm("unwrap_dem.phase.mask", "unwrap_mask.ppm"), 
		   "colorized phase unwrapping mask (convert2ppm)");
    }
    
    if (strncmp(cfg->unwrap->algorithm, "snaphu", 6)==0) {
      
      if (cfg->general->test == 1) {
	if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem.phase");
	else sprintf(tmp, "ml.phase");
	if (cfg->unwrap->filter < 0.0 || cfg->unwrap->filter > 3.0) {
	  printf("\n   WARNING: phase filter value out of range - "
		 "set to value of 1.6\n\n");
	  cfg->unwrap->filter = 1.6;
	  check_return(write_config(configFile, cfg), 
		       "Could not update configuration file"); 
	}
	if (cfg->unwrap->filter>0.0) {
	  check_return(phase_filter(tmp, (double) cfg->unwrap->filter, 
				    "filtered_phase"), 
		       "phase filtering (phase_filter)");
	  sprintf(tmp, "filtered_phase");
	}
	if (strcmp(tmp, "ml.phase")!=0) {
	  check_return(zeroify(tmp, "ml.phase", "escher_in.phase"), 
		       "phase value cosmetics (zeroify)");
	  sprintf(tmp, "escher_in.phase");
	}
	if (cfg->unwrap->flattening==1) {
	  check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	  check_return(raster_calc("escher.phase", "\'(a+b)*(a/a)*(b/b)\' "
				   "unwrap_dem.phase out_dem_phase.phase"),
		       "adding terrain induced phase back (raster_calc)");
	}
	else
	  check_return(escher(tmp,"escher"), "phase unwrapping (escher)");
	
	check_return(deramp("escher", "a.meta", base2str(0, cfg->general->base), 
			    "escher_nod", 1), 
		     "reramping unwrapped phase (deramp)");
	
	system("ln -s escher.meta unwrap_dem.phase.mask.meta");
	system("ln -s escher.meta unwrap_dem.phase.meta");
	check_return(convert2ppm("unwrap_dem.phase.mask", "unwrap_mask.ppm"), 
		     "colorized phase unwrapping mask (convert2ppm)");
      }
      
      sprintf(cmd, "make_snaphu_conf %s.phase unwrap.phase", cfg->igram_coh->igram); 
      system(cmd);
      fLog = FOPEN(logFile, "a");
      sprintf(logbuf, "Command line: %s\n", cmd);
      printf("%s", logbuf);
      printLog(logbuf);
      FCLOSE(fLog);
      if (cfg->unwrap->tiles_azimuth == 0 || cfg->unwrap->tiles_range == 0) {
	meta = meta_init("a.meta");
	if (datatype==2) {
	  meta_get_latLon(meta, cfg->trim_slc->line, 1, 0, &lat1, &lon);
	  meta_get_latLon(meta, cfg->trim_slc->length, 1, 0, &lat2, &lon);
	}
	else {
	  meta_get_latLon(meta, cfg->aisp_master->start_offset, 1, 0, &lat1, &lon);
	  meta_get_latLon(meta, cfg->aisp_master->end_offset, 1, 0, &lat2, &lon);
	}
	cfg->unwrap->tiles_azimuth = 
	  (int) (fabs(lat1-lat2)*cfg->unwrap->tiles_per_degree);
	cfg->unwrap->tiles_range = (int) (fabs(cfg->unwrap->tiles_per_degree*0.8));
      }
      if (cfg->unwrap->flattening==1) 
	check_return(snaphu(cfg->unwrap->algorithm, "ml.phase", "ml.amp", 
			    cfg->aisp_master->power_img, cfg->aisp_slave->power_img, 
			    "snaphu.conf", "unwrap.phase",
			    cfg->unwrap->tiles_azimuth, cfg->unwrap->tiles_range, 
			    cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
			    cfg->unwrap->procs, 1), "phase unwrapping (snaphu)");
      else
	check_return(snaphu(cfg->unwrap->algorithm, "ml.phase", "ml.amp", 
			    cfg->aisp_master->power_img, cfg->aisp_slave->power_img, 
			    "snaphu.conf", "unwrap.phase", 
			    cfg->unwrap->tiles_azimuth, cfg->unwrap->tiles_range, 
			    cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
			    cfg->unwrap->procs, 0), "phase unwrapping (snaphu)");
      system("ln -s ml.meta unwrap.meta");
    }
    
    check_return(deramp("unwrap", "a.meta", base2str(0, cfg->general->base), 
			"unwrap_nod", 1), 
		 "reramping unwrapped phase (deramp)");
    
    sprintf(cfg->unwrap->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Baseline refinement */
  if (check_status(cfg->refine->status)) {
    if (cfg->refine->max < 1 || cfg->refine->max > 15) {
      printf("\n   WARNING: maximum number of iterations out of range - "
	     "set to value of 15\n\n");
      cfg->refine->max = 15;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    newBase = base2str(0, cfg->general->base);
    for (i=0; i<cfg->refine->max; i++) 
      {
	veryoldBase = oldBase;
	oldBase = newBase;
	newBase = base2str(i+1, cfg->general->base);
	
	check_return(refine_base("unwrap_nod.phase", 
				 cfg->sim_phase->seeds, "a.meta", oldBase, newBase), 
		     "baseline refinement (refine_base)");
	if (i>0)
	  if (check_refinement(newBase,oldBase,veryoldBase)) break;
      }
    if (i==cfg->refine->max) 
      check_return(1, "Baseline iterations failed to converge");
    cfg->refine->iter = i+1; 
    
    check_return(deramp(cfg->igram_coh->igram, "a.meta", 
			base2str(cfg->refine->iter, cfg->general->base), 
			"igramd", 0), 
		 "deramping interferogram with refined baseline (deramp)");
    sprintf(tmp, "%s_ml", cfg->igram_coh->igram);
    check_return(multilook("igramd", tmp, "a.meta"), 
		 "multilooking refined interferogram (multilook)");
    
    sprintf(cfg->refine->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }       
  
  /* Elevation and elevation error */
  if (check_status(cfg->elevation->status)) {
    newBase = base2str(cfg->refine->iter, cfg->general->base);
    check_return(deramp("unwrap_nod", "a.meta", newBase, "unwrap", 0), 
		 "deramping unwrapped phase with refined baseline (deramp)");
    sprintf(cmd, "diff_las -d %s unwrap.phase out_dem_phase.phase", cfg->unwrap->qc);
    system(cmd);
    check_return(elev("unwrap.phase", newBase, "a.meta", cfg->elevation->dem, 
		      cfg->sim_phase->seeds), 
		 "creating elevation map (elev)");
    if (strcmp(cfg->unwrap->algorithm, "escher")==0) {
      check_return(eleverr(cfg->igram_coh->coh, newBase, "a.meta", 
			   "unwrap_dem.phase.mask" , cfg->elevation->error), 
		   "elevation error estimate (eleverr)");
    }
    else
      check_return(eleverr(cfg->igram_coh->coh, newBase, "a.meta", NULL, 
			   cfg->elevation->error), 
		   "elevation error estimate (eleverr)");
    
    sprintf(cfg->elevation->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Remapping to ground range */
  if (check_status(cfg->ground_range->status)) {
    check_return(deskew_dem(cfg->elevation->dem, "a.meta", "elevation.dem", "", 1), 
		 "remapping elevation to ground range DEM (deskew_dem)");
    check_return(deskew_dem("elevation.dem", "a.meta", "amplitude_float.img", 
			    "a_amp.img", 1), 
		 "remapping amplitude to ground range (deskew_dem)");
    check_return(deskew_dem("elevation.dem", "a.meta", "error.img", 
			    cfg->elevation->error, 1), 
		 "remapping error map to ground range (deskew_dem)");
    check_return(convert2byte("amplitude_float.img", "amplitude.img", 1, 2),
		 "converting ground range amplitude to byte (convert2byte)");
    check_return(deskew_dem("elevation.dem", "a.meta", "coh_gr.img", "coh.img", 0), 
		 "remapping coherence to ground range (deskew_dem)");
    
    sprintf(cfg->ground_range->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Geocoding */
  if (check_status(cfg->geocode->status)) {
    if (cfg->geocode->pix_spacing < 5 || cfg->geocode->pix_spacing > 500) {
      printf("\n   WARNING: pixel spacing out of range - "
	     "set to default value of 20\n\n");
      cfg->geocode->pix_spacing = 20;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    if (!fileExists(cfg->geocode->proj)) 
      check_return(1, "projection file does not exist");
    sprintf(tmp, "grep -c %s %s > /dev/null", cfg->geocode->key, cfg->geocode->proj);
    if (system(tmp)) check_return(1, "projection key not defined");
    check_return(geocode("elevation.meta", "elevation.dem", cfg->geocode->proj, 
			 cfg->geocode->key,
			 cfg->geocode->pix_spacing, cfg->geocode->dem), 
		 "geocoding ground range DEM (geocode)");      
    check_return(geocode("elevation.meta", "amplitude.img", cfg->geocode->proj, 
			 cfg->geocode->key, 
			 cfg->geocode->pix_spacing, cfg->geocode->amp), 
		 "geocoding ground range amplitude (geocode)");
    check_return(geocode("elevation.meta", "error.img", cfg->geocode->proj, 
			 cfg->geocode->key, 
			 cfg->geocode->pix_spacing, cfg->geocode->error), 
		 "geocoding ground range error map (geocode)");
    check_return(geocode("elevation.meta", "coh_gr.img", cfg->geocode->proj, 
			 cfg->geocode->key, 
			 cfg->geocode->pix_spacing, cfg->geocode->coh), 
		 "geocoding ground range coherence (geocode)");
    
    sprintf(cfg->geocode->status, "success");
  }   
  
  
  sprintf(cfg->general->status, "success");
  check_return(write_config(configFile, cfg), "Could not update configuration file");
  
  return(0);
}

