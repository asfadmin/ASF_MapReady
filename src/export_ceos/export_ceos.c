/********************************************************************************
NAME:
	export_ceos.c

SYNOPSIS:

DESCRIPTION:
	This program ingests level one CEOS data, geocodes and resamples them 
        (both optional) and exports them to a variety of output formats.

FILE REFERENCES:
	NAME:		USAGE:
	---------------------------------------------------------------------
	.config		configuration file for all input files

PROGRAM HISTORY:
	VERS:   DATE:   AUTHOR:
	----------------------------------------------------------------------
	1.0	10/02	R. Gens, original development
	1.1	10/03	R. Gens, extended output formats
        1.2     05/04   R. Gens, overhaul for new metadata, added resampling
        1.3     05/04   R. Gens, added batch mode

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	There are no known bugs.

ASSOCIATE PROGRAMS:

*********************************************************************************/

#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "export_ceos.h"
#include "functions.h"
#include "proj.h"
#include <unistd.h>

#define VERSION 1.3

void usage(char *name)
{	
  printf("\n    Usage: export_ceos [-c] <configuration file>\n\n");
  printf("           -c	creates only the configuration file and exits.\n\n");
  printf("    export_ceos will ...\n");
  printf(" %1.2f, ASF Software Tools, 2004\n\n", VERSION);
  exit(1);
}

void check_return(int ret, char *msg)
{
  if (ret!=0) {
    sprintf(errbuf, "\n   ERROR: %s\n\n", msg);
    printErr(errbuf);
  }
}

main(int argc, char *argv[])
{
  FILE *fBatch;
  s_config *cfg;
  char configFile[255], cmd[255], options[255], in[25], out[25];
  char proj[25], line[255], fileName[255], batchConfig[255];
  int cFlag=0, id;
  
  if (argc<2 || argc>3) usage("export_ceos");
  if (strcmp(argv[1], "-c")==0) {
    cFlag = 1;
    strcpy(configFile, argv[2]);
  }
  else strcpy(configFile, argv[1]);
  
  if (argc == 3) printf("\nCommand line: export_ceos -c %s", configFile);
  else printf("\nCommand line: export_ceos %s", configFile);
  printf("\nDate: ");
  system("date");
  printf("Program: export_ceos\n\n");
  
  logflag=quietflag=1;
  
  /* Read configuration file */
  if (!fileExists(configFile)) {
    check_return(init_config(configFile), 
		 "basic configuration file could not be initialized");
    exit(0);
  }
  else if (check_geocode_flag(configFile) && cFlag) {
    check_return(init_projection_config(configFile),
		 "extended geocoding configuration file could not be initialized");
    exit(0);
  }
  else if (check_resample_flag(configFile) && cFlag) {
    check_return(init_resample_config(configFile),
		 "extended resampling configuration file could not be initialized");
    exit(0);
  }
  else cfg = read_config(configFile, cFlag);
  sprintf(logFile, "%s", cfg->general->logFile);

  /* Batch mode processing */
  if (cfg->general->batch) {
    cfg->general->batch = 0;
    logflag = 0;
    fBatch = FOPEN(cfg->general->batchFile, "r");
    while (fgets(line, 255, fBatch) != NULL) {
      sscanf(line, "%s", fileName);
      sprintf(cfg->general->in_data_name, "%s.D", fileName);
      sprintf(cfg->general->in_meta_name, "%s.L", fileName);
      strcpy(cfg->general->out_name, fileName);
      sprintf(batchConfig, "%s.config", fileName);
      check_return(write_config(batchConfig, cfg),
		   "Could not write individual configuration file "
		   "for batch mode processing");
      check_return(export_ceos(batchConfig),
		   "Processing image in batch mode (export_ceos)");
    }
  }
  else {
 
    /* Prepare processing */
    if (strncmp(cfg->general->status, "new", 3)==0 && !cFlag) {
      if (!fileExists(cfg->general->in_data_name)) 
	check_return(1, "input data file does not exist");
      if (!fileExists(cfg->general->in_meta_name)) 
	check_return(1, "input metadata file does not exist");
    }
    
    /* Update configuration file */
    if (!cFlag) sprintf(cfg->general->status, "progress");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
    
    if (cFlag) {
      printf("   Initialized configuration file\n\n");
      if (logflag) {
	fLog = FOPEN(logFile, "a");
	printLog("   Initialized configuration file\n\n");
	FCLOSE(fLog);
      } 
      exit(0);
    }
    
    /* Get process ID. */
    id = (int)getpid();
    
    /* Ingest CEOS image */
    sprintf(in, "tmp%d", id);
    if (strncmp(cfg->general->data_type, "amplitude", 9)==0) {
      check_return(import2asf(cfg->general->in_data_name, 
			      cfg->general->in_meta_name, "-amplitude", in),
		   "Importing CEOS data (import2asf)");
      sprintf(out, "tmp%d_amp", id);
    }
    else if (strncmp(cfg->general->data_type, "power", 5)==0) {
      check_return(import2asf(cfg->general->in_data_name, 
			      cfg->general->in_meta_name, "-power", in),
		   "Importing CEOS data (import2asf)");
      sprintf(out, "tmp%d_power", id);
    }
    else if (strncmp(cfg->general->data_type, "sigma", 5)==0) {
      check_return(import2asf(cfg->general->in_data_name, 
			      cfg->general->in_meta_name, "-sigma", in),
		   "Importing CEOS data (import2asf)");
      sprintf(out, "tmp%d_sigma", id);
    }
    else if (strncmp(cfg->general->data_type, "gamma", 5)==0) {
      check_return(import2asf(cfg->general->in_data_name, 
			      cfg->general->in_meta_name, "-gamma", in),
		   "Importing CEOS data (import2asf)");
      sprintf(out, "tmp%d_gamma", id);
    }
    else if (strncmp(cfg->general->data_type, "beta", 4)==0) {
      check_return(import2asf(cfg->general->in_data_name, 
			      cfg->general->in_meta_name, "-beta", in),
		   "Importing CEOS data (import2asf)");
      sprintf(out, "tmp%d_beta", id);
    }
    sprintf(cmd, "cp %s.meta %s.meta", out, cfg->general->out_name);
    system(cmd);
  
    /* Geocoding */
    if (cfg->general->geocoding) {
      
      /* Creating projection parameter file */
      sprintf(proj, "tmp%d.proj", id);
      
      /*** Polar Stereographic ***/
      if (strncmp(uc(cfg->geocoding->projection), "POLAR", 5)==0) {
	sprintf(options, "-l %lf -p %lf -g %s -d %d", 
		cfg->polar->center_lon, cfg->polar->center_lat, 
		cfg->polar->units, cfg->polar->datum);
	check_return(projprm("plstereo", "key", proj, options), 
		     "generating projection parameter file (projprm)");
      }
      /*** Universal Transverse Mercator ***/
      if (strncmp(uc(cfg->geocoding->projection), "UTM", 3)==0) {
	sprintf(options, "-d %i -z %i", cfg->utm->datum, cfg->utm->zone);
	check_return(projprm("utm", "key", proj, options), 
		     "generating projection parameter file (projprm)");
      }
      
      /*** Albers Conic Equal Area ***/
      if (strncmp(uc(cfg->geocoding->projection), "ALBERS", 6)==0) {
	sprintf(options, "-a %lf -b %lf -c %lf -g %s -d %d",
		cfg->albers->first_parallel, cfg->albers->second_parallel, 
		cfg->albers->center_meridian, cfg->albers->units, 
		cfg->albers->datum);
	check_return(projprm("albers", "key", proj, options), 
		     "generating projection parameter file (projprm)");
      }
      
      /*** Lambert Conformal Conic ***
	   if (strncmp(uc(cfg->geocoding->projection), "LAMBERT_CC", )==0) { 
	   sprintf(options, "-x %lf -y %lf -g %s -d %d",
	   cfg->lambert1->latitude, cfg->lambert1->longitude, 
	   cfg->lambert1->units, cfg->lambert1->datum);
	   check_return(projprm("lambert", "key", proj, options), 
	   "generating projection parameter file (projprm)");
	   } ***/
      
      /*** Lambert Azimuthal Equal Area ***
	   if (strncmp(uc(cfg->geocoding->projection), "LAMBERT2", 8)==0) { 
	   *** still needs to be figured out ***
	   } ***/
      
      sprintf(in, "%s", out);
      sprintf(out, "tmp%d_geo", id);
      check_return(geocode(in, proj, "key", cfg->geocoding->pixel, 
			   cfg->geocoding->height, out), 
		   "geocoding image (geocode)");
    }
    
    /* Determine the corner coordinates of the image */
    if (cfg->general->browse) {
      check_return(corner_coords(out), 
		   "determining geographic coordinates of corner points "
		   "(corner_coords)");
      sprintf(cmd, "mv %s.corners %s.corners", out, cfg->general->out_name);
      system(cmd);
    }
    
    /* Resampling */
    if (cfg->general->resample || cfg->general->browse) {
      sprintf(in, "%s", out);
      sprintf(out, "tmp%d_small");
      if (cfg->general->browse) 
	sprintf(options, "-browse");
      else 
	sprintf(options, "-resample %d", cfg->resampling->kernel);
      check_return(filter(options, in, out),
		   "subsampling image (filter)");
    }
    
    /* Exporting image */
    if (strncmp(cfg->general->out_format, "JPEG", 4)==0) {
      sprintf(in, "%s.img", out);
      sprintf(out, "tmp%s_byte");
      check_return(stats(in, 0.01), 
		   "recalculating stats for trimmed histogram (stats)");
      check_return(convert2byte(in, out),
		   "converting image by byte (convert2byte)");
      check_return(convert2jpeg(out, cfg->general->out_name), 
		   "exporting image (convert2jpeg)");
    }
    else if (strncmp(cfg->general->out_format, "GEOTIFF", 7)==0)
      check_return(convert2geotiff(out, cfg->general->out_name), 
		   "exporting image (convert2geotiff)");
    
    
    /* Remove temporary files */
    sprintf(cmd, "rm -f tmp%d*", id);
    system(cmd);
    
    sprintf(cfg->general->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  return(0);
}


