/********************************************************************************
NAME:
	create_dem.c

SYNOPSIS:

DESCRIPTION:
	This program processes lever zero swath data to a DEM

FILE REFERENCES:
	NAME:		USAGE:
	---------------------------------------------------------------------
	.config		configuration file for all input files

PROGRAM HISTORY:
	VERS:   DATE:   AUTHOR:
	----------------------------------------------------------------------
	1.0	8/01	R. Gens, original development
	1.1	2/02	R. Gens, added ability to read CEOS raw and SLC data
	1.2	2/03	R. Gens, adapted processing flow for updated Doppler processing
	1.3     2/03    P. Denny, Update command line parsing, use new meta structures

HARDWARE/SOFTWARE LIMITATIONS:
	The maximum number of processors is hard-coded at 8.

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	There are no known bugs.

ASSOCIATE PROGRAMS:
	lz2raw_flywheel(1)
	avg_in_dop(1)
	aisp(1)
	paisp(1)
	resolve(1)
	fico(1)
	fit_line(1)
	calc_deltas(1)
	igram(1)
	pigram(1)
	coh(1)
	pcoh(1)
	ml(1)
	pml(1)
	amp2img(1)
	create_dem_grid(1)
	remap(1)
	fit_plane(1)
	makeddr(1)
	reskew_dem(1)
	fftMatch(1)
	trim(1)

*********************************************************************************/

#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "create_dem.h"
#include "functions.h"
#include "lzFetch.h"
#include "proj.h"

#define VERSION 1.1

/* prototype */
int check_refinement(char *base1, char *base2, char *base3);


void usage(char *name)
{	
 printf("\n"
	"USAGE:\n"
	"   %s [-c] <configuration file>\n", name);
 printf("\n"
	"REQUIRED ARGUMENT:\n"
	"   configuration file   File that tells this program what to do.\n");
 printf("\n"
	"OPTIONAL ARGUMENT:\n"
	"   -c   Only create the configuration file and exit.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Runs the complete SAR interferometric processing chain, from ingesting until\n"
	"   the level zero STF data the creation and geocoding of the digital elevation\n"
	"   model. All the input parameters for the programs are read in from a\n"
	"   configuration file.\n");
 printf("\n"
	"Version %1.2f, ASF InSAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}

void check_return(int ret, char *msg)
{
	if (ret!=0) {
	  sprintf(errbuf, "\n   ERROR: %s\n\n", msg);
	  printErr(errbuf);
	}
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

int main(int argc, char *argv[])
{

	dem_config *cfg;
	meta_parameters *meta=NULL;
	struct DDR ddr;
	FILE *fCorr, *fCoh;
	char configFile[255], cmd[255], path[255], data[255], metadata[255], tmp[255], options[255], metaFile[10];
	char *veryoldBase=NULL, *oldBase=NULL, *newBase=NULL;
	int i, delta, cFlag=0, datatype=0, off=1;
	float xshift, yshift, avg;
	double lat1=0.0, lat2=0.0, lon;
	extern int currArg; /* pre-initialized to 1 in asf.a */

	/* Parse command line args */
	while (currArg < (argc-1)) {
		char *key=argv[currArg++];
		if (strmatch(key,"-c")) {
			cFlag = 1;
		}
		else {printf("\n*****Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 1) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	strcpy(configFile, argv[currArg]);
	if (strcmp(configFile,"-c")==0) {printf("Configuration file name needed.\n"); usage(argv[0]);}


	StartWatch();
	if (cFlag) printf("\nCommand line: create_dem -c %s", configFile);
	else printf("\nCommand line: create_dem %s", configFile);
	printf("\nDate: ");
	system("date");
	printf("Program: create_dem\n\n");

	logflag=quietflag=0;
		
	/* Read configuration file */
	if (!fileExists(configFile)) check_return(1, "configuration file does not exist");
	cfg = read_config(configFile, cFlag);

	/* Check for logfile and quiet switch */
	if (cfg->general->log < 0 || cfg->general->log > 1) {
	  printf("\n   WARNING: log file flag set to invalid value - set to value of 1\n\n");
	  cfg->general->log = 1;
	  check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	}
	if (cfg->general->log == 1) {
	  logflag = 1;
	  sprintf(logFile, "%s.log", cfg->general->base);
	  if (strncmp(cfg->general->status, "new", 3)==0) fLog = FOPEN(logFile, "w");
	  else fLog = FOPEN(logFile, "a");
	  if (argc == 3) {
	    sprintf(logbuf, "\nCommand line: create_dem -c %s\n", configFile); printLog(logbuf); }
	  else {
	    sprintf(logbuf, "\nCommand line: create_dem %s\n", configFile); printLog(logbuf); }
	  StartWatchLog(fLog);
	  sprintf(logbuf, "Program: create_dem\n\n"); printLog(logbuf);
	  FCLOSE(fLog);
	}
	if (cfg->general->quiet < 0 || cfg->general->quiet > 1) {
	  printf("\n   WARNING: quiet flag set to invalid value - set to value of 1\n\n");
	  cfg->general->quiet = 1;
	  check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	}
	if (cfg->general->quiet == 1) quietflag = 1; 

	/* Check the number of processors */
	if (cfg->general->procs < 1 || cfg->general->procs > 8) {
	  printf("\n   WARNING: number of processors out of range - set to maximum value of 8\n\n");
	  cfg->general->procs = 8;
	  check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	}

	/* Names for the results to keep */
	if (strncmp(cfg->general->status, "new", 3)==0) {
	  sprintf(cfg->igram_coh->igram, "%s_igram", cfg->general->base);
	  sprintf(cfg->igram_coh->coh, "%s_coh.img", cfg->general->base);
	  sprintf(cfg->aisp_master->power_img, "%s_a_pwr.img", cfg->general->base);
	  sprintf(cfg->aisp_slave->power_img, "%s_b_pwr.img", cfg->general->base);
	  sprintf(cfg->sim_phase->seeds, "%s.seeds", cfg->general->base);
	  sprintf(cfg->elevation->dem, "%s_ht.img", cfg->general->base);
	  sprintf(cfg->elevation->error, "%s_err_ht.img", cfg->general->base);
	  sprintf(cfg->geocode->dem, "%s_dem.img", cfg->general->base);
	  sprintf(cfg->geocode->amp, "%s_amp.img", cfg->general->base);
	  sprintf(cfg->geocode->error, "%s_error.img", cfg->general->base);
	  sprintf(cfg->geocode->coh, "%s_coh.img", cfg->general->base);
	  check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	}

	/* Prepare processing */
	if (strcmp(cfg->master->data, cfg->slave->data)==0)
	  check_return(1, "master and slave image data file have the same name");
	if (strcmp(cfg->master->meta, cfg->slave->meta)==0)
	  check_return(1, "master and slave image metadata file have the same name");
	sscanf(cfg->master->path, "%s", path);
	sscanf(cfg->master->data, "%s", data);
	sscanf(cfg->master->meta, "%s", metadata);
	if (strncmp(cfg->general->status, "new", 3)==0 && !cFlag) {
	  sprintf(tmp, "%s/%s", path, data);
	  if (!fileExists(tmp)) check_return(1, "master image data file does not exist");
	  sprintf(cmd, "ln -s %s/%s .", path, data); system(cmd);
	  sprintf(tmp, "%s/%s", path, metadata);
	  if (!fileExists(tmp)) check_return(1, "master image metadata file does not exist");
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
	  if (!fileExists(tmp)) check_return(1, "slave image metadata file does not exist");
	  sprintf(cmd, "ln -s %s/%s .", path, metadata); system(cmd);
	  system("mkdir reg");
	}

	/* Update configuration file */
	if (!cFlag) sprintf(cfg->general->status, "progress");
	check_return(write_config(configFile, cfg), "Could not update configuration file");

	if (cFlag) {
	  printf("   Initialized configuration file\n\n");
	  if (logflag) {
	    fLog = FOPEN(logFile, "a");
	    printLog("   Initialized configuration file\n\n");
	    FCLOSE(fLog);
	  } 
	  exit(0);
	}
	
	/* Check the data type */
	printf("   Data type: %s\n", cfg->general->data_type);
  	if (logflag) {
	  fLog = FOPEN(logFile, "a");
	  sprintf(logbuf, "   Data type: %s\n", cfg->general->data_type);
	  printLog(logbuf);
	  FCLOSE(fLog);
  	} 
	if (strncmp(uc(cfg->general->data_type), "STF", 3)==0) datatype = 0;
	if (strncmp(uc(cfg->general->data_type), "RAW", 3)==0) datatype = 1;
	if (strncmp(uc(cfg->general->data_type), "SLC", 3)==0) datatype = 2;

	/* Ingest the various data types: STF, RAW, or SLC */
	if (datatype==0) {

		/* Ingest of level zero STF data */
		if (strncmp(cfg->lz2raw->status, "new", 3)==0) {
		  if (cfg->general->lat_begin < -90.0 || cfg->general->lat_begin > 90.0) {
		    cfg->general->lat_begin = -99.0;
		    cfg->general->lat_end = 99.0;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  if (cfg->general->lat_end < -90.0 || cfg->general->lat_end > 90.0) {
		    cfg->general->lat_begin = -99.0;
		    cfg->general->lat_end = 99.0;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  if (!fileExists(cfg->master->data)) check_return(1, "master image data file does not exist");
		  if (!fileExists(cfg->master->meta)) check_return(1, "master image metadata file does not exist");
		  check_return(lz2raw_flywheel(cfg->master->data, "a", cfg->lz2raw->prc_master, cfg->lz2raw->prcFlag, 
				cfg->general->lat_begin, cfg->general->lat_end), "ingesting master image(lz2raw_flywheel)");
		  if (!fileExists(cfg->slave->data)) check_return(1, "slave image data file does not exist");
		  if (!fileExists(cfg->slave->meta)) check_return(1, "slave image metadata file does not exist");
		  check_return(lz2raw_flywheel(cfg->slave->data, "b", cfg->lz2raw->prc_slave, cfg->lz2raw->prcFlag, 
				cfg->general->lat_begin, cfg->general->lat_end), "ingesting slave image (lz2raw_flywheel)");

		  strcat(strcpy(metaFile,"a"),".meta");
		  cfg->aisp_master->end_offset = lzInt(metaFile, "sar.original_line_count:", NULL);
		  cfg->aisp_master->patches = (int) ((cfg->aisp_master->end_offset-4096)/AISP_VALID_PATCH_LENGTH) + 2;
		  strcat(strcpy(metaFile,"b"),".meta");
		  cfg->aisp_slave->end_offset = lzInt(metaFile, "sar.original_line_count:", NULL);

		  /* temporary fix */
		  if (cfg->aisp_slave->end_offset > cfg->aisp_master->end_offset)
		    cfg->aisp_slave->end_offset = cfg->aisp_master->end_offset;
		  else
		    cfg->aisp_master->end_offset = cfg->aisp_slave->end_offset;

		  cfg->aisp_slave->patches = cfg->aisp_master->patches;
		  cfg->coreg_pL->start_master = cfg->aisp_master->end_offset - 4096;
		  cfg->coreg_pL->start_slave = cfg->aisp_slave->end_offset - 4096;

		  sprintf(cfg->lz2raw->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}

	}

	if (datatype==1) {

		/* Ingest of CEOS raw data */
		if (strncmp(cfg->ceos2raw->status, "new", 3)==0) {
		  if (!fileExists(cfg->master->data)) check_return(1, "master image data file does not exist");
		  if (!fileExists(cfg->master->meta)) check_return(1, "master image metadata file does not exist");
		  check_return(ceos2raw(cfg->master->data, "a"), "ingesting master image (ceos2raw)");
		  if (!fileExists(cfg->slave->data)) check_return(1, "slave image data file does not exist");
		  if (!fileExists(cfg->slave->meta)) check_return(1, "slave image metadata file does not exist");
		  check_return(ceos2raw(cfg->slave->data, "b"), "ingesting slave image (ceos2raw)");

		  /* Setting patches and offsets for processing */
		  strcat(strcpy(metaFile,"a"),".meta");
                  cfg->aisp_master->end_offset = lzInt(metaFile, "sar.original_line_count:", NULL);
	          cfg->aisp_master->patches = (int) ((cfg->aisp_master->end_offset-4096)/AISP_VALID_PATCH_LENGTH) + 2;
                  strcat(strcpy(metaFile,"b"),".meta");
                  cfg->aisp_slave->end_offset = lzInt(metaFile, "sar.original_line_count:", NULL);

                  if (cfg->aisp_slave->end_offset > cfg->aisp_master->end_offset)
                    cfg->aisp_slave->end_offset = cfg->aisp_master->end_offset;
                  else
                    cfg->aisp_master->end_offset = cfg->aisp_slave->end_offset;

	          cfg->aisp_slave->patches = cfg->aisp_master->patches;
                  cfg->coreg_pL->start_master = cfg->aisp_master->end_offset - 4096;
                  cfg->coreg_pL->start_slave = cfg->aisp_slave->end_offset - 4096;

		  sprintf(cfg->ceos2raw->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}
	}

	if (datatype<2) {

		/* Calculate average Doppler */
/*		if (strncmp(cfg->aisp_master->doppler, "average", 7)==0) {*/
	          if (strncmp(cfg->avg_in_dop->status, "new", 3)==0) {
	            check_return(avg_in_dop("a", "b", "reg/avedop"), "calculating the average Doppler (avg_in_dop)");
		    system("cp reg/avedop a.dop");
		    system("cp reg/avedop b.dop");
	            sprintf(cfg->avg_in_dop->status, "success");
	            check_return(write_config(configFile, cfg), "Could not update configuration file");
	          }
/*		}*/

		/* Coregister first patch */
		if ((strncmp(cfg->coreg_p1->status, "new", 3)==0) &&
			(strncmp(cfg->general->coreg, "AUTOMATIC", 9)==0)) {
		  check_return(aisp("-d 1 -c reg/avedop ", cfg->coreg_p1->start_master, 1 , "a", "reg/a_p1"), 
					"processing first patch of master image (aisp)");
		  check_return(aisp("-d 1 -c reg/avedop ", cfg->coreg_p1->start_slave, 1 , "b", "reg/b_p1"), 
					"processing first patch of slave image (aisp)");
		  check_return(resolve("reg/a_p1", "reg/b_p1", "reg/ctrl1"), "offset estimation first patch (resolve)");
		  if (cfg->coreg_p1->grid < 20 || cfg->coreg_p1->grid > 200) {
		    printf("\n   WARNING: grid size out of range - set to default value of 20\n\n");
		    cfg->coreg_p1->grid = 20;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
		}
		if (strncmp(cfg->coreg_p1->status, "new", 3)==0) {
		  if (cfg->coreg_p1->fft < 0 || cfg->coreg_p1->fft > 1) {
		    printf("\n   WARNING: FFT flag set to invalid value - set to value of 1\n\n");
		    cfg->coreg_p1->fft = 1;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  check_return(fico("reg/a_p1", "reg/b_p1", "reg/ctrl1", "reg/fico1", cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
					"fine coregistration first patch (fico)");
		  check_return(fit_line("reg/fico1", "reg/line1"), "fit regression line first patch (fit_line)");

		  sprintf(cfg->coreg_p1->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}
	
		/* Coregister last patch */
		if ((strncmp(cfg->coreg_pL->status, "new", 3)==0) &&
			(strncmp(cfg->general->coreg, "AUTOMATIC", 9)==0)) {
		  FILE *inFile;

		  check_return(aisp("-d 1 -c reg/avedop ", cfg->coreg_pL->start_master, 1 , "a", "reg/a_pL"), 
					"processing last patch of master image (aisp)");
		  check_return(aisp("-d 1 -c reg/avedop ", cfg->coreg_pL->start_slave, 1 , "b", "reg/b_pL"), 
					"processing last patch of master image (aisp)");
		  check_return(resolve("reg/a_pL", "reg/b_pL", "reg/ctrlL"), "offset estimation last patch (resolve)");
		  inFile = FOPEN("reg/ctrl1", "r");
		  fscanf(inFile, "%d%d", &cfg->coreg_p1->off_rng, &cfg->coreg_p1->off_az);
		  FCLOSE(inFile);
		  inFile = FOPEN("reg/ctrlL", "r");
		  fscanf(inFile, "%d%d", &cfg->coreg_pL->off_rng, &cfg->coreg_pL->off_az);
		  FCLOSE(inFile);
		  if (cfg->coreg_pL->grid < 20 || cfg->coreg_pL->grid > 200) {
		    printf("\n   WARNING: grid size out of range - set to default value of 20\n\n");
		    cfg->coreg_pL->grid = 20;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		}

		if (strncmp(cfg->coreg_pL->status, "new", 3)==0) {
		  if ((fabs(cfg->coreg_p1->off_rng - cfg->coreg_pL->off_rng) > cfg->general->max_off) ||
			(fabs(cfg->coreg_p1->off_az - cfg->coreg_pL->off_az) > cfg->general->max_off)) {
		    printf("   WARNING: estimated offset for first and last patch differs more than %d pixels\n",
				cfg->general->max_off);
		    printf("   Processing terminated to allow manual offset estimation\n\n");
		    strcpy(cfg->general->coreg, "MANUAL");
		    check_return(write_config(configFile, cfg), "Could not update configuration file");
		    exit(1);
		  }
		}
		
		if ((strncmp(cfg->coreg_pL->status, "new", 3)==0) &&
			(strncmp(cfg->general->coreg, "MANUAL", 6)==0)) {
		  FILE *inFile;

		  inFile = FOPEN("reg/ctrl1", "w");
		  fprintf(inFile, "%d\n%d\n32\n1\n4.100000\n6.100000\n", cfg->coreg_p1->off_rng, cfg->coreg_p1->off_az);
		  FCLOSE(inFile);
		  inFile = FOPEN("reg/ctrlL", "w");
		  fprintf(inFile, "%d\n%d\n32\n1\n4.100000\n6.100000\n", cfg->coreg_pL->off_rng, cfg->coreg_pL->off_az);
		  FCLOSE(inFile);
		}

		if (strncmp(cfg->coreg_pL->status, "new", 3)==0) {
		    if (cfg->coreg_pL->fft < 0 || cfg->coreg_pL->fft > 1) {
		    printf("\n   WARNING: FFT flag set to invalid value - set to value of 1\n\n");
		    cfg->coreg_pL->fft = 1;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  check_return(fico("reg/a_p1", "reg/b_p1", "reg/ctrl1", "reg/fico1", cfg->coreg_p1->grid, 
				cfg->coreg_p1->fft), "fine coregistration first patch (fico)");
		  check_return(fit_line("reg/fico1", "reg/line1"), "fit regression line first patch (fit_line)");
		  check_return(fico("reg/a_pL", "reg/b_pL", "reg/ctrlL", "reg/ficoL", cfg->coreg_pL->grid, 
				cfg->coreg_pL->fft), "fine coregistration last patch (fico)");
		  check_return(fit_line("reg/ficoL", "reg/lineL"), "fit regression line last patch (fit_line)");

		  sprintf(cfg->coreg_pL->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}
	
		/* Calculate update Doppler */
		if (strncmp(cfg->aisp_master->doppler, "updated", 7)==0) {
	          if (strncmp(cfg->doppler_per_patch->status, "new", 3)==0) {
	            check_return(doppler_per_patch(cfg->master->meta, cfg->slave->meta, "a.meta", "b.meta", 
				"reg/deltas", "a.dop", "b.dop"), 
				"calculating the updated Doppler (doppler_per_patch)");
	            sprintf(cfg->doppler_per_patch->status, "success");
	            check_return(write_config(configFile, cfg), "Could not update configuration file");
	          }
		}

		/* Processing the master image */
	        if (strncmp(cfg->aisp_master->status, "new", 3)==0) {
		  if (cfg->aisp_master->power < 0 || cfg->aisp_master->power > 1) {
		    printf("\n   WARNING: power flag set to invalid value - set to value of 1\n\n");
		    cfg->aisp_master->power = 1;
	            check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  sprintf(tmp, "-d 1 -c a.dop");
	          if (cfg->general->procs == 1) {
		    if (cfg->aisp_master->power == 1) sprintf(options, "%s -power", tmp);
		    if (cfg->aisp_master->deskew == 1) sprintf(options, "%s -e 1", tmp);
/*		    if (cfg->aisp_master->deskew == 1) sprintf(options, "%s -deskew", tmp);*/
		    check_return(aisp(options, cfg->aisp_master->start_offset, cfg->aisp_master->patches, "a", "a"), 
					"processing master image (aisp)");
		  }
	          else {
		    if (cfg->aisp_master->power == 1) sprintf(options, "%s -power", tmp);
/*		    if (datatype == 0) strcat(options, " -swath");*/
		    check_return(paisp(options, cfg->aisp_master->start_offset, cfg->aisp_master->patches, 
					cfg->general->procs, "a", "a"), "processing master image (paisp)");
		  }
		  if (cfg->aisp_master->power == 1) {
		    sprintf(tmp, "mv a_pwr.img %s_a_pwr.img", cfg->general->base); system(tmp);
		    sprintf(tmp, "mv a_pwr.ddr %s_a_pwr.ddr", cfg->general->base); system(tmp);
		  }
		  sprintf(cmd, "cp a.meta %s.meta", cfg->general->base); system(cmd);

	          sprintf(cfg->aisp_master->status, "success");
	          check_return(write_config(configFile, cfg), "Could not update configuration file");
	        }

		/* Coregister slave image */
		if (strncmp(cfg->aisp_slave->status, "new", 3)==0) {
		  check_return(calc_deltas("reg/line1", "reg/lineL", cfg->coreg_pL->start_master - cfg->coreg_p1->start_master,
						"reg/deltas"), "conversion of regression coefficients (calc_deltas)");	

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
		    printf("\n   WARNING: power flag set to invalid value - set to value of 1\n\n");
		    cfg->aisp_slave->power = 1;
		    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
		  }
		  sprintf(tmp, "-o reg/deltas -d 1 -c b.dop");
	          if (cfg->general->procs == 1) {
		    if (cfg->aisp_slave->power == 1) sprintf(options, "%s -power", tmp);
		    if (cfg->aisp_slave->deskew == 1) sprintf(options, "%s -e 1", tmp);
/*		    if (cfg->aisp_slave->deskew == 1) sprintf(options, "%s -deskew", tmp);*/
		    check_return(aisp(options, cfg->aisp_slave->start_offset, cfg->aisp_slave->patches, "b", "b_corr"), 
					"processing slave image (aisp)");
		  }
	          else {
		    if (cfg->aisp_master->power == 1) sprintf(options, "%s -power", tmp);
/*		    if (datatype == 0) strcat(options, " -swath");*/
		    check_return(paisp(options, cfg->aisp_slave->start_offset, cfg->aisp_slave->patches, 
					cfg->general->procs, "b", "b_corr"), "processing slave image (paisp)");
		  }
		  if (cfg->aisp_slave->power == 1) {
		    sprintf(tmp, "mv b_corr_pwr.img %s_b_pwr.img", cfg->general->base); system(tmp);
		    sprintf(tmp, "mv b_corr_pwr.ddr %s_b_pwr.ddr", cfg->general->base); system(tmp);
		  }

		  sprintf(cfg->aisp_slave->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}
	}

	if (datatype==2) {

		/* Ingest CEOS SLC data */
		if (strncmp(cfg->trim_slc->status, "new", 3)==0) {
		  if (!fileExists(cfg->master->data)) check_return(1, "master image data file does not exist");
		  if (!fileExists(cfg->master->meta)) check_return(1, "master image metadata file does not exist");
		  if (!fileExists(cfg->slave->data)) check_return(1, "slave image data file does not exist");
		  if (!fileExists(cfg->slave->meta)) check_return(1, "slave image metadata file does not exist");

		  if ((cfg->trim_slc->length == -99) || (cfg->trim_slc->width == -99)) { 
		    struct IOF_VFDR vfdr1, vfdr2;

		    get_ifiledr(cfg->master->data, &vfdr1);
		    get_ifiledr(cfg->slave->data, &vfdr2);
		    cfg->trim_slc->length = (vfdr1.linedata < vfdr2.linedata) ? vfdr1.linedata : vfdr2.linedata;
		    cfg->trim_slc->width = (vfdr1.datgroup < vfdr2.datgroup) ? vfdr1.datgroup : vfdr2.datgroup;
		  }
		  check_return(trim_slc(cfg->master->data, "a", cfg->trim_slc->line, cfg->trim_slc->sample,
			cfg->trim_slc->length, cfg->trim_slc->width), "ingesting master image (trim_slc)");
		  check_return(trim_slc(cfg->slave->data, "b", cfg->trim_slc->line, cfg->trim_slc->sample,
			cfg->trim_slc->length, cfg->trim_slc->width), "ingesting slave image (trim_slc)");
		  check_return(c2p("a", "a"), "converting complex master image into phase and amplitude (c2p)");
		  meta = meta_init("a.meta");
	  	  check_return(amp2img("a.amp", "a_amp.img", meta->sar->look_count, 1), 
					"creating byte amplitude master image (amp2img)");
		  check_return(c2p("b", "b"), "converting complex slave image into phase and amplitude (c2p)");
		  meta = meta_init("b.meta");
	  	  check_return(amp2img("b.amp", "b_amp.img", meta->sar->look_count, 1), 
					"creating byte amplitude slave image (amp2img)");
		  sprintf(cfg->trim_slc->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}

		/* Filter master and slave image 
		if (strncmp(cfg->cpx_autofilter->status, "new", 3)==0) {
		  if (cfg->general->procs == 1) 
		    check_return(cpx_autofilter("a", "b", "a_fil", "b_fil"), 
					"filtering master and slave image (cpx_autofilter)");
		  else
		    check_return(pcpx_autofilter("a", "b", "a_fil", "b_fil", cfg->general->procs), 
					"filtering master and slave image (pcpx_autofilter)");

		  sprintf(cmd, "ln -s a.meta a_fil.meta"); system(cmd);
		  sprintf(cmd, "ln -s b.meta b_fil.meta"); system(cmd);
		  sprintf(cfg->cpx_autofilter->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		} */

		/* Coregister slave image */
		if (strncmp(cfg->coreg_slave->status, "new", 3)==0) {
		  check_return(resolve("a", "b", "reg/ctrl"), "offset estimation (resolve)");
		  sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
		  check_return(fico("a", "b", "reg/ctrl", "reg/fico", cfg->coreg_slave->grid, cfg->coreg_slave->fft), 
					"fine coregistration slave image (fico)");
		  if (cfg->coreg_slave->warp == 1) {
		    check_return(fit_warp("reg/fico", "b", "reg/warp"), 
					"calculating offset grids (fit_warp)");
		    sprintf(tmp, "-warp reg/warp -sameSize");
		    if (cfg->coreg_slave->sinc) strcat(tmp, " -sinc");
	  	    check_return(remap("b.cpx", "b_corr.cpx", tmp), "resampling of slave image (remap)");		  
		  }
		  else {
		    check_return(fit_plane("reg/fico", "reg/matrix", 0.8), 
					"calculating transformation parameters (fit_plane)");
		    sprintf(tmp, "-matrix reg/matrix -sameSize");
		    if (cfg->coreg_slave->sinc) strcat(tmp, " -sinc");
	  	    check_return(remap("b.cpx", "b_corr.cpx", tmp), "resampling of slave image (remap)");		  
		  }
		  sprintf(cmd, "mv a_amp.img %s_a_amp.img", cfg->general->base); system(cmd);
		  sprintf(cmd, "mv a_amp.ddr %s_a_amp.ddr", cfg->general->base); system(cmd);
		  sprintf(cmd, "rm b_amp.img b_amp.ddr"); system(cmd);
		  check_return(c2p("b_corr", "b_corr"), 
					"converting complex slave image into phase and amplitude (c2p)");
		  sprintf(tmp, "%s_b_amp.img", cfg->general->base);
	  	  check_return(amp2img("b_corr.amp", tmp, meta->sar->look_count, 1), 
					"creating byte amplitude slave image (amp2img)");
		  sprintf(cfg->coreg_slave->status, "success");
		  check_return(write_config(configFile, cfg), "Could not update configuration file");
		}
	}

	/* Calculate the interferogram and coherence */
	if (strncmp(cfg->igram_coh->status, "new", 3)==0) {
	  if (cfg->general->procs == 1) 
	    check_return(igram("a", "b_corr", cfg->igram_coh->igram), "interferogram generation (igram)");
	  else 
	    check_return(pigram("a", "b_corr", cfg->igram_coh->igram, cfg->general->procs), 
				"interferogram generation (pigram)");

	  if (cfg->igram_coh->min < 0.0 || cfg->igram_coh->min > 1.0) {
	    printf("\n   WARNING: minimum average coherence out of range - set to value of 0.3\n\n");
	    cfg->igram_coh->min = 0.3;
	    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	  }
	  if (cfg->general->procs == 1) 
	    check_return(coh("a", "b_corr", cfg->igram_coh->coh), "generating coherence image (coh)");
	  else 
	    check_return(pcoh("a", "b_corr", cfg->igram_coh->coh, cfg->general->procs), 
				"generating coherence image (pcoh)");
	  if (fCoh = FOPEN(logFile, "r")) {
	    while (fgets(tmp, 255, fCoh) != NULL) {
	      if (strncmp(tmp, "   Average Coherence:", 21)==0) sscanf(tmp, "%s %s %f", tmp, tmp, &avg);
	    }
	    if (avg < cfg->igram_coh->min) {
	      sprintf(tmp, "average coherence level below minimum of %.1f", cfg->igram_coh->min);
	      check_return(1, tmp);
	    }
	    FCLOSE(fCoh);
	  }

	  if (cfg->igram_coh->ml < 0 || cfg->igram_coh->ml > 1) {
	    printf("\n   WARNING: multilook flag set to invalid value - set to value of 1\n\n");
	    cfg->igram_coh->ml = 1;
	    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	  }
	  if (cfg->igram_coh->ml == 1) {
	    sprintf(tmp, "%s_ml", cfg->igram_coh->igram);
	    if (cfg->general->procs == 1) 
	      check_return(ml(cfg->igram_coh->igram, tmp, "a.meta"), "multilooking interferogram (ml)");
	    else 
	      check_return(pml(cfg->igram_coh->igram, tmp, cfg->general->procs, "a.meta"), 
				"multilooking interferogram (pml)");
	  }

	  sprintf(cfg->igram_coh->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}

	sprintf(cmd, "cp a.meta %s.meta", cfg->igram_coh->igram);
	system(cmd);

	/* Refine the offset */
	if (strncmp(cfg->offset_match->status, "new", 3)==0) { 
	  i = 0;
	  while (off) {
	    meta = meta_init("a.meta");
	    sprintf(tmp, "%s.amp", cfg->igram_coh->igram);
	    check_return(amp2img(tmp, "sar_byte", meta->sar->look_count, 1), "creating byte amplitude image (amp2img)");
	    c_getddr("sar_byte", &ddr);
	    if (!fileExists(cfg->general->dem)) check_return(1, "reference DEM file does not exist");
	    check_return(create_dem_grid(cfg->general->dem, "sar_byte.img", "a.meta", "dem_grid"), 
			"creating a grid (create_dem_grid)");
	    check_return(fit_plane("dem_grid", "dem_plane", (double) 1.0), 
			"transformation parameters for resampling (fit_plane)");
	    sprintf(tmp, "-matrix dem_plane -translate 0 0 -width %d -height %d -bilinear -float", ddr.ns+400, ddr.nl);
	    check_return(remap(cfg->general->dem, "dem_big.dem", tmp),
			"resampling of reference DEM (remap)");		  
	    check_return(make_ddr("dem_big.dem", ddr.nl, ddr.ns+400, "float"), 
			"creating DDR file for resampled DEM (makeddr)"); 
	    check_return(reskew_dem("dem_big.dem", "a.meta", "dem_slant.ht", "dem_sim.amp"), 
			"transformation subset of reference DEM into slant range (reskew_dem)");
	    check_return(amp2img("dem_sim.amp", "dem_simbyte.img", 1, 1), 
				"creating simulated byte amplitude image (amp2img)");
	    c_getddr("sar_byte", &ddr);
	    check_return(trim("dem_simbyte.img", "dem_trimsim.img", 0, 0, ddr.nl, ddr.ns), 
				"re-sizing simulated byte amplitude (trim)");
	    check_return(fftMatch("sar_byte", "dem_trimsim.img", "dem_corr"), 
				"matching real and simulated amplitude (fftMatch)"); 

	    fCorr = FOPEN("dem_corr", "r");
	    fscanf(fCorr, "%f %f", &xshift, &yshift);
	    FCLOSE(fCorr);
	    sprintf(tmp, "cp dem_corr offset.%d", i);
	    system(tmp);
	    i++;
	    if (fabs(xshift)<cfg->offset_match->max && fabs(yshift)<cfg->offset_match->max) off = 0;

	    sprintf(cmd, "mv dem_slant.ht dem_lined.ht"); system(cmd);
	    sprintf(cmd, "mv dem_slant.ddr dem_lined.ddr"); system(cmd);
	  
	    meta = meta_init("a.meta");
	    meta->sar->time_shift -= yshift * meta->sar->azimuth_time_per_pixel;
	    meta->sar->slant_shift -= xshift * meta->general->x_pixel_size;
	    meta_write(meta, "a.meta");
	  }

	  sprintf(cfg->offset_match->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}
	
	/* Simulated phase image and seed points */
	if (strncmp(cfg->sim_phase->status, "new", 3)==0) { 
	  check_return(dem2phase("dem_lined.ht", "a.meta", base2str(0, cfg->general->base), "out_dem_phase.phase"), 
			"creating simulated phase (dem2phase)");
	  check_return(dem2seeds("dem_lined.ht", "sar_byte.img" , cfg->sim_phase->seeds, 0), 
			"creating seed points (dem2seeds)");

	  sprintf(cfg->sim_phase->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}

	/* Deramping and multilooking interferogram */
	if (strncmp(cfg->deramp_ml->status, "new", 3)==0) {
	  check_return(deramp(cfg->igram_coh->igram, "a.meta", base2str(0, cfg->general->base), "igramd", 0), 
					"deramping interferogram (deramp)");
	  if (cfg->general->procs == 1)
	    check_return(ml("igramd", "ml", "a.meta"), "multilooking interferogram (ml)");
	  else
	    check_return(pml("igramd", "ml", cfg->general->procs, "a.meta"), "multilooking interferogram (pml)");

	  sprintf(cfg->deramp_ml->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}

	/* Phase unwrapping */
	if (strncmp(cfg->unwrap->status, "new", 3)==0) {
	  if (cfg->unwrap->flattening < 0 || cfg->unwrap->flattening > 1) {
	    printf("\n   WARNING: flattening flag set to invalid value - set to value of 0\n\n");
	    cfg->unwrap->flattening = 0;
	    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	  }
	  if (cfg->unwrap->flattening==1) {
	    check_return(las_op("ml_dem.phase", "\'(a-b)%6.2831853-3.14159265\' ml.phase out_dem_phase.phase"),
					"subtracting terrain induced phase (las_op)");
	  }

	  if (strncmp(cfg->unwrap->algorithm, "escher", 6)==0) {
	    if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem.phase");
	    else sprintf(tmp, "ml.phase");
	    if (cfg->unwrap->filter < 0.0 || cfg->unwrap->filter > 3.0) {
	      printf("\n   WARNING: phase filter value out of range - set to value of 1.6\n\n");
	      cfg->unwrap->filter = 1.6;
	      check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	    }
	    if (cfg->unwrap->filter>0.0) {
	      check_return(phase_filter(tmp, (double) cfg->unwrap->filter, "filtered_phase"), 
					"phase filtering (phase_filter)");
	      sprintf(tmp, "filtered_phase");
	    }
	    if (strcmp(tmp, "ml.phase")!=0) {
	      check_return(zeroify(tmp, "ml.phase", "escher_in.phase"), "phase value cosmetics (zeroify)");
	      sprintf(tmp, "escher_in.phase");
	    }
	    if (cfg->unwrap->flattening==1) {
	      check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	      check_return(las_op("unwrap.phase", "\'(a+b)*(a/a)*(b/b)\' unwrap_dem.phase out_dem_phase.phase"),
					"adding terrain induced phase back (las_op)");
	    }
	    else
	      check_return(escher(tmp,"unwrap"), "phase unwrapping (escher)");
	    system("ln -s unwrap.ddr unwrap_dem.phase.mask.ddr");
	    system("ln -s unwrap.ddr unwrap_dem.phase.ddr");
	    check_return(las2ppm("unwrap_dem.phase.mask", "unwrap_mask.ppm"), "colorized phase unwrapping mask (las2ppm)");
	  }

	  if (strncmp(cfg->unwrap->algorithm, "snaphu", 6)==0) {
	    sprintf(cmd, "make_snaphu_conf %s.phase unwrap.phase", cfg->igram_coh->igram); system(cmd);
	    if (cfg->unwrap->tiles_azimuth == 0 || cfg->unwrap->tiles_range == 0) {
	      meta = meta_init("a.meta");
	      meta_get_latLon(meta, cfg->aisp_master->start_offset, 1, 0, &lat1, &lon);
	      meta_get_latLon(meta, cfg->aisp_master->end_offset, 1, 0, &lat2, &lon);
	      cfg->unwrap->tiles_azimuth = (int) (fabs(lat1-lat2)*cfg->unwrap->tiles_per_degree);
	      cfg->unwrap->tiles_range = cfg->unwrap->tiles_azimuth;
	    }
	    check_return(snaphu(cfg->unwrap->algorithm, "ml.phase", "ml.amp", cfg->aisp_master->power_img,
					cfg->aisp_slave->power_img, "snaphu.conf", "unwrap.phase", cfg->unwrap->tiles_azimuth,
					cfg->unwrap->tiles_range, cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
					cfg->general->procs), "phase unwrapping (snaphu)");
	    system("ln -s ml.ddr unwrap.ddr");
	  }
	  check_return(deramp("unwrap", "a.meta", base2str(0, cfg->general->base), "unwrap_nod", 1), 
					"reramping unwrapped phase (deramp)");

	  sprintf(cfg->unwrap->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}

	/* Baseline refinement */
	if (strncmp(cfg->refine->status, "new", 3)==0) {
	  if (cfg->refine->max < 1 || cfg->refine->max > 15) {
	    printf("\n   WARNING: maximum number of iterations out of range - set to value of 15\n\n");
	    cfg->refine->max = 15;
	    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	  }
	  newBase = base2str(0, cfg->general->base);
	  for (i=0; i<cfg->refine->max; i++) 
	  {
	    veryoldBase = oldBase;
	    oldBase = newBase;
	    newBase = base2str(i+1, cfg->general->base);

	    check_return(refine_base("unwrap_nod.phase", cfg->sim_phase->seeds, "a.meta", oldBase, newBase), 
					"baseline refinement (refine_base)");
	    if (i>0)
	      if (check_refinement(newBase,oldBase,veryoldBase)) break;
	  }
	  if (i==cfg->refine->max) check_return(1, "Baseline iterations failed to converge");
	  cfg->refine->iter = i+1; 
 
	  check_return(deramp(cfg->igram_coh->igram, "a.meta", base2str(cfg->refine->iter, cfg->general->base), "igramd", 0), 
					"deramping interferogram with refined baseline (deramp)");
	  sprintf(tmp, "%s_ml", cfg->igram_coh->igram);
	  if (cfg->general->procs == 1)
	    check_return(ml("igramd", tmp, "a.meta"), "multilooking refined interferogram (ml)");
	  else
	    check_return(pml("igramd", tmp, cfg->general->procs, "a.meta"), 
				"multilooking refined interferogram (pml)");

	  sprintf(cfg->refine->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}       

	/* Elevation and elevation error */
	if (strncmp(cfg->elevation->status, "new", 3)==0) {
	  newBase = base2str(cfg->refine->iter, cfg->general->base);
	  check_return(deramp("unwrap_nod", "a.meta", newBase, "unwrap", 0), 
				"deramping unwrapped phase with refined baseline (deramp)");
	  check_return(elev("unwrap.phase", newBase, "a.meta", cfg->elevation->dem, cfg->sim_phase->seeds), 
				"creating elevation map (elev)");
	  if (strcmp(cfg->unwrap->algorithm, "escher")==0) {
	    check_return(eleverr(cfg->igram_coh->coh, newBase, "a.meta", "unwrap_dem.phase.mask" , cfg->elevation->error), 
					"elevation error estimate (eleverr)");
	  }
	  else
	    check_return(eleverr(cfg->igram_coh->coh, newBase, "a.meta", NULL, cfg->elevation->error), 
					"elevation error estimate (eleverr)");

	  sprintf(cfg->elevation->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}

	/* Remapping to ground range */
	if (strncmp(cfg->ground_range->status, "new", 3)==0) {
	  check_return(deskew_dem(cfg->elevation->dem, "a.meta", "elevation.dem", "", 1), 
					"remapping elevation to ground range DEM (deskew_dem)");
	  check_return(deskew_dem("elevation.dem", "a.meta", "amplitude_float.img", "a_amp.img", 1), 
					"remapping amplitude to ground range (deskew_dem)");
	  check_return(deskew_dem("elevation.dem", "a.meta", "error.img", cfg->elevation->error, 1), 
					"remapping error map to ground range (deskew_dem)");
	  check_return(amp2img("amplitude_float.img", "amplitude.img", 1, 2),
					"converting ground range amplitude to byte (amp2img)");
	  check_return(deskew_dem("elevation.dem", "a.meta", "coh_gr.img", "coh.img", 0), 
					"remapping coherence to ground range (deskew_dem)");
					
	  sprintf(cfg->ground_range->status, "success");
	  check_return(write_config(configFile, cfg), "Could not update configuration file");
	}

	/* Geocoding */
	if (strncmp(cfg->geocode->status, "new", 3)==0) {
	  if (cfg->geocode->pix_spacing < 5 || cfg->geocode->pix_spacing > 500) {
	    printf("\n   WARNING: pixel spacing out of range - set to default value of 20\n\n");
	    cfg->geocode->pix_spacing = 20;
	    check_return(write_config(configFile, cfg), "Could not update configuration file"); 
	  }
	  if (!fileExists(cfg->geocode->proj)) check_return(1, "projection file does not exist");
	  sprintf(tmp, "grep -c %s %s > /dev/null", cfg->geocode->key, cfg->geocode->proj);
	  if (system(tmp)) check_return(1, "projection key not defined");
	  check_return(geocode("elevation.meta", "elevation.dem", cfg->geocode->proj, cfg->geocode->key,
				cfg->geocode->pix_spacing, cfg->geocode->dem), "geocoding ground range DEM (geocode)");      
	  check_return(geocode("elevation.meta", "amplitude.img", cfg->geocode->proj, cfg->geocode->key, 
				cfg->geocode->pix_spacing, cfg->geocode->amp), "geocoding ground range amplitude (geocode)");
	  check_return(geocode("elevation.meta", "error.img", cfg->geocode->proj, cfg->geocode->key, 
				cfg->geocode->pix_spacing, cfg->geocode->error), 
				"geocoding ground range error map (geocode)");
	  check_return(geocode("elevation.meta", "coh_gr.img", cfg->geocode->proj, cfg->geocode->key, 
				cfg->geocode->pix_spacing, cfg->geocode->coh), 
				"geocoding ground range coherence (geocode)");

	  sprintf(cfg->geocode->status, "success");
	}   
					

	StopWatch();
	if (logflag) StopWatchLog(fLog);

	sprintf(cfg->general->status, "success");
	check_return(write_config(configFile, cfg), "Could not update configuration file");

	return(0);
}

