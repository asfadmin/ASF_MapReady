/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"ips"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create] [-license] [-version] [-help] <config_file>\n\n"

#define ASF_DESCRIPTION_STRING \
"   "ASF_NAME_STRING" will run the complete SAR interferometric\n"\
"   processing chain, from ingesting CEOS or STF data\n"\
"   until the creation and geocoding of the digital elevation model.\n\n"\
"   Alternatively, ips can run in differential mode and produce \n"\
"   a differential interferogram (under development).\n\n"\
"   All the input parameters for the programs are read in from\n"\
"   a configuration file.\n\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   config_file\n"\
"        A configuration file that "ASF_NAME_STRING" uses to find which files to\n"\
"        use for input and output, what options to use, and how the data should\n"\
"        be processed. It is either read or created based on whether or not\n"\
"        the -create option is specified.\n\n"

#define ASF_OPTIONS_STRING \
"   -create\n"\
"        Create <config_file> instead of reading it.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print a help page and exit.\n\n"

#define ASF_EXAMPLES_STRING \
"   To create an editable configuration file named \"convert.config\" with some\n"\
"   example values, do this:\n"\
"      example> "ASF_NAME_STRING" -create convert.config\n"\
"\n"\
"   To process level 1 CEOS data using an \"ASF_NAME_STRING\" style configuration\n"\
"   file named \"config\", do this:\n"\
"      example> "ASF_NAME_STRING" config\n\n"

#define ASF_LIMITATIONS_STRING \
"   None known.\\n"

#define ASF_SEE_ALSO_STRING \
"   All other InSAR tools\\n"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) "ASF_COPYRIGHT_YEAR_STRING", University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/
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
#include "asf_reporting.h"
#include "asf_copyright.h"
#include "asf_license.h"
#include "asf_contact.h"

#define FLAG_NOT_SET -1
#define REQUIRED_ARGS 1

// Print minimalistic usage info & exit
static void print_usage(void)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  exit(EXIT_FAILURE);
}

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Required Arguments:\n" ASF_REQUIRED_ARGUMENTS_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n");
  exit(EXIT_SUCCESS);
}

// Print version and copyright & exit
static void print_version(void)
{
  asfPrintStatus(
    ASF_NAME_STRING", version "CONVERT_PACKAGE_VERSION_STRING"\n"
    ASF_COPYRIGHT_STRING);
  exit(EXIT_SUCCESS);
}

// Print our copyright and license notice & exit
static void print_license(int license_id)
{
  asfPrintStatus("\n"ASF_COPYRIGHT_STRING"\n");

  switch (license_id) {
    case ASF_BSD_ID:
      asfPrintStatus(ASF_BSD_LICENSE_STRING"\n");
      break;
    default:
      printf("License not found.\n");
      break;
  }
  exit(EXIT_SUCCESS);
}

/* Check to see if an option was supplied or not. If it was found, return its
   argument number. Otherwise, return FLAG_NOT_SET. STOLEN FROM ASF_IMPORT */
int checkForOption(char* key, int argc, char* argv[])
{
  int ii = 0;
  while(ii < argc)
  {
    if(strmatch(key, argv[ii]))
      return(ii);
    ++ii;
  }
  return(FLAG_NOT_SET);
}

void check_return(int ret, char *msg)
{
  if (ret!=0) {
    asfPrintError("%s\n", msg);
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
  char format[255], options[255], metaFile[25];
  char *veryoldBase=NULL, *oldBase=NULL, *newBase=NULL;
  int i, delta, createFlag=FLAG_NOT_SET, datatype=0, off=1, nLooks, nl, ns;
  float xshift, yshift, avg;
  double lat1=0.0, lat2=0.0, lon1;
  
  // Check for all those little helper options
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  if ( checkForOption("-license", argc, argv) != FLAG_NOT_SET ) {
      print_license(ASF_BSD_ID);
  }
  if ( checkForOption("-version", argc, argv) != FLAG_NOT_SET ) {
      print_version();
  }

  // Check which options were provided
  createFlag = checkForOption("-create", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS;               // command & REQUIRED_ARGS
  if (createFlag != FLAG_NOT_SET)  needed_args += 1; // option

  // Make sure we have the right number of args
  if(argc != needed_args) {
    print_usage();
  }

  if (createFlag != FLAG_NOT_SET)
    createFlag = 1;
  else
    createFlag = 0;

  // Fetch required arguments
  strcpy(configFile, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);
  
  // If requested, create a config file and exit (if the file does not exist),
  // otherwise read it
  if ( createFlag==TRUE && !fileExists(configFile) ) {
    init_config(configFile);
    exit(EXIT_SUCCESS);
  }
  // Extend the configuration file if the file already exist
  else if ( createFlag==TRUE && fileExists(configFile) ) {
    cfg = read_config(configFile, createFlag);
    // Assign names for results to be kept
    sprintf(cfg->igram_coh->igram, "%s_igram", cfg->general->base);
    sprintf(cfg->igram_coh->coh, "coh.img", cfg->general->base);
    sprintf(cfg->ardop_master->power_img, "%s_a_pwr.img", cfg->general->base);
    sprintf(cfg->ardop_slave->power_img, "%s_b_pwr.img", cfg->general->base);
    sprintf(cfg->sim_phase->seeds, "%s.seeds", cfg->general->base);
    sprintf(cfg->dinsar->igram, "%s_digram.img", cfg->general->base);
    sprintf(cfg->unwrap->qc, "%s_qc.phase", cfg->general->base);
    sprintf(cfg->elevation->dem, "%s_ht.img", cfg->general->base);
    sprintf(cfg->elevation->error, "%s_err_ht.img", cfg->general->base);
    sprintf(cfg->geocode->dem, "%s_dem", cfg->general->base);
    sprintf(cfg->geocode->amp, "%s_amp", cfg->general->base);
    sprintf(cfg->geocode->error, "%s_error", cfg->general->base);
    sprintf(cfg->geocode->coh, "%s_coh", cfg->general->base);
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
    asfPrintStatus("   Initialized complete configuration file\n\n");
    FCLOSE(fLog);
    exit(EXIT_SUCCESS);
  }
  else {
    cfg = read_config(configFile, createFlag);
  }
  
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
  
  // Determine datatype
  if (strncmp(uc(cfg->general->data_type), "STF", 3)==0) {
    datatype = 0;
    sprintf(format, "STF");
  }
  if (strncmp(uc(cfg->general->data_type), "RAW", 3)==0) {
    datatype = 1;
    sprintf(format, "CEOS");
  }
  if (strncmp(uc(cfg->general->data_type), "SLC", 3)==0) {
    datatype = 2;
    sprintf(format, "CEOS");
  }

  // Prepare processing
  sscanf(cfg->master->path, "%s", path);
  sscanf(cfg->master->data, "%s", data);
  sscanf(cfg->master->meta, "%s", metadata);
  if (strncmp(cfg->general->status, "new", 3)==0 && !createFlag) {
    sprintf(tmp, "%s/%s", path, data);
    if (!fileExists(tmp)) check_return(1, "master image data file does not exist");
    if (datatype==0) {
      sprintf(cmd, "ln -s %s/%s master.000", path, data);
      system(cmd);
    }
    else {
      sprintf(cmd, "ln -s %s/%s master.D", path, data);
      system(cmd);
    }
    sprintf(tmp, "%s/%s", path, metadata);
    if (!fileExists(tmp)) 
      check_return(1, "master image metadata file does not exist");
    if (datatype==0) {
      sprintf(cmd, "ln -s %s/%s master.000.par", path, metadata);
      system(cmd);
    }
    else {
      sprintf(cmd, "ln -s %s/%s master.L", path, metadata);
      system(cmd);
    }
  }
  sscanf(cfg->slave->path, "%s", path);
  sscanf(cfg->slave->data, "%s", data);
  sscanf(cfg->slave->meta, "%s", metadata);
  if (strncmp(cfg->general->status, "new", 3)==0 && !createFlag) {
    sprintf(tmp, "%s/%s", path, data);
    if (!fileExists(tmp)) check_return(1, "slave image data file does not exist");
    if (datatype==0) {
      sprintf(cmd, "ln -s %s/%s slave.000", path, data);
      system(cmd);
    }
    else {
      sprintf(cmd, "ln -s %s/%s slave.D", path, data); system(cmd);
      sprintf(tmp, "%s/%s", path, metadata);
    }
    if (!fileExists(tmp)) 
      check_return(1, "slave image metadata file does not exist");
    if (datatype==0) {
      sprintf(cmd, "ln -s %s/%s slave.000.par", path, metadata);
      system(cmd);
    }
    else {
      sprintf(cmd, "ln -s %s/%s slave.L", path, metadata);
      system(cmd);
    }
    system("mkdir reg");
  } 
  
  /* Update configuration file */
  if (!createFlag) sprintf(cfg->general->status, "progress");
  check_return(write_config(configFile, cfg), "Could not update configuration file");
  
  if (createFlag) {
    printf("   Initialized complete configuration file\n\n");
    if (logflag) {
      fLog = FOPEN(logFile, "a");
      printLog("   Initialized complete configuration file\n\n");
      FCLOSE(fLog);
    } 
    exit(0);
  }
  
  // Tell the user what data type and processing mode we found
  printf("   Data type: %s\n   Processing mode: %s\n", 
	 cfg->general->data_type, cfg->general->mode);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf, "   Data type: %s\n   Processing mode: %s\n", 
	  cfg->general->data_type, cfg->general->mode);
  printLog(logbuf);
  FCLOSE(fLog);
  
  /* Ingest the various data types: STF, RAW, or SLC */
  if (datatype==0) {
    
    /* Ingest of level zero STF data */
    if (check_status(cfg->ingest->status)) {
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
   
      if (!fileExists("master.000")) 
	check_return(1, "master image data file does not exist");
      if (!fileExists("master.000.par")) 
	check_return(1, "master image metadata file does not exist"); 
      check_return(asf_import("master", "a", format,
			      cfg->ingest->prc_master, cfg->ingest->prcflag, 
			      cfg->general->lat_begin, cfg->general->lat_end), 
		   "ingesting master image(asf_import)");
      if (!fileExists("slave.000")) 
	check_return(1, "slave image data file does not exist");
      if (!fileExists("slave.000.par")) 
	check_return(1, "slave image metadata file does not exist"); 
      check_return(asf_import("slave", "b", format,
			      cfg->ingest->prc_slave, cfg->ingest->prcflag, 
			      cfg->general->lat_begin, cfg->general->lat_end), 
		   "ingesting slave image (asf_import)");
      
      strcat(strcpy(metaFile,"a"),".meta");
      cfg->ardop_master->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      cfg->ardop_master->patches = 
	(int) ((cfg->ardop_master->end_offset-4096)/ARDOP_VALID_PATCH_LENGTH) + 2;
      strcat(strcpy(metaFile,"b"),".meta");
      cfg->ardop_slave->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      
      if (cfg->ardop_slave->end_offset > cfg->ardop_master->end_offset)
	cfg->ardop_slave->end_offset = cfg->ardop_master->end_offset;
      else
	cfg->ardop_master->end_offset = cfg->ardop_slave->end_offset;
      
      cfg->ardop_slave->patches = cfg->ardop_master->patches;
      
      sprintf(cfg->ingest->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
  }
  
  if (datatype==1) {
    
    /* Ingest of CEOS raw data */
    if (check_status(cfg->ingest->status)) {
      if (!fileExists("master.D")) 
	check_return(1, "master image data file does not exist");
      if (!fileExists("master.L")) 
	check_return(1, "master image metadata file does not exist");
      check_return(asf_import("master", "a", format,
			      cfg->ingest->prc_master, cfg->ingest->prcflag, 
			      cfg->general->lat_begin, cfg->general->lat_end), 
		   "ingesting master image (asf_import)");
      if (!fileExists("master.D")) 
	check_return(1, "slave image data file does not exist");
      if (!fileExists("master.L")) 
	check_return(1, "slave image metadata file does not exist"); 
      check_return(asf_import("slave", "b", format,
			      cfg->ingest->prc_master, cfg->ingest->prcflag, 
			      cfg->general->lat_begin, cfg->general->lat_end), 
		   "ingesting slave image (asf_import)");
      
      /* Setting patches and offsets for processing */
      strcat(strcpy(metaFile,"a"),".meta");
      cfg->ardop_master->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      cfg->ardop_master->patches = 
	(int) ((cfg->ardop_master->end_offset-4096)/ARDOP_VALID_PATCH_LENGTH) + 2;
      strcat(strcpy(metaFile,"b"),".meta");
      cfg->ardop_slave->end_offset = 
	lzInt(metaFile, "sar.original_line_count:", NULL);
      
      if (cfg->ardop_slave->end_offset > cfg->ardop_master->end_offset)
	cfg->ardop_slave->end_offset = cfg->ardop_master->end_offset;
      else
	cfg->ardop_master->end_offset = cfg->ardop_slave->end_offset;
      
      cfg->ardop_slave->patches = cfg->ardop_master->patches;
      
      sprintf(cfg->ingest->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
  }
  
  if (datatype<2) {
    
    /* Calculate average Doppler */
    if (strncmp(cfg->general->doppler, "average", 7)==0) {
      if (check_status(cfg->doppler->status)) {
	check_return(avg_in_dop("a", "b", "reg/avedop"), 
		     "calculating the average Doppler (avg_in_dop)");
	system("cp reg/avedop a.dop");
	system("cp reg/avedop b.dop");
	sprintf(cfg->doppler->status, "success");
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
      check_return(ardop(tmp, cfg->coreg_p1->start_master, cfg->coreg_p1->patches , 
			 "a", "reg/a_p1"), 
		   "processing first patch of master image (ardop)");
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c b.dop");
      check_return(ardop(tmp, cfg->coreg_p1->start_slave, cfg->coreg_p1->patches , 
			 "b", "reg/b_p1"), 
		   "processing first patch of slave image (ardop)");
      if (cfg->general->mflag) {
	strcat(strcpy(metaFile,"a"),".meta");
	nLooks = lzInt(metaFile, "sar.look_count:", NULL);
	strcat(strcpy(metaFile,"reg/a_p1_cpx"),".meta");
	nl = lzInt(metaFile, "general.line_count:", NULL);
	ns = lzInt(metaFile, "general.sample_count:", NULL);
	check_return(trim(cfg->general->mask, "reg/mask1", 
			  cfg->coreg_p1->start_master/nLooks, 0,
			  nl/nLooks, ns), "mask for first patch (trim)");
	check_return(coregister_coarse("reg/a_p1", "reg/b_p1", 
				       "reg/ctrl1", "reg/mask1"), 
		     "offset estimation first patch (coregister_coarse)");
      }
      else 
	check_return(coregister_coarse("reg/a_p1", "reg/b_p1", 
				       "reg/ctrl1", NULL), 
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
	  cfg->ardop_master->end_offset - cfg->coreg_pL->patches*4096;
	cfg->coreg_pL->start_slave = 
	  cfg->ardop_slave->end_offset - cfg->coreg_pL->patches*4096;
      }
      
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c a.dop");
      if (cfg->general->deskew == 1) sprintf(tmp, "-debug 1 -e 1");
      check_return(ardop(tmp, cfg->coreg_pL->start_master, cfg->coreg_pL->patches , 
			 "a", "reg/a_pL"), 
		   "processing last patch of master image (ardop)");
      if (cfg->general->deskew == 0) sprintf(tmp, "-debug 1 -c b.dop");
      check_return(ardop(tmp, cfg->coreg_pL->start_slave, cfg->coreg_pL->patches , 
			 "b", "reg/b_pL"), 
		   "processing last patch of slave image (ardop)");
      
      if (cfg->general->mflag) {
	strcat(strcpy(metaFile,"a"),".meta");
	nLooks = lzInt(metaFile, "sar.look_count:", NULL);
	strcat(strcpy(metaFile,"reg/a_pL_cpx"),".meta");
	nl = lzInt(metaFile, "general.line_count:", NULL);
	ns = lzInt(metaFile, "general.sample_count:", NULL);
	check_return(trim(cfg->general->mask, "reg/maskL", 
			  cfg->coreg_pL->start_master/nLooks, 0, 
			  nl/nLooks, ns), "mask for last patch (trim)");
	check_return(coregister_coarse("reg/a_pL", "reg/b_pL", 
				       "reg/ctrlL", "reg/maskL"), 
		     "offset estimation last patch (coregister_coarse)");
      }
      else
	check_return(coregister_coarse("reg/a_pL", "reg/b_pL", 
				       "reg/ctrlL", NULL), 
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
	check_return(coregister_fine("reg/a_p1_cpx", "reg/b_p1_cpx", "reg/ctrl1", 
				     "reg/fico1", "reg/mask1.img", 
				     cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		     "fine coregistration first patch (coregister_fine)");
	check_return(coregister_fine("reg/a_pL_cpx", "reg/b_pL_cpx", "reg/ctrlL", 
				     "reg/ficoL", "reg/maskL.img",
				     cfg->coreg_pL->grid, cfg->coreg_pL->fft), 
		     "fine coregistration last patch (coregister_fine)");
      }
      else {
	check_return(coregister_fine("reg/a_p1_cpx", "reg/b_p1_cpx", "reg/ctrl1", 
				     "reg/fico1", NULL, 
				     cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		     "fine coregistration first patch (coregister_fine)");
	check_return(coregister_fine("reg/a_pL_cpx", "reg/b_pL_cpx", "reg/ctrlL", 
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
    if (check_status(cfg->ardop_master->status)) {
      if (cfg->ardop_master->power < 0 || cfg->ardop_master->power > 1) {
	printf("\n   WARNING: power flag set to invalid value - "
	       "set to value of 1\n\n");
	cfg->ardop_master->power = 1;
	check_return(write_config(configFile, cfg), 
		     "Could not update configuration file"); 
      }
      if (cfg->general->deskew == 0) sprintf(options, "-debug 1 -c a.dop");
      if (cfg->general->deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (cfg->ardop_master->power == 1) strcat(options, " -power");
      
      cfg->ardop_master->start_offset = cfg->coreg_p1->start_master;
      check_return(ardop(options, cfg->ardop_master->start_offset, 
			 cfg->ardop_master->patches, "a", "a"), 
		   "processing master image (ardop)");
      if (cfg->ardop_master->power == 1) {
	sprintf(cmd, "mv a_pwr.img %s_a_pwr.img", cfg->general->base); system(cmd);
	sprintf(cmd, "mv a_pwr.meta %s_a_pwr.meta", cfg->general->base); system(cmd);
      }
      // sprintf(cmd, "cp a.meta %s.meta", cfg->general->base); system(cmd);
      
      sprintf(cfg->ardop_master->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
    /* Coregister slave image */
    if (check_status(cfg->ardop_slave->status)) {
      if (strncmp(cfg->general->coreg, "FRAME", 5)==0) { 
	/* match the whole slave to master */
	if (cfg->general->deskew == 0) sprintf(options, "-debug 1 -c b.dop");
	if (cfg->general->deskew == 1) sprintf(options, "-debug 1 -e 1");
	if (cfg->ardop_slave->power == 1) strcat(options, " -power");
	check_return(ardop(options, cfg->ardop_slave->start_offset, 
			   cfg->ardop_slave->patches, "b", "b"), 
		     "processing slave image (ardop)");
	if (cfg->general->mflag) 
	  check_return(coregister_fine("a_cpx", "b_cpx", "reg/ctrl", "reg/fico", 
				       cfg->general->mask,
				       cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "offset estimation slave image (coregister_fine)");
	else 
	  check_return(coregister_fine("a_cpx", "b_cpx", "reg/ctrl", "reg/fico", 
				       NULL, cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "offset estimation slave image (coregister_fine)");
	sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
	
	/* Taking care of the overlap */
	fCorr = FOPEN("reg/ctrl", "r");
	fscanf(fCorr, "%d", &delta);
	fscanf(fCorr, "%d", &delta);
	FCLOSE(fCorr);
	if (delta > 0) {
	  cfg->ardop_master->patches = (int) 
	    ((cfg->ardop_master->end_offset-4096-delta)/ARDOP_VALID_PATCH_LENGTH) + 1;
	  cfg->ardop_slave->patches = cfg->ardop_master->patches;
	  cfg->ardop_master->start_offset = delta;
	}
	else {
	  cfg->ardop_slave->patches = (int) 
	    ((cfg->ardop_slave->end_offset-4096+delta)/ARDOP_VALID_PATCH_LENGTH) + 1;
	  cfg->ardop_master->patches = cfg->ardop_slave->patches;
	  cfg->ardop_slave->start_offset = -delta;
	}
	check_return(ardop(options, cfg->ardop_master->start_offset, 
			   cfg->ardop_master->patches, "a", "a"), 
		     "processing master image (ardop)");
	check_return(ardop(options, cfg->ardop_slave->start_offset, 
			   cfg->ardop_slave->patches, "b", "b"), 
		     "processing slave image (ardop)");
	
	if (cfg->ardop_master->power == 1) {
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
	  check_return(coregister_coarse("a_cpx", "b_cpx", "reg/ctrl", 
					 cfg->general->mask), 
		       "offset estimation slave image (coregister_coarse)");
	  check_return(coregister_fine("a_cpx", "b_cpx", "reg/ctrl", "reg/fico", 
				       cfg->general->mask, 
				       cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "fine coregistration slave image (coregister_fine)");
	}
	else {
	  check_return(coregister_coarse("a_cpx", "b_cpx", "reg/ctrl", NULL), 
		       "offset estimation slave image (coregister_coarse)");
	  check_return(coregister_fine("a_cpx", "b_cpx", "reg/ctrl", "reg/fico",
				       NULL, cfg->coreg_p1->grid, cfg->coreg_p1->fft), 
		       "fine coregistration slave image (coregister_fine)");
	}
	check_return(fit_plane("reg/fico", "reg/matrix", 0.8), 
		     "calculate transformation parameters (fit_plane)");
	sprintf(tmp, "-matrix reg/matrix -sameSize");
	check_return(remap("b_cpx.img", "b_corr_cpx.img", tmp), 
		     "resampling of slave image (remap)");		  
      }
      
      else {
	check_return(calc_deltas("reg/line1", "reg/lineL", 
				 cfg->coreg_pL->start_master - 
				 cfg->coreg_p1->start_master, "reg/deltas"), 
		     "conversion of regression coefficients (calc_deltas)");	
	
	delta = cfg->coreg_p1->start_master - cfg->ardop_master->start_offset;
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
	
	if (cfg->ardop_slave->power < 0 || cfg->ardop_slave->power > 1) {
	  printf("\n   WARNING: power flag set to invalid value - "
		 "set to value of 1\n\n");
	  cfg->ardop_slave->power = 1;
	  check_return(write_config(configFile, cfg), 
		       "Could not update configuration file"); 
	}
	if (cfg->general->deskew == 0) 
	  sprintf(options, "-o reg/deltas -debug 1 -c b.dop");
	if (cfg->general->deskew == 1) 
	  sprintf(options, "-o reg/deltas -debug 1 -e 1");
	if (cfg->ardop_slave->power == 1) 
	  strcat(options, " -power");
	
	cfg->ardop_slave->start_offset = cfg->coreg_p1->start_slave;
	check_return(ardop(options, cfg->ardop_slave->start_offset, 
			   cfg->ardop_slave->patches, "b", "b_corr"), 
		     "processing slave image (ardop)");
      }
      if (cfg->ardop_slave->power == 1) {
	sprintf(cmd, "mv b_corr_pwr.img %s_b_pwr.img", cfg->general->base); 
	system(cmd);
	sprintf(cmd, "mv b_corr_pwr.meta %s_b_pwr.meta", cfg->general->base); 
	system(cmd);
      }
      
      sprintf(cfg->ardop_slave->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
  }
  
  if (datatype==2) {
    
    /* Ingest CEOS SLC data */
    if (check_status(cfg->ingest->status)) {
      if (!fileExists("master.D")) 
	check_return(1, "master image data file does not exist");
      if (!fileExists("master.L")) 
	check_return(1, "master image metadata file does not exist");
      if (!fileExists("slave.D")) 
	check_return(1, "slave image data file does not exist");
      if (!fileExists("slave.L")) 
	check_return(1, "slave image metadata file does not exist");

      /* Check later !!! Might need some more parameters in ingest block.
	 if ((cfg->trim_slc->length == -99) || (cfg->trim_slc->width == -99)) { 
	 struct IOF_VFDR vfdr1, vfdr2;
	 
	 get_ifiledr(cfg->master->data, &vfdr1);
	 get_ifiledr(cfg->slave->data, &vfdr2);
	 cfg->trim_slc->length = 
	 (vfdr1.linedata < vfdr2.linedata) ? vfdr1.linedata : vfdr2.linedata;
	 cfg->trim_slc->width = 
	 (vfdr1.datgroup < vfdr2.datgroup) ? vfdr1.datgroup : vfdr2.datgroup;
	 }
      */
      check_return(asf_import("master", "a_cpx", format,
			      cfg->ingest->prc_master, 0, -99.0, -99.0), 
		   "ingesting master image (asf_import)");
      check_return(asf_import("slave", "b_cpx", format,
			      cfg->ingest->prc_master, 0, -99.0, -99.0), 
 		   "ingesting slave image (asf_import)");
      check_return(c2p("a_cpx", "a"), 
		   "converting complex master image into phase and amplitude (c2p)");
      meta = meta_init("a_cpx.meta");
      check_return(convert2byte("a_amp.img", "a_amp_byte.img", meta->ifm->nLooks, 1), 
		   "creating byte amplitude master image (convert2byte)");
      check_return(c2p("b_cpx", "b"), 
		   "converting complex slave image into phase and amplitude (c2p)");
      meta = meta_init("b_cpx.meta");
      check_return(convert2byte("b_amp.img", "b_amp_byte.img", meta->ifm->nLooks, 1), 
		   "creating byte amplitude slave image (convert2byte)");
      sprintf(cfg->ingest->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
    
    /* Coregister slave image */
    if (check_status(cfg->coreg_slave->status)) {
      if (cfg->general->mflag) {
	check_return(coregister_coarse("a_cpx", "b_cpx", "reg/ctrl", 
				       cfg->general->mask), 
		     "offset estimation (coregister_coarse)");
	check_return(coregister_fine("a_cpx", "b_cpx", "reg/ctrl", "reg/fico", 
				     cfg->general->mask, 
				     cfg->coreg_slave->grid, cfg->coreg_slave->fft), 
		     "fine coregistration slave image (coregister_fine)");
      }
      else {
	check_return(coregister_coarse("a_cpx", "b_cpx", "reg/ctrl", NULL), 
		     "offset estimation (coregister_coarse)");
	check_return(coregister_fine("a_cpx", "b_cpx", "reg/ctrl", "reg/fico", NULL, 
				     cfg->coreg_slave->grid, cfg->coreg_slave->fft), 
		     "fine coregistration slave image (coregister_fine)");
      }
      sprintf(cmd, "cp base.00 %s.base.00", cfg->general->base); system(cmd);
      if (cfg->coreg_slave->warp == 1) {
	check_return(fit_warp("reg/fico", "b", "reg/warp"), 
		     "calculating offset grids (fit_warp)");
	sprintf(tmp, "-warp reg/warp -sameSize");
	if (cfg->coreg_slave->sinc) strcat(tmp, " -sinc");
	check_return(remap("b_cpx.img", "b_corr_cpx.img", tmp), 
		     "resampling of slave image (remap)");		  
      }
      else {
	check_return(fit_plane("reg/fico", "reg/matrix", 0.8), 
		     "calculating transformation parameters (fit_plane)");
	sprintf(tmp, "-matrix reg/matrix -sameSize");
	if (cfg->coreg_slave->sinc) strcat(tmp, " -sinc");
	check_return(remap("b_cpx.img", "b_corr_cpx.img", tmp), 
		     "resampling of slave image (remap)");		  
      }
      sprintf(cmd, "mv a_amp.img %s_a_amp.img", cfg->general->base); system(cmd);
      sprintf(cmd, "mv a_amp.meta %s_a_amp.meta", cfg->general->base); system(cmd);
      sprintf(cmd, "rm b_amp.img b_amp.meta"); system(cmd);
      check_return(c2p("b_corr_cpx", "b_corr"), 
		   "converting complex slave image into phase and amplitude (c2p)");
      sprintf(tmp, "%s_b_amp.img", cfg->general->base);
      check_return(convert2byte("b_corr_amp.img", tmp, meta->ifm->nLooks, 1), 
		   "creating byte amplitude slave image (convert2byte)");
      sprintf(cfg->coreg_slave->status, "success");
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file");
    }
  }
  
  /* Calculate the interferogram and coherence */
  if (check_status(cfg->igram_coh->status)) {
    check_return(igram("a_cpx.img", "b_corr_cpx.img", cfg->igram_coh->igram), 
		 "interferogram generation (igram)");
    
    if (cfg->igram_coh->min < 0.0 || cfg->igram_coh->min > 1.0) {
      printf("\n   WARNING: minimum average coherence out of range - "
	     "set to value of 0.3\n\n");
      cfg->igram_coh->min = 0.3;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    check_return(coh("a_cpx.img", "b_corr_cpx.img", cfg->igram_coh->coh), 
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
      check_return(multilook(cfg->igram_coh->igram, tmp, "a_cpx.meta"), 
		   "multilooking interferogram (multilook)");
    }
    
    sprintf(cfg->igram_coh->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Refine the offset */
  if (check_status(cfg->offset_match->status)) {
    
    sprintf(tmp, "%s_ml_amp.img", cfg->igram_coh->igram);
    check_return(asf_check_geolocation(tmp, cfg->general->dem, "offset", 
				       "dem_sim.img", "dem_slant.img"), 
		 "refining the geolocation of the SAR image");
    
    sprintf(cfg->offset_match->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Simulated phase image and seed points */
  if (check_status(cfg->sim_phase->status)) { 
    check_return(dem2phase("dem_slant.img", "a_cpx.meta", 
			   base2str(0, cfg->general->base), "out_dem_phase.img"), 
		 "creating simulated phase (dem2phase)");
    
    sprintf(tmp, "%s_ml_amp.img", cfg->igram_coh->igram);
    check_return(dem2seeds("dem_slant.img", tmp, cfg->sim_phase->seeds, 0), 
		 "creating seed points (dem2seeds)");
    
    sprintf(cfg->sim_phase->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Calculate differential interferogram */
  
  if (strncmp(cfg->general->mode, "DINSAR", 6)==0) {
    sprintf(tmp, "\'(a-b)%%6.2831853-3.14159265\' %s_ml_phase.img "
	    "out_dem_phase.img", cfg->igram_coh->igram);
    check_return(raster_calc("dinsar.phase", tmp), 
		 "calculating differential interferogram (raster_calc)");
    sprintf(cfg->dinsar->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
    exit(0);
  }
  
  /* Deramping and multilooking interferogram */
  if (check_status(cfg->deramp_ml->status)) {
    check_return(deramp(cfg->igram_coh->igram, 
			base2str(0, cfg->general->base), "igramd", 0), 
		 "deramping interferogram (deramp)");
    check_return(multilook("igramd", "ml", "a_cpx.meta"), 
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
      check_return(raster_calc("ml_dem_phase.img", "\'(a-b)%6.2831853-3.14159265\' "
			       "ml_phase.img out_dem_phase.img"),
		   "subtracting terrain induced phase (raster_calc)");
    }
    
    if (strncmp(cfg->unwrap->algorithm, "escher", 6)==0) {
      if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem_phase.img");
      else sprintf(tmp, "ml_phase.img");
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
      if (strcmp(tmp, "ml_phase.img")!=0) {
	check_return(zeroify(tmp, "ml_phase.img", "escher_in_phase.img"), 
		     "phase value cosmetics (zeroify)");
	sprintf(tmp, "escher_in_phase.img");
      }
      if (cfg->unwrap->flattening==1) {
	check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	check_return(raster_calc("unwrap_phase.img", "\'(a+b)*(a/a)*(b/b)\' "
				 "unwrap_dem.img out_dem_phase.img"),
		     "adding terrain induced phase back (raster_calc)");
      }
      else
	check_return(escher(tmp,"unwrap"), "phase unwrapping (escher)");
      
      check_return(deramp("unwrap", base2str(0, cfg->general->base), 
			  "unwrap_nod", 1), 
		   "reramping unwrapped phase (deramp)");
      
      system("ln -s unwrap_phase.meta unwrap_dem_phase.mask.meta");
      system("ln -s unwrap_phase.meta unwrap_dem_phase.meta");
      check_return(convert2ppm("unwrap_dem_phase.mask", "unwrap_mask.ppm"), 
		   "colorized phase unwrapping mask (convert2ppm)");
    }
    
    if (strncmp(cfg->unwrap->algorithm, "snaphu", 6)==0) {
      
      if (cfg->general->test == 1) {
	if (cfg->unwrap->flattening==1) sprintf(tmp, "ml_dem.phase");
	else sprintf(tmp, "ml_phase.img");
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
	if (strcmp(tmp, "ml_phase.img")!=0) {
	  check_return(zeroify(tmp, "ml_phase", "escher_in_phase.img"), 
		       "phase value cosmetics (zeroify)");
	  sprintf(tmp, "escher_in_phase.img");
	}
	if (cfg->unwrap->flattening==1) {
	  check_return(escher(tmp,"unwrap_dem"), "phase unwrapping (escher)");
	  check_return(raster_calc("escher.phase", "\'(a+b)*(a/a)*(b/b)\' "
				   "unwrap_dem_phase.img out_dem_phase.img"),
		       "adding terrain induced phase back (raster_calc)");
	}
	else
	  check_return(escher(tmp,"escher"), "phase unwrapping (escher)");
	
	check_return(deramp("escher", base2str(0, cfg->general->base), 
			    "escher_nod", 1), 
		     "reramping unwrapped phase (deramp)");
	
	system("ln -s escher.meta unwrap_dem_phase.mask.meta");
	system("ln -s escher.meta unwrap_dem_phase.meta");
	check_return(convert2ppm("unwrap_dem_phase.mask", "unwrap_mask.ppm"), 
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
	/* some more old trim_slc stuff to check!!!
	   
	if (datatype==2) {
	meta_get_latLon(meta, cfg->trim_slc->line, 1, 0, &lat1, &lon);
	meta_get_latLon(meta, cfg->trim_slc->length, 1, 0, &lat2, &lon);
	}
	else {
	*/
	meta_get_latLon(meta, cfg->ardop_master->start_offset, 1, 0, &lat1, &lon1);
	meta_get_latLon(meta, cfg->ardop_master->end_offset, 1, 0, &lat2, &lon1);
/*	}*/
	cfg->unwrap->tiles_azimuth = 
	  (int) (fabs(lat1-lat2)*cfg->unwrap->tiles_per_degree);
	cfg->unwrap->tiles_range = (int) (fabs(cfg->unwrap->tiles_per_degree*0.8));
      }
      if (cfg->unwrap->flattening==1) 
	check_return(snaphu(cfg->unwrap->algorithm, "ml_phase.img", "ml_amp.img", 
			    cfg->ardop_master->power_img, cfg->ardop_slave->power_img, 
			    "snaphu.conf", "unwrap_phase.img",
			    cfg->unwrap->tiles_azimuth, cfg->unwrap->tiles_range, 
			    cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
			    cfg->unwrap->procs, 1), "phase unwrapping (snaphu)");
      else
	check_return(snaphu(cfg->unwrap->algorithm, "ml_phase.img", "ml_amp.img", 
			    cfg->ardop_master->power_img, cfg->ardop_slave->power_img, 
			    "snaphu.conf", "unwrap_phase.img", 
			    cfg->unwrap->tiles_azimuth, cfg->unwrap->tiles_range, 
			    cfg->unwrap->overlap_azimuth, cfg->unwrap->overlap_range,
			    cfg->unwrap->procs, 0), "phase unwrapping (snaphu)");
      system("ln -s ml_phase.meta unwrap_phase.meta");
    }
    
    check_return(deramp("unwrap", base2str(0, cfg->general->base), 
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
	
	check_return(refine_base("unwrap_nod_phase.img", 
				 cfg->sim_phase->seeds, oldBase, newBase), 
		     "baseline refinement (refine_base)");
	if (i>0)
	  if (check_refinement(newBase,oldBase,veryoldBase)) break;
      }
    if (i==cfg->refine->max) 
      check_return(1, "Baseline iterations failed to converge");
    cfg->refine->iter = i+1; 
    
    check_return(deramp(cfg->igram_coh->igram, 
			base2str(cfg->refine->iter, cfg->general->base), 
			"igramd", 0), 
		 "deramping interferogram with refined baseline (deramp)");
    sprintf(tmp, "%s_ml", cfg->igram_coh->igram);
    check_return(multilook("igramd", tmp, "a_cpx.meta"), 
		 "multilooking refined interferogram (multilook)");
    
    sprintf(cfg->refine->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }       
  
  /* Elevation and elevation error */
  if (check_status(cfg->elevation->status)) {
    newBase = base2str(cfg->refine->iter, cfg->general->base);
    check_return(deramp("unwrap_nod", newBase, "unwrap", 0), 
		 "deramping unwrapped phase with refined baseline (deramp)");
    sprintf(cmd, "raster_diff -d %s unwrap_phase.img out_dem_phase.img", 
	    cfg->unwrap->qc);
    system(cmd);
    check_return(elev("unwrap_phase.img", newBase, cfg->elevation->dem, 
		      cfg->sim_phase->seeds), 
		 "creating elevation map (elev)");
    if (strcmp(cfg->unwrap->algorithm, "escher")==0) {
      check_return(eleverr(cfg->igram_coh->coh, newBase,
			   "unwrap_dem_phase.mask" , cfg->elevation->error), 
		   "elevation error estimate (eleverr)");
    }
    else
      check_return(eleverr(cfg->igram_coh->coh, newBase, NULL, 
			   cfg->elevation->error), 
		   "elevation error estimate (eleverr)");
    
    sprintf(cfg->elevation->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Remapping to ground range */
  if (check_status(cfg->ground_range->status)) {
    check_return(deskew_dem(cfg->elevation->dem, "elevation.img", "", 1), 
		 "remapping elevation to ground range DEM (deskew_dem)");
    check_return(deskew_dem(cfg->elevation->dem, "amplitude.img", 
			    "a_amp.img", 1), 
		 "remapping amplitude to ground range (deskew_dem)");
    check_return(deskew_dem(cfg->elevation->dem, "error.img", 
			    cfg->elevation->error, 1), 
		 "remapping error map to ground range (deskew_dem)");
    check_return(deskew_dem(cfg->elevation->dem, "coh_gr.img", "coh.img", 0), 
		 "remapping coherence to ground range (deskew_dem)");
    
    sprintf(cfg->ground_range->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  /* Geocoding */
  if (check_status(cfg->geocode->status)) {
    /* Need to create an options string to pass into asf_geocode.
       For UTM projection the center longitude can be passed in.
       For Albers Conic Equal Area, Lambert Conformal Conic and Polar Stereographic
       we can use predefined projection files.*/
    if (cfg->geocode->pixel_spacing < 5) {
      printf("\n   WARNING: pixel spacing out of range - "
	     "set to default value of 20\n\n");
      cfg->geocode->pixel_spacing = 20;
      check_return(write_config(configFile, cfg), 
		   "Could not update configuration file"); 
    }
    if (strcmp(cfg->geocode->name, "utm")==0 ||
	strcmp(cfg->geocode->name, "albers")==0 ||
	strcmp(cfg->geocode->name, "lamaz")==0 ||
	strcmp(cfg->geocode->name, "lamcc")==0 ||
	strcmp(cfg->geocode->name, "ps")==0) {
      if (!fileExists(cfg->geocode->proj)) 
	check_return(1, "projection file does not exist");
      sprintf(tmp, "-read-proj-file %s -resample-method %s -pixel-size %.1lf",
	      cfg->geocode->proj, cfg->geocode->resample, cfg->geocode->pixel_spacing);
    }
    check_return(asf_geocode(tmp, "elevation", cfg->geocode->dem), 
		 "geocoding ground range DEM (geocode)");      
    check_return(asf_geocode(tmp, "amplitude", cfg->geocode->amp), 
		 "geocoding ground range amplitude (geocode)");
    check_return(asf_geocode(tmp, "error", cfg->geocode->error), 
		 "geocoding ground range error map (geocode)");
    check_return(asf_geocode(tmp, "coh_gr", cfg->geocode->coh), 
		 "geocoding ground range coherence (geocode)");
    
    sprintf(cfg->geocode->status, "success");
  }   
  
  /* Exporting */
  if (check_status(cfg->export->status)) {
    if (strcmp(uc(cfg->export->format), "TIFF")==0) {
      sprintf(format, "tiff");
    }
    else if (strcmp(uc(cfg->export->format), "GEOTIFF")==0) {
      sprintf(format, "geotiff");
    }
    else if (strcmp(uc(cfg->export->format), "JPEG")==0) {
      sprintf(format, "jpeg");
    }
    else if (strcmp(uc(cfg->export->format), "PPM")==0) {
      sprintf(format, "ppm");
    }
    else {
      sprintf(tmp, "export format '%s' not supported", cfg->export->format);
      check_return(1, tmp);
    }
    
    sprintf(tmp, "-format %s", format);
    check_return(asf_export(tmp, cfg->geocode->dem, cfg->geocode->dem), 
		 "exporting geocoded DEM (export)");
    check_return(asf_export(tmp, cfg->geocode->amp, cfg->geocode->amp), 
		 "exporting geocoded amplitude image (export)");
    check_return(asf_export(tmp, cfg->geocode->error, cfg->geocode->error), 
		 "exporting geocoded DEM error map (export)");
    check_return(asf_export(tmp, cfg->geocode->coh, cfg->geocode->coh), 
		 "exporting geocoded coherence image (export)");
    
    sprintf(cfg->export->status, "success");
    check_return(write_config(configFile, cfg), 
		 "Could not update configuration file");
  }
  
  sprintf(cfg->general->status, "success");
  check_return(write_config(configFile, cfg), "Could not update configuration file");
  
  return(0);
}

