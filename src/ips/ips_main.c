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

#include "ips.h"

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

int main(int argc, char *argv[])
{
  dem_config *cfg;
  int createFlag = FLAG_NOT_SET;
  char configFile[255];

  // Check for all those little helper options
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

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
    sprintf(cfg->igram_coh->coh, "%s_coh.img", cfg->general->base);
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

  return ips(cfg, configFile, createFlag);
}
