#include <asf_contact.h>
#include <asf_license.h>

#include <asf_convert.h>
#include <asf_vector.h>
#include <sys/types.h> /* 'DIR' structure (for opendir) */
#include <dirent.h>    /* for opendir itself            */

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
"asf_kml_overlay"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-terrain_correct <demFile> | -refine_geolocation <demFile>]\n"\
"                [-reduction_factor <factor>] [-log <logFile>]\n"\
"                [-quiet] [-license] [-version] [-help]\n"\
"                <inFile> <outFile>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can generates KML overlay files from CEOS level one data.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   inFile\n"\
"        The level one CEOS level one data file.\n"\
"   outFile\n"\
"        The basename of the KML overlay file.\n"\

#define ASF_OPTIONS_STRING \
"   -terrain_correct <demFile>\n"\
"        Terrain corrects the image with the DEM.\n"\
"   -refine_geolocation <demFile>\n"\
"        Uses the DEM for refining the geolocation only.\n"\
"   -reduction_factor <factor>\n"\
"        Defines the factor that the pixel size is multiplied to reduce\n"\
"        the size of the output image.\n"\
"   -log <logFile>\n"\
"        Set the name and location of the log file. Default behavior is to\n"\
"        log to tmp<processIDnumber>.log\n"\
"   -quiet\n"\
"        Suppresses most non-essential output.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print this help page and exit.\n"

#define ASF_LIMITATIONS_STRING \
"   Level 0 data is not yet supported.  GeoTIFF SAR files created by ASF tools\n"\
"   do not contain SAR related data and this prevents certain processing:\n"\
"   terrain correction etc. when importing and processing from the GeoTIFF\n"\
"   format.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_mapready, convert2vector\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "proj.h"
#include "asf_contact.h"
#include <unistd.h>

#define REQUIRED_ARGS 1
#define FLAG_NOT_SET -1

static int log_f;

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
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_FAILURE);
}

/* Check to see if an option was supplied or not. If it was found, return its
   argument number. Otherwise, return FLAG_NOT_SET. STOLEN FROM ASF_IMPORT */
static int checkForOption(char* key, int argc, char* argv[])
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

static double max2(double a, double b)
{
  return a > b ? a : b;
}

static double max4(double a, double b, double c, double d)
{
  return max2(max2(a,b), max2(c,d));
}

static double min2(double a, double b)
{
  return a < b ? a : b;
}

static double min4(double a, double b, double c, double d)
{
  return min2(min2(a,b), min2(c,d));
}

int main(int argc, char *argv[])
{
  char inFile[512], outFile[512], *demFile=NULL;
  const int pid = getpid();
  extern int logflag, quietflag;
  int reduction;
  int quiet_f;  /* log_f is a static global */
  int tcFlag = FLAG_NOT_SET; // terrain correction flag
  int rgFlag = FLAG_NOT_SET; // refine geolocation only flag
  int redFlag = FLAG_NOT_SET; // reduction factor flag

  logflag = quietflag = FALSE;
  log_f = quiet_f = FLAG_NOT_SET;

  // Begin command line parsing ***********************************************
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  get_asf_share_dir_with_argv0(argv[0]);
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  // Check which options were provided
  log_f    = checkForOption("-log", argc, argv);
  quiet_f  = checkForOption("-quiet", argc, argv);
  tcFlag = checkForOption("-terrain_correct", argc, argv);
  rgFlag = checkForOption("-refine_geolocation", argc, argv);
  redFlag = checkForOption("-reduction_factor", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 2 + REQUIRED_ARGS; // command & REQUIRED_ARGS
  int num_flags = 0;
  if (log_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (tcFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (rgFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (redFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param

  // Make sure we have the right number of args
  if(argc != needed_args) {
    print_usage();
  }

  // Make sure argument for each flag (that requires an arg) is not another
  // option flag & is not a required argument
  if (log_f != FLAG_NOT_SET) {
    if ( (argv[log_f+1][0]=='-') || (log_f>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (tcFlag != FLAG_NOT_SET) {
    if ( (argv[tcFlag+1][0]=='-') || (tcFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (rgFlag != FLAG_NOT_SET) {
    if ( (argv[rgFlag+1][0]=='-') || (rgFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (redFlag != FLAG_NOT_SET) {
    if ( (argv[redFlag+1][0]=='-') || (redFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }

  // Make sure all options occur before the config file name argument
  if (num_flags == 1 &&
      (log_f   > 1 ||
       quiet_f > 1 ||
       tcFlag  > 1 ||
       rgFlag  > 1 ||
       redFlag > 1))
  {
    print_usage();
  }
  else if (num_flags > 1 &&
	   (log_f   >= argc - REQUIRED_ARGS - 1 ||
            quiet_f >= argc - REQUIRED_ARGS - 1 ||
	    tcFlag  >= argc - REQUIRED_ARGS - 1 ||
	    rgFlag  >= argc - REQUIRED_ARGS - 1 ||
	    redFlag >= argc - REQUIRED_ARGS -1))
  {
    print_usage();
  }

  // Make sure that the DEM is used for only one thing
  if (tcFlag != FLAG_NOT_SET && rgFlag != FLAG_NOT_SET)
    asfPrintError("The tool can either terrain correct or refine the "
		  "geolocation, not both!\n");

  // Do the actual flagging & such for each flag
  if (tcFlag != FLAG_NOT_SET) {
    demFile = (char *) MALLOC(sizeof(char)*1024);
    sprintf(demFile, "%s", argv[tcFlag+1]);
  }
  if (rgFlag != FLAG_NOT_SET) {
    demFile = (char *) MALLOC(sizeof(char)*1024);
    sprintf(demFile, "%s", argv[rgFlag+1]);
  }
  if (redFlag != FLAG_NOT_SET)
    reduction = argv[redFlag+1];
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
  }
  else {
    // default behavior: log to tmp<pid>.log
    sprintf(logFile, "tmp%i.log", pid);
  }
  logflag = TRUE;
  fLog = FOPEN(logFile, "a");
  // Set old school quiet flag (for use in our libraries)
  quietflag = quiet_f != FLAG_NOT_SET;

  // Fetch required arguments
  strcpy(inFile, argv[argc-2]);
  strcpy(outFile, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // No zipping at the moment
  asfPrintStatus("A system independent zipping function is currently not "
		 "available.\nFor the moment the KML and PNG files need to "
		 "manually zipped together.\n");
  if (redFlag)
    kml_overlay_ext(inFile, outFile, demFile, !tcFlag, !rgFlag, reduction, 
		    FALSE);
  else
    kml_overlay(inFile, outFile, demFile, !tcFlag, !rgFlag, FALSE);

  FREE(demFile);
  FCLOSE(fLog);
  remove(logFile);

  asfPrintStatus("\nSuccessful completion!\n\n");

  return(EXIT_SUCCESS);
}
