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
"   "ASF_NAME_STRING" [-reduction_factor <factor>] [-transparency <factor>]\n"\
"                [-colormap <look up table>] [-rgb <red>,<green>,<blue>]\n"\
"                [-polsarpro <type>] [-band <band name>]\n"\
"                [-byte_conversion <method>] [-log <logFile>]\n"\
"                [-quiet] [-license] [-version] [-help]\n"\
"                <inFile> <outFile>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can generates KML overlay files from CEOS level one data.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   inFile\n"\
"        The ASF internal or PolSARPro data file.\n"\
"   outFile\n"\
"        The basename of the KML overlay file.\n"\

#define ASF_OPTIONS_STRING \
"   -reduction_factor <factor>\n"\
"        Defines the factor that the pixel size is multiplied to reduce\n"\
"        the size of the output image.\n"\
"   -transparency <factor>\n"\
"        Defines the level of transparency of the overlay. Values range\n"\
"        from 0 to 100. The default value is 0.\n"\
"   -colormap <look up table>\n"\
"        Defines the look up table that is applied to generate a\n"\
"        color-coded output.\n"\
"   -rgb <red>,<green>,<blue>\n"\
"        Defines the RGB channels to form a color output, e.g. from\n"\
"        PolSARPro decompositions.\n"\
"   -polsarpro <type>\n"\
"        Defines the type of PolSARPro image data.\n"\
"        Values are: segmentation, decomposition and parameter.\n"\
"   -band <band name>\n"\
"        Defines the band name to to used from a multi-band image.\n"\
"   -byte_conversion <method>\n"\
"        Set the byte conversion for exporting the image to PNG\n"\
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

#define REQUIRED_ARGS 2
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

int main(int argc, char *argv[])
{
  char inFile[512], outFile[512], *colormap=NULL;
  char *polsarpro=NULL, *band=NULL, *rgb=NULL, *byteConversion=NULL;
  const int pid = getpid();
  extern int logflag, quietflag;
  int reduction = 8;
  int transparency = 0;
  int quiet_f;  /* log_f is a static global */
  int redFlag = FLAG_NOT_SET; // reduction factor flag
  int transFlag = FLAG_NOT_SET; // level of transparency flag
  int colorFlag = FLAG_NOT_SET; // colormap flag
  int rgbFlag = FLAG_NOT_SET; // RGB flag
  int polFlag = FLAG_NOT_SET; // polarimetry type flag
  int bandFlag = FLAG_NOT_SET; // band flag
  int byteConversionFlag = FLAG_NOT_SET; // byte conversion flag

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
  redFlag = checkForOption("-reduction_factor", argc, argv);
  transFlag = checkForOption("-transparency", argc, argv);
  colorFlag = checkForOption("-colormap", argc, argv);
  rgbFlag = checkForOption("-rgb", argc, argv);
  polFlag = checkForOption("-polsarpro", argc, argv);
  bandFlag = checkForOption("-band", argc, argv);
  byteConversionFlag = checkForOption("-byte_conversion", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS; // command & REQUIRED_ARGS
  int num_flags = 0;
  if (log_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (redFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (transFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (colorFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (rgbFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (polFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (bandFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (byteConversionFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;}

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
  if (redFlag != FLAG_NOT_SET) {
    if ( (argv[redFlag+1][0]=='-') || (redFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (transFlag != FLAG_NOT_SET) {
    if ( (argv[transFlag+1][0]=='-') || (transFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (colorFlag != FLAG_NOT_SET) {
    if ( (argv[colorFlag+1][0]=='-') || (colorFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (rgbFlag != FLAG_NOT_SET) {
    if ( (argv[rgbFlag+1][0]=='-') || (rgbFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (polFlag != FLAG_NOT_SET) {
    if ( (argv[polFlag+1][0]=='-') || (polFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (bandFlag != FLAG_NOT_SET) {
    if ( (argv[bandFlag+1][0]=='-') || (bandFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }
  if (byteConversionFlag != FLAG_NOT_SET) {
    if ( (argv[byteConversionFlag+1][0]=='-') || 
      (byteConversionFlag>=(argc-REQUIRED_ARGS)) ) {
      print_usage();
    }
  }

  // Make sure all options occur before the config file name argument
  if (num_flags == 1 &&
      (log_f     > 1 ||
       quiet_f   > 1 ||
       redFlag   > 1 ||
       transFlag > 1 ||
       colorFlag > 1 ||
       rgbFlag   > 1 ||
       polFlag   > 1 ||
       bandFlag  > 1 ||
       byteConversionFlag > 1))
  {
    print_usage();
  }
  else if (num_flags > 1 &&
	   (log_f     >= argc - REQUIRED_ARGS - 1 ||
            quiet_f   >= argc - REQUIRED_ARGS - 1 ||
	    redFlag   >= argc - REQUIRED_ARGS - 1 ||
	    transFlag >= argc - REQUIRED_ARGS - 1 ||
	    colorFlag >= argc - REQUIRED_ARGS - 1 ||
	    rgbFlag   >= argc - REQUIRED_ARGS - 1 ||
	    polFlag   >= argc - REQUIRED_ARGS - 1 ||
	    bandFlag  >= argc - REQUIRED_ARGS - 1 ||
	    byteConversionFlag >= argc - REQUIRED_ARGS -1))
  {
    print_usage();
  }

  // Do the actual flagging & such for each flag
  if (redFlag != FLAG_NOT_SET) {
    reduction = atoi(argv[redFlag+1]);
  }
  if (transFlag != FLAG_NOT_SET) {
    transparency = atoi(argv[transFlag+1]);
  }
  if (colorFlag != FLAG_NOT_SET) {
    colormap = (char *) MALLOC(sizeof(char)*1024);
    strcpy(colormap, argv[colorFlag+1]);
  }
  if (rgbFlag != FLAG_NOT_SET) {
    rgb = (char *) MALLOC(sizeof(char)*1024);
    strcpy(rgb, argv[rgbFlag+1]);
  }
  if (polFlag != FLAG_NOT_SET) {
    polsarpro = (char *) MALLOC(sizeof(char)*1024);
    strcpy(polsarpro, argv[polFlag+1]);
  }
  if (bandFlag != FLAG_NOT_SET) {
    band = (char *) MALLOC(sizeof(char)*1024);
    strcpy(band, argv[bandFlag+1]);
  }
  if (byteConversionFlag != FLAG_NOT_SET) {
    byteConversion = (char *) MALLOC(sizeof(char)*256);
    strcpy(byteConversion, argv[bandFlag+1]);
  }
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
  }
  else {
    // default behavior: log to tmp<pid>.log
    //sprintf(logFile, "tmp%i.log", pid);
    strcpy(logFile, get_tmp_log_file("asf_kml_overlay"));
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
		 "be manually zipped together.\n\n");
  kml_overlay_ext(inFile, outFile, reduction, transparency, colormap, rgb, 
		  polsarpro, band, FALSE, byteConversion);

  asfPrintStatus("\nSuccessful completion!\n\n");

  FCLOSE(fLog);
  remove(logFile);

  return(EXIT_SUCCESS);
}
