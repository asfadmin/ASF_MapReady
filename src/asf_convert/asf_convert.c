#include <asf_contact.h>
#include <asf_copyright.h>
#include <asf_license.h>

#include <asf_convert.h>
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
"asf_convert"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create] [-log <logFile>] [-quiet] [-license]\n"\
"               [-version] [-help]\n"\
"               <config_file>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can ingest level one CEOS data, geocode it, and export it to\n"\
"   a variety of imagery formats. The user is able to control how "ASF_NAME_STRING"\n"\
"   dictates the processiong flow by creating and/or editting a configuration\n"\
"   file which is fed into "ASF_NAME_STRING" when it is called.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   config_file\n"\
"        A configuration file that "ASF_NAME_STRING" uses to find which files to\n"\
"        use for input and output, what options to use, and how the data should\n"\
"        be processed. It is either read or created based on whether or not\n"\
"        the -create option is specified.\n"

#define ASF_OPTIONS_STRING \
"   -create\n"\
"        Create <config_file> instead of reading it.\n"\
"   -log <logFile>\n"\
"        Set the name and location of the log file. Default behavior is to\n"\
"        log to tmp<processIDnumber>.log\n"\
"   -quiet\n"\
"        Suppresses all non-essential output.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"   To create an editable configuration file named 'convert.conf' with some\n"\
"   example values, do this:\n"\
"      example> "ASF_NAME_STRING" -create convert.conf\n"\
"\n"\
"   To process level 1 CEOS data using an "ASF_NAME_STRING" style configuration\n"\
"   file named 'config', do this:\n"\
"      example> "ASF_NAME_STRING" config\n"

#define ASF_LIMITATIONS_STRING \
"   None known.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_import, asf_geocode, asf_export\n"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) "ASF_COPYRIGHT_YEAR_STRING", University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "proj.h"
#include "asf_reporting.h"
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

#define REQUIRED_ARGS 1

int main(int argc, char *argv[])
{
  char configFileName[255];
  const int pid = getpid();
  int createflag;
  extern int logflag, quietflag;
  int create_f, quiet_f;  /* log_f is a static global */

  createflag = logflag = quietflag = FALSE;
  create_f = log_f = quiet_f = FLAG_NOT_SET;

  // Begin command line parsing ***********************************************
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
  create_f = checkForOption("-create", argc, argv);
  log_f    = checkForOption("-log", argc, argv);
  quiet_f  = checkForOption("-quiet", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS;               // command & REQUIRED_ARGS
  if (create_f != FLAG_NOT_SET)  needed_args += 1; // option
  if (log_f    != FLAG_NOT_SET)  needed_args += 2; // option & param
  if (quiet_f  != FLAG_NOT_SET)  needed_args += 1; // option

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

  // Do the actual flagging & such for each flag
  if (create_f != FLAG_NOT_SET) {
    createflag = TRUE;
  }
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
  quietflag = (quiet_f!=FLAG_NOT_SET) ? TRUE : FALSE;

  // Fetch required arguments
  strcpy(configFileName, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // End command line parsing *************************************************

  asf_convert(createflag, configFileName);

  return(EXIT_SUCCESS);
}
