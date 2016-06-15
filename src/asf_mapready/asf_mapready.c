#include <asf_contact.h>
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
"asf_mapready"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create] [-input <inFile>] [-output <outFile>]\n"\
"                [-tmpdir <dir>] [-log <logFile>] [-quiet] [-license]\n"\
"                [-version] [-help]\n"\
"                <config_file>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can ingest level one CEOS and GeoTIFF format data, calibrate\n"\
"   it to various radiometries, perform polarimetric decompositions, perform\n"\
"   Faraday Rotation correction, perform terrain correction, geocode it, and\n"\
"   then export it to a variety of graphics file formats. The user is able to\n"\
"   control how "ASF_NAME_STRING" dictates the processiong flow by creating a\n"\
"   configuration file, which must then be edited, which is fed into\n"\
"   "ASF_NAME_STRING" when it is called.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   config_file\n"\
"        A configuration file that "ASF_NAME_STRING" uses to find which files\n"\
"        to use for input and output, what options to use, and how the data\n"\
"        should be processed. It is either read or created based on whether or\n"\
"        not the -create option is specified (see below).\n"\
"        NOTE: When a new configuration file is created, it is filled with\n"\
"        comments that help the user with the available settings.\n\n"\
"        Alternatively, a settings file can be used as long as the input and\n"\
"        output file are defined using the appropriate options.\n"

#define ASF_OPTIONS_STRING \
"   -create <config_file>\n"\
"        Create <config_file> instead of reading it.\n"\
"   -input <inFile>\n"\
"        Overwrites the input file name in a configuration file or\n"\
"        defines the input file name for a settings file.\n"\
"   -output <outFile>\n"\
"        Overwrites the output file name in a configuration file or\n"\
"        defines the output file name for a settings file.\n"\
"   -tmpdir <dir>\n"\
"        Overwrite the temporary directory in a configuration file or\n"\
"        defines the temporary directory for a settings file.\n"\
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

#define ASF_EXAMPLES_STRING \
"   To create an editable configuration file named 'example.cfg' with some\n"\
"   example values, use a command similar to the following:\n\n"\
"      example> "ASF_NAME_STRING" -create example.cfg\n"\
"\n"\
"   To process level 1 CEOS data using an "ASF_NAME_STRING" style configuration\n"\
"   file named 'config.my_conf', use a command similar to the following:\n\n"\
"      example> "ASF_NAME_STRING" config.my_conf\n"

#define ASF_LIMITATIONS_STRING \
"   Level 0 data is not yet supported.  GeoTIFF SAR files created by ASF tools\n"\
"   do not contain SAR related data and this prevents certain processing:\n"\
"   terrain correction etc. when importing and processing from the GeoTIFF\n"\
"   format.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_import, asf_terrcorr, asf_geocode, asf_export\n"

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
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   %s\n\n",
      version_string(ASF_NAME_STRING));
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
  char configFileName[255], *tmpConfigFile=NULL;
  char *inFile=NULL, *outFile=NULL, *tmpDir=NULL;
  const int pid = getpid();
  int createflag;
  extern int logflag, quietflag;
  int create_f, quiet_f;  /* log_f is a static global */
  int input_f, output_f, tmpdir_f;

  createflag = logflag = quietflag = FALSE;
  create_f = log_f = quiet_f = FLAG_NOT_SET;
  input_f = output_f = tmpdir_f = FLAG_NOT_SET;

  // Begin command line parsing ***********************************************
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  get_asf_share_dir_with_argv0(argv[0]);
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  // This is an undocumented option, for internal use (by the GUI)
  int save_dem = extract_flag_options(&argc, &argv, "-save-dem", "--save-dem", NULL);

  // Check which options were provided
  create_f = checkForOption("-create", argc, argv);
  log_f    = checkForOption("-log", argc, argv);
  quiet_f  = checkForOption("-quiet", argc, argv);
  input_f  = checkForOption("-input", argc, argv);
  output_f = checkForOption("-output", argc, argv);
  tmpdir_f = checkForOption("-tmpdir", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS;               // command & REQUIRED_ARGS
  int num_flags = 0;
  if (create_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (log_f    != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f  != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (input_f  != FLAG_NOT_SET) {needed_args += 2; num_flags++;}
  if (output_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;}
  if (tmpdir_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;}

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

  // Make sure all options occur before the config file name argument
  if (num_flags == 1 &&
      (create_f > 1 ||
       input_f  > 1 ||
       output_f > 1 ||
       tmpdir_f > 1 ||
       log_f    > 1 ||
       quiet_f  > 1))
  {
    print_usage();
  }
  else if (num_flags > 1 &&
           (create_f >= argc - REQUIRED_ARGS - 1 ||
	    input_f  >= argc - REQUIRED_ARGS - 1 ||
	    output_f >= argc - REQUIRED_ARGS - 1 ||
	    tmpdir_f >= argc - REQUIRED_ARGS - 1 ||
            log_f    >= argc - REQUIRED_ARGS - 1 ||
            quiet_f  >= argc - REQUIRED_ARGS - 1))
  {
    print_usage();
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
    //sprintf(logFile, "tmp%i.log", pid);
    strcpy(logFile, get_tmp_log_file("asf_mapready"));
  }
  if (input_f != FLAG_NOT_SET) {
    inFile = (char *) MALLOC(sizeof(char)*512);
    strcpy(inFile, argv[input_f+1]);
  }
  if (output_f != FLAG_NOT_SET) {
    outFile = (char *) MALLOC(sizeof(char)*512);
    strcpy(outFile, argv[output_f+1]);
  }
  if (input_f != FLAG_NOT_SET) {
    tmpDir = (char *) MALLOC(sizeof(char)*512);
    strcpy(tmpDir, argv[tmpdir_f+1]);
  }
  logflag = TRUE;
  fLog = FOPEN(logFile, "a");
  // Set old school quiet flag (for use in our libraries)
  quietflag = quiet_f != FLAG_NOT_SET;

  // Fetch required arguments
  strcpy(configFileName, argv[argc-1]);

  // Update configuration file (if needed)
  if (inFile || outFile || tmpDir) {
    tmpConfigFile = (char *) MALLOC(sizeof(char)*25);
    sprintf(tmpConfigFile, "tmp%i.cfg", pid);
    convert_config *cfg = read_convert_config(configFileName);
    cfg->general->short_config = TRUE;
    if (inFile)
      strcpy(cfg->general->in_name, inFile);
    if (outFile)
      strcpy(cfg->general->out_name, outFile);
    if (tmpDir)
      strcpy(cfg->general->tmp_dir, tmpDir);
    write_convert_config(tmpConfigFile, cfg);
  }

  // Report the command line
  asfSplashScreen(argc, argv);

  // End command line parsing *************************************************

  if (tmpConfigFile)
    asf_convert_ext(createflag, tmpConfigFile, save_dem);
  else
    asf_convert_ext(createflag, configFileName, save_dem);

  // remove log file if we created it (leave it if the user asked for it)
  FCLOSE(fLog);
  if (log_f == FLAG_NOT_SET)
    remove(logFile);

  if (tmpConfigFile) {
    remove_file(tmpConfigFile);
    FREE(tmpConfigFile);
  }
  if (inFile)
    FREE(inFile);
  if (outFile)
    FREE(outFile);
  if (tmpDir)
    FREE(tmpDir);

  return(EXIT_SUCCESS);
}
