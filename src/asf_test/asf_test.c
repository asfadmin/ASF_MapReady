#include <asf.h>
#include <asf_meta.h>
#include <asf_contact.h>
#include <asf_license.h>
#include <asf_test.h>
#include "CUnit/Automated.h"
#include "CUnit/Basic.h"

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
"asf_test"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create] [-log <logFile> ] [-license] [-version] [-help]\n"\
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
"        comments that help the user with the available settings.\n"

#define ASF_OPTIONS_STRING \
"   -create <config_file>\n"\
"        Create <config_file> instead of reading it.\n"\
"   -log <logFile>\n"\
"        Set the name and location of the log file.\n"\
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


/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#define REQUIRED_ARGS 1
#define FLAG_NOT_SET -1

// Need to be global for unit test framework
char configFile[1024];

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

int init_suite_success(void) { return 0; }
int init_suite_failure(void) { return -1; }
int clean_suite_success(void) { return 0; }
int clean_suite_failure(void) { return -1; }

static void cu_metadata(void)
{
  char inFile[1024], specsFile[1024];
  int ii;
  test_config *cfg = read_test_config(configFile);

  for (ii=0; ii<cfg->general->test_count; ii++) {
    strcpy(inFile, cfg->test[ii]->file);
    strcpy(specsFile, cfg->test[ii]->specs);
    CU_ASSERT_TRUE(meta_test(inFile, specsFile, REPORT_LEVEL_NONE));
  }
  free_test_config(cfg);  
}

static void manual_metadata(void)
{
  char inFile[1024];
  char specsFile[1024];
  int ii, suite_passed = TRUE;
  test_config *cfg = read_test_config(configFile);

  if (strcmp_case(cfg->general->status, "new") != 0)
    asfPrintError("Can't run manual tests. Current status: '%s'\nTo run these "
		  "tests manually, reset the general status to 'new'\n", 
		  cfg->general->status);
  asfPrintStatus("   Suite: %s\n", cfg->general->suite);
  for (ii=0; ii<cfg->general->test_count; ii++) {
    strcpy(inFile, cfg->test[ii]->file);
    strcpy(specsFile, cfg->test[ii]->specs);
    asfPrintStatus("\n   Test[%d]: %s ...\n", ii+1, cfg->test[ii]->test);
    if (strcmp_case(cfg->test[ii]->status, "skip") != 0) {
      if (meta_test(inFile, specsFile, REPORT_LEVEL_STATUS)) {
	asfPrintStatus("   Test passed\n");
	strcpy(cfg->test[ii]->status, "passed");
      }
      else {
	asfPrintStatus("   Test failed\n");
	strcpy(cfg->test[ii]->status, "failed");
	suite_passed = FALSE;
      }
    }
    else
      asfPrintStatus("skipped\n");
  }
  if (suite_passed) {
    asfPrintStatus("\n   Suite passed\n\n");
    strcpy(cfg->general->status, "passed");
  }
  else {
    asfPrintStatus("\n   Suite failed\n\n");
    strcpy(cfg->general->status, "failed");
  }
  write_test_config(configFile, cfg);
  free_test_config(cfg);  
}

static void manual_binary(void)
{
  asfPrintError("Manual binary tests not implemented yet!\n");  
}

int main(int argc, char *argv[])
{
  int createflag;
  extern int logflag;
  int create_f, log_f;

  createflag = FALSE;
  create_f = FLAG_NOT_SET;

  // Begin command line parsing ***********************************************
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  get_asf_share_dir_with_argv0(argv[0]);
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  // Check which options were provided
  create_f = checkForOption("-create", argc, argv);
  log_f    = checkForOption("-log", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS;               // command & REQUIRED_ARGS
  int num_flags = 0;
  if (create_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (log_f    != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param

  // Make sure we have the right number of args
  if (argc != needed_args) {
    print_usage();
  }

  // Make sure argument for each flag (that requires an arg) is not another
  // option flag & is not a required argument
  if (log_f != FLAG_NOT_SET) {
    if ((argv[log_f+1][0]=='-') || (log_f>=(argc-REQUIRED_ARGS))) {
      print_usage();
    }
  }

  // Make sure all options occur before the config file name argument
  if (num_flags == 1 && (create_f > 1 || log_f > 1)) {
    print_usage();
  }
  else if (num_flags > 1 && 
	   (create_f >= argc - REQUIRED_ARGS - 1 ||
	    log_f    >= argc - REQUIRED_ARGS - 1)) {
    print_usage();
  }

  // Do the actual flagging & such for each flag
  if (create_f != FLAG_NOT_SET) {
    createflag = TRUE;
  }
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
    logflag = TRUE;
    fLog = FOPEN(logFile, "w");
  }

  // Fetch required arguments
  strcpy(configFile, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // End command line parsing *************************************************

  // Get test information from configuration file
  test_config *cfg;
  char testcases[255];

  if (createflag && !fileExists(configFile)) {
    init_test_config(configFile);
    return(EXIT_SUCCESS);
  }
  else if (createflag && fileExists(configFile)) {
    cfg = read_test_config(configFile);
    check_return(write_test_config(configFile, cfg),
		 "Could not update configuration file");
    return(EXIT_SUCCESS);
  }
  else if (!fileExists(configFile)) {
    asfPrintStatus("Could not fin config file (%s)\n", configFile);
    return(EXIT_FAILURE);
  }
  
  cfg = read_test_config(configFile);
  sprintf(testcases, "%s testcases", cfg->general->suite);

  if (strcmp_case(cfg->general->interface, "automated") == 0 ||
      strcmp_case(cfg->general->interface, "basic") == 0) {

    // Give a status update
    if (strcmp_case(cfg->general->type, "metadata") == 0)
      asfPrintStatus("Found %d metadata tests in test suite '%s'.\n", 
		     cfg->general->test_count, cfg->general->suite);
    else if (strcmp_case(cfg->general->type, "binary") == 0)
      asfPrintStatus("Found %d binary data tests in test suite '%s'.\n", 
		     cfg->general->test_count, cfg->general->suite);
    else
      asfPrintError("Unknown test type (%s)\n", cfg->general->type);

    // Go about the unit testing
    CU_pSuite pSuite = NULL;
    
    // Initialize test registry
    if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();
    
    // Add suite to registry
    pSuite = CU_add_suite(cfg->general->suite, 
			  init_suite_success, clean_suite_success);
    if (NULL == pSuite) {
      CU_cleanup_registry();
      return CU_get_error();
    }
    
    // Add tests to suite
    if (strcmp_case(cfg->general->type, "metadata") == 0) {
      if (NULL == CU_add_test(pSuite, testcases, cu_metadata)) {
	CU_cleanup_registry();
	return CU_get_error();
      }
    }
    else if (strcmp_case(cfg->general->type, "binary") == 0) {
      asfPrintError("Binary units tests not implemented yet!\n");
    }
    
    if (cfg->general->test_count > 0) {
      // Run all tests
      if (strcmp_case(cfg->general->interface, "automated") == 0) {
	asfPrintStatus("Running tests in automatic mode ...\n\n");
	CU_set_output_filename(cfg->general->suite);
	CU_automated_run_tests();
	CU_list_tests_to_file();
      }
      else if (strcmp_case(cfg->general->interface, "basic") == 0) {
	asfPrintStatus("Running tests in basic mode ...\n\n");
	CU_basic_set_mode(CU_BRM_VERBOSE);
	CU_basic_run_tests();
      }
    }
    
    // Cleanup
    CU_cleanup_registry();
    free_test_config(cfg);
  }
  else if (strcmp_case(cfg->general->interface, "manual") == 0) {
    asfPrintStatus("Running tests in manual mode ...\n\n");

    char type[25];
    strcpy(type, cfg->general->type);
    if (strcmp_case(type, "metadata") != 0 && 
	strcmp_case(type, "binary") != 0)
      asfPrintError("Unknown test type (%s)\n", cfg->general->type);
    free_test_config(cfg);
    
    // Run metadata tests
    if (strcmp_case(type, "metadata") == 0)
      manual_metadata();
    // Run binary tests
    else if (strcmp_case(type, "binary") == 0)
      manual_binary();
  }
  else {
    free_test_config(cfg);    
    asfPrintError("Unknown test interface (%s)\n", cfg->general->interface);
  }

  return(EXIT_SUCCESS);
}
