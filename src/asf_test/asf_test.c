#include <asf.h>
#include <asf_meta.h>
#include <asf_contact.h>
#include <asf_license.h>
#include <asf_test.h>
#include <ceos.h>
#include <asf_convert.h>
#include <proj.h>
#include <unistd.h>
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
"   "ASF_NAME_STRING" [-create] [-clean] [-log <logFile> ] [-license]\n"\
"            [-version] [-help] <type> <config_file>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can ingest level one CEOS and GeoTIFF format data, calibrate\n"\
"   it to various radiometries, perform polarimetric decompositions, perform\n"\
"   Faraday Rotation correction, perform terrain correction, geocode it, and\n"\
"   then export it to a variety of graphics file formats. The user is able to\n"\
"   control how "ASF_NAME_STRING" dictates the processiong flow by creating a\n"\
"   configuration file, which must then be edited, which is fed into\n"\
"   "ASF_NAME_STRING" when it is called.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   <type>\n"\
"        Run tests based on a list of configuration files. The <type> defines\n"\
"        the unit test type: automated, basic or manual.\n"\
"   config_file\n"\
"        A configuration file that "ASF_NAME_STRING" uses to find which files\n"\
"        to use for input and output, what options to use, and how the data\n"\
"        should be processed. It is either read or created based on whether or\n"\
"        not the -create option is specified (see below).\n"\
"        NOTE: When a new configuration file is created, it is filled with\n"\
"        comments that help the user with the available settings.\n"

#define ASF_OPTIONS_STRING \
"   -create\n"\
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

#define REQUIRED_ARGS 2
#define FLAG_NOT_SET -1

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


static void manual_metadata(char *configFile)
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
      if (meta_test_ext(inFile, specsFile, REPORT_LEVEL_STATUS)) {
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
      asfPrintStatus("   Test skipped\n");
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

static void manual_geotiff(char *configFile)
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
      if (geotiff_test_ext(inFile, specsFile, REPORT_LEVEL_STATUS)) {
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
      asfPrintStatus("   Test skipped\n");
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

static void manual_binary(char *configFile)
{
  asfPrintError("Manual binary tests not implemented yet!\n");  
}

static void manual_library(char *configFile)
{
  char function[50], specsFile[1024];
  int ii, suite_passed = TRUE;
  test_config *cfg = read_test_config(configFile);

  if (strcmp_case(cfg->general->status, "new") != 0)
    asfPrintError("Can't run manual tests. Current status: '%s'\nTo run these "
		  "tests manually, reset the general status to 'new'\n", 
		  cfg->general->status);
  asfPrintStatus("   Suite: %s\n", cfg->general->suite);
  for (ii=0; ii<cfg->general->test_count; ii++) {
    strcpy(function, cfg->test[ii]->lib->name);
    strcpy(specsFile, cfg->test[ii]->specs);
    asfPrintStatus("\n   Test[%d]: %s ...\n", ii+1, cfg->test[ii]->test);
    if (strcmp_case(cfg->test[ii]->status, "skip") != 0) {
      if (lib_test_ext(function, function, specsFile, REPORT_LEVEL_STATUS)) {
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
      asfPrintStatus("   Test skipped\n");
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

void cu_difftext(char *testFile, char *referenceFile, char *exceptFile)
{
  char line[1024], tLine[1024], rLine[1024];
  int ii, nExcept = 0;

  // Read exceptions
  FILE *fp = FOPEN(exceptFile, "r");
  while(fgets(line, 1024, fp)) {
    chomp(line);
    nExcept++;
  }
  FCLOSE(fp);
  char **exception = (char **) MALLOC(sizeof(char *)*nExcept);
  fp = FOPEN(exceptFile, "r");
  for (ii=0; ii<nExcept; ii++) {
    exception[ii] = (char *) MALLOC(sizeof(char)*1024);
    fgets(exception[ii], 1024, fp);
  }
  FCLOSE(fp);

  // Go through both text files simultaneously
  FILE *fpTest = FOPEN(testFile, "r");
  FILE *fpRef = FOPEN(referenceFile, "r");
  while (fgets(tLine, 1024, fpTest)) {
    fgets(rLine, 1024, fpRef);
    for (ii=0; ii<nExcept; ii++) {
      if (strcmp_case(rLine, exception[ii]) != 0) {
	if (strcmp_case(tLine, rLine) != 0) {
	  asfForcePrintStatus("\ntest: %sreference: %s", tLine, rLine);
	  CU_ASSERT_TRUE(strcmp_case(tLine, rLine) == 0);
	}
      }
    }
  }

  // Clean up
  for (ii=0; ii<nExcept; ii++)
    FREE(exception[ii]);
  FREE(exception);
}

void cu_diffimage(char *testFile, char *referenceFile)
{
  char **bands1 = NULL, **bands2 = NULL;
  int ii, num_bands1, num_bands2, complex = 0;
  stats_t *stats1 = NULL, *stats2 = NULL;
  complex_stats_t *complex_stats1 = NULL, *complex_stats2 = NULL;
  psnr_t *psnrs = NULL;
  complex_psnr_t *complex_psnr = NULL;
  shift_data_t *data_shift = NULL;
  CU_ASSERT_FALSE(diffimage(testFile, referenceFile, NULL, NULL, 
			    &bands1, &bands2, &num_bands1, &num_bands2, 
			    &complex, &stats1, &stats2, 
			    &complex_stats1, &complex_stats2, 
			    &psnrs, &complex_psnr, &data_shift));

  // We assume here that both images have the same bands
  if (complex) {
    for (ii=0; ii<num_bands1; ii++) {
      if (!complex_stats1->i.stats_good) {
	asfForcePrintStatus("\nTest file statistics test failed!\n");
	asfForcePrintStatus("File name         : %s (%s) real part\n", 
			    testFile, bands1[ii]);
	asfForcePrintStatus("Minimum value     : %lf\n", complex_stats1->i.min);
	asfForcePrintStatus("Maximum value     : %lf\n", complex_stats1->i.max);
	asfForcePrintStatus("Mean value        : %lf\n", 
			    complex_stats1->i.mean);
	asfForcePrintStatus("Standard deviation: %lf\n", 
			    complex_stats1->i.sdev);
      }
      CU_ASSERT_TRUE(complex_stats1->i.stats_good);
      if (!complex_stats1->q.stats_good) {
	asfForcePrintStatus("\nTest file statistics test failed!\n");
	asfForcePrintStatus("File name         : %s (%s) imaginary part\n", 
			    testFile, bands1[ii]);
	asfForcePrintStatus("Minimum value     : %lf\n", complex_stats1->q.min);
	asfForcePrintStatus("Maximum value     : %lf\n", complex_stats1->q.max);
	asfForcePrintStatus("Mean value        : %lf\n", 
			    complex_stats1->q.mean);
	asfForcePrintStatus("Standard deviation: %lf\n", 
			    complex_stats1->q.sdev);
      }
      CU_ASSERT_TRUE(complex_stats1->q.stats_good);
      if (!complex_stats2->i.stats_good) {
	asfForcePrintStatus("\nReference file statistics test failed!\n");
	asfForcePrintStatus("File name         : %s (%s) real part\n", 
			    referenceFile, bands2[ii]);
	asfForcePrintStatus("Minimum value     : %lf\n", complex_stats2->i.min);
	asfForcePrintStatus("Maximum value     : %lf\n", complex_stats2->i.max);
	asfForcePrintStatus("Mean value        : %lf\n", 
			    complex_stats2->i.mean);
	asfForcePrintStatus("Standard deviation: %lf\n", 
			    complex_stats2->i.sdev);
      }
      CU_ASSERT_TRUE(complex_stats2->i.stats_good);
      if (!complex_stats2->q.stats_good) {
	asfForcePrintStatus("\nReference file statistics test failed!\n");
	asfForcePrintStatus("File name         : %s (%s) imaginary "
			    "part\n", referenceFile, bands2[ii]);
	asfForcePrintStatus("Minimum value     : %lf\n", complex_stats2->q.min);
	asfForcePrintStatus("Maximum value     : %lf\n", complex_stats2->q.max);
	asfForcePrintStatus("Mean value        : %lf\n", 
			    complex_stats2->q.mean);
	asfForcePrintStatus("Standard deviation: %lf\n", 
			    complex_stats2->q.sdev);
      }
      CU_ASSERT_TRUE(complex_stats2->q.stats_good);
      if (!complex_psnr->i.psnr_good) {
	asfForcePrintStatus("\nPeak signal-to-noise ratio test failed!\n");
	asfForcePrintStatus("PSNR (%s): %lf\n", 
			    bands1[ii], complex_psnr->i.psnr);
      }
      CU_ASSERT_TRUE(complex_psnr->i.psnr_good);
      if (!complex_psnr->q.psnr_good) {
	asfForcePrintStatus("\nPeak signal-to-noise ratio test failed!\n");
	asfForcePrintStatus("PSNR (%s): %lf\n", 
			    bands1[ii], complex_psnr->q.psnr);
      }
      CU_ASSERT_TRUE(complex_psnr->q.psnr_good);
    }
  }
  else {
    for (ii=0; ii<num_bands1; ii++) {
      if (!stats1->stats_good) {
	asfForcePrintStatus("\nTest file statistics test failed!\n");
	asfForcePrintStatus("File name         : %s (%s)\n", 
			    testFile, bands1[ii]);
	asfForcePrintStatus("Minimum value     : %lf\n", stats1->min);
	asfForcePrintStatus("Maximum value     : %lf\n", stats1->max);
	asfForcePrintStatus("Mean value        : %lf\n", stats1->mean);
	asfForcePrintStatus("Standard deviation: %lf\n\n", stats1->sdev);
      }
      CU_ASSERT_TRUE(stats1->stats_good);
      if (!stats2->stats_good) {
	asfForcePrintStatus("\nReference file statistics test failed!\n");
	asfForcePrintStatus("File name         : %s (%s)\n", 
			    referenceFile, bands2[ii]);
	asfForcePrintStatus("Minimum value     : %lf\n", stats2->min);
	asfForcePrintStatus("Maximum value     : %lf\n", stats2->max);
	asfForcePrintStatus("Mean value        : %lf\n", stats2->mean);
	asfForcePrintStatus("Standard deviation: %lf\n", stats2->sdev);
      }
      CU_ASSERT_TRUE(stats2->stats_good);
      if (!psnrs->psnr_good) {
	asfForcePrintStatus("\nPeak signal-to-noise ratio test failed!\n");
	asfForcePrintStatus("PSNR (%s): %lf\n", bands1[ii], psnrs->psnr);
      }
      CU_ASSERT_TRUE(psnrs->psnr_good);
    }
  }
  if (!data_shift->cert_good) {
    asfForcePrintStatus("\nGeolocation certainty test failed!\n");
    asfForcePrintStatus("Correlation certainty: %.2f\n", 
			data_shift->cert*100.0);
    CU_ASSERT_TRUE(data_shift->cert_good);
  }
  if (!data_shift->dxdy_good) {
    float radial = sqrt(data_shift->dx * data_shift->dx + 
			data_shift->dy * data_shift->dy);
    asfForcePrintStatus("\nGeolocation shift test failed!\n");
    asfForcePrintStatus("dx    : %10.6f\n", data_shift->dx);
    asfForcePrintStatus("dy    : %10.6f\n", data_shift->dy);
    asfForcePrintStatus("radial: %10.6f\n", radial);
    CU_ASSERT_TRUE(data_shift->dxdy_good);
  }
  for (ii=0; ii<num_bands1; ii++)
    FREE(bands1[ii]);
  FREE(bands1);
  for (ii=0; ii<num_bands2; ii++)
    FREE(bands2[ii]);
  FREE(bands2);
  if (stats1)
    FREE(stats1);
  if (stats2)
    FREE(stats2);
  if (complex_stats1)
    FREE(complex_stats1);
  if (complex_stats1)
    FREE(complex_stats1);
  if (psnrs)
    FREE(psnrs);
  if (complex_psnr)
    FREE(complex_psnr);
  FREE(data_shift);
}

void cleanup_data(char *inFile)
{
  char line[1024];
  FILE *fp = FOPEN(inFile, "r");
  while(fgets(line, 1024, fp)) {
    chomp(line);
    remove_file(line);
  }
  FCLOSE(fp);
}

void cleanup_test_results(char *configFile)
{
  char line[1024];
  FILE *fp = FOPEN(configFile, "r");
  while(fgets(line, 1024, fp)) {
    if (strcmp_case(trim_spaces(line), "uavsar_geotiff") == 0)
      cleanup_data("uavsar/geotiff/test_results.lst");
    if (strcmp_case(trim_spaces(line), "alos_browse") == 0)
      cleanup_data("alos/browse/test_results.lst");
    if (strcmp_case(trim_spaces(line), "alos_leader") == 0)
      cleanup_data("alos/leader/test_results.lst");
    if (strcmp_case(trim_spaces(line), "rsat1_geotiff") == 0)
      cleanup_data("end2end/geotiff/test_results.lst");
    if (strcmp_case(trim_spaces(line), "rsat1_map_projections") == 0)
      ;
    if (strcmp_case(trim_spaces(line), "rsat1_overlay") == 0)
      cleanup_data("rsat1/overlay/test_results.lst");
  }
  FCLOSE(fp);
}

int main(int argc, char *argv[])
{
  char configFile[1024], interface[25];
  int createflag, unitflag, cleanflag;
  extern int logflag;
  int create_f, clean_f, log_f;

  createflag = unitflag = cleanflag = FALSE;
  create_f = clean_f, FLAG_NOT_SET;

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
  clean_f  = checkForOption("-clean", argc, argv);
  log_f    = checkForOption("-log", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS;               // command & REQUIRED_ARGS
  int num_flags = 0;
  if (create_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (clean_f  != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
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
  if (clean_f != FLAG_NOT_SET) {
    cleanflag = TRUE;
  }
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
    logflag = TRUE;
    fLog = FOPEN(logFile, "w");
  }

  // Fetch required arguments
  strcpy(interface, argv[argc-2]);
  strcpy(configFile, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // End command line parsing *************************************************

  // Get test information from configuration file
  test_config *cfg;
  char line[1024];

  // Creating configuration files
  if (createflag && !fileExists(configFile)) {
    init_test_config(configFile);
    return(EXIT_SUCCESS);
  }
  else if (createflag && fileExists(configFile)) {
    cfg = read_test_config(configFile);
    check_return(write_test_config(configFile, cfg),
		 "Could not update configuration file");
    free_test_config(cfg);    
    return(EXIT_SUCCESS);
  }
  else if (!fileExists(configFile))
    asfPrintError("Could not find config file (%s)\n", configFile);
  
  // Unit tests or single configuration file?
  if (strcmp_case(interface, "basic") == 0 || 
      strcmp_case(interface, "automated") == 0)
    unitflag = TRUE;

  if (unitflag) {

    extern int quietflag;
    quietflag = 2;
    if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();

    int test = FALSE;
    FILE *fpList = FOPEN(configFile, "r");
    while(fgets(line, 1024, fpList)) {
      if (strcmp_case(trim_spaces(line), "uavsar_metadata") == 0)
	add_uavsar_metadata_tests();
      if (strcmp_case(trim_spaces(line), "uavsar_geotiff") == 0)
	add_uavsar_geotiff_tests();
      if (strcmp_case(trim_spaces(line), "rsat1_map_projections") == 0)
	add_rsat1_map_projections_tests();
      if (strcmp_case(trim_spaces(line), "rsat1_geotiff") == 0)
	add_rsat1_geotiff_tests();
      if (strcmp_case(trim_spaces(line), "alos_browse") == 0)
	add_alos_browse_tests();
      if (strcmp_case(trim_spaces(line), "alos_leader") == 0)
	add_alos_leader_tests();
      if (strcmp_case(trim_spaces(line), "rsat1_overlay") == 0)
	add_rsat1_overlay_tests();
      if (strcmp_case(trim_spaces(line), "alos_calibration") == 0)
	add_alos_calibration_tests();
      test = TRUE;
    }
    FCLOSE(fpList);

    if (test && strcmp_case(interface, "basic") == 0) {
      asfPrintStatus("Running tests in basic mode ...\n");
      CU_basic_set_mode(CU_BRM_VERBOSE);
      if (CUE_SUCCESS != CU_basic_run_tests()) {
	CU_cleanup_registry();
	return CU_get_error();
      }
    }
    if (test && strcmp_case(interface, "automated") == 0) {
      asfPrintStatus("Running tests in automated mode ...\n\n");
      CU_set_output_filename("asf_tools");
      CU_automated_run_tests();
      CU_list_tests_to_file();
    }
    CU_cleanup_registry();
    if (cleanflag)
      cleanup_test_results(configFile);
  }
  else { // Configuration file for manual mode
    cfg = read_test_config(configFile);
    asfPrintStatus("Running tests in manual mode ...\n\n");

    // Run metadata tests
    if (strcmp_case(cfg->general->type, "metadata") == 0)
      manual_metadata(configFile);
    // Run geotiff tests
    else if (strcmp_case(cfg->general->type, "geotiff") == 0)
      manual_geotiff(configFile);
    // Run binary tests
    else if (strcmp_case(cfg->general->type, "binary") == 0)
      manual_binary(configFile);
    // Run library tests
    else if (strcmp_case(cfg->general->type, "library") == 0)
      manual_library(configFile);
    
    free_test_config(cfg);
  }

  return(EXIT_SUCCESS);
}
