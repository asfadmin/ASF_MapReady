#include <asf_contact.h>
#include <asf_license.h>

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
"asf_dem_ingest"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-proj <projFile>] [-pixel-size <pixel size>] [-list]\n"\
"                [-log <logFile>] [-quiet] [-license] [-version] [-help]\n"\
"                <inFile> <outFile> <dem type>\n"

#define ASF_DESCRIPTION_STRING \
"   This tool ingest a variety of DEMs into ASF internal format. Optionally,\n"\
"   the DEMs can be geocoded into a map projection.\n"\
"   The tool knows about the ASTER DEM zip file structure, so it mosaics the\n"\
"   individual tiles together into one file (if provided with a projection file).\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   inFile\n"\
"        The digital elevation model data file.\n"\
"   outFile\n"\
"        The full name of the DEM file in the ASF internal format.\n"\
"   dem type\n"\
"        DEM type: SRTM, ASTER, NED\n"

#define ASF_OPTIONS_STRING \
"   -proj <projFile>\n"\
"        Geocode the DEM(s) with this projection file\n"\
"   -pixel-size <pixel size>\n"\
"        Define the pixel size for the geocoding\n"\
"   -list <file>\n"\
"        The input file is list of (most probably zipped) DEM files.\n"\
"        All these tiles are going to be mosaicked together.\n"\
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

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/


#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_import.h"
#include "asf_vector.h"
#include "proj.h"
#include "asf_contact.h"
#include <unistd.h>
#include "gdal.h"
#include "dem_meta.h"
#include "asf_geocode.h"

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

static char *get_proj_file(char *projFile) 
{
  char location[50], *ret;

  if (strncmp_case(projFile, "ALBERS_EQUAL_AREA_CONIC", 23) == 0)
    strcpy(location, "albers_equal_area_conic");
  else if (strncmp_case(projFile, "EQUIDISTANT", 11) == 0)
    strcpy(location, "equidistant");
  else if (strncmp_case(projFile, "EQUI_RECTANGULAR", 16) == 0)
    strcpy(location, "equi_rectangular");
  else if (strncmp_case(projFile, "LAMBERT_AZIMUTHAL_EQUAL_AREA", 28) == 0)
    strcpy(location, "lambert_azimuthal_equal_area");
  else if (strncmp_case(projFile, "LAMBERT_CONFORMAL_CONIC", 23) == 0)
    strcpy(location, "lambert_conformal_conic");
  else if (strncmp_case(projFile, "MERCATOR", 8) == 0)
    strcpy(location, "mercator");
  else if (strncmp_case(projFile, "POLAR_STEREOGRAPHIC", 19) == 0)
    strcpy(location, "polar_stereographic");
  else if (strncmp_case(projFile, "SINUSOIDAL", 10) == 0)
    strcpy(location, "sinusoidal");
  else if (strncmp_case(projFile, "UTM", 3) == 0)
    strcpy(location, "utm");
  ret = (char *) MALLOC(sizeof(char) *
			(strlen(projFile)*2 + strlen(get_asf_share_dir()) + 5));
  sprintf(ret, "%s%cprojections%c%s%c%s", get_asf_share_dir(), DIR_SEPARATOR,
	  DIR_SEPARATOR, location, DIR_SEPARATOR, projFile);

  return ret;
}

int main(int argc, char *argv[])
{
  char inFile[512], outFile[512], *projFile=NULL, type[10];
  const int pid = getpid();
  extern int logflag, quietflag;
  int quiet_f;  /* log_f is a static global */
  int proj_f, pixel_f, list_f, ii, file_count, list=FALSE;
  char **import_files;
  double pixel_size = -99;

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
  proj_f   = checkForOption("-proj", argc, argv);
  pixel_f  = checkForOption("-pixel-size", argc, argv);
  list_f   = checkForOption("-list", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 3 + REQUIRED_ARGS; // command & REQUIRED_ARGS
  int num_flags = 0;
  if (log_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (proj_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (pixel_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (list_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  
  // Make sure we have the right number of args
  if(argc != needed_args) {
    print_usage();
  }

  // Make sure all options occur before the config file name argument
  if (num_flags == 1 &&
      (log_f   > 1 ||
       quiet_f > 1))
  {
    print_usage();
  }
  else if (num_flags > 1 &&
	   (log_f   >= argc - REQUIRED_ARGS - 1 ||
            quiet_f >= argc - REQUIRED_ARGS - 1))
  {
    print_usage();
  }

  if (proj_f != FLAG_NOT_SET) {
    projFile = (char *) MALLOC(sizeof(char)*512);
    strcpy(projFile, argv[proj_f+1]);
  }
  if (pixel_f != FLAG_NOT_SET) {
    pixel_size = atof(argv[pixel_f+1]);
  }
  if (list_f != FLAG_NOT_SET) {
    list = TRUE;
  }
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
  }
  else {
    // default behavior: log to tmp<pid>.log
    //sprintf(logFile, "tmp%i.log", pid);
    strcpy(logFile, get_tmp_log_file("asf_dem_ingest"));
  }
  logflag = TRUE;
  fLog = FOPEN(logFile, "a");
  // Set old school quiet flag (for use in our libraries)
  quietflag = quiet_f != FLAG_NOT_SET;

  // Fetch required arguments
  strcpy(inFile, argv[argc-3]);
  strcpy(outFile, argv[argc-2]);
  strcpy(type, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  char *tmp_dir = (char *) MALLOC(sizeof(char)*512);
  char *tmpFile = (char *) MALLOC(sizeof(char)*512);
  if (projFile || list ||
      (strcmp_case(type, "ASTER") == 0 &&
       strcmp_case(findExt(inFile), ".ZIP") == 0)) {
    strcpy(tmp_dir, "tmp_dem-");
    strcat(tmp_dir, time_stamp_dir());
    create_clean_dir(tmp_dir);
    sprintf(tmpFile, "%s%cimport", tmp_dir, DIR_SEPARATOR);
    import_dem(inFile, list, tmpFile, type, tmp_dir, 
	       &import_files, &file_count);
  }
  else {
    strcpy(tmp_dir, "./");
    import_dem(inFile, list, outFile, type, tmp_dir, 
	       &import_files, &file_count);
  }
  if (projFile) {
    project_parameters_t pps;
    projection_type_t proj_type; 
    datum_type_t datum;
    spheroid_type_t spheroid;
    if (fileExists(projFile))
      read_proj_file(projFile, &pps, &proj_type, &datum, &spheroid);
    else {
      projFile = get_proj_file(projFile);
      read_proj_file(projFile, &pps, &proj_type, &datum, &spheroid);
    }
    if (file_count > 1)
      asf_mosaic(&pps, proj_type, FALSE, RESAMPLE_BILINEAR, 0.0, datum, 
		 spheroid, pixel_size, FALSE, 0, import_files, outFile, 0.0, 
		 -999, 999, -999, 999, "OVERLAY", FALSE);
    else 
      asf_geocode(&pps, proj_type, FALSE, RESAMPLE_BILINEAR, 0.0, datum, 
		  pixel_size, NULL, tmpFile, outFile, 0.0, FALSE);
  }
  else if (strlen(tmp_dir) > 2) {
    /*
    char *imgFile = (char *) MALLOC(sizeof(char)*512);
    char *metaFile = (char *) MALLOC(sizeof(char)*512);
    for (ii=0; ii<file_count; ii++) {
      sprintf(imgFile, "%s.img", get_basename(import_files[ii]));
      fileCopy(import_files[ii], imgFile);
      sprintf(metaFile, "%s.meta", get_basename(import_files[ii]));
      sprintf(tmpFile, "%s%c%s.meta", 
	      tmp_dir, DIR_SEPARATOR, get_basename(import_files[ii]));
      fileCopy(tmpFile, metaFile);
    }
    FREE(imgFile);
    FREE(metaFile);
    */
    combine(import_files, file_count, outFile);
  }
  for (ii=0; ii<file_count; ii++)
    FREE(import_files[ii]);
  FREE(import_files);
  if (strlen(tmp_dir) > 2)
    remove_dir(tmp_dir);
  FREE(tmp_dir);
  FREE(tmpFile);

  asfPrintStatus("\nSuccessful completion!\n\n");

  FCLOSE(fLog);
  remove(logFile);

  return(EXIT_SUCCESS);

}
