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
"asf_fgdc_meta"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create <type> <configFile] [-config <configFile>]\n"\
"                [-kml] [-log <logFile>] [-quiet] [-license] [-version] "\
"[-help]\n"\
"                <inFile> <outFile>\n"

#define ASF_DESCRIPTION_STRING \
"   This program can generates FGDC compatible metadata.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   inFile\n"\
"        The remote sensing data file.\n"\
"   outFile\n"\
"        The full name of the metadata .xml file.\n"\

#define ASF_OPTIONS_STRING \
"   -create <type> <configFile>\n"\
"        Generates a new configuration file of type 'short' or 'long'.\n"\
"   -config <configFile>\n"\
"        Uses the configuration to enrich the metadata.\n"\
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
#include "fgdc_meta.h"

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

static void generateKML(char *inFile, char *outFile)
{
  meta_parameters *meta = NULL;
  meta_projection *proj = NULL;
  GDALDatasetH hGdal = NULL;
  GDALDriverH hDriver = NULL;
  char format[50];

  // Register all known drivers
  GDALAllRegister();

  // Open file
  hGdal = GDALOpen(inFile, GA_ReadOnly);
  if (!hGdal)
    asfPrintError("Failed to open file (%s)\n", inFile);

  // Determine what we are dealing with
  hDriver = GDALGetDatasetDriver(hGdal);
  strcpy(format, GDALGetDriverShortName(hDriver));

  // For CEOS we might need some extra help
  // Just in case some of the parameters are not correctly filled
  // e.g. KSAT raw did not have correct number of samples
  int rowcount, colcount;
  if (strcmp_case(format, "CEOS") == 0) {
    report_level_t level = REPORT_LEVEL_NONE;
    ceos_description *ceos = get_ceos_description(inFile, level);
    if (ceos->product == RAW)
      meta = meta_read_raw(inFile);
    else
      meta = meta_read(inFile);
    FREE(ceos);
    colcount = meta->general->sample_count;
    rowcount = meta->general->line_count;
  }
  else {
    colcount = GDALGetRasterXSize(hGdal);
    rowcount = GDALGetRasterYSize(hGdal);
    proj = gdal2meta_projection(hGdal, rowcount, colcount);
  }
  if (!meta && !proj) {
    asfPrintWarning("No projection information to be extracted.\n"
		    "Can't generate KML files!\n");
    return;
  }

  // For the URSA metadata ingest we will generate KML files that carries
  // the geographic information.
  
  // Setup file names
  char *tmpDir = (char *) MALLOC(sizeof(char)*512);
  sprintf(tmpDir, "kml-");
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);  

  // Create the point file (and polygon file) and convert
  FILE *fp;
  char *baseName = get_basename(outFile);
  char pointFile[512], pointKML[512], polygonFile[512], polygonKML[512];
  double lat, lon, height;
  double startX, startY, centerX, centerY, endX, endY;
  if (proj) {
    startX = proj->startX;
    startY = proj->startY;
    endX = startX + proj->perX*colcount;
    endY = startY + proj->perY*rowcount;
    centerX = startX + (endX - startX)/2;
    centerY = startY + (endY - startY)/2;
  }

  // Write boundary polygon file
  sprintf(polygonFile, "%s%c%s_boundary.csv", 
	  tmpDir, DIR_SEPARATOR, baseName);
  fp = FOPEN(polygonFile, "w");
  if (meta) {
    fprintf(fp, "# Format: POLYGON\n#\n# ID,Latitude,Longitude\n");
    fprintf(fp, "1,%.4lf,%.4lf\n", meta->location->lat_start_near_range,
	    meta->location->lon_start_near_range);
    fprintf(fp, "2,%.4lf,%.4lf\n", meta->location->lat_start_far_range,
	    meta->location->lon_start_far_range);
    fprintf(fp, "3,%.4lf,%.4lf\n", meta->location->lat_end_far_range,
	    meta->location->lon_end_far_range);
    fprintf(fp, "4,%.4lf,%.4lf\n", meta->location->lat_end_near_range,
	    meta->location->lon_end_near_range);
  }
  else {
    proj_to_latlon(proj, startX, startY, 0.0, &lat, &lon, &height);
    fprintf(fp, "1,%.4lf,%.4lf\n", lat*R2D, lon*R2D);
    proj_to_latlon(proj, endX, startY, 0.0, &lat, &lon, &height);
    fprintf(fp, "2,%.4lf,%.4lf\n", lat*R2D, lon*R2D);
    proj_to_latlon(proj, endX, endY, 0.0, &lat, &lon, &height);
    fprintf(fp, "3,%.4lf,%.4lf\n", lat*R2D, lon*R2D);
    proj_to_latlon(proj, startX, endY, 0.0, &lat, &lon, &height);
    fprintf(fp, "4,%.4lf,%.4lf\n", lat*R2D, lon*R2D);
  }
  FCLOSE(fp);
  sprintf(polygonKML, "%s_boundary.kml", baseName);
  polygon2kml(polygonFile, polygonKML, FALSE);

  // Write center point file
  sprintf(pointFile, "%s%c%s_center.csv", tmpDir, DIR_SEPARATOR, baseName);
  fp = FOPEN(pointFile, "w");
  fprintf(fp, "# Format: POINT\n#\n# ID,Latitude,Longitude\n");
  if (meta)
    fprintf(fp, "1,%.4lf,%.4lf\n", meta->general->center_latitude,
	    meta->general->center_longitude);
  else {
    proj_to_latlon(proj, centerX, centerY, 0.0, &lat, &lon, &height);
    fprintf(fp, "1,%.4lf,%.4lf\n", lat*R2D, lon*R2D);
  }
  FCLOSE(fp);
  sprintf(pointKML, "%s_center.kml", baseName);
  point2kml(pointFile, pointKML, FALSE);
  
  // Clean up
  remove_dir(tmpDir);
  FREE(tmpDir);
  FREE(baseName);
  if (proj)
    FREE(proj);
  GDALClose(hGdal);
  GDALDestroyDriverManager();
  if (meta)
    meta_free(meta);
}

int main(int argc, char *argv[])
{
  char inFile[512], outFile[512], *configFile=NULL, type[10];
  const int pid = getpid();
  extern int logflag, quietflag;
  int quiet_f;  /* log_f is a static global */
  int createFlag = FLAG_NOT_SET; // create configuration file flag
  int configFlag = FLAG_NOT_SET; // use configuration file flag
  int kmlFlag = FLAG_NOT_SET;

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
  createFlag = checkForOption("-create", argc, argv);
  configFlag = checkForOption("-config", argc, argv);
  kmlFlag = checkForOption("-kml", argc, argv);

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 2 + REQUIRED_ARGS; // command & REQUIRED_ARGS
  int num_flags = 0;
  if (log_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (createFlag != FLAG_NOT_SET) {needed_args += 3; num_flags++;} // option & params
  if (configFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (kmlFlag != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  
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

  // Make sure that only one option is used
  if (createFlag != FLAG_NOT_SET && configFlag != FLAG_NOT_SET)
    asfPrintError("The tool can either create or use the "
		  "configuration file, not both!\n");

  // Do the actual flagging & such for each flag
  if (createFlag != FLAG_NOT_SET) {
    sprintf(type, "%s", argv[createFlag+1]);
    configFile = (char *) MALLOC(sizeof(char)*1024);
    sprintf(configFile, "%s", argv[createFlag+2]);
  }
  if (configFlag != FLAG_NOT_SET) {
    configFile = (char *) MALLOC(sizeof(char)*1024);
    sprintf(configFile, "%s", argv[configFlag+1]);
  }
  if (log_f != FLAG_NOT_SET) {
    strcpy(logFile, argv[log_f+1]);
  }
  else {
    // default behavior: log to tmp<pid>.log
    //sprintf(logFile, "tmp%i.log", pid);
    strcpy(logFile, get_tmp_log_file("asf_fgdc_meta"));
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

  if (createFlag != FLAG_NOT_SET) {
    if (init_fgdc_config(configFile, type))
      asfPrintError("Could not create configuration file (%s)\n", configFile);
  }
  else if (configFlag != FLAG_NOT_SET)
    import_fgdc(inFile, configFile, outFile);
  else
    import_fgdc(inFile, NULL, outFile);

  if (kmlFlag != FLAG_NOT_SET)
    generateKML(inFile, outFile);

  FREE(configFile);
  FCLOSE(fLog);
  remove(logFile);

  asfPrintStatus("\nSuccessful completion!\n\n");

  return(EXIT_SUCCESS);
}
