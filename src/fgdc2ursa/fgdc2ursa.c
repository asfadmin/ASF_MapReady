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
"fgdc2meta"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-create <type> <configFile] [-config <configFile>]\n"\
"                [-log <logFile>] [-quiet] [-license] [-version] [-help]\n"\
"                <inFile>\n"

#define ASF_DESCRIPTION_STRING \
"   This program ingests FGDC compliant metadata into the URSA database.\n"

#define ASF_REQUIRED_ARGUMENTS_STRING \
"   inFile\n"\
"        The full name of the metadata .xml file.\n"

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
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_contact.h"
#include <unistd.h>
#include <ctype.h>
#include "fgdc2ursa.h"
#include "xml_util.h"

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

static int subStr(int start, int length, char *string)
{
  char buf[100];
  strncpy(buf, &string[start], length);
  buf[length] = 0;
  return atoi(buf);
}

static int isNumeric(char *string)
{
  int ii;
  for (ii =0; ii<strlen(string); ii++) {
    if (!isdigit(string[ii]))
      return FALSE;
  }
  return TRUE;
}

static char *checkTime(int year, int month, int day, 
		       int hour, int minute, int second)
{
  int error = FALSE;
  char tmp[255];
  char *message = (char *) MALLOC(sizeof(char)*2048);
  strcpy(message, "");

  if (year < 1900 || year > 2100) {
    error = TRUE;
    sprintf(tmp, 
	    "Year (%i) outside expected range (1900 < year < 2100)\n", year);
    strcat(message, tmp);
  }
  if (month < 1 || month > 12) {
    error = TRUE;
    sprintf(tmp, 
	    "Month (%i) outside expected range (1 <= month <= 12)\n", month);
    strcat(message, tmp);
  }
  if (day < 1 || day > 31) {
    error = TRUE;
    sprintf(tmp, "Day (%i) outside expected range (1 <= day <= 31)\n", day);
    strcat(message, tmp);
  }
  if (hour < 0 || hour > 23) {
    error = TRUE;
    sprintf(tmp, "Hour (%i) outside expected range (0 <= hour < 24)\n", hour);
    strcat(message, tmp);
  }
  if (minute < 0 || minute > 59) {
    error = TRUE;
    sprintf(tmp, "Minute(%i) outside expected range (0 <= minute < 60)\n",
	    minute);
    strcat(message, tmp);
  }
  if (second < 0 || second > 59) {
    error = TRUE;
    sprintf(tmp, "Second(%i) outside expected range (0 <= second < 60)\n",
	    second);
    strcat(message, tmp);
  }
  if (!error) {
    FREE(message);
    message = NULL;
  }

  return message;
}


void getCenterTime(char *start_time, char *end_time, char *center_time)
{
  ymd_date beginDate, endDate;
  hms_time beginTime, endTime;

  sscanf(start_time, "%d-%d-%d %d:%d:%lf",
	 &beginDate.year, &beginDate.month, &beginDate.day,
	 &beginTime.hour, &beginTime.min, &beginTime.sec);
  sscanf(end_time, "%d-%d-%d %d:%d:%lf",
	 &endDate.year, &endDate.month, &endDate.day,
	 &endTime.hour, &endTime.min, &endTime.sec);
  double deltaTime = 
    fabs(time_difference(&beginDate, &beginTime, &endDate, &endTime)) / 2.0;
  add_time(deltaTime, &beginDate, &beginTime);
  sprintf(center_time, "%04d-%02d-%02d %02d:%02d:%02.0lf\n",
	  beginDate.year, beginDate.month, beginDate.day,
	  beginTime.hour, beginTime.min, beginTime.sec);
}

int main(int argc, char *argv[])
{
  char inFile[512], *configFile=NULL, type[10];
  const int pid = getpid();
  extern int logflag, quietflag;
  int quiet_f;  /* log_f is a static global */
  int createFlag = FLAG_NOT_SET; // create configuration file flag
  int configFlag = FLAG_NOT_SET; // use configuration file flag

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

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 1 + REQUIRED_ARGS; // command & REQUIRED_ARGS
  int num_flags = 0;
  if (log_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (createFlag != FLAG_NOT_SET) {needed_args += 3; num_flags++;} // option & params
  if (configFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param

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
    strcpy(logFile, asf_tmp_log_file("fgdc2ursa"));
  }
  logflag = TRUE;
  fLog = FOPEN(logFile, "a");
  // Set old school quiet flag (for use in our libraries)
  quietflag = quiet_f != FLAG_NOT_SET;

  // Fetch required arguments
  strcpy(inFile, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // Initialze structure
  dataset_t *data = (dataset_t *) MALLOC(sizeof(dataset_t));

  strcpy(data->dataset_id, MAGIC_UNSET_STRING);
  strcpy(data->origin, MAGIC_UNSET_STRING);
  strcpy(data->title, MAGIC_UNSET_STRING);
  strcpy(data->online_link, MAGIC_UNSET_STRING);
  data->west_bounding = MAGIC_UNSET_DOUBLE;
  data->east_bounding = MAGIC_UNSET_DOUBLE;
  data->north_bounding = MAGIC_UNSET_DOUBLE;
  data->south_bounding = MAGIC_UNSET_DOUBLE;
  data->near_start_lat = MAGIC_UNSET_DOUBLE;
  data->near_start_lon = MAGIC_UNSET_DOUBLE;
  data->far_start_lat = MAGIC_UNSET_DOUBLE;
  data->far_start_lon = MAGIC_UNSET_DOUBLE;
  data->far_end_lat = MAGIC_UNSET_DOUBLE;
  data->far_end_lon = MAGIC_UNSET_DOUBLE;
  data->near_end_lat = MAGIC_UNSET_DOUBLE;
  data->near_end_lon = MAGIC_UNSET_DOUBLE;
  data->center_lat = MAGIC_UNSET_DOUBLE;
  data->center_lon = MAGIC_UNSET_DOUBLE;
  strcpy(data->processing_level, MAGIC_UNSET_STRING);
  strcpy(data->platform, MAGIC_UNSET_STRING);
  strcpy(data->instrument, MAGIC_UNSET_STRING);
  data->band_count = MAGIC_UNSET_INT;
  data->browse_location = NULL;
  data->browse_format = NULL;
  strcpy(data->access, MAGIC_UNSET_STRING);
  strcpy(data->copyright, MAGIC_UNSET_STRING);
  data->start_time = NULL;
  data->center_time = NULL;
  data->end_time = NULL;
  strcpy(data->orbit_direction, MAGIC_UNSET_STRING);
  strcpy(data->mode, MAGIC_UNSET_STRING);
  strcpy(data->spatial_reference, MAGIC_UNSET_STRING);
  strcpy(data->cell_value, MAGIC_UNSET_STRING);
  strcpy(data->raster_object, MAGIC_UNSET_STRING);
  data->row_count = MAGIC_UNSET_INT;
  data->col_count = MAGIC_UNSET_INT;
  strcpy(data->format, MAGIC_UNSET_STRING);
  data->fees = MAGIC_UNSET_DOUBLE;

  // Open XML for reading
  xmlDoc *doc = xmlReadFile(inFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse metadata file (%s)\n", inFile);

  // Read in all parameters and perform error checking
  char string[512], tmp[255], errorMessage[8192]="Metadata parsing errors:\n";
  int nValue, error = FALSE;
  double fValue;

  // Dataset ID
  strcpy(string, xml_get_string_value(doc, "metadata.idinfo.datsetid"));
  if (strlen(string) > MAX_DATASET_ID) {
    error = TRUE;
    sprintf(tmp, "Dataset ID - String length: %d, allowed characters: %d\n", 
	    (int) strlen(string), MAX_DATASET_ID);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->dataset_id, 
	   xml_get_string_value(doc, "metadata.idinfo.datsetid"));
    //printf("Dataset ID: %s\n", data->dataset_id);
  }

  // Origin
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.citation.citeinfo.origin"));
  if (strlen(string) > MAX_ORIGIN) {
    error = TRUE;
    sprintf(tmp, "Origin - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_ORIGIN);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->origin, xml_get_string_value(doc, 
      "metadata.idinfo.citation.citeinfo.origin"));
    //printf("Origin: %s\n", data->origin);
  }

  // Title
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.citation.citeinfo.title"));
  if (strlen(string) > MAX_TITLE) {
    error = TRUE;
    sprintf(tmp, "Title - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_TITLE);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->title, xml_get_string_value(doc, 
      "metadata.idinfo.citation.citeinfo.title"));
    //printf("Title: %s\n", data->title);
  }

  // Online link
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.citation.citeinfo.onlink"));
  if (strlen(string) > MAX_ONLINE_LINK) {
    error = TRUE;
    sprintf(tmp, "Online link - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_ONLINE_LINK);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->online_link, xml_get_string_value(doc, 
      "metadata.idinfo.citation.citeinfo.onlink"));
    //printf("Online link: %s\n", data->online_link);
  }

  // West bounding coordinate
  fValue = xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.westbc");
  if (fValue < -180.0 || fValue > 180.0) {
    error = TRUE;
    sprintf(tmp, "West bounding coordinate - Value (%.4lf) outside the "
	    "expected value range (-180.0 < value < 180.0)\n", fValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->west_bounding =
      xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.westbc");
    //printf("West bounding coordinate: %.4lf\n", data->west_bounding);
  }

  // East bounding coordinate
  fValue = xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.eastbc");
  if (fValue < -180.0 || fValue > 180.0) {
    error = TRUE;
    sprintf(tmp, "East bounding coordinate - Value (%.4lf) outside the "
	    "expected value range (-180.0 < value < 180.0)\n", fValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->east_bounding =
      xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.eastbc");
    //printf("East bounding coordinate: %.4lf\n", data->east_bounding);
  }

  // North bounding coordinate
  fValue = xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.northbc");
  if (fValue < -90.0 || fValue > 90.0) {
    error = TRUE;
    sprintf(tmp, "West bounding coordinate - Value (%.4lf) outside the "
	    "expected value range (-90.0 < value < 90.0)\n", fValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->north_bounding =
      xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.northbc");
    //printf("West bounding coordinate: %.4lf\n", data->north_bounding);
  }

  // South bounding coordinate
  fValue = xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.southbc");
  if (fValue < -90.0 || fValue > 90.0) {
    error = TRUE;
    sprintf(tmp, "South bounding coordinate - Value (%.4lf) outside the "
	    "expected value range (-90.0 < value < 90.0)\n", fValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->south_bounding =
      xml_get_double_value(doc, "metadata.idinfo.spdom.bounding.southbc");
    //printf("South bounding coordinate: %.4lf\n", data->south_bounding);
  }

  // Open KML for reading
  char centerFile[512], boundaryFile[512], *baseName;
  char coordStr[50];
  float height;
  baseName = get_basename(inFile);
  sprintf(boundaryFile, "%s_boundary.kml", baseName);
  sprintf(centerFile, "%s_center.kml", baseName);

  xmlDoc *xmlBoundary = xmlReadFile(boundaryFile, NULL, 0);
  if (!xmlBoundary)
    asfPrintError("Could not parse boundary file (%s)\n", boundaryFile);
  else {
    strcpy(coordStr, xml_get_string_value(xmlBoundary, 
      "kml.Document.Placemark.Polygon.outerBoundaryIs.LinearRing.coordinates"));    sscanf(coordStr, "%f,%f,%f\n%f,%f,%f\n%f,%f,%f\n%f,%f", 
	   &data->near_start_lat, &data->near_start_lon, &height,
	   &data->near_end_lat, &data->near_end_lon, &height,
	   &data->far_end_lat, &data->far_end_lon, &height,
	   &data->far_start_lat, &data->far_start_lon);
    //printf("Near start latitude: %.4f\n", data->near_start_lat);
    //printf("Near start longitude: %.4f\n", data->near_start_lon);
    //printf("Near end latitude: %.4f\n", data->near_end_lat);
    //printf("Near end longitude: %.4f\n", data->near_end_lon);
    //printf("Far start latitude: %.4f\n", data->far_start_lat);
    //printf("Far start longitude: %.4f\n", data->far_start_lon);
    //printf("Far end latitude: %.4f\n", data->far_end_lat);
    //printf("Far end longitude: %.4f\n", data->far_end_lon);
  }
  xmlFreeDoc(xmlBoundary);

  xmlDoc *xmlCenter = xmlReadFile(centerFile, NULL, 0);
  if (!xmlCenter)
    asfPrintWarning("Could not parse center point file (%s)\n", centerFile);
  else {
    strcpy(coordStr, xml_get_string_value(xmlCenter, 
      "kml.Document.Placemark.Point.coordinates"));
    sscanf(coordStr, "%f,%f", &data->center_lat, &data->center_lon);
    //printf("Center latitude: %.4f\n", data->center_lat);
    //printf("Center longitude: %.4f\n", data->center_lon);
  }
  xmlFreeDoc(xmlCenter);

  // Processing level
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.proclevl.prolevid"));
  if (strlen(string) > MAX_PROCESSING_LEVEL) {
    error = TRUE;
    sprintf(tmp, 
	    "Processing level - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_PROCESSING_LEVEL);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->processing_level, xml_get_string_value(doc, 
      "metadata.idinfo.proclevl.prolevid"));
    //printf("Processing level: %s\n", data->processing_level);
  }

  // Place
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.keywords.place.placekey[0]"));
  if (strlen(string) > MAX_PLACE) {
    error = TRUE;
    sprintf(tmp, 
	    "Place - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_PLACE);
    strcat(errorMessage, tmp);
  }
  else {
    data->place = (char *) MALLOC(sizeof(char)*MAX_PLACE);
    strcpy(data->place, xml_get_string_value(doc, 
      "metadata.idinfo.keywords.place.placekey[0]"));
    //printf("Place: %s\n", data->place);
  }

  // Platform
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.plainsid.platflnm"));
  if (strlen(string) > MAX_PLATFORM) {
    error = TRUE;
    sprintf(tmp, "Platform - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_PLATFORM);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->platform, xml_get_string_value(doc, 
      "metadata.idinfo.plainsid.platflnm"));
    //printf("Platform: %s\n", data->platform);
  }

  // Instrument
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.plainsid.instflnm"));
  if (strlen(string) > MAX_INSTRUMENT) {
    error = TRUE;
    sprintf(tmp, "Instrument - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_INSTRUMENT);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->instrument, xml_get_string_value(doc, 
      "metadata.idinfo.plainsid.instflnm"));
    //printf("Instrument: %s\n", data->instrument);
  }

  // Number of bands
  nValue = xml_get_int_value(doc, "metadata.idinfo.bandidnt.numbands");
  if (nValue < 0) {
    error = TRUE;
    sprintf(tmp, "Number of bands - Value (%i) outside the "
	    "expected value range (value > 0)\n", nValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->band_count =
      xml_get_int_value(doc, "metadata.idinfo.bandidnt.numbands");
    //printf("Number of bands: %i\n", data->band_count);
  }

  // Browse image location
  strcpy(string, xml_get_string_value(doc, "metadata.idinfo.browse.browsen"));
  if (strlen(string) > MAX_BROWSE_LOCATION) {
    error = TRUE;
    sprintf(tmp, "Browse image location - String length: %d, allowed "
	    "characters: %d\n", (int) strlen(string), MAX_BROWSE_LOCATION);
    strcat(errorMessage, tmp);
  }
  else if (strcmp_case(string, MAGIC_UNSET_STRING) != 0) {
    data->browse_location = (char *) MALLOC(sizeof(char)*MAX_BROWSE_LOCATION);
    strcpy(data->browse_location, xml_get_string_value(doc, 
      "metadata.idinfo.browse.browsen"));
    //printf("Browse image location: %s\n", data->browse_location);
  }

  // Browse image format
  strcpy(string, xml_get_string_value(doc, "metadata.idinfo.browse.browset"));
  if (strlen(string) > MAX_BROWSE_FORMAT) {
    error = TRUE;
    sprintf(tmp, "Browse image format - String length: %d, allowed characters:"
	    " %d\n", (int) strlen(string), MAX_BROWSE_FORMAT);
    strcat(errorMessage, tmp);
  }
  else if (strcmp_case(string, MAGIC_UNSET_STRING) != 0) {
    data->browse_format = (char *) MALLOC(sizeof(char)*MAX_BROWSE_FORMAT);
    strcpy(data->browse_format, xml_get_string_value(doc, 
      "metadata.idinfo.browse.browset"));
    //printf("Browse format: %s\n", data->browse_format);
  }

  // Data access level
  strcpy(string, xml_get_string_value(doc, 
    "metadata.idinfo.secinfo.secclass"));
  if (strlen(string) > MAX_ACCESS) {
    error = TRUE;
    sprintf(tmp, "Data access level - String length: %d, allowed characters: "
	    "%d\n", (int) strlen(string), MAX_ACCESS);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->access, xml_get_string_value(doc, 
      "metadata.idinfo.secinfo.secclass"));
    //printf("Data access level: %s\n", data->access);
  }

  // Copyright
  strcpy(string, xml_get_string_value(doc, "metadata.idinfo.copyright"));
  if (strlen(string) > MAX_COPYRIGHT) {
    error = TRUE;
    sprintf(tmp, "Copyright - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_COPYRIGHT);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->copyright, xml_get_string_value(doc, 
      "metadata.idinfo.copyright"));
    //printf("Copyright: %s\n", data->copyright);
  }

  // Start time
  char dateStr[25], timeStr[25], *message;
  int year, month, day, hour, minute, second;
  strcpy(dateStr, xml_get_string_value(doc, 
    "metadata.idinfo.timeperd.timeinfo.rngdates.begdate"));
  year = subStr(0, 4, dateStr);
  month = subStr(4, 2, dateStr);
  day = subStr(6, 2, dateStr);
  strcpy(timeStr, xml_get_string_value(doc, 
    "metadata.idinfo.timeperd.timeinfo.rngdates.begtime"));
  hour = subStr(0, 2, timeStr);
  minute = subStr(2, 2, timeStr);
  second = subStr(4, 2, timeStr);
  if (strcmp_case(dateStr, MAGIC_UNSET_STRING) != 0 &&
      strcmp_case(timeStr, MAGIC_UNSET_STRING) != 0) {
    if (isNumeric(dateStr) && isNumeric(timeStr)) {
      message = checkTime(year, month, day, hour, minute, second);
      if (message) {
	error = TRUE;
	sprintf(tmp, "Start time - %s", message);
	strcat(errorMessage, tmp);
      }
      else {
	data->start_time = (char *) MALLOC(sizeof(char)*MAX_START_TIME);
	sprintf(data->start_time, "%04d-%02d-%02d %02d:%02d:%02d", 
		year, month, day, hour, minute, second);
	//printf("Start time: %s\n", data->start_time);
      }
    }
  }

  // Center time
  strcpy(dateStr, xml_get_string_value(doc, 
    "metadata.idinfo.timeperd.timeinfo.sngdate.caldate"));
  year = subStr(0, 4, dateStr);
  month = subStr(4, 2, dateStr);
  day = subStr(6, 2, dateStr);
  strcpy(timeStr, xml_get_string_value(doc, 
    "metadata.idinfo.timeperd.timeinfo.sngdate.time"));
  hour = subStr(0, 2, timeStr);
  minute = subStr(2, 2, timeStr);
  second = subStr(4, 2, timeStr);
  if (strcmp_case(dateStr, MAGIC_UNSET_STRING) != 0 &&
      strcmp_case(timeStr, MAGIC_UNSET_STRING) != 0) {
    if (isNumeric(dateStr) && isNumeric(timeStr)) {
      message = checkTime(year, month, day, hour, minute, second);
      if (message) {
	error = TRUE;
	sprintf(tmp, "Center time - %s", message);
	strcat(errorMessage, tmp);
      }
      else {
	data->center_time = (char *) MALLOC(sizeof(char)*MAX_CENTER_TIME);
	sprintf(data->center_time, "%04d-%02d-%02d %02d:%02d:%02d", 
		year, month, day, hour, minute, second);
	//printf("Center time: %s\n", data->center_time);
      }
    }
  }

  // End time
  strcpy(dateStr, xml_get_string_value(doc, 
    "metadata.idinfo.timeperd.timeinfo.rngdates.enddate"));
  year = subStr(0, 4, dateStr);
  month = subStr(4, 2, dateStr);
  day = subStr(6, 2, dateStr);
  strcpy(timeStr, xml_get_string_value(doc, 
    "metadata.idinfo.timeperd.timeinfo.rngdates.endtime"));
  hour = subStr(0, 2, timeStr);
  minute = subStr(2, 2, timeStr);
  second = subStr(4, 2, timeStr);
  if (strcmp_case(dateStr, MAGIC_UNSET_STRING) != 0 &&
      strcmp_case(timeStr, MAGIC_UNSET_STRING) != 0) {
    if (isNumeric(dateStr) && isNumeric(timeStr)) {
      message = checkTime(year, month, day, hour, minute, second);
      if (message) {
	error = TRUE;
	sprintf(tmp, "End time - %s", message);
	strcat(errorMessage, tmp);
      }
      else {
	data->end_time = (char *) MALLOC(sizeof(char)*MAX_END_TIME);
	sprintf(data->end_time, "%04d-%02d-%02d %02d:%02d:%02d", 
		year, month, day, hour, minute, second);
	//printf("End time: %s\n", data->end_time);
      }
    }
  }

  // Orbit direction
  nValue = xml_get_int_value(doc, "metadata.dataqual.acqinfo.ascdscin");
  if (nValue != MAGIC_UNSET_INT) {
    if (nValue < 0 || nValue > 1) {
      error = TRUE;
      sprintf(tmp, "Orbit direction - Value (%i) outside the "
	      "expected value range (0 <= value <= 1)\n", nValue);
      strcat(errorMessage, tmp);
    }
    else {
      if (nValue == 0)
	strcpy(data->orbit_direction, "Ascending");
      else if (nValue == 1)
	strcpy(data->orbit_direction, "Descending");
      //printf("Orbit direction: %s\n", data->orbit_direction);
    }
  }
  else
    strcpy(data->orbit_direction, "Unknown");

  // Imaging mode
  strcpy(string, xml_get_string_value(doc, "metadata.idinfo.plainsid.mode"));
  if (strcmp_case(string, MAGIC_UNSET_STRING) != 0) {
    if (strlen(string) > MAX_MODE) {
      error = TRUE;
      sprintf(tmp, "Imaging mode - String length: %d, allowed characters: %d"
	      "\n", (int) strlen(string), MAX_MODE);
      strcat(errorMessage, tmp);
    }
    else {
      strcpy(data->mode, 
	     xml_get_string_value(doc, "metadata.idinfo.plainsid.mode"));
      //printf("Imaging mode: %s\n", data->mode);
    }
  }
  else
    strcpy(data->mode, "Unknown");

  // Spatial reference
  strcpy(string, xml_get_string_value(doc, "metadata.spdoinfo.direct"));
  if (strcmp_case(string, "Point") !=  0 &&
      strcmp_case(string, "Vector") != 0 &&
      strcmp_case(string, "Raster") != 0) {
    error = TRUE;
    sprintf(tmp, "Spatial reference - String (%s) not valid; expected "
	    "\"Point\", \"Vector\" or \"Raster\"\n", string);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->spatial_reference, 
	   xml_get_string_value(doc, "metadata.spdoinfo.direct"));
    //printf("Spatial reference: %s\n", data->spatial_reference);
  }

  // Cell value type
  strcpy(string, xml_get_string_value(doc, 
    "metadata.spdoinfo.rastinfo.cvaltype"));
  if (strlen(string) > MAX_CELL_VALUE) {
    error = TRUE;
    sprintf(tmp, 
	    "Cell value type - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_CELL_VALUE);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->cell_value, xml_get_string_value(doc, 
      "metadata.spdoinfo.rastinfo.cvaltype"));
    //printf("Cell value type: %s\n", data->cell_value);
  }

  // Raster object type
  strcpy(string, xml_get_string_value(doc, 
    "metadata.spdoinfo.rastinfo.rasttype"));
  if (strcmp_case(string, "Point") != 0 &&
      strcmp_case(string, "Pixel") != 0 &&
      strcmp_case(string, "Grid Cell") != 0 &&
      strcmp_case(string, "Voxel") != 0 &&
      strcmp_case(string, "Swath") != 0) {
    error = TRUE;
    sprintf(tmp, "Raster object type - String (%s) not valid; expected "
	    "\"Point\", \"Pixel\", \"Grid Cell\", \"Voxel\" or \"Swath\".\n",
	    string);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->raster_object, xml_get_string_value(doc, 
      "metadata.spdoinfo.rastinfo.rasttype"));
    //printf("Raster object type: %s\n", data->raster_object);
  }

  // Number of rows
  nValue = xml_get_int_value(doc, "metadata.spdoinfo.rastinfo.rowcount");
  if (nValue < 0) {
    error = TRUE;
    sprintf(tmp, "Number of rows - Value (%i) outside the "
	    "expected value range (value > 0)\n", nValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->row_count =
      xml_get_int_value(doc, "metadata.spdoinfo.rastinfo.rowcount");
    //printf("Number of rows: %i\n", data->row_count);
  }

  // Number of columns
  nValue = xml_get_int_value(doc, "metadata.spdoinfo.rastinfo.colcount");
  if (nValue < 0) {
    error = TRUE;
    sprintf(tmp, "Number of columns - Value (%i) outside the "
	    "expected value range (value > 0)\n", nValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->col_count =
      xml_get_int_value(doc, "metadata.spdoinfo.rastinfo.colcount");
    //printf("Number of columns: %i\n", data->col_count);
  }

  // Format name
  strcpy(string, xml_get_string_value(doc, 
    "metadata.distinfo.stdorder.digform.digtinfo.formname"));
  if (strlen(string) > MAX_FORMAT) {
    error = TRUE;
    sprintf(tmp, 
	    "Format name - String length: %d, allowed characters: %d\n",
	    (int) strlen(string), MAX_FORMAT);
    strcat(errorMessage, tmp);
  }
  else {
    strcpy(data->format, xml_get_string_value(doc, 
      "metadata.distinfo.stdorder.digform.digtinfo.formname"));
    //printf("Format name: %s\n", data->format);
  }

  // Fees
  fValue = xml_get_double_value(doc, "metadata.distinfo.stdorder.fees");
  if (fValue < 0.0) {
    error = TRUE;
    sprintf(tmp, "Fees - Value (%.2lf) outside the expected value range "
	    "(value >= 0.0)\n", fValue);
    strcat(errorMessage, tmp);
  }
  else {
    data->fees = xml_get_double_value(doc, "metadata.distinfo.stdorder.fees");
    //printf("Fees: %.2lf\n", data->fees);
  }

  if (error)
    asfPrintError("%s", errorMessage);

  xmlFreeDoc(doc);
  xmlCleanupParser();

  FREE(configFile);
  FCLOSE(fLog);
  remove(logFile);

  // Add dataset to database
  addData(data);

  FREE(data);

  asfPrintStatus("\nSuccessful completion!\n\n");

  return(EXIT_SUCCESS);
}
