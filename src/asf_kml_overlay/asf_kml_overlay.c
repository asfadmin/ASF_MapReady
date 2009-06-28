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
"                [-log <logFile>] [-quiet] [-license] [-version] [-help]\n"\
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

static void create_and_set_tmp_dir(char *in_name, char *out_name, char *tmp_dir)
{
  int length = strlen(in_name)+1;
  if ((strlen(out_name)+1) > length) {
    length = strlen(out_name)+1;
  }
  char *junk     = MALLOC(sizeof(char)*length);
  char *basename = MALLOC(sizeof(char)*length);
  char *out_dir  = MALLOC(sizeof(char)*length);

  split_dir_and_file(in_name, junk, basename);
  split_dir_and_file(out_name, out_dir, junk);
  FREE(junk);

  int tmp_len = strlen(tmp_dir);
  int out_len = strlen(out_dir);

  if (0==tmp_len) {
    if (0==out_len) {
      strcpy(tmp_dir, "");
    }
    else {
      strcpy(tmp_dir, out_dir);
      strcat(tmp_dir, DIR_SEPARATOR_STR);
    }
    strcat(tmp_dir, "kml-");
    strcat(tmp_dir, basename);
    strcat(tmp_dir, "-");
    strcat(tmp_dir, time_stamp_dir());
    create_clean_dir(tmp_dir);
  }
  else {
    DIR *dirp = opendir(tmp_dir);
    if (!dirp)
      create_clean_dir(tmp_dir);
    else
      closedir(dirp);
  }

  set_asf_tmp_dir(tmp_dir);

  FREE(basename);
  FREE(out_dir);
}

int main(int argc, char *argv[])
{
  char inFile[512], outFile[512], demFile[512];
  const int pid = getpid();
  extern int logflag, quietflag;
  int quiet_f;  /* log_f is a static global */
  int tcFlag = FLAG_NOT_SET; // terrain correction flag
  int rgFlag = FLAG_NOT_SET; // refine geolocation only flag

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

  // We need to make sure the user specified the proper number of arguments
  int needed_args = 2 + REQUIRED_ARGS; // command & REQUIRED_ARGS
  int num_flags = 0;
  if (log_f != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (quiet_f != FLAG_NOT_SET) {needed_args += 1; num_flags++;} // option
  if (tcFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param
  if (rgFlag != FLAG_NOT_SET) {needed_args += 2; num_flags++;} // option & param

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

  // Make sure all options occur before the config file name argument
  if (num_flags == 1 &&
      (log_f   > 1 ||
       quiet_f > 1 ||
       tcFlag  > 1 ||
       rgFlag  > 1))
  {
    print_usage();
  }
  else if (num_flags > 1 &&
	   (log_f   >= argc - REQUIRED_ARGS - 1 ||
            quiet_f >= argc - REQUIRED_ARGS - 1 ||
	    tcFlag  >= argc - REQUIRED_ARGS - 1 ||
	    rgFlag  >= argc - REQUIRED_ARGS - 1))
  {
    print_usage();
  }

  // Make sure that the DEM is used for only one thing
  if (tcFlag != FLAG_NOT_SET && rgFlag != FLAG_NOT_SET)
    asfPrintError("The tool can either terrain correct or refine the "
		  "geolocation, not both!\n");

  // Do the actual flagging & such for each flag
  if (tcFlag != FLAG_NOT_SET) {
    sprintf(demFile, "%s", argv[tcFlag+1]);
  }
  if (rgFlag != FLAG_NOT_SET) {
    sprintf(demFile, "%s", argv[rgFlag+1]);
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
  quietflag = quiet_f != FLAG_NOT_SET;

  // Fetch required arguments
  strcpy(inFile, argv[argc-2]);
  strcpy(outFile, argv[argc-1]);

  // Report the command line
  asfSplashScreen(argc, argv);

  // End command line parsing *************************************************

  // Check whether required files actually exist
  char **dataName=NULL, **metaName=NULL;
  int step=1, nBands, trailer;
  require_ceos_pair(inFile, &dataName, &metaName, &nBands, &trailer);

  // Create temporary processing directory
  char *tmpDir = MALLOC(sizeof(char)*1024);
  create_and_set_tmp_dir(inFile, outFile, tmpDir);
  strcat(tmpDir, DIR_SEPARATOR_STR);
  printf("tmpDir: %s\n", tmpDir);

  meta_parameters *meta;
  meta = meta_read(inFile);
  double pixel_size = meta->general->x_pixel_size;
  int sample_count = meta->general->sample_count;
  meta_free(meta);
  while (step) {
    if (sample_count > 750 && sample_count < 1100) {
      step = FALSE;
    }
    pixel_size *= 2.0;
    sample_count /= 2;
  }

  // Generate output names
  char *baseName = get_basename(outFile);
  char metaFile[512], pngFile[512], kmlFile[512], kmzFile[512];
  sprintf(metaFile, "%s.meta", baseName);
  sprintf(pngFile, "%s.png", baseName);
  sprintf(kmlFile, "%s.kml", baseName);
  sprintf(kmzFile, "%s.kmz", baseName);

  // Generating a customized configuration for asf_mapready
  char configFileName[255];
  sprintf(configFileName, "%sasf_mapready.config", tmpDir);
  FILE *fp = FOPEN(configFileName, "w");
  fprintf(fp, "Temporary asf_mapready configuration file\n\n");
  fprintf(fp, "[General]\n");
  fprintf(fp, "input file = %s\n", inFile);
  fprintf(fp, "output file = %s\n", pngFile);
  fprintf(fp, "import = 1\n");
  if (tcFlag != FLAG_NOT_SET || rgFlag != FLAG_NOT_SET)
    fprintf(fp, "terrain correction = 1\n");
  else
    fprintf(fp, "terrain correction = 0\n");
  fprintf(fp, "geocoding = 1\n");
  fprintf(fp, "export =1\n");
  fprintf(fp, "dump envi header = 0\n");
  fprintf(fp, "short configuration file = 1\n\n");
  fprintf(fp, "[Import]\n");
  fprintf(fp, "format = CEOS\n");
  fprintf(fp, "radiometry = AMPLITUDE_IMAGE\n\n");
  if (tcFlag != FLAG_NOT_SET || rgFlag != FLAG_NOT_SET) {
    fprintf(fp, "[Terrain correction]\n");
    fprintf(fp, "digital elevation model = %s\n", demFile);
    fprintf(fp, "pixel spacing = %.2lf\n", pixel_size);
    fprintf(fp, "auto mask water = 1\n");
    fprintf(fp, "water height cutoff = 1.0\n");
    fprintf(fp, "smooth dem holes = 1\n");
    fprintf(fp, "interpolate = 1\n");
    if (rgFlag != FLAG_NOT_SET)
      fprintf(fp, "refine geolocation only = 1\n\n");
    else
      fprintf(fp, "refine geolocation only = 0\n\n");
  }
  fprintf(fp, "[Geocoding]\n");
  fprintf(fp, "projection = %s/projections/equi_rectangular/"
	  "equi_rectangular_world.proj\n", get_asf_share_dir());
  fprintf(fp, "pixel spacing = %.2lf\n", pixel_size);
  fprintf(fp, "force = 1\n\n");
  fprintf(fp, "[Export]\n");
  fprintf(fp, "format = PNG_GE\n");
  fprintf(fp, "byte conversion = SIGMA\n");
  FCLOSE(fp);

  // Run input file through asf_mapready
  asfPrintStatus("\n\nGenerating overlay PNG file ...\n\n");
  asf_convert(FALSE, configFileName);

  // Remove log file if we created it (leave it if the user asked for it)
  if (log_f == FLAG_NOT_SET)
    remove(logFile);

  // Calculate the lat/lon extents from the geocoded browse image
  meta = meta_read(metaFile);
  double startX = meta->projection->startX;
  double startY = meta->projection->startY;
  double perX = meta->projection->perX;
  double perY = meta->projection->perY;
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;

  double lat_UL, lon_UL, lat_UR, lon_UR, lat_LL, lon_LL, lat_LR, lon_LR;
    
  double ul_x = startX;
  double ul_y = startY;
  double ur_x = startX + perX * ns;
  double ur_y = startY;
  double ll_x = startX;
  double ll_y = startY + perY * nl;
  double lr_x = startX + perX * ns;
  double lr_y = startY + perY * nl;
  
  EQR2latLon(ul_x, ul_y, &lat_UL, &lon_UL);
  EQR2latLon(ur_x, ur_y, &lat_UR, &lon_UR);
  EQR2latLon(ll_x, ll_y, &lat_LL, &lon_LL);
  EQR2latLon(lr_x, lr_y, &lat_LR, &lon_LR);
  
  meta_free(meta);

  double north = max4(lat_UL, lat_LL, lat_LR, lat_UR);
  double south = min4(lat_UL, lat_LL, lat_LR, lat_UR);
  double east = max4(lon_UL, lon_LL, lon_LR, lon_UR);
  double west = min4(lon_UL, lon_LL, lon_LR, lon_UR);

  // Generate a configuration file for convert2vector
  asfPrintStatus("\n\nGenerating KML file ...\n\n");
  sprintf(configFileName, "%sconvert2vector.config", tmpDir);
  fp = FOPEN(configFileName, "w");
  fprintf(fp, "[General]\n");
  fprintf(fp, "input file = %s\n", metaFile);
  fprintf(fp, "output file = %s\n", kmlFile);
  fprintf(fp, "input format = META\n");
  fprintf(fp, "output format = KML\n");
  fprintf(fp, "list = 0\n\n");
  fprintf(fp, "[KML]\n");
  fprintf(fp, "time = 0\n");
  fprintf(fp, "boundary = line\n");
  fprintf(fp, "height = clampToGround\n");
  fprintf(fp, "width = 2\n");
  fprintf(fp, "color = ffff9900\n");
  fprintf(fp, "overlay = %s\n", pngFile);
  fprintf(fp, "north = %.4lf\n", north);
  fprintf(fp, "south = %.4lf\n", south);
  fprintf(fp, "east = %.4lf\n", east);
  fprintf(fp, "west = %.4lf\n", west);
  fprintf(fp, "transparency = 50\n");
  FCLOSE(fp);

  // Run configuration file through convert2vector
  c2v_config *cfg = read_c2v_config(configFileName);
  convert2vector(cfg);
  FREE(cfg);

  // Zip the KML and PNG into a KMZ file
  char cmd[512];
  asfPrintStatus("\n\nGenerating KMZ file ...\n\n");
  sprintf(cmd, "zip %s %s %s", kmzFile, kmlFile, pngFile);
  asfSystem(cmd);

  // Clean up
  remove_dir(tmpDir);
  remove_file(kmlFile);
  remove_file(pngFile);
  remove_file(metaFile);
  FREE(tmpDir);
  asfPrintStatus("\nSuccessful completion!\n\n");

  return(EXIT_SUCCESS);
}
