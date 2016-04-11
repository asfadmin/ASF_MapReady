#include <asf_contact.h>
#include <asf_license.h>

/*
 *  Status: PROJECT HALTED ON 22-APRIL-2009 BY OFFICIAL DECREE FROM SCOTT ARKO
 *
 *  Remaining To-Dos for Minimum Test and Validation:
 *  - The program does not provide good output yet.  Images are blank and filled with
 *    a single greyscale value
 *  - The program does not yet insert a colortable into the output metadata file yet.
 *    The output metadata should have a IDL Colormap #33 inserted into it (Google it)
 *  - A single typical RADARSAT-1 image takes a very long time to run, something around
 *    half an hour with current speed-ups.  MORE speed-up is needed.  The solution is to
 *    calculate windspeeds for a set of radar cross-sections that span min to max for the
 *    image, across a set of incidence angles that span min to max for the image.  This
 *    will effectively produce a 2D grid of windspeed values into which we can bilinearly
 *    interpolate output pixel windspeeds (as a function of each pixel's incidence angle
 *    and cross-section.)  The code below finds the min/max incidence angle and cross-section
 *    but does not yet create the interpolation table nor has the code for quickly determining
 *    proper indices into the table (for the interpolation) exist yet.
 *  => ALL RESULTS NEED TO BE VALIDATED AGAINST RESULTS FROM FRANK MONALDO'S IDL CODE
 *
 *  To-Dos for Producing a Full-Featured Wind Speed Utility:
 *  - The current rendition only supports RSAT1 using the CMOD5 algorithm, both HH and VV
 *    poloarizations.  All other platforms supported by Frank Monaldo's IDL version need to
 *    be added: ERSx, PALSAR, TERRASAR-X.
 *  - In support of the item above, the XMOD and LMOD algorithms need to be ported from IDL
 *    into C as well.
 *  - For the -cmod4 flag, the CMOD4 algorithm needs to be ported to C as well so it can
 *    be used for C-band data.
 *  - As of right now, there is no land mask support.  Wind speed values will be calculated
 *    for land mass pixels as well as over water, but the results over the land regions
 *    will be meaningless.  The program needs to be able to utilize an externally generated
 *    land mask file much as asf_terrcorr utilizes an externally generated water mask, so
 *    it can avoid processing over land (faster too.)  COMPLICATING FACTORS: Either include
 *    the original land data and add complexity to the application of a colormap, OR fill
 *    the land regions with no-data values.  It would be nice to have the results in a
 *    graphics file format with the water regions colored according to the colormap, but
 *    the original land region data displayed in greyscale.  I will leave these decisions
 *    and complexities to whoever picks up this task in the future ...if anyone ever does.
 *  - There is no reason that a DEM cannot be provided, and maybe a water height, for the
 *    purpose of automatically generating a land mask during processing.  See the -dem
 *    option below.
 *  => ALL RESULTS NEED TO BE VALIDATED AGAINST RESULTS FROM FRANK MONALDO'S IDL CODE
 *
 *  'Good luck with that!' and 'So long and thanks for all the fish!'
 */

// Defaults
#define DEFAULT_LANDMASK_HEIGHT "1.0"
#define DEFAULT_HH_POL_ALPHA 0.6
// See call to ws_inv_cmod5() on line 206 for min/max windspeed setting for cmod5
#define MIN_CMOD5_WINDSPEED 1.0
#define MAX_CMOD5_WINDSPEED 70.0
#define WINDSPEED_BAND_BASENAME "WINDSPEED-"
// See Frank Monaldo's ws_sig2ws.pro (February 2009 version), lines 126-130 for limits
// FIXME: Need to check with Frank and find out why the latitude constraints...
#define MIN_CMOD4_LATITUDE 16.0
#define MAX_CMOD4_LATITUDE 54.0

/*==================BEGIN ASF DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines.
*/
#define ASF_NAME_STRING \
"asf_windspeed"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" -wind-dir <wind direction> [-band <band_id | all>] [-colormap <file>]\n"\
"                 [-log <logFile>] [-cmod4] [-landmask <maskFile> || -landmask-height <height>]\n"\
"                 [-dem <dem file>] [-quiet] [-real-quiet] [-license] [-version]\n"\
"                 [-help]\n"\
"                 <inBaseName> <outBaseName>\n"

#define ASF_DESCRIPTION_STRING \
"   Ingests RADARSAT-1, ALOS PALSAR, TERRASAR-X, and ERS-1 varieties of data in ASF Internal format\n"\
"   in HH or, VV polarizations (only), calculates windspeed from normalized\n"\
"   (calibrated sigma nought) radar cross section, and outputs ASF Internal format metadata\n"\
"   and data files in floating point format (including an imbedded color table.) The wind speed\n"\
"   calculation will use an appropriate algorithm based on the wavelength of the data,\n"\
"   i.e. CMOD5 for C-band data (unless forced to CMOD4 with the -cmod4 switch) etc.  These\n"\
"   algorithms were developed for the VV polarization, but if an HH polarization is being\n"\
"   processed then a modified version developed for HH polarizations by Dr. Frank M. Monaldo of\n"\
"   John Hopkins University, Applied Physics Laboratory will be applied.  If a land mask file is\n"\
"   provided then it will be utilized for masking out land regions, i.e. land regions will have\n"\
"   pixel values set to a no-data value.  By default, "ASF_NAME_STRING" will automatically\n"\
"   develop a land mask by assuming that any terrain higher than "DEFAULT_LANDMASK_HEIGHT"\n"\
"   meters is land (unless another value is provided with the -landmask-height option.)  The\n"\
"   floating point values represent calculated wind speed in meters per second (m/s) at each\n"\
"   pixel location.  The color table is available for creating a pseudo-color display of and/or\n"\
"   graphics file output (see asf_export and asf_view.)  When applying the color table, these\n"\
"   tools will round the floating point values to the nearest byte value then utilize that byte\n"\
"   value as a look-up index into the color table.  "ASF_NAME_STRING" can also perform\n"\
"   several other tasks during look up such as ingesting individual bands at a time, force a\n"\
"   particular look-up table rather than the built-in default (from Dr. Frank M. Monaldo of John\n"\
"   Hopkins University, Applied Physics Laboratory), apply latitude constraints, etcetera.\n"\
"\n"\
"   NOTE: Automatic land mask determination requires a DEM file, i.e. when specifying\n"\
"   -landmask-height you must also specify a DEM file with the -dem option.  If you specify only\n"\
"   a DEM file, then the default height (see above) for determining where land is will be applied\n"\
"   to the DEM in order to automatically develop a land mask.  Alternatively, you may use the\n"\
"   -landmask option to specify a land mask file (see Appendix B in the MapReady manual.)  Finally,\n"\
"   if you do not use either of these two methods to tell "ASF_NAME_STRING" how to mask out land\n"\
"   areas, then "ASF_NAME_STRING" will apply the windspeed algorithm to the entire image as though it\n"\
"   were all water.  This is the default behavior.\n"\

#define ASF_INPUT_STRING \
"   A default single wind direction estimate must be provided with the -wind-dir parameter.  Note \n"\
"   that future releases will likely support utilizing wind direction grids in various formats.\n"

#define ASF_OUTPUT_STRING \
"   Outputs data and metadata files with the user-provided base name and\n"\
"   appropriate extensions (.img and .meta) as described above.\n"

#define ASF_OPTIONS_STRING \
"   -wind-dir <wind direction>\n"\
"        A best estimate average wind direction (for the entire image) must be\n"\
"        provided.  The wind direction is the direction it is coming FROM, not\n"\
"        the direction it is blowing towards.  Wind direction value can range from\n"\
"        0 to 360, with 0 meaning directly from the North.\n"\
"   -band <band_id | all>\n"\
"        If the data contains multiple data files, one for each band (channel)\n"\
"        then ingest and process the band identified by 'band_id' (only).  If 'all' is\n"\
"        specified rather than a band_id, then import all available bands into\n"\
"        a single ASF-format file.  Default is '-band all'.\n"\
"   -colormap <colormap_file>\n"\
"        Associates a color map (RGB index) with the resultant wind speed file, specifically the\n"\
"        windspeed bands within the output file.  Results in the default colormap being replaced\n"\
"        with the user-specified map for the purpose of applying pseudo-color to the output,\n"\
"        i.e. with asf_view or asf_export.  The colormap files must exist in the application\n"\
"        installation 'share' directory's look up table directory.  You may provide your own\n"\
"        colormap by placing it in this folder.  The file format must either be in ASF format\n"\
"        (.lut) or in JASC-PAL (.pal) format.  See existing look-up tables for examples.\n"\
"   -log <logFile>\n"\
"        Output will be written to a specified log file.\n"\
"   -cmod4\n"\
"        For C-band data (only), force the windspeed calculation to use the CMOD4 algorithm\n"\
"        rather than the default CMOD5 algorithm.  Note that this only applies to C-band data\n"\
"        and as usual, if the polarization is \"HH\" rather than \"VV\" then the horizontal\n"\
"        polarization version of the CMOD4 algorithm will be applied (per Dr. Frank M. Monaldo,\n"\
"        John Hopkins University, Applied Physics Laboratory) .\n"\
"   -landmask <maskFile>\n"\
"        A land mask file can be used to force the processing to ignore areas where land exists.\n"\
"        Normally, a default land mask is generated by assuming all elevations greater than the\n"\
"        default of "DEFAULT_LANDMASK_HEIGHT", or if the -landmask-height option is utilized, the\n"\
"        land mask is automatically generated by assuming that all terrain higher than the\n"\
"        height setting is assumed to be land.  If a land mask file is created (ASF Internal\n"\
"        Format or GeoTIFF - See Appendix B in the ASF MapReady User Manual), then all areas\n"\
"        in the mask that have pixels set to '1' are included in the windspeed processing and\n"\
"        all areas that have pixels set to '0' are ignored in the processing.  A no-data value\n"\
"        will be written to all pixels in the output file that are ignored due to masking.  Use\n"\
"        the instructions in Appendix B of the ASF MapReady User Manual to generate the land mask\n"\
"        but rather than setting water regions to '0' and land regions to '1', do the opposite.\n"\
"        Set land regions to '0' so they will be ignored and set water regions to '1' so they\n"\
"        will be included for processing.\n"\
"   -landmask-height <height>\n"\
"        Sets minimum height below which all data is assumed to be water.  Pixels with terrain\n"\
"        equal to or higher than this height will not be processed for wind speed determination.\n"\
"        See -landmask option above for more information.  Also see the -dem option. When using\n"\
"        the -landmask-height option, then you must also specify a DEM with the -dem option below.\n"\
"   -dem <dem basename>\n"\
"        The DEM file used for automatic land mask generation.  See -landmask and -landmask-height.\n"\
"   -quiet\n"\
"        Supresses all non-essential output.\n"\
"   -real-quiet\n"\
"        Supresses all output.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print a help page and exit.\n\n"\

#define ASF_EXAMPLES_STRING \
"   To produce an ASF Internal Format file containing per-pixel wind speed values\n"\
"   from ASF format dataset, assuming a wind direction from the NW, run:\n\n"\
"   Example>\n"\
"     "ASF_NAME_STRING" -wind-dir 315 in_basename out_basename\n"\
"\n"\

#define ASF_NOTES_STRING \
"\n"

#define ASF_LIMITATIONS_STRING \
"   The input data must be calibrated to sigma nought, not in decibels, in ground range geometry,\n"\
"   in HH or VV polarizations, and from one of the following platforms: RSAT1, ERS1, ALOS PALSAR,\n"\
"   or TERRASAR-X.\n"\

#define ASF_SEE_ALSO_STRING \
"   asf_mapready, asf_import, asf_view, asf_export\n"

/*===================END ASF DOCUMENTATION===================*/

#include "asf_import.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "ceos.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "asf_raster.h"
#include <ctype.h>

#define REQUIRED_ARGS 2

#define FLAG_SET 1
#define FLAG_NOT_SET -1

/* Index keys for all flags used in this program via a 'flags' array */
typedef enum {
    f_LOG = 1,
    f_QUIET,
    f_REAL_QUIET,
    f_BAND,
    f_COLORMAP,
    f_WINDDIR,
    f_CMOD4,
    f_LANDMASK,
    f_LANDMASK_HEIGHT,
    f_DEM,
    NUM_WINDSPEED_FLAGS
} windspeed_flag_indices_t;

/* Platform/data type/wavelength related types */
typedef enum {
   p_RSAT1 = 1,
   p_PALSAR, // Not supported yet
   p_TERRASARX, // Not supported yet
   p_ERS1, // Not supported yet
   p_ERS2, // Not supported yet
   NUM_PLATFORM_TYPES
} platform_type_t;

/* Prototypes */
int asf_windspeed(platform_type_t platform_type, char *band_id,
                  double wind_dir, int cmod4,
                  double landmaskHeight, char *landmaskFile, char *demFile,
                  char *inBaseName, char *colormapName, char *outBaseName);
double asf_r_look(meta_parameters *md);
int ws_inv_cmod5(double sigma0, double phi0, double theta0,
                 double *wnd1, double *wnd2,
                 double min_ws, double max_ws, int npts,
                 double hh);
double *ws_cmod5(double wd[], int npts, double wdir, double incid, double r_look);
double arr_min(double *arr, int n);
double arr_max(double *arr, int n);
double ws_pol_ratio(double theta, double alpha);

// IDL look-alikes
int ll2rb(double lon_r, double lat_r,
          double lon_t, double lat_t,
          double *range, double *bearing);
int polrec3d(double radius, double theta, double phi,
             double *x, double *y, double *z);
int rot_3d(int axis, double x, double y, double z, double angle,
           double *xrot, double *yrot, double *zrot);
int recpol3d(double x, double y, double z, double *r, double *az, double *ax);
int recpol(double x, double y, double *r, double *a);
double *maken(double first, double last, int num);
double *poly_fit(double *y, double *x,
                 int ix1, int ix2, int ix3, int degree,
                 double *fit);

/* Helpful functions */

/* Check to see if an option was supplied or not
   If it was found, return its argument number
   Otherwise, return FLAG_NOT_SET */
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
      "Input:\n" ASF_INPUT_STRING "\n"
      "Output:\n"ASF_OUTPUT_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "Notes:\n" ASF_NOTES_STRING "\n"
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n",
      DEFAULT_RANGE_SCALE);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    char inBaseName[256]="";
    char outBaseName[256]="";
    char *colormapName = NULL;
    char band_id[256]="";
    double wind_dir = 315.0; // Default to wind from the NW
    int cmod4 = 0;
    char *landmaskFile = NULL;
    double landmaskHeight = atof(DEFAULT_LANDMASK_HEIGHT);
    platform_type_t platform_type;
    char *demFile = NULL;
    int ii;
    int flags[NUM_WINDSPEED_FLAGS];

    /* Set all flags to 'not set' */
    for (ii=0; ii<NUM_WINDSPEED_FLAGS; ii++) {
        flags[ii] = FLAG_NOT_SET;
    }

    /**********************BEGIN COMMAND LINE PARSING STUFF**********************/
    if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
        || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
        || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
        print_help();
    }
    handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

    /*Check to see if any options were provided*/
    flags[f_LOG] = checkForOption("-log", argc, argv);
    flags[f_QUIET] = checkForOption("-quiet", argc, argv);
    flags[f_REAL_QUIET] = checkForOption("-real-quiet", argc, argv);
    flags[f_BAND] = checkForOption("-band", argc, argv);
    flags[f_COLORMAP] = checkForOption("-colormap", argc, argv);
    flags[f_WINDDIR] = checkForOption("-wind-dir", argc, argv);
    flags[f_CMOD4] = checkForOption("-cmod4", argc, argv);
    flags[f_LANDMASK] = checkForOption("-landmask", argc, argv);
    flags[f_LANDMASK_HEIGHT] = checkForOption("-landmask-height", argc, argv);
    flags[f_DEM] = checkForOption("-dem", argc, argv);

    { /*We need to make sure the user specified the proper number of arguments*/
        int needed_args = 1 + REQUIRED_ARGS;    /*command + REQUIRED_ARGS*/
        if(flags[f_LOG] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
        if(flags[f_QUIET] != FLAG_NOT_SET)    needed_args += 1;/*option*/
        if(flags[f_REAL_QUIET] != FLAG_NOT_SET) needed_args += 1;/*option*/
        if(flags[f_BAND] != FLAG_NOT_SET)     needed_args += 2;/*option & parameter*/
        if(flags[f_COLORMAP] != FLAG_NOT_SET)   needed_args += 2; /*option & parameter*/
        if(flags[f_WINDDIR] != FLAG_NOT_SET) needed_args += 2; /*option & parameter*/
        if(flags[f_CMOD4] != FLAG_NOT_SET) needed_args += 2; /*option & parameter*/
        if(flags[f_LANDMASK] != FLAG_NOT_SET) needed_args += 2; /*option & parameter*/
        if(flags[f_LANDMASK_HEIGHT] != FLAG_NOT_SET) needed_args += 2; /*option & parameter*/
        if(flags[f_DEM] != FLAG_NOT_SET) needed_args += 2; /*option & parameter*/

        /*Make sure we have enough arguments*/
        if(argc != needed_args)
            print_usage();/*This exits with a failure*/
    }

    if(flags[f_LOG] != FLAG_NOT_SET)
        /*Make sure the field following -log isn't another option*/
        if(   argv[flags[f_LOG]+1][0] == '-'
            || flags[f_LOG] >= argc-REQUIRED_ARGS)
            print_usage();
    if(flags[f_BAND] != FLAG_NOT_SET)
      /*Make sure the field following -format isn't another option*/
      if(   argv[flags[f_BAND]+1][0] == '-'
            || flags[f_BAND] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_COLORMAP] != FLAG_NOT_SET)
      /*Make sure the field following -colormap isn't another option*/
      if(   argv[flags[f_COLORMAP]+1][0] == '-'
            || flags[f_COLORMAP] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_WINDDIR] != FLAG_NOT_SET)
      if(   argv[flags[f_WINDDIR]+1][0] == '-'
            || flags[f_WINDDIR] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_CMOD4] != FLAG_NOT_SET)
      if(   argv[flags[f_CMOD4]+1][0] == '-'
            || flags[f_CMOD4] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_LANDMASK] != FLAG_NOT_SET)
      if(   argv[flags[f_LANDMASK]+1][0] == '-'
            || flags[f_LANDMASK] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_LANDMASK_HEIGHT] != FLAG_NOT_SET)
      if(   argv[flags[f_LANDMASK_HEIGHT]+1][0] == '-'
            || flags[f_LANDMASK_HEIGHT] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_DEM] != FLAG_NOT_SET)
      if(   argv[flags[f_DEM]+1][0] == '-'
            || flags[f_DEM] >= argc-REQUIRED_ARGS)
        print_usage();

    /* Be sure to open log ASAP */
    if(flags[f_LOG] != FLAG_NOT_SET)
        strcpy(logFile, argv[flags[f_LOG] + 1]);
    else /*default behavior: log to tmp<pid>.log*/
        //sprintf(logFile, "tmp%i.log", (int)getpid());
        strcpy(logFile, get_tmp_log_file("asf_windspeed"));
    logflag = TRUE; /* Since we always log, set the old school logflag to true */

    // Open log file in output folder
    char path[1024], tmp[1024];
    split_dir_and_file(argv[argc-1], path, tmp);
    strcpy(tmp, logFile);
    sprintf(logFile, "%s%s", path, tmp);
    fLog = fopen(logFile, "a");
    if ( fLog == NULL ) {
      logflag = FALSE;
    }

    /* Set old school quiet flag (for use in our libraries) */
    quietflag = flags[f_QUIET] != FLAG_NOT_SET;
    if (flags[f_REAL_QUIET] != FLAG_NOT_SET) quietflag = 2;

    /* We must be close to good enough at this point... log & quiet flags are set
       Report what was retrieved at the command line */
    asfSplashScreen(argc, argv);

    if(flags[f_COLORMAP] != FLAG_NOT_SET) {
      colormapName = (char *) MALLOC(sizeof(char)*1024);
      strcpy(colormapName, argv[flags[f_COLORMAP] + 1]);
    }
    if (flags[f_WINDDIR] != FLAG_NOT_SET) {
      wind_dir = atof(argv[flags[f_WINDDIR]+1]);
    }
    if (flags[f_LANDMASK] != FLAG_NOT_SET) {
      landmaskFile = (char *) MALLOC(sizeof(char)*1024);
      strncpy(landmaskFile, argv[flags[f_LANDMASK]], 8);
    }
    if (flags[f_LANDMASK_HEIGHT] != FLAG_NOT_SET) {
      landmaskHeight = atof(argv[flags[f_LANDMASK_HEIGHT]]);
    }
    if (flags[f_DEM] != FLAG_NOT_SET) {
      demFile = (char *) MALLOC(sizeof(char)*1024);
      strncpy(demFile, argv[flags[f_DEM]], 8);
    }

    // Check validity
    if (flags[f_LANDMASK] != FLAG_NOT_SET &&
        (flags[f_LANDMASK_HEIGHT] != FLAG_NOT_SET || flags[f_DEM] != FLAG_NOT_SET)) {
      asfPrintStatus("\nCannot use the -landmask option together with -landmask-height\n"
                     "or -dem.\n\n");
      print_usage();
    }
    if (wind_dir < 0.0 || wind_dir >= 360.0) {
      asfPrintError("Wind direction specified with the -wind-dir argument must range\n"
          "from 0.0 up to, but not including, 360.0.  Wind direction is specified\n"
          "as being the direction the wind blows FROM in degrees clockwise from\n"
          "North, with North being 0.0 degrees.\n");
    }
    if (landmaskHeight <= 0.0) {
      asfPrintStatus("\nLandmask height set with -landmask-height must be a positive height\n\n");
      print_usage();
    }

    if(flags[f_BAND] != FLAG_NOT_SET) {
      strcpy(band_id, argv[flags[f_BAND] + 1]);
      if (strlen(band_id) && strcmp("ALL", uc(band_id)) == 0) {
        strcpy(band_id, "");
      }
    }

    /* Fetch required arguments */
    strcpy(inBaseName,  argv[argc - 2]);
    strcpy(outBaseName, argv[argc - 1]);

    /***********************END COMMAND LINE PARSING STUFF***********************/

    asfSplashScreen (argc, argv);

    // Check the metadata to find out if this is data we an process
    meta_parameters *md = meta_read(inBaseName);
    meta_general *mg = md->general; // convenience ptr
    meta_sar *ms = md->sar; // convenience ptr
    if ((strncmp(mg->sensor, "RSAT-1", 6) != 0 &&
         strncmp(mg->sensor, "ERS1",   4) != 0 &&
         strncmp(mg->sensor, "ALOS",   4) != 0 &&
         strncmp(mg->sensor, "TSX-1",  5) != 0) ||
        strncmp(mg->sensor_name, "SAR", 3) != 0)
    {
      asfPrintError("Only SAR products from RSAT1, ERS1, ALOS PALSAR, and TERRASAR\n"
          "are supported.  Found sensor %s, sensor_name %s in metadata.\n",
          mg->sensor, mg->sensor_name);
    }
    if (mg->radiometry != r_SIGMA) {
      asfPrintError("Only sigma-nought products (not in decibels) are currently supported.\n"
          "See asf_import -help for more info on how to produce these from \n"
          "original format data.\n");
    }
    if (!strstr(mg->bands, "HH") && !strstr(mg->bands, "VV")) {
      asfPrintError("Only HH and VV polarizations are supported.  Found bands %s\n",
                    mg->bands);
    }
    if (ms->image_type != 'G') {
      asfPrintError("Only ground range images are currently supported.  Found %s image\n",
                    (ms->image_type == 'R') ? "GEOREFERENCED" :
                    (ms->image_type == 'S') ? "SLANT RANGE"   :
                    (ms->image_type == 'P') ? "MAP PROJECTED" : "UNKNOWN TYPE");
    }

    // As-yet unimplemented feature bail-outs...
    if (strncmp(mg->sensor, "RSAT-1", 6) != 0) {
      // Temporary bail-out until we add the other platforms
      asfPrintError("Platforms other than RSAT1 not yet supported...\n");
    }
    if (flags[f_COLORMAP]        != FLAG_NOT_SET ||
        flags[f_CMOD4]           != FLAG_NOT_SET ||
        flags[f_LANDMASK]        != FLAG_NOT_SET ||
        flags[f_LANDMASK_HEIGHT] != FLAG_NOT_SET ||
        flags[f_DEM]             != FLAG_NOT_SET)
    {
      asfPrintError("The following options are not yet supported:\n"
          "  -colormap\n"
          "  -cmod4\n"
          "  -landmask\n"
          "  -landmask-height\n"
          "  -dem\n");
    }

    // Determine platform type
    // RSAT-1 (C-band), ERS1 (C-band), ALOS (L-band), TSX-1 (X-band)
    platform_type = (strncmp(mg->sensor, "RSAT-1", 6) == 0) ? p_RSAT1     :
                    (strncmp(mg->sensor, "ERS1",   4) == 0) ? p_ERS1      :
                    (strncmp(mg->sensor, "ALOS",   4) == 0) ? p_PALSAR    :
                    (strncmp(mg->sensor, "TSX-1",  5) == 0) ? p_TERRASARX : 0;
    meta_free(md);

    asf_windspeed(platform_type, band_id, wind_dir, cmod4,
                  landmaskHeight, landmaskFile, demFile,
                  inBaseName, colormapName, outBaseName);

    FREE(colormapName);
    FREE(landmaskFile);
    FREE(demFile);

    /* If the user didn't ask for a log file then we can nuke the one that
       we've been keeping since we've finished everything  */
    if (logflag) {
        fclose (fLog);
        remove(logFile);
    }

    exit(EXIT_SUCCESS);
}

int asf_windspeed(platform_type_t platform_type, char *band_id,
                  double wind_dir, int cmod4,
                  double landmaskHeight, char *landmaskFile, char *demFile,
                  char *inBaseName, char *colormapName, char *outBaseName)
{
  char *inDataName, outDataName[1024], outMetaName[1024];
  FILE *in = NULL, *out = NULL;

  asfPrintStatus("\n   Determining windspeeds in: %s\n", inBaseName);

  strcpy(outDataName, outBaseName);
  strcpy(outMetaName, outBaseName);
  inDataName = (char *)MALLOC(sizeof(char) * (strlen(inBaseName) + 10));
  strcpy(inDataName, inBaseName);
  append_ext_if_needed(inDataName, ".img", NULL);
  append_ext_if_needed(outDataName, ".img", NULL);
  append_ext_if_needed(outMetaName, ".meta", NULL);

  // New images for processing in to out
  meta_parameters *imd = meta_read(inBaseName);
  meta_general *img = imd->general; // convenience ptr
  meta_parameters *omd = meta_read(inBaseName);
  meta_general *omg = omd->general; // convenience ptr
  meta_sar *oms = omd->sar; // convenience ptr
  omg->band_count = 0;
  strcpy(omg->bands, "");
  strcpy(oms->polarization, "");
  if (strstr(img->bands, "VV") == NULL && strstr(img->bands, "HH") == NULL) {
    asfPrintError("Cannot find any VV or HH polarized bands in this data.  Available\n"
        "bands are %s).  Wind speeds can only be determined on Sigma0-\n"
        "calibrated SAR data in HH or VV polarizations.\n", img->bands);
  }
  in = (FILE *)FOPEN(inDataName, "rb");
  out = (FILE *)FOPEN(outDataName, "wb");
  FREE(inDataName);

  // For each band
  double alpha = 1.0; // Default for VV polarization;
  int band_num;
  float *data = (float *)MALLOC(sizeof(float) * img->sample_count);
  for (band_num = 0; band_num < img->band_count; band_num++) {
    // Get band name, check for proper polarization, and create new output bandname, set alpha
    char *band_name = get_band_name(img->bands, img->band_count, band_num);
    long offset = img->line_count * band_num;
    char polarization[2]="";
    if (strncmp_case(band_name, "SIGMA-VV", 8) == 0 ||
        strncmp_case(band_name, "SIGMA-HH", 8) == 0)
    {
      asfPrintStatus("\nProcessing wind speed calculations on band %s...\n\n", band_name);

      (omg->band_count)++;
      strcpy(polarization, (strstr(band_name, "VV") != NULL) ? "VV" : "HH");
      strcpy(oms->polarization, polarization);
      sprintf(&omg->bands[strlen(omg->bands)], "%s%s%s", WINDSPEED_BAND_BASENAME, polarization,
              (band_num < img->band_count - 1 && img->band_count > 0) ? ", " : "");
      alpha = (strcmp(polarization, "VV") == 0) ? 1.0 : DEFAULT_HH_POL_ALPHA; // For CMODx
    }
    else {
      asfPrintStatus("\nFound band: %s (Cannot calculate wind speed on this type of band)\n\n",
                     band_name);
      continue; // Skip this band
    }

    // Calculate average r_look for entire image (r_look is the angle between the NADIR line
    // and a line point directly north, the 'look angle' of the platform.)
    double r_look = asf_r_look(imd);
    double phi_diff = wind_dir - r_look;

    // Pre-populate incidence angles (as a function of sample) and get min/max incidence angle
    // as well
    int line, sample;
    double *incids = (double *)MALLOC(img->sample_count * sizeof(double));
    double min_incid = DBL_MAX;
    double max_incid = DBL_MIN;
    for (sample = 0; sample < img->sample_count; sample++) {
      incids[sample] = R2D * meta_incid(imd, img->line_count / 2, sample);
      min_incid = (incids[sample] < min_incid) ? incids[sample] : min_incid;
      max_incid = (incids[sample] > max_incid) ? incids[sample] : max_incid;
    }

    // Get min/max radar cross-sections
    asfPrintStatus("\nFinding min/max radar cross-sections...\n\n");
    double rcs_min = DBL_MAX;
    double rcs_max = DBL_MIN;
    for (line = 0; line < img->line_count; line++) {
      // Get a line
      get_float_line(in, imd, line+offset, data);
      for (sample = 0; sample < img->sample_count; sample++) {
        if (meta_is_valid_double(data[sample]) && data[sample] >= 0.0) {
          rcs_min = (data[sample] < rcs_min) ? data[sample] : rcs_min;
          rcs_max = (data[sample] > rcs_max) ? data[sample] : rcs_max;
        }
      }
      asfLineMeter(line, img->line_count);
    }

    // FIXME: Generate 2D array of windspeeds here.  One dimension is incidence angle and
    // the other is radar cross-section.  The values in the table are wind speed as a function
    // of incidence angle and radar cross-section (given the provided wind direction.)  The idea
    // is to more-quickly populate a grid of results and then to interpolate results for each
    // pixel of the image rather then perform the full calculation (very sloooow)

    double windspeed1 = 0.0, windspeed2 = 0.0;
    for (line = 0; line < img->line_count; line++) {
      // Get a line
      get_float_line(in, imd, line+offset, data);
      for (sample = 0; sample < img->sample_count; sample++) {
        // FIXME: Here is where we should apply a land mask ...in this if-statement expression
        if (meta_is_valid_double(data[sample]) && data[sample] >= 0.0) {
          // Calculate windspeed
          // FIXME: This returns the angle, at the target pixel location, between straight up
          // and the line to the satellite.  Make sure Frank's code doesn't assume the angle
          // between the line to the satellite and a horizontal line, i.e. 90 degrees minus
          // this angle.
          double incidence_angle = incids[sample];
          switch (platform_type) {
            case p_RSAT1:
              if (!cmod4) {
                // Use CMOD5 to calculate windspeeds
                double hh = alpha;
                ws_inv_cmod5((double)data[sample], phi_diff, incidence_angle,
                             &windspeed1, &windspeed2,
                             (double)MIN_CMOD5_WINDSPEED, (double)MAX_CMOD5_WINDSPEED, 25,
                             hh);
                data[sample] = windspeed1; // When 2 answers exist, take the lower (per Frank Monaldo)
              }
              else {
                // Use CMOD4 to calculate windspeeds
                asfPrintError("The CMOD4 algorithm is not yet supported.  Avoid the -cmod4\n"
                    "option for now and let %s default to using the CMOD5 algorithm\n"
                    "instead.\n");
              }
              break;
            case p_PALSAR:
            case p_TERRASARX:
            case p_ERS1:
            case p_ERS2:
            default:
              asfPrintError("Found a platform type (%s) that is not yet supported.\n",
                            (platform_type == p_PALSAR)    ? "PALSAR" :
                            (platform_type == p_TERRASARX) ? "TerraSAR-X" :
                            (platform_type == p_ERS1)      ? "ERS-1" :
                            (platform_type == p_ERS2)      ? "ERS-2" : "UNKNOWN PLATFORM");
          }
        }
      }
      put_float_line(out, omd, line+offset, data);
      asfLineMeter(line, img->line_count);
    }
  } // end for (each band)
  FREE(data);

  // Insert colormap into metadata

  meta_write(omd, outMetaName);
  meta_free(imd);
  meta_free(omd);

  asfPrintStatus("Windspeed calculation complete.\n\n");

  return EXIT_SUCCESS;
}

// Lat/lon to range and bearing, ll2rb()
// Calculates range and bearing from reference lat/lon (lat_r, lon_r)
// to target lat/lon (lat_t, lon_t).  Lat/lon should be in degrees.
// Latitude is from -90.0 to +90.0 degrees.  Longitude is from -180.0 to
// +180.0 degrees.  Range is in radians, bearing is in degrees.
// Assumes the earth is a sphere.  To convert range to distance on the
// surface of the sphere, multiply it by the radius of the sphere.
#define X_AXIS 1
#define Y_AXIS 2
#define Z_AXIS 3
int ll2rb(double lon_r, double lat_r,
          double lon_t, double lat_t,
          double *range, double *bearing)
{
  double radius = 1.0;
  double x1, y1, z1,
         x2, y2, z2,
         x3, y3, z3;

  // Fix for someone who passed in longitudes that go from -180 to +180 instead of 0-360
  double _lon_r = (lon_r < 0.0) ? lon_r + 360.0 : lon_r;
  double _lon_t = (lon_t < 0.0) ? lon_t + 360.0 : lon_t;

  if (lat_r  < -90.0 || lat_r >   90.0 ||
      lat_t  < -90.0 || lat_t >   90.0 ||
      _lon_r <   0.0 || lon_r >= 360.0 ||
      _lon_t <   0.0 || lon_t >= 360.0)
  {
    asfPrintError("ll2rb() latitude/longitude out of range.  Found:\n"
        " (lat_r, lon_r) = (%0.2f, %0.2f)\n"
        " (lat_t, lon_t) = (%0.2f, %0.2f)\n",
        lat_r, lon_r, lat_t, lon_t);
  }
  polrec3d(radius, (90.0 - lat_t) * D2R,  _lon_t * D2R, &x1, &y1, &z1);
  rot_3d(Z_AXIS, x1, y1, z1, -(180.0 - _lon_r) * D2R, &x2, &y2, &z2);
  rot_3d(Y_AXIS, x2, y2, z2, -(90.0 - lat_r) * D2R, &x3, &y3, &z3);
  recpol3d(x3, y3, z3, &radius, range, bearing);
  g_assert(*bearing > 0.0); // Should never happen.  Drill down to recpol() below.
  g_assert((*bearing * R2D) <= 360.0); // Ditto...
  *bearing = fmod(360.0 - (*bearing * R2D), 360.0);

  return 0;
}
#undef X_AXIS
#undef Y_AXIS
#undef Z_AXIS

// Convert spherical coordinates to cartesian.  Assumes
// radius is in the units of your choosing, theta and phi
// in radians.
int polrec3d(double radius, double theta, double phi,
             double *x, double *y, double *z)
{
  *x = sin(theta) * cos(phi);
  *y = sin(theta) * sin(phi);
  *z = cos(theta);

  return 0;
}

// Assumes angle is in radians.  Axis is defined by X = 1, Y = 2, and Z = 3
int rot_3d(int axis,
           double x, double y, double z, double angle,
           double *xrot, double *yrot, double *zrot)
{
  double c = cos(angle);
  double s = sin(angle);

  switch (axis) {
    case 1:
      // Rotate around the X-Axis
      *xrot =          x;
      *yrot =  c*y + s*z;
      *zrot = -s*y + c*z;
      break;
    case 2:
      // Rotate around the Y-Axis
      *xrot =  c*x - s*z;
      *yrot =          y;
      *zrot =  s*x + c*z;
      break;
    case 3:
      // Rotate around the Z-Axis
      *xrot =  c*x + s*y;
      *yrot = -s*x + c*y;
      *zrot =          z;
      break;
    default:
      // Should never reach here (duhhhh)
      asfPrintError("Bad axis number (%d).  Use 1 for X axis, 2 for Y axis, and 3 for Z axis.\n",
                    axis);
      break;
  }

  return 0;
}

// Converts cartesian vector to spherical polar form.  Angle az is the angle from the
// Z-axis and ax is the angle from the X-axis, both in radians. r is the output radius.
int recpol3d(double x, double y, double z, double *r, double *az, double *ax)
{
  double rxy;
  recpol(x,   y, &rxy, ax);
  recpol(z, rxy,    r, az);

  return 0;
}

// Convert 2D rectangular coordinates (x, y) to polar coordinates (r, a)
// Returned angle ranges from 0 to 2PI
int recpol(double x, double y, double *r, double *a)
{
  *a = atan2(y, x); // Returns -PI to PI
  *a = (*a < 0.0) ? *a + 2.0*PI : *a;
  *r = sqrt(x*x + y*y);

  return 0;
}

double asf_r_look(meta_parameters *md)
{
  double r_look = 0.0;
  meta_general *mg = md->general;
  meta_sar *ms = md->sar;
  meta_location *ml = md->location;
  g_assert(ms != NULL); // Should already be checked long before calling this function
  g_assert(ml != NULL); // Will have to use meta_get_latLon() if this ever goes false...
  char look_direction = ms->look_direction; // 'L' or 'R'
  char orbit_direction = mg->orbit_direction; // 'A' or 'D'
  double range = 0.0, bearing = 0.0;

  // Get reference and target lat/lon's
  double near_start_lat = ml->lat_start_near_range; // Reference latitude for ll2rb
  double near_start_lon = ml->lon_start_near_range; // Reference longitude  "  "
  double near_end_lat   = ml->lat_end_near_range;   // Target latitude  "  "
  double near_end_lon   = ml->lon_end_near_range;   // Target longitude  "  "
  double far_start_lon  = ml->lon_start_far_range;

  // Trick the look direction for the math below, if necessary
  look_direction = (orbit_direction == 'A' && near_start_lon > far_start_lon) ? 'L' : look_direction;
  look_direction = (orbit_direction == 'D' && near_start_lon < far_start_lon) ? 'L' : look_direction;

  // Find direction of ground track from lat/lon pair
  ll2rb(near_start_lon, near_start_lat,
        near_end_lon, near_end_lat,
        &range, &bearing);
  bearing = (look_direction == 'R') ? bearing + 90.0 : bearing + 270.0;

  // Radar look direction from bearing
  r_look = fmod(bearing, 360.0);

  return r_look;
}

#define WND1_IS_ONLY_SOLUTION  -99
#define WND_FROM_MAX_SIGMA0   -999
#define WND1_IS_MINIMUM_WIND -9999
#define SIGN(a)  (((a) < 0.0) ? (-1.0) : (((a) > 0.0) ? (1.0) : (0.0)))
#ifndef MIN
#define MIN(a,b)  (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a,b)  (((a) > (b)) ? (a) : (b))
#endif
int ws_inv_cmod5(double sigma0, double phi0, double theta0,
                 double *wnd1, double *wnd2,
                 double min_ws, double max_ws, int npts,
                 double hh)
{
  // ws_cmod5 will return a 2-element array where the first element is the sigma0
  // that you'd get at min_ws, and the second element is the sigma0 that you get
  // at max_ws;
  // (phi0 is phi_diff, the diff between wind_dir and r_look, and theta0 is incidence angle)
  // Note that the 0.0 (last parameter) is r_look according to ws_cmod5, but the ws_cmod5 function
  // doesn't make use of it ...or more accurately, it's already built into the phi0 value.
  g_assert(max_ws > min_ws);
  g_assert(npts > 0);
  double *wd;
  int i;

  // Make an npts-element array of windspeeds that range from min_ws to max_ws linearly
  wd = maken(min_ws, max_ws, npts);
  g_assert(wd != NULL);

  // Calculate an npts-element array of normalized radar cross section (NRCS) using
  // the CMOD5 algorithm
  double *sg0 = ws_cmod5(wd, npts, phi0, theta0, 0.0); // Returned sg0[] has npts elements in it
  g_assert(sg0 != NULL);

  // See lines 118-123 in ws_inv_cmod5.pro
  // NOTE: The CMOD5 and CMOD4 algorithms need an adjustment when the polarization
  // is HH (since the original algorithms were developed for VV polarization only)
  // hh == -3 when polarization is VV, hh == 0.6 when polarization is HH
  // FIXME: Add cases for hh eq to -1 and -2
  double rr = (hh > 0.0) ? ws_pol_ratio(theta0, hh) : 1.0;
  sigma0 = (hh != -3) ? sigma0 / rr : sigma0; // HH adjustment or not

  // If sigma0 is lower than what you'd get at minimum windspeed, then set the windspeed to
  // the minimum and return. (Line 129)
  double min_nrcs = arr_min(sg0, npts); // Min should be sg0[0], but make sure...
  if (sigma0 < min_nrcs) {
    *wnd1 = wd[0];
    *wnd2 = WND1_IS_MINIMUM_WIND;
    FREE(wd);
    FREE(sg0);
    return 0;
  }

  // Create sign of differences array
  int ct=0;
  double *s = (double *)MALLOC(sizeof(double) * npts);
  for (i=0; i<npts; i++) {
    s[i] = SIGN(sg0[i] - sigma0);
    ct += s[i] == 0 ? 1 : 0;
    s[i] = (ct > 0 && s[i] == 0) ? 1.0 : s[i];
  }

  // Count the sign changes
  ct = 0;
  int ww = -1; // Where first sign change occurs
  int ww2 = -1; // Where second sign change occurs
  for (i=1; i<npts; i++) {
    ct += (s[i] != s[i-1]) ? 1 : 0;
    ww = (ct > 0 && ww < 0) ? i : ww;
    ww2 = (ct > 0 && ww > 0 && ww2 < 0) ? i : ww2;
  }

  // Calculate wind for the 3 cases (no sign changes, one sign change, two sign changes, and other)
  switch(ct) {
    case 0:
      {
        // No sign changes means sigma0 > max(sg0[])
        int nn = npts;
        double max_sg0 = arr_max(sg0, npts);
        int idx_first_max = -1;
        int *w = (int *)CALLOC(nn, sizeof(int));
        int wx = 0;
        for (i=0; i<npts; i++) {
          if (sg0[i] >= max_sg0) {
            idx_first_max = (idx_first_max < 0) ? i : idx_first_max;
            w[wx++] = i; // Collect indices of max's
          }
        }
        if (idx_first_max == nn - 1) {
          // Last element was the largest element, so send back max wind
          *wnd1 = wd[nn-1];
          *wnd2 = WND_FROM_MAX_SIGMA0;
        }
        else {
          int ix1 = (w[0] - 1 >= 0) ? w[0] - 1 : 0;
          int ix2 = w[0];
          int ix3 = w[0] + 1;
          double fit1;
          double *f1 = poly_fit(wd, sg0, ix1, ix2, ix3, 2, &fit1);
          *wnd1 = (f1[1]+sqrt(f1[1]*f1[1]-4.0*f1[2]*(f1[0]-sg0[ix2])))/2/f1[2];
          *wnd2 = WND_FROM_MAX_SIGMA0;
        }
        FREE(w);
      }
      break;
    case 1:
      {
        // Single solution
        int ix1 = ww;
        int ix2 = (ww+1) > npts - 1 ? npts - 1 : ww+1;
        int ix3 = (ww+2) > npts - 1 ? npts - 1 : ww+2;
        double fit1;
        double *f1 = poly_fit(wd, sg0, ix1, ix2, ix3, 2, &fit1);
        *wnd1 = (f1[1]+sqrt(f1[1]*f1[1]-4.0*f1[2]*(f1[0]-sigma0)))/2/f1[2];
        *wnd2 = WND1_IS_ONLY_SOLUTION;
      }
      break;
    case 2:
      {
        // Two solutions (usually lowest answer of the two is best answer)
        int ix1 = ww;
        int ix2 = (ww+1) > npts - 1 ? npts - 1 : ww+1;
        int ix3 = (ww+2) > npts - 1 ? npts - 1 : ww+2;
        int ix4 = ww2;
        int ix5 = (ww2+1) > npts - 1 ? npts - 1 : ww2+1;
        int ix6 = (ww2+2) > npts - 1 ? npts - 1 : ww2+2;
        double fit1, fit2;
        double *f1 = poly_fit(wd, sg0, ix1, ix2, ix3, 2, &fit1);
        double *f2 = poly_fit(wd, sg0, ix4, ix5, ix6, 2, &fit2);
        *wnd1 = (f1[1]+sqrt(f1[1]*f1[1]-4.0*f1[2]*(f1[0]-sigma0)))/2/f1[2];
        *wnd2 = (f2[1]+sqrt(f2[1]*f2[1]-4.0*f2[2]*(f2[0]-sigma0)))/2/f2[2];
      }
      break;
    default:
      *wnd1 = WND_FROM_MAX_SIGMA0;
      *wnd2 = WND_FROM_MAX_SIGMA0;
      break;
  }

  FREE(s);
  FREE(wd);
  FREE(sg0);

  return 0;
}

double *ws_cmod5(double u10[], int npts, double wdir, double incid, double r_look)
{
  int i;

  // Necessary constants
  double thetm  = 40.0; // Degrees
  double thethr = 25.0; // Degrees
  double zpow   = 1.6;
  double c[28]  =
    {-0.6880, -0.7930,  0.3380, -0.1730,  0.0000,  0.0040, 0.1110,
      0.0162,  6.3400,  2.5700, -2.1800,  0.4000, -0.6000, 0.0450,
      0.0070,  0.3300,  0.0120, 22.0000,  1.9500,  3.0000, 8.3900,
     -3.4400,  1.3600,  5.3500,  1.9900,  0.2900,  3.8000, 1.5300};
  double y0, pn, a, b, csfi, x, a0, a1, a2, gam, s1, v0, d1, d2;
  double *s2    = (double *)MALLOC(sizeof(double) * npts);
  double *a3    = (double *)MALLOC(sizeof(double) * npts);
  double *b0    = (double *)MALLOC(sizeof(double) * npts);
  double *b1    = (double *)MALLOC(sizeof(double) * npts);
  double *b2    = (double *)MALLOC(sizeof(double) * npts);
  double *v2    = (double *)MALLOC(sizeof(double) * npts);
  double *sigma = (double *)MALLOC(sizeof(double) * npts);
  double t;

  y0 = c[18];
  pn = c[19]; // c[19] is 3.000
  t = y0 - 1.0;
  // a = y0 - (y0 - 1.0) / pn;
  a = y0 - t / pn;
  //b = 1.0 / (pn * powf((y0 - 1.0), (pn - 1)));
  b = 1.0 / (pn * t*t);

  // Compute difference between radar look and wind direction,
  // convert to radians, and calculate the cosine
  csfi = cos((wdir - r_look)*D2R);

  // Note that since we are taking ratios, there is no need to
  // convert the incidence angle parameters to radians:
  x = (incid - thetm) / thethr;
  a0 = ((c[3]*x + c[2])*x + c[1])*x + c[0];
  a1 = c[4] + c[5]*x;
  a2 = c[6] + c[7]*x;
  gam = ( c[10]*x + c[9] ) * x + c[8];
  s1 = c[11] + c[12]*x;
  for (i=0; i<npts; i++) {
    s2[i] = a2 * u10[i];
    // a3 = s2;
    // w = where(s2 lt s1, anz)
    // if anz gt 0 then a3(w)=s1(w)
    a3[i] = (s2[i] < s1) ? s1 : s2[i];
    a3[i] = (s2[i] < s1) ? powf((s2[i] / s1),(s1 * (1.0 - a3[i]))) * a3[i] :
                    1.0 / (1.0 + exp(-a3[i]));
    b0[i] = powf(a3[i],gam) * powf(10.0, (a0 + a1 + u10[i]));
    // Note the lower case 'x' below was a capital 'X' in Frank's code:
    //     b1 = c[14] * u10 * (0.5 + X - tanh(4.0 * (x + c[15] + c[16] * u10)));
    b1[i] = c[14] * u10[i] * (0.5 + x - tanh(4.0 * (x + c[15] + c[16] * u10[i])));
    b1[i] = (c[13] * (1.0 + x) - b1[i]) / (exp(0.34 * (u10[i] - c[17])) + 1.0);
  }
  v0 = (c[22]*x + c[21])*x + c[20];
  d1 = (c[25]*x + c[24])*x + c[23];
  d2 = c[26] + c[27]*x;
  for (i=0; i<npts; i++) {
    // v2[i] = (v2[i] < y0) ? (a+b*powf((v2[i]-1.0),pn)) : (u10[i] / v0 + 1.0);
    t = v2[i] - 1.0; // Note: pn == 3
    v2[i] = (v2[i] < y0) ? (a+b*(t*t*t)) : (u10[i] / v0 + 1.0);
    b2[i] = (-d1 + d2*v2[i])*exp(-v2[i]);
    sigma[i] = b0[i] * powf((1.0 + b1[i]*csfi + b2[i]*(2.0*csfi*csfi - 1.0)),zpow);
  }

  FREE(s2);
  FREE(a3);
  FREE(b0);
  FREE(b1);
  FREE(b2);
  FREE(v2);

  return sigma;
}

double *maken(double first, double last, int num)
{
  int i;

  if (num < 1) return NULL;
  double *arr = (double *)MALLOC(sizeof(double) * num);
  double incr = (last - first)/(num - 1);

  for (i=0; i<num; i++) {
    arr[i] = first + (double)i*incr;
  }

  return arr;
}

double arr_min(double *arr, int n)
{
  double min;
  int i;
  g_assert(n > 0 && arr != NULL);

  min = arr[0];
  for (i=1; i<n; i++) {
    min = MIN(min, arr[i]);
  }

  return min;
}

double arr_max(double *arr, int n)
{
  double max;
  int i;
  g_assert(n > 0 && arr != NULL);

  max = arr[0];
  for (i=1; i<n; i++) {
    max = MAX(max, arr[i]);
  }

  return max;
}

// Fit a polynomial to a function using linear least-squares
// Unlike the IDL function poly_fit, this routine is limited to degree==2
double *poly_fit(double *y, double *x,
                 int ix1, int ix2, int ix3, int degree,
                 double *fit)
{
  g_assert(y != NULL && x != NULL);
  if (degree != 2) asfPrintError("poly_fit() only supports 2nd degree polynomial fits...\n");
  double *coeffs = (double *)MALLOC(3 * sizeof(double));
  double x1 = x[ix1];
  double x2 = x[ix2];
  double x3 = x[ix3];
  double y1 = y[ix1];
  double y2 = y[ix2];
  double y3 = y[ix3];

  double M = y1 - y2;
  double N = y3 - y2;
  double P = x1 - x2;
  double Q = x3 - x1;
  double R = x3 - x2;
  double S = x2*x2 - x1*x1;

  coeffs[0] = (N/(R*Q)) - (M/(P*Q));                // a
  coeffs[1] = (M + coeffs[0]*S)/P;                  // b
  coeffs[2] = y1 - coeffs[0]*x1*x1 - coeffs[1]*x1;  // c

  return coeffs;
}

// FIXME: Implement Hauser ratio with and without phi dependence
double ws_pol_ratio(double theta, double alpha)
{
  g_assert(alpha > 0.0);
  double tantheta = tan(theta * D2R);
  double tantheta2 = tantheta*tantheta;
  double rp = (1.0 + alpha*tantheta2) / (1.0 + 2.0*tantheta2);
  rp *= rp;

  return rp;
}
