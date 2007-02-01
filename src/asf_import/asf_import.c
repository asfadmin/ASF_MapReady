#include <asf_contact.h>
#include <asf_license.h>
/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header. :)
*/

#define ASF_NAME_STRING \
"alos_import"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-amplitude | -sigma | -gamma | -beta | -power] [-db]\n"\
"              [-format <inputFormat>] [-band <band_id | all>] [-image-data-type <type>]\n"\
"              [-lut <file>] [-lat <lower> <upper>] [-prc] [-old] [-log <logFile>] [-quiet]\n"\
"              [-license] [-version] [-azimuth-scale[=<scale>] | -fix-meta-ypix[=<pixsiz>]]\n"\
"              [-range-scale[=<scale>]\n"\
"              [-help]\n"\
"              <inBaseName> <outBaseName>\n"

#define ASF_DESCRIPTION_STRING \
"   Ingests all varieties of CEOS and STF data formats and outputs ASF\n"\
"   internal format metadata and data files. When the calibration\n"\
"   parameters are applied using the -sigma, -gamma, or the -beta\n"\
"   option the resulting image will have power scale values.\n"

#define ASF_INPUT_STRING \
"   The format of the input file must be specified as CEOS or STF.\n"

#define ASF_OUTPUT_STRING \
"   Outputs data and metadata files with the user-provided base name and\n"\
"   appropriate extensions.\n"

#define ASF_OPTIONS_STRING \
"   -amplitude\n"\
"        Create an amplitude image. This is the default behavior.\n"\
"   -sigma\n"\
"        Create a calibrated image (sigma power scale values).\n"\
"   -gamma\n"\
"        Create a calibrated image (gamma power scale values).\n"\
"   -beta\n"\
"        Create a calibrated image (beta power scale values).\n"\
"   -power\n"\
"        Create a power image.\n"\
"   -db  Output calibrated image in decibles. This can only be used with\n"\
"        -sigma, -beta, or -gamma. When performing statistics on the imagery\n"\
"        it is highly recommended that the image is left in power scale\n"\
"        (ie do not use the -db flag if you plan on statistical analysis)\n"\
"   -format <inputFormat>\n"\
"        Force input data to be read as the given format type. Valid formats\n"\
"        are 'ceos', 'stf', and 'geotiff'. 'CEOS' is the default behavior.\n"\
"   -band <band_id | all>\n"\
"        If the data contains multiple data files, one for each band (channel)\n"\
"        then import the band identified by 'band_id' (only).  If 'all' is\n"\
"        specified rather than a band_id, then import all available bands into\n"\
"        a single ASF-format file.  Default is '-band all'.\n"\
"   -image-data-type <type>\n"\
"        Force input data to be interpreted as the given image data type. Valid\n"\
"        formats are 'geocoded_image', 'dem', and 'mask'.  This parameter is \n"\
"        ignored unless the -format parameter is \"geotiff\".\n"\
"   -lut <file>\n"\
"        Applies a user defined look up table to the data. Look up contains\n"\
"        incidence angle dependent scaling factor.\n"\
"   -lat <lower> <upper>\n"\
"        Specify lower and upper latitude contraints (only available\n"\
"        for STF). Note that the program is not able to verify whether\n"\
"        the chosen latitude constraint is within the image.\n"\
"   -prc Replace the restituted state vectors from the original raw data\n"\
"        acquired by the ERS satellites with preceision\n"\
"   -old Output in old style ASF internal format.\n"\
"   -log <logFile>\n"\
"        Output will be written to a specified log file.\n"\
"   -quiet\n"\
"        Supresses all non-essential output.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print a help page and exit.\n\n"\
"The following options allow correction of scaling errors in the original\n"\
"data.\n\n"\
"   -range-scale[=<scale-factor>]\n"\
"        Apply the provided range scale factor to the imported data.  If\n"\
"        the option is specified without an argument, a default value of\n"\
"        %f will be used.\n\n"\
"        The metadata will not be updated after scaling - this option is\n"\
"        intended to be used to correct errors in the data.\n\n"\
"   -azimuth-scale[=<scale-factor>]\n"\
"        Apply the provided azimuth scale factor to the imported data.  If\n"\
"        the option is specified without an argument, a default value will\n"\
"        be calculated from the metadata.\n\n"\
"        This option cannot be used with -fix-meta-ypix\n\n"\
"        The metadata will not be updated after scaling - this option is\n"\
"        intended to be used to correct errors in the data.\n\n"\
"   -fix-meta-ypix[=<pixel-size>]\n"\
"        This option is similar to -azimuth-scale, but does not resample the\n"\
"        input data, it just changes the y pixel size in the metadata.\n"\
"        This option cannot be used with -azimuth-scale.\n"\

#define ASF_EXAMPLES_STRING \
"   To import CEOS format to the ASF tools internal format run:\n"\
"        example> asf_import fileCEOS fileASF\n"\
"\n"\
"   To import a STF fileset (fileSTF.000 & file.000.par) you will need to\n"\
"   specify the -format option since STF is not the default.\n"\
"        example> asf_import -format stf fileSTF.000 fileASF\n" \
"\n"\
"   To import an ALOS Palsar fileset (IMG-HH-file, IMG-HV-file, IMG-VH-file,\n"\
"   IMG-VV-file, and LED-file) you will need to specify the input basename 'file'.\n"\
"   When importing optical or other multi-band CEOS-formatted data such as ALOS\n"\
"   Avnir optical images, you only need to specify the file names in the same way\n"\
"   as well.\n"\
"        example> asf_import file outfile\n"\
"\n"\
"   To import a single band of an ALOS fileset (IMG-HH-file, IMG-HV-file,\n"\
"   IMG-VH-file,IMG-VV-file, and LED-file; or IMG-01-file, IMG-02-file, etc) you\n"\
"   will need to specify the band_id and input basename 'file'.\n"\
"        example1> asf_import -band VH file outfile\n"\
"        example2> asf_import -band 04 file outfile\n"

#define ASF_LIMITATIONS_STRING \
"   CEOS base name issue:\n"\
"        If you have two or more CEOS filesets ([*.D & *.L], [*.RAW & *.LDR],\n"\
"        or [dat.* & lea.*]) with the same base name, then this program will\n"\
"        automatically fetch the first set in the aforementioned list.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_convert, asf_export\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#include "asf_import.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "ceos.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "asf_raster.h"
#include <ctype.h>

#ifdef linux
char *strdup(char *);
#endif

#define REQUIRED_ARGS 2

#define FLAG_SET 1
#define FLAG_NOT_SET -1

/* Index keys for all flags used in this program via a 'flags' array */
typedef enum {
    f_AMP=1,
    f_SIGMA,
    f_BETA,
    f_GAMMA,
    f_POWER,
    f_DB,
    f_SPROCKET,
    f_LUT,
    f_LAT_CONSTRAINT,
    f_PRC,
    f_FORMAT,
    f_OLD_META,
    f_METADATA_FILE,
    f_LOG,
    f_QUIET,
    f_RANGE_SCALE,
    f_AZIMUTH_SCALE,
    f_FIX_META_YPIX,
    f_IMAGE_DATA_TYPE,
    f_BAND,
    NUM_IMPORT_FLAGS
} import_flag_indices_t;

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

/* Check to see if an option with (or without) an argument
   was supplied.  If it was found, return its argument
   number.  otherwise return FLAG_NOT_SET.

   The argument is assumed to be of the form "<key>[=value]"
   The [=value] part is optional. */
static int checkForOptionWithArg(char *key, int argc, char *argv[])
{
  int ii = 0;
  while (ii < argc)
  {
    /* make a copy of the arg, and remove anything past the "=" */
    char *arg = STRDUP(argv[ii]);
    char *eq = strchr(arg, '=');
    if (eq) *eq = '\0';

    /* now check for an exact match */
    int match = !strcmp(key, arg);
    FREE(arg);
    if (match) {
      return ii;
    }
    ++ii;
  }
  return FLAG_NOT_SET;
}

static double getDoubleOptionArgWithDefault(char *arg, double def)
{
  double val = def;
  char *arg_cpy = strdup(arg);
  char *eq = strchr(arg_cpy, '=');
  if (eq) {
    ++eq;
    char *endptr;
    double d = strtod(eq, &endptr);
    if (endptr != eq) val = d;
  }
  free(arg_cpy);

  return val;
}

static void
pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName)
{
  if (*flag_count==0)
    strcat(flags_used, flagName);
  else if (*flag_count==1)
    strcat(strcat(flags_used, " and "), flagName);
  else if (*flag_count>1)
    strcat(strcat(flags_used, ", and "), flagName);
  else
    asfPrintError("Programmer error dealing with the %s flag.\n", flagName);
  (*flag_count)++;
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
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n",
      DEFAULT_RANGE_SCALE);
  exit(EXIT_SUCCESS);
}

/******************************************************************************
* Lets rock 'n roll!
*****************************************************************************/
int main(int argc, char *argv[])
{
    char inBaseName[256]="";
    char inMetaName[256]="";
    char outBaseName[256]="";
    char *inMetaNameOption=NULL;
    char *lutName=NULL;
    char *prcPath=NULL;
    char format_type[256]="";
    char band_id[256]="";
    char image_data_type[256]="";
    int ii;
    int flags[NUM_IMPORT_FLAGS];
    double lowerLat=NAN, upperLat=NAN;
    double range_scale=NAN, azimuth_scale=NAN, correct_y_pixel_size=NAN;
    int do_resample;
    int do_metadata_fix;

    /* Set all flags to 'not set' */
    for (ii=0; ii<NUM_IMPORT_FLAGS; ii++) {
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
    flags[f_AMP] = checkForOption("-amplitude", argc, argv);
    flags[f_SIGMA] = checkForOption("-sigma", argc, argv);
    flags[f_BETA] = checkForOption("-beta", argc, argv);
    flags[f_GAMMA] = checkForOption("-gamma", argc, argv);
    flags[f_POWER] = checkForOption("-power", argc, argv);
    flags[f_DB] = checkForOption("-db", argc, argv);
    flags[f_SPROCKET] = checkForOption("-sprocket", argc, argv);
    flags[f_LUT] = checkForOption("-lut", argc, argv);
    flags[f_LAT_CONSTRAINT] = checkForOption("-lat", argc, argv);
    flags[f_PRC] = checkForOption("-prc", argc, argv);
    flags[f_OLD_META] = checkForOption("-old", argc, argv);
    flags[f_METADATA_FILE] = FLAG_NOT_SET;
    /* checkForOption("-metadata", argc, argv); */
    flags[f_LOG] = checkForOption("-log", argc, argv);
    flags[f_QUIET] = checkForOption("-quiet", argc, argv);
    flags[f_FORMAT] = checkForOption("-format", argc, argv);
    flags[f_IMAGE_DATA_TYPE] = checkForOption("-image-data-type", argc, argv);
    flags[f_BAND] = checkForOption("-band", argc, argv);

    flags[f_RANGE_SCALE] = checkForOptionWithArg("-range-scale", argc, argv);
    if (flags[f_RANGE_SCALE] == FLAG_NOT_SET)
        flags[f_RANGE_SCALE] = checkForOptionWithArg("-range_scale", argc, argv);

    flags[f_AZIMUTH_SCALE] = checkForOptionWithArg("-azimuth-scale", argc, argv);
    if (flags[f_AZIMUTH_SCALE] == FLAG_NOT_SET)
        flags[f_AZIMUTH_SCALE] = checkForOptionWithArg("-azimuth_scale", argc, argv);

    flags[f_FIX_META_YPIX] = checkForOptionWithArg("-fix-meta-ypix", argc, argv);
    if (flags[f_FIX_META_YPIX] == FLAG_NOT_SET)
        flags[f_FIX_META_YPIX] = checkForOptionWithArg("-fix_meta_ypix", argc, argv);

    do_resample = flags[f_RANGE_SCALE] != FLAG_NOT_SET ||
        flags[f_AZIMUTH_SCALE] != FLAG_NOT_SET;

    if (flags[f_SPROCKET] != FLAG_NOT_SET)
        asfPrintError("At this point, sprocket layers are not ... working.\n");

    if (do_resample)
    {
        range_scale = flags[f_RANGE_SCALE] == FLAG_NOT_SET ? 1.0 :
	  getDoubleOptionArgWithDefault(argv[flags[f_RANGE_SCALE]], -1);

        azimuth_scale = flags[f_AZIMUTH_SCALE] == FLAG_NOT_SET ? 1.0 :
	  getDoubleOptionArgWithDefault(argv[flags[f_AZIMUTH_SCALE]], -1);
    }

    do_metadata_fix = flags[f_FIX_META_YPIX] != FLAG_NOT_SET;

    if (do_metadata_fix)
    {
        correct_y_pixel_size =
	  getDoubleOptionArgWithDefault(argv[flags[f_FIX_META_YPIX]], -1);
    }

    { /*Check for mutually exclusive options: we can only have one of these*/
        int temp = 0;
        if(flags[f_AMP] != FLAG_NOT_SET)      temp++;
        if(flags[f_SIGMA] != FLAG_NOT_SET)    temp++;
        if(flags[f_BETA] != FLAG_NOT_SET)     temp++;
        if(flags[f_GAMMA] != FLAG_NOT_SET)    temp++;
        if(flags[f_POWER] != FLAG_NOT_SET)    temp++;
        if(flags[f_SPROCKET] != FLAG_NOT_SET) temp++;
        if(flags[f_LUT] != FLAG_NOT_SET)      temp++;
        if(temp > 1)/*If more than one option was selected*/

            print_usage();
    }

    /* Cannot specify the fix-meta-ypix & the resampling of azimuth
    options at the same time */
    if (flags[f_FIX_META_YPIX] != FLAG_NOT_SET &&
        flags[f_AZIMUTH_SCALE] != FLAG_NOT_SET)
    {
        asfPrintStatus("You cannot specify both -azimuth-scale "
            "and -fix-meta-ypix.\n");
        print_usage();
    }

    { /*We need to make sure the user specified the proper number of arguments*/
        int needed_args = 1 + REQUIRED_ARGS;    /*command + REQUIRED_ARGS*/
        if(flags[f_AMP] != FLAG_NOT_SET)      needed_args += 1;/*option*/
        if(flags[f_SIGMA] != FLAG_NOT_SET)    needed_args += 1;/*option*/
        if(flags[f_BETA] != FLAG_NOT_SET)     needed_args += 1;/*option*/
        if(flags[f_GAMMA] != FLAG_NOT_SET)    needed_args += 1;/*option*/
        if(flags[f_POWER] != FLAG_NOT_SET)    needed_args += 1;/*option*/
        if(flags[f_DB] != FLAG_NOT_SET)       needed_args += 1;/*option*/
        if(flags[f_SPROCKET] != FLAG_NOT_SET) needed_args += 1;/*option*/
        if(flags[f_LUT] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
        if(flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET)
            needed_args += 3;/*option & parameter & parameter*/
        if(flags[f_PRC] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
        if(flags[f_OLD_META] != FLAG_NOT_SET) needed_args += 1;/*option*/
        if(flags[f_METADATA_FILE] != FLAG_NOT_SET)  needed_args += 2;/*option & parameter*/
        if(flags[f_LOG] != FLAG_NOT_SET)      needed_args += 2;/*option & parameter*/
        if(flags[f_QUIET] != FLAG_NOT_SET)    needed_args += 1;/*option*/
        if(flags[f_FORMAT] != FLAG_NOT_SET)   needed_args += 2;/*option & parameter*/
        if(flags[f_BAND] != FLAG_NOT_SET)     needed_args += 2;/*option & parameter*/
        if(flags[f_IMAGE_DATA_TYPE] != FLAG_NOT_SET)  needed_args += 2; /*option & parameter*/
        if(flags[f_RANGE_SCALE] != FLAG_NOT_SET)   needed_args += 1;/*option*/
        if(flags[f_AZIMUTH_SCALE] != FLAG_NOT_SET)   needed_args += 1;/*option*/
        if(flags[f_FIX_META_YPIX] != FLAG_NOT_SET)
            needed_args += 1;/*option*/

        /*Make sure we have enough arguments*/
        if(argc != needed_args)
            print_usage();/*This exits with a failure*/
    }

    /*We also need to make sure any options that have parameters are specified
    correctly.  This includes: -lat, -prc, -log, -lut*/
    if(flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET)
        /*Make sure there's no "bleeding" into the required arguments
        No check for '-' in the two following fields because negative numbers
        are legit (eg -lat -67.5 -70.25)*/
        if(flags[f_LAT_CONSTRAINT] >= argc - (REQUIRED_ARGS+1))
            print_usage();
    if(flags[f_PRC] != FLAG_NOT_SET)
        /*Make sure the field following -prc isn't another option
        Also check for bleeding into required arguments*/
        if(   argv[flags[f_PRC]+1][0] == '-'
            || flags[f_PRC] >= argc-REQUIRED_ARGS)
            print_usage();
    if(flags[f_METADATA_FILE] != FLAG_NOT_SET)
        /*Make sure the field following -metadata isn't another option*/
        if(   argv[flags[f_METADATA_FILE] + 1][0] == '-'
            || flags[f_METADATA_FILE] >= argc - REQUIRED_ARGS)
            print_usage();
    if(flags[f_LOG] != FLAG_NOT_SET)
        /*Make sure the field following -log isn't another option*/
        if(   argv[flags[f_LOG]+1][0] == '-'
            || flags[f_LOG] >= argc-REQUIRED_ARGS)
            print_usage();
    if(flags[f_LUT] != FLAG_NOT_SET)
        /*Make sure the field following -lut isn't another option*/
        if(   argv[flags[f_LUT]+1][0] == '-'
            || flags[f_LUT] >= argc-REQUIRED_ARGS)
            print_usage();
    if(flags[f_FORMAT] != FLAG_NOT_SET)
        /*Make sure the field following -format isn't another option*/
        if(   argv[flags[f_FORMAT]+1][0] == '-'
            || flags[f_FORMAT] >= argc-REQUIRED_ARGS)
            print_usage();
    if(flags[f_BAND] != FLAG_NOT_SET)
      /*Make sure the field following -format isn't another option*/
      if(   argv[flags[f_BAND]+1][0] == '-'
            || flags[f_BAND] >= argc-REQUIRED_ARGS)
        print_usage();
    if(flags[f_IMAGE_DATA_TYPE] != FLAG_NOT_SET)
        /*Make sure the field following -format isn't another option*/
        if(   argv[flags[f_IMAGE_DATA_TYPE]+1][0] == '-'
            || flags[f_IMAGE_DATA_TYPE] >= argc-REQUIRED_ARGS)
            print_usage();

    /* Be sure to open log ASAP */
    if(flags[f_LOG] != FLAG_NOT_SET)
        strcpy(logFile, argv[flags[f_LOG] + 1]);
    else /*default behavior: log to tmp<pid>.log*/
        sprintf(logFile, "tmp%i.log", (int)getpid());
    logflag = TRUE; /* Since we always log, set the old school logflag to true */
    fLog = fopen (logFile, "a");
    if ( fLog == NULL ) {
        logflag = FALSE;
    }
    /* Set old school quiet flag (for use in our libraries) */
    quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

    /*We must be close to good enough at this point... log & quiet flags are set
    Report what was retrieved at the command line */
    asfSplashScreen(argc, argv);

    if(flags[f_PRC] != FLAG_NOT_SET) {
        prcPath = (char *)MALLOC(sizeof(char)*256);
        strcpy(prcPath, argv[flags[f_PRC] + 1]);
    }

    if(flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET) {
        lowerLat = strtod(argv[flags[f_LAT_CONSTRAINT] + 2],NULL);
        upperLat = strtod(argv[flags[f_LAT_CONSTRAINT] + 1],NULL);
        if(lowerLat > upperLat) {
            float swap = upperLat;
            upperLat = lowerLat;
            lowerLat = swap;
        }
        if(lowerLat < -90.0 || lowerLat > 90.0 || upperLat < -90.0 || upperLat > 90.0)
        {
            asfPrintError("Invalid latitude constraint (must be -90 to 90)");
        }
    }

    if(flags[f_LUT] != FLAG_NOT_SET) {
        lutName = (char *) MALLOC(sizeof(char)*256);
        strcpy(lutName, argv[flags[f_LUT] + 1]);
    }

    { /* BEGIN: Check for conflict between pixel type flags */
        char flags_used[256] = "";
        int flag_count=0;

        if (flags[f_AMP] != FLAG_NOT_SET) {
            pixel_type_flag_looker(&flag_count, flags_used, "amplitude");
        }
        if (flags[f_SIGMA] != FLAG_NOT_SET) {
            pixel_type_flag_looker(&flag_count, flags_used, "sigma");
        }
        if (flags[f_GAMMA] != FLAG_NOT_SET) {
            pixel_type_flag_looker(&flag_count, flags_used, "gamma");
        }
        if (flags[f_BETA] != FLAG_NOT_SET) {
            pixel_type_flag_looker(&flag_count, flags_used, "beta");
        }
        if (flags[f_POWER] != FLAG_NOT_SET) {
            pixel_type_flag_looker(&flag_count, flags_used, "power");
        }
        if (flags[f_LUT] != FLAG_NOT_SET) {
            pixel_type_flag_looker(&flag_count, flags_used, "lut");
        }
        if (flag_count > 1) {
            sprintf(logbuf, "Cannot mix the %s flags.", flags_used);
            asfPrintError(logbuf);
        }
    } /* END: Check for conflict between pixel type flags */

    if (   flags[f_DB] != FLAG_NOT_SET
        && !(   flags[f_SIGMA] != FLAG_NOT_SET
        || flags[f_GAMMA] != FLAG_NOT_SET
        || flags[f_BETA] != FLAG_NOT_SET ) )
    {
        asfPrintWarning("-db flag must be specified with -sigma, -gamma, or -beta. Ignoring -db.\n");
    }


    /* Get the input metadata name if the flag was specified (probably for a meta
    * name with a different base name than the data name) */
    if(flags[f_METADATA_FILE] != FLAG_NOT_SET) {
        inMetaNameOption=(char *)MALLOC(sizeof(char)*256);
        strcpy(inMetaNameOption, argv[flags[f_METADATA_FILE] + 1]);
    }

    /* Deal with input format type */
    if(flags[f_FORMAT] != FLAG_NOT_SET) {
        strcpy(format_type, argv[flags[f_FORMAT] + 1]);
        for (ii=0; ii<strlen(format_type); ii++) {
            format_type[ii] = (char)toupper(format_type[ii]);
        }
    }
    else
        strcpy(format_type, "CEOS");

    /* Deal with band_id */
    if(flags[f_BAND] != FLAG_NOT_SET) {
      strcpy(band_id, argv[flags[f_BAND] + 1]);
      if (strlen(band_id) && strcmp("ALL", uc(band_id)) == 0) {
        strcpy(band_id, "");
      }
    }

    /* Deal with input image data type */
    if(flags[f_IMAGE_DATA_TYPE] != FLAG_NOT_SET &&
       strncmp(format_type, "GEOTIFF", 7) == 0)
    {
      strcpy(image_data_type, argv[flags[f_IMAGE_DATA_TYPE] + 1]);
      for (ii=0; ii<strlen(image_data_type); ii++) {
        image_data_type[ii] = (char)toupper(image_data_type[ii]);
      }
    }
    else
    {
      if (flags[f_IMAGE_DATA_TYPE] != FLAG_NOT_SET) {
        asfPrintWarning("-image-data-type parameter ignored (only valid for GeoTIFF\n"
            "image format type)\n");
      }
      strcpy(image_data_type, "???");
    }

    /* Make sure STF specific options are not used with other data types */
    if (strcmp(format_type, "STF")!=0) {
        if (flags[f_PRC] != FLAG_NOT_SET) {
            asfPrintWarning("Precision state vectors only work with STF data\n"
                "and will not be used with this data set!\n");
            flags[f_PRC]=FLAG_NOT_SET;
        }
        if (flags[f_LAT_CONSTRAINT] != FLAG_NOT_SET) {
            asfPrintWarning("No latitude constraints only work with STF data\n"
                "and will not be used with this data set!\n");
            flags[f_LAT_CONSTRAINT]=FLAG_NOT_SET;
        }
    }

    /* Fetch required arguments */
    strcpy(inBaseName, argv[argc - 2]);
    strcpy(outBaseName,argv[argc - 1]);

    /***********************END COMMAND LINE PARSING STUFF***********************/

    /* If the user wants sprocket layers, first check to see if the asf data is
    already there and go straight to creating the layers */
    if (flags[f_SPROCKET] != FLAG_NOT_SET) {
        char asfimage[256], asfmeta[256];
        strcat(strcpy(asfimage,outBaseName),TOOLS_IMAGE_EXT);
        strcat(strcpy(asfmeta,outBaseName),TOOLS_META_EXT);
        if (fileExists(asfimage) && fileExists(asfmeta)) {
            /* Cal params are only retrieved from CEOS data, so nab the CEOS name */
            get_ceos_metadata_name(inBaseName, inMetaName);
            create_sprocket_layers(outBaseName, inMetaName);
            /* Nix the log file if the user didn't ask for it */
            if (flags[f_LOG] == FLAG_NOT_SET) {
                fclose (fLog);
                remove(logFile);
            }
            exit(EXIT_SUCCESS);
        }
    }

    printf("%s\n",date_time_stamp());
    printf("Program: asf_import\n\n");
    if (logflag) {
        printLog("Program: asf_import\n\n");
    }

    { // scoping block
        int db_flag = flags[f_DB] != FLAG_NOT_SET;

        double *p_correct_y_pixel_size = NULL;
        if (do_metadata_fix)
            p_correct_y_pixel_size = &correct_y_pixel_size;

        double *p_range_scale = NULL;
        if (flags[f_RANGE_SCALE] != FLAG_NOT_SET)
            p_range_scale = &range_scale;

        double *p_azimuth_scale = NULL;
        if (flags[f_AZIMUTH_SCALE] != FLAG_NOT_SET)
            p_azimuth_scale = &azimuth_scale;

        radiometry_t radiometry;
        if(flags[f_AMP] != FLAG_NOT_SET)      radiometry = r_AMP;
        if(flags[f_SIGMA] != FLAG_NOT_SET)    radiometry = r_SIGMA;
        if(flags[f_BETA] != FLAG_NOT_SET)     radiometry = r_BETA;
        if(flags[f_GAMMA] != FLAG_NOT_SET)    radiometry = r_GAMMA;
        if(flags[f_POWER] != FLAG_NOT_SET)    radiometry = r_POWER;

        asf_import(radiometry, db_flag, format_type, band_id, image_data_type, lutName,
                   prcPath, lowerLat, upperLat, p_range_scale, p_azimuth_scale,
                   p_correct_y_pixel_size, inMetaNameOption,
                   inBaseName, outBaseName);
    }

    if (lutName)
        free(lutName);
    if (prcPath)
        free(prcPath);

    /* If the user didn't ask for a log file then we can nuke the one that
       we've been keeping since we've finished everything  */
    if (flags[f_LOG] == FLAG_NOT_SET) {
        fclose (fLog);
        remove(logFile);
    }

    exit(EXIT_SUCCESS);
}

