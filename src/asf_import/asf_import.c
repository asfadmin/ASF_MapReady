#include <asf_contact.h>
#include <asf_copyright.h>
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
"asf_import"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-amplitude | -sigma | -gamma | -beta | -power] [-db]\n"\
"              [-format <inputFormat>] [-lut <file>] [-lat <lower> <upper>]\n"\
"              [-prc] [-old] [-log <logFile>] [-quiet] [-license] [-version]\n"\
"              [-azimuth-scale[=<scale>] | -fix-meta-ypix[=<pixsiz>]]\n"\
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
"        %f will be used.\n"\
"   -azimuth-scale[=<scale-factor>]\n"\
"        Apply the provided azimuth scale factor to the imported data.  If\n"\
"        the option is specified without an argument, a default value of\n"\
"        %f will be used.  This option cannot be used with\n"\
"        -fix-meta-ypix\n"\
"   --fix-meta-ypix[=<pixel-size>]\n"\
"        This option is similar to -azimuth-scale, but does not resample the\n"\
"        input data, it just changes the y pixel size in the metadata.\n"\
"        This option cannot be used with -azimuth-scale.\n"\

#define ASF_EXAMPLES_STRING \
"   To import CEOS format to the ASF tools internal format run:\n"\
"        example> asf_import fileCEOS fileASF\n"\
"\n"\
"   To import a STF fileset (fileSTF.000 & file.000.par) you will need to\n"\
"   specify the -format option since STF is not the default.\n"\
"        example> asf_import -format stf fileSTF.000 fileASF\n"

#define ASF_LIMITATIONS_STRING \
"   CEOS base name issue:\n"\
"        If you have two or more CEOS filesets ([*.D & *.L], [*.RAW & *.LDR],\n"\
"        or [dat.* & lea.*]) with the same base name, then this program will\n"\
"        automatically fetch the first set in the aforementioned list.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_convert, asf_export\n"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) "ASF_COPYRIGHT_YEAR_STRING", University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#include "asf_import.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "ceos.h"
#include "decoder.h"
#include "find_geotiff_name.h"
#include "get_ceos_names.h"
#include "get_stf_names.h"
#include "asf_reporting.h"
#include "asf_raster.h"
#include <ctype.h>

#define REQUIRED_ARGS 2

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

/*
These next few functions are used to fix scaling errors in the data that
result from the PP using an incorrect swath velocity during some of
the calculations.

Here is a summary of the fixes as described by Orion Lawlor in an e-mail
on 3/14/06:

xpix_ypix prints out these values for an ARDoP image.
> > azimuth pixel size at scene center: 3.9648920 meters   (xpix_ypix)
> > ASF geolocate azimuth velocity: 6660.144 m/s   (xpix_ypix)
> > PP swath velocity: 6626.552 m/s = ...
The velocities should be about the same for the corresponding
L1 image.

The PP L1 pixel spacing is supposed to always be 12.5m.
But because the PP miscalulates the swath velocity, L1 images actually
have a pixel spacing of:
PP spacing * real velocity / PP velocity = real spacing
12.5 * 6660.144/6626.552 = 12.5633662876 m

An ARDOP image's pixel spacing is properly computed by xpix_ypix (and
not in the "yPix" field of the .meta file!) and multilooked, so the
ARDOP image pixel spacing is really:
3.9648920 m/pix * 5-pixel ARDOP multilook = 19.8244600 m/pix

The expected L1-to-multilooked-ARDOP image scale factor is just the
ratio of the two image's pixel spacings:
19.8244600 m/pix / 12.5633662876 m/pix = 1.5779576545
*/
float get_default_azimuth_scale(const char *outBaseName)
{
    char outMetaName[256];
    sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

    meta_parameters *omd = meta_read(outMetaName);

    float pp_velocity, corrected_velocity;
    xpyp_getVelocities(omd, &pp_velocity, &corrected_velocity);

    asfPrintStatus("       PP Velocity: %g\n", pp_velocity);
    asfPrintStatus("Corrected Velocity: %g\n", corrected_velocity);

    float qua, az_pixsiz;
    xpyp_getPixSizes(omd, &qua, &az_pixsiz);

    asfPrintStatus("      y pixel size: %g (xpix_ypix)\n", az_pixsiz);
    asfPrintStatus("      y pixel size: %g (metadata)\n",
        omd->general->y_pixel_size);

    float real_spacing =
        omd->general->y_pixel_size * corrected_velocity / pp_velocity;

    float scale = az_pixsiz / real_spacing;

    asfPrintStatus("             Scale: %g\n\n", scale);

    return scale;
}

float get_default_ypix(const char *outBaseName)
{
    char outMetaName[256];
    sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

    meta_parameters *omd = meta_read(outMetaName);

    float qua, az_pixsiz;
    xpyp_getPixSizes(omd, &qua, &az_pixsiz);
    meta_free(omd);

    return az_pixsiz;
}

void fix_ypix(const char *outBaseName, double correct_y_pixel_size)
{
    asfPrintStatus("Applying y pixel size correction to the metadata...\n");

    char outMetaName[256];
    sprintf(outMetaName, "%s%s", outBaseName, TOOLS_META_EXT);

    meta_parameters *omd = meta_read(outMetaName);

    asfPrintStatus("Original y pixel size: %g\n",
        omd->general->y_pixel_size);
    asfPrintStatus("            corrected: %g\n",
        correct_y_pixel_size);

    omd->general->y_pixel_size = correct_y_pixel_size;
    meta_write(omd, outMetaName);
    meta_free(omd);
}

/******************************************************************************
* Lets rock 'n roll!
*****************************************************************************/
int main(int argc, char *argv[])
{
    char inBaseName[256]="";
    char inDataName[256]="", inMetaName[256]="";
    char outBaseName[256]="";
    char unscaledBaseName[256]="";
    char inMetaNameOption[256], prcPath[256]="";
    char *lutName=NULL;
    char format_type[256]="";
    int ii;
    int flags[NUM_FLAGS];
    double lowerLat=NAN, upperLat=NAN;
    int do_resample;
    int do_metadata_fix;

    /* Set all flags to 'not set' */
    for (ii=0; ii<NUM_FLAGS; ii++) {
        flags[ii] = FLAG_NOT_SET;
    }

    /**********************BEGIN COMMAND LINE PARSING STUFF**********************/
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

    do_metadata_fix = flags[f_FIX_META_YPIX] != FLAG_NOT_SET;

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

    if(flags[f_PRC] != FLAG_NOT_SET)
        strcpy(prcPath, argv[flags[f_PRC] + 1]);

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

    strcpy(unscaledBaseName, outBaseName);
    if (do_resample) {
        strcat(unscaledBaseName, "_unscaled");
    }

    /* Ingest all sorts of flavors of CEOS data */
    if (strncmp(format_type, "CEOS", 4) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        if (flags[f_METADATA_FILE] == FLAG_NOT_SET)
            require_ceos_pair(inBaseName, inDataName, inMetaName);
        else {
            /* Allow the base name to be different for data & meta */
            require_ceos_data(inBaseName, inDataName);
            require_ceos_metadata(inMetaNameOption, inMetaName);
        }
        import_ceos(inDataName, inMetaName, lutName, unscaledBaseName, flags);
    }
    /* Ingest ENVI format data */
    else if (strncmp(format_type, "ENVI", 4) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        import_envi(inDataName, inMetaName, unscaledBaseName, flags);
    }
    /* Ingest ESRI format data */
    else if (strncmp(format_type, "ESRI", 4) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        import_esri(inDataName, inMetaName, unscaledBaseName, flags);
    }
    /* Ingest Vexcel Sky Telemetry Format (STF) data */
    else if (strncmp(format_type, "STF", 3) == 0) {
        asfPrintStatus("   Data format: %s\n", format_type);
        if (flags[f_METADATA_FILE] == FLAG_NOT_SET)
            require_stf_pair(inBaseName, inDataName, inMetaName);
        else {
            /* Allow the base name to be different for data & meta */
            require_stf_data(inBaseName, inDataName);
            require_stf_metadata(inMetaNameOption, inMetaName);
        }
        import_stf(inDataName, inMetaName, unscaledBaseName, flags,
            lowerLat, upperLat, prcPath);
    }
    else if ( strncmp (format_type, "GEOTIFF", 7) == 0 ) {
      asfPrintStatus("   Data format: %s\n", format_type);
      GString *inGeotiffName = find_geotiff_name (inBaseName);
      if ( inGeotiffName == NULL ) {
	asfPrintError ("Couldn't find a GeoTIFF file (i.e. a file with "
		       "extension '.tif', '.tiff', '.TIF', or '.TIFF') "
		       "corresponding to specified inBaseName");
      }
      // At the moment, we are set up to ingest only a specific
      // GeoTIFF variant, the Shuttle Radar Topography Mission (SRTM)
      // data from the USGS seamless system.  Later on we should
      // probably do intelligent detection of GeoTIFF flavor, perhaps
      // falling back to a catch-all that tried to ingest arbitrary
      // GeoTIFF.
      import_srtm_seamless (inGeotiffName->str, outBaseName);
      g_string_free (inGeotiffName, TRUE);
    }
    /* Don't recognize this data format; report & quit */
    else {
        asfPrintError("Unrecognized data format: '%s'",format_type);
    }

    /* resample, if necessary */
    if (do_resample)
    {
        double range_scale = flags[f_RANGE_SCALE] == FLAG_NOT_SET ? 1.0 :
        getDoubleOptionArgWithDefault(argv[flags[f_RANGE_SCALE]],
            DEFAULT_RANGE_SCALE);

        double azimuth_scale = flags[f_AZIMUTH_SCALE] == FLAG_NOT_SET ? 1.0 :
        getDoubleOptionArgWithDefault(argv[flags[f_AZIMUTH_SCALE]],
            get_default_azimuth_scale(unscaledBaseName));

        asfPrintStatus("Resampling with scale factors: %g range, %g azimuth.\n",
            range_scale, azimuth_scale);

        resample(unscaledBaseName, outBaseName, range_scale, azimuth_scale);
    }

    /* metadata pixel size fix, if necessary */
    if (do_metadata_fix)
    {
        double correct_y_pixel_size =
            getDoubleOptionArgWithDefault(argv[flags[f_FIX_META_YPIX]],
            get_default_ypix(outBaseName));

        fix_ypix(outBaseName, correct_y_pixel_size);
    }

    /* If the user asked for sprocket layers, create sprocket data layers
    now that we've got asf tools format data */
    if (flags[f_SPROCKET] != FLAG_NOT_SET) {
        create_sprocket_layers(outBaseName, inMetaName);
    }

    /* If the user didn't ask for a log file then we can nuke the one that
    we've been keeping since we've finished everything */
    if (flags[f_LOG] == FLAG_NOT_SET) {
        fclose (fLog);
        remove(logFile);
    }

    exit(EXIT_SUCCESS);
}
