/*==================BEGIN ASF DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines.
*/

#define ASF_NAME_STRING \
"asf_export"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-format <output_format>] [-byte <sample mapping option>]\n"\
"              [-rgb <red> <green> <blue>] [-band <band_id | all>]\n"\
"              [-lut <look up table file>] [-truecolor] [-falsecolor]\n"\
"              [-log <log_file>] [-quiet] [-license] [-version] [-help]\n"\
"              <in_base_name> <out_full_name>\n"

#define ASF_DESCRIPTION_STRING \
"   This program ingests ASF internal format data and exports said data to a\n"\
"   number of graphics file formats (TIFF/GEOTIFF, JPEG, PGM, PNG, POlSARPRO,\n"\
"   HDF5 and netCDF).\n"\
"   If the input data was geocoded and the ouput format supports geocoding,\n"\
"   that information will be included.  Optionally, you may apply look-up tables,\n"\
"   assign color bands (-rgb, -truecolor, -falsecolor).\n"

#define ASF_INPUT_STRING \
"   A file set in the ASF internal data format.\n"

#define ASF_OUTPUT_STRING \
"   The converted data in the output file, with the requested format.\n"

#define ASF_OPTIONS_STRING \
"   -format <format>\n"\
"        Format to export to. Must be one of the following:\n"\
"            tiff      - Tagged Image File Format, with byte valued pixels\n"\
"            geotiff   - GeoTIFF file, with floating point or byte valued pixels\n"\
"            jpeg      - Lossy compressed image, with byte valued pixels\n"\
"            pgm       - Portable graymap image, with byte valued pixels\n"\
"            png       - Portable network graphic, with byte valued pixels\n"\
"            polsarpro - Flat binary floating point files in PolSARPro format\n"\
"            hdf5      - HDF5 format, with floating point or byte valued pixels\n"\
"            netcdf    - netCDF format compliant to the CF conventions\n"\
"   NOTE: When exporting to a GeoTIFF format file, all map-projection\n"\
"         information is included in GeoKeys as specified in the GeoTIFF\n"\
"         standard.  The other graphics file formats do not support the\n"\
"         storing of map-projection parameters in the output file.  If you\n"\
"         wish to maintain the map-projection and/or georeference (corner\n"\
"         point) information in the output, you should choose the GeoTIFF\n"\
"         output format.\n\n"\
"   NOTE: When exporting to a GeoTIFF format file, the data format (floating\n"\
"         point, byte, 16-bit integer, etc) will be maintained.  Many viewers\n"\
"         cannot view non-integer data.  Leaving the data format the same as\n"\
"         the original produces the most accurate export, but remapping it to\n"\
"         byte range (0-255) with the -byte option will result in the greatest\n"\
"         compatibility with viewers and GIS software packages.\n\n"\
"   -byte <sample mapping option>\n"\
"        Converts output image to byte using the following options:\n"\
"            truncate\n"\
"                values less than 0 are mapped to 0, values greater than 255\n"\
"                are mapped to 255, and values in between are converted to\n"\
"                whole numbers (the fractional part of the values are truncated,\n"\
"                not rounded.)\n"\
"            minmax\n"\
"                determines the minimum and maximum values of the input image\n"\
"                and linearly maps those values to the byte range of 0 to 255.\n"\
"                The remapping is accomplished using real (floating point)\n"\
"                numbers then the result is converted to a whole number by\n"\
"                truncating the fractional part.\n"\
"            sigma\n"\
"                determines the mean and standard deviation of an image and\n"\
"                defines a range of +/- two standard deviations (2-sigma)\n"\
"                around the mean value, and maps this buffer to the byte range\n"\
"                0 to 255 as described for minmax above.  The range limits are\n"\
"                adjusted if the either of the 2-sigma range limits lie outside\n"\
"                the range of the original values.  As with the other remapping\n"\
"                methods, the calculation of values are made with real numbers\n"\
"                and the result is converted to a whole number by truncating\n"\
"                any fractional part.\n"\
"            histogram_equalize\n"\
"                develops a look-up table by integrating the (no-adaptation)\n"\
"                whole-image histogram and then normalizing the result to the\n"\
"                0-255 range.  The result is that areas of low contrast, i.e.\n"\
"                flat topography, have a stronger contrast expansion applied\n"\
"                while areas of high contrast, i.e. non-flat topography, will\n"\
"                receive less contrast expansion.  Histogram equalization\n"\
"                is a useful transform for making hard-to-see detail more\n"\
"                visible for the the viewer but is likewise a nonlinear\n"\
"                transform that results in minor (apparent) topography shifts\n"\
"                within the image.  Since shifts occur the most in areas where\n"\
"                the contrast is expanded the most, i.e. flat topography, and\n"\
"                less in areas of more interesting topography, the pragmatic\n"\
"                conclusion is that the nonlinear shifts are quite\n"\
"                insignificant for the majority of users ...only important\n"\
"                if performing precision geography measurements or overlays.\n"\
"   -rgb <red> <green> <blue>\n"\
"        Converts output image into a color RGB image.\n"\
"        <red>, <green>, and <blue> specify which band (channel)\n"\
"        is assigned to color planes red, green, or blue,\n"\
"        ex) '-rgb HH VH VV', or '-rgb 3 2 1'.  If the word 'ignore' is\n"\
"        provided as one or more bands, then the associated color plane\n"\
"        will not be exported, e.g. '-rgb ignore 2 1' will result in an\n"\
"        RGB file that has a zero in each RGB pixel's red component, band 2\n"\
"        assigned to the green channel, and band 1 assigned to the blue.\n"\
"        The result will be an image with only greens and blues in it.\n"\
"        Currently implemented for GeoTIFF, TIFF, JPEG and PNG.\n"\
"        Cannot be used together with the -band option.\n"\
"   -lut <look up table file>\n"\
"        Applies a color look up table to the image while exporting.\n"\
"        Only allowed for single-band images.  Images must contain byte (8-bit)\n"\
"        data (except for data deriving from PolSARpro classifications.) Some\n"\
"        look-up table files are in the look_up_tables subdirectory in\n"\
"        the asf_tools share directory.  The tool will look in\n"\
"        this directory for the specified file if it isn't found\n"\
"        in the current directory.\n"\
"   -truecolor\n"\
"        For 3 or 4 band optical satellite images where the first band is the\n"\
"        the blue band, the second green, and the third red.  This option will\n"\
"        export the third band as the red element, the second band as the green\n"\
"        element, and the first band as the blue element.  Performs a 2-sigma\n"\
"        constrast expansion on each individual band during the export (similar\n"\
"        to most GIS software packages.)  To export a true-color image WITHOUT\n"\
"        the contrast expansion associated with the truecolor option, use the\n"\
"        -rgb option instead.  The rgb option will directly assign the\n"\
"        available bands, unaltered, to the RGB color channels in the output\n"\
"        file, e.g.\n\n"\
"          'asf_export -rgb 03 02 01 <infile> <outfile>'.\n\n"\
"        Only allowed for multi-band images with 3 or more bands.\n"\
"        The truecolor option cannot be used together with any of the\n"\
"        following options: -rgb, -band, or -falsecolor.\n"\
"   -falsecolor\n"\
"        For 4 band optical satellite images where the second band is green,\n"\
"        the third red, and the fourth is the near-infrared band.  Exports\n"\
"        the fourth (IR) band as the red element, the third band as the green\n"\
"        element, and the second band as the blue element.  Performs a 2-sigma\n"\
"        constrast expansion on the individual bands during the export.  To\n"\
"        export a falsecolor image WITHOUT the contrast expansion, use the\n"\
"        -rgb flag to directly assign the available bands to the RGB color\n"\
"        channels, e.g.\n\n"\
"          'asf_export -rgb 04 03 02 <infile> <outfile>'.\n\n"\
"        Only allowed for multi-band images with 4 bands.\n"\
"        Cannot be used together with any of the following: -rgb, -band,\n"\
"        or -truecolor.\n"\
"   -band <band_id | all>\n"\
"        If the data contains multiple data files, one for each band (channel)\n"\
"        then export the band identified by 'band_id' (only).  If 'all' is\n"\
"        specified rather than a band_id, then export all available bands into\n"\
"        individual files, one for each band.  Default is '-band all'.\n"\
"        Cannot be chosen together with the -rgb option.\n"\
"   -log <logFile>\n"\
"        Output will be written to a specified log file.\n"\
"   -quiet\n"\
"        Supresses all non-essential output.\n"\
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"   -help\n"\
"        Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"   To export to the default GeoTIFF format from file1.img and file1.meta\n"\
"   to file1.tif:\n"\
"        example> "ASF_NAME_STRING" file1 file1\n\n"\
"   NOTE: When exporting to a GeoTIFF format file, all map-projection\n"\
"         information is included in GeoKeys as specified in the GeoTIFF\n"\
"         standard.\n\n"\
"   NOTE: When exporting to a GeoTIFF format file, the data format (floating\n"\
"         point, byte, 16-bit integer, etc) will be maintained.  Many viewers\n"\
"         cannot view non-integer data.  Leaving the data format the same as\n"\
"         the original produces the most accurate export, but remapping it to\n"\
"         byte range (0-255) with the -byte option will result in the greatest\n"\
"         compatibility with viewers and GIS software packages.\n"\
"\n"\
"   To export to file2.jpg in the jpeg format:\n"\
"        example> "ASF_NAME_STRING" -format jpeg file1 file2\n"\
"\n"

#define ASF_LIMITATIONS_STRING \
"   Currently supports ingest of ASF format floating point and byte data (only).\n"\
"\n"\
"   Floating-point image formats (i.e., geotiff) are not supported in many\n"\
"   image viewing programs.\n"

#define ASF_SEE_ALSO_STRING \
"   asf_mapready, asf_import\n"

/*===================END ASF DOCUMENTATION===================*/


#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include <cla.h>
#include <envi.h>
#include <esri.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_statistics.h>

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_export.h>
#include <asf_contact.h>
#include <asf_license.h>

// Local prototypes
int is_numeric(char *str);

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
      "Version:\n   %s\n\n",
      version_string(ASF_NAME_STRING));
  exit(EXIT_FAILURE);
}

int
checkForOption (char *key, int argc, char *argv[])
{
  int ii = 0;
  while ( ii < argc ) {
    if ( strmatch (key, argv[ii]) )
      return ii;
    ++ii;
  }
  return FLAG_NOT_SET;
}

static const char *sigma_str(int with_sigma) {
    return with_sigma ? "w/sigma" : "";
}

// Main program body.
int
main (int argc, char *argv[])
{
  output_format_t format = 0;
  meta_parameters *md;
  char *in_base_name, *output_name;
  char **band_names=NULL;
  int rgb=0;
  int true_color = FALSE;
  int false_color = FALSE;
  int num_bands_found = 0;
  int ignored[3] = {0, 0, 0};
  int num_ignored = 0;

  in_base_name = (char *) MALLOC(sizeof(char)*255);
  output_name = (char *) MALLOC(sizeof(char)*255);

/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  // Command line input goes in it's own structure.
  command_line_parameters_t command_line;
  strcpy (command_line.format, "");
  command_line.size = NO_MAXIMUM_OUTPUT_SIZE;
  strcpy (command_line.in_data_name, "");
  strcpy (command_line.in_meta_name, "");
  strcpy (command_line.output_name, "");
  command_line.verbose = FALSE;
  command_line.quiet = FALSE;
  strcpy (command_line.leader_name, "");
  strcpy (command_line.cal_params_file, "");
  strcpy (command_line.cal_comment, "");
  command_line.sample_mapping = 0;
  strcpy(command_line.red_channel, "");
  strcpy(command_line.green_channel, "");
  strcpy(command_line.blue_channel, "");
  strcpy(command_line.band, "");
  strcpy(command_line.look_up_table_name, "");
  command_line.use_pixel_is_point = 0;

  int formatFlag, logFlag, quietFlag, byteFlag, rgbFlag, bandFlag, lutFlag, pixelIsPointFlag;
  int truecolorFlag, falsecolorFlag;
  int needed_args = 3;  //command & argument & argument
  int ii;
  char sample_mapping_string[25];

  //Check to see which options were specified
  if (   (checkForOption("--help", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-h", argc, argv) != FLAG_NOT_SET)
      || (checkForOption("-help", argc, argv) != FLAG_NOT_SET) ) {
      print_help();
  }
  get_asf_share_dir_with_argv0(argv[0]);
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  formatFlag = checkForOption ("-format", argc, argv);
  logFlag = checkForOption ("-log", argc, argv);
  quietFlag = checkForOption ("-quiet", argc, argv);
  byteFlag = checkForOption ("-byte", argc, argv);
  rgbFlag = checkForOption ("-rgb", argc, argv);
  bandFlag = checkForOption ("-band", argc, argv);
  lutFlag = checkForOption ("-lut", argc, argv);
  truecolorFlag = checkForOption("-truecolor", argc, argv);
  falsecolorFlag = checkForOption("-falsecolor", argc, argv);
  pixelIsPointFlag = checkForOption("-point", argc, argv);

  if ( formatFlag != FLAG_NOT_SET ) {
    needed_args += 2;           // Option & parameter.
  }
  if ( quietFlag != FLAG_NOT_SET ) {
    needed_args += 1;           // Option & parameter.
  }
  if ( logFlag != FLAG_NOT_SET ) {
    needed_args += 2;           // Option & parameter.
  }
  if ( byteFlag != FLAG_NOT_SET ) {
    needed_args += 2;           // Option & parameter.
  }
  if ( rgbFlag != FLAG_NOT_SET ) {
    needed_args += 4;           // Option & 3 parameters.
  }
  if ( bandFlag != FLAG_NOT_SET ) {
    needed_args += 2;           // Option & parameter.
  }
  if ( lutFlag != FLAG_NOT_SET ) {
    needed_args += 2;           // Option & parameter.
  }
  if ( truecolorFlag != FLAG_NOT_SET ) {
    needed_args += 1;           // Option only
  }
  if ( falsecolorFlag != FLAG_NOT_SET ) {
    needed_args += 1;           // Option only
  }
  if ( pixelIsPointFlag != FLAG_NOT_SET ) {
    needed_args += 1;
  }
  if ( argc != needed_args ) {
    print_usage ();                   // This exits with a failure.
  }

  // We also need to make sure the last three options are close to
  // what we expect.
  if ( argv[argc - 1][0] == '-' || argv[argc - 2][0] == '-' ) {
    print_usage (); // This exits with a failure.
  }

  // Make sure any options that have parameters are followed by
  // parameters (and not other options) Also make sure options'
  // parameters don't bleed into required arguments.
  if ( formatFlag != FLAG_NOT_SET ) {
    if ( argv[formatFlag + 1][0] == '-' || formatFlag >= argc - 3 ) {
      print_usage ();
    }
  }
  if ( byteFlag != FLAG_NOT_SET ) {
    if ( argv[byteFlag + 1][0] == '-' || byteFlag >= argc - 3 ) {
      print_usage ();
    }
  }
  if ( rgbFlag != FLAG_NOT_SET ) {
    if (( argv[rgbFlag + 1][0] == '-' && argv[rgbFlag + 2][0] == '-' &&
          argv[rgbFlag + 3][0] == '-' ) || rgbFlag >= argc - 5 ) {
      print_usage ();
    }
  }
  if ( bandFlag != FLAG_NOT_SET ) {
    if ( argv[bandFlag + 1][0] == '-' || bandFlag >= argc - 3 ) {
      print_usage ();
    }
  }
  if ( lutFlag != FLAG_NOT_SET ) {
    if ( argv[lutFlag + 1][0] == '-' || lutFlag >= argc - 3 ) {
      print_usage ();
    }
  }
  if ( logFlag != FLAG_NOT_SET ) {
    if ( argv[logFlag + 1][0] == '-' || logFlag >= argc - 3 ) {
      print_usage ();
    }
  }

  // Make sure there are no flag incompatibilities
  if ( (rgbFlag != FLAG_NOT_SET           &&
        (bandFlag != FLAG_NOT_SET         ||
         truecolorFlag != FLAG_NOT_SET    ||
         falsecolorFlag != FLAG_NOT_SET))
     ||
       (bandFlag != FLAG_NOT_SET         &&
        (rgbFlag != FLAG_NOT_SET         ||
         truecolorFlag != FLAG_NOT_SET   ||
         falsecolorFlag != FLAG_NOT_SET))
     ||
       (truecolorFlag != FLAG_NOT_SET    &&
        (bandFlag != FLAG_NOT_SET        ||
         rgbFlag != FLAG_NOT_SET         ||
         falsecolorFlag != FLAG_NOT_SET))
     ||
       (falsecolorFlag != FLAG_NOT_SET   &&
        (bandFlag != FLAG_NOT_SET        ||
         truecolorFlag != FLAG_NOT_SET   ||
         rgbFlag != FLAG_NOT_SET))
     )
  {
    asfPrintWarning("The following options may only be used one at a time:\n"
        "    %s\n    %s\n    %s\n    %s\n    %s\n    %s\n",
        "-rgb", "-truecolor", "-falsecolor", "-band");
    print_help();
  }
  if ( (rgbFlag != FLAG_NOT_SET         ||
        truecolorFlag != FLAG_NOT_SET   ||
        falsecolorFlag != FLAG_NOT_SET) &&
        lutFlag != FLAG_NOT_SET
     )
    asfPrintError("Look up table option can only be used on single-band "
          "images.\n");

  if( logFlag != FLAG_NOT_SET ) {
    strcpy(logFile, argv[logFlag+1]);
  }
  else {
    //sprintf(logFile, "tmp%i.log", (int)getpid());
    strcpy(logFile, get_tmp_log_file("asf_export"));
  }
  logflag = TRUE; // Since we always log, set the old school logflag to true
  fLog = FOPEN (logFile, "a");

  // Set old school quiet flag (for use in our libraries)
  quietflag = ( quietFlag != FLAG_NOT_SET ) ? TRUE : FALSE;

  // We're good enough at this point... print the splash screen.
  asfSplashScreen (argc, argv);

  // Grab the input and output name
  strcpy (in_base_name, argv[argc - 2]);
  strcpy (output_name, argv[argc - 1]);
  strcpy (command_line.output_name, output_name);

  // If user added ".img", strip it.
  char *ext = findExt(in_base_name);
  if (ext && strcmp(ext, ".img") == 0) *ext = '\0';

  // Set default output type
  if( formatFlag != FLAG_NOT_SET ) {
    strcpy (command_line.format, argv[formatFlag + 1]);
  }
  else {
    // Default behavior: produce a geotiff.
    strcpy (command_line.format, "geotiff");
  }

  // Compose input metadata name
  strcpy (command_line.in_meta_name, in_base_name);
  strcat (command_line.in_meta_name, ".meta");

  if (strcmp_case(command_line.format, "HDF5") != 0 &&
    strcmp_case(command_line.format, "NETCDF") != 0) { 
    // for some validation, need the metadata
    md = meta_read (command_line.in_meta_name);

    if (strcmp_case (command_line.format, "PGM") == 0 &&
        (rgbFlag != FLAG_NOT_SET      ||
        truecolorFlag != FLAG_NOT_SET ||
        falsecolorFlag != FLAG_NOT_SET)
       )
    {
      asfPrintWarning("Greyscale PGM output is not compatible with color options:\n"
                      "(RGB, True Color, False Color, color look-up tables, etc\n)"
                      "...Defaulting to producing separate greyscale PGM files for available band.\n");
      rgbFlag = FLAG_NOT_SET;
      truecolorFlag = FLAG_NOT_SET;
      falsecolorFlag = FLAG_NOT_SET;
    }

    // Set the default byte scaling mechanisms
    if (md->optical) {
       // for optical data, default sample mapping is NONE
        command_line.sample_mapping = NONE;
    }
    // for other data, default is based on the output type
    else if (strcmp_case (command_line.format, "TIFF") == 0 ||
             strcmp_case (command_line.format, "TIF")  == 0 ||
             strcmp_case (command_line.format, "JPEG") == 0 ||
             strcmp_case (command_line.format, "JPG")  == 0 ||
             strcmp_case (command_line.format, "PNG")  == 0 ||
             strcmp_case (command_line.format, "PGM")  == 0 ||
       strcmp_case (command_line.format, "PNG_ALPHA") == 0 ||
       strcmp_case (command_line.format, "PNG_GE") == 0)
    {
      command_line.sample_mapping = SIGMA;
    }
    else if (strcmp_case (command_line.format, "GEOTIFF") == 0) {
      command_line.sample_mapping = NONE;
    }

    if ( quietFlag != FLAG_NOT_SET )
      command_line.quiet = TRUE;
    else
      command_line.quiet = FALSE;

    // Set rgb combination
    if ( rgbFlag != FLAG_NOT_SET ) {
      int i;

      for (i=0, num_ignored = 0; i<3; i++) {
        ignored[i] = strncmp("IGNORE", uc(argv[rgbFlag + i + 1]), 6) == 0 ? 1 : 0;
        num_ignored += ignored[i] ? 1 : 0;
      }
      asfRequire(num_ignored < 3,
                 "Cannot ignore all bands.  Exported image would be blank.\n");

      strcpy (command_line.red_channel, ignored[0] ? "Ignored" : argv[rgbFlag + 1]);
      strcpy (command_line.green_channel, ignored[1] ? "Ignored" : argv[rgbFlag + 2]);
      strcpy (command_line.blue_channel, ignored[2] ? "Ignored" : argv[rgbFlag + 3]);

      // Check to see if the bands are numeric and in range
      int r_channel = atoi(command_line.red_channel);
      int g_channel = atoi(command_line.green_channel);
      int b_channel = atoi(command_line.blue_channel);

      /////////// Numeric channel case ////////////
      // Remove trailing non-numeric characters from the channel number
      // string and pad front end nicely with a zero
      if (!ignored[0] && is_numeric(command_line.red_channel) &&
          r_channel >= 1 && r_channel <= MAX_BANDS) {
        sprintf(command_line.red_channel, "%02d", atoi(command_line.red_channel));
      }
      if (!ignored[1] && is_numeric(command_line.green_channel) &&
          g_channel >= 1 && g_channel <= MAX_BANDS) {
        sprintf(command_line.green_channel, "%02d", atoi(command_line.green_channel));
      }
      if (!ignored[2] && is_numeric(command_line.blue_channel) &&
          b_channel >= 1 && b_channel <= MAX_BANDS) {
        sprintf(command_line.blue_channel, "%02d", atoi(command_line.blue_channel));
      }
    }

    // Set up the bands for true or false color optical data
    true_color = false_color = 0;
    int with_sigma = FALSE;
    if (truecolorFlag != FLAG_NOT_SET || falsecolorFlag != FLAG_NOT_SET) {
      int ALOS_optical = (md->optical && strncmp(md->general->sensor, "ALOS", 4) == 0) ? 1 : 0;
      if (md->optical && truecolorFlag != FLAG_NOT_SET) {
        if (ALOS_optical) {
          with_sigma = TRUE;
          strcpy(command_line.red_channel,   "03");
          strcpy(command_line.green_channel, "02");
          strcpy(command_line.blue_channel,  "01");
          true_color = 1;
          asfPrintStatus("Applying True Color contrast expansion to following channels:");
        }
        else {
          char **bands = extract_band_names(md->general->bands, 3);
          asfRequire(bands != NULL,
                     "-truecolor option specified for non-true color optical image.\n");

          asfPrintWarning("Attempting to use the -truecolor option with non-ALOS\n"
              "optical data.\n");
          strcpy(command_line.red_channel, bands[2]);
          strcpy(command_line.green_channel, bands[1]);
          strcpy(command_line.blue_channel, bands[0]);
          int i;
          for (i=0; i<3; i++) {
            FREE(bands[i]);
          }
          FREE(bands);
        }
      }
      if (md->optical && falsecolorFlag != FLAG_NOT_SET) {
        if (ALOS_optical) {
          with_sigma = TRUE;
          strcpy(command_line.red_channel,   "04");
          strcpy(command_line.green_channel, "03");
          strcpy(command_line.blue_channel,  "02");
          false_color = 1;
          asfPrintStatus("Applying False Color contrast expansion to the following channels:");
        }
        else {
          char **bands = extract_band_names(md->general->bands, 4);
          asfRequire(bands != NULL,
                     "-falsecolor option specified for an optical image with fewer than 4 bands.\n");

          asfPrintWarning("Attempting to use the -falsecolor option with non-ALOS\n"
              "optical data.\n");
          strcpy(command_line.red_channel, bands[3]);
          strcpy(command_line.green_channel, bands[2]);
          strcpy(command_line.blue_channel, bands[1]);
          int i;
          for (i=0; i<3; i++) {
            FREE(bands[i]);
          }
          FREE(bands);
        }
      }
      if (!ALOS_optical && !md->optical) {
        asfPrintError("-truecolor or -falsecolor option selected with non-optical data\n");
      }
    }

    if (rgbFlag != FLAG_NOT_SET ||
       truecolorFlag != FLAG_NOT_SET ||
       falsecolorFlag != FLAG_NOT_SET)
    {
      char red_band[16], green_band[16], blue_band[16];

      asfPrintStatus("\nRed channel  : %s %s\n", command_line.red_channel, sigma_str(with_sigma));
      asfPrintStatus("Green channel: %s %s\n", command_line.green_channel, sigma_str(with_sigma));
      asfPrintStatus("Blue channel : %s %s\n\n", command_line.blue_channel, sigma_str(with_sigma));

      if (is_numeric(command_line.red_channel) &&
          is_numeric(command_line.green_channel) &&
          is_numeric(command_line.blue_channel))
      {
        sprintf(red_band, "%02d", atoi(command_line.red_channel));
        sprintf(green_band, "%02d", atoi(command_line.green_channel));
        sprintf(blue_band, "%02d", atoi(command_line.blue_channel));
        band_names = find_bands(in_base_name, rgbFlag,
                                red_band,
                                green_band,
                                blue_band,
                                &num_bands_found);
      }
      else {
        band_names = find_bands(in_base_name, rgbFlag,
                                command_line.red_channel,
                                command_line.green_channel,
                                command_line.blue_channel,
                                &num_bands_found);
      }
    }

    // Set band
    if ( bandFlag != FLAG_NOT_SET) {
      strcpy (command_line.band, argv[bandFlag + 1]);
      band_names = find_single_band(in_base_name, command_line.band,
                    &num_bands_found);
    }
    else if (rgbFlag == FLAG_NOT_SET &&
            truecolorFlag == FLAG_NOT_SET &&
            falsecolorFlag == FLAG_NOT_SET &&
            bandFlag == FLAG_NOT_SET) {
      bandFlag=1; // For proper messaging to the user
      strcpy (command_line.band, "all");
      band_names = find_single_band(in_base_name, command_line.band,
                                    &num_bands_found);
    }

    // Read look up table name
    if ( lutFlag != FLAG_NOT_SET) {
      strcpy(command_line.look_up_table_name, argv[lutFlag + 1]);
      rgb = 1;
    }

    // Set scaling mechanism
    if ( byteFlag != FLAG_NOT_SET ) {
      strcpy (sample_mapping_string, argv[byteFlag + 1]);
      for ( ii = 0; ii < strlen(sample_mapping_string); ii++) {
        sample_mapping_string[ii] = toupper (sample_mapping_string[ii]);
      }
      if ( strcmp (sample_mapping_string, "TRUNCATE") == 0 )
        command_line.sample_mapping = TRUNCATE;
      else if ( strcmp(sample_mapping_string, "MINMAX") == 0 )
        command_line.sample_mapping = MINMAX;
      else if ( strcmp(sample_mapping_string, "SIGMA") == 0 )
        command_line.sample_mapping = SIGMA;
      else if ( strcmp(sample_mapping_string, "HISTOGRAM_EQUALIZE") == 0 )
        command_line.sample_mapping = HISTOGRAM_EQUALIZE;
      else if ( strcmp(sample_mapping_string, "NONE") == 0 ) {
          asfPrintWarning("Sample remapping method (-byte option) is set to NONE\n"
                  "which doesn't make sense.  Defaulting to TRUNCATE...\n");
          command_line.sample_mapping = TRUNCATE;
      }
      else
        asfPrintError("Unrecognized byte scaling method '%s'.\n",
                      sample_mapping_string);
    }

    int is_polsarpro = (md->general->bands && strstr(md->general->bands, "POLSARPRO") != NULL) ? 1 : 0;
    if ( !is_polsarpro               &&
         lutFlag != FLAG_NOT_SET     &&
         bandFlag == FLAG_NOT_SET    &&
         md->general->band_count > 1)
    {
      asfPrintError("Look up tables can only be applied to single band"
            " images\n");
    }

    if ( !is_polsarpro                       &&
         lutFlag != FLAG_NOT_SET             &&
         command_line.sample_mapping == NONE &&
         md->general->data_type != ASF_BYTE      &&
         md->general->band_count == 1)
    {
      asfPrintError("Look up tables can only be applied to byte output"
            " images\n");
    }

    // Report what is going to happen
    if (rgbFlag != FLAG_NOT_SET ||
       truecolorFlag != FLAG_NOT_SET ||
       falsecolorFlag != FLAG_NOT_SET)
    {
      if (num_bands_found >= 3) {
        asfPrintStatus("Exporting multiband image ...\n\n");
        rgb = 1;
      }
      else {
        asfPrintError("Not all RGB channels found.\n");
      }
    }
    else if (bandFlag != FLAG_NOT_SET) {
      if (strcmp_case(command_line.band, "ALL") == 0) {
        if (multiband(command_line.format, 
          extract_band_names(md->general->bands, md->general->band_count), 
          md->general->band_count))
    asfPrintStatus("Exporting multiband image ...\n\n");
        else  if (num_bands_found > 1)
          asfPrintStatus("Exporting each band into individual greyscale files ...\n\n");
      }
      else if (num_bands_found == 1) {
        if (lutFlag != FLAG_NOT_SET)
      asfPrintStatus("Exporting band '%s' applying look up table ...\n\n",
                 command_line.band);
        else
      asfPrintStatus("Exporting band '%s' as greyscale ...\n\n",
                 command_line.band);
      }
      else
        asfPrintError("Band could not be found in the image.\n");
    }
    else if (lutFlag != FLAG_NOT_SET)
      asfPrintStatus("Exporting applying look up table.\n\n");
    else
      asfPrintStatus("Exporting as greyscale.\n\n");

    //If user added ".img", strip it.
    ext = findExt(in_base_name);
    if (ext && strcmp(ext, ".img") == 0) *ext = '\0';
    meta_free(md);
  }

  if (strcmp_case(command_line.format, "GEOTIFF") != 0 &&
      pixelIsPointFlag != FLAG_NOT_SET )
  {
    asfPrintWarning("-point option has no effect");
  }
  else {
    command_line.use_pixel_is_point = pixelIsPointFlag != FLAG_NOT_SET; 
  }

/***********************END COMMAND LINE PARSING STUFF***********************/

  if ( strcmp_case (command_line.format, "ENVI") == 0 ) {
    format = ENVI;
  }
  else if ( strcmp_case (command_line.format, "ESRI") == 0 ) {
    format = ESRI;
  }
  else if ( strcmp_case (command_line.format, "GEOTIFF") == 0 ||
            strcmp_case (command_line.format, "GEOTIF") == 0) {
    format = GEOTIFF;
  }
  else if ( strcmp_case (command_line.format, "TIFF") == 0 ||
            strcmp_case (command_line.format, "TIF") == 0) {
    format = TIF;
  }
  else if ( strcmp_case (command_line.format, "JPEG") == 0 ||
            strcmp_case (command_line.format, "JPG") == 0) {
    format = JPEG;
  }
  else if ( strcmp_case (command_line.format, "PGM") == 0 ) {
    format = PGM;
  }
  else if ( strcmp_case (command_line.format, "PNG") == 0 ) {
    format = PNG;
  }
  else if ( strcmp_case (command_line.format, "PNG_ALPHA") == 0 ) {
    format = PNG_ALPHA;
  }
  else if ( strcmp_case (command_line.format, "PNG_GE") == 0 ) {
    format = PNG_GE;
  }
  else if ( strcmp_case (command_line.format, "KML") == 0 ) {
    format = KML;
  }
  else if ( strcmp_case (command_line.format, "POLSARPRO") == 0 ) {
    format = POLSARPRO_HDR;
  }
  else if ( strcmp_case (command_line.format, "HDF5") == 0 ) {
    format = HDF;
  }
  else if ( strcmp_case (command_line.format, "NETCDF") == 0 ) {
    format = NC;
  }
  else {
    asfPrintError("Unrecognized output format specified\n");
  }

  /* Complex data generally can't be output into meaningful images, so
     we refuse to deal with it.  */
  /*
  md = meta_read (command_line.in_meta_name);
  asfRequire (   md->general->data_type == ASF_BYTE
              || md->general->data_type == INTEGER16
              || md->general->data_type == INTEGER32
              || md->general->data_type == REAL32
              || md->general->data_type == REAL64,
              "Cannot cope with complex data, exiting...\n");

  meta_free (md);
  */

  // Do that exporting magic!
  asf_export_bands(format, command_line.sample_mapping, rgb,
                   true_color, false_color,
                   command_line.look_up_table_name, command_line.use_pixel_is_point,
                   in_base_name, command_line.output_name, band_names,
                   NULL, NULL);

  // If the user didn't ask for a log file then nuke the one that's been kept
  // since everything has finished successfully
  if (logFlag == FLAG_NOT_SET) {
      fclose (fLog);
      remove(logFile);
  }

  if (band_names) {
    for (ii = 0; ii<num_bands_found; ii++) {
      FREE(band_names[ii]);
    }
    FREE(band_names);
  }
  FREE(in_base_name);
  FREE(output_name);
  exit (EXIT_SUCCESS);
}

int is_numeric(char *str)
{
  char *s = str;
  int numericness = 1;

  while (*s != '\0') {
    if (!isdigit(*s)) numericness = 0;
    s++;
  }

  return numericness;
}
