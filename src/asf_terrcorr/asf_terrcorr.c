#define ASF_NAME_STRING "asf_terrcorr"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-keep (-k)]\n"\
"          [-no-resample] [-no-interp] [-pixel-size <size>]\n"\
"          [-mask-file <filename> | -auto-water-mask]\n"\
"          [-fill <fill value> | -no-fill] [-update-original-meta (-u)]\n"\
"          <in_base_name> <dem_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program takes two inputs: (1) An unprojected SAR image\n"\
"     in the ASF internal format, and (2) a geocoded digital elevation\n"\
"     model (DEM) encompassing the area in the SAR image, also in the\n"\
"     ASF internal format.  The output is an image that has been\n"\
"     corrected for the distortions introduced by the side-looking\n"\
"     geometry and the local topography.\n"

#define ASF_INPUT_STRING \
"     Two input files are required.  Each input file, the SAR image and\n"\
"     the DEM, need to be in the ASF internal format, and you should just\n"\
"     specify the basename (i.e., do not give the file's extension) of each.\n"\
"     The input SAR image can be in either slant range or ground range.  If\n"\
"     it is in ground range, it will be converted to slant range.\n"

#define ASF_OUTPUT_STRING \
"     The base name of the terrain corrected image to produce.  The output\n"\
"     image is in ground range.\n\n"\
"     A layover/shadow mask of the region will also be automatically produced\n"\
"     and it will have the same base name as the output file, with the\n"\
"     suffix '_mask' added.\n"

#define ASF_OPTIONS_STRING \
"     -keep (-k)\n"\
"          The terrain correction process produces a number of intermediate\n"\
"          files on the way to generating the final product.  Normally, these\n"\
"          are deleted when the process completes, however if you wish to\n"\
"          keep these files around you may do so by adding this option.\n"\
"\n"\
"     -no-resample\n"\
"          If the DEM has a pixel size that is significantly larger than\n"\
"          the SAR image, by default the SAR image is downsampled to a pixel\n"\
"          size half that of the DEM.  With -no-resample, no resampling of\n"\
"          this type will be done.  However, the quality of the terrain\n"\
"          corrected product is still limited by the resolution of the DEM.\n"\
"\n"\
"     -no-interp\n"\
"          Layover regions in the output image are going to contain data that\n"\
"          is not scientifically valid.  With this option, those regions will be\n"\
"          left blank instead of filled with interpolated values.\n\n"\
"          If you are planning to analyze image statistics, you will probably\n"\
"          want to use this option, even though the results won't look as\n"\
"          pretty.\n"\
"\n"\
"     -pixel_size <pixel spacing>\n"\
"          Specifies the pixel spacing of the terrain corrected image.\n"\
"          "ASF_NAME_STRING" by default will preserve the pixel size of the input\n"\
"          image (however, in cases where the DEM and SAR resolution differ by\n"\
"          more than a factor of two your image may be automatically\n"\
"          downsampled).\n"\
"\n"\
"     -mask-file <filename>\n"\
"          Specify a file that contains regions to be omitted from terrain\n"\
"          correction.  These regions will also not be considered when\n"\
"          attempting to correlate the SAR and DEM images.  Since accurate\n"\
"          correlation is essential for a successful terrain correction,\n"\
"          images with large featureless regions (such as water, or glaciers)\n"\
"          may not correlate well, and therefore may not terrain correct very\n"\
"          well, either.  You may use this option to mask those regions,\n"\
"          confining the correlation to areas that you expect to provide good\n"\
"          matches.\n\n"\
"          The mask file should be 0 in the unmasked regions, and positive in\n"\
"          the masked regions.\n\n"\
"          By default, the output image is left blank (filled with zeros) in the\n"\
"          regions that are masked.  You may override this behavior using the\n"\
"          -fill or -no-fill options, below.\n\n"\
"          You cannot use this option together with -auto-water-mask.\n"\
"\n"\
"     -auto-water-mask\n"\
"          A mask file (see the -mask-file option, above) is automatically\n"\
"          produced from the DEM.  Areas where the DEM height is less than 1\n"\
"          meter are masked.\n\n"\
"          You cannot use this option together with -mask-file.\n"\
"\n"\
"     -fill <fill value>\n"\
"          Specifies a particular value to put in the terrain corrected image\n"\
"          where the mask file (specified with -mask-file) indicates that the\n"\
"          pixel is masked.  By default, the fill value is 0.\n"\
"\n"\
"     -no-fill\n"\
"          With this option, instead of filling the masked regions (specified\n"\
"          with -mask-file) with a constant fill value, the masked pixels will\n"\
"          be filled with pixel values from the original SAR image.\n\n"\
"          This may not work particularly well when the masked regions have high\n"\
"          elevation.\n"\
"\n"\
"     -update-original-meta (-u)\n"\
"          The correlation process that is done during the terrain correction\n"\
"          process produces offsets in range and azimuth that, when applied \n"\
"          to the SAR image's metadata, result in a good match with the DEM.\n"\
"          When specifying this option, the input SAR image's metadata is\n"\
"          updated with these offsets, increasing the accuracy of the original\n"\
"          data.\n"\
"\n"\
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -quiet\n"\
"          Supresses all non-essential output.\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"     Terrain correction using default options:\n"\
"     > "ASF_NAME_STRING" input_image dem_image output_image\n\n"\
"     Terrain correct with an output image pixel size of 30 meters:\n"\
"     > "ASF_NAME_STRING" -pixel-size 30 input_image dem_image output_image\n\n"\
"     Update the metadata for input_image to correct geolocation errors:\n"\
"     > "ASF_NAME_STRING" -u input_image dem_image output_image\n\n"\
"     Ignore water regions during terrain correction:\n"\
"     > "ASF_NAME_STRING" -auto-water-mask input_image dem_image output_image\n"

#define ASF_LIMITATIONS_STRING \
"     Can be quite slow when terrain correcting with a mask.\n"

#define ASF_SEE_ALSO_STRING \
"     asf_import, asf_export\n"

#include <stdio.h>
#include <asf.h>
#include <asf_sar.h>
#include <asf_terrcorr.h>
#include <asf_license.h>
#include <asf_contact.h>

#define NUM_ARGS 3

// Print minimalistic usage info & exit
static void usage(const char *name)
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

int strmatches(const char *key, ...)
{
  va_list ap;
  char *arg = NULL;
  int found = FALSE;

  va_start(ap, key);
  do {
    arg = va_arg(ap, char *);
    if (arg) {
      if (strcmp(key, arg) == 0) {
	found = TRUE;
	break;
      }
    }
  } while (arg);

  return found;
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *demFile, *inMaskFile, *outFile;
  double pixel_size = -1;
  int dem_grid_size = 20;
  int currArg = 1;
  int clean_files = TRUE;
  int do_resample = TRUE;
  int do_interp = TRUE;
  int do_fftMatch_verification = TRUE;
  int do_corner_matching = FALSE;
  int generate_water_mask = FALSE;
  int save_clipped_dem = FALSE;
  int update_original_metadata_with_offsets = FALSE;

  // -1 -> no masking, other values mean fill it with that value
  int fill_value = 0; 

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);
  inMaskFile = NULL;

  if (strmatches(argv[1],"-help","--help",NULL)) {
      print_help();
  }

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
        CHECK_ARG(1);
        strcpy(logFile,GET_ARG(1));
        fLog = FOPEN(logFile, "a");
        logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
        quietflag = TRUE;
    }
    else if (strmatches(key,"-keep","--keep","-k",NULL)) {
        clean_files = FALSE;
    }
    else if (strmatches(key,"-no-resample","--no-resample",NULL)) {
        do_resample = FALSE;
    }
    else if (strmatches(key,"-no-verify-match","--no-verify-match",NULL)) {
        do_fftMatch_verification = FALSE;
    }
    else if (strmatches(key,"-no-corner-match","--no-corner-match",NULL)) {
        do_corner_matching = FALSE;
    }
    else if (strmatches(key,"-no-interp","--no-interp",NULL)) {
        do_interp = FALSE;
    }
    else if (strmatches(key,"-pixel-size","--pixel-size","-ps",NULL)) {
        CHECK_ARG(1);
        pixel_size = atof(GET_ARG(1));
    }
    else if (strmatches(key,"-dem-grid-size","--dem-grid-size",NULL)) {
        CHECK_ARG(1);
        dem_grid_size = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"-mask-file","--mask-file",NULL)) {
        CHECK_ARG(1);
        inMaskFile = GET_ARG(1);
    }
    else if (strmatches(key,"-auto-water-mask","--auto-water-mask",NULL)) {
        generate_water_mask = TRUE;
    }
    else if (strmatches(key, "-u", "-update-original-meta",
                        "--update-original-meta", NULL))
    {
        update_original_metadata_with_offsets = TRUE;
    }
    else if (strmatches(key,"-fill","--fill",NULL)) {
        CHECK_ARG(1);
        fill_value = atoi(GET_ARG(1)); // user requested a specific fill value
    }
    else if (strmatches(key,"-no-fill","--no-fill",NULL)) {
        // leave masked regions alone - fill with sar data
        fill_value = LEAVE_MASK; 
    }
    else if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(ASF_NAME_STRING);
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(ASF_NAME_STRING);
  }

  inFile = argv[currArg];
  demFile = argv[currArg+1];
  outFile = argv[currArg+2];

  int ret =  asf_terrcorr_ext(inFile, demFile,inMaskFile,outFile, pixel_size,
                              clean_files, do_resample, do_corner_matching,
                              do_interp, do_fftMatch_verification,
                              dem_grid_size, TRUE, fill_value, 
                              generate_water_mask, save_clipped_dem,
                              update_original_metadata_with_offsets);

  return ret==0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
