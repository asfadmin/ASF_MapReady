#define ASF_NAME_STRING "asf_terrcorr"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-keep (-k)]\n"\
"          [-no-resample] [-no-interp] [-pixel-size <size>]\n"\
"          [-mask-file <filename> | -auto-water-mask]\n"\
"          [-mask-height-cutoff <height in meters>]\n"\
"          [-fill <fill value> | -no-fill] [-update-original-meta (-u)]\n"\
"          [-other-file <basename>] [-do-radiometric] [-smooth-dem-holes]\n"\
"          [-no-match] [-grid-match] [-offsets <range> <azimuth>]\n"\
"          [-use-zero-offsets-if-match-fails] [-save-ground-range-dem]\n"\
"          [-save-incidence-angles] [-use-nearest-neighbor]\n"\
"          [-use-bilinear]\n"\
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
"     it is in ground range, it will be converted to slant range.\n\n"\
"     Alternatively, instead of a DEM you may provide a directory containing\n"\
"     DEMs (in ASF Internal Format, with both .img and .meta files).\n"\
"     In this case, a suitable DEM will be selected from the DEMs in the\n"\
"     directory (including all subdirectories, recursively searched), or\n"\
"     one will be composed (i.e., mosaicked) from the DEMs in the given\n"\
"     directory.  All DEMs covering the given SAR scene are included in\n"\
"     the mosaic.\n"

#define ASF_OUTPUT_STRING \
"     The base name of the terrain corrected image to produce.  The output\n"\
"     image is in ground range.\n\n"\
"     A layover/shadow mask of the region will also be automatically produced\n"\
"     and it will have the same base name as the output file, with the\n"\
"     suffix '_mask' added.\n\n"\
"     The layover shadow mask values are as follows:\n"\
"          Normal: %d\n"\
"          User Masked: %d\n"\
"          Shadow: %d\n"\
"          Layover: %d\n"\
"          Invalid: %d\n\n"\
"     You may export the layover/shadow mask using asf_export to produce\n"\
"     a color-coded mask, by using the '-lut layover_mask' option with\n"\
"     asf_export.\n\n"

#define ASF_OPTIONS_STRING \
"     -keep (-k)\n"\
"          The terrain correction process produces a number of intermediate\n"\
"          files on the way to generating the final product.  Normally, these\n"\
"          temporary files are deleted when the process completes, however\n"\
"          if you wish to keep these files around you may do so with this\n"\
"          option.\n"\
"\n"\
"     -no-resample\n"\
"          If the DEM has a pixel size that is significantly larger (a factor\n"\
"          of 2) than the SAR image, by default the SAR image is downsampled\n"\
"          to a pixel size half that of the DEM.  With -no-resample, no\n"\
"          resampling of this type will be done.  However, the quality of the\n"\
"          terrain corrected product is still limited by the resolution of\n"\
"          the DEM.\n"\
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
"     -use-gr-dem\n"\
"     -use-sr-dem\n"\
"          The DEM that is provided to asf_terrcorr is generally in ground\n"\
"          range, and is converted to slant range as part of the coregistration\n"\
"          process.  After coregistration, you can use either the original\n"\
"          ground range DEM, or the slant range DEM, to do the actual terrain\n"\
"          correction.  By default, the slant range DEM is used, specifying\n"\
"          -use-gr-dem will use the ground range DEM.\n\n"\
"          The choice only makes a significant difference in regions of layover.\n"\
"          (And in those regions, the difference is quite significant!)\n"\
"\n"\
"          Both produce similar results in areas without layover.  In regions\n"\
"          of layover, however the results are quite different; using the slant\n"\
"          range DEM results in more aggressive interpolations, which sometimes\n"\
"          results in streaky-looking layover regions, whereas using the ground\n"\
"          range DEM preserves more structure within the layover regions, but\n"\
"          looks worse if the coregistration is off in those areas.\n"\
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
"          the masked regions.  In other words, the mask should contain non-zero\n"\
"          values in the area of interest.\n\n"\
"          By default, the output image is left blank (filled with zeros) in the\n"\
"          regions that are masked.  You may override this behavior using the\n"\
"          -fill or -no-fill options, below.\n\n"\
"          You cannot use this option together with -auto-water-mask.\n"\
"\n"\
"     -auto-water-mask\n"\
"          A mask file (see the -mask-file option, above) is automatically\n"\
"          produced from the DEM.  Areas where the DEM height is less than 1\n"\
"          meter are masked.  (Though you may change this cutoff with the\n"\
"          option -mask-height-cutoff.)\n\n"\
"          You cannot use this option together with -mask-file.\n"\
"\n"\
"     -mask-height-cutoff <height>\n"\
"          Only applies to automatic water masking.\n\n"\
"          DEM pixels of the given value or below are masked.  The default\n"\
"          is 1 meter.\n"\
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
"\n"\
"     -do-radiometric\n"\
"          Apply radiometric terrain correction.  Radiometric terrain correction\n"\
"          is still experimental.  Currently, this option scales values using\n"\
"          the method described by Ulander (\"Radiometric Slope Correction of\n"\
"          Synthetic-Aperture Radar Images\", IEEE Transactions on Geoscience and\n"\
"          Remote Sensing, Vol 34, No 5, Sept 1996). This correction scales\n"\
"          by cos(phi), where phi is the angle between the terrain normal, and\n"\
"          the image plane normal.\n"\
"\n"\
"     -smooth-dem-holes\n"\
"          Some DEMs have holes in them, this is particularly a problem with\n"\
"          SRTM DEMs.  If this is the case, the terrain corrected product will\n"\
"          often contain streaks in the areas near the holes.  You can fill\n"\
"          the holes with interpolated values prior to terrain correction with\n"\
"          this option, which will reduce or even eliminate these streaks.\n"\
"          However, you should still expect suboptimal results within the\n"\
"          holes.\n"\
"\n"\
"     -update-original-meta (-u)\n"\
"          The correlation process that is done during the terrain correction\n"\
"          process produces offsets in range and azimuth that, when applied \n"\
"          to the SAR image's metadata, result in a good match with the DEM.\n"\
"          When specifying this option, the input SAR image's metadata is\n"\
"          updated with these offsets, increasing the accuracy of the original\n"\
"          data.\n"\
"\n"\
"     -other-file <basename>\n"\
"          The correlation process that is done during the terrain correction\n"\
"          process produces offsets in range and azimuth that, when applied \n"\
"          to the SAR image's metadata, result in a good match with the DEM.\n"\
"          Sometimes, you will want these same offsets applied to other data\n"\
"          files.  Specifying -other-file will update the metadata for this\n"\
"          listed file with the calculated offsets, in addition to the files\n"\
"          already updated (specifically, the terrain corrected product, and,\n"\
"          if you specified -u, the input SAR image).\n\n"\
"          This option may be specified more than once.\n"\
"\n"\
"     -no-match\n"\
"          Don't use the DEM for matching.\n"\
"\n"\
"     -grid-match\n"\
"          Instead of using a full-image match, generate a fftmatch.txt file\n"\
"          with FFT match offsets using grid tiles over the images.  The\n"\
"          program will exit after generating this grid.  The user must\n"\
"          then warp the image (using fit_warp and remap tools) and then\n"\
"          run asf_terrcorr again without matching.\n"\
"\n"\
"     -offsets <range offset> <azimuth offset>\n"\
"          Use these offsets instead of matching the DEM to the SAR image.\n"\
"          The offsets are in pixels.\n"\
"\n"\
"     -use-zero-offsets-if-match-fails\n"\
"          Try to use the DEM to refine the geolocation of the SAR image\n"\
"          before terrain correcting, but if this fails proceed with terrain\n"\
"          correction using offsets of zero.  (I.e., assume the geolocation\n"\
"          of the SAR image is good enough for terrain correction.)  This\n"\
"          often produces good results, especially if the terrain is fairly\n"\
"          flat (a situation that often results in poor or failed\n"\
"          coregistration).\n\n"\
"          Normally, if the coregistration fails, asf_terrcorr will abort\n"\
"          with an error.\n"\
"\n"\
"     -save-ground-range-dem\n"\
"          Save the ground range DEM (for radiometric terrain correction "\
"later).\n"\
"\n"\
"     -save-incidence-angles\n"\
"          During radiometric correction, create a side product containing the\n"\
"          incidence angles from the sensor to vertical.\n"\
"\n"\
"     -no-speckle\n"\
"          When generating the simulated SAR image for co-registration, do\n"\
"          not add speckle.  Generally, this will cause co-registration to\n"\
"          fail; usually, you would do this only if you are interested only\n"\
"          in the simulated image itself (and not the terrain corrected\n"\
"          product).  So, processing will stop after generating the simulated\n"\
"          SAR image.\n"\
"\n"\
"     -use-nearest-neighbor\n"\
"     -use-bilinear\n"\
"          Specifies the interpolation method when performing geometric\n"\
"          terrain correction.  Using nearest neighbor preserves much\n"\
"          of the original radiometric charactaristics, using bilinear\n"\
"          results in a smoother image.  Default is bilinear.\n"\
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
"     refine_geolocation\n"

#include <stdio.h>
#include <asf.h>
#include <asf_sar.h>
#include <asf_terrcorr.h>
#include <asf_license.h>
#include <asf_contact.h>

#define NUM_ARGS 3
#define MAX_OTHER 10

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
      "Version:\n   %s\n\n",
      (int)MASK_NORMAL,(int)MASK_USER_MASK,(int)MASK_SHADOW,(int)MASK_LAYOVER,(int)MASK_INVALID_DATA,
      version_string(ASF_NAME_STRING));
  exit(EXIT_FAILURE);
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
  int doRadiometric = FALSE;
  int update_original_metadata_with_offsets = FALSE;
  float mask_height_cutoff = 1.0;
  int mask_height_cutoff_specified = FALSE;
  int smooth_dem_holes = FALSE;
  int no_matching = FALSE;
  int grid_matching = FALSE;
  int use_gr_dem = FALSE;
  int add_speckle = TRUE;
  int if_coreg_fails_use_zero_offsets = FALSE;
  int save_ground_dem = FALSE;
  int save_incid_angles = TRUE;
  int use_nearest_neighbor = FALSE;
  double range_offset = 0.0;
  double azimuth_offset = 0.0;
  char *other_files[MAX_OTHER];
  int i,n_other = 0;

  for (i=0; i<MAX_OTHER; ++i)
      other_files[i]=NULL;

  // -1 -> no masking, other values mean fill it with that value
  int fill_value = 0;

  handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);
  inMaskFile = NULL;

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-keep","--keep","-k",NULL)) {
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
    else if (strmatches(key,"-no-match","--no-match",NULL)) {
      no_matching = TRUE;
    }
    else if (strmatches(key,"-grid-match","--grid-match",NULL)) {
      grid_matching = TRUE;
    }
    else if (strmatches(key,"-use-gr-dem", "--use-gr-dem",NULL)) {
      use_gr_dem = TRUE;
    }
    else if (strmatches(key,"-use-sr-dem", "--use-sr-dem",NULL)) {
      use_gr_dem = FALSE;
    }
    else if (strmatches(key,"-no-speckle", "--no-speckle",NULL)) {
      add_speckle = FALSE;
    }
    else if (strmatches(key,"-use-zero-offsets-if-match-fails",
                            "--use-zero-offsets-if-match-fails",NULL)) {
      if_coreg_fails_use_zero_offsets = TRUE;
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
    else if (strmatches(key,"-mask-height-cutoff","--mask-height-cutoff",NULL)) {
        CHECK_ARG(1);
        mask_height_cutoff = atof(GET_ARG(1));
        mask_height_cutoff_specified = TRUE;
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
    else if (strmatches(key,"-offsets","--offsets",NULL)) {
      CHECK_ARG(2);
      range_offset = atof(GET_ARG(2));
      azimuth_offset = atof(GET_ARG(1));
      no_matching = TRUE;
    }
    else if (strmatches(key,"-no-fill","--no-fill",NULL)) {
        // leave masked regions alone - fill with sar data
        fill_value = LEAVE_MASK;
    }
    else if (strmatches(key,"-do-radiometric","--do-radiometric",NULL)) {
        // for the 3.1 release, we will always do formula #5
        // it's the only one we've had time to test.  In later releases,
        // we can switch to the other version ... this does mean
        // that -do-radiometric will change from a flag to an option with
        // an argument.
#ifndef ALLOW_ALL_RADIOMETRIC_TC_FORMULAS
        doRadiometric = 5;
#else
        CHECK_ARG(1);
        char *form = STRDUP(GET_ARG(1));
        doRadiometric = atoi(form);

        // give the do-radiometric help in here for now, later should be
        // put into the rest of the help (when it is officially supported)
        if (strmatches(form, "help", "?", NULL) || doRadiometric==0) {
            asfPrintStatus(
                "Specify a radiometric terrain correction formula to use.\n"
                "Formula numbers are as follows:\n"
                "    1 :    LI\n"
                "    2 :    GO\n"
                "    3 :    SQ\n"
                "    4 :    VX\n"
                "    5 :    1 - .7*pow(cos(li), 7)\n"
                "\n"
                "e.g.,\n"
                "    asf_terrcorr -do-radiometric 1  ...\n\n");
            exit(1);
        }

        free(form);
#endif
    }
    else if (strmatches(key,"-smooth-dem-holes","--smooth-dem-holes",NULL)) {
        smooth_dem_holes = TRUE;
    }
    else if (strmatches(key,"-save-ground-range-dem","--save-ground-range-dem",
			NULL)) {
      save_ground_dem = TRUE;
    }
    else if (strmatches(key,"-use-nearest-neighbor","--use-nearest-neighbor",NULL)) {
      use_nearest_neighbor = TRUE;
    }
    else if (strmatches(key,"-use-bilinear","--use-bilinear",NULL)) {
      use_nearest_neighbor = FALSE;
    }
    else if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else if (strmatches(key,"-other-file","--other-file",NULL)) {
        CHECK_ARG(1);
        if (n_other >= MAX_OTHER)
            asfPrintError("-other-file option only supported %d times.\n", MAX_OTHER);
        other_files[n_other++] = STRDUP(GET_ARG(1));
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

  if (mask_height_cutoff_specified && !generate_water_mask) {
    asfPrintWarning("Ignoring -mask-height-cutoff option, as you did not "
                    "request a water mask.\n");
  }

  inFile = argv[currArg];
  demFile = argv[currArg+1];
  outFile = argv[currArg+2];

  int matching_level = MATCHING_FULL;
  if (no_matching)
    matching_level = MATCHING_NONE;
  else if (grid_matching)
    matching_level = MATCHING_GRID;

  int ret =  asf_terrcorr_ext(inFile, demFile,inMaskFile,outFile, pixel_size,
                              clean_files, do_resample, do_corner_matching,
                              do_interp, do_fftMatch_verification,
                              dem_grid_size, TRUE, fill_value,
                              generate_water_mask, save_clipped_dem,
                              update_original_metadata_with_offsets,
                              mask_height_cutoff, doRadiometric,
                              smooth_dem_holes, other_files,
                              matching_level, range_offset, azimuth_offset,
                              use_gr_dem, add_speckle,
                              if_coreg_fails_use_zero_offsets, save_ground_dem,
                              save_incid_angles, use_nearest_neighbor);

  for (i=0; i<MAX_OTHER; ++i)
      if (other_files[i])
          free(other_files[i]);

  return ret==0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
