#include <stdio.h>
#include <asf.h>
#include <asf_sar.h>
#include <asf_terrcorr.h>
#include <asf_license.h>

#define ASF_NAME_STRING "asf_terrcorr"

#define NUM_ARGS 3
void usage(const char *name)
{
  printf("Usage: %s [-log <logfile>] [-quiet] [-keep (-k)] [-no-resample]\n"
     "          [-no-verify-fftMatch] [-no-corner-match] [-no-interp]\n"
     "          [-pixel-size <size>] [-dem-grid-size <size>]\n"
     "          [-mask-file <filename> | -auto-water-mask]\n"
     "          [-fill <fill value> | -no-fill] [-update-original-meta (-u)]\n"
     "          <inFile> <demFile> <outFile>\n", name);
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
  int do_corner_matching = TRUE;
  int generate_water_mask = FALSE;
  int save_clipped_dem = FALSE;
  int update_original_metadata_with_offsets = FALSE;

  // -1 -> no masking, other values mean fill it with that value
  int fill_value = 0; 

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);
  inMaskFile = NULL;
  
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
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
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
