#include <stdio.h>
#include <asf.h>
#include <asf_terrcorr.h>

#define ASF_NAME_STRING "refine_geolocation"

#define NUM_ARGS 3
void usage(const char *name)
{
  printf("Usage: %s [-log <logfile>] [-quiet] \n"
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
  char *inFile, *demFile, *outFile;
  double pixel_size = -1;
  int dem_grid_size = 20;
  int currArg = 1;
  int clean_files = TRUE;
  int do_resample = TRUE;
  int do_interp = FALSE;
  int do_fftMatch_verification = TRUE;
  int do_corner_matching = FALSE;
  int do_terrain_correction = FALSE;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

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

  int ret = asf_terrcorr_ext(inFile, demFile, outFile, pixel_size, clean_files,
			     do_resample, do_corner_matching, do_interp,
			     do_fftMatch_verification, dem_grid_size,
                             do_terrain_correction);

  return ret ? EXIT_SUCCESS : EXIT_FAILURE;
}
