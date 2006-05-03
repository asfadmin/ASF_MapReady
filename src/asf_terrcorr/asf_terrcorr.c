#include <stdio.h>
#include <asf.h>
#include <asf_reporting.h>
#include <asf_terrcorr.h>

#define NUM_ARGS 3
void usage(const char *name)
{
  printf("Usage: %s [-log <logfile>] [-quiet] [-keep] [-no-resample]\n"
         "          [-no-verify-fftMatch] [-pixel-size <size>]\n"
         "          [-dem-grid-size <size>]\n"
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
  char *inFile, *demFile, *resampleFile, *srFile, *outFile;
  char *demGridFile, *demPolyFile, *demClipped, *demSlant, *demSimAmp;
  char *demTrimSimAmp, *corrFile, *corrFile2, *demTrimSlant;
  double demRes, sarRes;
  int demWidth, demHeight;
  meta_parameters *metaSAR, *metaDEM;
  double dx, dy, azScale, pixel_size = -1;
  int dem_grid_size = 20;
  int currArg, idx, idy;
  int polyOrder = 5, clean_files = TRUE, do_resample = TRUE,
    do_fftMatch_verification = TRUE;

  currArg = 1;

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
    else if (strmatches(key,"-pixel-size","--pixel-size","-ps",NULL)) {
      CHECK_ARG(1);
      pixel_size = atof(GET_ARG(1));
    }
    else if (strmatches(key,"-dem-grid-size","--dem-grid-size",NULL)) {
      CHECK_ARG(1);
      dem_grid_size = atoi(GET_ARG(1));
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
			     do_resample, do_fftMatch_verification,
			     dem_grid_size);
  return ret ? EXIT_SUCCESS : EXIT_FAILURE;
}
