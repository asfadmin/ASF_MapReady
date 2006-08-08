#include <stdio.h>
#include <asf.h>
#include <asf_terrcorr.h>

#define ASF_NAME_STRING "refine_geolocation"

void usage(const char *name)
{
  printf("Usage: %s [-log <logfile>] [-quiet] [-update] \n"
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
  int currArg = 1;
  int update_flag = FALSE;
  int NUM_ARGS = 2;

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
    else if (strmatches(key,"-update","--update","-u",NULL)) {
      update_flag = TRUE;
    }
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if (!update_flag) ++NUM_ARGS;
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  inFile = argv[currArg];
  demFile = argv[currArg+1];
  if (update_flag) {
      outFile = appendToBasename(inFile, "_tmp");      
  } else {
      outFile = argv[currArg+2];
  }

  int ret = refine_geolocation(inFile, demFile, outFile, update_flag);

  if (update_flag) {
      char *meta = appendExt(outFile, ".meta");
      remove(meta);
      FREE(meta);
      FREE(outFile);
  }

  return ret==0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
