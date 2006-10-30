#include <stdio.h>
#include <asf.h>
#include <asf_terrcorr.h>
#include <asf_license.h>

#define ASF_NAME_STRING "refine_geolocation"

static char *generate_fake_spaces(const char *s)
{
    static char sp[255];
    int i;
    for (i = 0; i < strlen(s); ++i) sp[i]=' ';
    sp[i] = '\0';
    return sp;
}

void usage(const char *name)
{
  printf("Usage: %s [-log <logfile>] [-quiet] [-update] [-mask-file <file>]\n"
         "       %s [-auto-water-mask] [-mask-height-cutoff <height in meters>]\n"
         "       %s <inFile> <demFile> <outFile>\n", name,
         generate_fake_spaces(name), generate_fake_spaces(name));
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
  char *inFile, *demFile, *maskFile=NULL, *outFile;
  int currArg = 1;
  int update_flag = FALSE;
  int NUM_ARGS = 2;
  int auto_water_mask = FALSE;
  float mask_height_cutoff = 1.0;

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
    else if (strmatches(key,"-mask-file","--mask-file",NULL)) {
        CHECK_ARG(1);
        maskFile = GET_ARG(1);
    }
    else if (strmatches(key,"-mask-height-cutoff","--mask-height-cutoff",NULL)) {
        CHECK_ARG(1);
        mask_height_cutoff = atof(GET_ARG(1));
    }
    else if (strmatches(key,"-auto-water-mask","--auto-water-mask",NULL)) {
        auto_water_mask = TRUE;
    }
    else {
        --currArg;
        break;
    }
  }
  if (!update_flag) ++NUM_ARGS;
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  inFile = argv[currArg];
  demFile = argv[currArg+1];
  if (update_flag) {
      outFile = appendToBasename(inFile, "_tmp");      
  } else {
      outFile = argv[currArg+2];
  }

  int ret = refine_geolocation(inFile, demFile, maskFile, outFile, 
                               update_flag, auto_water_mask,
                               mask_height_cutoff);

  if (update_flag) {
      char *meta = appendExt(outFile, ".meta");
      remove(meta);
      FREE(meta);
      FREE(outFile);
  }

  return ret==0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
