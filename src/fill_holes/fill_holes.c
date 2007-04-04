#include "asf_sar.h"
#include "asf.h"

#include <stdio.h>
#include <stdlib.h>

#include <asf_license.h>
#include <asf_contact.h>

#include "float_image.h"

#define ASF_NAME_STRING "fill_holes"

#define VERSION 0.2

static void usage()
{
    asfPrintStatus("\n");
    asfPrintStatus("Usage: %s [-log <logfile>] [-quiet] [-cutoff <height>]\n",
                   ASF_NAME_STRING);
    asfPrintStatus("       <infile> <outfile>\n\n");
    asfPrintStatus("   cutoff   Height below which is considered a hole\n");
    asfPrintStatus("   infile   Input file base name\n");
    asfPrintStatus("   outfile  Output file base name\n");
    asfPrintStatus("\n");
    exit(EXIT_FAILURE);
}

static void print_help(void)
{
    usage();
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

int main(int argc,char *argv[])
{
  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  char  infile[256];     /* Input file name               */
  char  outfile[256];    /* Output file name              */
  float cutoff = -900;   /* Height below which is a hole  */

  if (argc <= 2)
    usage();
  else if (strmatches(argv[1],"-help","--help",NULL))
    print_help();

  do {
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
    else if (strmatches(key,"--cutoff","-cutoff","-c",NULL)) {
        CHECK_ARG(1);
        cutoff = atof(GET_ARG(1));
    }
    else if (strmatches(key,"--",NULL)) {
        break;
    }
    else if (key[0] == '-') {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage();
    }
    else {
        // this was a file/dir to process -- back up
        --currArg;
        break;
    }
  } while (currArg < argc);

  if (currArg > argc-2) {
      printf("\n**Not enough arguments.\n");
      usage();
  }

  if (!quietflag)
      asfSplashScreen(argc, argv);

  create_name(infile,argv[currArg],".img");
  create_name(outfile,argv[currArg+1],".img");

  meta_parameters *meta = meta_read(infile);

  asfPrintStatus("Reading DEM: %s\n", infile);
  FloatImage *img = float_image_new_from_metadata(meta, infile);

  asfPrintStatus("Interpolating DEM holes...\n");
  interp_dem_holes_float_image(img, cutoff, TRUE);

  asfPrintStatus("Writing smoothed dem: %s\n", outfile);
  float_image_store(img, outfile, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  float_image_free(img);
  meta_write(meta, outfile);
  meta_free(meta);

  asfPrintStatus("Done.\n");
  if (fLog) fclose(fLog);
  exit(EXIT_SUCCESS);
}

