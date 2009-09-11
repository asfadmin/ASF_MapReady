#include "asf_sar.h"
#include "asf.h"

#include <stdio.h>
#include <stdlib.h>

#include <asf_license.h>
#include <asf_contact.h>

#include "float_image.h"

#include "fill_holes_help.h"

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
    if (argc > 1) {
        check_for_help(argc, argv);
        handle_license_and_version_args(argc, argv, TOOL_NAME);
    }
    if (argc < 3) {
        asfPrintStatus("**Not enough arguments\n");
        usage();
        return 1;
    }

  char  infile[256];          // Input file name                         
  char  outfile[256];         // Output file name                        
  float cutoff = -900;        // Height below which is a hole            
  float max_slope = 60;       // Maximum slope allowed (from horizontal) 
  int max_hole_width = 1000;  // Maximum width of a hole

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
    else if (strmatches(key,"--max-slope","-max-slope",NULL)) {
        CHECK_ARG(1);
        max_slope = atof(GET_ARG(1));
    }
    else if (strmatches(key,"--max-hole-width","-max-hole-width",NULL)) {
        CHECK_ARG(1);
        max_hole_width = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"--",NULL)) {
        break;
    }
    else if (key[0] == '-') {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage();
      return 1;
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
      return 1;
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
                               //max_hole_width, max_slope);

  meta_write(meta, outfile);
  asfPrintStatus("Writing smoothed dem: %s\n", outfile);
  float_image_store(img, outfile, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  float_image_free(img);
  meta_free(meta);

  asfPrintStatus("Done.\n");
  if (fLog) fclose(fLog);
  exit(EXIT_SUCCESS);
}

