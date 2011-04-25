/*******************************************************************************
NAME: diffimage

ALGORITHM DESCRIPTION:
  Calculates statistics in each input file
  Calculates PSNR between the two input files
  Performs an fftMatch between the two input files
  Checks for differences in stats, a PSNR that is too low, and for
    shifts in geolocation

ISSUES:
        The images must have at least 75% overlap.
        diffimage will occasionally fail to find the
        correct offset especially with noisy images
        or images with flat topography (low non-noise
        spatial frequency.)

*******************************************************************************/
#include "asf.h"
#include "asf_nan.h"
#include <unistd.h>
#include <math.h>
#include <ctype.h>
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "asf_raster.h"
#include "typlim.h"
#include "float_image.h"
#include "uint8_image.h"
#include "proj.h"
#include "libasf_proj.h"
#include "asf_jpeg.h"
#include "asf_tiff.h"
#include "geo_tiffp.h"
#include "geo_keyp.h"
#include <png.h>
#include <gsl/gsl_math.h>
#include "diffimage_tolerances.h"
#include "geotiff_support.h"
#include "asf_complex.h"

#ifndef png_jmpbuf
#  define png_jmpbuf (png_ptr)    ((png_ptr)->jmpbuf)
#endif

#define MISSING_PSNR -32000
#define FILE1_FFTFILE "tmp_file1.img"
#define FILE1_FFTFILE_META "tmp_file1.meta"
#define FILE2_FFTFILE "tmp_file2.img"
#define FILE2_FFTFILE_META "tmp_file2.meta"
#define CORR_FILE "tmp_corr"

/**** PROTOTYPES ****/
void usage(char *name);

int main(int argc, char **argv)
{
  char *inFile1,*inFile2;
  char *outputFile = NULL;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */
  char ch;
  extern FILE *fLog;            /* output file descriptor, stdout or log file */
  FILE *fError;                 /* Error log, stderr or log file */
  extern int logflag, quietflag;
  int outputflag=0;
  int bandflag=0;
  int strictflag=0;
  int band=0;
  char msg[1024];

  fLog = NULL;
  logflag = quietflag=0;

  /* process command line */
  // FIXME: Might want to add -band1 and -band2 flags that would allow comparing
  // any arbitrary band in file1 to any arbitrary band in file2
  while ((c=getopt(argc,argv,"o:l:b:s:")) != EOF)
  {
    ch = (char)c;
    switch (ch) {
      case 'l':/* -log <filename>, get logfile; this is sorta hacked */
        if (0==strncmp(optarg,"og",2)) {
          sscanf(argv[optind++], "%s", logFile);
          logflag=1;
          fLog = FOPEN(logFile, "w");
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
      case 'o':/* -output <filename>, get output filename ...
		  empty if no differences */
        if (0==strncmp(optarg,"utput",5)) {
	  outputFile= (char*) CALLOC(1024, sizeof(char));
	  sscanf(argv[optind++], "%s", outputFile);
          outputflag=1;
        }
        else {
          strcpy(outputFile, "");
          outputflag=0;
        }
        break;
      case 'b':/* -band flag */
        if (0==strncmp(optarg,"and",3)) {
          sscanf(argv[optind++], "%d", &band);
          bandflag=1;
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
      case 's': /* -strict flag */
        if (0==strncmp(optarg,"trict",5)) {
          strictflag=1;
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
      default:
        FREE(outputFile);
        usage(argv[0]);
        break;
    }
  }
  asfSplashScreen(argc, argv);

  // After parsing out the command line arguments, there should be 2 arguments 
  // left... the two files to compare
  if ((argc-optind) != 2) {
    if ((argc-optind) > 2) {
      printf("\n => Too many inputs.\n");
    }
    if ((argc-optind) < 2) {
      printf("\n => Too few inputs.\n");
    }
    FREE(outputFile);
    usage(argv[0]);
  }
  else {
    // Grab the file names
    inFile1=argv[optind];
    inFile2=argv[optind+1];
  }
  if (strcmp(inFile1, inFile2) == 0) {
    asfPrintError("inFile1 and inFile2 must be different files\n");
  }
  // Set up output redirection for error and log messages
  if (logflag == 0) {
    fLog = NULL;
    fError = NULL;
  }
  else {
    fError = fLog;
  }
  if (!outputflag) {
      sprintf(msg, "Missing output file name ...file differences will be "
	      "directed to stderr (only)\n");
      printf("** Warning: ********\n%s** End of warning **\n\n", msg);
  }
  if (outputflag && strcmp(logFile, outputFile) == 0) {
    sprintf(msg, "Log file cannot be the same as the output file:\n"
            "     Log file: %s\n  Output file: %s\n",
            logFile, outputFile);
    if (outputFile) FREE(outputFile);
    asfPrintError(msg);
  }

  char **bands1 = NULL, **bands2 = NULL;
  int num_bands1, num_bands2, complex = 0;
  stats_t *stats1 = NULL, *stats2 = NULL;
  complex_stats_t *complex_stats1 = NULL, *complex_stats2 = NULL;
  psnr_t *psnrs = NULL;
  complex_psnr_t *complex_psnr = NULL;
  shift_data_t *data_shift = NULL;
  diffimage(inFile1, inFile2, outputFile, logFile, &bands1, &bands2, 
	    &num_bands1, &num_bands2, &complex, &stats1, &stats2, 
	    &complex_stats1, &complex_stats2, &psnrs, &complex_psnr, 
	    &data_shift);

  return (0);
}

void usage(char *name)
{
  printf("\nUSAGE:\n"
      "   %s [-output <diff_output_file>] [-log <file>] [-band <band number>]\n"
      "             [-strict] <img1.ext> <img2.ext>\n"
         "\nOPTIONS:\n"
      "   -output <diff_output_file>:  output to write image differencing\n"
      "                 results to (required.)\n"
      "   -log <file>:  allows the output to be written to a log file\n"
      "                 in addition to stdout (not required but strongly suggested.)\n"
      "   -band <band number>:  allows comparing one selected band from within\n"
      "                 multi-banded image files, including the separate color bands\n"
      "                 found within color images.  Default is to compare all\n"
      "                 available bands.\n"
      "   -strict:      forces statistics comparison to utilize all available\n"
      "                 fields (min, max, mean, sdev, PSNR).  Without the -strict\n"
      "                 option specified, only mean, sdev, and PSNR are utilized.\n"
      "                 default is non-strict.\n"
      "\nINPUTS:\n"
      "   <img1.ext>:   any supported single-banded graphics file image.\n"
      "                 supported types include TIFF, GEOTIFF, PNG, PGM, PPM,\n"
      "                 JPEG, and ASF IMG.  File extension is REQUIRED.\n"
      "   <img2.ext>:   an image to line up with the first.\n"
      "                 image 2 should be the same graphics file type and not\n"
      "                 be bigger than image 1.  File extension is REQUIRED.\n"
      "\nDESCRIPTION:\n"
      "   1. diffimage calculates image statistics within each input image\n"
      "      and calculates the peak signal-to-noise (PSNR) between the two\n"
      "      images.\n"
      "   2. diffimage then lines up the two images, to slightly better\n"
      "      than single-pixel precision, and determines if any geolocation\n"
      "      shift has occurred between the two and the size of the shift.\n"
      "      Because an fft-based match is utilized it will work with images of\n"
      "      any size, but is most efficient when the image dimensions are\n"
      "      near a power of 2.  The images need not be square.\n"
      "   3. diffimage then compares image statistics and geolocation shift\n"
      "      (if it occurred) and determines if the two images are different from\n"
      "      each other or not.\n"
      "   4. If there are no differences, the output file will exist but will be\n"
      "      of zero length.  Otherwise, a summary of the differences will be placed\n"
      "      in both the output file and the log file (if specified.)\n\n"
          "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " "MAPREADY_VERSION_STRING ")\n\n",
      name);
  exit(1);
}
