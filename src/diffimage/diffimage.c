/*******************************************************************************
NAME: diffimage

SYNOPSIS: diffimage [-log <file>] [-quiet]
                        <img1.ext> <img2.ext>

DESCRIPTION:

        diffimage lines up two images, to slightly better the
        single-pixel precision.  It will work with images of any
        size, but is most efficient when the image dimensions are
        near a power of 2.  The images need not be square.

        The lining-up is performed using image correlation via
        the much-vaunted Fast Fourier Transform (FFT).  The working
        space size is rounded up to the nearest power of 2 (for the FFT).
        The first image is read in completely, and a chip of the second
        image is also read in.  The average brightness of the chip is
        calculated and subtracted from both images.  The images are then
        FFT'd to frequency space, the FFT'd chip is conjugated, and the
        two images are multiplied together.  The product is inverse-FFT'd,
        and the resulting correlation image shows a brightness peak
        where the two images line up best.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0            6/07   B. Dixon    As released

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
        The images must have at least 75% overlap.
        diffimage will occasionally fail to find the
        correct offset (especially with noisy images).

*******************************************************************************/
#include "asf.h"
#include <math.h>
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "asf_raster.h"
#include "typlim.h"
#include <float_image.h>
#include <proj.h>
#include <libasf_proj.h>
#include <geokeys.h>
#include <geo_tiffp.h>
#include <geo_keyp.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include "diffimage_tolerances.h"

#define VERSION 1.0

/**** TYPES ****/
typedef enum {
  UNKNOWN_GRAPHICS_TYPE=0,
  ASF_IMG,
  JPEG,
  PGM,
  PPM,
  STD_TIFF,
  GEO_TIFF,
  BMP,
  GIF,
  PNG
} graphics_file_t;

typedef struct {
  int stats_good;
  double min;
  double max;
  double mean;
  double sdev;
  double rmse;
  gsl_histogram *hist;
  gsl_histogram_pdf *hist_pdf;
} stats_t;

/**** PROTOTYPES ****/
void usage(char *name);
void msg_out(FILE *fpLog, int quiet, char *msg);
void err_out(FILE *fpError, int quiet, char *msg);
int asf_img_file_found(char *file);
graphics_file_t getGraphicsFileType (char *file);
void graphicsFileType_toStr (graphics_file_t type, char *type_str);
void fftDiff(char *inFile1, char *inFile2, float *bestLocX, float *bestLocY, float *certainty);
float get_maxval(meta_parameters *meta);
void calc_asf_img_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               double *psnr, int band);
void calc_jpeg_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats, double *psnr);
void calc_pgm_ppm_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats, double *psnr);
void calc_tiff_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats, double *psnr);
void print_stats_results(char *filename1, char *filename2,
                         stats_t *s1, stats_t *s2,
                         double psnr);
void diff_check(char *outputFile, char *inFile1, char *inFile2,
                stats_t *stats1, stats_t *stats2, double *psnr,
                int strict, int num_bands);
void diffErrOut(char *outputFile, char *err_msg);

int main(int argc, char **argv)
{
  char *inFile1,*inFile2;
  char *outputFile;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */
  extern FILE *fLog;            /* output file descriptor, stdout or log file */
  FILE *fError;                 /* Error log, stderr or log file */
  extern int logflag, quietflag;
  int outputflag=0;
  int bandflag=0;
  int strictflag=0;
  int band=0;
  char msg[1024];
  char type_str[255];
  stats_t inFile1_stats[MAX_BANDS];
  stats_t inFile2_stats[MAX_BANDS];
  double psnr[MAX_BANDS]; // peak signal to noise ratio
  //float bestLocX, bestLocY, certainty;

  fLog=NULL;
  logflag=quietflag=0;
  outputFile=(char*)CALLOC(1024, sizeof(char));

  /* process command line */
  asfSplashScreen(argc, argv);
  while ((c=getopt(argc,argv,"o:l:b:s:")) != EOF)
  {
    switch (c) {
      case 'l':/* -log <filename>, get logfile; this is sorta hacked */
        if (0==strncmp(optarg,"og",2)) {
          sscanf(argv[optind++], "%s", logFile);
          logflag=1;
          fLog = FOPEN(logFile, "w");
        }
        else {
          usage(argv[0]);
        }
        break;
      case 'o':/* -output <filename>, get output filename ...empty if no differences */
        if (0==strncmp(optarg,"utput",5)) {
          sscanf(argv[optind++], "%s", outputFile);
          outputflag=1;
        }
        else {
          usage(argv[0]);
        }
        break;
      case 'b':/* -band flag */
        if (0==strncmp(optarg,"and",3)) {
          sscanf(argv[optind++], "%d", &band);
          bandflag=1;
        }
        else {
          usage(argv[0]);
        }
        break;
      case 's': /* -strict flag */
        if (0==strncmp(optarg,"trict",5)) {
          strictflag=1;
        }
        else {
          usage(argv[0]);
        }
        break;
      default:
        if (fLog) {
          FCLOSE(fLog);
        }
        usage(argv[0]);
        break;
    }
  }

  // After parsing out the command line arguments, there should be 2 arguments left...
  // the two files to compare
  if ((argc-optind) != 2) {
    if ((argc-optind) > 2) {
      printf("\n => Too many inputs.\n");
    }
    if ((argc-optind) < 2) {
      printf("\n => Too few inputs.\n");
    }
    if (fLog) {
      FCLOSE(fLog);
    }
    usage(argv[0]);
  }
  else {
    // Grab the file names
    inFile1=argv[optind];
    inFile2=argv[optind+1];
  }
  if (!outputflag) {
    sprintf(msg, "Missing output file name ...no place to store file differences!\n");
    asfPrintError(msg);
  }

  // Set up output redirection for error and log messages
  if (logflag == 0) {
    fLog = NULL;
    fError = NULL;
  }
  else {
    // fLog should already point to an open log file at this point
    fError = fLog;
  }

  // Determine input file graphical format types and check to see if they are
  // supported and the same type (etc ...error checking)
  if (strcmp(inFile1, inFile2) == 0) {
    return (0); // PASS - a file compared to itself is always the same
  }
  if (!fileExists(inFile1)) {
    sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?", inFile1);
    err_out(fError, quietflag, msg);
    return (1);
  }
  if (!fileExists(inFile2)) {
    sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?", inFile2);
    err_out(fError, quietflag, msg);
    return (1);
  }
  graphics_file_t type1, type2;
  type1 = getGraphicsFileType(inFile1);
  type2 = getGraphicsFileType(inFile2);
  if (type1 != ASF_IMG  &&
      type1 != JPEG     &&
      type1 != PGM      &&
      type1 != PPM      &&
      type1 != STD_TIFF &&
      type1 != GEO_TIFF )
  {
    graphicsFileType_toStr(type1, type_str);
    sprintf(msg, "Graphics file type %s is not currently supported (Image #1: %s)\n",
            type_str, inFile1);
    err_out(fError, quietflag, msg);
    return (1);
  }
  if (type2 != ASF_IMG  &&
      type2 != JPEG     &&
      type2 != PGM      &&
      type2 != PPM      &&
      type2 != STD_TIFF &&
      type2 != GEO_TIFF )
  {
    graphicsFileType_toStr(type2, type_str);
    sprintf(msg, "Graphics file type %s is not currently supported (Image #2: %s)\n",
            type_str, inFile2);
    err_out(fError, quietflag, msg);
    return (1);
  }
  if (type1 != type2) {
    char type1_str[255], type2_str[255];
    graphicsFileType_toStr(type1, type1_str);
    graphicsFileType_toStr(type2, type2_str);
    sprintf(msg, "Graphics files must be the same type.\n"
          "       %s is type %s, and %s is type %s\n",
          inFile1, type1_str, inFile2, type2_str);
    err_out(fError, quietflag, msg);
    return (1);
  }

  /***** Calculate stats and PSNR *****/
  //   Can't be here unless both graphics file types were the same,
  //   so choose the input process based on just one of them.
  switch (type1) {
    case ASF_IMG:
      {
        int band_count1, band_count2;
        char inFile1_meta[1024], inFile2_meta[1024];
        char *f1, *f2, *c;
        meta_parameters *md1 = NULL;
        meta_parameters *md2 = NULL;

        // Read metadata and check for multi-bandedness
        if (inFile1 != NULL && strlen(inFile1) > 0) {
          f1 = STRDUP(inFile1);
          c = findExt(f1);
          *c = '\0';
          sprintf(inFile1_meta, "%s.meta", f1);
          if (fileExists(inFile1_meta)) {
            md1 = meta_read(inFile1_meta);
          }
          else {
            // Can't find metadata file
            sprintf(msg, "Cannot find metadata file %s\n", inFile1_meta);
            diffErrOut(outputFile, msg);
            asfPrintError(msg);
          }
          if (md1 == NULL) {
            // Can't find metadata file
            sprintf(msg, "Cannot find metadata file %s\n", inFile1_meta);
            diffErrOut(outputFile, msg);
            asfPrintError(msg);
          }
        }
        if (inFile2 != NULL && strlen(inFile2) > 0) {
          f2 = STRDUP(inFile2);
          c = findExt(f2);
          *c = '\0';
          sprintf(inFile2_meta, "%s.meta", f2);
          if (fileExists(inFile2_meta)) {
            md2 = meta_read(inFile2_meta);
          }
          else {
            // Can't find metadata file
            sprintf(msg, "Cannot find metadata file %s\n", inFile2_meta);
            diffErrOut(outputFile, msg);
            asfPrintError(msg);
          }
          if (md2 == NULL) {
            // Can't find metadata file
            sprintf(msg, "Cannot find metadata file %s\n", inFile2_meta);
            diffErrOut(outputFile, msg);
            asfPrintError(msg);
          }
        }
        band_count1 = md1->general->band_count;
        band_count2 = md2->general->band_count;
        if (bandflag && !(band < band_count1 && band < band_count2 && band >= 0)) {
          sprintf(msg, "Invalid band number.  Band number must be 0 (first band)\n"
              "or greater, and less than the number of available bands in the file\n"
              "Example:  If the files have 3 bands, then band numbers 0, 1, or 2 are\n"
              "the valid band number choices.  \n"
              "\nFile1 has %d bands.  File2 has %d bands\n",
              band_count1, band_count2);
          diffErrOut(outputFile, msg);
          asfPrintError(msg);
        }
        if (!bandflag && band_count1 != band_count2) {
          sprintf(msg, "Files do not have the same number of bands.\n"
              "Cannot compare all bands.  Consider using the -band option to\n"
              "compare individual bands within the files.\n"
              "\nFile1 has %d bands.  File2 has %d bands\n",
              band_count1, band_count2);
          diffErrOut(outputFile, msg);
          asfPrintError(msg);
        }

        //////////////////////////////////////////////////////////////////////////////////////
        // Calculate statistics, PSNR, measure image-to-image shift in geolocation, and then
        // check the results
        if (!bandflag) {
          // Process every available band
          int band_no;
          for (band_no=0; band_no < band_count1; band_no++) {
            calc_asf_img_stats_2files(inFile1, inFile2,
                                      &inFile1_stats[band_no], &inFile2_stats[band_no],
                                      &psnr[band_no], band_no);
            // fftMatch(shifts_t *shift, band_no) goes here
          }
          // FIXME: Add *shift to diff_check
          diff_check(outputFile,
                     inFile1, inFile2, inFile1_stats, inFile2_stats, psnr, strictflag,
                     band_count1); // Assumes both files have the same band count or would not be here
        }
        else {
          // Process selected band
          calc_asf_img_stats_2files(inFile1, inFile2,
                                    inFile1_stats, inFile2_stats,
                                    psnr, band);
          // fftMatch(shifts_t *shift, band_no) goes here
          // FIXME: Add *shift to diff_check
          diff_check(outputFile,
                     inFile1, inFile2, &inFile1_stats[band], &inFile2_stats[band], &psnr[band],
                     strictflag, 1);
        }
        //
        //////////////////////////////////////////////////////////////////////////////////////
      }
      break;
    case JPEG:
      {
        ;
      }
      break;
    case PGM:
    case PPM:
    {
      ;
    }
    break;
    case STD_TIFF:
    case GEO_TIFF:
    {
      ;
    }
    break;
    default:
      sprintf(msg, "Unrecognized image file type found.\n");
      diffErrOut(outputFile, msg);
      asfPrintError(msg);
      break;
  }

  // Find geolocation shifts between the images
  //fftDiff(inFile1, inFile2, &bestLocX, &bestLocY, &certainty);

  if (fLog != NULL) {
    fclose(fLog);
  }

  return (0);
}

void usage(char *name)
{
  printf("\nUSAGE:\n"
         "   %s <-output <diff_output_file>> [-log <file>] [-band <band number>] [-strict] <img1.ext> <img2.ext>\n"
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
      "                 supported types include TIFF, GEOTIFF, PGM, PPM,\n"
      "                 JPEG, and ASF IMG.  File extension is REQUIRED.\n"
      "   <img2.ext>:   an image to line up with the first.\n"
      "                 image 2 should be the same graphics file type and not\n"
      "                 be bigger than image 1.  File extension is REQUIRED.\n"
      "\nDESCRIPTION:\n"
      "   1. diffimage calculates image statistics within each input image\n"
      "   and calculates the peak signal-to-noise (PSNR) between the two\n"
      "   images.\n"
      "   2. diffimage then lines up the two images, to slightly better\n"
      "   than single-pixel precision, and determines if any geolocation\n"
      "   shift has occurred between the two and the size of the shift.\n"
      "   Because an fft-based match is utilized it will work with images of\n"
      "   any size, but is most efficient when the image dimensions are\n"
      "   near a power of 2.  The images need not be square.\n"
      "   3. diffimage then compares image statistics and geolocation shift\n"
      "   (if it occurred) and determines if the two images are different from\n"
      "   each other or not.\n"
      "   4. If there are no differences, the output file will exist but will be\n"
      "   of zero length.  Otherwise, a summary of the differences will be placed\n"
      "   in both the output file and the log file (if specified.)\n"
      "\nVersion %.2f, Alaska Satellite Facility Tools\n\n",name,VERSION);
  exit(1);
}

void msg_out(FILE *fpLog, int quiet, char *msg)
{
  if (fpLog != NULL) {
    fprintf(fpLog, msg);
  }
  if (!quiet) {
    printf(msg);
  }
}

void err_out(FILE *fpError, int quiet, char *msg)
{
  char err_msg[1024];
  sprintf(err_msg, "\nERROR: %s\n\n", msg);
  if (fpError != NULL) {
    fprintf(fpError, err_msg);
  }
  fprintf(stderr, err_msg);
}

graphics_file_t getGraphicsFileType (char *file)
{
  FILE *fp = (FILE *)fopen(file, "r");
  uint8 magic[4];
  graphics_file_t file_type = UNKNOWN_GRAPHICS_TYPE;
  TIFF *itif;
  GTIF *gtif;

  if (fp != NULL) {
    int i;

    // Read the magic number from the file header
    for (i=0; i<4; i++) {
      magic[i] = (uint8)fgetc(fp);
    }
    fclose(fp);

    // Check for a valid magic number combination
    if (magic[0] == 0xFF && magic[1] == 0xD8) {
      file_type = JPEG;
    }
    else if (magic[0] == 'P' && (magic[1] == '5' || magic[1] == '2')) {
      file_type = PGM;
    }
    else if (magic[0] == 'P' && (magic[1] == '6' || magic[1] == '3')) {
      file_type = PPM;
    }
    else if ((magic[0] == 'I' && magic[1] == 'I') || (magic[0] == 'M' && magic[1] == 'M')) {
      file_type = STD_TIFF;

      itif = XTIFFOpen(file, "r");
      if (itif != NULL) {
        gtif = GTIFNew(itif);
        if (gtif != NULL) {
          double *tie_point, *pixel_scale;
          short model_type, raster_type, linear_units;
          int read_count, tie_points, pixel_scales;

          (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS, &tie_points, &tie_point);
          (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE, &pixel_scales, &pixel_scale);
          read_count = GTIFKeyGet(gtif, GTModelTypeGeoKey, &model_type, 0, 1);
          read_count += GTIFKeyGet(gtif, GTRasterTypeGeoKey, &raster_type, 0, 0);
          read_count += GTIFKeyGet(gtif, ProjLinearUnitsGeoKey, &linear_units, 0, 1);

          if (tie_points == 6 && pixel_scales == 3 && read_count == 3) {
            file_type = GEO_TIFF;
          }

          if (tie_point != NULL) free(tie_point);
          if (pixel_scale != NULL) free(pixel_scale);
          GTIFFree(gtif);
        }
        XTIFFClose(itif);
      }
    }
    else if (magic[0] == 'B' && magic[1] == 'M') {
      file_type = BMP;
    }
    else if (magic[0] == 'G' && magic[1] == 'I' && magic[2] == 'F') {
      file_type = GIF;
    }
    else if (magic[1] == 'P' && magic[2] == 'N' && magic[3] == 'G') {
      file_type = PNG;
    }
    else if (asf_img_file_found(file)) {
      file_type = ASF_IMG;
    }
    else {
      file_type = UNKNOWN_GRAPHICS_TYPE;
    }
  }

  return file_type;
}

int asf_img_file_found(char *file)
{
  int found = 0;

  if (fileExists(file) &&
      strncmp(uc(findExt(file)), ".IMG", 4) == 0)
  {
    found = 1;
  }
  else {
    found = 0;
  }

  return found;
}

void fftDiff(char *inFile1, char *inFile2, float *bestLocX, float *bestLocY, float *certainty)
{
  ;
}

void graphicsFileType_toStr (graphics_file_t type, char *type_str)
{
  switch (type) {
    case ASF_IMG:
      strcpy (type_str, "ASF_IMG");
      break;
    case JPEG:
      strcpy (type_str, "JPEG");
      break;
    case PGM:
      strcpy (type_str, "PGM");
      break;
    case PPM:
      strcpy (type_str, "PPM");
      break;
    case STD_TIFF:
      strcpy (type_str, "TIFF");
      break;
    case GEO_TIFF:
      strcpy (type_str, "GEOTIFF");
      break;
    case BMP:
      strcpy (type_str, "BMP");
      break;
    case GIF:
      strcpy (type_str, "GIF");
      break;
    case PNG:
      strcpy (type_str, "PNG");
      break;
    case UNKNOWN_GRAPHICS_TYPE:
    default:
      strcpy(type_str, "UNRECOGNIZED");
      break;
  }
}

void calc_asf_img_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               double *psnr, int band)
{
  char *f1;
  char *f2;
  char inFile1_meta[255], inFile2_meta[255];
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  char *c;
  int band_count1, band_count2;
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;
  meta_parameters *md1 = NULL;
  meta_parameters *md2 = NULL;

  // Init stats
  s1->stats_good = s2->stats_good = 1; // Presumed innocent until proven guilty
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
  *psnr = 0.0;

  // Read metadata and check for multi-bandedness
  if (inFile1 != NULL && strlen(inFile1) > 0) {
    f1 = STRDUP(inFile1);
    c = findExt(f1);
    *c = '\0';
    sprintf(inFile1_meta, "%s.meta", f1);
    if (fileExists(inFile1_meta)) {
      md1 = meta_read(inFile1_meta);
      s1->stats_good = (md1 != NULL) ? s1->stats_good : 0;
    }
  }
  else {
    s1->stats_good = 0;
  }
  if (inFile2 != NULL && strlen(inFile2) > 0) {
    f2 = STRDUP(inFile2);
    c = findExt(f2);
    *c = '\0';
    sprintf(inFile2_meta, "%s.meta", f2);
    if (fileExists(inFile2_meta)) {
      md2 = meta_read(inFile2_meta);
      s2->stats_good = (md2 != NULL) ? s2->stats_good : 0;
    }
  }
  else {
    s2->stats_good = 0;
  }
  band_count1 = (md1 != NULL) ? md1->general->band_count : 0;
  band_count2 = (md2 != NULL) ? md2->general->band_count : 0;

  // Calculate the stats from the data.
  s1->hist = NULL;
  s1->hist_pdf = NULL;
  s2->hist = NULL;
  s2->hist_pdf = NULL;
  if (band_count1 > 0 && md1 != NULL) {
    band_names1 = extract_band_names(md1->general->bands, md1->general->band_count);
    if (band_names1 != NULL) {
      asfPrintStatus("\nCalculating statistics for %s band in %s", band_names1[band], inFile1);
      calc_stats_rmse_from_file(inFile1, band_names1[band],
                                md1->general->no_data,
                                &s1->min, &s1->max, &s1->mean,
                                &s1->sdev, &s1->rmse, &s1->hist);
    }
  }
  if (band_count2 > 0 && md2 != NULL) {
    band_names2 = extract_band_names(md2->general->bands, md2->general->band_count);
    if (band_names2 != NULL) {
      asfPrintStatus("\nCalculating statistics for %s band in %s", band_names2[band], inFile2);
      calc_stats_rmse_from_file(inFile2, band_names2[band],
                                md2->general->no_data,
                                &s2->min, &s2->max, &s2->mean,
                                &s2->sdev, &s2->rmse, &s2->hist);
    }
  }

  // Calculate the peak signal to noise ratio (PSNR) between the two images
  double se;
  double rmse;
  int ii, jj;
  int band1 = band;
  int band2 = band;
  long pixel_count = 0;
  long lines, samples;
  long offset1 = md1->general->line_count * band1;
  long offset2 = md2->general->line_count * band2;
  float max_val;
  float *data1;
  float *data2;
  if (md1 != NULL && md2 != NULL) {
    data1 = (float*)MALLOC(sizeof(float) * md1->general->sample_count);
    data2 = (float*)MALLOC(sizeof(float) * md2->general->sample_count);
    if (data1 == NULL || data2 == NULL ||
        (md1->general->data_type != md2->general->data_type)) {
      *psnr = -1; // Since PSNR is always zero or positive, this value means 'invalid PSNR'
    }
    else {
      asfPrintStatus("\nCalculating PSNR...\n");
      FILE *fp1 = fopen(inFile1, "rb");
      FILE *fp2 = fopen(inFile2, "rb");
      if (fp1 != NULL && fp2 != NULL) {
        se = 0.0;
        max_val = get_maxval(md1); // Since both file's data types are the same, this is OK
        lines = MIN(md1->general->line_count, md2->general->line_count);
        if (lines < md1->general->line_count) {
          asfPrintWarning("File2 has fewer lines than File1 (%d v. %d).\n"
              "Only the first %d lines will be utilized for PSNR calculation\n",
              md2->general->line_count, md1->general->line_count, lines);
        }
        if (lines < md2->general->line_count) {
          asfPrintWarning("File1 has fewer lines than File2 (%d v. %d).\n"
              "Only the first %d lines will be utilized for PSNR calculation\n",
          md1->general->line_count, md2->general->line_count, lines);
        }
        samples = MIN(md1->general->sample_count, md2->general->sample_count);
        if (samples < md1->general->sample_count) {
          asfPrintWarning("File2 has fewer samples per line than File1 (%d v. %d).\n"
              "Only the first %d samples within each line of data will be utilized for PSNR calculation\n",
          md2->general->sample_count, md1->general->sample_count, samples);
        }
        if (samples < md2->general->sample_count) {
          asfPrintWarning("File1 has fewer samples per line than File2 (%d v. %d).\n"
              "Only the first %d samples within each line of data will be utilized for PSNR calculation\n",
          md1->general->sample_count, md2->general->sample_count, samples);
        }
        for (ii=0; ii<lines; ++ii) {
          asfPercentMeter((double)ii/(double)lines);
          get_float_line(fp1, md1, ii + offset1, data1);
          get_float_line(fp2, md2, ii + offset2, data2);
          for (jj=0; jj<samples; ++jj) {
            se += (data1[jj] - data2[jj]) * (data1[jj] - data2[jj]);
            pixel_count++;
          }
        }
        asfPercentMeter(1.0);
        FCLOSE(fp1);
        FCLOSE(fp2);
        if (pixel_count > 0 && max_val > 0) {
          rmse = sqrt(se/pixel_count);
          *psnr = (rmse > 0) ? 10.0 * log10(max_val/rmse) : 0;
        }
        else {
          *psnr = -1;
        }
      }
      else {
        *psnr = -1;
      }
    }
  }
  else {
    *psnr = -1;
  }

  // Cleanup and begone
  if (f1 != NULL) FREE(f1);
  if (f2 != NULL) FREE(f2);
  if (band_names1 != NULL && md1 != NULL) {
    for (ii=0; ii<md1->general->band_count; ii++) {
      if (band_names1[ii] != NULL) FREE(band_names1[ii]);
    }
    FREE(band_names1);
  }
  if (band_names2 != NULL && md2 != NULL) {
    for (ii=0; ii<md2->general->band_count; ii++) {
      if (band_names2[ii] != NULL) FREE(band_names2[ii]);
    }
    FREE(band_names2);
  }
  if (data1 != NULL) FREE(data1);
  if (data2 != NULL) FREE(data2);
  if (md1 != NULL) meta_free(md1);
  if (md2 != NULL) meta_free(md2);
}

void calc_jpeg_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats, double *psnr)
{
//  char *f1 = inFile1;
//  char *f2 = inFile2;
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
}

void calc_pgm_ppm_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats, double *psnr)
{
//  char *f1 = inFile1;
//  char *f2 = inFile2;
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
}

void calc_tiff_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats, double *psnr)
{
//  char *f1 = inFile1;
//  char *f2 = inFile2;
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
}

float get_maxval(meta_parameters *meta)
{
  float ret;

  // Only non-complex types with 32 bits or less are supported
  switch (meta->general->data_type) {
    case BYTE:
      ret = powf(2, sizeof(unsigned char)) - 1.0;
      break;
    case INTEGER16:
      ret = powf(2, sizeof(short int)) - 1.0;
      break;
    case INTEGER32:
      ret = powf(2, sizeof(int)) - 1.0;
      break;
    case REAL32:
      ret = MAXREAL;
      break;
    default:
      ret = 0.0;
      break;
  }

  return ret;
}

void print_stats_results(char *filename1, char *filename2,
                         stats_t *s1, stats_t *s2,
                         double psnr)
{
  asfPrintStatus(
      "\nStatistics for File1: %s\n"
      "   min: %f\n"
      "   max: %f\n"
      "  mean: %f\n"
      "  sdev: %f\n"
      "  rmse: %f\n"
      "result: %s\n",
      filename1, s1->min, s1->max, s1->mean, s1->sdev, s1->rmse,
      s1->stats_good ? "GOOD STATS" : "UNRELIABLE STATS");
  asfPrintStatus(
      "\nStatistics for File2: %s\n"
      "   min: %f\n"
      "   max: %f\n"
      "  mean: %f\n"
      "  sdev: %f\n"
      "  rmse: %f\n"
      "result: %s\n",
      filename2, s2->min, s2->max, s2->mean, s2->sdev, s2->rmse,
      s2->stats_good ? "GOOD STATS" : "UNRELIABLE STATS");
  asfPrintStatus("\nPSNR between files: %f\n\n", psnr);
}

void diff_check(char *outputFile, char *inFile1, char *inFile2,
                stats_t *stats1, stats_t *stats2, double *psnr,
                int strict, int num_bands)
{
  int band;
  double baseline_range;
  double min_tol;
  double max_tol;
  double mean_tol;
  double sdev_tol;
  //  double rmse_tol;
  double psnr_tol;
  double min_diff;
  double max_diff;
  double mean_diff;
  double sdev_diff;
  //  double rmse_diff;
  FILE *outputFP = NULL;

  outputFP = fopen(outputFile, "w");

  // Get or produce band names for intelligent output...
  char type_str[255];
  graphics_file_t type = getGraphicsFileType(inFile1);
  graphicsFileType_toStr(type, type_str);
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  if (type == ASF_IMG) {
    // Grab band names from the metadata
    meta_parameters *md1, *md2;
    int band_count1=1, band_count2=1;
    char *f, *c;
    char inFile1_meta[1024], inFile2_meta[1024];
    if (inFile1 != NULL && strlen(inFile1) > 0) {
      f = STRDUP(inFile1);
      c = findExt(f);
      *c = '\0';
      sprintf(inFile1_meta, "%s.meta", f);
      if (fileExists(inFile1_meta)) {
        md1 = meta_read(inFile1_meta);
      }
      FREE(f);
    }
    if (inFile2 != NULL && strlen(inFile2) > 0) {
      f = STRDUP(inFile2);
      c = findExt(f);
      *c = '\0';
      sprintf(inFile2_meta, "%s.meta", f);
      if (fileExists(inFile2_meta)) {
        md2 = meta_read(inFile2_meta);
      }
      FREE(f);
    }
    band_count1 = (md1 != NULL) ? md1->general->band_count : 1;
    band_count2 = (md2 != NULL) ? md2->general->band_count : 1;

    if (band_count1 > 0 && md1 != NULL) {
      band_names1 = extract_band_names(md1->general->bands, md1->general->band_count);
    }
    if (band_count2 > 0 && md2 != NULL) {
      band_names2 = extract_band_names(md2->general->bands, md2->general->band_count);
    }
  }
  else {
    // Produce numeric band names (if no ASF metadata)
    // FIXME: Look for metadata band names in GeoTIFFs ...they may have been exported by ASF
    band_names1 = (char**)MALLOC(MAX_BANDS*sizeof(char*));
    if (band_names1 != NULL) {
      int i;
      for (i=0; i<MAX_BANDS; i++) {
        band_names1[i] = (char*)CALLOC(64, sizeof(char));
        if (band_names1[i] != NULL) {
          sprintf(band_names1[i], "%02d", i);
        }
      }
    }
    band_names2 = (char**)MALLOC(MAX_BANDS*sizeof(char*));
    if (band_names2 != NULL) {
      int i;
      for (i=0; i<MAX_BANDS; i++) {
        band_names2[i] = (char*)CALLOC(64, sizeof(char));
        if (band_names2[i] != NULL) {
          sprintf(band_names2[i], "%02d", i);
        }
      }
    }
  }

  // Check each band for differences
  char band_str1[64];
  char band_str2[64];
  for (band=0; band<num_bands; band++) {
    // If differences exist, then produce an output file with content, else
    // produce an empty output file (handy for scripts that check for file existence
    // AND file size greater than zero)
    // Compare statistics
    baseline_range = fabs(stats1[band].sdev) * 6.0; // Assume 6-sigma range (99.999999%) is full range of data
    min_tol = (MIN_DIFF_TOL/100.0)*baseline_range;
    max_tol = (MAX_DIFF_TOL/100.0)*baseline_range;
    mean_tol = (MEAN_DIFF_TOL/100.0)*baseline_range;
    sdev_tol = (SDEV_DIFF_TOL/100.0)*stats1[band].sdev;
    psnr_tol = PSNR_TOL;
    min_diff = fabs(stats2[band].min - stats1[band].min);
    max_diff = fabs(stats2[band].max - stats1[band].max);
    mean_diff = fabs(stats2[band].mean - stats1[band].mean);
    sdev_diff = fabs(6.0*stats2[band].sdev - 6.0*stats1[band].sdev);

    if (strict &&
        (stats1[band].stats_good && stats2[band].stats_good) &&
        (min_diff > min_tol ||
         max_diff > max_tol ||
         mean_diff > mean_tol ||
         sdev_diff > sdev_tol ||
         psnr[band] > psnr_tol))
    {
      // Strict comparison utilizes all values
      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      char msg[1024];
      if (num_bands > 1) {
        sprintf(band_str1, "band %s in ", band_names1[band]);
        sprintf(band_str2, "band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      sprintf(msg, "FAIL: Comparing %s%s to %s%s\n      resulted in differences found:\n\n",
              band_str1, inFile2, band_str2, inFile1);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [min]   File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              min_diff > min_tol ? "FAIL" : "PASS",
              stats1[band].min, stats2[band].min, min_tol, MIN_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [max]   File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              max_diff > max_tol ? "FAIL" : "PASS",
              stats1[band].max, stats2[band].max, max_tol, MAX_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              mean_diff > mean_tol ? "FAIL" : "PASS",
              stats1[band].mean, stats2[band].mean, mean_tol, MEAN_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              sdev_diff > sdev_tol ? "FAIL" : "PASS",
              stats1[band].sdev, stats2[band].sdev, sdev_tol, SDEV_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                  PSNR Tolerance: %11f\n",
              psnr[band] > psnr_tol ? "FAIL" : "PASS",
              psnr[band], psnr_tol);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
    else if (!strict &&
             (stats1[band].stats_good && stats2[band].stats_good) &&
             (mean_diff > mean_tol ||
              sdev_diff > sdev_tol ||
              psnr[band] > psnr_tol)) {
      // If not doing strict checking, skip comparing min and max values
      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      char msg[1024];
      if (num_bands > 1) {
        sprintf(band_str1, "band %s in ", band_names1[band]);
        sprintf(band_str2, "band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      sprintf(msg, "FAIL: Comparing %s%s to %s%s\n      resulted in differences found:\n\n",
              band_str1, inFile2, band_str2, inFile1);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              mean_diff > mean_tol ? "FAIL" : "PASS",
              stats1[band].mean, stats2[band].mean, mean_tol, MEAN_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              sdev_diff > sdev_tol ? "FAIL" : "PASS",
              stats1[band].sdev, stats2[band].sdev, sdev_tol, SDEV_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                  PSNR Tolerance: %11f\n",
              psnr[band] > psnr_tol ? "FAIL" : "PASS",
              psnr[band], psnr_tol);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
    else if (stats1[band].stats_good && stats2[band].stats_good) {
      asfPrintStatus("\nNo differences found\n\n");
    }

    if (!stats1[band].stats_good || !stats2[band].stats_good) {
      char msg[1024];

      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      if (!stats1[band].stats_good) {
        sprintf(msg, "FAIL: %s file statistics not found.\n", inFile1);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }
      if (!stats2[band].stats_good) {
        sprintf(msg, "FAIL: %s file statistics not found.\n", inFile2);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
  } // For each band

  FCLOSE(outputFP);
  // FIXME: FREE() the band name arrays
}

void diffErrOut(char *outputFile, char *err_msg)
{
  char msg[1024];
  FILE *outputFP = NULL;

  if (outputFile != NULL && strlen(outputFile) > 0) {
    outputFP = FOPEN(outputFile, "wa");

    fprintf(outputFP, "\n-----------------------------------------------\n");

    sprintf(msg, "FAIL: %s\n", err_msg);
    fprintf(outputFP, msg);

    fprintf(outputFP, "-----------------------------------------------\n\n");

    FCLOSE(outputFP);
  }
  else {
    asfPrintError("Invalid output file name (NULL or zero length)\n");
  }
}


