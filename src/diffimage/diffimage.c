/*******************************************************************************
NAME: diffimage

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0            7/07   B. Dixon    As released

ALGORITHM DESCRIPTION:
  Calculates statistics in each input file
  Calculates PSNR between the two input files
  Performs an fftMatch between the two input files
  Checks for differences in stats, a PSNR that is too low, and for
    shifts in geolocation

BUGS:
        The images must have at least 75% overlap.
        diffimage will occasionally fail to find the
        correct offset (especially with noisy images).

*******************************************************************************/
#include "asf.h"
#include "asf_nan.h"
#include <math.h>
#include <ctype.h>
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "asf_raster.h"
#include "typlim.h"
#include <float_image.h>
#include <uint8_image.h>
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
#include <png.h>
#include <jpeglib.h>
#include <gsl/gsl_math.h>
#include "diffimage_tolerances.h"
#include "geotiff_support.h"

#define VERSION 1.0

#ifndef png_jmpbuf
#  define png_jmpbuf (png_ptr)    ((png_ptr)->jmpbuf)
#endif

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define FLOAT_TOLERANCE 0.000001
#define MISSING_PSNR -32000
#define MIN_MATCH_CERTAINTY 0.30
#define FILE1_FFTFILE "tmp_file1.img"
#define FILE1_FFTFILE_META "tmp_file1.meta"
#define FILE2_FFTFILE "tmp_file2.img"
#define FILE2_FFTFILE_META "tmp_file2.meta"
#define CORR_FILE "tmp_corr"

/**** TYPES ****/
typedef enum {
  UNKNOWN_GRAPHICS_TYPE=0,
  ASF_IMG,
  JPEG,
  PGM,
  PPM,
  PBM,
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

typedef struct {
  float dx; // In whatever units the image is in
  float dy;
  float cert; // Certainty from fftMatch
} shift_data_t;

#define MISSING_TIFF_DATA -1
typedef struct {
  uint32 width;
  uint32 height;
  short sample_format;
  short bits_per_sample;
  short planar_config;
  data_type_t data_type; // ASF data type
  int num_bands;
  int is_scanline_format;
} tiff_data_t;

#define MISSING_PNG_DATA -1
typedef struct {
  uint32 width;
  uint32 height;
  int bit_depth;
  int color_type; // Defined by PNG lib
  char color_type_str[64]; // Human-readable color type desc.
  int interlace_type;
  int compression_type;
  int filter_type;
  data_type_t data_type; // ASF data type
  int num_bands;
} png_info_t;

#define MISSING_PPM_PGM_DATA -1
typedef struct {
  char magic[2];
  uint32 width;
  uint32 height;
  uint32 max_val; // May be 255 or less
  int ascii_data;
  uint32 img_offset; // Just past header
  int bit_depth; // Should only be 8-bits per element
  data_type_t data_type; // ASF data type
  int num_bands; // 1 or 3
} ppm_pgm_info_t;

#define MISSING_JPEG_DATA -1
typedef struct {
  struct jpeg_error_mgr pub;
  jmp_buf setjmp_buffer;
} jpeg_error_hdlr_t;
typedef struct {
  uint32 width;
  uint32 height;
  data_type_t data_type; // ASF data type
  int num_bands; // Number of color elements
} jpeg_info_t;
typedef jpeg_error_hdlr_t *jpeg_err_hdlr;

#define MISSING_GTIF_DATA -1
typedef struct {
  int gtif_data_exists;
  char *GTcitation;
  char *PCScitation;
  int tie_point_elements; // Number of tie point elements (usually a multiple of 6)
  int num_tie_points; // Number of elements divided by 6 since (i,j,k) maps to (x,y,z)
  double *tie_point;  // Usually only 1, but who knows what evil lurks in the hearts of men?
  int pixel_scale_elements; // Usually a multiple of 3
  int num_pixel_scales;
  double *pixel_scale;  // Should always be 3 of these ...for ScaleX, ScaleY, and ScaleZ
  short model_type;
  short raster_type;
  short linear_units;
  double scale_factor;
  datum_type_t datum;
  char hemisphere;
  unsigned long pro_zone; // UTM zone (UTM only)
  short proj_coords_trans;
  short pcs;
  short geodetic_datum;
  short geographic_datum;
  double false_easting;
  double false_northing;
  double natLonOrigin;
  double lonCenter;
  double falseOriginLon;
  double falseOriginLat;
  double natLatOrigin;
  double latCenter;
  double stdParallel1;
  double stdParallel2;
  double lonPole;
} geotiff_data_t;

/**** PROTOTYPES ****/
void usage(char *name);
void msg_out(FILE *fpLog, int quiet, char *msg);
void err_out(FILE *fpError, int quiet, char *msg);
int asf_img_file_found(char *file);
graphics_file_t getGraphicsFileType (char *file);
void graphicsFileType_toStr (graphics_file_t type, char *type_str);
void fftDiff(char *inFile1, char *inFile2, float *bestLocX, float *bestLocY, float *certainty);
float get_maxval(data_type_t data_type);
void calc_asf_img_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               double *psnr, int band);
void calc_ppm_pgm_stats_2files(char *inFile1, char *inFile2, char *outfile,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               double *psnr, int band);
void calc_jpeg_stats_2files(char *inFile1, char *inFile2, char *outfile,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            double *psnr, int band);
void calc_tiff_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            double *psnr, int band);
void calc_png_stats_2files(char *inFile1, char *inFile2, char *outfile,
                           stats_t *inFile1_stats, stats_t *inFile2_stats,
                           double *psnr, int band);
void print_stats_results(char *filename1, char *filename2,
                         char *band_str1, char *band_str2,
                         stats_t *s1, stats_t *s2,
                         double psnr);
void diff_check_stats(char *outputFile, char *inFile1, char *inFile2,
                      stats_t *stats1, stats_t *stats2, double *psnr,
                      int strict, data_type_t t, int num_bands);
void diff_check_geotiff(char *outfile, geotiff_data_t *g1, geotiff_data_t *g2);
void diff_check_geolocation(char *outputFile, char *inFile1, char *inFile2,
                            int num_bands, shift_data_t *shift,
                           stats_t *stats1, stats_t *stats2);
void diffErrOut(char *outputFile, char *err_msg);
char *data_type2str(data_type_t data_type);
void get_tiff_info_from_file(char *file, tiff_data_t *t);
void get_tiff_info(TIFF *tif, tiff_data_t *t);
void get_geotiff_keys(char *file, geotiff_data_t *g);
void projection_type_2_str(projection_type_t proj, char *proj_str);
int  tiff_image_band_statistics_from_file(char *inFile, int band_no, int *stats_exist,
                                          double *min, double *max,
                                          double *mean, double *sdev, double *rmse,
                                          int use_mask_value, float mask_value);
void tiff_image_band_psnr_from_files(char *inFile1, char *inFile2,
                                     int band1, int band2, double *psnr);
float tiff_image_get_float_pixel(TIFF *tif, int row, int col, int band_no);
void tiff_get_float_line(TIFF *tif, float *buf, int row, int band_no);
void get_png_info_hdr_from_file(char *inFile, png_info_t *ihdr1, char *outfile);
int png_image_band_statistics_from_file(char *inFile, char *outfile,
                                        int band_no, int *stats_exist,
                                        double *min, double *max,
                                        double *mean, double *sdev, double *rmse,
                                        int use_mask_value, float mask_value);
void png_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                    int band1, int band2, double *psnr);
void png_sequential_get_float_line(png_structp png_ptr, png_infop info_ptr, float *buf, int band);
void get_ppm_pgm_info_hdr_from_file(char *inFile, ppm_pgm_info_t *pgm, char *outfile);
void ppm_pgm_get_float_line(FILE *fp, float *buf, int row, ppm_pgm_info_t *pgm, int band_no);
int ppm_pgm_image_band_statistics_from_file(char *inFile, char *outfile,
                                             int band_no, int *stats_exist,
                                             double *min, double *max,
                                             double *mean, double *sdev, double *rmse,
                                             int use_mask_value, float mask_value);
void ppm_pgm_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                        int band1, int band2, double *psnr);
void get_band_names(char *inFile, FILE *outputFP,
                    char ***band_names, int *num_extracted_bands);
void free_band_names(char ***band_names, int num_extracted_bands);
METHODDEF(void) jpeg_err_exit(j_common_ptr cinfo);
GLOBAL(void) get_jpeg_info_hdr_from_file(char *inFile, jpeg_info_t *jpg, char *outputFile);
GLOBAL(void) jpeg_image_band_statistics_from_file(char *inFile, char *outfile,
                                                  int band_no, int *stats_exist,
                                                  double *min, double *max,
                                                  double *mean, double *sdev, double *rmse,
                                                  int use_mask_value, float mask_value);
GLOBAL(void) jpeg_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                             int band1, int band2, double *psnr);
void make_generic_meta(char *file, uint32 height, uint32 width, data_type_t data_type);
void fftShiftCheck(char *file1, char *file2, char *corr_file,
                   shift_data_t *shifts);
void export_ppm_pgm_to_asf_img(char *inFile, char *outfile,
                               char *fft_file, char *fft_meta_file,
                               uint32 height, uint32 width, data_type_t data_type,
                               int band);
void export_jpeg_to_asf_img(char *inFile, char *outfile,
                            char *fft_file, char *fft_meta_file,
                            uint32 height, uint32 width, data_type_t data_type,
                            int band);
void export_tiff_to_asf_img(char *inFile, char *outfile,
                            char *fft_file, char *fft_meta_file,
                            uint32 height, uint32 width, data_type_t data_type,
                            int band);
void export_png_to_asf_img(char *inFile, char *outfile,
                           char *fft_file, char *fft_meta_file,
                           uint32 height, uint32 width, data_type_t data_type,
                           int band);

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
  int num_names_extracted1 = 0;
  int num_names_extracted2 = 0;
  char msg[1024];
  char type_str[255];
  stats_t inFile1_stats[MAX_BANDS];
  stats_t inFile2_stats[MAX_BANDS];
  double psnr[MAX_BANDS]; // peak signal to noise ratio
  shift_data_t shifts[MAX_BANDS];
  //float bestLocX, bestLocY, certainty;

  fLog=NULL;
  logflag=quietflag=0;
  outputFile=(char*)CALLOC(1024, sizeof(char));

  /* process command line */
  // FIXME: Might want to add -band1 and -band2 flags that would allow comparing
  // any arbitrary band in file1 to any arbitrary band in file2
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
          FREE(outputFile);
          usage(argv[0]);
        }
        break;
      case 'o':/* -output <filename>, get output filename ...empty if no differences */
        if (0==strncmp(optarg,"utput",5)) {
          sscanf(argv[optind++], "%s", outputFile);
          outputflag=1;
        }
        else {
          FREE(outputFile);
          usage(argv[0]);
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

  // After parsing out the command line arguments, there should be 2 arguments left...
  // the two files to compare
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
  // Set up output redirection for error and log messages
  if (logflag == 0) {
    fLog = NULL;
    fError = NULL;
  }
  else {
    fError = fLog;
  }
  if (!outputflag) {
    sprintf(msg, "Missing output file name ...no place to store file differences!\n");
    asfPrintError(msg);
  }
  if (strcmp(logFile, outputFile) == 0) {
    sprintf(msg, "Log file cannot be the same as the output file:\n     Log file: %s\n  Output file: %s\n",
            logFile, outputFile);
    if (outputFile) FREE(outputFile);
    asfPrintError(msg);
  }

  // Empty the output file
  FILE *fp=(FILE*)FOPEN(outputFile, "w");
  if(fp) FCLOSE(fp);

  // Determine input file graphical format types and check to see if they are
  // supported and the same type (etc ...error checking)
  if (strcmp(inFile1, inFile2) == 0) {
    FREE(outputFile);
    return (0); // PASS - a file compared to itself is always the same
  }
  if (!fileExists(inFile1)) {
    sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?", inFile1);
    err_out(fError, quietflag, msg);
    FREE(outputFile);
    return (1);
  }
  if (!fileExists(inFile2)) {
    sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?", inFile2);
    err_out(fError, quietflag, msg);
    FREE(outputFile);
    return (1);
  }
  graphics_file_t type1, type2;
  type1 = getGraphicsFileType(inFile1);
  type2 = getGraphicsFileType(inFile2);
  if (type1 != ASF_IMG  &&
      type1 != JPEG     &&
      type1 != PGM      &&
      type1 != PPM      &&
      type1 != PNG      &&
      type1 != STD_TIFF &&
      type1 != GEO_TIFF )
  {
    graphicsFileType_toStr(type1, type_str);
    sprintf(msg, "Graphics file type %s is not currently supported (Image #1: %s)\n",
            type_str, inFile1);
    err_out(fError, quietflag, msg);
    FREE(outputFile);
    return (1);
  }
  if (type2 != ASF_IMG  &&
      type2 != JPEG     &&
      type2 != PGM      &&
      type2 != PPM      &&
      type1 != PNG      &&
      type2 != STD_TIFF &&
      type2 != GEO_TIFF )
  {
    graphicsFileType_toStr(type2, type_str);
    sprintf(msg, "Graphics file type %s is not currently supported (Image #2: %s)\n",
            type_str, inFile2);
    err_out(fError, quietflag, msg);
    FREE(outputFile);
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
    FREE(outputFile);
    return (1);
  }

  /***** Calculate stats and PSNR *****/
  //   Can't be here unless both graphics file types were the same,
  //   so choose the input process based on just one of them.
  char **band_names1, **band_names2;
  char band_str1[255], band_str2[255];
  switch (type1) {
    case ASF_IMG:
      {
        int band_count1, band_count2;
        char inFile1_meta[1024], inFile2_meta[1024];
        char *f1, *f2, *c;
        meta_parameters *md1 = NULL;
        meta_parameters *md2 = NULL;

        // Read metadata and check for multi-bandedness
        get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
        get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
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
            FREE(outputFile);
            FREE(f1);
            free_band_names(&band_names1, num_names_extracted1);
            free_band_names(&band_names2, num_names_extracted2);
            asfPrintError(msg);
          }
          if (md1 == NULL) {
            // Can't find metadata file
            sprintf(msg, "Cannot find metadata file %s\n", inFile1_meta);
            diffErrOut(outputFile, msg);
            FREE(outputFile);
            FREE(f1);
            free_band_names(&band_names1, num_names_extracted1);
            free_band_names(&band_names2, num_names_extracted2);
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
            FREE(outputFile);
            FREE(f1);
            FREE(f2);
            free_band_names(&band_names1, num_names_extracted1);
            free_band_names(&band_names2, num_names_extracted2);
            asfPrintError(msg);
          }
          if (md2 == NULL) {
            // Can't find metadata file
            sprintf(msg, "Cannot find metadata file %s\n", inFile2_meta);
            diffErrOut(outputFile, msg);
            FREE(outputFile);
            FREE(f1);
            FREE(f2);
            free_band_names(&band_names1, num_names_extracted1);
            free_band_names(&band_names2, num_names_extracted2);
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
          meta_free(md1);
          meta_free(md2);
          FREE(outputFile);
          FREE(f1);
          FREE(f2);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (!bandflag && band_count1 != band_count2) {
          sprintf(msg, "Files do not have the same number of bands.\n"
              "Cannot compare all bands.  Consider using the -band option to\n"
              "compare individual bands within the files.\n"
              "\nFile1 has %d bands.  File2 has %d bands\n",
              band_count1, band_count2);
          diffErrOut(outputFile, msg);
          meta_free(md1);
          meta_free(md2);
          FREE(outputFile);
          FREE(f1);
          FREE(f2);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (md1->general->data_type != md2->general->data_type) {
          char *s1 = data_type2str(md1->general->data_type);
          char *s2 = data_type2str(md2->general->data_type);
          sprintf(msg, "Files do not have the same data type.\n"
              "\nFile1 has %s data.  File2 has %s data\n",
              s1, s2);
          FREE(s1);
          FREE(s2);
          diffErrOut(outputFile, msg);
          meta_free(md1);
          meta_free(md2);
          FREE(outputFile);
          FREE(f1);
          FREE(f2);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }

        //////////////////////////////////////////////////////////////////////////////////////
        // Calculate statistics, PSNR, measure image-to-image shift in geolocation, and then
        // check the results
        if (!bandflag) {
          // Process every available band
          int band_no;
          for (band_no=0; band_no < band_count1; band_no++) {
            strcpy(band_str1, "");
            strcpy(band_str2, "");
            if (band_count1 > 0) {
              sprintf(band_str1, "Band %s in ", band_names1[band_no]);
              sprintf(band_str2, "Band %s in ", band_names2[band_no]);
            }
            asfPrintStatus("\nCalculating statistics for\n  %s%s and\n  %s%s\n",
                           band_str1, inFile1, band_str2, inFile2);
            calc_asf_img_stats_2files(inFile1, inFile2,
                                      &inFile1_stats[band_no], &inFile2_stats[band_no],
                                      &psnr[band_no], band_no);
            if (inFile1_stats[band_no].stats_good && inFile2_stats[band_no].stats_good) {
              fftShiftCheck(inFile1, inFile2,
                            CORR_FILE, &shifts[band_no]);
            }
            else {
              shifts[band_no].dx = 0.0;
              shifts[band_no].dy = 0.0;
              shifts[band_no].cert = 1.0;
            }
          }

          // Assumes both files have the same band count and data types or would not be here
          diff_check_stats(outputFile,
                           inFile1, inFile2, inFile1_stats, inFile2_stats, psnr, strictflag,
                           md1->general->data_type, band_count1);
          diff_check_geolocation(outputFile, inFile1, inFile2, band_count1, shifts,
                                inFile1_stats, inFile2_stats);
        }
        else {
          // Process selected band
          asfPrintStatus("\nCalculating statistics for\n  %s and\n  %s\n", inFile1, inFile2);
          calc_asf_img_stats_2files(inFile1, inFile2,
                                    inFile1_stats, inFile2_stats,
                                    psnr, band);
          if (inFile1_stats[band].stats_good && inFile2_stats[band].stats_good) {
            fftShiftCheck(inFile1, inFile2,
                          CORR_FILE, &shifts[band]);
          }
          else {
            shifts[band].dx = 0.0;
            shifts[band].dy = 0.0;
            shifts[band].cert = 1.0;
          }

          // Assumes both files have the same band count and data types or would not be here
          diff_check_stats(outputFile,
                           inFile1, inFile2, &inFile1_stats[band], &inFile2_stats[band], &psnr[band],
                           strictflag, md1->general->data_type, 1);
          diff_check_geolocation(outputFile, inFile1, inFile2, 1, shifts,
                                 &inFile1_stats[band], &inFile2_stats[band]);
        }
        //
        //////////////////////////////////////////////////////////////////////////////////////
        free_band_names(&band_names1, num_names_extracted1);
        free_band_names(&band_names2, num_names_extracted2);
      }
      break;
    case JPEG:
      {
        jpeg_info_t jpg1, jpg2;

        // Determine number of bands and data type etc.
        get_jpeg_info_hdr_from_file(inFile1, &jpg1, outputFile);
        get_jpeg_info_hdr_from_file(inFile2, &jpg2, outputFile);
        get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
        get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);

        if (jpg1.data_type != jpg2.data_type) {
          char *s1 = data_type2str(jpg1.data_type);
          char *s2 = data_type2str(jpg2.data_type);
          sprintf(msg, "Files do not have the same data type.\n"
              "\nFile1 has %s data.  File2 has %s data\n",
              s1, s2);
          FREE(s1);
          FREE(s2);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (bandflag && !(band < jpg1.num_bands && band < jpg2.num_bands && band >= 0)) {
          sprintf(msg, "Invalid band number.  Band number must be 0 (first band)\n"
              "or greater, and less than the number of available bands in the file\n"
                  "Example:  If the files have 3 bands, then band numbers 0, 1, or 2 are\n"
                  "the valid band number choices.  \n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              jpg1.num_bands, jpg2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (!bandflag && jpg1.num_bands != jpg2.num_bands) {
          sprintf(msg, "Files do not have the same number of bands.\n"
              "Cannot compare all bands.  Consider using the -band option to\n"
                  "compare individual bands within the files.\n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              jpg1.num_bands, jpg2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }

        //////////////////////////////////////////////////////////////////////////////////////
        // Calculate statistics, PSNR, measure image-to-image shift in geolocation, and then
        // check the results
        if (!bandflag) {
          // Process every available band
          int band_no;
          for (band_no=0; band_no < jpg1.num_bands; band_no++) {
            strcpy(band_str1, "");
            strcpy(band_str2, "");
            if (jpg1.num_bands > 1) {
              sprintf(band_str1, "Band %s in ", band_names1[band_no]);
              sprintf(band_str2, "Band %s in ", band_names2[band_no]);
            }
            asfPrintStatus("\nCalculating statistics for\n  %s%s and\n  %s%s\n",
                           band_str1, inFile1, band_str2, inFile2);
            calc_jpeg_stats_2files(inFile1, inFile2, outputFile,
                                   &inFile1_stats[band_no], &inFile2_stats[band_no],
                                   &psnr[band_no], band_no);
            asfPrintStatus("\nMeasuring image shift from\n  %s%s to\n  %s%s\n",
                           band_str1, inFile1, band_str2, inFile2);

            if (inFile1_stats[band_no].stats_good && inFile2_stats[band_no].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
              export_jpeg_to_asf_img(inFile1, outputFile,
                                     FILE1_FFTFILE, FILE1_FFTFILE_META,
                                     jpg1.height, jpg1.width, REAL32, band_no);
              export_jpeg_to_asf_img(inFile2, outputFile,
                                     FILE2_FFTFILE, FILE2_FFTFILE_META,
                                     jpg2.height, jpg2.width, REAL32, band_no);
              fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                            CORR_FILE, &shifts[band_no]);
            }
            else {
              shifts[band_no].dx = 0.0;
              shifts[band_no].dy = 0.0;
              shifts[band_no].cert = 1.0;
            }
          }
          diff_check_stats(outputFile,
                           inFile1, inFile2, inFile1_stats, inFile2_stats, psnr, strictflag,
                           jpg1.data_type, jpg1.num_bands);
          diff_check_geolocation(outputFile, inFile1, inFile2, jpg1.num_bands, shifts,
                                 inFile1_stats, inFile2_stats);
        }
        else {
          // Process selected band
          asfPrintStatus("\nCalculating statistics for\n  %s and\n  %s\n",
                         inFile1, inFile2);
          calc_jpeg_stats_2files(inFile1, inFile2, outputFile,
                                 inFile1_stats, inFile2_stats,
                                 psnr, band);
          if (inFile1_stats[band].stats_good && inFile2_stats[band].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
            export_jpeg_to_asf_img(inFile1, outputFile,
                                   FILE1_FFTFILE, FILE1_FFTFILE_META,
                                   jpg1.height, jpg1.width, REAL32, band);
            export_jpeg_to_asf_img(inFile2, outputFile,
                                   FILE2_FFTFILE, FILE2_FFTFILE_META,
                                   jpg2.height, jpg2.width, REAL32, band);
            fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                          CORR_FILE, &shifts[band]);
          }
          else {
            shifts[band].dx = 0.0;
            shifts[band].dy = 0.0;
            shifts[band].cert = 1.0;
          }

          // Check for differences in stats or geolocation
          diff_check_stats(outputFile,
                           inFile1, inFile2, &inFile1_stats[band], &inFile2_stats[band], &psnr[band],
                           strictflag, jpg1.data_type, 1);
          diff_check_geolocation(outputFile, inFile1, inFile2, 1, &shifts[band],
                                 &inFile1_stats[band], &inFile2_stats[band]);
        }
        //
        //////////////////////////////////////////////////////////////////////////////////////
        free_band_names(&band_names1, num_names_extracted1);
        free_band_names(&band_names2, num_names_extracted2);
      }
      break;
    case PNG:
      {
        png_info_t ihdr1, ihdr2;

        // Determine number of bands and data type etc.
        get_png_info_hdr_from_file(inFile1, &ihdr1, outputFile);
        get_png_info_hdr_from_file(inFile2, &ihdr2, outputFile);
        get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
        get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);

        if (ihdr1.data_type != ihdr2.data_type) {
          char *s1 = data_type2str(ihdr1.data_type);
          char *s2 = data_type2str(ihdr2.data_type);
          sprintf(msg, "Files do not have the same data type.\n"
              "\nFile1 has %s data.  File2 has %s data\n",
              s1, s2);
          FREE(s1);
          FREE(s2);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (bandflag && !(band < ihdr1.num_bands && band < ihdr2.num_bands && band >= 0)) {
          sprintf(msg, "Invalid band number.  Band number must be 0 (first band)\n"
              "or greater, and less than the number of available bands in the file\n"
                  "Example:  If the files have 3 bands, then band numbers 0, 1, or 2 are\n"
                  "the valid band number choices.  \n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              ihdr1.num_bands, ihdr2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (!bandflag && ihdr1.num_bands != ihdr2.num_bands) {
          sprintf(msg, "Files do not have the same number of bands.\n"
              "Cannot compare all bands.  Consider using the -band option to\n"
                  "compare individual bands within the files.\n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              ihdr1.num_bands, ihdr2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }

        //////////////////////////////////////////////////////////////////////////////////////
        // Calculate statistics, PSNR, measure image-to-image shift in geolocation, and then
        // check the results
        if (!bandflag) {
          // Process every available band
          int band_no;
          for (band_no=0; band_no < ihdr1.num_bands; band_no++) {
            strcpy(band_str1, "");
            strcpy(band_str2, "");
            if (ihdr1.num_bands > 1) {
              sprintf(band_str1, "Band %s in ", band_names1[band_no]);
              sprintf(band_str2, "Band %s in ", band_names2[band_no]);
            }
            asfPrintStatus("\nCalculating statistics for\n  %s%s and\n  %s%s\n",
                          band_str1, inFile1, band_str2, inFile2);
            calc_png_stats_2files(inFile1, inFile2, outputFile,
                                  &inFile1_stats[band_no], &inFile2_stats[band_no],
                                  &psnr[band_no], band_no);
            if (inFile1_stats[band_no].stats_good && inFile2_stats[band_no].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
              export_png_to_asf_img(inFile1, outputFile,
                                    FILE1_FFTFILE, FILE1_FFTFILE_META,
                                    ihdr1.height, ihdr1.width, REAL32, band_no);
              export_png_to_asf_img(inFile2, outputFile,
                                    FILE2_FFTFILE, FILE2_FFTFILE_META,
                                    ihdr2.height, ihdr2.width, REAL32, band_no);
              fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                            CORR_FILE, &shifts[band_no]);
            }
            else {
              shifts[band_no].dx = 0.0;
              shifts[band_no].dy = 0.0;
              shifts[band_no].cert = 1.0;
            }
          }
          diff_check_stats(outputFile,
                          inFile1, inFile2, inFile1_stats, inFile2_stats, psnr, strictflag,
                          ihdr1.data_type, ihdr1.num_bands);
          diff_check_geolocation(outputFile, inFile1, inFile2, ihdr1.num_bands, shifts,
                                inFile1_stats, inFile2_stats);
        }
        else {
          // Process selected band
          asfPrintStatus("\nCalculating statistics for\n  %s and\n  %s\n",
                        inFile1, inFile2);
          calc_png_stats_2files(inFile1, inFile2, outputFile,
                                inFile1_stats, inFile2_stats,
                                psnr, band);
          if (inFile1_stats[band].stats_good && inFile2_stats[band].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
            export_png_to_asf_img(inFile1, outputFile,
                                  FILE1_FFTFILE, FILE1_FFTFILE_META,
                                  ihdr1.height, ihdr1.width, REAL32, band);
            export_png_to_asf_img(inFile2, outputFile,
                                  FILE2_FFTFILE, FILE2_FFTFILE_META,
                                  ihdr2.height, ihdr2.width, REAL32, band);
            fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                          CORR_FILE, &shifts[band]);
          }
          else {
            shifts[band].dx = 0.0;
            shifts[band].dy = 0.0;
            shifts[band].cert = 1.0;
          }
          diff_check_stats(outputFile,
                           inFile1, inFile2, &inFile1_stats[band], &inFile2_stats[band], &psnr[band],
                          strictflag, ihdr1.data_type, 1);
          diff_check_geolocation(outputFile, inFile1, inFile2, 1, shifts,
                                 &inFile1_stats[band], &inFile2_stats[band]);
        }
        //
        //////////////////////////////////////////////////////////////////////////////////////
        free_band_names(&band_names1, num_names_extracted1);
        free_band_names(&band_names2, num_names_extracted2);
      }
      break;
    case PPM:
    case PGM:
      {
        ppm_pgm_info_t pgm1, pgm2;

        // Determine number of bands and data type etc.
        get_ppm_pgm_info_hdr_from_file(inFile1, &pgm1, outputFile);
        get_ppm_pgm_info_hdr_from_file(inFile2, &pgm2, outputFile);
        get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
        get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);

        if (pgm1.data_type != pgm2.data_type) {
          char *s1 = data_type2str(pgm1.data_type);
          char *s2 = data_type2str(pgm2.data_type);
          sprintf(msg, "Files do not have the same data type.\n"
              "\nFile1 has %s data.  File2 has %s data\n",
              s1, s2);
          FREE(s1);
          FREE(s2);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (bandflag && !(band < pgm1.num_bands && band < pgm2.num_bands && band >= 0)) {
          sprintf(msg, "Invalid band number.  Band number must be 0 (first band)\n"
              "or greater, and less than the number of available bands in the file\n"
                  "Example:  If the files have 3 bands, then band numbers 0, 1, or 2 are\n"
                  "the valid band number choices.  \n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              pgm1.num_bands, pgm2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (!bandflag && pgm1.num_bands != pgm2.num_bands) {
          sprintf(msg, "Files do not have the same number of bands.\n"
              "Cannot compare all bands.  Consider using the -band option to\n"
                  "compare individual bands within the files.\n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              pgm1.num_bands, pgm2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }

        //////////////////////////////////////////////////////////////////////////////////////
        // Calculate statistics, PSNR, measure image-to-image shift in geolocation, and then
        // check the results
        if (!bandflag) {
          // Process every available band
          int band_no;
          for (band_no=0; band_no < pgm1.num_bands; band_no++) {
            strcpy(band_str1, "");
            strcpy(band_str2, "");
            if (pgm1.num_bands > 1) {
              sprintf(band_str1, "Band %s in ", band_names1[band_no]);
              sprintf(band_str2, "Band %s in ", band_names2[band_no]);
            }
            asfPrintStatus("\nCalculating statistics for\n  %s%s and\n  %s%s\n",
                          band_str1, inFile1, band_str2, inFile2);
            calc_ppm_pgm_stats_2files(inFile1, inFile2, outputFile,
                                      &inFile1_stats[band_no], &inFile2_stats[band_no],
                                      &psnr[band_no], band_no);
            if (inFile1_stats[band_no].stats_good && inFile2_stats[band_no].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
              export_ppm_pgm_to_asf_img(inFile1, outputFile,
                                        FILE1_FFTFILE, FILE1_FFTFILE_META,
                                        pgm1.height, pgm1.width, REAL32, band_no);
              export_ppm_pgm_to_asf_img(inFile2, outputFile,
                                        FILE2_FFTFILE, FILE2_FFTFILE_META,
                                        pgm2.height, pgm2.width, REAL32, band_no);
              fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                            CORR_FILE, &shifts[band_no]);
            }
            else {
              shifts[band_no].dx = 0.0;
              shifts[band_no].dy = 0.0;
              shifts[band_no].cert = 1.0;
            }
          }
          diff_check_stats(outputFile,
                          inFile1, inFile2, inFile1_stats, inFile2_stats, psnr, strictflag,
                          pgm1.data_type, pgm1.num_bands);
          diff_check_geolocation(outputFile, inFile1, inFile2, pgm1.num_bands, shifts,
                                inFile1_stats, inFile2_stats);
        }
        else {
          // Process selected band
          asfPrintStatus("\nCalculating statistics for\n  %s and\n  %s\n",
                        inFile1, inFile2);
          calc_ppm_pgm_stats_2files(inFile1, inFile2, outputFile,
                                    inFile1_stats, inFile2_stats,
                                    psnr, band);
          if (inFile1_stats[band].stats_good && inFile2_stats[band].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
            export_ppm_pgm_to_asf_img(inFile1, outputFile,
                                      FILE1_FFTFILE, FILE1_FFTFILE_META,
                                      pgm1.height, pgm1.width, REAL32, band);
            export_ppm_pgm_to_asf_img(inFile2, outputFile,
                                      FILE2_FFTFILE, FILE2_FFTFILE_META,
                                      pgm2.height, pgm2.width, REAL32, band);
            fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                          CORR_FILE, &shifts[band]);
          }
          else {
            shifts[band].dx = 0.0;
            shifts[band].dy = 0.0;
            shifts[band].cert = 1.0;
          }

          // Check for differences
          diff_check_stats(outputFile,
                           inFile1, inFile2, &inFile1_stats[band], &inFile2_stats[band], &psnr[band],
                          strictflag, pgm1.data_type, 1);
          diff_check_geolocation(outputFile, inFile1, inFile2, 1, shifts,
                                 &inFile1_stats[band], &inFile2_stats[band]);
        }
        //
        //////////////////////////////////////////////////////////////////////////////////////
        free_band_names(&band_names1, num_names_extracted1);
        free_band_names(&band_names2, num_names_extracted2);
      }
      break;
    case STD_TIFF:
    case GEO_TIFF:
      {
        int geotiff = (type1 == GEO_TIFF) ? 1 : 0;
        tiff_data_t t1, t2;
        geotiff_data_t g1, g2;

        // Determine number of bands and data type
        get_tiff_info_from_file(inFile1, &t1);
        get_tiff_info_from_file(inFile2, &t2);
        get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
        get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
        if (t1.data_type != t2.data_type) {
          char *s1 = data_type2str(t1.data_type);
          char *s2 = data_type2str(t2.data_type);
          sprintf(msg, "Files do not have the same data type.\n"
                  "\nFile1 has %s data.  File2 has %s data\n",
                  s1, s2);
          FREE(s1);
          FREE(s2);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (bandflag && !(band < t1.num_bands && band < t2.num_bands && band >= 0)) {
          sprintf(msg, "Invalid band number.  Band number must be 0 (first band)\n"
              "or greater, and less than the number of available bands in the file\n"
                  "Example:  If the files have 3 bands, then band numbers 0, 1, or 2 are\n"
                  "the valid band number choices.  \n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              t1.num_bands, t2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }
        if (!bandflag && t1.num_bands != t2.num_bands) {
          sprintf(msg, "Files do not have the same number of bands.\n"
              "Cannot compare all bands.  Consider using the -band option to\n"
                  "compare individual bands within the files.\n"
                  "\nFile1 has %d bands.  File2 has %d bands\n",
              t1.num_bands, t2.num_bands);
          diffErrOut(outputFile, msg);
          FREE(outputFile);
          free_band_names(&band_names1, num_names_extracted1);
          free_band_names(&band_names2, num_names_extracted2);
          asfPrintError(msg);
        }

        //////////////////////////////////////////////////////////////////////////////////////
        // Calculate statistics, PSNR, measure image-to-image shift in geolocation, and then
        // check the results
        if (!bandflag) {
          // Process every available band
          int band_no;
          for (band_no=0; band_no < t1.num_bands; band_no++) {
            strcpy(band_str1, "");
            strcpy(band_str2, "");
            if (t1.num_bands > 1) {
              sprintf(band_str1, "Band %s in ", band_names1[band_no]);
              sprintf(band_str2, "Band %s in ", band_names2[band_no]);
            }
            asfPrintStatus("\nCalculating statistics for\n  %s%s and\n  %s%s\n",
                          band_str1, inFile1, band_str2, inFile2);
            calc_tiff_stats_2files(inFile1, inFile2,
                                  &inFile1_stats[band_no], &inFile2_stats[band_no],
                                  &psnr[band_no], band_no);
            if (inFile1_stats[band_no].stats_good && inFile2_stats[band_no].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
              export_tiff_to_asf_img(inFile1, outputFile,
                                     FILE1_FFTFILE, FILE1_FFTFILE_META,
                                     t1.height, t1.width, REAL32, band_no);
              export_tiff_to_asf_img(inFile2, outputFile,
                                     FILE2_FFTFILE, FILE2_FFTFILE_META,
                                     t2.height, t2.width, REAL32, band_no);
              fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                            CORR_FILE, &shifts[band_no]);
            }
            else {
              shifts[band_no].dx = 0.0;
              shifts[band_no].dy = 0.0;
              shifts[band_no].cert = 1.0;
            }
            if (geotiff) {
              get_geotiff_keys(inFile1, &g1);
              get_geotiff_keys(inFile2, &g2);
              diff_check_geotiff(outputFile, &g1, &g2);
            }
            // fftMatch(shifts_t *shift, band_no) goes here
          }
          diff_check_stats(outputFile,
                          inFile1, inFile2, inFile1_stats, inFile2_stats, psnr, strictflag,
                          t1.data_type, t1.num_bands);
          diff_check_geolocation(outputFile, inFile1, inFile2, t1.num_bands, shifts,
                                inFile1_stats, inFile2_stats);
        }
        else {
          // Process selected band
          asfPrintStatus("\nCalculating statistics for\n  %s%s and\n  %s%s\n",
                        inFile1, inFile2);
          calc_tiff_stats_2files(inFile1, inFile2,
                                inFile1_stats, inFile2_stats,
                                psnr, band);
          if (inFile1_stats[band].stats_good && inFile2_stats[band].stats_good) {
              // Find shift in geolocation (if it exists)
              // (Export to an ASF internal format file for fftMatch() compatibility)
            export_tiff_to_asf_img(inFile1, outputFile,
                                   FILE1_FFTFILE, FILE1_FFTFILE_META,
                                   t1.height, t1.width, REAL32, band);
            export_tiff_to_asf_img(inFile2, outputFile,
                                   FILE2_FFTFILE, FILE2_FFTFILE_META,
                                   t2.height, t2.width, REAL32, band);
            fftShiftCheck(FILE1_FFTFILE, FILE2_FFTFILE,
                          CORR_FILE, &shifts[band]);
          }
          else {
            shifts[band].dx = 0.0;
            shifts[band].dy = 0.0;
            shifts[band].cert = 1.0;
          }
          if (geotiff) {
            get_geotiff_keys(inFile1, &g1);
            get_geotiff_keys(inFile2, &g2);
            diff_check_geotiff(outputFile, &g1, &g2);
          }
          // fftMatch(shifts_t *shift, band_no) goes here
          diff_check_stats(outputFile,
                          inFile1, inFile2, &inFile1_stats[band], &inFile2_stats[band], &psnr[band],
                          strictflag, t1.data_type, 1);
          diff_check_geolocation(outputFile, inFile1, inFile2, 1, shifts,
                                 &inFile1_stats[band], &inFile2_stats[band]);
        }
        //
        //////////////////////////////////////////////////////////////////////////////////////
        free_band_names(&band_names1, num_names_extracted1);
        free_band_names(&band_names2, num_names_extracted2);
      }
      break;
    default:
      sprintf(msg, "Unrecognized image file type found.\n");
      diffErrOut(outputFile, msg);
      FREE(outputFile);
      free_band_names(&band_names1, num_names_extracted1);
      free_band_names(&band_names2, num_names_extracted2);
      asfPrintError(msg);
      break;
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
      "                 supported types include TIFF, GEOTIFF, PNG, PGM, PPM,\n"
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
  FILE *fp = (FILE *)FOPEN(file, "rb");
  uint8 magic[4];
  graphics_file_t file_type = UNKNOWN_GRAPHICS_TYPE;
  TIFF *itif = NULL;
  GTIF *gtif = NULL;

  if (fp != NULL) {
    int i;

    // Read the magic number from the file header
    for (i=0; i<4; i++) {
      magic[i] = (uint8)fgetc(fp);
    }
    if(fp) FCLOSE(fp);

    // Check for a valid magic number combination
    if (magic[0] == 0xFF && magic[1] == 0xD8) {
      file_type = JPEG;
    }
    else if (magic[0] == 'P' && (magic[1] == '4' || magic[1] == '1')) {
      file_type = PBM;
    }
    else if (magic[0] == 'P' && (magic[1] == '5' || magic[1] == '2')) {
      file_type = PGM;
    }
    else if (magic[0] == 'P' && (magic[1] == '6' || magic[1] == '3')) {
      file_type = PPM;
    }
    else if ((magic[0] == 'I' && magic[1] == 'I') || (magic[0] == 'M' && magic[1] == 'M')) {
      file_type = STD_TIFF;

      itif = XTIFFOpen(file, "rb");
      if (itif != NULL) {
        gtif = GTIFNew(itif);
        if (gtif != NULL) {
          double *tie_point = NULL;
          double *pixel_scale = NULL;
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
    case PBM:
      strcpy (type_str, "PBM");
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
  double sse;
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
      *psnr = MISSING_PSNR;
    }
    else {
      asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s in %s\n",
                    band_names1[band1], inFile1, band_names2[band2], inFile2);
      FILE *fp1 = fopen(inFile1, "rb");
      FILE *fp2 = fopen(inFile2, "rb");
      if (fp1 != NULL && fp2 != NULL) {
        sse = 0.0;
        max_val = get_maxval(md1->general->data_type); // Since both file's data types are the same, this is OK
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
            sse += (data1[jj] - data2[jj]) * (data1[jj] - data2[jj]);
            pixel_count++;
          }
        }
        asfPercentMeter(1.0);
        if (fp1) FCLOSE(fp1);
        if (fp2) FCLOSE(fp2);
        if (pixel_count > 0 && max_val > 0) {
          rmse = sqrt(sse/pixel_count);
          *psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
        }
        else {
          *psnr = MISSING_PSNR;
        }
      }
      else {
        *psnr = MISSING_PSNR;
      }
    }
  }
  else {
    *psnr = MISSING_PSNR;
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

void calc_jpeg_stats_2files(char *inFile1, char *inFile2, char *outfile,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            double *psnr, int band)
{
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->stats_good = s2->stats_good = 0;
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
  s1->hist = s2->hist = NULL;
  s1->hist_pdf = s2->hist_pdf = NULL;
  *psnr = MISSING_PSNR;

  jpeg_image_band_statistics_from_file(inFile1, outfile, band, &s1->stats_good,
                                       &s1->min, &s1->max,
                                       &s1->mean, &s1->sdev, &s1->rmse,
                                       0, UINT8_IMAGE_DEFAULT_MASK);
  jpeg_image_band_statistics_from_file(inFile2, outfile, band, &s2->stats_good,
                                       &s2->min, &s2->max,
                                       &s2->mean, &s2->sdev, &s2->rmse,
                                       0, UINT8_IMAGE_DEFAULT_MASK);
  jpeg_image_band_psnr_from_files(inFile1, inFile2, outfile, band, band, psnr);
}

void calc_ppm_pgm_stats_2files(char *inFile1, char *inFile2, char *outfile,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               double *psnr, int band)
{
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->stats_good = s2->stats_good = 0;
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
  s1->hist = s2->hist = NULL;
  s1->hist_pdf = s2->hist_pdf = NULL;
  *psnr = MISSING_PSNR;

  ppm_pgm_image_band_statistics_from_file(inFile1, outfile, band, &s1->stats_good,
                                          &s1->min, &s1->max,
                                          &s1->mean, &s1->sdev, &s1->rmse,
                                          0, UINT8_IMAGE_DEFAULT_MASK);
  ppm_pgm_image_band_statistics_from_file(inFile2, outfile, band, &s2->stats_good,
                                          &s2->min, &s2->max,
                                          &s2->mean, &s2->sdev, &s2->rmse,
                                          0, UINT8_IMAGE_DEFAULT_MASK);
  ppm_pgm_image_band_psnr_from_files(inFile1, inFile2, outfile, band, band, psnr);
}

void calc_tiff_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            double *psnr, int band)
{
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->stats_good = s2->stats_good = 0;
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
  s1->hist = s2->hist = NULL;
  s1->hist_pdf = s2->hist_pdf = NULL;
  *psnr = MISSING_PSNR;

  tiff_image_band_statistics_from_file(inFile1, band, &s1->stats_good,
                                       &s1->min, &s1->max,
                                       &s1->mean, &s1->sdev, &s1->rmse,
                                       0, FLOAT_IMAGE_DEFAULT_MASK);
  tiff_image_band_statistics_from_file(inFile2, band, &s2->stats_good,
                                       &s2->min, &s2->max,
                                       &s2->mean, &s2->sdev, &s2->rmse,
                                       0, FLOAT_IMAGE_DEFAULT_MASK);
  tiff_image_band_psnr_from_files(inFile1, inFile2, band, band, psnr);
}

float get_maxval(data_type_t data_type)
{
  float ret;

  // Only non-complex types with 32 bits or less are supported
  switch (data_type) {
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
                         char *band_str1, char *band_str2,
                         stats_t *s1, stats_t *s2,
                         double psnr)
{
  asfPrintStatus(
      "\nStatistics for %s%s\n"
      "   min: %f\n"
      "   max: %f\n"
      "  mean: %f\n"
      "  sdev: %f\n"
      "  rmse: %f\n"
      "result: %s\n",
      band_str1, filename1, s1->min, s1->max, s1->mean, s1->sdev, s1->rmse,
      s1->stats_good ? "GOOD STATS" : "UNRELIABLE STATS");
  asfPrintStatus(
      "\nStatistics for %s%s\n"
      "   min: %f\n"
      "   max: %f\n"
      "  mean: %f\n"
      "  sdev: %f\n"
      "  rmse: %f\n"
      "result: %s\n",
      band_str2, filename2, s2->min, s2->max, s2->mean, s2->sdev, s2->rmse,
      s2->stats_good ? "GOOD STATS" : "UNRELIABLE STATS");
  asfPrintStatus("\nPSNR between files: %f\n\n", psnr);
}

void diff_check_stats(char *outputFile, char *inFile1, char *inFile2,
                      stats_t *stats1, stats_t *stats2, double *psnr,
                      int strict, data_type_t data_type, int num_bands)
{
  char msg[1024];
  int band;
  double baseline_range1;
  double baseline_range2;
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

  int num_extracted_bands1=0, num_extracted_bands2=0;
  FILE *outputFP = NULL;
  outputFP = fopen(outputFile, "a");

  // Get or produce band names for intelligent output...
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  get_band_names(inFile1, outputFP, &band_names1, &num_extracted_bands1);
  get_band_names(inFile2, outputFP, &band_names2, &num_extracted_bands2);

  // Check each band for differences
  char band_str1[64];
  char band_str2[64];
  for (band=0; band<num_bands; band++) {
    // If differences exist, then produce an output file with content, else
    // produce an empty output file (handy for scripts that check for file existence
    // AND file size greater than zero)
    // Compare statistics
    baseline_range1 = fabs(stats1[band].sdev) * 6.0; // Assume 6-sigma range (99.999999%) is full range of data
    baseline_range2 = fabs(stats2[band].sdev) * 6.0; // Assume 6-sigma range (99.999999%) is full range of data
    min_tol = (MIN_DIFF_TOL/100.0)*baseline_range1;
    max_tol = (MAX_DIFF_TOL/100.0)*baseline_range1;
    mean_tol = (MEAN_DIFF_TOL/100.0)*baseline_range1;
    sdev_tol = (SDEV_DIFF_TOL/100.0)*baseline_range1;
    // FIXME: Rather than use data_type, use the baseline range to develop a suitable PSNR tolerance
    switch (data_type) {
      case BYTE:
        psnr_tol = BYTE_PSNR_TOL;
        break;
      case INTEGER16:
        psnr_tol = INTEGER16_PSNR_TOL;
        break;
      case INTEGER32:
        psnr_tol = INTEGER32_PSNR_TOL;
        break;
      case REAL32:
      default:
        psnr_tol = REAL32_PSNR_TOL;
        break;
    }

    min_diff = fabs(stats2[band].min - stats1[band].min);
    max_diff = fabs(stats2[band].max - stats1[band].max);
    mean_diff = fabs(stats2[band].mean - stats1[band].mean);
    sdev_diff = fabs(baseline_range2 - baseline_range1);
    if (strict &&
        (stats1[band].stats_good && stats2[band].stats_good) &&
        (min_diff > min_tol ||
         max_diff > max_tol ||
         mean_diff > mean_tol ||
         sdev_diff > sdev_tol ||
         psnr[band] < psnr_tol))
    {
      // Strict comparison utilizes all values
      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      sprintf(msg, "FAIL: Comparing\n  %s%s\nto\n  %s%s\n\n",
              band_str1, inFile1, band_str2, inFile2);
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
              stats1[band].sdev, stats2[band].sdev, sdev_tol/6.0, SDEV_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      if (psnr[band] != MISSING_PSNR) {
        sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                    PSNR Minimum: %11f (higher == better)\n",
                psnr[band] < psnr_tol ? "FAIL" : "PASS",
                psnr[band], psnr_tol);
      }
      else {
        sprintf(msg, "[FAIL] [PSNR]   PSNR:      MISSING,                    PSNR Minimum: %11f (higher == better)\n",
                psnr_tol);
      }
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
    else if (!strict &&
             (stats1[band].stats_good && stats2[band].stats_good) &&
             (mean_diff > mean_tol ||
              sdev_diff > sdev_tol ||
              psnr[band] < psnr_tol)) {
      // If not doing strict checking, skip comparing min and max values
      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      char msg[1024];
      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      sprintf(msg, "FAIL: Comparing\n  %s%s\nto\n  %s%s\n\n",
              band_str1, inFile1, band_str2, inFile2);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              mean_diff > mean_tol ? "FAIL" : "PASS",
              stats1[band].mean, stats2[band].mean, mean_tol, MEAN_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: %11f (%3f Percent)\n",
              sdev_diff > sdev_tol ? "FAIL" : "PASS",
              stats1[band].sdev, stats2[band].sdev, sdev_tol/6.0, SDEV_DIFF_TOL);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      if (psnr[band] != MISSING_PSNR) {
        sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                    PSNR Minimum: %11f (higher == better)\n",
                psnr[band] < psnr_tol ? "FAIL" : "PASS",
                psnr[band], psnr_tol);
      }
      else {
        sprintf(msg, "[FAIL] [PSNR]   PSNR:      MISSING,                    PSNR Minimum: %11f (higher == better)\n",
                psnr_tol);
      }
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
    else if (stats1[band].stats_good && stats2[band].stats_good) {
      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      asfPrintStatus("\nNo differences found comparing\n  %s%s to\n  %s%s\n\n",
                    band_str1, inFile1, band_str2, inFile2);
      print_stats_results(inFile1, inFile2,
                          band_str1, band_str2,
                          &stats1[band], &stats2[band],
                          psnr[band]);
    }

    if (!stats1[band].stats_good || !stats2[band].stats_good) {
      char msg[1024];

      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      if (!stats1[band].stats_good) {
        sprintf(msg, "FAIL: %s%s image statistics missing or band is empty.\n", band_str1, inFile1);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }
      if (!stats2[band].stats_good) {
        sprintf(msg, "FAIL: %s%s image statistics missing or band is empty.\n", band_str2, inFile2);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
  } // For each band

  if (outputFP) FCLOSE(outputFP);
  free_band_names(&band_names1, num_extracted_bands1);
  free_band_names(&band_names2, num_extracted_bands2);
}

void diffErrOut(char *outputFile, char *err_msg)
{
  char msg[1024];
  FILE *outputFP = NULL;

  if (outputFile != NULL && strlen(outputFile) > 0) {
    outputFP = FOPEN(outputFile, "a");

    fprintf(outputFP, "\n-----------------------------------------------\n");

    sprintf(msg, "FAIL: %s\n", err_msg);
    fprintf(outputFP, msg);

    fprintf(outputFP, "-----------------------------------------------\n\n");

    if (outputFP) FCLOSE(outputFP);
  }
  else {
    asfPrintError("Invalid output file name (NULL or zero length)\n");
  }
}

// User must free the returned string
char *data_type2str(data_type_t data_type)
{
  char *retstr = (char*)CALLOC(64, sizeof(char));

  switch (data_type) {
    case BYTE:
      strcpy(retstr, "BYTE");
      break;
    case INTEGER16:
      strcpy(retstr, "INTEGER16");
      break;
    case INTEGER32:
      strcpy(retstr, "INTEGER32");
      break;
    case REAL32:
      strcpy(retstr, "REAL32");
      break;
    case REAL64:
      strcpy(retstr, "REAL64");
      break;
    case COMPLEX_BYTE:
      strcpy(retstr, "COMPLEX_BYTE");
      break;
    case COMPLEX_INTEGER16:
      strcpy(retstr, "COMPLEX_INTEGER16");
      break;
    case COMPLEX_INTEGER32:
      strcpy(retstr, "COMPLEX_INTEGER32");
      break;
    case COMPLEX_REAL32:
      strcpy(retstr, "COMPLEX_REAL32");
      break;
    case COMPLEX_REAL64:
      strcpy(retstr, "COMPLEX_REAL64");
      break;
    default:
      strcpy(retstr, "UNKNOWN");
      break;
  }

  return retstr;
}

void get_tiff_info_from_file(char *file, tiff_data_t *t)
{
  TIFF *tif;

  t->sample_format = MISSING_TIFF_DATA;
  t->bits_per_sample = MISSING_TIFF_DATA;
  t->planar_config = MISSING_TIFF_DATA;
  t->data_type = 0;
  t->num_bands = 0;
  t->is_scanline_format = 0;
  t->height = 0;
  t->width = 0;

  tif = XTIFFOpen(file, "rb");
  if (tif != NULL) {
    get_tiff_info(tif, t);
  }

  XTIFFClose(tif);
}

void get_tiff_info(TIFF *tif, tiff_data_t *t)
{
  get_tiff_data_config(tif,
                       &t->sample_format,
                       &t->bits_per_sample,
                       &t->planar_config,
                       &t->data_type,
                       &t->num_bands,
                       &t->is_scanline_format);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &t->height);
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &t->width);
  if (t->planar_config != PLANARCONFIG_CONTIG &&
      t->planar_config != PLANARCONFIG_SEPARATE &&
      t->num_bands == 1)
  {
    t->planar_config = PLANARCONFIG_CONTIG;
  }
}

void get_geotiff_keys(char *file, geotiff_data_t *g)
{
  TIFF *tif = XTIFFOpen(file, "rb");
  GTIF *gtif = NULL;

  // Init values to 'missing'
  g->gtif_data_exists = 0;
  g->GTcitation = NULL;   // Should be unallocated
  g->PCScitation = NULL;  // Should be unallocated

  // Read geotiff info
  if (tif != NULL) {
    gtif = GTIFNew(tif);
    if (gtif != NULL) {
      int count, read_count;
      int citation_length;
      int typeSize;
      tagtype_t citation_type;

      // Get citations
      citation_length = GTIFKeyInfo(gtif, GTCitationGeoKey, &typeSize, &citation_type);
      if (citation_length > 0) {
        g->GTcitation = (char*)MALLOC(citation_length * typeSize);
        GTIFKeyGet(gtif, GTCitationGeoKey, g->GTcitation, 0, citation_length);
      }
      else {
        g->GTcitation = NULL;
      }
      citation_length = GTIFKeyInfo(gtif, PCSCitationGeoKey, &typeSize, &citation_type);
      if (citation_length > 0) {
        g->PCScitation = (char*)MALLOC(citation_length * typeSize);
        GTIFKeyGet(gtif, PCSCitationGeoKey, g->PCScitation, 0, citation_length);
      }
      else {
        g->PCScitation = NULL;
      }
      if ((g->GTcitation != NULL && strlen(g->GTcitation) > 0) ||
           (g->PCScitation != NULL && strlen(g->PCScitation) > 0))
      {
        g->gtif_data_exists = 1;
      }

      // Get tie points and pixel scale
      (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_TIEPOINTS, &count, &g->tie_point);
      if (count >= 6) {
        g->gtif_data_exists = 1;
        g->tie_point_elements = count;
        g->num_tie_points = count / 6;
      }
      (gtif->gt_methods.get)(gtif->gt_tif, GTIFF_PIXELSCALE, &count, &g->pixel_scale);
      if (count >= 3) {
        g->gtif_data_exists = 1;
        g->pixel_scale_elements = count;
        g->num_pixel_scales = count / 3;
      }

      // Get model type, raster type, and linear units
      read_count = GTIFKeyGet (gtif, GTModelTypeGeoKey, &g->model_type, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      read_count = GTIFKeyGet (gtif, GTRasterTypeGeoKey, &g->raster_type, 0, 0);
      if (read_count >= 1) g->gtif_data_exists = 1;
      read_count = GTIFKeyGet (gtif, ProjLinearUnitsGeoKey, &g->linear_units, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;

      // Get UTM related info if it exists
      read_count = GTIFKeyGet(gtif, ProjectedCSTypeGeoKey, &g->pcs, 0, 1);
      if (read_count == 1 && PCS_2_UTM(g->pcs, &g->hemisphere, &g->datum, &g->pro_zone)) {
        g->gtif_data_exists = 1;
      }
      else {
        read_count = GTIFKeyGet(gtif, ProjectionGeoKey, &g->pcs, 0, 1);
        if (read_count == 1 && PCS_2_UTM(g->pcs, &g->hemisphere, &g->datum, &g->pro_zone)) {
          g->gtif_data_exists = 1;
        }
        else {
          g->hemisphere = '\0';
          g->datum = UNKNOWN_DATUM;
          g->pro_zone = MISSING_GTIF_DATA;
        }
      }

      // Get projection type (ProjCoordTransGeoKey) and other projection parameters
      read_count = GTIFKeyGet(gtif, ProjCoordTransGeoKey, &g->proj_coords_trans, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->proj_coords_trans = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet(gtif, GeographicTypeGeoKey, &g->geographic_datum, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->geographic_datum = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet(gtif, GeogGeodeticDatumGeoKey, &g->geodetic_datum, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->geodetic_datum = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet(gtif, ProjScaleAtNatOriginGeoKey, &g->scale_factor, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->scale_factor = MISSING_GTIF_DATA;

      // Get generic projection parameters (Note: projection type is defined by
      // the g->proj_coords_trans value)
      read_count = GTIFKeyGet (gtif, ProjFalseEastingGeoKey, &g->false_easting, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->false_easting = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjFalseNorthingGeoKey, &g->false_northing, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->false_northing = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjNatOriginLongGeoKey, &g->natLonOrigin, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->natLonOrigin = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjNatOriginLatGeoKey, &g->natLatOrigin, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->natLatOrigin = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjStdParallel1GeoKey, &g->stdParallel1, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->stdParallel1 = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjStdParallel2GeoKey, &g->stdParallel2, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->stdParallel2 = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjCenterLongGeoKey, &g->lonCenter, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->lonCenter = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjCenterLatGeoKey, &g->latCenter, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->latCenter = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjFalseOriginLongGeoKey, &g->falseOriginLon, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->falseOriginLon = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjFalseOriginLatGeoKey, &g->falseOriginLat, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->falseOriginLat = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjStraightVertPoleLongGeoKey, &g->lonPole, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->lonPole = MISSING_GTIF_DATA;
    }
  }
}

void diff_check_geotiff(char *outfile, geotiff_data_t *g1, geotiff_data_t *g2)
{
  int failed=0;
  int i;
  int num_tie_points, num_pixel_scales;
  char dummy_hem;
  datum_type_t dummy_datum;
  long dummy_zone;
  projection_type_t projection_type1, projection_type2;
  FILE *outputFP = NULL;

  // FIXME: This stuff needs to be projection type-specific since I have to use
  // lat/lon together with latLon2utm() and check geolocations in meters
  // (due to longitude convergence at the poles)

  // Determine projection types
  if (PCS_2_UTM(g1->pcs, &dummy_hem, &dummy_datum, &dummy_zone)) {
    projection_type1 = UNIVERSAL_TRANSVERSE_MERCATOR;
  }
  else {
    switch(g1->proj_coords_trans) {
      case CT_TransverseMercator:
      case CT_TransvMercator_Modified_Alaska:
      case CT_TransvMercator_SouthOriented:
        projection_type1 = UNIVERSAL_TRANSVERSE_MERCATOR;
        break;
      case CT_AlbersEqualArea:
        projection_type1 = ALBERS_EQUAL_AREA;
        break;
      case CT_LambertConfConic_1SP:
      case CT_LambertConfConic_2SP:
        projection_type1 = LAMBERT_CONFORMAL_CONIC;
        break;
      case CT_PolarStereographic:
        projection_type1 = POLAR_STEREOGRAPHIC;
        break;
      case CT_LambertAzimEqualArea:
        projection_type1 = LAMBERT_AZIMUTHAL_EQUAL_AREA;
        break;
      default:
        projection_type1 = MISSING_GTIF_DATA;
        break;
    }
  }
  if (PCS_2_UTM(g2->pcs, &dummy_hem, &dummy_datum, &dummy_zone)) {
    projection_type2 = UNIVERSAL_TRANSVERSE_MERCATOR;
  }
  else {
    switch(g2->proj_coords_trans) {
      case CT_TransverseMercator:
      case CT_TransvMercator_Modified_Alaska:
      case CT_TransvMercator_SouthOriented:
        projection_type2 = UNIVERSAL_TRANSVERSE_MERCATOR;
        break;
      case CT_AlbersEqualArea:
        projection_type2 = ALBERS_EQUAL_AREA;
        break;
      case CT_LambertConfConic_1SP:
      case CT_LambertConfConic_2SP:
        projection_type2 = LAMBERT_CONFORMAL_CONIC;
        break;
      case CT_PolarStereographic:
        projection_type2 = POLAR_STEREOGRAPHIC;
        break;
      case CT_LambertAzimEqualArea:
        projection_type2 = LAMBERT_AZIMUTHAL_EQUAL_AREA;
        break;
      default:
        projection_type2 = MISSING_GTIF_DATA;
        break;
    }
  }

  ///////////////// Check for failures /////////////////
  ////
  // Check tie points and pixel scale
  if (g1->tie_point_elements != g2->tie_point_elements ||
      g1->num_tie_points != g2->num_tie_points ||
      g1->pixel_scale_elements != g2->pixel_scale_elements ||
      g1->num_pixel_scales != g2->num_pixel_scales) {
    failed = 1;
  }
  num_tie_points = MIN(g1->num_tie_points, g2->num_tie_points);
  num_pixel_scales = MIN(g1->num_pixel_scales, g2->num_pixel_scales);
  for (i=0; i<num_tie_points; i++) {
    if (g1->tie_point[i] - g2->tie_point[i] > PROJ_LOC_DIFF_TOL_m) failed = 1;
  }
  for (i=0; i<num_pixel_scales; i++) {
    if (g1->pixel_scale[i] != g2->pixel_scale[i]) failed = 1;
  }

  // Perform comparison based on projection type
  if (!(g1->gtif_data_exists && g2->gtif_data_exists)) {
    failed=1;
  }
  switch (projection_type1) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      if (g1->pcs != g2->pcs                    ||
          projection_type1 != projection_type2  ||
          g1->scale_factor != g2->scale_factor)
        failed = 1;
      break;
    case ALBERS_EQUAL_AREA:
      if (g1->proj_coords_trans != g2->proj_coords_trans  ||
          g1->stdParallel1 != g2->stdParallel1            ||
          g1->stdParallel2 != g2->stdParallel2            ||
          g1->false_easting != g2->false_easting          ||
          g1->false_northing != g2->false_northing        ||
          g1->natLonOrigin != g2->natLonOrigin            ||
          g1->lonCenter != g2->lonCenter                  ||
          g1->natLatOrigin != g2->natLatOrigin)
        failed=1;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      if (g1->proj_coords_trans != g2->proj_coords_trans  ||
          g1->stdParallel1 != g2->stdParallel1            ||
          g1->stdParallel2 != g2->stdParallel2            ||
          g1->false_easting != g2->false_easting          ||
          g1->false_northing != g2->false_northing        ||
          g1->natLonOrigin != g2->natLonOrigin            ||
          g1->natLatOrigin != g2->natLatOrigin            ||
          g1->falseOriginLon != g2->falseOriginLon        ||
          g1->falseOriginLat != g2->falseOriginLat        ||
          g1->scale_factor != g2->scale_factor)
        failed=1;
      break;
    case POLAR_STEREOGRAPHIC:
      if (g1->proj_coords_trans != g2->proj_coords_trans  ||
          g1->natLatOrigin != g2->natLatOrigin            ||
          g1->lonPole != g2->lonPole                      ||
          g1->false_easting != g2->false_easting          ||
          g1->false_northing != g2->false_northing)
        failed=1;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      if (g1->proj_coords_trans != g2->proj_coords_trans  ||
          g1->false_easting != g2->false_easting          ||
          g1->false_northing != g2->false_northing        ||
          g1->lonCenter != g2->lonCenter                  ||
          g1->latCenter != g2->latCenter)
        failed=1;
      break;
    default:
      failed=1;
      break;
  }

  ///////////////// Report failures /////////////////
  ////
  // Report results if the comparisons failed
  outputFP = fopen(outfile, "a");
  if (outputFP == NULL) {
    // Failed to open output file
    asfPrintError("Cannot open output file for reporting geotiff file differences\n");
  }
  if (failed && outputFP != NULL) {
    char msg[1024];

    fprintf(outputFP, "\n-----------------------------------------------\n");
    asfPrintStatus("\n-----------------------------------------------\n");

    // Report results based on projection type
    if (!g1->gtif_data_exists) {
      sprintf(msg, "ERROR: GeoTIFF data not found in File1\n");
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    if (!g2->gtif_data_exists) {
      sprintf(msg, "ERROR: GeoTIFF data not found in File2\n");
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    if (projection_type1 != projection_type2) {
      char type_str1[256], type_str2[256];
      projection_type_2_str(projection_type1, type_str1);
      projection_type_2_str(projection_type2, type_str2);
      sprintf(msg, "Projection type found in File1\n  %s\nnot equal to projection type in File2\n  %s\n",
             type_str1, type_str2);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    if (g1->tie_point_elements != g2->tie_point_elements ||
        g1->tie_point_elements % 6 != 0 ||
        g2->tie_point_elements % 6 != 0) {
      sprintf(msg,"Input files have differing or invalid numbers of tie point elements:\n"
          "  File1: %d tie point elements\n"
          "  File2: %d tie point elements\n",
          g1->tie_point_elements, g2->tie_point_elements);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    if (g1->num_tie_points != g2->num_tie_points) {
      sprintf(msg,"Input files have differing numbers of tie points:\n"
          "  File1: %d tie points\n"
              "  File2: %d tie points\n",
          g1->num_tie_points, g2->num_tie_points);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    for (i=0; i<num_tie_points; i++) {
      if (g1->tie_point[i] - g2->tie_point[i] > PROJ_LOC_DIFF_TOL_m) {
        char tmp1[16], tmp2[16];
        sprintf(tmp1,"%f", g1->tie_point[i]);
        sprintf(tmp2,"%f", g2->tie_point[i]);
        sprintf(msg, "  Tie point element #%d differs:\n"
                "    File1: %s\n"
                "    File2: %s\n",
                i,
                g1->tie_point[i] != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                g2->tie_point[i] != MISSING_GTIF_DATA ? tmp2 : "MISSING");
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }
    }
    if (g1->pixel_scale_elements != g2->pixel_scale_elements ||
        g1->pixel_scale_elements % 3 != 0 ||
        g2->pixel_scale_elements % 3 != 0) {
      sprintf(msg,"Input files have differing or invalid numbers of pixel scale elements:\n"
          "  File1: %d pixel scale elements\n"
          "  File2: %d pixel scale elements\n",
          g1->pixel_scale_elements, g2->pixel_scale_elements);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    if (g1->num_pixel_scales != g2->num_pixel_scales) {
      sprintf(msg,"Input files have differing numbers of pixel scales:\n"
          "  File1: %d pixel scales\n"
          "  File2: %d pixel scales\n",
          g1->num_pixel_scales, g2->num_pixel_scales);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);
    }
    for (i=0; i<num_pixel_scales; i++) {
      if (g1->pixel_scale[i] - g2->pixel_scale[i] > PROJ_LOC_DIFF_TOL_m) {
        char tmp1[16], tmp2[16];
        sprintf(tmp1,"%f", g1->pixel_scale[i]);
        sprintf(tmp2,"%f", g2->pixel_scale[i]);
        sprintf(msg, "  Pixel scale element #%d differs:\n"
            "    File1: %s\n"
            "    File2: %s\n",
            i,
            g1->pixel_scale[i] != MISSING_GTIF_DATA ? tmp1 : "MISSING",
            g2->pixel_scale[i] != MISSING_GTIF_DATA ? tmp2 : "MISSING");
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }
    }

    if (g1->gtif_data_exists &&
        g2->gtif_data_exists &&
        projection_type1 == projection_type2)
    {
      switch (projection_type1) {
        case UNIVERSAL_TRANSVERSE_MERCATOR:
          if (g1->pcs != g2->pcs ||
              g1->scale_factor != g2->scale_factor) {
            sprintf(msg, "UTM Projections with differences found: \n\n");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
          }
          if (g1->pcs != g2->pcs) {
            char tmp1[16], tmp2[16];
            sprintf(tmp1,"%d", g1->pcs);
            sprintf(tmp2,"%d", g2->pcs);
            sprintf(msg, "  PCS value from ProjectedCSTypeGeoKey or ProjectionGeoKey differ:\n"
                "    File1: %s\n"
                "    File2: %s\n",
                g1->pcs != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                g2->pcs != MISSING_GTIF_DATA ? tmp2 : "MISSING");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
          }
          if (g1->scale_factor != g2->scale_factor) {
            char tmp1[16], tmp2[16];
            sprintf(tmp1,"%f", g1->scale_factor);
            sprintf(tmp2,"%f", g2->scale_factor);
            sprintf(msg, "  UTM scale factors differ:\n"
                "    File1: %s\n"
                "    File2: %s\n",
                g1->scale_factor != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                g2->scale_factor != MISSING_GTIF_DATA ? tmp2 : "MISSING");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
          }
          break;
        case ALBERS_EQUAL_AREA:
          if (g1->proj_coords_trans != g2->proj_coords_trans  ||
              g1->stdParallel1 != g2->stdParallel1            ||
              g1->stdParallel2 != g2->stdParallel2            ||
              g1->false_easting != g2->false_easting          ||
              g1->false_northing != g2->false_northing        ||
              g1->natLonOrigin != g2->natLonOrigin            ||
              g1->lonCenter != g2->lonCenter                  ||
              g1->natLatOrigin != g2->natLatOrigin) {
            sprintf(msg, "Albers Equal Area Projections with differences found: \n\n");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
            if (g1->proj_coords_trans != g2->proj_coords_trans) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%d", g1->proj_coords_trans);
              sprintf(tmp2,"%d", g2->proj_coords_trans);
              sprintf(msg, "  Projection type code from ProjCoordTransGeoKey differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->proj_coords_trans != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->proj_coords_trans != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->stdParallel1 != g2->stdParallel1) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->stdParallel1);
              sprintf(tmp2,"%f", g2->stdParallel1);
              sprintf(msg, "  First standard parallels differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->stdParallel1 != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->stdParallel1 != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->stdParallel2 != g2->stdParallel2) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->stdParallel2);
              sprintf(tmp2,"%f", g2->stdParallel2);
              sprintf(msg, "  Second standard parallels differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->stdParallel2 != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->stdParallel2 != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_easting != g2->false_easting) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_easting);
              sprintf(tmp2,"%f", g2->false_easting);
              sprintf(msg, "  False eastings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_easting != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_easting != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_northing != g2->false_northing) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_northing);
              sprintf(tmp2,"%f", g2->false_northing);
              sprintf(msg, "  False northings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_northing != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_northing != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->natLonOrigin != g2->natLonOrigin) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->natLonOrigin);
              sprintf(tmp2,"%f", g2->natLonOrigin);
              sprintf(msg, "  Central meridians (from ProjNatOriginLongGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->natLonOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->natLonOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->lonCenter != g2->lonCenter) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->lonCenter);
              sprintf(tmp2,"%f", g2->lonCenter);
              sprintf(msg, "  Central meridians (from ProjCenterLongGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->lonCenter != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->lonCenter != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->natLatOrigin != g2->natLatOrigin) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->natLatOrigin);
              sprintf(tmp2,"%f", g2->natLatOrigin);
              sprintf(msg, "  Latitudes of Origin (from ProjNatOriginLatGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->natLatOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->natLatOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
          }
          break;
        case LAMBERT_CONFORMAL_CONIC:
          if (g1->proj_coords_trans != g2->proj_coords_trans  ||
              g1->stdParallel1 != g2->stdParallel1          ||
              g1->stdParallel2 != g2->stdParallel2          ||
              g1->false_easting != g2->false_easting        ||
              g1->false_northing != g2->false_northing      ||
              g1->natLonOrigin != g2->natLonOrigin          ||
              g1->natLatOrigin != g2->natLatOrigin          ||
              g1->falseOriginLon != g2->falseOriginLon      ||
              g1->falseOriginLat != g2->falseOriginLat      ||
              g1->scale_factor != g2->scale_factor) {
            sprintf(msg, "Lambert Conformal Conic Projections with differences found: \n\n");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
            if (g1->proj_coords_trans != g2->proj_coords_trans) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%d", g1->proj_coords_trans);
              sprintf(tmp2,"%d", g2->proj_coords_trans);
              sprintf(msg, "  Projection type code from ProjCoordTransGeoKey differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->proj_coords_trans != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->proj_coords_trans != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->stdParallel1 != g2->stdParallel1) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->stdParallel1);
              sprintf(tmp2,"%f", g2->stdParallel1);
              sprintf(msg, "  First standard parallels differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->stdParallel1 != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->stdParallel1 != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->stdParallel2 != g2->stdParallel2) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->stdParallel2);
              sprintf(tmp2,"%f", g2->stdParallel2);
              sprintf(msg, "  Second standard parallels differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->stdParallel2 != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->stdParallel2 != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_easting != g2->false_easting) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_easting);
              sprintf(tmp2,"%f", g2->false_easting);
              sprintf(msg, "  False eastings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_easting != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_easting != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_northing != g2->false_northing) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_northing);
              sprintf(tmp2,"%f", g2->false_northing);
              sprintf(msg, "  False northings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_northing != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_northing != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->natLonOrigin != g2->natLonOrigin) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->natLonOrigin);
              sprintf(tmp2,"%f", g2->natLonOrigin);
              sprintf(msg, "  Longitudes of Origin (from ProjNatOriginLongGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->natLonOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->natLonOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->natLatOrigin != g2->natLatOrigin) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->natLatOrigin);
              sprintf(tmp2,"%f", g2->natLatOrigin);
              sprintf(msg, "  Latitudes of Origin (from ProjNatOriginLatGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->natLatOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->natLatOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->falseOriginLon != g2->falseOriginLon) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->falseOriginLon);
              sprintf(tmp2,"%f", g2->falseOriginLon);
              sprintf(msg, "  Longitudes of Origin (from ProjFalseOriginLongGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->falseOriginLon != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->falseOriginLon != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->falseOriginLat != g2->falseOriginLat) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->falseOriginLat);
              sprintf(tmp2,"%f", g2->falseOriginLat);
              sprintf(msg, "  Latitudes of Origin (from ProjFalseOriginLatGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->falseOriginLat != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->falseOriginLat != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
          }
          break;
        case POLAR_STEREOGRAPHIC:
          if (g1->proj_coords_trans != g2->proj_coords_trans  ||
              g1->natLatOrigin != g2->natLatOrigin          ||
              g1->lonPole != g2->lonPole                    ||
              g1->false_easting != g2->false_easting        ||
              g1->false_northing != g2->false_northing) {
            sprintf(msg, "Polar Stereographic Projections with differences found: \n\n");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
            if (g1->proj_coords_trans != g2->proj_coords_trans) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%d", g1->proj_coords_trans);
              sprintf(tmp2,"%d", g2->proj_coords_trans);
              sprintf(msg, "  Projection type code from ProjCoordTransGeoKey differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->proj_coords_trans != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->proj_coords_trans != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->natLatOrigin != g2->natLatOrigin) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->natLatOrigin);
              sprintf(tmp2,"%f", g2->natLatOrigin);
              sprintf(msg, "  Latitudes of Origin (from ProjNatOriginLatGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->natLatOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->natLatOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->lonPole != g2->lonPole) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->lonPole);
              sprintf(tmp2,"%f", g2->lonPole);
              sprintf(msg, "  Longitudes of Straight Vertical Pole (from ProjStraightVertPoleLongGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->lonPole != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->lonPole != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_easting != g2->false_easting) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_easting);
              sprintf(tmp2,"%f", g2->false_easting);
              sprintf(msg, "  False eastings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_easting != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_easting != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_northing != g2->false_northing) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_northing);
              sprintf(tmp2,"%f", g2->false_northing);
              sprintf(msg, "  False northings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_northing != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_northing != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
          }
          break;
        case LAMBERT_AZIMUTHAL_EQUAL_AREA:
          if (g1->proj_coords_trans != g2->proj_coords_trans  ||
              g1->false_easting != g2->false_easting        ||
              g1->false_northing != g2->false_northing      ||
              g1->lonCenter != g2->lonCenter                ||
              g1->latCenter != g2->latCenter) {
            failed=1;
            sprintf(msg, "Lambert Azimuthal Equal Area Projections with differences found: \n\n");
            fprintf(outputFP, msg);
            asfPrintStatus(msg);
            if (g1->proj_coords_trans != g2->proj_coords_trans) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%d", g1->proj_coords_trans);
              sprintf(tmp2,"%d", g2->proj_coords_trans);
              sprintf(msg, "  Projection type code from ProjCoordTransGeoKey differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->proj_coords_trans != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->proj_coords_trans != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_easting != g2->false_easting) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_easting);
              sprintf(tmp2,"%f", g2->false_easting);
              sprintf(msg, "  False eastings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_easting != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_easting != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->false_northing != g2->false_northing) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->false_northing);
              sprintf(tmp2,"%f", g2->false_northing);
              sprintf(msg, "  False northings differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->false_northing != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->false_northing != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->lonCenter != g2->lonCenter) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->lonCenter);
              sprintf(tmp2,"%f", g2->lonCenter);
              sprintf(msg, "  Central meridians (from ProjCenterLongGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->lonCenter != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->lonCenter != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
            if (g1->latCenter != g2->latCenter) {
              char tmp1[16], tmp2[16];
              sprintf(tmp1,"%f", g1->latCenter);
              sprintf(tmp2,"%f", g2->latCenter);
              sprintf(msg, "  Latitudes of Origin (from ProjCenterLatGeoKey) differ:\n"
                  "    File1: %s\n"
                      "    File2: %s\n",
                  g1->latCenter != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                  g2->latCenter != MISSING_GTIF_DATA ? tmp2 : "MISSING");
              fprintf(outputFP, msg);
              asfPrintStatus(msg);
            }
          }
          break;
        default:
          // Should never reach this code
          sprintf(msg, "Found unsupported or missing projection type\n\n");
          fprintf(outputFP, msg);
          asfPrintStatus(msg);
          break;
      }
    }

    fprintf(outputFP, "-----------------------------------------------\n\n");
    asfPrintStatus("-----------------------------------------------\n\n");
  }

  if (failed && outputFP != NULL) {
    if (outputFP) FCLOSE(outputFP);
  }
}

void projection_type_2_str(projection_type_t proj, char *proj_str)
{
  switch (proj) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      strcpy(proj_str, "UTM");
      break;
    case ALBERS_EQUAL_AREA:
      strcpy(proj_str, "Albers Equal Area");
      break;
    case LAMBERT_CONFORMAL_CONIC:
      strcpy(proj_str, "Lambert Conformal Conic");
      break;
    case POLAR_STEREOGRAPHIC:
      strcpy(proj_str, "Polar Stereographic");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      strcpy(proj_str, "Lambert Azimuthal Equal Area");
      break;
    default:
      strcpy(proj_str, "Unknown");
      break;
  }
}

int tiff_image_band_statistics_from_file(char *inFile, int band_no,
                                         int *stats_exist,
                                         double *min, double *max,
                                         double *mean, double *sdev, double *rmse,
                                         int use_mask_value, float mask_value)
{
  tiff_data_t t;
  tsize_t scanlineSize;

  *stats_exist = 1; // Innocent until presumed guilty...
  get_tiff_info_from_file(inFile, &t);
  // Note: For single-plane (greyscale) images, planar_config will remain unset so
  // we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0)
  {
    *stats_exist = 0;
    return 1;
  }

  // Minimum and maximum sample values as integers.
  double fmin = FLT_MAX;
  double fmax = -FLT_MAX;
  double cs; // Current sample value

  *mean = 0.0;
  double s = 0.0;

  uint32 sample_count = 0;      // Samples considered so far.
  uint32 ii, jj;
  TIFF *tif = XTIFFOpen(inFile, "rb");
  if (tif == NULL) {
    *stats_exist = 0;
    return 1;
  }
  scanlineSize = TIFFScanlineSize(tif);
  if (scanlineSize <= 0) {
    if (tif) XTIFFClose(tif);
    *stats_exist = 0;
    return 1;
  }
  if (t.num_bands > 1 &&
      t.planar_config != PLANARCONFIG_CONTIG &&
      t.planar_config != PLANARCONFIG_SEPARATE)
  {
    if (tif) XTIFFClose(tif);
    *stats_exist = 0;
    return 1;
  }
  float *buf = (float*)MALLOC(t.width * sizeof(float));

  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all rows in the TIFF
    for ( ii = 0; ii < t.height; ii++ )
    {
      // Planar configuration is chunky, e.g. interlaced pixels, rgb rgb etc.
      asfPercentMeter((double)ii/(double)t.height);
      tiff_get_float_line(tif, buf, ii, band_no);
      for (jj = 0 ; jj < t.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
          continue;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    for ( ii = 0; ii < t.height; ii++ )
    {
      asfPercentMeter((double)ii/(double)t.height);
      tiff_get_float_line(tif, buf, ii, band_no);
      for (jj = 0 ; jj < t.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }

  // Verify the new extrema have been found.
  //if (fmin == FLT_MAX || fmax == -FLT_MAX)
  if (gsl_fcmp (fmin, FLT_MAX, 0.00000000001) == 0 ||
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0)
  {
    if (buf) free(buf);
    if (tif) XTIFFClose(tif);
    *stats_exist = 0;
    return 1;
  }

  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  *rmse = *sdev; // This assumes the sample count is large enough to ensure that the sample standard
                 // deviation is very very similar to the population standard deviation.

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX)
  {
    if (buf) free(buf);
    if (tif) XTIFFClose(tif);
    *stats_exist = 0;
    return 1;
  }

  if (buf) free(buf);
  if (tif) XTIFFClose(tif);
  return 0;
}

void tiff_image_band_psnr_from_files(char *inFile1, char *inFile2,
                                     int band1, int band2,
                                     double *psnr)
{
  double cs1, cs2;
  tiff_data_t t1, t2;
  tsize_t scanlineSize1, scanlineSize2;

  get_tiff_info_from_file(inFile1, &t1);
  if (t1.sample_format == MISSING_TIFF_DATA ||
      t1.bits_per_sample == MISSING_TIFF_DATA ||
      t1.planar_config == MISSING_TIFF_DATA ||
      t1.data_type == 0 ||
      t1.num_bands == MISSING_TIFF_DATA ||
      t1.is_scanline_format == MISSING_TIFF_DATA ||
      t1.height == 0 ||
      t1.width == 0)
  {
    asfPrintWarning("Missing TIFF header values in %s\n", inFile1);
    *psnr = MISSING_PSNR;
    return;
  }
  get_tiff_info_from_file(inFile2, &t2);
  if (t2.sample_format == MISSING_TIFF_DATA ||
      t2.bits_per_sample == MISSING_TIFF_DATA ||
      t2.planar_config == MISSING_TIFF_DATA ||
      t2.data_type == 0 ||
      t2.num_bands == MISSING_TIFF_DATA ||
      t2.is_scanline_format == MISSING_TIFF_DATA ||
      t2.height == 0 ||
      t2.width == 0)
  {
    asfPrintWarning("Missing TIFF header values in %s\n", inFile2);
    *psnr = MISSING_PSNR;
    return;
  }

  // Calculate the peak signal to noise ratio (PSNR) between the two images
  double sse;
  double rmse;
  int ii, jj;
  long height = MIN(t1.height, t2.height);
  long width = MIN(t1.width, t2.width);
  long pixel_count = 0;
  float max_val;
  char **band_names1, **band_names2;
  int num_extracted_names1=0;
  int num_extracted_names2=0;

  if (band1 < 0 || band1 > t1.num_bands - 1 ||
      band2 < 0 || band2 > t2.num_bands - 1) {
    asfPrintWarning("Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)\n",
                    band1, t1.num_bands, band2, t2.num_bands);
    *psnr = MISSING_PSNR;
    return;
  }
  if (t1.height != t2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. %d).\n"
        "PSNR calculation will only use the first %d rows from each file.\n",
        t1.height, t2.height, height);
  }
  if (t1.width != t2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row (%d v. %d).\n"
        "PSNR calculation will only use the first %d pixels from each row of data.\n",
        t1.width, t2.width, width);
  }

  if (t1.data_type != t2.data_type) {
    asfPrintWarning("TIFF files have different data types.\n");
    *psnr = MISSING_PSNR;
    return;
  }

  get_band_names(inFile1, NULL, &band_names1, &num_extracted_names1);
  get_band_names(inFile2, NULL, &band_names2, &num_extracted_names2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);

  TIFF *tif1 = XTIFFOpen(inFile1, "rb");
  if (tif1 == NULL) {
    asfPrintWarning("Cannot open %s\n", inFile1);
    *psnr = MISSING_PSNR;
    return;
  }
  scanlineSize1 = TIFFScanlineSize(tif1);
  if (scanlineSize1 <= 0) {
    asfPrintWarning("Invalid scanline size (%d)\n", scanlineSize1);
    if (tif1) XTIFFClose(tif1);
    *psnr = MISSING_PSNR;
    return;
  }
  if (t1.num_bands > 1 &&
      t1.planar_config != PLANARCONFIG_CONTIG &&
      t1.planar_config != PLANARCONFIG_SEPARATE)
  {
    asfPrintWarning("Invalid planar configuration in %s\n", inFile1);
    if (tif1) XTIFFClose(tif1);
    *psnr = MISSING_PSNR;
    return;
  }
  TIFF *tif2 = XTIFFOpen(inFile2, "rb");
  if (tif2 == NULL) {
    asfPrintWarning("Cannot open %s\n", inFile2);
    *psnr = MISSING_PSNR;
    return;
  }
  scanlineSize2 = TIFFScanlineSize(tif2);
  if (scanlineSize2 <= 0) {
    asfPrintWarning("Invalid scanline size (%d)\n", scanlineSize2);
    if (tif2) XTIFFClose(tif2);
    *psnr = MISSING_PSNR;
    return;
  }
  if (t2.num_bands > 1 &&
      t2.planar_config != PLANARCONFIG_CONTIG &&
      t2.planar_config != PLANARCONFIG_SEPARATE)
  {
    asfPrintWarning("Invalid planar configuration in %s\n", inFile2);
    if (tif2) XTIFFClose(tif2);
    *psnr = MISSING_PSNR;
    return;
  }
  float *buf1 = (float*)CALLOC(t1.width, sizeof(float));
  float *buf2 = (float*)CALLOC(t2.width, sizeof(float));
  if (buf1 == NULL || buf2 == NULL) {
    asfPrintWarning("Cannot allocate memory for TIFF data buffers.\n");
    *psnr = MISSING_PSNR;
    if (buf1) FREE(buf1);
    if (buf2) FREE(buf2);
    return;
  }

  sse = 0.0;
  max_val = get_maxval(t1.data_type); // Since both file's data types are the same, this is OK
  for (ii=0; ii<height; ++ii) {
    asfPercentMeter((double)ii/(double)height);
    tiff_get_float_line(tif1, buf1, ii, band1);
    tiff_get_float_line(tif2, buf2, ii, band2);
    for (jj=0; jj<width; ++jj) {
      cs1 = buf1[jj];
      cs2 = buf2[jj];
      sse += (cs1 - cs2) * (cs1 - cs2);
      pixel_count++;
    }
  }
  asfPercentMeter(1.0);
  if (pixel_count > 0 && max_val > 0) {
    rmse = sqrt(sse/pixel_count);
    *psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
  }
  else {
    *psnr = MISSING_PSNR;
  }

  free_band_names(&band_names1, num_extracted_names1);
  free_band_names(&band_names2, num_extracted_names2);
  XTIFFClose(tif1);
  XTIFFClose(tif2);
  if (buf1) FREE(buf1);
  if (buf2) FREE(buf2);
}

void tiff_get_float_line(TIFF *tif, float *buf, int row, int band_no)
{
  tiff_data_t t;

  get_tiff_info(tif, &t);
  // Note: For single-plane (greyscale) images, planar_config may remain unset so
  // we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0)
  {
    asfPrintError("Cannot read tif file\n");
  }

  // Read a scanline
  tsize_t scanlineSize = TIFFScanlineSize(tif);
  tdata_t *tif_buf = _TIFFmalloc(scanlineSize);
  if (t.planar_config == PLANARCONFIG_CONTIG || t.num_bands == 1) {
    TIFFReadScanline(tif, tif_buf, row, 0);
  }
  else {
    // Planar configuration is band-sequential
    TIFFReadScanline(tif, tif_buf, row, band_no);
  }

  int col;
  for (col=0; col<t.width; col++) {
    switch(t.bits_per_sample) {
      case 8:
        switch(t.sample_format) {
          case SAMPLEFORMAT_UINT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((uint8*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint8*)(tif_buf))[col]);
            }
            break;
          case SAMPLEFORMAT_INT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((int8*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((int8*)(tif_buf))[col]);   // Current sample.
            }
            break;
          default:
            // There is no such thing as an IEEE 8-bit floating point
            if (tif_buf) _TIFFfree(tif_buf);
            if (tif) XTIFFClose(tif);
            asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
            break;
        }
        break;
      case 16:
        switch(t.sample_format) {
          case SAMPLEFORMAT_UINT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((uint16*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint16*)(tif_buf))[col]);   // Current sample.
            }
            break;
          case SAMPLEFORMAT_INT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((int16*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint16*)(tif_buf))[col]);   // Current sample.
            }
            break;
          default:
            // There is no such thing as an IEEE 16-bit floating point
            if (tif_buf) _TIFFfree(tif_buf);
            if (tif) XTIFFClose(tif);
            asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
            break;
        }
        break;
      case 32:
        switch(t.sample_format) {
          case SAMPLEFORMAT_UINT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((uint32*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((uint32*)(tif_buf))[col]);   // Current sample.
            }
            break;
          case SAMPLEFORMAT_INT:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((long*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((long*)(tif_buf))[col]);   // Current sample.
            }
            break;
          case SAMPLEFORMAT_IEEEFP:
            if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
              buf[col] = (float)(((float*)(tif_buf))[(col*t.num_bands)+band_no]);   // Current sample.
            }
            else {
              // Planar configuration is band-sequential or single-banded
              buf[col] = (float)(((float*)(tif_buf))[col]);   // Current sample.
            }
            break;
          default:
            if (tif_buf) _TIFFfree(tif_buf);
            if (tif) XTIFFClose(tif);
            asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
            break;
        }
        break;
      default:
        if (tif_buf) _TIFFfree(tif_buf);
        if (tif) XTIFFClose(tif);
        asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF file.\n");
        break;
    }
  }
  if (tif_buf) {
    _TIFFfree(tif_buf);
  }
}

float tiff_image_get_float_pixel(TIFF *tif, int row, int col, int band_no)
{
  tiff_data_t t;
  float cs;

  get_tiff_info(tif, &t);
  // Note: For single-plane (greyscale) images, planar_config may remain unset so
  // we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0)
  {
    asfPrintError("Cannot read tif file\n");
  }

  // Get a float line from the file
  float *buf = (float*)MALLOC(t.width*sizeof(float));
  tiff_get_float_line(tif, buf, row, band_no);
  cs = buf[col];
  FREE(buf);

  // Return as float
  return cs;
}

void get_png_info_hdr_from_file(char *inFile, png_info_t *ihdr, char *outfile)
{
  png_structp png_ptr;
  png_infop info_ptr;
  png_uint_32  width, height;
  int bit_depth, color_type, nbands, interlace_type, compression_type, filter_type;
  unsigned char sig[8];
  char msg[1024];
  FILE *pngFP, *outputFP;

  // Init stuff
  pngFP = (FILE*)FOPEN(inFile,"rb");
  outputFP = (FILE*)FOPEN(outfile, "a");
  ihdr->width = 0;
  ihdr->height = 0;
  ihdr->bit_depth = MISSING_PNG_DATA;
  ihdr->color_type = MISSING_PNG_DATA;
  strcpy(ihdr->color_type_str, "UNKNOWN");
  ihdr->interlace_type = MISSING_PNG_DATA;
  ihdr->compression_type = MISSING_PNG_DATA;
  ihdr->filter_type = MISSING_PNG_DATA;
  ihdr->data_type = 0;
  ihdr->num_bands = 0;

  // Initialize PNG file for read
  if (png_access_version_number() < 10000 ||
      png_access_version_number() > 20000) {
    sprintf(msg, "Invalid PNG file version (%0.4f) found.  Only version 1.xxxx is supported\n",
            (float)png_access_version_number()/10000.0);
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  fread(sig, 1, 8, pngFP); // Important: Leaves file pointer offset into file by 8 bytes for png lib
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n", "file type header bytes invalid");
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_ptr = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_init_io(png_ptr, pngFP);
  png_set_sig_bytes(png_ptr, 8); // Because of the sig-reading offset ...must do this for PNG lib

  // Read info and IHDR
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, &compression_type, &filter_type);
  nbands = (int)png_get_channels(png_ptr, info_ptr);

  // Preliminary error checking on returned values (make sure results are valid)
  if (bit_depth != 1 &&
      bit_depth != 2 &&
      bit_depth != 4 &&
      bit_depth != 8 &&
      bit_depth != 16)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file bit depth found (%d).\n"
        "Must be 1, 2, 4, 8, or 16.\n", bit_depth);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (color_type != PNG_COLOR_TYPE_GRAY &&
      color_type != PNG_COLOR_TYPE_GRAY_ALPHA &&
      color_type != PNG_COLOR_TYPE_PALETTE &&
      color_type != PNG_COLOR_TYPE_RGB &&
      color_type != PNG_COLOR_TYPE_RGB_ALPHA &&
      color_type != PNG_COLOR_MASK_PALETTE &&
      color_type != PNG_COLOR_MASK_COLOR &&
      color_type != PNG_COLOR_MASK_ALPHA)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file color type found.\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (filter_type != PNG_FILTER_TYPE_BASE &&
      filter_type != PNG_INTRAPIXEL_DIFFERENCING)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file filter type found.\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (compression_type != PNG_COMPRESSION_TYPE_BASE) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file compression type found.\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (interlace_type != PNG_INTERLACE_NONE &&
      interlace_type != PNG_INTERLACE_ADAM7)
  {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file interlace type found.\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  // Populate return struct
  ihdr->width = (uint32)width;
  ihdr->height = (uint32)height;
  ihdr->bit_depth = bit_depth;
  ihdr->color_type = color_type;
  strcpy(ihdr->color_type_str,
         color_type == PNG_COLOR_TYPE_GRAY ? "GRAY" :
         color_type == PNG_COLOR_TYPE_GRAY_ALPHA ? "GRAY+A" :
         color_type == PNG_COLOR_TYPE_PALETTE ? "PALETTE" :
         color_type == PNG_COLOR_TYPE_RGB ? "RGB" :
         color_type == PNG_COLOR_TYPE_RGB_ALPHA ? "RGBA" :
         color_type == PNG_COLOR_MASK_PALETTE ? "COLOR PALETTE MASK" :
         color_type == PNG_COLOR_MASK_COLOR ? "COLOR MASK" :
         color_type == PNG_COLOR_MASK_ALPHA ? "ALPHA MASK" :
         "UNKNOWN");
  ihdr->interlace_type = interlace_type;
  ihdr->compression_type = compression_type;
  ihdr->filter_type = filter_type;
  switch (bit_depth) {
    case 1:
    case 2:
    case 4:
      ihdr->data_type=0; // UNKNOWN TYPE
      break;
    case 8:
      ihdr->data_type=BYTE;
      break;
    case 16:
      ihdr->data_type=INTEGER16;
      break;
    default:
      ihdr->data_type=0; // UNKNOWN TYPE
      break;
  }
  ihdr->num_bands = nbands;

  // Clean up and return
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  if (pngFP) FCLOSE(pngFP);
  if (outputFP) FCLOSE(outputFP);
}

void calc_png_stats_2files(char *inFile1, char *inFile2, char *outfile,
                           stats_t *inFile1_stats, stats_t *inFile2_stats,
                           double *psnr, int band)
{
  stats_t *s1 = inFile1_stats;
  stats_t *s2 = inFile2_stats;

  // Init stats
  s1->stats_good = s2->stats_good = 0;
  s1->min = s2->min = 0.0;
  s1->max = s2->max = 0.0;
  s1->mean = s2->mean = 0.0;
  s1->sdev = s2->sdev = 0.0;
  s1->rmse = s2->rmse = 0.0;
  s1->hist = s2->hist = NULL;
  s1->hist_pdf = s2->hist_pdf = NULL;
  *psnr = MISSING_PSNR;

  png_image_band_statistics_from_file(inFile1, outfile, band, &s1->stats_good,
                                      &s1->min, &s1->max,
                                      &s1->mean, &s1->sdev, &s1->rmse,
                                      0, UINT8_IMAGE_DEFAULT_MASK);
  png_image_band_statistics_from_file(inFile2, outfile, band, &s2->stats_good,
                                      &s2->min, &s2->max,
                                      &s2->mean, &s2->sdev, &s2->rmse,
                                      0, UINT8_IMAGE_DEFAULT_MASK);
  png_image_band_psnr_from_files(inFile1, inFile2, outfile, band, band, psnr);
}

int png_image_band_statistics_from_file(char *inFile, char *outfile,
                                        int band_no,
                                        int *stats_exist,
                                        double *min, double *max,
                                        double *mean, double *sdev, double *rmse,
                                        int use_mask_value, float mask_value)
{
  png_structp png_ptr;
  png_infop info_ptr;
  png_uint_32  width, height;
  int bit_depth, color_type, interlace_type, compression_type, filter_type;
  unsigned char sig[8];
  char msg[1024];
  FILE *pngFP, *outputFP;
  png_info_t ihdr;

  *stats_exist = 1; // Innocent until presumed guilty...
  get_png_info_hdr_from_file(inFile, &ihdr, outfile);
  if (ihdr.bit_depth == MISSING_PNG_DATA ||
      ihdr.color_type == MISSING_PNG_DATA ||
      (ihdr.color_type_str == NULL || strlen(ihdr.color_type_str) <= 0) ||
      ihdr.interlace_type == MISSING_PNG_DATA ||
      ihdr.compression_type == MISSING_PNG_DATA ||
      ihdr.filter_type == MISSING_PNG_DATA ||
      ihdr.data_type == 0 ||
      ihdr.num_bands == MISSING_PNG_DATA ||
      ihdr.height == 0 ||
      ihdr.width == 0)
  {
    *stats_exist = 0;
    return 1;
  }

  // Initialize PNG file for read
  pngFP = (FILE*)FOPEN(inFile,"rb");
  outputFP = (FILE*)FOPEN(outfile, "a");
  fread(sig, 1, 8, pngFP); // Important: Leaves file pointer offset into file by 8 bytes for png lib
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n", "file type header bytes invalid");
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_ptr = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_init_io(png_ptr, pngFP);
  png_set_sig_bytes(png_ptr, 8); // Because of the sig-reading offset ...must do this for PNG lib

  // Read info and IHDR
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, &compression_type, &filter_type);

  // Minimum and maximum sample values as integers.
  double fmin = FLT_MAX;
  double fmax = -FLT_MAX;
  double cs; // Current sample value

  *mean = 0.0;
  double s = 0.0;

  uint32 sample_count = 0;      // Samples considered so far.
  uint32 ii, jj;
  float *buf = (float*)MALLOC(ihdr.width * sizeof(float));

  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all rows in the TIFF
    for ( ii = 0; ii < ihdr.height; ii++ )
    {
      // Planar configuration is chunky, e.g. interlaced pixels, rgb rgb etc.
      asfPercentMeter((double)ii/(double)ihdr.height);
      png_sequential_get_float_line(png_ptr, info_ptr, buf, band_no);
      for (jj = 0 ; jj < ihdr.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
          continue;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    for ( ii = 0; ii < ihdr.height; ii++ )
    {
      asfPercentMeter((double)ii/(double)ihdr.height);
      png_sequential_get_float_line(png_ptr, info_ptr, buf, band_no);
      for (jj = 0 ; jj < ihdr.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }

  // Verify the new extrema have been found.
  //if (fmin == FLT_MAX || fmax == -FLT_MAX)
  if (gsl_fcmp (fmin, FLT_MAX, 0.00000000001) == 0 ||
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0)
  {
    if (buf) free(buf);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    *stats_exist = 0;
    return 1;
  }

  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  *rmse = *sdev; // This assumes the sample count is large enough to ensure that the sample standard
                 // deviation is very very similar to the population standard deviation.

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX)
  {
    if (buf) free(buf);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP) FCLOSE(pngFP);
    if (outputFP) FCLOSE(outputFP);
    *stats_exist = 0;
    return 1;
  }

  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  if (buf) free(buf);
  if (outputFP) FCLOSE(outputFP);
  if (pngFP) FCLOSE(pngFP);
  return 0;
}

void png_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                     int band1, int band2,
                                     double *psnr)
{
  double cs1, cs2;
  png_structp png_ptr1 = NULL, png_ptr2 = NULL;
  png_infop info_ptr1 = NULL, info_ptr2 = NULL;
  png_uint_32 png_width, png_height;
  int bit_depth, color_type, interlace_type, compression_type, filter_type;
  int num_names_extracted1=0, num_names_extracted2=0;
  unsigned char sig[8];
  char msg[1024];
  FILE *pngFP1, *pngFP2, *outputFP;
  png_info_t ihdr1, ihdr2;

  outputFP = (FILE*)FOPEN(outfile, "a");

  get_png_info_hdr_from_file(inFile1, &ihdr1, outfile);
  if (ihdr1.bit_depth == MISSING_PNG_DATA ||
      ihdr1.color_type == MISSING_PNG_DATA ||
      (ihdr1.color_type_str == NULL || strlen(ihdr1.color_type_str) <= 0) ||
      ihdr1.interlace_type == MISSING_PNG_DATA ||
      ihdr1.compression_type == MISSING_PNG_DATA ||
      ihdr1.filter_type == MISSING_PNG_DATA ||
      ihdr1.data_type == 0 ||
      ihdr1.num_bands == MISSING_PNG_DATA ||
      ihdr1.height == 0 ||
      ihdr1.width == 0)
  {
    sprintf(msg, "Cannot read PNG file header in file1\n  %s\n", inFile1);
    if (outputFP) fprintf(outputFP, msg);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }
  get_png_info_hdr_from_file(inFile2, &ihdr2, outfile);
  if (ihdr2.bit_depth == MISSING_PNG_DATA ||
      ihdr2.color_type == MISSING_PNG_DATA ||
      (ihdr2.color_type_str == NULL || strlen(ihdr2.color_type_str) <= 0) ||
      ihdr2.interlace_type == MISSING_PNG_DATA ||
      ihdr2.compression_type == MISSING_PNG_DATA ||
      ihdr2.filter_type == MISSING_PNG_DATA ||
      ihdr2.data_type == 0 ||
      ihdr2.num_bands == MISSING_PNG_DATA ||
      ihdr2.height == 0 ||
      ihdr2.width == 0)
  {
    sprintf(msg, "Cannot read PNG file header in file2\n  %s\n", inFile2);
    if (outputFP) fprintf(outputFP, msg);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }

  // Initialize PNG file for read
  pngFP1 = (FILE*)FOPEN(inFile1,"rb");
  fread(sig, 1, 8, pngFP1); // Important: Leaves file pointer offset into file by 8 bytes for png lib
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n  %s\n", "file type header bytes invalid", inFile1);
    fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  pngFP2 = (FILE*)FOPEN(inFile2,"rb");
  fread(sig, 1, 8, pngFP2); // Important: Leaves file pointer offset into file by 8 bytes for png lib
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n  %s\n", "file type header bytes invalid", inFile2);
    fprintf(outputFP, msg);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  png_ptr1 = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr1) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr1 = png_create_info_struct(png_ptr1);
  if (!info_ptr1) {
    png_destroy_read_struct(&png_ptr1, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_ptr2 = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr2) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr2 = png_create_info_struct(png_ptr2);
  if (!info_ptr2) {
    png_destroy_read_struct(&png_ptr2, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, msg);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  if (setjmp(png_jmpbuf(png_ptr1))) {
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  png_init_io(png_ptr1, pngFP1);
  png_set_sig_bytes(png_ptr1, 8); // Because of the sig-reading offset ...must do this for PNG lib
  png_init_io(png_ptr2, pngFP2);
  png_set_sig_bytes(png_ptr2, 8); // Because of the sig-reading offset ...must do this for PNG lib

  // Read info and IHDR
  png_read_info(png_ptr1, info_ptr1);
  png_get_IHDR(png_ptr1, info_ptr1, &png_width, &png_height, &bit_depth, &color_type,
               &interlace_type, &compression_type, &filter_type);
  png_read_info(png_ptr2, info_ptr2);
  png_get_IHDR(png_ptr2, info_ptr2, &png_width, &png_height, &bit_depth, &color_type,
               &interlace_type, &compression_type, &filter_type);

  // Calculate the peak signal to noise ratio (PSNR) between the two images
  double sse;
  double rmse;
  int ii, jj;
  long height = MIN(ihdr1.height, ihdr2.height);
  long width = MIN(ihdr1.width, ihdr2.width);
  long pixel_count = 0;
  float max_val;
  if (band1 < 0 || band1 > ihdr1.num_bands - 1 ||
      band2 < 0 || band2 > ihdr2.num_bands - 1) {
    sprintf(msg,"Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)\n",
                    band1, ihdr1.num_bands, band2, ihdr2.num_bands);
    asfPrintWarning(msg);
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }
  if (ihdr1.height != ihdr2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. %d).\n"
         "PSNR calculation will only use the first %d rows from each file.\n",
         ihdr1.height, ihdr2.height, height);
  }
  if (ihdr1.width != ihdr2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row (%d v. %d).\n"
            "PSNR calculation will only use the first %d pixels from each row of data.\n",
            ihdr1.width, ihdr2.width, width);
  }

  if (ihdr1.data_type != ihdr2.data_type) {
    sprintf(msg,"PNG files have differing data types.\n");
    asfPrintWarning(msg);
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }

  char **band_names1, **band_names2;
  get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
  get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);
  float *buf1 = (float*)CALLOC(ihdr1.width, sizeof(float));
  float *buf2 = (float*)CALLOC(ihdr2.width, sizeof(float));
  if (buf1 == NULL || buf2 == NULL) {
    asfPrintWarning("Cannot allocate memory for PNG data buffers.\n");
    *psnr = MISSING_PSNR;
    if (buf1) FREE(buf1);
    if (buf2) FREE(buf2);
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (outputFP) FCLOSE(outputFP);
    return;
  }

  sse = 0.0;
  max_val = get_maxval(ihdr1.data_type); // Since both file's data types are the same, this is OK
  for (ii=0; ii<height; ++ii) {
    asfPercentMeter((double)ii/(double)height);
    png_sequential_get_float_line(png_ptr1, info_ptr1, buf1, band1);
    png_sequential_get_float_line(png_ptr2, info_ptr2, buf2, band2);
    for (jj=0; jj<width; ++jj) {
      cs1 = buf1[jj];
      cs2 = buf2[jj];
      sse += (cs1 - cs2) * (cs1 - cs2);
      pixel_count++;
    }
  }
  asfPercentMeter(1.0);
  if (pixel_count > 0 && max_val > 0) {
    rmse = sqrt(sse/pixel_count);
    *psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
  }
  else {
    *psnr = MISSING_PSNR;
  }

  free_band_names(&band_names1, num_names_extracted1);
  free_band_names(&band_names2, num_names_extracted2);
  if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
  if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
  if (outputFP != NULL) fprintf(outputFP, msg);
  if (pngFP1) FCLOSE(pngFP1);
  if (pngFP2) FCLOSE(pngFP2);
  if (outputFP) FCLOSE(outputFP);
  if (buf1) FREE(buf1);
  if (buf2) FREE(buf2);
}

// NOTE: No row offset provided because png_ptr acts like a file pointer ...just read all the rows
// in sequence.
void png_sequential_get_float_line(png_structp png_ptr, png_infop info_ptr, float *buf, int band)
{
  int num_bands = (int)png_get_channels(png_ptr, info_ptr);
  if (band < 0 || band > num_bands - 1) {
    asfPrintError("png_get_float_line(): bad band number (band %d, bands %d through %d available)\n",
                  band, 0, num_bands - 1);
  }
  png_uint_32 width, height;
  int bit_depth, color_type;
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, NULL, NULL, NULL);
  if (bit_depth != 8) {
    asfPrintError("png_get_float_line(): PNG image has unsupported bit depth (%d).  Bit depth of 8-bits supported.\n",
                  bit_depth);
  }
  if (color_type != PNG_COLOR_TYPE_GRAY &&
      color_type != PNG_COLOR_TYPE_RGB) {
    asfPrintError("png_get_float_line(): PNG image must be RGB or greyscale.  Alpha band, palette-color,\n"
        "and mask-type images not supported.\n");
  }

  int rgb = color_type == PNG_COLOR_TYPE_RGB ? 1 : 0;
  png_bytep png_buf = (png_bytep)MALLOC(width*sizeof(png_byte)*(rgb ? 3 : 1));
  int col;

  // Read current row
  png_read_row(png_ptr, png_buf, NULL);
  for (col=0; col<width; col++) {
    if (rgb)
      buf[col] = (float)png_buf[col*3+band];
    else
      buf[col] = (float)png_buf[col];
  }
}

void get_ppm_pgm_info_hdr_from_file(char *inFile, ppm_pgm_info_t *pgm, char *outfile)
{
  int ch, i;
  unsigned char tmp[1024]; // For reading width, height, max_val only
  int max_chars   = 1024; // Length of tmp for loop termination when reading sequential uchars
  FILE *fp = (FILE*)FOPEN(inFile, "rb");

  strcpy(pgm->magic,"");
  pgm->width=MISSING_PPM_PGM_DATA;
  pgm->height=MISSING_PPM_PGM_DATA;
  pgm->max_val=MISSING_PPM_PGM_DATA;
  pgm->ascii_data=1; // Default to unsupported type ...ASCII data separated by whitespace
  pgm->img_offset = MISSING_PSNR; // Will result in a failed fseek()
  pgm->bit_depth=0;
  pgm->data_type=0;
  pgm->num_bands=0;

  //// Read magic number that identifies
  FREAD(pgm->magic, sizeof(unsigned char), 2, fp);
  if (pgm->magic[0] == 'P' && (pgm->magic[1] == '5' || pgm->magic[1] == '6'))
  {
    pgm->ascii_data = 0;
    pgm->img_offset = 2; // Just past magic number
  }
  else {
    // Shouldn't be possible to be here, but what the hey...
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): Found invalid or unsupported (ASCII data?) PPM/PGM file header.\n");
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  //// Determine number of channels (3 for rgb, 1 for gray)
  if (pgm->magic[1] == '6') {
    pgm->num_bands = 3; // RGB PPM file
  }
  else if (pgm->magic[1] == '5') {
    pgm->num_bands = 1; // Grayscale PGM file
  }
  else {
    // Shouldn't be able to reach this code
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): Found invalid or unsupported (ASCII data?) PPM/PGM file header...\n");
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  //// Read image width (in pixels)
  // Move past white space
  ch = fgetc(fp);
  pgm->img_offset++;
  while (isspace(ch) && ch != EOF) {
    ch = fgetc(fp); // Move past white space
    pgm->img_offset++;
  }
  i = 0;
  while (!isspace(ch) && i < max_chars && ch != EOF) {
    tmp[i++] = ch;
    ch = fgetc(fp);
    pgm->img_offset++;
  }
  tmp[i]='\0';
  if (i == max_chars || ch == EOF) {
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): '%s' field in file header too long.\n",
            "width");
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }
  pgm->width = atoi(tmp);

  //// Read image height (in rows)
  while (isspace(ch) && ch != EOF) {
    ch = fgetc(fp); // Move past white space
    pgm->img_offset++;
  }
  i = 0;
  while (!isspace(ch) && i < max_chars && ch != EOF) {
    tmp[i++] = ch;
    ch = fgetc(fp);
    pgm->img_offset++;
  }
  tmp[i]='\0';
  if (i == max_chars || ch == EOF) {
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): '%s' field in file header too long.\n",
            "height");
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }
  pgm->height = atoi(tmp);

  //// Read max-allowed pixel value
  while (isspace(ch) && ch != EOF) {
    ch = fgetc(fp); // Move past white space
    pgm->img_offset++;
  }
  i = 0;
  while (!isspace(ch) && i < max_chars && ch != EOF) {
    tmp[i++] = ch;
    ch = fgetc(fp);
    pgm->img_offset++;
  }
  tmp[i]='\0';
  if (i == max_chars || ch == EOF) {
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): '%s' field in file header too long.\n",
            "max. pixel value");
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }
  pgm->max_val = atoi(tmp);

  //// Move past white space to beginning of image data to set pgm->img_offset for an fseek()
  while (isspace(ch) && ch != EOF) {
    ch = fgetc(fp); // Move past white space
    pgm->img_offset++;
  }
  pgm->img_offset--;
  if (ch == EOF) {
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "No image data found in PNG file\n  %s\n",
            inFile);
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  //// Determine byte-boundary bit-depth and ASF data type for holding
  //// the PPM/PGM file's image data
  if (powf(2.0, 32.0) > pgm->max_val) {
    pgm->bit_depth = 8;
    pgm->data_type = BYTE;
  }
  else if (powf(2.0, 16.0) > pgm->max_val) {
    pgm->bit_depth = 16;
    pgm->data_type = INTEGER16;
  }
  else if (powf(2.0, 8.0) > pgm->max_val) {
    pgm->bit_depth = 32;
    pgm->data_type = INTEGER32;
  }
  else {
    FILE *outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "Invalid max. pixel value found in PNG file (%d)\n  %s\n",
            pgm->max_val,
            inFile);
    fprintf(outFP, msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  if (fp) FCLOSE(fp);
}

void get_band_names(char *inFile, FILE *outputFP,
                    char ***band_names, int *num_extracted_bands)
{
  char msg[1024];
  char type_str[255];
  graphics_file_t type = getGraphicsFileType(inFile);
  graphicsFileType_toStr(type, type_str);
  if (type == ASF_IMG) {
    // Grab band names from the metadata
    meta_parameters *md;
    int band_count=1;
    char *f, *c;
    char inFile_meta[1024];
    if (inFile != NULL && strlen(inFile) > 0) {
      f = STRDUP(inFile);
      c = findExt(f);
      *c = '\0';
      sprintf(inFile_meta, "%s.meta", f);
      if (fileExists(inFile_meta)) {
        md = meta_read(inFile_meta);
      }
      FREE(f);
    }
    band_count = (md != NULL) ? md->general->band_count : 1;
    *num_extracted_bands = band_count;

    if (band_count > 0 && md != NULL) {
      *band_names = extract_band_names(md->general->bands, md->general->band_count);
      if (*band_names == NULL) {
        // free_band_names() will assume MAX_BANDS have been made available if
        // no bands have been extracted otherwise, so ...sigh, allocate MAX_BANDS
        // here ...they'll be freed unused.
        *num_extracted_bands=0;
        *band_names = (char**)MALLOC(MAX_BANDS*sizeof(char*));
        if (*band_names != NULL) {
          int i;
          for (i=0; i<MAX_BANDS; i++) {
            (*band_names)[i] = (char*)CALLOC(64, sizeof(char));
            if ((*band_names)[i] == NULL) {
              sprintf(msg, "Cannot allocate memory for band name array.\n");
              if (outputFP != NULL) fprintf(outputFP, msg);
              asfPrintError(msg);
            }
          }
        }
        else {
          sprintf(msg, "Cannot allocate memory for band name array.\n");
          if (outputFP != NULL) fprintf(outputFP, msg);
          asfPrintError(msg);
        }
      }
    }
  }
  else {
    // Other file types...
    //
    // Allocate memory for the band names
    *band_names = (char**)MALLOC(MAX_BANDS*sizeof(char*));
    if (*band_names != NULL) {
      int i;
      for (i=0; i<MAX_BANDS; i++) {
        (*band_names)[i] = (char*)CALLOC(64, sizeof(char));
        if ((*band_names)[i] == NULL) {
          sprintf(msg, "Cannot allocate memory for band name array.\n");
          if (outputFP != NULL) fprintf(outputFP, msg);
          asfPrintError(msg);
        }
      }
    }
    else {
      sprintf(msg, "Cannot allocate memory for band name array.\n");
      if (outputFP != NULL) fprintf(outputFP, msg);
      asfPrintError(msg);
    }

    if (type == GEO_TIFF) {
      // If band names are embedded in the GeoTIFF, then grab them.  Otherwise
      // assign numeric band names
      geotiff_data_t g;
      int *empty = (int*)CALLOC(MAX_BANDS, sizeof(int));
      char *band_str = (char*)CALLOC(25, sizeof(char));
      if (band_str == NULL) {
        sprintf(msg, "Cannot allocate memory for band name string.\n");
        if (outputFP != NULL) fprintf(outputFP, msg);
        asfPrintError(msg);
      }

      get_geotiff_keys(inFile, &g);
      if (g.gtif_data_exists && g.GTcitation != NULL && strlen(g.GTcitation) > 0) {
        char *tmp_citation = STRDUP(g.GTcitation);
        get_bands_from_citation(num_extracted_bands, &band_str, empty, tmp_citation);
        FREE(tmp_citation);
      }
      else if (g.gtif_data_exists && g.PCScitation != NULL && strlen(g.PCScitation) > 0) {
        char *tmp_citation = STRDUP(g.PCScitation);
        get_bands_from_citation(num_extracted_bands, &band_str, empty, tmp_citation);
        FREE(tmp_citation);
      }
      if (*num_extracted_bands <= 0) {
        // Could not find band strings in the citations, so assign numeric band IDs
        int i;
        for (i=0; i<MAX_BANDS; i++) {
          sprintf((*band_names)[i], "%02d", i);
        }
      }
      else {
        // Extract the band names from the band names string
        //
        // extract_band_names() allocated memory, so better free it first...
        int i;
        for (i=0; i<MAX_BANDS; i++) {
          FREE((*band_names)[i]);
        }
        FREE(*band_names);
        *band_names = extract_band_names(band_str, *num_extracted_bands);
      }

      FREE(empty);
      FREE(band_str);
      if (g.GTcitation)FREE(g.GTcitation);
      if (g.PCScitation)FREE(g.PCScitation);
    }
    else {
      // For non ASF IMG and GeoTIFF images, just use numeric band id's
      int i;
      for (i=0; i<MAX_BANDS; i++) {
        sprintf((*band_names)[i], "%02d", i);
      }
    }
  }
}

void ppm_pgm_get_float_line(FILE *fp, float *buf, int row, ppm_pgm_info_t *pgm, int band_no)
{
  if (band_no < 0 || band_no > pgm->num_bands - 1) {
    asfPrintError("Invalid band number (%d).  Valid: 0 through %d (only)\n",
                  band_no, pgm->num_bands - 1);
  }
  unsigned char *pgm_buf = (unsigned char*)MALLOC(pgm->num_bands * pgm->width * sizeof(unsigned char));
  if (pgm_buf == NULL) {
    asfPrintError("Cannot allocate memory for PPM/PGM scanline buffer\n");
  }

  FREAD(pgm_buf, sizeof(unsigned char), pgm->width * pgm->num_bands, fp);

  int col;
  for (col=0; col<pgm->width; col++) {
    buf[col] = (float)(((unsigned char*)pgm_buf)[(col*pgm->num_bands)+band_no]);
  }
  if (pgm_buf)FREE(pgm_buf);
}

int ppm_pgm_image_band_statistics_from_file(char *inFile, char *outfile,
                                             int band_no, int *stats_exist,
                                             double *min, double *max,
                                             double *mean, double *sdev, double *rmse,
                                             int use_mask_value, float mask_value)
{
  ppm_pgm_info_t pgm;

  *stats_exist = 1; // Innocent until presumed guilty...
  get_ppm_pgm_info_hdr_from_file(inFile, &pgm, outfile);
  if (pgm.magic == NULL || strlen(pgm.magic) == 0 ||
      pgm.width <= 0 ||
      pgm.height <= 0 ||
      pgm.max_val <= 0 ||
      pgm.img_offset < 0 ||
      pgm.bit_depth != 8 ||
      pgm.data_type != BYTE ||
      pgm.num_bands <= 0)
  {
    *stats_exist = 0;
    return 1;
  }

  // Minimum and maximum sample values as integers.
  double fmin = FLT_MAX;
  double fmax = -FLT_MAX;
  double cs; // Current sample value

  *mean = 0.0;
  double s = 0.0;

  uint32 sample_count = 0;      // Samples considered so far.
  uint32 ii, jj;
  FILE *fp = FOPEN(inFile, "rb");
  if (fp == NULL) {
    *stats_exist = 0;
    return 1;
  }
  float *buf = (float*)MALLOC(pgm.width * sizeof(float));
  if (buf == NULL) {
    *stats_exist = 0;
    return 1;
  }

  // If there is a mask value we are supposed to ignore,
  FSEEK64(fp, (long long)pgm.img_offset, SEEK_SET);
  if ( use_mask_value ) {
    // iterate over all rows in the PPM or PGM file
    for ( ii = 0; ii < pgm.height; ii++ )
    {
      asfPercentMeter((double)ii/(double)pgm.height);
      ppm_pgm_get_float_line(fp, buf, ii, &pgm, band_no);
      for (jj = 0 ; jj < pgm.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
          continue;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    for ( ii = 0; ii < pgm.height; ii++ )
    {
      asfPercentMeter((double)ii/(double)pgm.height);
      ppm_pgm_get_float_line(fp, buf, ii, &pgm, band_no);
      for (jj = 0 ; jj < pgm.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }

  // Verify the new extrema have been found.
  //if (fmin == FLT_MAX || fmax == -FLT_MAX)
  if (gsl_fcmp (fmin, FLT_MAX, 0.00000000001) == 0 ||
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0)
  {
    if (buf) free(buf);
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return 1;
  }

  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  *rmse = *sdev; // This assumes the sample count is large enough to ensure that the sample standard
                 // deviation is very very similar to the population standard deviation.

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX)
  {
    if (buf) free(buf);
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return 1;
  }

  if (buf) free(buf);
  if (fp) FCLOSE(fp);
  return 0;
}

void ppm_pgm_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                        int band1, int band2, double *psnr)
{
  ppm_pgm_info_t pgm1, pgm2; // Header info
  double cs1, cs2;
  char msg[1024];
  FILE *fp1, *fp2, *outputFP;

  outputFP = (FILE*)FOPEN(outfile, "a");

  // Get header info and fseek() to beginning of image data
  get_ppm_pgm_info_hdr_from_file(inFile1, &pgm1, outfile);
  if (pgm1.magic == NULL || strlen(pgm1.magic) == 0 ||
      pgm1.width <= 0 ||
      pgm1.height <= 0 ||
      pgm1.max_val <= 0 ||
      pgm1.img_offset < 0 ||
      pgm1.bit_depth != 8 ||
      pgm1.data_type != BYTE ||
      pgm1.num_bands <= 0)
  {
    sprintf(msg, "Cannot read PPM/PNG file header in file1\n  %s\n", inFile1);
    if (outputFP) fprintf(outputFP, msg);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }
  get_ppm_pgm_info_hdr_from_file(inFile2, &pgm2, outfile);
  if (pgm2.magic == NULL || strlen(pgm2.magic) == 0 ||
      pgm2.width <= 0 ||
      pgm2.height <= 0 ||
      pgm2.max_val <= 0 ||
      pgm2.img_offset < 0 ||
      pgm2.bit_depth != 8 ||
      pgm2.data_type != BYTE ||
      pgm2.num_bands <= 0)
  {
    sprintf(msg, "Cannot read PPM/PNG file header in file2\n  %s\n", inFile2);
    if (outputFP) fprintf(outputFP, msg);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }

  // Calculate the peak signal to noise ratio (PSNR) between the two images
  double sse;
  double rmse;
  int ii, jj;
  long height = MIN(pgm1.height, pgm2.height);
  long width = MIN(pgm1.width, pgm2.width);
  long pixel_count = 0;
  float max_val;
  if (band1 < 0 || band1 > pgm1.num_bands - 1 ||
      band2 < 0 || band2 > pgm2.num_bands - 1) {
    sprintf(msg,"Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)\n",
            band1, pgm1.num_bands, band2, pgm2.num_bands);
    asfPrintWarning(msg);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }
  if (pgm1.height != pgm2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. %d).\n"
        "PSNR calculation will only use the first %d rows from each file.\n",
        pgm1.height, pgm2.height, height);
  }
  if (pgm1.width != pgm2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row (%d v. %d).\n"
        "PSNR calculation will only use the first %d pixels from each row of data.\n",
        pgm1.width, pgm2.width, width);
  }

  if (pgm1.data_type != pgm2.data_type) {
    sprintf(msg,"PPM/PGM files have differing data types.\n");
    asfPrintWarning(msg);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outputFP) FCLOSE(outputFP);
    *psnr = MISSING_PSNR;
    return;
  }

  char **band_names1, **band_names2;
  int num_names_extracted1=0, num_names_extracted2=0;
  get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
  get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);
  float *buf1 = (float*)CALLOC(pgm1.width, sizeof(float));
  float *buf2 = (float*)CALLOC(pgm2.width, sizeof(float));
  if (buf1 == NULL || buf2 == NULL) {
    asfPrintWarning("Cannot allocate memory for PNG data buffers.\n");
    *psnr = MISSING_PSNR;
    if (buf1) FREE(buf1);
    if (buf2) FREE(buf2);
    if (outputFP != NULL) fprintf(outputFP, msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outputFP) FCLOSE(outputFP);
    return;
  }

  sse = 0.0;
  max_val = get_maxval(pgm1.data_type); // Since both file's data types are the same, this is OK
  fp1 = (FILE*)FOPEN(inFile1, "rb");
  fp2 = (FILE*)FOPEN(inFile2, "rb");
  FSEEK64(fp1, (long long)pgm1.img_offset, SEEK_SET);
  FSEEK64(fp2, (long long)pgm2.img_offset, SEEK_SET);
  for (ii=0; ii<height; ++ii) {
    asfPercentMeter((double)ii/(double)height);
    // Get float lines
    ppm_pgm_get_float_line(fp1, buf1, ii, &pgm1, band1);
    ppm_pgm_get_float_line(fp2, buf2, ii, &pgm2, band2);
    for (jj=0; jj<width; ++jj) {
      cs1 = buf1[jj];
      cs2 = buf2[jj];
      sse += (cs1 - cs2) * (cs1 - cs2);
      pixel_count++;
    }
  }
  asfPercentMeter(1.0);
  if (pixel_count > 0 && max_val > 0) {
    rmse = sqrt(sse/pixel_count);
    *psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
  }
  else {
    *psnr = MISSING_PSNR;
  }

  if (fp1) FCLOSE(fp1);
  if (fp2) FCLOSE(fp2);
  if (outputFP) FCLOSE(outputFP);
  if (buf1) FREE(buf1);
  if (buf2) FREE(buf2);
  free_band_names(&band_names1, num_names_extracted1);
  free_band_names(&band_names2, num_names_extracted2);
}

void free_band_names(char ***band_names, int num_extracted_bands)
{
  if (*band_names != NULL) {
    int i;
    if (num_extracted_bands > 0) {
      for (i=0; i<num_extracted_bands; i++) {
        if ((*band_names)[i] != NULL) FREE((*band_names)[i]);
      }
    }
    else {
      for (i=0; i<MAX_BANDS; i++) {
        if ((*band_names)[i] != NULL) FREE((*band_names)[i]);
      }
    }
    FREE(*band_names);
  }
}

METHODDEF(void) jpeg_err_exit(j_common_ptr cinfo)
{
  jpeg_err_hdlr jpeg_err = (jpeg_err_hdlr) cinfo->err;

  (*cinfo->err->output_message) (cinfo);

  longjmp (jpeg_err->setjmp_buffer, 1);
}

GLOBAL(void) get_jpeg_info_hdr_from_file(char *inFile, jpeg_info_t *jpg, char *outputFile)
{
  char msg[1024];
  struct jpeg_decompress_struct cinfo;
  jpeg_error_hdlr_t jerr;
  FILE *fp, *outFP;

  // Init
  jpg->width = 0;
  jpg->height = 0;
  jpg->data_type = 0;
  jpg->num_bands = 0;

  // Try file open
  fp = (FILE*)FOPEN(inFile, "rb");
  if (fp == NULL) {
    sprintf(msg,"Cannot open JPEG file:\n  %s\n", inFile);
    outFP = (FILE*)FOPEN(outputFile,"a");
    if (outFP) fprintf(outFP, msg);
    asfPrintError(msg);
  }

  // Setup jpeg lib error handler
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = jpeg_err_exit; // Error hook to catch jpeg lib errors
  if (setjmp(jerr.setjmp_buffer)) {
    // JPEG lib error occurred
    jpeg_destroy_decompress (&cinfo);
    if (fp) FCLOSE(fp);
    sprintf(msg,"JPEG library critical error ...Aborting\n");
    outFP = (FILE*)FOPEN(outputFile,"a");
    if (outFP) fprintf(outFP, msg);
    asfPrintError(msg);
  }

  // Init jpeg lib to read
  jpeg_create_decompress(&cinfo); // Init decompression object
  jpeg_stdio_src(&cinfo, fp); // Set input to open inFile

  // Read jpeg file header
  jpeg_read_header(&cinfo, TRUE);

  // Populate the header struct
  jpg->width = cinfo.image_width * cinfo.num_components;
  jpg->height = cinfo.image_height;
  jpg->data_type = BYTE;
  jpg->num_bands = cinfo.num_components;

  // Finish up
  jpeg_destroy_decompress(&cinfo);
  if (fp) FCLOSE(fp);
}

GLOBAL(void) jpeg_image_band_statistics_from_file(char *inFile, char *outfile,
                                         int band_no, int *stats_exist,
                                         double *min, double *max,
                                         double *mean, double *sdev, double *rmse,
                                         int use_mask_value, float mask_value)
{
  jpeg_info_t jpg;
  char msg[1024];
  struct jpeg_decompress_struct cinfo;
  FILE *outFP;
  jpeg_error_hdlr_t jerr;
  JSAMPARRAY jpg_buf;

  *stats_exist = 1; // Innocent until presumed guilty...
  get_jpeg_info_hdr_from_file(inFile, &jpg, outfile);
  if (jpg.width <= 0 ||
      jpg.height <= 0 ||
      jpg.data_type != BYTE ||
      jpg.num_bands <= 0)
  {
    *stats_exist = 0;
    return;
  }

  // Minimum and maximum sample values as integers.
  double fmin = FLT_MAX;
  double fmax = -FLT_MAX;
  double cs; // Current sample value

  *mean = 0.0;
  double s = 0.0;

  uint32 sample_count = 0;      // Samples considered so far.
  uint32 jj;
  FILE *fp = FOPEN(inFile, "rb");
  if (fp == NULL) {
    *stats_exist = 0;
    return;
  }

  // Setup jpeg lib error handler
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = jpeg_err_exit; // Error hook to catch jpeg lib errors
  if (setjmp(jerr.setjmp_buffer)) {
    // JPEG lib error occurred
    jpeg_destroy_decompress (&cinfo);
    if (fp) FCLOSE(fp);
    sprintf(msg,"JPEG library critical error ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) fprintf(outFP, msg);
    asfPrintError(msg);
  }

  // Init jpeg lib to read
  jpeg_create_decompress(&cinfo); // Init decompression object
  jpeg_stdio_src(&cinfo, fp); // Set input to open inFile

  // Read jpeg file header
  jpeg_read_header(&cinfo, TRUE);
  jpeg_start_decompress(&cinfo);

  // Allocate buffer for reading jpeg
  jpg_buf = (*cinfo.mem->alloc_sarray) (
      (j_common_ptr)&cinfo, JPOOL_IMAGE, jpg.width, 1);

  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all rows in the PPM or PGM file
    while (cinfo.output_scanline < cinfo.image_height)
    {
      asfPercentMeter((double)cinfo.output_scanline/(double)jpg.height);
      jpeg_read_scanlines(&cinfo, jpg_buf, 1); // Reads one scanline
      for (jj = 0 ; jj < jpg.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        if (jpg.num_bands == 1) {
          cs = (float)jpg_buf[0][jj];
        }
        else if (jpg.num_bands == 3) {
          cs = (float)jpg_buf[0][jj*3+band_no];
        }
        else {
          // Shouldn't get here...
          *stats_exist = 0;
          return;
        }
        if ( !isnan(mask_value) && (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
          continue;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }
  else {
    // There is no mask value to ignore, so we do the same as the
    // above loop, but without the possible continue statement.
    while (cinfo.output_scanline < cinfo.image_height)
    {
      asfPercentMeter((double)cinfo.output_scanline/(double)jpg.height);
      jpeg_read_scanlines(&cinfo, jpg_buf, 1); // Reads one scanline
      for (jj = 0 ; jj < jpg.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        if (jpg.num_bands == 1) {
          cs = (float)jpg_buf[0][jj];
        }
        else if (jpg.num_bands == 3) {
          cs = (float)jpg_buf[0][jj*3+band_no];
        }
        else {
          // Shouldn't get here...
          *stats_exist = 0;
          return;
        }
        if ( G_UNLIKELY (cs < fmin) ) { fmin = cs; }
        if ( G_UNLIKELY (cs > fmax) ) { fmax = cs; }
        double old_mean = *mean;
        *mean += (cs - *mean) / (sample_count + 1);
        s += (cs - old_mean) * (cs - *mean);
        sample_count++;
      }
    }
    asfPercentMeter(1.0);
  }

  // Verify the new extrema have been found.
  if (gsl_fcmp (fmin, FLT_MAX, 0.00000000001) == 0 ||
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0)
  {
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return;
  }

  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  *rmse = *sdev; // This assumes the sample count is large enough to ensure that the sample standard
                 // deviation is very very similar to the population standard deviation.

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX)
  {
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return;
  }

  // Finish up
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  if (fp) FCLOSE(fp);
  return;
}

GLOBAL(void) jpeg_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                     int band1, int band2, double *psnr)
{
  jpeg_info_t jpg1, jpg2;
  char msg[1024];
  struct jpeg_decompress_struct cinfo1, cinfo2;
  FILE *outFP=NULL, *fp1=NULL, *fp2=NULL;
  jpeg_error_hdlr_t jerr1, jerr2;
  JSAMPARRAY jpg_buf1, jpg_buf2;
  float cs1, cs2;

  fp1 = (FILE*)FOPEN(inFile1, "rb");
  fp2 = (FILE*)FOPEN(inFile2, "rb");
  if (fp1 == NULL) {
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) fprintf(outFP, "Cannot open file: %s\n", inFile1);
    if (outFP) FCLOSE(outFP);
    *psnr = MISSING_PSNR;
    return;
  }
  if (fp2 == NULL) {
    if (outFP) fprintf(outFP, "Cannot open file: %s\n", inFile2);
    if (outFP) FCLOSE(outFP);
    *psnr = MISSING_PSNR;
    return;
  }

  get_jpeg_info_hdr_from_file(inFile1, &jpg1, outfile);
  if (jpg1.width <= 0 ||
      jpg1.height <= 0 ||
      jpg1.data_type != BYTE ||
      jpg1.num_bands <= 0)
  {
    *psnr = MISSING_PSNR;
    return;
  }
  get_jpeg_info_hdr_from_file(inFile2, &jpg2, outfile);
  if (jpg2.width <= 0 ||
      jpg2.height <= 0 ||
      jpg2.data_type != BYTE ||
      jpg2.num_bands <= 0)
  {
    *psnr = MISSING_PSNR;
    return;
  }

  // Setup jpeg lib error handlers
  cinfo1.err = jpeg_std_error(&jerr1.pub);
  jerr1.pub.error_exit = jpeg_err_exit; // Error hook to catch jpeg lib errors
  if (setjmp(jerr1.setjmp_buffer)) {
    // JPEG lib error occurred
    jpeg_destroy_decompress (&cinfo1);
    jpeg_destroy_decompress (&cinfo2);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    sprintf(msg,"JPEG library critical error ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) fprintf(outFP, msg);
    asfPrintError(msg);
  }
  cinfo2.err = jpeg_std_error(&jerr2.pub);
  jerr2.pub.error_exit = jpeg_err_exit; // Error hook to catch jpeg lib errors
  if (setjmp(jerr2.setjmp_buffer)) {
    // JPEG lib error occurred
    jpeg_destroy_decompress (&cinfo1);
    jpeg_destroy_decompress (&cinfo2);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    sprintf(msg,"JPEG library critical error ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) fprintf(outFP, msg);
    asfPrintError(msg);
  }

  // Init jpeg lib to read files
  jpeg_create_decompress(&cinfo1); // Init decompression object
  jpeg_stdio_src(&cinfo1, fp1); // Set input to open inFile
  jpeg_create_decompress(&cinfo2); // Init decompression object
  jpeg_stdio_src(&cinfo2, fp2); // Set input to open inFile

  // Read jpeg file headers
  jpeg_read_header(&cinfo1, TRUE);
  jpeg_start_decompress(&cinfo1);
  jpeg_read_header(&cinfo2, TRUE);
  jpeg_start_decompress(&cinfo2);

  // Allocate buffers for reading jpeg
  jpg_buf1 = (*cinfo1.mem->alloc_sarray) (
      (j_common_ptr)&cinfo1, JPOOL_IMAGE, jpg1.width, 1);
  jpg_buf2 = (*cinfo2.mem->alloc_sarray) (
      (j_common_ptr)&cinfo2, JPOOL_IMAGE, jpg2.width, 1);

  // Calculate the peak signal to noise ratio (PSNR) between the two images
  double sse;
  double rmse;
  int jj;
  long height = MIN(jpg1.height, jpg2.height);
  long width = MIN(jpg1.width, jpg2.width);
  long pixel_count = 0;
  float max_val;
  if (band1 < 0 || band1 > jpg1.num_bands - 1 ||
      band2 < 0 || band2 > jpg2.num_bands - 1) {
    sprintf(msg,"Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)\n",
            band1, jpg1.num_bands, band2, jpg2.num_bands);
    asfPrintWarning(msg);
    if (outFP != NULL) fprintf(outFP, msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outFP) FCLOSE(outFP);
    *psnr = MISSING_PSNR;
    return;
  }
  if (jpg1.height != jpg2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. %d).\n"
        "PSNR calculation will only use the first %d rows from each file.\n",
    jpg1.height, jpg2.height, height);
  }
  if (jpg1.width != jpg2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row (%d v. %d).\n"
        "PSNR calculation will only use the first %d pixels from each row of data.\n",
    jpg1.width, jpg2.width, width);
  }

  if (jpg1.data_type != jpg2.data_type) {
    sprintf(msg,"PPM/PGM files have differing data types.\n");
    asfPrintWarning(msg);
    if (outFP != NULL) fprintf(outFP, msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outFP) FCLOSE(outFP);
    *psnr = MISSING_PSNR;
    return;
  }

  char **band_names1, **band_names2;
  int num_names_extracted1=0, num_names_extracted2=0;
  get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
  get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);

  sse = 0.0;
  max_val = get_maxval(jpg1.data_type); // Since both file's data types are the same, this is OK
  while (cinfo1.output_scanline < height &&
         cinfo2.output_scanline < height)
  {
    asfPercentMeter((double)cinfo1.output_scanline/(double)jpg1.height);
    jpeg_read_scanlines(&cinfo1, jpg_buf1, 1); // Reads one scanline
    jpeg_read_scanlines(&cinfo2, jpg_buf2, 1); // Reads one scanline
    for (jj=0; jj<width; ++jj) {
      cs1 = (float)jpg_buf1[0][jj];
      cs2 = (float)jpg_buf2[0][jj];
      sse += (cs1 - cs2) * (cs1 - cs2);
      pixel_count++;
    }
  }
  asfPercentMeter(1.0);
  if (pixel_count > 0 && max_val > 0) {
    rmse = sqrt(sse/pixel_count);
    *psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
  }
  else {
    *psnr = MISSING_PSNR;
  }

  jpeg_finish_decompress(&cinfo1);
  jpeg_destroy_decompress(&cinfo1);
  jpeg_finish_decompress(&cinfo2);
  jpeg_destroy_decompress(&cinfo2);
  if (fp1) FCLOSE(fp1);
  if (fp2) FCLOSE(fp2);
  if (outFP) FCLOSE(outFP);
  free_band_names(&band_names1, num_names_extracted1);
  free_band_names(&band_names2, num_names_extracted2);
}

// Creates and writes VERY simple metadata file to coax the fftMatch() functions
// to work.  The only requirements are that the number of lines and samples, and
// data type, are required.  The rest of the metadata is ignored.
void make_generic_meta(char *file, uint32 height, uint32 width, data_type_t data_type)
{
  char file_meta[1024];
  char *c;
  char *f = STRDUP(file);
  meta_parameters *md;

  c = findExt(f);
  *c = '\0';
  sprintf(file_meta, "%s.meta", f);
  md = raw_init();
  md->general->line_count = height;
  md->general->sample_count = width;
  md->general->data_type = data_type;
  md->general->image_data_type = AMPLITUDE_IMAGE; // This just quiets the fftMatch warnings during meta_read()

  meta_write(md, file_meta);
  meta_free(md);
}

void diff_check_geolocation(char *outputFile, char *inFile1, char *inFile2,
                            int num_bands, shift_data_t *shift,
                            stats_t *stats1, stats_t *stats2)
{
  char msg[1024];
  int band;
  int low_certainty;
  float shift_tol = PROJ_LOC_DIFF_TOL_m;
  float radial_shift_diff = 0.0;
  int num_extracted_bands1=0, num_extracted_bands2=0;
  FILE *outputFP = NULL;
  outputFP = fopen(outputFile, "a");
  shift_data_t *s = shift;
  stats_t *s1 = stats1;
  stats_t *s2 = stats2;

  // Get or produce band names for intelligent output...
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  get_band_names(inFile1, outputFP, &band_names1, &num_extracted_bands1);
  get_band_names(inFile2, outputFP, &band_names2, &num_extracted_bands2);

  // Check each band for differences
  char band_str1[64];
  char band_str2[64];
  for (band=0; band<num_bands; band++) {
    radial_shift_diff = sqrt(shift[band].dx * shift[band].dx + shift[band].dy * shift[band].dy);
    low_certainty = ISNAN(s[band].cert) ? 1 :
        !ISNAN(s[band].cert) && s[band].cert < MIN_MATCH_CERTAINTY ? 1 : 0;
    if (low_certainty &&
        (FLOAT_COMPARE_TOLERANCE(s1->mean, 0.0, FLOAT_TOLERANCE) ||
        FLOAT_COMPARE_TOLERANCE(s1->sdev, 0.0, FLOAT_TOLERANCE) ||
        FLOAT_COMPARE_TOLERANCE(s2->mean, 0.0, FLOAT_TOLERANCE) ||
        FLOAT_COMPARE_TOLERANCE(s2->sdev, 0.0, FLOAT_TOLERANCE)))
    {
      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      sprintf(msg, "FAIL: Correlation match between images failed\n"
          "when trying to find shift in geolocation between non-blank\n"
          "images.  Comparing\n  %s%s  and\n  %s%s\n\n",
              band_str1, inFile1, band_str2, inFile2);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
    else if (fabs(shift[band].dx) > shift_tol ||
        fabs(shift[band].dy) > shift_tol ||
        radial_shift_diff > shift_tol)
    {
      fprintf(outputFP, "\n-----------------------------------------------\n");
      asfPrintStatus("\n-----------------------------------------------\n");

      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      sprintf(msg, "FAIL: Comparing geolocations of\n  %s%s  and\n  %s%s\n\n",
              band_str1, inFile1, band_str2, inFile2);
      fprintf(outputFP, msg);
      asfPrintStatus(msg);

      if (shift[band].dx > shift_tol) {
        sprintf(msg, "[%s] [x-loc]  File1: %12f,  File2: %12f, Tolerance: %11f (Certainty: %f%%)\n",
                shift[band].dx > shift_tol ? "FAIL" : "PASS",
                0.0, shift[band].dx, shift_tol, 100.0 * shift[band].cert);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }
      if (shift[band].dy > shift_tol) {
        sprintf(msg, "[%s] [y-loc]  File1: %12f,  File2: %12f, Tolerance: %11f (Certainty: %f%%)\n",
                shift[band].dy > shift_tol ? "FAIL" : "PASS",
                0.0, shift[band].dy, shift_tol, 100.0 * shift[band].cert);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }
      if (radial_shift_diff > shift_tol) {
        sprintf(msg, "[%s] [radial dist]  File1: %12f,  File2: %12f, Tolerance: %11f\n",
                radial_shift_diff > shift_tol ? "FAIL" : "PASS",
                0.0, radial_shift_diff, shift_tol);
        fprintf(outputFP, msg);
        asfPrintStatus(msg);
      }

      fprintf(outputFP, "-----------------------------------------------\n\n");
      asfPrintStatus("-----------------------------------------------\n\n");
    }
    else
    {
      asfPrintStatus("\nNo differences found in geolocations\n\n");
    }
  } // For each band

  if (outputFP) FCLOSE(outputFP);
  free_band_names(&band_names1, num_extracted_bands1);
  free_band_names(&band_names2, num_extracted_bands2);
}

void fftShiftCheck(char *file1, char *file2, char *corr_file,
                   shift_data_t *shifts)
{
  fftMatch(file1, file2, corr_file, &shifts->dx, &shifts->dy, &shifts->cert);
}

void export_jpeg_to_asf_img(char *inFile, char *outfile,
                            char *fft_file, char *fft_meta_file,
                            uint32 height, uint32 width, data_type_t data_type,
                            int band_no)
{
  meta_parameters *md;
  float *buf;
  jpeg_info_t jpg;
  char msg[1024];
  struct jpeg_decompress_struct cinfo;
  FILE *outFP=NULL, *imgFP=NULL;
  jpeg_error_hdlr_t jerr;
  JSAMPARRAY jpg_buf;

  // Write out the (very very simple) metadata file ...only contains line_count, sample_count, and data_type
  make_generic_meta(fft_meta_file, height, width, data_type);
  md = meta_read(fft_meta_file);

  buf = (float*)MALLOC(width * sizeof(float));
  if (buf == NULL) {
    sprintf(msg,"Cannot allocate float buffer...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  get_jpeg_info_hdr_from_file(inFile, &jpg, outfile);
  if (jpg.width <= 0 ||
      jpg.height <= 0 ||
      jpg.data_type != BYTE ||
      jpg.num_bands <= 0)
  {
    sprintf(msg,"Invalid JPEG found...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  uint32 jj;
  FILE *fp = FOPEN(inFile, "rb");
  if (fp == NULL) {
    sprintf(msg,"Cannot open JPEG file for read...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  // Setup jpeg lib error handler
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = jpeg_err_exit; // Error hook to catch jpeg lib errors
  if (setjmp(jerr.setjmp_buffer)) {
    // JPEG lib error occurred
    jpeg_destroy_decompress (&cinfo);
    if (fp) FCLOSE(fp);
    sprintf(msg,"JPEG library critical error ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  // Init jpeg lib to read
  jpeg_create_decompress(&cinfo); // Init decompression object
  jpeg_stdio_src(&cinfo, fp); // Set input to open inFile

  // Read jpeg file header
  jpeg_read_header(&cinfo, TRUE);
  jpeg_start_decompress(&cinfo);

  // Allocate buffer for reading jpeg
  jpg_buf = (*cinfo.mem->alloc_sarray) (
      (j_common_ptr)&cinfo, JPOOL_IMAGE, jpg.width, 1);

  asfPrintStatus("Converting JPEG to ASF Internal Format .img file...\n");
  imgFP = (FILE*)FOPEN(fft_file, "wb");
  if (imgFP == NULL) {
    sprintf(msg,"Cannot open IMG file for write...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  int row=0;
  while (cinfo.output_scanline < cinfo.image_height)
  {
    asfPercentMeter((double)cinfo.output_scanline/(double)jpg.height);
    jpeg_read_scanlines(&cinfo, jpg_buf, 1); // Reads one scanline
    for (jj = 0 ; jj < jpg.width; jj++ ) {
      if (jpg.num_bands == 1) {
        buf[jj] = (float)jpg_buf[0][jj];
      }
      else if (jpg.num_bands == 3) {
        buf[jj] = (float)jpg_buf[0][jj*3+band_no];
      }
      else {
        // Shouldn't get here...
        sprintf(msg,"Invalid number of bands in JPEG file ...Aborting\n");
        outFP = (FILE*)FOPEN(outfile,"a");
        if (outFP) {
          fprintf(outFP, msg);
          FCLOSE(outFP);
        }
        asfPrintError(msg);
      }
    }
    put_float_line(imgFP, md, row, buf);
    row++;
  }
  asfPercentMeter(1.0);

  // Finish up
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  if (fp) FCLOSE(fp);
  if (imgFP) FCLOSE(imgFP);
  if (buf) FREE(buf);
  meta_free(md);
}

void export_ppm_pgm_to_asf_img(char *inFile, char *outfile,
                               char *fft_file, char *fft_meta_file,
                               uint32 height, uint32 width, data_type_t data_type,
                               int band_no)
{
  meta_parameters *md;
  uint32 ii;
  float *buf;
  FILE *imgFP=NULL;
  ppm_pgm_info_t pgm;
  char msg[1024];
  FILE *outFP=NULL;

  make_generic_meta(fft_meta_file, height, width, data_type);
  md = meta_read(fft_meta_file);

  get_ppm_pgm_info_hdr_from_file(inFile, &pgm, outfile);
  if (pgm.magic == NULL || strlen(pgm.magic) == 0 ||
      pgm.width <= 0 ||
      pgm.height <= 0 ||
      pgm.max_val <= 0 ||
      pgm.img_offset < 0 ||
      pgm.bit_depth != 8 ||
      pgm.data_type != BYTE ||
      pgm.num_bands <= 0)
  {
    sprintf(msg,"Invalid PPM/PGM file found...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  FILE *fp = FOPEN(inFile, "rb");
  if (fp == NULL) {
    sprintf(msg,"Cannot open PPM/PGM file for read ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  imgFP = FOPEN(fft_file, "wb");
  if (imgFP == NULL) {
    sprintf(msg,"Cannot open IMG file for write ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  buf = (float*)MALLOC(pgm.width * sizeof(float));
  if (buf == NULL) {
    sprintf(msg,"Cannot allocate memory ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  asfPrintStatus("\nConverting PPM/PGM file to IMG format...\n");
  FSEEK64(fp, (long long)pgm.img_offset, SEEK_SET);
  for ( ii = 0; ii < pgm.height; ii++ )
  {
    asfPercentMeter((double)ii/(double)pgm.height);
    ppm_pgm_get_float_line(fp, buf, ii, &pgm, band_no);
    put_float_line(imgFP, md, ii, buf);
  }
  asfPercentMeter(1.0);

  if (buf) free(buf);
  if (fp) FCLOSE(fp);
  if (imgFP) FCLOSE(imgFP);
  meta_free(md);
}

void export_tiff_to_asf_img(char *inFile, char *outfile,
                            char *fft_file, char *fft_meta_file,
                            uint32 height, uint32 width, data_type_t data_type,
                            int band_no)
{
  meta_parameters *md;
  FILE *imgFP=NULL, *outFP=NULL;
  char msg[1024];

  make_generic_meta(fft_meta_file, height, width, data_type);
  md = meta_read(fft_meta_file);

  tiff_data_t t;
  tsize_t scanlineSize;

  get_tiff_info_from_file(inFile, &t);
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0)
  {
    sprintf(msg,"Invalid TIFF file found ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  uint32 ii;
  TIFF *tif = XTIFFOpen(inFile, "rb");
  if (tif == NULL) {
    sprintf(msg,"Cannot open TIFF file for read ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  imgFP = (FILE*)FOPEN(fft_file, "wb");
  if (tif == NULL) {
    if (tif) XTIFFClose(tif);
    sprintf(msg,"Cannot open IMG file for write ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  scanlineSize = TIFFScanlineSize(tif);
  if (scanlineSize <= 0) {
    if (tif) XTIFFClose(tif);
    sprintf(msg,"Invalid scan line size in TIFF file ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  if (t.num_bands > 1 &&
      t.planar_config != PLANARCONFIG_CONTIG &&
      t.planar_config != PLANARCONFIG_SEPARATE)
  {
    if (tif) XTIFFClose(tif);
    sprintf(msg,"Invalid planar configuration found in TIFF file ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  float *buf = (float*)MALLOC(t.width * sizeof(float));

  asfPrintStatus("Converting TIFF file to IMG file..\n");
  for ( ii = 0; ii < t.height; ii++ )
  {
    asfPercentMeter((double)ii/(double)t.height);
    tiff_get_float_line(tif, buf, ii, band_no);
    put_float_line(imgFP, md, ii, buf);
  }
  asfPercentMeter(1.0);

  if (buf) free(buf);
  if (tif) XTIFFClose(tif);
  if (imgFP) FCLOSE(imgFP);
  meta_free(md);
}

void export_png_to_asf_img(char *inFile, char *outfile,
                           char *fft_file, char *fft_meta_file,
                           uint32 img_height, uint32 img_width, data_type_t img_data_type,
                           int band_no)
{
  meta_parameters *md;
  FILE *imgFP=NULL, *outFP=NULL;

  make_generic_meta(fft_meta_file, img_height, img_width, img_data_type);
  md = meta_read(fft_meta_file);

  png_structp png_ptr;
  png_infop info_ptr;
  png_uint_32  width, height;
  int bit_depth, color_type, interlace_type, compression_type, filter_type;
  unsigned char sig[8];
  char msg[1024];
  FILE *pngFP;
  png_info_t ihdr;

  get_png_info_hdr_from_file(inFile, &ihdr, outfile);
  if (ihdr.bit_depth == MISSING_PNG_DATA ||
      ihdr.color_type == MISSING_PNG_DATA ||
      (ihdr.color_type_str == NULL || strlen(ihdr.color_type_str) <= 0) ||
      ihdr.interlace_type == MISSING_PNG_DATA ||
      ihdr.compression_type == MISSING_PNG_DATA ||
      ihdr.filter_type == MISSING_PNG_DATA ||
      ihdr.data_type == 0 ||
      ihdr.num_bands == MISSING_PNG_DATA ||
      ihdr.height == 0 ||
      ihdr.width == 0)
  {
    sprintf(msg,"Invalid PNG file found ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  imgFP = (FILE*)FOPEN(fft_file,"wb");
  if (imgFP == NULL) {
    sprintf(msg,"Cannot open IMG file ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }

  // Initialize PNG file for read
  pngFP = (FILE*)FOPEN(inFile,"rb");
  if (pngFP == NULL) {
    sprintf(msg,"Cannot open PNG file ...Aborting\n");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  fread(sig, 1, 8, pngFP); // Important: Leaves file pointer offset into file by 8 bytes for png lib
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    if (pngFP) FCLOSE(pngFP);
    sprintf(msg, "Invalid PNG file (%s)\n", "file type header bytes invalid");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  png_ptr = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    if (pngFP) FCLOSE(pngFP);
    sprintf(msg, "Cannot allocate PNG read struct ...Aborting");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    if (pngFP) FCLOSE(pngFP);
    sprintf(msg, "Cannot allocate PNG info struct ...Aborting");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  if (setjmp(png_jmpbuf(png_ptr))) {
    if (pngFP) FCLOSE(pngFP);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred ...Aborting");
    outFP = (FILE*)FOPEN(outfile,"a");
    if (outFP) {
      fprintf(outFP, msg);
      FCLOSE(outFP);
    }
    asfPrintError(msg);
  }
  png_init_io(png_ptr, pngFP);
  png_set_sig_bytes(png_ptr, 8); // Because of the sig-reading offset ...must do this for PNG lib

  // Read info and IHDR
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, &compression_type, &filter_type);

  uint32 ii;
  float *buf = (float*)MALLOC(ihdr.width * sizeof(float));

  for ( ii = 0; ii < ihdr.height; ii++ )
  {
    asfPercentMeter((double)ii/(double)ihdr.height);
    png_sequential_get_float_line(png_ptr, info_ptr, buf, band_no);
    put_float_line(imgFP, md, ii, buf);
  }
  asfPercentMeter(1.0);

  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  if (buf) free(buf);
  if (outFP) FCLOSE(outFP);
  if (pngFP) FCLOSE(pngFP);
  if (imgFP) FCLOSE(imgFP);
  meta_free(md);
}


