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
#include "asf_endian.h"
#include <unistd.h>
#include <math.h>
#include <ctype.h>
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "asf_raster.h"
#include "envi.h"
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

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define FLOAT_TOLERANCE 0.000001
#define FLOAT_EQUIVALENT2(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, FLOAT_TOLERANCE))
#define MISSING_PSNR -32000
#define CORR_FILE "tmp_corr"

#define MISSING_TIFF_DATA -1
typedef struct {
  uint32 width;
  uint32 height;
  short sample_format;
  short bits_per_sample;
  short planar_config;
  data_type_t data_type; // ASF data type
  short num_bands;
  int is_scanline_format;
  int is_palette_color_tiff;
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
int envi_img_file_found(char *file);
void matrix2asf(char *inFile, char **outImg, char **outMeta);
graphics_file_t getGraphicsFileType (char *file);
void graphicsFileType_toStr (graphics_file_t type, char *type_str);
void fftDiff(char *inFile1, char *inFile2, float *bestLocX, float *bestLocY, 
	     float *certainty);
float get_maxval(data_type_t data_type);
void calc_asf_img_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               psnr_t *psnr, int band);
void calc_ppm_pgm_stats_2files(char *inFile1, char *inFile2, char *outfile,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               psnr_t *psnr, int band);
void calc_jpeg_stats_2files(char *inFile1, char *inFile2, char *outfile,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            psnr_t *psnr, int band);
void calc_tiff_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            psnr_t *psnr, int band);
void calc_png_stats_2files(char *inFile1, char *inFile2, char *outfile,
                           stats_t *inFile1_stats, stats_t *inFile2_stats,
                           psnr_t *psnr, int band);
void print_stats_results(char *filename1, char *filename2,
                         char *band_str1, char *band_str2,
                         stats_t *s1, stats_t *s2,
                         psnr_t psnr);
void diff_check_stats(char *outputFile, char *inFile1, char *inFile2,
                      stats_t *stats1, stats_t *stats2, psnr_t *psnr,
                      int strict, data_type_t t, int num_bands);
void diff_check_geotiff(char *outfile, geotiff_data_t *g1, geotiff_data_t *g2);
void diff_check_geolocation(char *outputFile, char *inFile1, char *inFile2,
                            int num_bands, shift_data_t *shift,
			    stats_t *stats1, stats_t *stats2);
void diffErrOut(char *outputFile, char *err_msg);
void get_tiff_info_from_file(char *file, tiff_data_t *t);
void get_tiff_info(TIFF *tif, tiff_data_t *t);
void get_geotiff_keys(char *file, geotiff_data_t *g);
void projection_type_2_str(projection_type_t proj, char *proj_str);
int  tiff_image_band_statistics_from_file(char *inFile, int band_no, 
					  int *stats_exist,
                                          double *min, double *max,
                                          double *mean, double *sdev, 
					  double *rmse,
                                          int use_mask_value, float mask_value);
void tiff_image_band_psnr_from_files(char *inFile1, char *inFile2,
                                     int band1, int band2, psnr_t *psnr);
float tiff_image_get_float_pixel(TIFF *tif, int row, int col, int band_no);
void tiff_get_float_line(TIFF *tif, float *buf, int row, int band_no);
void get_png_info_hdr_from_file(char *inFile, png_info_t *ihdr1, char *outfile);
int png_image_band_statistics_from_file(char *inFile, char *outfile,
                                        int band_no, int *stats_exist,
                                        double *min, double *max,
                                        double *mean, double *sdev, 
					double *rmse,
                                        int use_mask_value, float mask_value);
void png_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
                                    int band1, int band2, psnr_t *psnr);
void png_sequential_get_float_line(png_structp png_ptr, png_infop info_ptr, 
				   float *buf, int band);
void get_ppm_pgm_info_hdr_from_file(char *inFile, ppm_pgm_info_t *pgm, 
				    char *outfile);
void ppm_pgm_get_float_line(FILE *fp, float *buf, int row, ppm_pgm_info_t *pgm,
			    int band_no);
int ppm_pgm_image_band_statistics_from_file(char *inFile, char *outfile,
					    int band_no, int *stats_exist,
					    double *min, double *max,
					    double *mean, double *sdev, 
					    double *rmse,
					    int use_mask_value, 
					    float mask_value);
void ppm_pgm_image_band_psnr_from_files(char *inFile1, char *inFile2, 
					char *outfile,
                                        int band1, int band2, psnr_t *psnr);
void get_band_names(char *inFile, FILE *outputFP,
                    char ***band_names, int *num_extracted_bands);
void free_band_names(char ***band_names, int num_extracted_bands);
METHODDEF(void) jpeg_err_exit(j_common_ptr cinfo);
GLOBAL(void) get_jpeg_info_hdr_from_file(char *inFile, jpeg_info_t *jpg, 
					 char *outputFile);
GLOBAL(void) jpeg_image_band_statistics_from_file(char *inFile, char *outfile,
                                                  int band_no, int *stats_exist,
                                                  double *min, double *max,
                                                  double *mean, double *sdev, 
						  double *rmse,
                                                  int use_mask_value, 
						  float mask_value);
GLOBAL(void) jpeg_image_band_psnr_from_files(char *inFile1, char *inFile2, 
					     char *outfile,
                                             int band1, int band2, 
					     psnr_t *psnr);
void make_generic_meta(char *file, uint32 height, uint32 width,
                data_type_t data_type, char **band_names,
                int num_bands);
void fftShiftCheck(char *file1, char *file2, char *corr_file,
                   shift_data_t *shifts);
void export_ppm_pgm_to_asf_img(char *inFile, char *outfile,
                               char *fft_file, char *fft_meta_file,
                               uint32 height, uint32 width, 
			       data_type_t data_type, int band);
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
char *pcs2description(int pcs);
void calc_asf_complex_image_stats_2files(char *inFile1, char *inFile2,
                                         complex_stats_t *inFile1_complex_stats,
                                         complex_stats_t *inFile2_complex_stats,
                                         complex_psnr_t *psnr, int band);
void calc_complex_stats_rmse_from_file(const char *inFile, const char *band, 
				       double mask, double *i_min, 
				       double *i_max, double *i_mean,
                                       double *i_sdev, double *i_rmse, 
				       gsl_histogram **i_histogram,
                                       double *q_min, double *q_max, 
				       double *q_mean,
                                       double *q_sdev, double *q_rmse, 
				       gsl_histogram **q_histogram);
void diff_check_complex_stats(char *outputFile, char *inFile1, char *inFile2,
                              complex_stats_t *cstats1, 
			      complex_stats_t *cstats2,
                              complex_psnr_t *cpsnr, int strict,
                              data_type_t data_type, int num_bands);

int diffimage(char *inFile1, char *inFile2, char *outputFile, char *logFile,
	      char ***bands1, char ***bands2,
	      int *num_bands1, int *num_bands2, int *complex,
	      stats_t **stats1, stats_t **stats2,
	      complex_stats_t **complex_stats1, 
	      complex_stats_t **complex_stats2,
	      psnr_t **psnrs, complex_psnr_t **complex_psnr,
	      shift_data_t **data_shift)
{
        extern FILE *fLog; /* output file descriptor, stdout or log file */
        int bandflag=0, strictflag=0;
        int band = 0;
        if (!bandflag)
                band = 0;
        char msg[1024];
        char type_str[255];
        msg[0] = '\0';
        stats_t inFile1_stats[MAX_BANDS], inFile2_stats[MAX_BANDS];
        complex_stats_t inFile1_complex_stats[MAX_BANDS];
        complex_stats_t inFile2_complex_stats[MAX_BANDS];
        psnr_t psnr[MAX_BANDS]; // peak signal to noise ratio
        complex_psnr_t cpsnr[MAX_BANDS];
        shift_data_t shifts[MAX_BANDS];

        if (logFile && strlen(logFile) > 0)
                fLog = FOPEN(logFile, "w");

        // Create temporary directory and file names
        char *baseName, tmpDir[1024];
        char file1_fftFile[1024], file1_fftMetaFile[1024];
        char file2_fftFile[1024], file2_fftMetaFile[1024];
        if (outputFile && strlen(outputFile) > 0)
                baseName = get_basename(outputFile);
        else
                baseName = get_basename(inFile1);
        strcpy(tmpDir, baseName);
        strcat(tmpDir, "-");
        strcat(tmpDir, time_stamp_dir());
        create_clean_dir(tmpDir);
        sprintf(file1_fftFile, "%s/tmp_file1.img", tmpDir);
        sprintf(file2_fftFile, "%s/tmp_file2.img", tmpDir);
        sprintf(file1_fftMetaFile, "%s/tmp_file1.meta", tmpDir);
        sprintf(file2_fftMetaFile, "%s/tmp_file2.meta", tmpDir);

        // Empty the output file
        FILE *fp = NULL;
        if (outputFile && strlen(outputFile) > 0) 
                fp = (FILE*) FOPEN(outputFile, "w");
        if(fp) 
                FCLOSE(fp);

        /*
         * Determine input file graphical format types and check to see if they
         * are supported and the same type (etc ...error checking)
         */
        if (strcmp(inFile1, inFile2) == 0) {
                FREE(outputFile);
                return (0); /* A file compared to itself is always the same. */
        }
        if (!fileExists(inFile1)) {
                sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?", inFile1);
                FREE(outputFile);
                asfPrintError(msg);
        }
        if (!fileExists(inFile2)) {
                sprintf(msg, "File not found: %s\n  => Did you forget to use the filename extension?", inFile2);
                FREE(outputFile);
                asfPrintError(msg);
        }
        graphics_file_t type1, type2;
        type1 = getGraphicsFileType(inFile1);
        type2 = getGraphicsFileType(inFile2);
        if (type1 != ASF_IMG &&
                        type1 != ENVI_IMG &&
                        type1 != ENVI_MAT &&
                        type1 != JPEG_IMG &&
                        type1 != PGM_IMG &&
                        type1 != PPM_IMG &&
                        type1 != PNG_IMG &&
                        type1 != STD_TIFF_IMG &&
                        type1 != GEO_TIFF_IMG &&
                        type1 != GEO_TIFF_MAT) {
                graphicsFileType_toStr(type1, type_str);
                sprintf(msg, "Graphics file type %s "
                                "is not currently supported (Image #1: %s)\n",
                                type_str, inFile1);
                FREE(outputFile);
                asfPrintError(msg);
        }
        if (type2 != ASF_IMG &&
                        type2 != ENVI_IMG &&
                        type2 != ENVI_MAT &&
                        type2 != JPEG_IMG &&
                        type2 != PGM_IMG &&
                        type2 != PPM_IMG &&
                        type1 != PNG_IMG &&
                        type2 != STD_TIFF_IMG &&
                        type2 != GEO_TIFF_IMG &&
                        type2 != GEO_TIFF_MAT) {
                graphicsFileType_toStr(type2, type_str);
                sprintf(msg, "Graphics file type %s "
                                " is not currently supported (Image #2: %s)\n",
                                type_str, inFile2);
                FREE(outputFile);
                asfPrintError(msg);
        }

        /***** Calculate stats and PSNR *****/
        char **band_names1, **band_names2;
        char band_str1[255], band_str2[255];
        char *data_type1 = NULL;
        char *data_type2 = NULL;
        switch (type1) {
        case ASF_IMG: {
                strcpy(file1_fftFile, inFile1);
                strcpy(file2_fftFile, inFile2);
                char *ext;
                char *f1 = STRDUP(inFile1);
                char *f2 = STRDUP(inFile2);
                ext = findExt(f1);
                *ext = '\0';
                ext = findExt(f2);
                *ext = '\0';
                sprintf(file1_fftMetaFile, "%s.meta", f1);
                sprintf(file2_fftMetaFile, "%s.meta", f2);
                break;
        }
        case ENVI_IMG:
        case ENVI_MAT:
        case GEO_TIFF_MAT: {
                char *outFile = (char *) MALLOC(sizeof(char)*1024);
                char *outMeta = (char *) MALLOC(sizeof(char)*1024);
                matrix2asf(inFile1, &outFile, &outMeta);
                strcpy(file1_fftFile, outFile);
                strcpy(file1_fftMetaFile, outMeta);
                matrix2asf(inFile2, &outFile, &outMeta);
                strcpy(file2_fftFile, outFile);
                strcpy(file2_fftMetaFile, outMeta);
                break;
        }
        case JPEG_IMG: {
                jpeg_info_t jpg1, jpg2;
                get_jpeg_info_hdr_from_file(inFile1, &jpg1, outputFile);
                get_jpeg_info_hdr_from_file(inFile2, &jpg2, outputFile);
	        *complex = 0;
                data_type1 = data_type2str(jpg1.data_type);
                data_type2 = data_type2str(jpg2.data_type);
                export_jpeg_to_asf_img(inFile1, outputFile, file1_fftFile,
                                file1_fftMetaFile, jpg1.height, jpg1.width,
                                REAL32, band);
                export_jpeg_to_asf_img(inFile2, outputFile, file2_fftFile,
                                file2_fftMetaFile, jpg2.height, jpg2.width,
                                REAL32, band);
                break;
        }
        case PNG_IMG: {
                png_info_t ihdr1, ihdr2;
                get_png_info_hdr_from_file(inFile1, &ihdr1, outputFile);
                get_png_info_hdr_from_file(inFile2, &ihdr2, outputFile);
                data_type1 = data_type2str(ihdr1.data_type);
                data_type2 = data_type2str(ihdr2.data_type);
                export_png_to_asf_img(inFile1, outputFile, file1_fftFile,
                                file1_fftMetaFile, ihdr1.height, ihdr1.width,
                                REAL32, band);
                export_png_to_asf_img(inFile2, outputFile, file2_fftFile,
                                file2_fftMetaFile, ihdr2.height, ihdr2.width,
                                REAL32, band);
                break;
        }
        case PPM_IMG:
        case PGM_IMG: {
                ppm_pgm_info_t pgm1, pgm2;
                get_ppm_pgm_info_hdr_from_file(inFile1, &pgm1, outputFile);
                get_ppm_pgm_info_hdr_from_file(inFile2, &pgm2, outputFile);
                data_type1 = data_type2str(pgm1.data_type);
                data_type2 = data_type2str(pgm2.data_type);
                export_ppm_pgm_to_asf_img(inFile1, outputFile, file1_fftFile,
                                file1_fftMetaFile, pgm1.height, pgm1.width,
                                REAL32, band);
                export_ppm_pgm_to_asf_img(inFile2, outputFile, file2_fftFile,
                                file2_fftMetaFile, pgm2.height, pgm2.width,
                                REAL32, band);
                break;
        }
        case STD_TIFF_IMG:
        case GEO_TIFF_IMG: {
                int geotiff = (type1 == GEO_TIFF_IMG) ? 1 : 0;
                geotiff_data_t g1, g2;
                if (geotiff) {
                        get_geotiff_keys(inFile1, &g1);
                        get_geotiff_keys(inFile2, &g2);
                        diff_check_geotiff(outputFile, &g1, &g2);
                }
                tiff_data_t t1, t2;

                get_tiff_info_from_file(inFile1, &t1);
                get_tiff_info_from_file(inFile2, &t2);
                export_tiff_to_asf_img(inFile1, outputFile, file1_fftFile,
                                file1_fftMetaFile, t1.height, t1.width, REAL32,
                                band);
                export_tiff_to_asf_img(inFile2, outputFile, file2_fftFile,
                                file2_fftMetaFile, t2.height, t2.width, REAL32,
                                band);
	        *complex = 0;
                break;
        }
        default:
                sprintf(msg, "Unrecognized image file type found.\n");
                diffErrOut(outputFile, msg);
                FREE(outputFile);
                asfPrintError(msg);
                break;
        }
        meta_parameters *md1 = NULL;
        meta_parameters *md2 = NULL;

        // Read metadata and check for multi-bandedness
        
        if (fileExists(file1_fftMetaFile))
                md1 = meta_read(file1_fftMetaFile);
        if (fileExists(file2_fftMetaFile))
                md2 = meta_read(file2_fftMetaFile);
        if (md1 == NULL || md2 == NULL) {
                char* missing_file;
                if (md1 == NULL)
                        missing_file = file1_fftMetaFile;
                else
                        missing_file = file2_fftMetaFile;
                sprintf(msg, "Cannot find metadata file %s\n", missing_file);
                diffErrOut(outputFile, msg);
                FREE(outputFile);
                asfPrintError(msg);
        }
        int band_count1 = md1->general->band_count;
        int band_count2 = md2->general->band_count;
        band_names1 = extract_band_names(md1->general->bands,
                        md1->general->band_count);
        band_names2 = extract_band_names(md2->general->bands,
                        md2->general->band_count);
	*bands1 = band_names1;
	*bands2 = band_names2;
	*num_bands1 = band_count1;
	*num_bands2 = band_count2;

        if (bandflag && !(band < band_count1 && band < band_count2 &&
                        band >= 0))
                sprintf(msg, "Invalid band number. Band number must be 0 (first band) or greater, and less\nthan the number of available bands in the file.\nExample: If the files have 3 bands, then band numbers 0, 1, or 2 are the valid\nband number choices.\n\nFile1 has %d bands. File2 has %d bands\n",
                                band_count1, band_count2);
        if (!bandflag && band_count1 != band_count2)
                sprintf(msg, "Files do not have the same number of bands.\nCannot compare all bands. Consider using the -band option to compare individual\nbands within the files.\n\nFile1 has %d bands. File2 has %d bands\n",
                                band_count1, band_count2);
        if (md1->general->data_type != md2->general->data_type) {
                char *s1 = data_type2str(md1->general->data_type);
                char *s2 = data_type2str(md2->general->data_type);
                sprintf(msg, "Files do not have the same data type.\n\nFile1 has %s data. File2 has %s data\n", s1, s2);
        }
        if (data_type1 != NULL && data_type2 != NULL &&
                        strcmp(data_type1, data_type2) != 0)
                sprintf(msg, "Files do not have the same data type.\n\nFile1 has %s data. File2 has %s data\n", data_type1, data_type2);
        if (data_type1 != NULL)
                FREE(data_type1);
        if (data_type2 != NULL)
                FREE(data_type2);
        if (msg[0] != '\0') {
                diffErrOut(outputFile, msg);
                meta_free(md1);
                meta_free(md2);
                FREE(outputFile);
                free_band_names(&band_names1, band_count1);
                free_band_names(&band_names2, band_count2);
                asfPrintError(msg);
        }
        int is_complex = md2->general->data_type == COMPLEX_BYTE ||
                        md2->general->data_type == COMPLEX_INTEGER16 ||
                        md2->general->data_type == COMPLEX_INTEGER32 ||
                        md2->general->data_type == COMPLEX_REAL32 ||
                        md2->general->data_type == COMPLEX_REAL64;

	// Allocate space for output
        *complex = is_complex;
	if (is_complex) {
	        *complex_stats1 = (complex_stats_t*) MALLOC(
                                sizeof(complex_stats_t) * band_count1);
	        *complex_stats2 = (complex_stats_t*) MALLOC(
                                sizeof(complex_stats_t) * band_count2);
	        *complex_psnr = (complex_psnr_t*) MALLOC(
                                sizeof(complex_psnr_t) * band_count1);
	}
	else {
	        *stats1 = (stats_t*) MALLOC(
                                sizeof(stats_t) * band_count1);
	        *stats2 = (stats_t*) MALLOC(
                                sizeof(stats_t) * band_count2);
	        *psnrs = (psnr_t*) MALLOC(sizeof(psnr_t) * band_count1);
	}
	*data_shift = (shift_data_t*) MALLOC(sizeof(shift_data_t));

        if (is_complex && md2->general->data_type != COMPLEX_BYTE) {
                char *data_type_str = data_type2str(md2->general->data_type);
                sprintf(msg, "Complex data types other than COMPLEX_BYTE not yet supported (Found %s)\n", data_type_str);
                if (data_type_str)
                        FREE(data_type_str);
                diffErrOut(outputFile, msg);
                meta_free(md1);
                meta_free(md2);
                FREE(outputFile);
                free_band_names(&band_names1, band_count1);
                free_band_names(&band_names2, band_count2);
                asfPrintError(msg);
        }

        /* Calculate statistics, PSNR, measure image-to-image shift in
         * geolocation, and then check the results
         */
        int band_no;
        int empty_band1, empty_band2;
        if (bandflag) {
                band_count1 = band + 1;
                band_no = band;
        } else {
                band_no = 0;
        }
        for (; band_no < band_count1; band_no++) {
                strcpy(band_str1, "");
                strcpy(band_str2, "");
                if (band_count1 > 0) {
                        sprintf(band_str1, "Band %s in ", band_names1[band_no]);
                        sprintf(band_str2, "Band %s in ", band_names2[band_no]);
                }
                asfPrintStatus("\nCalculating statistics for\n  %s%s and\n"
                                "  %s%s\n", band_str1, file1_fftFile, band_str2,
                                file2_fftFile);
                if (is_complex) {
                        /*
                         * For complex data, only check stats and psnr . . . and
                         * don't check for shifts in geolocation (doesn't make
                         * sense)
                         */
                        calc_asf_complex_image_stats_2files(file1_fftFile,
                                        file2_fftFile,
                                        &inFile1_complex_stats[band_no],
                                        &inFile2_complex_stats[band_no],
                                        &cpsnr[band_no], band_no);
                        (*complex_stats1)[band_no] =
                                        inFile1_complex_stats[band_no];
                        (*complex_stats2)[band_no] =
                                        inFile2_complex_stats[band_no];
                        (*complex_psnr)[band_no] = cpsnr[band_no];
                } else {
                        calc_asf_img_stats_2files(file1_fftFile, file2_fftFile,
                                        &inFile1_stats[band_no],
                                        &inFile2_stats[band_no], &psnr[band_no],
                                        band_no);
                        (*stats1)[band_no] = inFile1_stats[band_no];
                        (*stats2)[band_no] = inFile2_stats[band_no];
                        (*psnrs)[band_no] = psnr[band_no];
                        empty_band1 = (
                                        FLOAT_EQUIVALENT2(
                                        inFile1_stats[band_no].mean, 0.0) &&
                                        FLOAT_EQUIVALENT2(
                                        inFile1_stats[band_no].sdev, 0.0))
                                        ? 1 : 0;
                        empty_band2 = (
                                        FLOAT_EQUIVALENT2(
                                        inFile2_stats[band_no].mean, 0.0) &&
                                        FLOAT_EQUIVALENT2(
                                        inFile2_stats[band_no].sdev, 0.0))
                                        ? 1 : 0;
                        if (!empty_band1 && !empty_band2 &&
                                        inFile1_stats[band_no].stats_good &&
                                        inFile2_stats[band_no].stats_good &&
                                        band_no == 0) {
                                if (band_no == 0) {
                                        fftShiftCheck(file1_fftFile,
                                                        file2_fftFile,
                                                        CORR_FILE,
                                                        &shifts[band_no]);
                                        (*data_shift)[band_no] =
                                                        shifts[band_no];
                                }
                        } else {
                                shifts[band_no].dx = 0.0;
                                shifts[band_no].dy = 0.0;
                                shifts[band_no].cert = 1.0;
                        }
                }
        }
                  
        /* 
         * Assumes both files have the same band count and data types or would
         * not be here
         */
        if (is_complex) {
                /*
                 * No diff check on geolocation (doesn't make sense for complex
                 * data)
                 */
                diff_check_complex_stats(outputFile, file1_fftFile,
                                file2_fftFile, inFile1_complex_stats,
                                inFile2_complex_stats, cpsnr, strictflag,
                                md2->general->data_type, band_count2);
        } else {
                diff_check_stats(outputFile, file1_fftFile, file2_fftFile,
                                inFile1_stats, inFile2_stats, psnr, strictflag,
                                md2->general->data_type, band_count2);
                diff_check_geolocation(outputFile, file1_fftFile, file2_fftFile,
                                1, shifts, inFile1_stats, inFile2_stats);
        }
        remove_dir(tmpDir);

        return (0);
}

graphics_file_t getGraphicsFileType (char *file)
{
  char *matrixType = (char *) MALLOC(sizeof(char)*5);
  char *error = (char *) MALLOC(sizeof(char)*255);
  int matrix = isPolsarproMatrix(file, &matrixType, &error);
  if (matrix && strstr(error, ".tif"))
    return ENVI_MAT;
  else if (matrix && strstr(error, ".bin"))
    return GEO_TIFF_MAT;

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
      file_type = JPEG_IMG;
    }
    else if (magic[0] == 'P' && (magic[1] == '4' || magic[1] == '1')) {
      file_type = PBM_IMG;
    }
    else if (magic[0] == 'P' && (magic[1] == '5' || magic[1] == '2')) {
      file_type = PGM_IMG;
    }
    else if (magic[0] == 'P' && (magic[1] == '6' || magic[1] == '3')) {
      file_type = PPM_IMG;
    }
    else if ((magic[0] == 'I' && magic[1] == 'I') || 
	     (magic[0] == 'M' && magic[1] == 'M')) {
      file_type = STD_TIFF_IMG;

      itif = XTIFFOpen(file, "rb");
      if (itif != NULL) {
        gtif = GTIFNew(itif);
        if (gtif != NULL) {
          double *tie_point = NULL;
          double *pixel_scale = NULL;
          short model_type, raster_type, linear_units;
          int read_count, tie_points, pixel_scales;

          (gtif->gt_methods.get)
	    (gtif->gt_tif, GTIFF_TIEPOINTS, &tie_points, &tie_point);
          (gtif->gt_methods.get)
	    (gtif->gt_tif, GTIFF_PIXELSCALE, &pixel_scales, &pixel_scale);
          read_count = GTIFKeyGet(gtif, GTModelTypeGeoKey, &model_type, 0, 1);
          read_count += 
	    GTIFKeyGet(gtif, GTRasterTypeGeoKey, &raster_type, 0, 0);
          read_count += 
	    GTIFKeyGet(gtif, ProjLinearUnitsGeoKey, &linear_units, 0, 1);

          if (tie_points == 6 && pixel_scales == 3 && read_count == 3) {
            file_type = GEO_TIFF_IMG;
          }

          if (tie_point != NULL) free(tie_point);
          if (pixel_scale != NULL) free(pixel_scale);
          GTIFFree(gtif);
        }
        XTIFFClose(itif);
      }
    }
    else if (magic[0] == 'B' && magic[1] == 'M') {
      file_type = BMP_IMG;
    }
    else if (magic[0] == 'G' && magic[1] == 'I' && magic[2] == 'F') {
      file_type = GIF_IMG;
    }
    else if (magic[1] == 'P' && magic[2] == 'N' && magic[3] == 'G') {
      file_type = PNG_IMG;
    }
    else if (envi_img_file_found(file)) {
      file_type = ENVI_IMG;
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

int envi_img_file_found(char *file)
{
  int found = FALSE;
  char *hdrFile = (char *) MALLOC(sizeof(char)*(strlen(file)+5));
  sprintf(hdrFile, "%s.hdr", file);
  if (fileExists(hdrFile)) {
    char buf[10];
    FILE *fp = FOPEN(hdrFile, "r");
    fgets(buf, 10, fp);
    FCLOSE(fp);
    if (strncmp_case(buf, "ENVI", 4) == 0)
      found = TRUE;
  }
  FREE(hdrFile);

  return found;
}

void fftDiff(char *inFile1, char *inFile2, float *bestLocX, float *bestLocY, 
	     float *certainty)
{
  ;
}

void graphicsFileType_toStr (graphics_file_t type, char *type_str)
{
  switch (type) {
    case ASF_IMG:
      strcpy (type_str, "ASF_IMG");
      break;
    case ENVI_IMG:
    case ENVI_MAT:
      strcpy (type_str, "ENVI");
      break;
    case JPEG_IMG:
      strcpy (type_str, "JPEG");
      break;
    case PBM_IMG:
      strcpy (type_str, "PBM");
      break;
    case PGM_IMG:
      strcpy (type_str, "PGM");
      break;
    case PPM_IMG:
      strcpy (type_str, "PPM");
      break;
    case STD_TIFF_IMG:
      strcpy (type_str, "TIFF");
      break;
    case GEO_TIFF_IMG:
    case GEO_TIFF_MAT:
      strcpy (type_str, "GEOTIFF");
      break;
    case BMP_IMG:
      strcpy (type_str, "BMP");
      break;
    case GIF_IMG:
      strcpy (type_str, "GIF");
      break;
    case PNG_IMG:
      strcpy (type_str, "PNG");
      break;
    case UNKNOWN_GRAPHICS_TYPE:
    default:
      strcpy(type_str, "UNRECOGNIZED");
      break;
  }
}

void calc_asf_complex_image_stats_2files(char *inFile1, char *inFile2,
					 complex_stats_t *inFile1_complex_stats,
					 complex_stats_t *inFile2_complex_stats,
					 complex_psnr_t *psnr, int band)
{
  char *f1;
  char *f2;
  char inFile1_meta[255], inFile2_meta[255];
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  char *c;
  int band_count1, band_count2;
  complex_stats_t *cs1 = inFile1_complex_stats;
  complex_stats_t *cs2 = inFile2_complex_stats;
  meta_parameters *md1 = NULL;
  meta_parameters *md2 = NULL;
  
  // Init stats
  // Presumed innocent until proven guilty
  cs1->i.stats_good = cs2->i.stats_good = 1; 
  cs1->i.min = cs2->i.min = 0.0;
  cs1->i.max = cs2->i.max = 0.0;
  cs1->i.mean = cs2->i.mean = 0.0;
  cs1->i.sdev = cs2->i.sdev = 0.0;
  cs1->i.rmse = cs2->i.rmse = 0.0;
  psnr->i.psnr = 0.0;
  psnr->i.psnr_good = 0;
  // Presumed innocent until proven guilty
  cs1->q.stats_good = cs2->q.stats_good = 1; 
  cs1->q.min = cs2->q.min = 0.0;
  cs1->q.max = cs2->q.max = 0.0;
  cs1->q.mean = cs2->q.mean = 0.0;
  cs1->q.sdev = cs2->q.sdev = 0.0;
  cs1->q.rmse = cs2->q.rmse = 0.0;
  psnr->q.psnr = 0.0;
  psnr->q.psnr_good = 0;
  
  // Read metadata and check for multi-bandedness
  if (inFile1 != NULL && strlen(inFile1) > 0) {
    f1 = STRDUP(inFile1);
    c = findExt(f1);
    *c = '\0';
    sprintf(inFile1_meta, "%s.meta", f1);
    if (fileExists(inFile1_meta)) {
      md1 = meta_read(inFile1_meta);
      cs1->i.stats_good = (md1 != NULL) ? cs1->i.stats_good : 0;
      cs1->q.stats_good = (md1 != NULL) ? cs1->q.stats_good : 0;
    }
  }
  else {
    cs1->i.stats_good = 0;
    cs1->i.stats_good = 0;
  }
  if (inFile2 != NULL && strlen(inFile2) > 0) {
    f2 = STRDUP(inFile2);
    c = findExt(f2);
    *c = '\0';
    sprintf(inFile2_meta, "%s.meta", f2);
    if (fileExists(inFile2_meta)) {
      md2 = meta_read(inFile2_meta);
      cs2->i.stats_good = (md2 != NULL) ? cs2->i.stats_good : 0;
      cs2->q.stats_good = (md2 != NULL) ? cs2->q.stats_good : 0;
    }
  }
  else {
    cs2->i.stats_good = 0;
    cs2->q.stats_good = 0;
  }
  
  // Calculate the stats from the data.
  cs1->i.hist = NULL;
  cs1->i.hist_pdf = NULL;
  cs1->q.hist = NULL;
  cs1->q.hist_pdf = NULL;
  cs2->i.hist = NULL;
  cs2->i.hist_pdf = NULL;
  cs2->q.hist = NULL;
  cs2->q.hist_pdf = NULL;
  if (band_count1 > 0 && md1 != NULL) {
    get_band_names(inFile1, NULL, &band_names1, &band_count1);
    if (band_names1 != NULL) {
      asfPrintStatus("\nCalculating statistics for %s band in %s", 
		     band_names1[band], inFile1);
      calc_complex_stats_rmse_from_file(inFile1, band_names1[band],
					md1->general->no_data,
					&cs1->i.min, &cs1->i.max, &cs1->i.mean,
					&cs1->i.sdev, &cs1->i.rmse, 
					&cs1->i.hist, &cs1->q.min, &cs1->q.max,
					&cs1->q.mean, &cs1->q.sdev, 
					&cs1->q.rmse, &cs1->q.hist);
    }
  }
  if (band_count2 > 0 && md2 != NULL) {
    get_band_names(inFile2, NULL, &band_names2, &band_count2);
    if (band_names2 != NULL) {
      asfPrintStatus("\nCalculating statistics for %s band in %s", 
		     band_names2[band], inFile2);
      calc_complex_stats_rmse_from_file(inFile2, band_names2[band],
					md2->general->no_data,
					&cs2->i.min, &cs2->i.max, &cs2->i.mean,
					&cs2->i.sdev, &cs2->i.rmse, 
					&cs2->i.hist, &cs2->q.min, &cs2->q.max,
					&cs2->q.mean, &cs2->q.sdev, 
					&cs2->q.rmse, &cs2->q.hist);
    }
  }
  
  // Calculate the peak signal to noise ratio (PSNR) between the two images
  double sse_i, sse_q;
  double rmse_i, rmse_q;
  int ii, jj;
  int band1 = band;
  int band2 = band;
  long pixel_count = 0;
  long lines, samples;
  long offset1 = md1->general->line_count * band1;
  long offset2 = md2->general->line_count * band2;
  float max_val;
  complexFloat *cdata1 = NULL; // components: real, imag
  complexFloat *cdata2 = NULL;
  if (md1 != NULL && md2 != NULL) {
    cdata1 = 
      (complexFloat *)CALLOC(md1->general->sample_count, sizeof(complexFloat));
    cdata2 = 
      (complexFloat *)CALLOC(md2->general->sample_count, sizeof(complexFloat));
    if (cdata1 == NULL || cdata2 == NULL ||
	(md1->general->data_type != md2->general->data_type)) {
      psnr->i.psnr = MISSING_PSNR;
      psnr->i.psnr_good = 0;
      psnr->q.psnr = MISSING_PSNR;
      psnr->q.psnr_good = 0;
    }
    else {
      asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  "
		     "Band %s in %s\n",
		     band_names1[band1], inFile1, band_names2[band2], inFile2);
      FILE *fp1 = fopen(inFile1, "rb");
      FILE *fp2 = fopen(inFile2, "rb");
      if (fp1 != NULL && fp2 != NULL) {
	sse_i = 0.0;
	sse_q = 0.0;
	max_val = get_maxval(md1->general->data_type);
	lines = MIN(md1->general->line_count, md2->general->line_count);
	if (lines < md1->general->line_count) {
	  asfPrintWarning("File2 has fewer lines than File1 (%d v. %d).\n"
			  "Only the first %d lines will be utilized for PSNR "
			  "calculation\n",
			  md2->general->line_count, md1->general->line_count, 
			  lines);
	}
	if (lines < md2->general->line_count) {
	  asfPrintWarning("File1 has fewer lines than File2 (%d v. %d).\n"
			  "Only the first %d lines will be utilized for PSNR "
			  "calculation\n",
			  md1->general->line_count, md2->general->line_count, 
			  lines);
	}
	samples = MIN(md1->general->sample_count, md2->general->sample_count);
	if (samples < md1->general->sample_count) {
	  asfPrintWarning("File2 has fewer samples per line than File1 "
			  "(%d v. %d).\nOnly the first %d samples within each "
			  "line of data will be utilized for PSNR "
			  "calculation\n",
			  md2->general->sample_count, 
			  md1->general->sample_count, samples);
	}
	if (samples < md2->general->sample_count) {
	  asfPrintWarning("File1 has fewer samples per line than File2 "
			  "(%d v. %d).\nOnly the first %d samples within each "
			  "line of data will be utilized for PSNR "
			  "calculation\n",
			  md1->general->sample_count, 
			  md2->general->sample_count, samples);
	}
	for (ii=0; ii<lines; ++ii) {
	  asfPercentMeter((double)ii/(double)lines);
	  get_complexFloat_line(fp1, md1, ii + offset1, cdata1);
	  get_complexFloat_line(fp2, md2, ii + offset2, cdata2);
	  for (jj=0; jj<samples; ++jj) {
	    sse_i += (cdata1[jj].imag - cdata2[jj].imag) *
	      (cdata1[jj].imag - cdata2[jj].imag);
	    sse_q += (cdata1[jj].real - cdata2[jj].real) *
	      (cdata1[jj].real - cdata2[jj].real);
	    pixel_count++;
	  }
	}
	asfPercentMeter(1.0);
	if (fp1) FCLOSE(fp1);
	if (fp2) FCLOSE(fp2);
	if (pixel_count > 0 && max_val > 0) {
	  rmse_i = sqrt(sse_i/pixel_count);
	  rmse_q = sqrt(sse_q/pixel_count);
	  psnr->i.psnr = 10.0 * log10(max_val/(rmse_i+.00000000000001));
	  psnr->i.psnr_good = 1;
	  psnr->q.psnr = 10.0 * log10(max_val/(rmse_q+.00000000000001));
	  psnr->q.psnr_good = 1;
	}
	else {
	  psnr->i.psnr = MISSING_PSNR;
	  psnr->i.psnr_good = 0;
	  psnr->q.psnr = MISSING_PSNR;
	  psnr->q.psnr_good = 0;
	}
      }
      else {
	psnr->i.psnr = MISSING_PSNR;
	psnr->i.psnr_good = 0;
	psnr->q.psnr = MISSING_PSNR;
	psnr->q.psnr_good = 0;
      }
    }
  }
  else {
    psnr->i.psnr = MISSING_PSNR;
    psnr->i.psnr_good = 0;
    psnr->q.psnr = MISSING_PSNR;
    psnr->q.psnr_good = 0;
  }
  
  // Cleanup and be gone
  FREE(f1);
  FREE(f2);
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
  FREE(cdata1);
  FREE(cdata2);
  if (md1 != NULL) meta_free(md1);
  if (md2 != NULL) meta_free(md2);
}

void calc_asf_img_stats_2files(char *inFile1, char *inFile2,
                               stats_t *inFile1_stats, stats_t *inFile2_stats,
                               psnr_t *psnr, int band)
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
  psnr->psnr = 0.0;

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
    band_names1 = 
      extract_band_names(md1->general->bands, md1->general->band_count);
    if (band_names1 != NULL) {
      asfPrintStatus("\nCalculating statistics for %s band in %s", 
		     band_names1[band], inFile1);
      calc_stats_rmse_from_file(inFile1, band_names1[band],
                                md1->general->no_data,
                                &s1->min, &s1->max, &s1->mean,
                                &s1->sdev, &s1->rmse, &s1->hist);
    }
  }
  if (band_count2 > 0 && md2 != NULL) {
    band_names2 = 
      extract_band_names(md2->general->bands, md2->general->band_count);
    if (band_names2 != NULL) {
      asfPrintStatus("\nCalculating statistics for %s band in %s", 
		     band_names2[band], inFile2);
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
      psnr->psnr = MISSING_PSNR;
      psnr->psnr_good = 0;
    }
    else {
      asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  "\
		     "Band %s in %s\n",
		     band_names1[band1], inFile1, band_names2[band2], inFile2);
      FILE *fp1 = fopen(inFile1, "rb");
      FILE *fp2 = fopen(inFile2, "rb");
      if (fp1 != NULL && fp2 != NULL) {
        sse = 0.0;
	// Since both file's data types are the same, this is OK
        max_val = get_maxval(md1->general->data_type);
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
          asfPrintWarning("File2 has fewer samples per line than File1 (%d v. "
			  "%d).\nOnly the first %d samples within each line of "
			  "data will be utilized for PSNR calculation\n",
			  md2->general->sample_count, 
			  md1->general->sample_count, samples);
        }
        if (samples < md2->general->sample_count) {
          asfPrintWarning("File1 has fewer samples per line than File2 (%d v. "
			  "%d).\nOnly the first %d samples within each line of "
			  "data will be utilized for PSNR calculation\n",
			  md1->general->sample_count, 
			  md2->general->sample_count, samples);
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
          psnr->psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
          psnr->psnr_good = 1;
        }
        else {
          psnr->psnr = MISSING_PSNR;
          psnr->psnr_good = 0;
        }
      }
      else {
        psnr->psnr = MISSING_PSNR;
        psnr->psnr_good = 0;
      }
    }
  }
  else {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
                            psnr_t *psnr, int band)
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
  psnr->psnr = MISSING_PSNR;
  psnr->psnr_good = 0;

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
                               psnr_t *psnr, int band)
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
  psnr->psnr = MISSING_PSNR;
  psnr->psnr_good = 0;

  ppm_pgm_image_band_statistics_from_file(inFile1, outfile, band, 
					  &s1->stats_good,
                                          &s1->min, &s1->max,
                                          &s1->mean, &s1->sdev, &s1->rmse,
                                          0, UINT8_IMAGE_DEFAULT_MASK);
  ppm_pgm_image_band_statistics_from_file(inFile2, outfile, band, 
					  &s2->stats_good,
                                          &s2->min, &s2->max,
                                          &s2->mean, &s2->sdev, &s2->rmse,
                                          0, UINT8_IMAGE_DEFAULT_MASK);
  ppm_pgm_image_band_psnr_from_files(inFile1, inFile2, outfile, band, band, 
				     psnr);
}

void calc_tiff_stats_2files(char *inFile1, char *inFile2,
                            stats_t *inFile1_stats, stats_t *inFile2_stats,
                            psnr_t *psnr, int band)
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
  psnr->psnr = MISSING_PSNR;
  psnr->psnr_good = 0;

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
  case ASF_BYTE:
    ret = pow(2, sizeof(unsigned char) * 8.0) - 1;
    break;
  case COMPLEX_BYTE:
    // HACK ALERT!  This assumes that COMPLEX_BYTE is
    // from raw data off a satellite and has only 5-bit
    // resolution... Probably a safe assumption for diffimage
    ret = pow(2, 5) - 1;
    break;
  case INTEGER16:
  case COMPLEX_INTEGER16:
    ret = pow(2, sizeof(short int) * 8.0) - 1;
    break;
  case INTEGER32:
  case COMPLEX_INTEGER32:
    ret = pow(2, sizeof(int) * 8.0) - 1;
    break;
  case REAL32:
  case COMPLEX_REAL32:
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
                         psnr_t psnr)
{
  asfPrintStatus("\nStatistics for %s%s\n"
		 "   min: %f\n"
		 "   max: %f\n"
		 "  mean: %f\n"
		 "  sdev: %f\n"
		 "  rmse: %f\n"
		 "result: %s\n",
		 band_str1, filename1, s1->min, s1->max, s1->mean, s1->sdev, 
		 s1->rmse, s1->stats_good ? "GOOD STATS" : "UNRELIABLE STATS");
  asfPrintStatus("\nStatistics for %s%s\n"
		 "   min: %f\n"
		 "   max: %f\n"
		 "  mean: %f\n"
		 "  sdev: %f\n"
		 "  rmse: %f\n"
		 "result: %s\n",
		 band_str2, filename2, s2->min, s2->max, s2->mean, s2->sdev, 
		 s2->rmse, s2->stats_good ? "GOOD STATS" : "UNRELIABLE STATS");
  asfPrintStatus("\nPSNR between files: %f\n\n", psnr.psnr);
}

void diff_check_stats(char *outputFile, char *inFile1, char *inFile2,
                      stats_t *stats1, stats_t *stats2, psnr_t *psnr,
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
  int output_file_exists = 0;
  if (outputFile && strlen(outputFile) > 0) {
    outputFP = (FILE*)FOPEN(outputFile, "a");
    if (outputFP) {
      output_file_exists = 1;
    }
    else {
      outputFP = stderr;
      output_file_exists = 0;
    }
  }
  else {
    outputFP = stderr;
    output_file_exists = 0;
  }
  
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
    // produce an empty output file (handy for scripts that check for file 
    // existence AND file size greater than zero)
    // Compare statistics
    // Assume 6-sigma range (99.999999%) is full range of data
    baseline_range1 = fabs(stats1[band].sdev) * 6.0; 
    // Assume 6-sigma range (99.999999%) is full range of dat
    baseline_range2 = fabs(stats2[band].sdev) * 6.0; 
    min_tol = (MIN_DIFF_TOL/100.0)*baseline_range1;
    max_tol = (MAX_DIFF_TOL/100.0)*baseline_range1;
    mean_tol = (MEAN_DIFF_TOL/100.0)*baseline_range1;
    sdev_tol = (SDEV_DIFF_TOL/100.0)*baseline_range1;
    // FIXME: Rather than use data_type, use the baseline range to develop 
    // a suitable PSNR tolerance
    switch (data_type) {
      case ASF_BYTE:
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
         psnr[band].psnr < psnr_tol))
    {
      // Strict comparison utilizes all values
      fprintf(outputFP, "\n-----------------------------------------------\n");

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
      fprintf(outputFP, "%s", msg);

      sprintf(msg, "[%s] [min]   File1: %12f,  File2: %12f, Tolerance: %11f "
	      "(%3f Percent)\n", min_diff > min_tol ? "FAIL" : "PASS",
              stats1[band].min, stats2[band].min, min_tol, MIN_DIFF_TOL);
      fprintf(outputFP, "%s", msg);

      sprintf(msg, "[%s] [max]   File1: %12f,  File2: %12f, Tolerance: %11f "
	      "(%3f Percent)\n", max_diff > max_tol ? "FAIL" : "PASS",
              stats1[band].max, stats2[band].max, max_tol, MAX_DIFF_TOL);
      fprintf(outputFP, "%s", msg);

      sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: %11f "
	      "(%3f Percent)\n", mean_diff > mean_tol ? "FAIL" : "PASS",
              stats1[band].mean, stats2[band].mean, mean_tol, MEAN_DIFF_TOL);
      fprintf(outputFP, "%s", msg);

      sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: %11f "
	      "(%3f Percent)\n", sdev_diff > sdev_tol ? "FAIL" : "PASS",
              stats1[band].sdev, stats2[band].sdev, sdev_tol/6.0, 
	      SDEV_DIFF_TOL);
      fprintf(outputFP, "%s", msg);

      if (psnr[band].psnr_good) {
        sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                    "
		"PSNR Minimum: %11f (higher == better)\n",
                psnr[band].psnr < psnr_tol ? "FAIL" : "PASS",
                psnr[band].psnr, psnr_tol);
      }
      else {
        sprintf(msg, "[FAIL] [PSNR]   PSNR:      MISSING,                    "
		"PSNR Minimum: %11f (higher == better)\n", psnr_tol);
      }
      fprintf(outputFP, "%s", msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
    }
    else if (!strict &&
             (stats1[band].stats_good && stats2[band].stats_good) &&
             (mean_diff > mean_tol ||
              sdev_diff > sdev_tol ||
              psnr[band].psnr < psnr_tol)) {
      // If not doing strict checking, skip comparing min and max values
      fprintf(outputFP, "\n-----------------------------------------------\n");

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
      fprintf(outputFP, "%s", msg);

      sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: %11f "
	      "(%3f Percent)\n", mean_diff > mean_tol ? "FAIL" : "PASS",
              stats1[band].mean, stats2[band].mean, mean_tol, MEAN_DIFF_TOL);
      fprintf(outputFP, "%s", msg);

      sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: %11f "
	      "(%3f Percent)\n", sdev_diff > sdev_tol ? "FAIL" : "PASS",
              stats1[band].sdev, stats2[band].sdev, sdev_tol/6.0, 
	      SDEV_DIFF_TOL);
      fprintf(outputFP, "%s", msg);

      if (psnr[band].psnr_good) {
        sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                    "
		"PSNR Minimum: %11f (higher == better)\n",
                psnr[band].psnr < psnr_tol ? "FAIL" : "PASS",
                psnr[band].psnr, psnr_tol);
      }
      else {
        sprintf(msg, "[FAIL] [PSNR]   PSNR:      MISSING,                    "
		"PSNR Minimum: %11f (higher == better)\n", psnr_tol);
      }
      fprintf(outputFP, "%s", msg);

      fprintf(outputFP, "-----------------------------------------------\n\n");
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
      asfPrintStatus("\nNo differences found in Image Statistics when "
		     "comparing\n  %s%s to\n  %s%s\n\n",
		     band_str1, inFile1, band_str2, inFile2);
      print_stats_results(inFile1, inFile2, band_str1, band_str2,
                          &stats1[band], &stats2[band], psnr[band]);
    }

    if (!stats1[band].stats_good || !stats2[band].stats_good) {
      char msg[1024];

      fprintf(outputFP, "\n-----------------------------------------------\n");

      if (num_bands > 1) {
        sprintf(band_str1, "Band %s in ", band_names1[band]);
        sprintf(band_str2, "Band %s in ", band_names2[band]);
      }
      else {
        strcpy(band_str1, "");
        strcpy(band_str2, "");
      }
      if (!stats1[band].stats_good) {
        sprintf(msg, "FAIL: %s%s image statistics missing.\n", 
		band_str1, inFile1);
        fprintf(outputFP, "%s", msg);
      }
      if (!stats2[band].stats_good) {
        sprintf(msg, "FAIL: %s%s image statistics missing.\n", 
		band_str2, inFile2);
        fprintf(outputFP, "%s", msg);
      }

      fprintf(outputFP, "-----------------------------------------------\n\n");
    }
  } // For each band

  if (output_file_exists && outputFP) FCLOSE(outputFP);
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
    fprintf(outputFP, "%s", msg);

    fprintf(outputFP, "-----------------------------------------------\n\n");

    if (outputFP) FCLOSE(outputFP);
  }
  else {
      fprintf(stderr, "\n-----------------------------------------------\n");

      sprintf(msg, "FAIL: %s\n", err_msg);
      fprintf(stderr, "%s", msg);

      fprintf(stderr, "-----------------------------------------------\n\n");
  }
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
                       &t->is_scanline_format,
                       &t->is_palette_color_tiff,
                       REPORT_LEVEL_WARNING);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &t->height);
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &t->width);
  if (t->planar_config != PLANARCONFIG_CONTIG &&
      t->planar_config != PLANARCONFIG_SEPARATE &&
      t->num_bands == 1) {
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
      citation_length = GTIFKeyInfo(gtif, GTCitationGeoKey, &typeSize, 
				    &citation_type);
      if (citation_length > 0) {
        g->GTcitation = (char*)MALLOC(citation_length * typeSize);
        GTIFKeyGet(gtif, GTCitationGeoKey, g->GTcitation, 0, citation_length);
      }
      else {
        g->GTcitation = NULL;
      }
      citation_length = GTIFKeyInfo(gtif, PCSCitationGeoKey, &typeSize, 
				    &citation_type);
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
      (gtif->gt_methods.get)
	(gtif->gt_tif, GTIFF_TIEPOINTS, &count, &g->tie_point);
      if (count >= 6) {
        g->gtif_data_exists = 1;
        g->tie_point_elements = count;
        g->num_tie_points = count / 6;
      }
      (gtif->gt_methods.get)
	(gtif->gt_tif, GTIFF_PIXELSCALE, &count, &g->pixel_scale);
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
      read_count = GTIFKeyGet (gtif, ProjLinearUnitsGeoKey, &g->linear_units, 
			       0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;

      // Get UTM related info if it exists
      read_count = GTIFKeyGet(gtif, ProjectedCSTypeGeoKey, &g->pcs, 0, 1);
      if (read_count == 1 && 
	  PCS_2_UTM(g->pcs, &g->hemisphere, &g->datum, &g->pro_zone)) {
        g->gtif_data_exists = 1;
      }
      else {
        read_count = GTIFKeyGet(gtif, ProjectionGeoKey, &g->pcs, 0, 1);
        if (read_count == 1 && 
	    PCS_2_UTM(g->pcs, &g->hemisphere, &g->datum, &g->pro_zone)) {
          g->gtif_data_exists = 1;
        }
        else {
          g->hemisphere = '\0';
          g->datum = UNKNOWN_DATUM;
          g->pro_zone = MISSING_GTIF_DATA;
        }
      }

      // Get projection type (ProjCoordTransGeoKey) and other projection parameters
      read_count = 
	GTIFKeyGet(gtif, ProjCoordTransGeoKey, &g->proj_coords_trans, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->proj_coords_trans = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet(gtif, GeographicTypeGeoKey, &g->geographic_datum, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->geographic_datum = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet(gtif, GeogGeodeticDatumGeoKey, &g->geodetic_datum, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->geodetic_datum = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet(gtif, ProjScaleAtNatOriginGeoKey, &g->scale_factor, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->scale_factor = MISSING_GTIF_DATA;

      // Get generic projection parameters (Note: projection type is defined by
      // the g->proj_coords_trans value)
      read_count = 
	GTIFKeyGet (gtif, ProjFalseEastingGeoKey, &g->false_easting, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->false_easting = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjFalseNorthingGeoKey, &g->false_northing, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->false_northing = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjNatOriginLongGeoKey, &g->natLonOrigin, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->natLonOrigin = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjNatOriginLatGeoKey, &g->natLatOrigin, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->natLatOrigin = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjStdParallel1GeoKey, &g->stdParallel1, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->stdParallel1 = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjStdParallel2GeoKey, &g->stdParallel2, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->stdParallel2 = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjCenterLongGeoKey, &g->lonCenter, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->lonCenter = MISSING_GTIF_DATA;
      read_count = GTIFKeyGet (gtif, ProjCenterLatGeoKey, &g->latCenter, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->latCenter = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjFalseOriginLongGeoKey, &g->falseOriginLon, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->falseOriginLon = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjFalseOriginLatGeoKey, &g->falseOriginLat, 0, 1);
      if (read_count >= 1) g->gtif_data_exists = 1;
      else g->falseOriginLat = MISSING_GTIF_DATA;
      read_count = 
	GTIFKeyGet (gtif, ProjStraightVertPoleLongGeoKey, &g->lonPole, 0, 1);
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
  unsigned long dummy_zone;
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
    case LAT_LONG_PSEUDO_PROJECTION:
      break;
    default:
      failed=1;
      break;
  }

  ///////////////// Report failures /////////////////
  ////
  // Report results if the comparisons failed
  int out_file_exists = 0;
  if (outfile && strlen(outfile) > 0) {
    outputFP = fopen(outfile, "a");
    if (outputFP == NULL) {
      // Failed to open output file
      asfPrintWarning("Cannot open output file for reporting geotiff file "
		      "differences.  Output\n"
		      "will be directed to stderr (only)\n");
    }
    else {
      out_file_exists = 1;
    }
  }
  else {
    outputFP = stderr;
    out_file_exists = 0;
  }
  if (failed) {
    char msg[1024];

    fprintf(outputFP, "\n-----------------------------------------------\n");

    // Report results based on projection type
    if (!g1->gtif_data_exists) {
      sprintf(msg, "ERROR: GeoTIFF data not found in File1\n");
      fprintf(outputFP, "%s", msg);
    }
    if (!g2->gtif_data_exists) {
      sprintf(msg, "ERROR: GeoTIFF data not found in File2\n");
      fprintf(outputFP, "%s", msg);
    }
    if (projection_type1 != projection_type2) {
      char type_str1[256], type_str2[256];
      projection_type_2_str(projection_type1, type_str1);
      projection_type_2_str(projection_type2, type_str2);
      sprintf(msg, "Projection type found in File1\n  %s\nnot equal to "
	      "projection type in File2\n  %s\n", type_str1, type_str2);
      fprintf(outputFP, "%s", msg);
    }
    if (g1->tie_point_elements != g2->tie_point_elements ||
        g1->tie_point_elements % 6 != 0 ||
        g2->tie_point_elements % 6 != 0) {
      sprintf(msg,"Input files have differing or invalid numbers of tie point "
	      "elements:\n"
	      "  File1: %d tie point elements\n"
	      "  File2: %d tie point elements\n",
	      g1->tie_point_elements, g2->tie_point_elements);
      fprintf(outputFP, "%s", msg);
    }
    if (g1->num_tie_points != g2->num_tie_points) {
      sprintf(msg,"Input files have differing numbers of tie points:\n"
	      "  File1: %d tie points\n"
              "  File2: %d tie points\n",
	      g1->num_tie_points, g2->num_tie_points);
      fprintf(outputFP, "%s", msg);
    }
    for (i=0; i<num_tie_points; i++) {
      if (g1->tie_point[i] - g2->tie_point[i] > PROJ_LOC_DIFF_TOL_m) {
        char tmp1[16], tmp2[16];
        sprintf(tmp1,"%f", g1->tie_point[i]);
        sprintf(tmp2,"%f", g2->tie_point[i]);
        sprintf(msg, "  Tie point element #%d differs:\n"
                "    File1: %s\n"
                "    File2: %s\n",
                i, g1->tie_point[i] != MISSING_GTIF_DATA ? tmp1 : "MISSING",
                g2->tie_point[i] != MISSING_GTIF_DATA ? tmp2 : "MISSING");
        fprintf(outputFP, "%s", msg);
      }
    }
    if (g1->pixel_scale_elements != g2->pixel_scale_elements ||
        g1->pixel_scale_elements % 3 != 0 ||
        g2->pixel_scale_elements % 3 != 0) {
      sprintf(msg,"Input files have differing or invalid numbers of pixel "
	      "scale elements:\n"
	      "  File1: %d pixel scale elements\n"
	      "  File2: %d pixel scale elements\n",
	      g1->pixel_scale_elements, g2->pixel_scale_elements);
      fprintf(outputFP, "%s", msg);
    }
    if (g1->num_pixel_scales != g2->num_pixel_scales) {
      sprintf(msg,"Input files have differing numbers of pixel scales:\n"
	      "  File1: %d pixel scales\n"
	      "  File2: %d pixel scales\n",
	      g1->num_pixel_scales, g2->num_pixel_scales);
      fprintf(outputFP, "%s", msg);
    }
    for (i=0; i<num_pixel_scales; i++) {
      if (g1->pixel_scale[i] - g2->pixel_scale[i] > PROJ_LOC_DIFF_TOL_m) {
        char tmp1[16], tmp2[16];
        sprintf(tmp1,"%f", g1->pixel_scale[i]);
        sprintf(tmp2,"%f", g2->pixel_scale[i]);
        sprintf(msg, "  Pixel scale element #%d differs:\n"
		"    File1: %s\n"
		"    File2: %s\n", i,
		g1->pixel_scale[i] != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		g2->pixel_scale[i] != MISSING_GTIF_DATA ? tmp2 : "MISSING");
        fprintf(outputFP, "%s", msg);
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
	  fprintf(outputFP, "%s", msg);
	}
	if (g1->pcs != g2->pcs) {
	  char tmp1[16], tmp2[16], *tmp3, *tmp4;
	  sprintf(tmp1,"%d", g1->pcs);
	  sprintf(tmp2,"%d", g2->pcs);
	  tmp3 = pcs2description(g1->pcs);
	  tmp4 = pcs2description(g2->pcs);
	  sprintf(msg, "  PCS value from ProjectedCSTypeGeoKey or "
		  "ProjectionGeoKey differ:\n"
		  "    File1: %s (%s)\n"
		  "    File2: %s (%s)\n",
		  g1->pcs != MISSING_GTIF_DATA ? tmp1 : "MISSING", tmp3,
		  g2->pcs != MISSING_GTIF_DATA ? tmp2 : "MISSING", tmp4);
	  FREE(tmp3);
	  FREE(tmp4);
	  fprintf(outputFP, "%s", msg);
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
	  fprintf(outputFP, "%s", msg);
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
	  sprintf(msg, "Albers Equal Area Projections with differences found:"
		  "\n\n");
	  fprintf(outputFP, "%s", msg);
	  if (g1->proj_coords_trans != g2->proj_coords_trans) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%d", g1->proj_coords_trans);
	    sprintf(tmp2,"%d", g2->proj_coords_trans);
	    sprintf(msg, "  Projection type code from ProjCoordTransGeoKey "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->false_easting != g2->false_easting) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->false_easting);
	    sprintf(tmp2,"%f", g2->false_easting);
	    sprintf(msg, "  False eastings differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->false_easting != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->false_easting != 
		    MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->false_northing != g2->false_northing) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->false_northing);
	    sprintf(tmp2,"%f", g2->false_northing);
	    sprintf(msg, "  False northings differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->false_northing != 
		    MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->false_northing != 
		    MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->natLonOrigin != g2->natLonOrigin) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->natLonOrigin);
	    sprintf(tmp2,"%f", g2->natLonOrigin);
	    sprintf(msg, "  Central meridians (from ProjNatOriginLongGeoKey) "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->natLonOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->natLonOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->lonCenter != g2->lonCenter) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->lonCenter);
	    sprintf(tmp2,"%f", g2->lonCenter);
	    sprintf(msg, "  Central meridians (from ProjCenterLongGeoKey) "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->lonCenter != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->lonCenter != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->natLatOrigin != g2->natLatOrigin) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->natLatOrigin);
	    sprintf(tmp2,"%f", g2->natLatOrigin);
	    sprintf(msg, "  Latitudes of Origin (from ProjNatOriginLatGeoKey)"
		    " differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->natLatOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->natLatOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
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
	  sprintf(msg, "Lambert Conformal Conic Projections with differences "
		  "found: \n\n");
	  fprintf(outputFP, "%s", msg);
	  if (g1->proj_coords_trans != g2->proj_coords_trans) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%d", g1->proj_coords_trans);
	    sprintf(tmp2,"%d", g2->proj_coords_trans);
	    sprintf(msg, "  Projection type code from ProjCoordTransGeoKey "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->natLonOrigin != g2->natLonOrigin) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->natLonOrigin);
	    sprintf(tmp2,"%f", g2->natLonOrigin);
	    sprintf(msg, "  Longitudes of Origin (from ProjNatOriginLongGeoKey)"
		    " differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->natLonOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->natLonOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->natLatOrigin != g2->natLatOrigin) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->natLatOrigin);
	    sprintf(tmp2,"%f", g2->natLatOrigin);
	    sprintf(msg, "  Latitudes of Origin (from ProjNatOriginLatGeoKey) "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->natLatOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->natLatOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->falseOriginLon != g2->falseOriginLon) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->falseOriginLon);
	    sprintf(tmp2,"%f", g2->falseOriginLon);
	    sprintf(msg, "  Longitudes of Origin (from "
		    "ProjFalseOriginLongGeoKey) differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->falseOriginLon != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->falseOriginLon != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->falseOriginLat != g2->falseOriginLat) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->falseOriginLat);
	    sprintf(tmp2,"%f", g2->falseOriginLat);
	    sprintf(msg, "  Latitudes of Origin (from ProjFalseOriginLatGeoKey)"
		    " differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->falseOriginLat != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->falseOriginLat != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	}
	break;
      case POLAR_STEREOGRAPHIC:
	if (g1->proj_coords_trans != g2->proj_coords_trans  ||
	    g1->natLatOrigin != g2->natLatOrigin          ||
	    g1->lonPole != g2->lonPole                    ||
	    g1->false_easting != g2->false_easting        ||
	    g1->false_northing != g2->false_northing) {
	  sprintf(msg, "Polar Stereographic Projections with differences found:"
		  " \n\n");
	  fprintf(outputFP, "%s", msg);
	  if (g1->proj_coords_trans != g2->proj_coords_trans) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%d", g1->proj_coords_trans);
	    sprintf(tmp2,"%d", g2->proj_coords_trans);
	    sprintf(msg, "  Projection type code from ProjCoordTransGeoKey "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->natLatOrigin != g2->natLatOrigin) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->natLatOrigin);
	    sprintf(tmp2,"%f", g2->natLatOrigin);
	    sprintf(msg, "  Latitudes of Origin (from ProjNatOriginLatGeoKey) "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->natLatOrigin != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->natLatOrigin != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->lonPole != g2->lonPole) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->lonPole);
	    sprintf(tmp2,"%f", g2->lonPole);
	    sprintf(msg, "  Longitudes of Straight Vertical Pole (from "
		    "ProjStraightVertPoleLongGeoKey) differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->lonPole != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->lonPole != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	  sprintf(msg, "Lambert Azimuthal Equal Area Projections with "
		  "differences found: \n\n");
	  fprintf(outputFP, "%s", msg);
	  if (g1->proj_coords_trans != g2->proj_coords_trans) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%d", g1->proj_coords_trans);
	    sprintf(tmp2,"%d", g2->proj_coords_trans);
	    sprintf(msg, "  Projection type code from ProjCoordTransGeoKey "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->proj_coords_trans != 
		    MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
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
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->lonCenter != g2->lonCenter) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->lonCenter);
	    sprintf(tmp2,"%f", g2->lonCenter);
	    sprintf(msg, "  Central meridians (from ProjCenterLongGeoKey) "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->lonCenter != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->lonCenter != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	  if (g1->latCenter != g2->latCenter) {
	    char tmp1[16], tmp2[16];
	    sprintf(tmp1,"%f", g1->latCenter);
	    sprintf(tmp2,"%f", g2->latCenter);
	    sprintf(msg, "  Latitudes of Origin (from ProjCenterLatGeoKey) "
		    "differ:\n"
		    "    File1: %s\n"
		    "    File2: %s\n",
		    g1->latCenter != MISSING_GTIF_DATA ? tmp1 : "MISSING",
		    g2->latCenter != MISSING_GTIF_DATA ? tmp2 : "MISSING");
	    fprintf(outputFP, "%s", msg);
	  }
	}
	break;
      case LAT_LONG_PSEUDO_PROJECTION:
	break;
      default:
	// Should never reach this code
	sprintf(msg, "Found unsupported or missing projection type\n\n");
	fprintf(outputFP, "%s", msg);
	break;
      }
      }
    
    fprintf(outputFP, "-----------------------------------------------\n\n");
  }
  
  if (failed && out_file_exists && outputFP != NULL) {
    FCLOSE(outputFP);
  }
}

void projection_type_2_str(projection_type_t proj, char *proj_str)
{
  switch (proj) 
    {
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
    case LAT_LONG_PSEUDO_PROJECTION:
      strcpy(proj_str, "Geographic");
      break;
    default:
      strcpy(proj_str, "Unknown");
      break;
    }
}

int tiff_image_band_statistics_from_file(char *inFile, int band_no,
                                         int *stats_exist,
                                         double *min, double *max,
                                         double *mean, double *sdev, 
					 double *rmse,
                                         int use_mask_value, float mask_value)
{
  tiff_data_t t;
  tsize_t scanlineSize;
  
  *stats_exist = 1; // Innocent until presumed guilty...
  get_tiff_info_from_file(inFile, &t);
  // Note: For single-plane (greyscale) images, planar_config will remain 
  // unset so we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0) {
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
      t.planar_config != PLANARCONFIG_SEPARATE) {
    if (tif) XTIFFClose(tif);
    *stats_exist = 0;
    return 1;
  }
  float *buf = (float*)MALLOC(t.width * sizeof(float));
  
  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all rows in the TIFF
    for ( ii = 0; ii < t.height; ii++ ) {
      // Planar configuration is chunky, e.g. interlaced pixels, rgb rgb etc.
      asfPercentMeter((double)ii/(double)t.height);
      tiff_get_float_line(tif, buf, ii, band_no);
      for (jj = 0 ; jj < t.width; jj++ ) {
	// iterate over each pixel sample in the scanline
	cs = buf[jj];
	if ( !isnan(mask_value) && 
	     (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
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
    for ( ii = 0; ii < t.height; ii++ ) {
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
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0) {
    if (buf) free(buf);
    if (tif) XTIFFClose(tif);
    *stats_exist = 0;
    return 1;
  }
  
  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  // This assumes the sample count is large enough to ensure that the sample 
  // standard deviation is very very similar to the population standard 
  // deviation.
  *rmse = *sdev; 
  
  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX) {
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
                                     psnr_t *psnr)
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
      t1.width == 0) {
    asfPrintWarning("Missing TIFF header values in %s\n", inFile1);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
      t2.width == 0) {
    asfPrintWarning("Missing TIFF header values in %s\n", inFile2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
    asfPrintWarning("Invalid band number for file1 (%d v. %d) or file2 (%d v. "
		    "%d)\n", band1, t1.num_bands, band2, t2.num_bands);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (t1.height != t2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. "
		    "%d).\nPSNR calculation will only use the first %d rows "
		    "from each file.\n", t1.height, t2.height, height);
  }
  if (t1.width != t2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row "
		    "(%d v. %d).\nPSNR calculation will only use the first %d "
		    "pixels from each row of data.\n", 
		    t1.width, t2.width, width);
  }
  
  if (t1.data_type != t2.data_type) {
    asfPrintWarning("TIFF files have different data types.\n");
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  
  get_band_names(inFile1, NULL, &band_names1, &num_extracted_names1);
  get_band_names(inFile2, NULL, &band_names2, &num_extracted_names2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s "
		 "in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);
  
  TIFF *tif1 = XTIFFOpen(inFile1, "rb");
  if (tif1 == NULL) {
    asfPrintWarning("Cannot open %s\n", inFile1);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  scanlineSize1 = TIFFScanlineSize(tif1);
  if (scanlineSize1 <= 0) {
    asfPrintWarning("Invalid scanline size (%d)\n", scanlineSize1);
    if (tif1) XTIFFClose(tif1);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (t1.num_bands > 1 &&
      t1.planar_config != PLANARCONFIG_CONTIG &&
      t1.planar_config != PLANARCONFIG_SEPARATE) {
    asfPrintWarning("Invalid planar configuration in %s\n", inFile1);
    if (tif1) XTIFFClose(tif1);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  TIFF *tif2 = XTIFFOpen(inFile2, "rb");
  if (tif2 == NULL) {
    asfPrintWarning("Cannot open %s\n", inFile2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  scanlineSize2 = TIFFScanlineSize(tif2);
  if (scanlineSize2 <= 0) {
    asfPrintWarning("Invalid scanline size (%d)\n", scanlineSize2);
    if (tif2) XTIFFClose(tif2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (t2.num_bands > 1 &&
      t2.planar_config != PLANARCONFIG_CONTIG &&
      t2.planar_config != PLANARCONFIG_SEPARATE) {
    asfPrintWarning("Invalid planar configuration in %s\n", inFile2);
    if (tif2) XTIFFClose(tif2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  float *buf1 = (float*)CALLOC(t1.width, sizeof(float));
  float *buf2 = (float*)CALLOC(t2.width, sizeof(float));
  if (buf1 == NULL || buf2 == NULL) {
    asfPrintWarning("Cannot allocate memory for TIFF data buffers.\n");
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    if (buf1) FREE(buf1);
    if (buf2) FREE(buf2);
    return;
  }
  
  sse = 0.0;
  // Since both file's data types are the same, this is OK
  max_val = get_maxval(t1.data_type); 
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
    psnr->psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
    psnr->psnr_good = 1;
  }
  else {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
  // Note: For single-plane (greyscale) images, planar_config may remain unset
  // so we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0) {
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
    switch(t.bits_per_sample) 
      {
      case 8:
	switch(t.sample_format) {
	case SAMPLEFORMAT_UINT:
	  if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	    // Current sample.
	    buf[col] = (float)(((uint8*)(tif_buf))[(col*t.num_bands)+band_no]);
	  }
	  else {
	    // Planar configuration is band-sequential or single-banded
	    buf[col] = (float)(((uint8*)(tif_buf))[col]);
	  }
	  break;
	case SAMPLEFORMAT_INT:
	  if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	    // Current sample.
	    buf[col] = (float)(((int8*)(tif_buf))[(col*t.num_bands)+band_no]);
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
	  asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF "
			"file.\n");
	  break;
	}
	break;
      case 16:
	switch(t.sample_format) 
	  {
	  case SAMPLEFORMAT_UINT:
	    if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	      // Current sample.
	      buf[col] = 
		(float)(((uint16*)(tif_buf))[(col*t.num_bands)+band_no]);
	    }
	    else {
	      // Planar configuration is band-sequential or single-banded
	      buf[col] = (float)(((uint16*)(tif_buf))[col]); // Current sample.
	    }
	    break;
	  case SAMPLEFORMAT_INT:
	    if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	      // Current sample.
	      buf[col] = 
		(float)(((int16*)(tif_buf))[(col*t.num_bands)+band_no]);
	    }
	    else {
	      // Planar configuration is band-sequential or single-banded
	      buf[col] = (float)(((uint16*)(tif_buf))[col]); // Current sample.
	    }
	    break;
	  default:
	    // There is no such thing as an IEEE 16-bit floating point
	    if (tif_buf) _TIFFfree(tif_buf);
	    if (tif) XTIFFClose(tif);
	    asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF "
			  "file.\n");
	    break;
	  }
	break;
      case 32:
	switch(t.sample_format) 
	  {
	  case SAMPLEFORMAT_UINT:
	    if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	      // Current sample.
	      buf[col] = 
		(float)(((uint32*)(tif_buf))[(col*t.num_bands)+band_no]);
	    }
	    else {
	      // Planar configuration is band-sequential or single-banded
	      buf[col] = (float)(((uint32*)(tif_buf))[col]); // Current sample.
	    }
	    break;
	  case SAMPLEFORMAT_INT:
	    if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	      // Current sample.
	      buf[col] = (float)(((long*)(tif_buf))[(col*t.num_bands)+band_no]);
	    }
	    else {
	      // Planar configuration is band-sequential or single-banded
	      buf[col] = (float)(((long*)(tif_buf))[col]); // Current sample.
	    }
	    break;
	  case SAMPLEFORMAT_IEEEFP:
	    if (t.planar_config == PLANARCONFIG_CONTIG && t.num_bands > 1) {
	      // Current sample.
	      buf[col] = 
		(float)(((float*)(tif_buf))[(col*t.num_bands)+band_no]);
	    }
	    else {
	      // Planar configuration is band-sequential or single-banded
	      buf[col] = (float)(((float*)(tif_buf))[col]);   // Current sample.
	    }
	    break;
	  default:
	    if (tif_buf) _TIFFfree(tif_buf);
	    if (tif) XTIFFClose(tif);
	    asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF "
			  "file.\n");
	    break;
	  }
	break;
      default:
	if (tif_buf) _TIFFfree(tif_buf);
	if (tif) XTIFFClose(tif);
	asfPrintError("tiff_get_float_line(): Unexpected data type in TIFF "
		      "file.\n");
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
  // Note: For single-plane (greyscale) images, planar_config may remain unset
  // so  we can't use it as a guide for checking TIFF validity...
  if (t.sample_format == MISSING_TIFF_DATA ||
      t.bits_per_sample == MISSING_TIFF_DATA ||
      t.data_type == 0 ||
      t.num_bands == MISSING_TIFF_DATA ||
      t.is_scanline_format == MISSING_TIFF_DATA ||
      t.height == 0 ||
      t.width == 0) {
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
  int bit_depth, color_type, nbands, interlace_type, compression_type;
  int filter_type;
  unsigned char sig[8];
  char msg[1024];
  FILE *pngFP, *outputFP;
  
  // Init stuff
  int output_file_exists = 0;
  pngFP = (FILE*)FOPEN(inFile,"rb");
  if (outfile && strlen(outfile) > 0) {
    outputFP = (FILE*)FOPEN(outfile, "a");
    if (outputFP) {
      output_file_exists = 1;
    }
    else {
      outputFP = stderr;
      output_file_exists = 0;
    }
  }
  else {
    outputFP = stderr;
    output_file_exists = 0;
  }
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
    sprintf(msg, "Invalid PNG file version (%0.4f) found.  Only version "
	    "1.xxxx is supported\n",
	    (float)png_access_version_number()/10000.0);
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  // Important: Leaves file pointer offset into file by 8 bytes for png lib
  fread(sig, 1, 8, pngFP); 
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n", "file type header bytes invalid");
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_init_io(png_ptr, pngFP);
  // Because of the sig-reading offset ...must do this for PNG lib
  png_set_sig_bytes(png_ptr, 8); 
  
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
      bit_depth != 16) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file bit depth found (%d).\n"
	    "Must be 1, 2, 4, 8, or 16.\n", bit_depth);
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (color_type != PNG_COLOR_TYPE_GRAY &&
      color_type != PNG_COLOR_TYPE_GRAY_ALPHA &&
      color_type != PNG_COLOR_TYPE_PALETTE &&
      color_type != PNG_COLOR_TYPE_RGB &&
      color_type != PNG_COLOR_TYPE_RGB_ALPHA &&
      color_type != PNG_COLOR_MASK_PALETTE &&
      color_type != PNG_COLOR_MASK_COLOR &&
      color_type != PNG_COLOR_MASK_ALPHA) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file color type found.\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (filter_type != PNG_FILTER_TYPE_BASE &&
      filter_type != PNG_INTRAPIXEL_DIFFERENCING) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file filter type found.\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (compression_type != PNG_COMPRESSION_TYPE_BASE) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file compression type found.\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (interlace_type != PNG_INTERLACE_NONE &&
      interlace_type != PNG_INTERLACE_ADAM7) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "Invalid PNG file interlace type found.\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
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
  switch (bit_depth) 
    {
    case 1:
    case 2:
    case 4:
      ihdr->data_type=0; // UNKNOWN TYPE
      break;
    case 8:
      ihdr->data_type=ASF_BYTE;
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
  if (output_file_exists && outputFP) FCLOSE(outputFP);
}

void calc_png_stats_2files(char *inFile1, char *inFile2, char *outfile,
                           stats_t *inFile1_stats, stats_t *inFile2_stats,
                           psnr_t *psnr, int band)
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
  psnr->psnr = MISSING_PSNR;
  psnr->psnr_good = 0;
  
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
                                        double *mean, double *sdev, 
					double *rmse,
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
      ihdr.width == 0) {
    *stats_exist = 0;
    return 1;
  }
  
  // Initialize PNG file for read
  int output_file_exists = 0;
  pngFP = (FILE*)FOPEN(inFile,"rb");
  if (outfile && strlen(outfile) > 0) {
    outputFP = (FILE*)FOPEN(outfile, "a");
    if (outputFP) {
      output_file_exists = 1;
    }
    else {
      outputFP = stderr;
      output_file_exists = 0;
    }
  }
  else {
    outputFP = stderr;
    output_file_exists = 0;
  }
  // Important: Leaves file pointer offset into file by 8 bytes for png lib
  fread(sig, 1, 8, pngFP); 
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n", "file type header bytes invalid");
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  if (setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_init_io(png_ptr, pngFP);
  // Because of the sig-reading offset ...must do this for PNG lib
  png_set_sig_bytes(png_ptr, 8); 
  
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
    for ( ii = 0; ii < ihdr.height; ii++ ) {
      // Planar configuration is chunky, e.g. interlaced pixels, rgb rgb etc.
      asfPercentMeter((double)ii/(double)ihdr.height);
      png_sequential_get_float_line(png_ptr, info_ptr, buf, band_no);
      for (jj = 0 ; jj < ihdr.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if (!isnan(mask_value) && 
	    (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
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
    for ( ii = 0; ii < ihdr.height; ii++ ) {
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
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0) {
    if (buf) free(buf);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    *stats_exist = 0;
    return 1;
  }
  
  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  // This assumes the sample count is large enough to ensure that the sample 
  // standard deviation is very very similar to the population standard 
  // deviation.
  *rmse = *sdev; 

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX) {
    if (buf) free(buf);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP) FCLOSE(pngFP);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    *stats_exist = 0;
    return 1;
  }
  
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  if (buf) free(buf);
  if (output_file_exists && outputFP) FCLOSE(outputFP);
  if (pngFP) FCLOSE(pngFP);
  return 0;
}

void png_image_band_psnr_from_files(char *inFile1, char *inFile2, char *outfile,
				    int band1, int band2, psnr_t *psnr)
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

  int output_file_exists = 0;
  if (outfile && strlen(outfile) > 0) {
    outputFP = (FILE*)FOPEN(outfile, "a");
    if (outputFP) {
      output_file_exists = 1;
    }
    else {
      outputFP = stderr;
      output_file_exists = 0;
    }
  }
  else {
    outputFP = stderr;
    output_file_exists = 0;
  }

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
      ihdr1.width == 0) {
    sprintf(msg, "Cannot read PNG file header in file1\n  %s\n", inFile1);
    if (outputFP) fprintf(outputFP, "%s", msg);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
      ihdr2.width == 0) {
    sprintf(msg, "Cannot read PNG file header in file2\n  %s\n", inFile2);
    if (outputFP) fprintf(outputFP, "%s", msg);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }

  // Initialize PNG file for read
  pngFP1 = (FILE*)FOPEN(inFile1,"rb");
  // Important: Leaves file pointer offset into file by 8 bytes for png lib
  fread(sig, 1, 8, pngFP1); 
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n  %s\n", "file type header bytes "
	    "invalid", inFile1);
    fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  pngFP2 = (FILE*)FOPEN(inFile2,"rb");
  // Important: Leaves file pointer offset into file by 8 bytes for png lib
  fread(sig, 1, 8, pngFP2); 
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    sprintf(msg, "Invalid PNG file (%s)\n  %s\n", "file type header bytes "
	    "invalid", inFile2);
    fprintf(outputFP, "%s", msg);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  png_ptr1 = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr1) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr1 = png_create_info_struct(png_ptr1);
  if (!info_ptr1) {
    png_destroy_read_struct(&png_ptr1, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  png_ptr2 = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr2) {
    sprintf(msg, "Cannot allocate PNG read struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }
  info_ptr2 = png_create_info_struct(png_ptr2);
  if (!info_ptr2) {
    png_destroy_read_struct(&png_ptr2, NULL, NULL);
    sprintf(msg, "Cannot allocate PNG info struct (out of memory?)\n");
    fprintf(outputFP, "%s", msg);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  if (setjmp(png_jmpbuf(png_ptr1))) {
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    sprintf(msg, "PNG library error occurred (invalid PNG file?)\n");
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    asfPrintError(msg);
  }

  png_init_io(png_ptr1, pngFP1);
  // Because of the sig-reading offset ...must do this for PNG lib
  png_set_sig_bytes(png_ptr1, 8); 
  png_init_io(png_ptr2, pngFP2);
  // Because of the sig-reading offset ...must do this for PNG lib
  png_set_sig_bytes(png_ptr2, 8); 

  // Read info and IHDR
  png_read_info(png_ptr1, info_ptr1);
  png_get_IHDR(png_ptr1, info_ptr1, &png_width, &png_height, &bit_depth, 
	       &color_type, &interlace_type, &compression_type, &filter_type);
  png_read_info(png_ptr2, info_ptr2);
  png_get_IHDR(png_ptr2, info_ptr2, &png_width, &png_height, &bit_depth, 
	       &color_type, &interlace_type, &compression_type, &filter_type);

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
    sprintf(msg,"Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)"
	    "\n", band1, ihdr1.num_bands, band2, ihdr2.num_bands);
    asfPrintWarning(msg);
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (ihdr1.height != ihdr2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. "
		    "%d).\nPSNR calculation will only use the first %d rows "
		    "from each file.\n", ihdr1.height, ihdr2.height, height);
  }
  if (ihdr1.width != ihdr2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row "
		    "(%d v. %d).\nPSNR calculation will only use the first %d "
		    "pixels from each row of data.\n",
		    ihdr1.width, ihdr2.width, width);
  }

  if (ihdr1.data_type != ihdr2.data_type) {
    sprintf(msg,"PNG files have differing data types.\n");
    asfPrintWarning(msg);
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }

  char **band_names1, **band_names2;
  get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
  get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s in"
		 " %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);
  float *buf1 = (float*)CALLOC(ihdr1.width, sizeof(float));
  float *buf2 = (float*)CALLOC(ihdr2.width, sizeof(float));
  if (buf1 == NULL || buf2 == NULL) {
    asfPrintWarning("Cannot allocate memory for PNG data buffers.\n");
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    if (buf1) FREE(buf1);
    if (buf2) FREE(buf2);
    if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
    if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (pngFP1) FCLOSE(pngFP1);
    if (pngFP2) FCLOSE(pngFP2);
    if (output_file_exists && outputFP) FCLOSE(outputFP);
    return;
  }

  sse = 0.0;
  // Since both file's data types are the same, this is OK
  max_val = get_maxval(ihdr1.data_type); 
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
    psnr->psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
    psnr->psnr_good = 1;
  }
  else {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
  }
  
  free_band_names(&band_names1, num_names_extracted1);
  free_band_names(&band_names2, num_names_extracted2);
  if (png_ptr1) png_destroy_read_struct(&png_ptr1, &info_ptr1, NULL);
  if (png_ptr2) png_destroy_read_struct(&png_ptr2, &info_ptr2, NULL);
  if (pngFP1) FCLOSE(pngFP1);
  if (pngFP2) FCLOSE(pngFP2);
  if (output_file_exists && outputFP) FCLOSE(outputFP);
  if (buf1) FREE(buf1);
  if (buf2) FREE(buf2);
}

// NOTE: No row offset provided because png_ptr acts like a file pointer ...
// just read all the rows in sequence.
void png_sequential_get_float_line(png_structp png_ptr, png_infop info_ptr, 
				   float *buf, int band)
{
  int num_bands = (int)png_get_channels(png_ptr, info_ptr);
  if (band < 0 || band > num_bands - 1) {
    asfPrintError("png_get_float_line(): bad band number (band %d, bands %d "
		  "through %d available)\n", band, 0, num_bands - 1);
  }
  png_uint_32 width, height;
  int bit_depth, color_type;
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, 
	       NULL, NULL, NULL);
  if (bit_depth != 8) {
    asfPrintError("png_get_float_line(): PNG image has unsupported bit depth "
		  "(%d).  Bit depth of 8-bits supported.\n", bit_depth);
  }
  if (color_type != PNG_COLOR_TYPE_GRAY &&
      color_type != PNG_COLOR_TYPE_RGB) {
    asfPrintError("png_get_float_line(): PNG image must be RGB or greyscale. "
		  "Alpha band, palette-color,\nand mask-type images not "
		  "supported.\n");
  }

  int rgb = color_type == PNG_COLOR_TYPE_RGB ? 1 : 0;
  png_bytep png_buf = (png_bytep)MALLOC(width*sizeof(png_byte)*(rgb ? 3 : 1));
  int col, ii;

  // Read current row
  png_read_row(png_ptr, png_buf, NULL);
  for (col=0; col<width; col++) {
    if (rgb) {
      for (ii=0; ii<num_bands; ii++)
        buf[col+ii*width] = (float)png_buf[col*3+ii];
    }
    else
      buf[col] = (float)png_buf[col];
  }
}

void get_ppm_pgm_info_hdr_from_file(char *inFile, ppm_pgm_info_t *pgm, 
				    char *outfile)
{
  int ch, i;
  unsigned char tmp[1024]; // For reading width, height, max_val only
  // Length of tmp for loop termination when reading sequential uchars
  int max_chars   = 1024; 
  FILE *fp = (FILE*)FOPEN(inFile, "rb");

  strcpy(pgm->magic,"");
  pgm->width=MISSING_PPM_PGM_DATA;
  pgm->height=MISSING_PPM_PGM_DATA;
  pgm->max_val=MISSING_PPM_PGM_DATA;
  // Default to unsupported type ...ASCII data separated by whitespace
  pgm->ascii_data=1; 
  pgm->img_offset = MISSING_PSNR; // Will result in a failed fseek()
  pgm->bit_depth=0;
  pgm->data_type=0;
  pgm->num_bands=0;

  //// Read magic number that identifies
  ASF_FREAD(pgm->magic, sizeof(unsigned char), 2, fp);
  if (pgm->magic[0] == 'P' && (pgm->magic[1] == '5' || pgm->magic[1] == '6')) {
    pgm->ascii_data = 0;
    pgm->img_offset = 2; // Just past magic number
  }
  else {
    // Shouldn't be possible to be here, but what the hey...
    FILE *outFP=NULL;
    if (outfile && strlen(outfile) > 0) outFP=(FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): Found invalid or "
	    "unsupported (ASCII data?)\nPPM/PGM file header.\n");
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  // Determine number of channels (3 for rgb, 1 for gray)
  if (pgm->magic[1] == '6') {
    pgm->num_bands = 3; // RGB PPM file
  }
  else if (pgm->magic[1] == '5') {
    pgm->num_bands = 1; // Grayscale PGM file
  }
  else {
    // Shouldn't be able to reach this code
    FILE *outFP=NULL;
    if (outfile && strlen(outfile) > 0) outFP = (FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): Found invalid or "
	    "unsupported\n(ASCII data?) PPM/PGM file header...\n");
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  // Read image width (in pixels)
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
    FILE *outFP = NULL;
    if (outfile && strlen(outfile) > 0) outFP = (FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): '%s' field in file header "
	    "too long.\n", "width");
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }
  pgm->width = atoi((char *)tmp);

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
    FILE *outFP = NULL;
    if (outfile && strlen(outfile) > 0) outFP = (FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): '%s' field in file header "
	    "too long.\n", "height");
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }
  pgm->height = atoi((char *)tmp);

  // Read max-allowed pixel value
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
    FILE *outFP = NULL;
    if (outfile && strlen(outfile) > 0) outFP = (FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "get_ppm_pgm_info_hdr_from_file(): '%s' field in file header "
	    "too long.\n", "max. pixel value");
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }
  pgm->max_val = atoi((char *)tmp);

  // Move past white space to beginning of image data to set pgm->img_offset 
  // for an fseek()
  while (isspace(ch) && ch != EOF) {
    ch = fgetc(fp); // Move past white space
    pgm->img_offset++;
  }
  pgm->img_offset--;
  if (ch == EOF) {
    FILE *outFP = NULL;
    if (outfile && strlen(outfile) > 0) outFP = (FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "No image data found in PNG file\n  %s\n",
            inFile);
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp) FCLOSE(fp);
    asfPrintError(msg);
  }

  // Determine byte-boundary bit-depth and ASF data type for holding
  // the PPM/PGM file's image data
  if ((float)pow(20, 32) > pgm->max_val) {
    pgm->bit_depth = 8;
    pgm->data_type = ASF_BYTE;
  }
  else if ((float)pow(2, 16) > pgm->max_val) {
    pgm->bit_depth = 16;
    pgm->data_type = INTEGER16;
  }
  else if ((float)pow(2, 8) > pgm->max_val) {
    pgm->bit_depth = 32;
    pgm->data_type = INTEGER32;
  }
  else {
    FILE *outFP = NULL;
    if (outfile && strlen(outfile) > 0) outFP = (FILE*)FOPEN(outfile,"a");
    char msg[1024];
    sprintf(msg, "Invalid max. pixel value found in PNG file (%d)\n  %s\n",
            pgm->max_val,
            inFile);
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
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
    meta_parameters *md=NULL;
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
      *band_names = 
	extract_band_names(md->general->bands, md->general->band_count);
      if (*band_names == NULL) {
        // free_band_names() will assume MAX_BANDS have been made available if
        // no bands have been extracted otherwise, so ...sigh, allocate 
	// MAX_BANDS here ...they'll be freed unused.
        *num_extracted_bands=0;
        *band_names = (char**)MALLOC(MAX_BANDS*sizeof(char*));
        if (*band_names != NULL) {
          int i;
          for (i=0; i<MAX_BANDS; i++) {
            (*band_names)[i] = (char*)CALLOC(64, sizeof(char));
            if ((*band_names)[i] == NULL) {
              sprintf(msg, "Cannot allocate memory for band name array.\n");
              if (outputFP != NULL) fprintf(outputFP, "%s", msg);
              asfPrintError(msg);
            }
          }
        }
        else {
          sprintf(msg, "Cannot allocate memory for band name array.\n");
          if (outputFP != NULL) fprintf(outputFP, "%s", msg);
          asfPrintError(msg);
        }
      }
    }
    meta_free(md);
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
          if (outputFP != NULL) fprintf(outputFP, "%s", msg);
          asfPrintError(msg);
        }
      }
    }
    else {
      sprintf(msg, "Cannot allocate memory for band name array.\n");
      if (outputFP != NULL) fprintf(outputFP, "%s", msg);
      asfPrintError(msg);
    }
 
    if (type == PNG_IMG) {
      png_info_t ihdr;
      get_png_info_hdr_from_file(inFile, &ihdr, NULL);
      *num_extracted_bands = ihdr.num_bands;
    }
    
    if (type == JPEG_IMG) {
      jpeg_info_t jpg;
      get_jpeg_info_hdr_from_file(inFile, &jpg, NULL);
      *num_extracted_bands = jpg.num_bands;
    }
    
    if (type == GEO_TIFF_IMG || type == GEO_TIFF_MAT) {
      // If band names are embedded in the GeoTIFF, then grab them.  Otherwise
      // assign numeric band names
      geotiff_data_t g;
      int *empty = (int*)CALLOC(MAX_BANDS, sizeof(int));
      char *band_str = (char*)CALLOC(255, sizeof(char));
      if (band_str == NULL) {
        sprintf(msg, "Cannot allocate memory for band name string.\n");
        if (outputFP != NULL) fprintf(outputFP, "%s", msg);
        asfPrintError(msg);
      }

      get_geotiff_keys(inFile, &g);
      if (g.gtif_data_exists && 
	  g.GTcitation != NULL && strlen(g.GTcitation) > 0) {
        char *tmp_citation = STRDUP(g.GTcitation);
        get_bands_from_citation(num_extracted_bands, &band_str, empty, 
				tmp_citation, 0);
        FREE(tmp_citation);
      }
      else if (g.gtif_data_exists && g.PCScitation != NULL && 
	       strlen(g.PCScitation) > 0) {
        char *tmp_citation = STRDUP(g.PCScitation);
        get_bands_from_citation(num_extracted_bands, &band_str, empty, 
				tmp_citation, 0);
        FREE(tmp_citation);
      }
      if (*num_extracted_bands <= 0) {
        // Could not find band strings in the citations, so assign numeric 
	// band IDs
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

void ppm_pgm_get_float_line(FILE *fp, float *buf, int row, ppm_pgm_info_t *pgm,
			    int band_no)
{
  if (band_no < 0 || band_no > pgm->num_bands - 1) {
    asfPrintError("Invalid band number (%d).  Valid: 0 through %d (only)\n",
                  band_no, pgm->num_bands - 1);
  }
  unsigned char *pgm_buf = 
    (unsigned char*)MALLOC(pgm->num_bands * pgm->width * sizeof(unsigned char));
  if (pgm_buf == NULL) {
    asfPrintError("Cannot allocate memory for PPM/PGM scanline buffer\n");
  }

  ASF_FREAD(pgm_buf, sizeof(unsigned char), pgm->width * pgm->num_bands, fp);

  int col;
  for (col=0; col<pgm->width; col++) {
    buf[col] = (float)(((unsigned char*)pgm_buf)[(col*pgm->num_bands)+band_no]);
  }
  if (pgm_buf)FREE(pgm_buf);
}

int ppm_pgm_image_band_statistics_from_file(char *inFile, char *outfile,
					    int band_no, int *stats_exist,
					    double *min, double *max,
					    double *mean, double *sdev, 
					    double *rmse, int use_mask_value, 
					    float mask_value)
{
  ppm_pgm_info_t pgm;

  *stats_exist = 1; // Innocent until presumed guilty...
  get_ppm_pgm_info_hdr_from_file(inFile, &pgm, outfile);
  if (pgm.magic == NULL || strlen(pgm.magic) == 0 ||
      pgm.width <= 0 ||
      pgm.height <= 0 ||
      pgm.max_val <= 0 ||
      pgm.bit_depth != 8 ||
      pgm.data_type != ASF_BYTE ||
      pgm.num_bands <= 0) {
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
    for ( ii = 0; ii < pgm.height; ii++ ) {
      asfPercentMeter((double)ii/(double)pgm.height);
      ppm_pgm_get_float_line(fp, buf, ii, &pgm, band_no);
      for (jj = 0 ; jj < pgm.width; jj++ ) {
        // iterate over each pixel sample in the scanline
        cs = buf[jj];
        if ( !isnan(mask_value) && 
	     (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
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
    for ( ii = 0; ii < pgm.height; ii++ ) {
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
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0) {
    if (buf) free(buf);
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return 1;
  }

  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  // This assumes the sample count is large enough to ensure that the sample 
  // standard deviation is very very similar to the population standard 
  // deviation.
  *rmse = *sdev; 

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX) {
    if (buf) free(buf);
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return 1;
  }

  if (buf) free(buf);
  if (fp) FCLOSE(fp);
  return 0;
}

void ppm_pgm_image_band_psnr_from_files(char *inFile1, char *inFile2, 
					char *outfile,
                                        int band1, int band2, psnr_t *psnr)
{
  ppm_pgm_info_t pgm1, pgm2; // Header info
  double cs1, cs2;
  char msg[1024];
  FILE *fp1 = NULL, *fp2 = NULL, *outputFP = NULL;

  if (outfile && strlen(outfile) > 0) {
      outputFP = (FILE*)FOPEN(outfile, "a");
  }
  else {
      outputFP = NULL;
  }

  // Get header info and fseek() to beginning of image data
  get_ppm_pgm_info_hdr_from_file(inFile1, &pgm1, outfile);
  if (pgm1.magic == NULL || strlen(pgm1.magic) == 0 ||
      pgm1.width <= 0 ||
      pgm1.height <= 0 ||
      pgm1.max_val <= 0 ||
      pgm1.bit_depth != 8 ||
      pgm1.data_type != ASF_BYTE ||
      pgm1.num_bands <= 0) {
    sprintf(msg, "Cannot read PPM/PNG file header in file1\n  %s\n", inFile1);
    if (outputFP) fprintf(outputFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outputFP) FCLOSE(outputFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  get_ppm_pgm_info_hdr_from_file(inFile2, &pgm2, outfile);
  if (pgm2.magic == NULL || strlen(pgm2.magic) == 0 ||
      pgm2.width <= 0 ||
      pgm2.height <= 0 ||
      pgm2.max_val <= 0 ||
      pgm2.bit_depth != 8 ||
      pgm2.data_type != ASF_BYTE ||
      pgm2.num_bands <= 0) {
    sprintf(msg, "Cannot read PPM/PNG file header in file2\n  %s\n", inFile2);
    if (outputFP) fprintf(outputFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outputFP) FCLOSE(outputFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
    sprintf(msg,"Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)"
	    "\n", band1, pgm1.num_bands, band2, pgm2.num_bands);
    asfPrintWarning(msg);
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outputFP) FCLOSE(outputFP); else fprintf(stderr, "%s", msg);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (pgm1.height != pgm2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. "
		    "%d).\nPSNR calculation will only use the first %d rows "
		    "from each file.\n", pgm1.height, pgm2.height, height);
  }
  if (pgm1.width != pgm2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row "
		    "(%d v. %d).\nPSNR calculation will only use the first %d "
		    "pixels from each row of data.\n",
		    pgm1.width, pgm2.width, width);
  }

  if (pgm1.data_type != pgm2.data_type) {
    sprintf(msg,"PPM/PGM files have differing data types.\n");
    asfPrintWarning(msg);
    if (outputFP != NULL) fprintf(outputFP, "%s", msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outputFP) FCLOSE(outputFP); else fprintf(stderr, "%s", msg);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }

  char **band_names1, **band_names2;
  int num_names_extracted1=0, num_names_extracted2=0;
  get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
  get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s "
		 "in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);
  float *buf1 = (float*)CALLOC(pgm1.width, sizeof(float));
  float *buf2 = (float*)CALLOC(pgm2.width, sizeof(float));
  if (buf1 == NULL || buf2 == NULL) {
    asfPrintWarning("Cannot allocate memory for PNG data buffers.\n");
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    if (buf1) FREE(buf1);
    if (buf2) FREE(buf2);
    if (outputFP) fprintf(outputFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    if (outputFP) FCLOSE(outputFP);
    return;
  }

  sse = 0.0;
  // Since both file's data types are the same, this is OK
  max_val = get_maxval(pgm1.data_type); 
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
    psnr->psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
    psnr->psnr_good = 1;
  }
  else {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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

GLOBAL(void) get_jpeg_info_hdr_from_file(char *inFile, jpeg_info_t *jpg, 
					 char *outputFile)
{
  char msg[1024];
  struct jpeg_decompress_struct cinfo;
  jpeg_error_hdlr_t jerr;
  FILE *fp, *outFP = NULL;

  // Init
  jpg->width = 0;
  jpg->height = 0;
  jpg->data_type = 0;
  jpg->num_bands = 0;

  // Try file open
  fp = (FILE*)FOPEN(inFile, "rb");
  if (fp == NULL) {
    sprintf(msg,"Cannot open JPEG file:\n  %s\n", inFile);
    if (outputFile && strlen(outputFile) > 0) 
      outFP = (FILE*)FOPEN(outputFile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
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
    if (outputFile && strlen(outputFile) > 0) 
      outFP = (FILE*)FOPEN(outputFile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  // Init jpeg lib to read
  jpeg_create_decompress(&cinfo); // Init decompression object
  jpeg_stdio_src(&cinfo, fp); // Set input to open inFile

  // Read jpeg file header
  jpeg_read_header(&cinfo, TRUE);

  // Populate the header struct
  jpg->width = cinfo.image_width;
  jpg->byte_width = cinfo.image_width * cinfo.num_components;
  jpg->height = cinfo.image_height;
  jpg->data_type = ASF_BYTE;
  jpg->num_bands = cinfo.num_components;

  // Finish up
  jpeg_destroy_decompress(&cinfo);
  if (fp) FCLOSE(fp);
}

GLOBAL(void) jpeg_image_band_statistics_from_file(char *inFile, char *outfile,
						  int band_no, int *stats_exist,
						  double *min, double *max,
						  double *mean, double *sdev, 
						  double *rmse,
						  int use_mask_value, 
						  float mask_value)
{
  jpeg_info_t jpg;
  char msg[1024];
  struct jpeg_decompress_struct cinfo;
  FILE *outFP = NULL;
  jpeg_error_hdlr_t jerr;
  JSAMPARRAY jpg_buf;

  *stats_exist = 1; // Innocent until presumed guilty...
  get_jpeg_info_hdr_from_file(inFile, &jpg, outfile);
  if (jpg.width <= 0 ||
      jpg.height <= 0 ||
      jpg.data_type != ASF_BYTE ||
      jpg.num_bands <= 0) {
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
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  // Init jpeg lib to read
  jpeg_create_decompress(&cinfo); // Init decompression object
  jpeg_stdio_src(&cinfo, fp); // Set input to open inFile

  // Read jpeg file header
  jpeg_read_header(&cinfo, TRUE);
  jpeg_start_decompress(&cinfo);

  // Allocate buffer for reading jpeg
  jpg_buf = (*cinfo.mem->alloc_sarray) 
    ((j_common_ptr)&cinfo, JPOOL_IMAGE, jpg.byte_width, 1);

  // If there is a mask value we are supposed to ignore,
  if ( use_mask_value ) {
    // iterate over all rows in the PPM or PGM file
    while (cinfo.output_scanline < cinfo.image_height) {
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
        if ( !isnan(mask_value) && 
	     (gsl_fcmp (cs, mask_value, 0.00000000001) == 0 ) ) {
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
    while (cinfo.output_scanline < cinfo.image_height) {
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
      gsl_fcmp (fmax, -FLT_MAX, 0.00000000001) == 0) {
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return;
  }
  
  *min = fmin;
  *max = fmax;
  *sdev = sqrt (s / (sample_count - 1));
  *rmse = *sdev; 
  // This assumes the sample count is large enough to ensure that the sample 
  // standard deviation is very very similar to the population standard 
  // deviation.

  // The new extrema had better be in the range supported range
  if (fabs(*mean) > FLT_MAX || fabs(*sdev) > FLT_MAX) {
    if (fp) FCLOSE(fp);
    *stats_exist = 0;
    return;
  }
  
  // Finish up
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  if (fp) FCLOSE(fp);
  if (outFP) FCLOSE(outFP);
  return;
}

GLOBAL(void) jpeg_image_band_psnr_from_files(char *inFile1, char *inFile2, 
					     char *outfile, int band1, 
					     int band2, psnr_t *psnr)
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
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP)
        fprintf(outFP, "Cannot open file: %s\n", inFile1);
    else
        fprintf(stderr, "Cannot open file: %s\n", inFile1);
    if (outFP) FCLOSE(outFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (fp2 == NULL) {
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP)
      fprintf(outFP, "Cannot open file: %s\n", inFile2);
    else
      fprintf(stderr, "Cannot open file: %s\n", inFile2);
    if (outFP) FCLOSE(outFP);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }

  get_jpeg_info_hdr_from_file(inFile1, &jpg1, outfile);
  if (jpg1.width <= 0 ||
      jpg1.height <= 0 ||
      jpg1.data_type != ASF_BYTE ||
      jpg1.num_bands <= 0) {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  get_jpeg_info_hdr_from_file(inFile2, &jpg2, outfile);
  if (jpg2.width <= 0 ||
      jpg2.height <= 0 ||
      jpg2.data_type != ASF_BYTE ||
      jpg2.num_bands <= 0) {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
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
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
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
  jpg_buf1 = (*cinfo1.mem->alloc_sarray) 
    ((j_common_ptr)&cinfo1, JPOOL_IMAGE, jpg1.byte_width, 1);
  jpg_buf2 = (*cinfo2.mem->alloc_sarray) 
    ((j_common_ptr)&cinfo2, JPOOL_IMAGE, jpg2.byte_width, 1);

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
    sprintf(msg,"Invalid band number for file1 (%d v. %d) or file2 (%d v. %d)"
	    "\n", band1, jpg1.num_bands, band2, jpg2.num_bands);
    asfPrintWarning(msg);
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (jpg1.height != jpg2.height) {
    asfPrintWarning("File1 and File2 have a differing numbers of rows (%d v. "
		    "%d).\nPSNR calculation will only use the first %d rows "
		    "from each file.\n", jpg1.height, jpg2.height, height);
  }
  if (jpg1.width != jpg2.width) {
    asfPrintWarning("File1 and File2 have a different number of pixels per row "
		    "(%d v. %d).\nPSNR calculation will only use the first %d "
		    "pixels from each row of data.\n",
		    jpg1.width, jpg2.width, width);
  }

  if (jpg1.data_type != jpg2.data_type) {
    sprintf(msg,"Input JPEG files have differing data types.\n");
    asfPrintWarning(msg);
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }
  if (jpg1.num_bands != jpg2.num_bands) {
    sprintf(msg,"Input JPEG files have a different number of bands (%d v. %d)"
	    "\n", jpg1.num_bands, jpg2.num_bands);
    asfPrintWarning(msg);
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    if (fp1) FCLOSE(fp1);
    if (fp2) FCLOSE(fp2);
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
    return;
  }

  char **band_names1, **band_names2;
  int num_names_extracted1=0, num_names_extracted2=0;
  get_band_names(inFile1, NULL, &band_names1, &num_names_extracted1);
  get_band_names(inFile2, NULL, &band_names2, &num_names_extracted2);
  asfPrintStatus("\nCalculating PSNR between\n  Band %s in %s and\n  Band %s "
		 "in %s\n",
                 band_names1[band1], inFile1, band_names2[band2], inFile2);

  sse = 0.0;
  // Since both file's data types are the same, this is OK
  max_val = get_maxval(jpg1.data_type); 
  while (cinfo1.output_scanline < height &&
         cinfo2.output_scanline < height) {
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
  // Read any unread lines so the jpeg library won't complain when 
  // jpeg_finish_decompress() and jpeg_destroy_decompress() are called...
  while (cinfo1.output_scanline < jpg1.height) {
    jpeg_read_scanlines(&cinfo1, jpg_buf1, 1);
  }
  while (cinfo2.output_scanline < jpg2.height) {
    jpeg_read_scanlines(&cinfo2, jpg_buf2, 1);
  }
  if (pixel_count > 0 && max_val > 0) {
    rmse = sqrt(sse/pixel_count);
    psnr->psnr = 10.0 * log10(max_val/(rmse+.00000000000001));
    psnr->psnr_good = 1;
  }
  else {
    psnr->psnr = MISSING_PSNR;
    psnr->psnr_good = 0;
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
/*
 * Creates and writes VERY simple metadata file to coax the fftMatch() functions
 * to work. The only requirements are that  the number of lines and samples, and
 * data type, are required. The rest of the metadata is ignored.
 *
 * I've added a  band names option so  that the converted images  can still have
 * band names that work nicely.
 */
void make_generic_meta(char *file, uint32 height, uint32 width, 
		       data_type_t data_type, char **band_names,
                       int num_bands)
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
  char bands[1024];
  int band_no;
  int char_no = 0;
  for (band_no = 0; band_no < num_bands; ++band_no)
        char_no += sprintf(bands + char_no, "%s%s", band_names[band_no],
                        band_no == num_bands - 1 ? "" : ",");
  strcpy(md->general->bands, bands);
  md->general->band_count = num_bands;
  // This just quiets the fftMatch warnings during meta_read()
  md->general->image_data_type = AMPLITUDE_IMAGE; 
  
  meta_write(md, file_meta);
  meta_free(md);
}

void diff_check_geolocation(char *outputFile, char *inFile1, char *inFile2,
                            int num_bands, shift_data_t *shift,
                            stats_t *stats1, stats_t *stats2)
{
  char msg[1024];
  int band;
  int low_certainty, empty_band1, empty_band2;
  float shift_tol = PROJ_LOC_DIFF_TOL_m;
  float radial_shift_diff = 0.0;
  int num_extracted_bands1=0, num_extracted_bands2=0;
  FILE *outputFP = NULL;
  shift_data_t *s = shift;
  stats_t *s1 = stats1;
  stats_t *s2 = stats2;

  int have_out_file = 0;
  if (outputFile && strlen(outputFile) > 0) {
      outputFP = fopen(outputFile, "a");
      have_out_file = (outputFP) ? 1 : 0;
  }
  else {
      outputFP = stderr;
      have_out_file = 0;
  }

  // Get or produce band names for intelligent output...
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  get_band_names(inFile1, outputFP, &band_names1, &num_extracted_bands1);
  get_band_names(inFile2, outputFP, &band_names2, &num_extracted_bands2);

  // Check each band for differences
  char band_str1[64];
  char band_str2[64];
  for (band=0; band<num_bands; band++) {
    s[band].cert_good = TRUE;
    s[band].dxdy_good = TRUE;
    radial_shift_diff = sqrt(s[band].dx * s[band].dx + s[band].dy * s[band].dy);
    low_certainty = ISNAN(s[band].cert) ? 1 :
        !ISNAN(s[band].cert) && s[band].cert < MIN_MATCH_CERTAINTY ? 1 : 0;
    empty_band1 = s1[band].sdev < 1.0;
    empty_band2 = s2[band].sdev < 1.0;
    // We are not checking for empty bands anymore - RG
    // if (!empty_band1 || !empty_band2) {
      // One or more images have a non-empty band for the current band number, 
      // so check it.
      if (low_certainty)
      {
        fprintf(outputFP, "\n---------------------------------------------\n");

        if (num_bands > 1) {
          sprintf(band_str1, "Band %s in ", band_names1[band]);
          sprintf(band_str2, "Band %s in ", band_names2[band]);
        }
        else {
          strcpy(band_str1, "");
          strcpy(band_str2, "");
        }
        sprintf(msg, "FAIL: diff_check_geolocation() - Correlation match "
		"between images failed\nwhen trying to find shift in "
		"geolocation between non-blank\nimages.  Comparing\n  %s%s  "
		"and\n  %s%s\nCertainty = %0.6f\n\n",
                band_str1, inFile1, band_str2, inFile2, s[band].cert);
        fprintf(outputFP, "%s", msg);
	s[band].cert_good = FALSE;

        fprintf(outputFP, "---------------------------------------------\n\n");
      }
      else if (fabs(s[band].dx)  > shift_tol ||
               fabs(s[band].dy)  > shift_tol ||
               radial_shift_diff > shift_tol)
      {
        // The certainty value was OK but the shifts were out of tolerance
        fprintf(outputFP, "\n----------------------------------------------\n");

        if (num_bands > 1) {
          sprintf(band_str1, "Band %s in ", band_names1[band]);
          sprintf(band_str2, "Band %s in ", band_names2[band]);
        }
        else {
          strcpy(band_str1, "");
          strcpy(band_str2, "");
        }
        sprintf(msg, "FAIL: Comparing geolocations of\n  %s%s  and\n  %s%s\n"
                "Shifts: dx = %0.3f pixels, dy = %0.3f pixels\n\n",
                band_str1, inFile1, band_str2, inFile2,
               s[band].dx, s[band].dy);
        fprintf(outputFP, "%s", msg);
	s[band].dxdy_good = FALSE;

        if (s[band].dx > shift_tol) {
          sprintf(msg, "[%s] [x-loc]  File1: %12f,  File2: %12f, Tolerance: "
		  "%11f (Certainty: %f%%)\n",
                  s[band].dx > shift_tol ? "FAIL" : "PASS",
                  0.0, s[band].dx, shift_tol, 100.0 * s[band].cert);
          fprintf(outputFP, "%s", msg);
        }
        if (s[band].dy > shift_tol) {
          sprintf(msg, "[%s] [y-loc]  File1: %12f,  File2: %12f, Tolerance: "
		  "%11f (Certainty: %f%%)\n",
                  s[band].dy > shift_tol ? "FAIL" : "PASS",
                  0.0, s[band].dy, shift_tol, 100.0 * s[band].cert);
          fprintf(outputFP, "%s", msg);
        }
        if (radial_shift_diff > shift_tol) {
          sprintf(msg, "[%s] [radial dist]  File1: %12f,  File2: %12f, "
		  "Tolerance: %11f\n",
                  radial_shift_diff > shift_tol ? "FAIL" : "PASS",
                  0.0, radial_shift_diff, shift_tol);
          fprintf(outputFP, "%s", msg);
        }
	
        fprintf(outputFP, "---------------------------------------------\n\n");
      }
      else {
        asfPrintStatus("\nNo differences found in geolocations\n\n");
	s[band].cert_good = TRUE;
	s[band].dxdy_good = TRUE;
      }
    //}
    /* else {
      if (empty_band1) {
        asfPrintStatus("Band %s (%02d) in %s is empty ...not checking "
		       "geolocation\n\n", band_names1[band], band, inFile1);
      }
      if (empty_band2) {
        asfPrintStatus("Band %s (%02d) in %s is empty ...not checking "
		       "geolocation\n\n", band_names2[band], band, inFile2);
      }
    }*/
  } // For each band

  if (have_out_file && outputFP) FCLOSE(outputFP);
  free_band_names(&band_names1, num_extracted_bands1);
  free_band_names(&band_names2, num_extracted_bands2);
}

void fftShiftCheck(char *file1, char *file2, char *corr_file,
                shift_data_t *shifts)
{
        fftMatch_either(file1, file2, &shifts->dx, &shifts->dy, &shifts->cert);
        // shifts->dx = shifts->dy = 0.0;
        // shifts->cert = 1.0;
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

  get_jpeg_info_hdr_from_file(inFile, &jpg, outfile);
  if (jpg.width <= 0 ||
      jpg.height <= 0 ||
      jpg.data_type != ASF_BYTE ||
      jpg.num_bands <= 0) {
    sprintf(msg,"Invalid JPEG found...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  // Write out the (very very simple) metadata file ...
  // only contains line_count, sample_count, and data_type
  char **bands;
  int num_bands;
  get_band_names(inFile, NULL, &bands, &num_bands);
  make_generic_meta(fft_meta_file, jpg.height, jpg.width, jpg.data_type, bands,
                num_bands);
  free_band_names(&bands, num_bands);
  md = meta_read(fft_meta_file);

  buf = (float*)MALLOC(jpg.width * sizeof(float));
  if (buf == NULL) {
    sprintf(msg,"Cannot allocate float buffer...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  uint32 jj, kk;
  FILE *fp = FOPEN(inFile, "rb");
  if (fp == NULL) {
    sprintf(msg,"Cannot open JPEG file for read...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg); else fprintf(stderr, "%s", msg);
    if (outFP) FCLOSE(outFP);
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
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
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
      (j_common_ptr)&cinfo, JPOOL_IMAGE, jpg.byte_width, 1);

  asfPrintStatus("Converting JPEG to ASF Internal Format .img file...\n");
  imgFP = (FILE*)FOPEN(fft_file, "wb");
  if (imgFP == NULL) {
    sprintf(msg,"Cannot open IMG file for write...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  int row=0;
  while (cinfo.output_scanline < cinfo.image_height)
  {
    asfPercentMeter((double)cinfo.output_scanline/(double)jpg.height);
    jpeg_read_scanlines(&cinfo, jpg_buf, 1); // Reads one scanline
    for (kk=0; kk<jpg.num_bands; kk++) {
      for (jj = 0 ; jj < jpg.width; jj++ ) {
        if (jpg.num_bands == 1) {
          buf[jj] = (float)jpg_buf[0][jj];
        }
        else if (jpg.num_bands == 3) {
          buf[jj] = (float)jpg_buf[0][jj*3+kk];
        }
        else {
          // Shouldn't get here...
          sprintf(msg,"Invalid number of bands in JPEG file ...Aborting\n");
          if (outfile && strlen(outfile) > 0) 
            outFP = (FILE*)FOPEN(outfile,"a"); 
          else 
            outFP = NULL;
          if (outFP) fprintf(outFP, "%s", msg);
          if (outFP) FCLOSE(outFP);
          asfPrintError(msg);
        }
      }
      put_band_float_line(imgFP, md, kk, row, buf);
    }
    row++;
  }
  asfPercentMeter(1.0);

  // Finish up
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  if (fp) FCLOSE(fp);
  if (outFP) FCLOSE(outFP);
  if (imgFP) FCLOSE(imgFP);
  if (buf) FREE(buf);
  meta_free(md);
}

void export_ppm_pgm_to_asf_img(char *inFile, char *outfile,
                               char *fft_file, char *fft_meta_file,
                               uint32 height, uint32 width, 
			       data_type_t data_type, int band_no)
{
  meta_parameters *md;
  uint32 ii;
  float *buf;
  FILE *imgFP=NULL;
  ppm_pgm_info_t pgm;
  char msg[1024];
  FILE *outFP=NULL;

  char **bands;
  int num_bands;
  get_band_names(inFile, NULL, &bands, &num_bands);
  make_generic_meta(fft_meta_file, height, width, data_type, bands, num_bands);
  free_band_names(&bands, num_bands);
  md = meta_read(fft_meta_file);

  get_ppm_pgm_info_hdr_from_file(inFile, &pgm, outfile);
  if (pgm.magic == NULL || strlen(pgm.magic) == 0 ||
      pgm.width <= 0 ||
      pgm.height <= 0 ||
      pgm.max_val <= 0 ||
      pgm.bit_depth != 8 ||
      pgm.data_type != ASF_BYTE ||
      pgm.num_bands <= 0)
  {
    sprintf(msg,"Invalid PPM/PGM file found...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  FILE *fp = FOPEN(inFile, "rb");
  if (fp == NULL) {
    sprintf(msg,"Cannot open PPM/PGM file for read ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  imgFP = FOPEN(fft_file, "wb");
  if (imgFP == NULL) {
    sprintf(msg,"Cannot open IMG file for write ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  buf = (float*)MALLOC(pgm.width * sizeof(float));
  if (buf == NULL) {
    sprintf(msg,"Cannot allocate memory ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  asfPrintStatus("\nConverting PPM/PGM file to IMG format...\n");
  FSEEK64(fp, (long long)pgm.img_offset, SEEK_SET);
  for ( ii = 0; ii < pgm.height; ii++ ) {
    asfPercentMeter((double)ii/(double)pgm.height);
    ppm_pgm_get_float_line(fp, buf, ii, &pgm, band_no);
    put_float_line(imgFP, md, ii, buf);
  }
  asfPercentMeter(1.0);

  if (buf) free(buf);
  if (fp) FCLOSE(fp);
  if (outFP) FCLOSE(outFP);
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

  char **bands;
  int num_bands;
  get_band_names(inFile, NULL, &bands, &num_bands);
  make_generic_meta(fft_meta_file, height, width, data_type, bands, num_bands);
  free_band_names(&bands, num_bands);
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
      t.width == 0) {
    sprintf(msg,"Invalid TIFF file found ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  uint32 ii, kk;
  TIFF *tif = XTIFFOpen(inFile, "rb");
  if (tif == NULL) {
    sprintf(msg,"Cannot open TIFF file for read ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  imgFP = (FILE*)FOPEN(fft_file, "wb");
  if (tif == NULL) {
    if (tif) XTIFFClose(tif);
    sprintf(msg,"Cannot open IMG file for write ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  scanlineSize = TIFFScanlineSize(tif);
  if (scanlineSize <= 0) {
    if (tif) XTIFFClose(tif);
    sprintf(msg,"Invalid scan line size in TIFF file ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  if (t.num_bands > 1 &&
      t.planar_config != PLANARCONFIG_CONTIG &&
      t.planar_config != PLANARCONFIG_SEPARATE)
  {
    if (tif) XTIFFClose(tif);
    sprintf(msg,"Invalid planar configuration found in TIFF file ..."
	    "Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  float *buf = (float*)MALLOC(t.width * sizeof(float));

  asfPrintStatus("Converting TIFF file to IMG file..\n");
  for ( ii = 0; ii < t.height; ii++ ) {
    asfPercentMeter((double)ii/(double)t.height);
    for (kk = 0; kk < t.num_bands; kk++) {
      tiff_get_float_line(tif, buf, ii, kk);
      put_band_float_line(imgFP, md, kk, ii, buf);
    }
  }
  asfPercentMeter(1.0);

  if (buf) free(buf);
  //if (tmp) free(tmp);
  if (tif) XTIFFClose(tif);
  if (imgFP) FCLOSE(imgFP);
  if (outFP) FCLOSE(outFP);
  meta_free(md);
}

void export_png_to_asf_img(char *inFile, char *outfile,
                           char *fft_file, char *fft_meta_file,
                           uint32 img_height, uint32 img_width, 
			   data_type_t img_data_type, int band_no)
{
  meta_parameters *md;
  FILE *imgFP=NULL, *outFP=NULL;

  char **bands;
  int num_bands;
  get_band_names(inFile, NULL, &bands, &num_bands);
  make_generic_meta(fft_meta_file, img_height, img_width, img_data_type, bands,
                num_bands);
  free_band_names(&bands, num_bands);
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
      ihdr.width == 0) {
    sprintf(msg,"Invalid PNG file found ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  imgFP = (FILE*)FOPEN(fft_file,"wb");
  if (imgFP == NULL) {
    sprintf(msg,"Cannot open IMG file ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }

  // Initialize PNG file for read
  pngFP = (FILE*)FOPEN(inFile,"rb");
  if (pngFP == NULL) {
    sprintf(msg,"Cannot open PNG file ...Aborting\n");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  // Important: Leaves file pointer offset into file by 8 bytes for png lib
  fread(sig, 1, 8, pngFP); 
  if (!png_check_sig(sig, 8)) {
    // Bad PNG magic number (signature)
    if (pngFP) FCLOSE(pngFP);
    sprintf(msg, "Invalid PNG file (%s)\n", "file type header bytes invalid");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  png_ptr = png_create_read_struct(
      PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    if (pngFP) FCLOSE(pngFP);
    sprintf(msg, "Cannot allocate PNG read struct ...Aborting");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    if (pngFP) FCLOSE(pngFP);
    sprintf(msg, "Cannot allocate PNG info struct ...Aborting");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  if (setjmp(png_jmpbuf(png_ptr))) {
    if (pngFP) FCLOSE(pngFP);
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    sprintf(msg, "PNG library error occurred ...Aborting");
    if (outfile && strlen(outfile) > 0) 
      outFP = (FILE*)FOPEN(outfile,"a"); 
    else 
      outFP = NULL;
    if (outFP) fprintf(outFP, "%s", msg);
    if (outFP) FCLOSE(outFP);
    asfPrintError(msg);
  }
  png_init_io(png_ptr, pngFP);
  // Because of the sig-reading offset ...must do this for PNG lib
  png_set_sig_bytes(png_ptr, 8); 

  // Read info and IHDR
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
               &interlace_type, &compression_type, &filter_type);

  uint32 ii, kk;
  float *buf = (float*)MALLOC(ihdr.width * ihdr.num_bands * sizeof(float));

  for ( ii = 0; ii < ihdr.height; ii++ ) {
    asfPercentMeter((double)ii/(double)ihdr.height);
    png_sequential_get_float_line(png_ptr, info_ptr, buf, band_no);
    for (kk = 0; kk < ihdr.num_bands; kk++) {      
      put_band_float_line(imgFP, md, kk, ii, &buf[kk*ihdr.width]);
    }
  }
  asfPercentMeter(1.0);    

  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  if (buf) free(buf);
  if (outFP) FCLOSE(outFP);
  if (pngFP) FCLOSE(pngFP);
  if (imgFP) FCLOSE(imgFP);
  meta_free(md);
}

// Calling code must free the returned string
char *pcs2description(int pcs)
{
  char *pcs_description = (char*)MALLOC(512*sizeof(char));
  
  int pcs_datum = pcs/100;
  int pcs_zone  = pcs - (pcs_datum * 100);
  char pcs_hem  = 
    (pcs_datum == 323) ? 'S' :
    (pcs_datum == 327) ? 'S' :
    (pcs_datum == 269) ? 'N' :
    (pcs_datum == 267) ? 'N' :
    (pcs_datum == 322) ? 'N' :
    (pcs_datum == 326) ? 'N' : '?';
    sprintf(pcs_description, "UTM Zone %02d, %c hemisphere", pcs_zone, pcs_hem);
    switch(pcs_datum) 
      {
      case 269:
	strcat(pcs_description, ", NAD_83 datum");
	break;
      case 267:
	strcat(pcs_description, ", NAD_27 datum");
	break;
      case 322:
      case 323:
	strcat(pcs_description, ", WGS_72 datum");
	break;
      case 326:
      case 327:
	strcat(pcs_description, ", WGS_84 datum");
	break;
      default:
	strcat(pcs_description, ", UNKNOWN or UNRECOGNIZED datum");
	break;
      }
    
    return pcs_description;
}

void calc_complex_stats_rmse_from_file(const char *inFile, const char *band, 
				       double mask, double *i_min, 
				       double *i_max, double *i_mean,
                                       double *i_stdDev, double *i_rmse, 
				       gsl_histogram **i_histogram,
                                       double *q_min, double *q_max, 
				       double *q_mean, double *q_stdDev, 
				       double *q_rmse, 
				       gsl_histogram **q_histogram)
{
  double i_se, q_se;
  int ii,jj;
  
  *i_min = 999999;
  *i_max = -999999;
  *i_mean = 0.0;
  *q_min = 999999;
  *q_max = -999999;
  *q_mean = 0.0;
  
  meta_parameters *meta = meta_read(inFile);
  int band_number =
    (!band || strlen(band) == 0 || strcmp(band, "???") == 0) ? 0 :
    get_band_number(meta->general->bands, meta->general->band_count, band);
  long offset = meta->general->line_count * band_number;
  complexFloat *cdata = 
    CALLOC(meta->general->sample_count, sizeof(complexFloat));
  
  // pass 1 -- calculate mean, min & max
  FILE *fp = FOPEN(inFile, "rb");
  long long i_pixel_count=0;
  long long q_pixel_count=0;
  asfPrintStatus("\nCalculating min, max, and mean...\n");
  for (ii=0; ii<meta->general->line_count; ++ii) {
    asfPercentMeter(((double)ii/(double)meta->general->line_count));
    get_complexFloat_line(fp, meta, ii + offset, cdata);
    
    for (jj=0; jj<meta->general->sample_count; ++jj) {
      if (ISNAN(mask) || !FLOAT_EQUIVALENT2(cdata[jj].imag, mask)) {
	if (cdata[jj].imag < *i_min) *i_min = cdata[jj].imag;
	if (cdata[jj].imag > *i_max) *i_max = cdata[jj].imag;
	*i_mean += cdata[jj].imag;
	++i_pixel_count;
      }
      if (ISNAN(mask) || !FLOAT_EQUIVALENT2(cdata[jj].real, mask)) {
	if (cdata[jj].real < *q_min) *q_min = cdata[jj].real;
	if (cdata[jj].real > *q_max) *q_max = cdata[jj].real;
	*q_mean += cdata[jj].real;
	++q_pixel_count;
      }
    }
  }
  asfPercentMeter(1.0);
  FCLOSE(fp);
  
  *i_mean /= i_pixel_count;
  *q_mean /= q_pixel_count;
  
  // Guard against weird data
  if(!(*i_min<*i_max)) *i_max = *i_min + 1;
  if(!(*q_min<*q_max)) *q_max = *q_min + 1;
  
  // Initialize the histogram.
  const int num_bins = 256;
  gsl_histogram *i_hist = gsl_histogram_alloc (num_bins);
  gsl_histogram_set_ranges_uniform (i_hist, *i_min, *i_max);
  *i_stdDev = 0.0;
  gsl_histogram *q_hist = gsl_histogram_alloc (num_bins);
  gsl_histogram_set_ranges_uniform (q_hist, *q_min, *q_max);
  *q_stdDev = 0.0;
  
  // pass 2 -- update histogram, calculate standard deviation
  fp = FOPEN(inFile, "rb");
  asfPrintStatus("\nCalculating standard deviation, rmse, and histogram...\n");
  i_se = q_se = 0.0;
  for (ii=0; ii<meta->general->line_count; ++ii) {
    asfPercentMeter(((double)ii/(double)meta->general->line_count));
    get_complexFloat_line(fp, meta, ii + offset, cdata);
    
    for (jj=0; jj<meta->general->sample_count; ++jj) {
      if (ISNAN(mask) || !FLOAT_EQUIVALENT2(cdata[jj].imag, mask)) {
	*i_stdDev += (cdata[jj].imag - *i_mean) * (cdata[jj].imag - *i_mean);
	gsl_histogram_increment (i_hist, cdata[jj].imag);
	i_se += (cdata[jj].imag - *i_mean) * (cdata[jj].imag - *i_mean);
      }
      if (ISNAN(mask) || !FLOAT_EQUIVALENT2(cdata[jj].real, mask)) {
	*q_stdDev += (cdata[jj].real - *q_mean) * (cdata[jj].real - *q_mean);
	gsl_histogram_increment (q_hist, cdata[jj].real);
	q_se += (cdata[jj].real - *q_mean) * (cdata[jj].real - *q_mean);
      }
    }
  }
  asfPercentMeter(1.0);
  FCLOSE(fp);
  *i_stdDev = sqrt(*i_stdDev/(i_pixel_count - 1));
  *i_rmse = sqrt(i_se/(i_pixel_count - 1));
  *q_stdDev = sqrt(*q_stdDev/(q_pixel_count - 1));
  *q_rmse = sqrt(q_se/(q_pixel_count - 1));
  
  FREE(cdata);
  
  *i_histogram = i_hist;
  *q_histogram = q_hist;
}

void diff_check_complex_stats(char *outputFile, char *inFile1, char *inFile2,
                              complex_stats_t *cstats1, 
			      complex_stats_t *cstats2,
                              complex_psnr_t *cpsnr, int strict,
                              data_type_t data_type, int num_bands)
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
  stats_t *stats1 = NULL;
  stats_t *stats2 = NULL;
  psnr_t *psnr = NULL;
  
  int num_extracted_bands1=0, num_extracted_bands2=0;
  FILE *outputFP = NULL;
  int output_file_exists = 0;
  if (outputFile && strlen(outputFile) > 0) {
    outputFP = (FILE*)FOPEN(outputFile, "a");
    if (outputFP) {
      output_file_exists = 1;
    }
    else {
      outputFP = stderr;
      output_file_exists = 0;
    }
  }
  else {
    outputFP = stderr;
    output_file_exists = 0;
  }
  
  // Get or produce band names for intelligent output...
  char **band_names1 = NULL;
  char **band_names2 = NULL;
  get_band_names(inFile1, outputFP, &band_names1, &num_extracted_bands1);
  get_band_names(inFile2, outputFP, &band_names2, &num_extracted_bands2);
  
  // Check each band for differences
  char band_str1[64];
  char band_str2[64];
  for (band=0; band<num_bands; band++) {
    int i;
    for (i=0; i<2; i++) {
      char val_name[16];
      // Loops twice ...once to diff check the stats on the i values, once to 
      // diff check the stats on the q values
      stats1 = i ? &cstats1[band].i : &cstats1[band].q;
      stats2 = i ? &cstats2[band].i : &cstats2[band].q;
      psnr   = i ? &cpsnr[band].i   : &cpsnr[band].q;
      strcpy(val_name, i ? "i-values" : "q-values");
      
      // If differences exist, then produce an output file with content, else
      // produce an empty output file (handy for scripts that check for file 
      // existence AND file size greater than zero)
      //
      // Compare statistics
      // Assume 6-sigma range (99.999999%) is full range of data
      baseline_range1 = fabs(stats1->sdev) * 6.0; 
      // Assume 6-sigma range (99.999999%) is full range of data
      baseline_range2 = fabs(stats2->sdev) * 6.0; 
      min_tol = (MIN_DIFF_TOL/100.0)*baseline_range1;
      max_tol = (MAX_DIFF_TOL/100.0)*baseline_range1;
      mean_tol = (MEAN_DIFF_TOL/100.0)*baseline_range1;
      sdev_tol = (SDEV_DIFF_TOL/100.0)*baseline_range1;
      // FIXME: Rather than use data_type, use the baseline range to develop 
      // a suitable PSNR tolerance
      switch (data_type) 
	{
	case ASF_BYTE:
	case COMPLEX_BYTE:
	  psnr_tol = BYTE_PSNR_TOL;
	  break;
	case INTEGER16:
	case COMPLEX_INTEGER16:
	  psnr_tol = INTEGER16_PSNR_TOL;
	  break;
	case INTEGER32:
	case COMPLEX_INTEGER32:
	  psnr_tol = INTEGER32_PSNR_TOL;
	  break;
	case REAL32:
	case COMPLEX_REAL32:
	default:
	  psnr_tol = REAL32_PSNR_TOL;
	  break;
	}
      
      min_diff = fabs(stats2->min - stats1->min);
      max_diff = fabs(stats2->max - stats1->max);
      mean_diff = fabs(stats2->mean - stats1->mean);
      sdev_diff = fabs(baseline_range2 - baseline_range1);
      if (strict &&
	  (stats1->stats_good && stats2->stats_good) &&
	  (min_diff > min_tol ||
	   max_diff > max_tol ||
	   mean_diff > mean_tol ||
	   sdev_diff > sdev_tol ||
	   psnr->psnr < psnr_tol)) {
	// Strict comparison utilizes all values
	fprintf(outputFP, "\n---------------------------------------------\n");
	fprintf(outputFP, "Files contain COMPLEX values.\n");
	
	if (num_bands > 1) {
	  sprintf(band_str1, "Band %s in ", band_names1[band]);
                    sprintf(band_str2, "Band %s in ", band_names2[band]);
	}
	else {
	  strcpy(band_str1, "");
	  strcpy(band_str2, "");
	}
	sprintf(msg, "Comparing %s...\nFAIL: Comparing\n  %s%s\nto\n  %s%s\n\n",
		val_name, band_str1, inFile1, band_str2, inFile2);
	fprintf(outputFP, "%s", msg);
	
	sprintf(msg, "[%s] [min]   File1: %12f,  File2: %12f, Tolerance: %11f "
		"(%3f Percent)\n", min_diff > min_tol ? "FAIL" : "PASS",
		stats1->min, stats2->min, min_tol, MIN_DIFF_TOL);
	fprintf(outputFP, "%s", msg);
	
	sprintf(msg, "[%s] [max]   File1: %12f,  File2: %12f, Tolerance: %11f "
		"(%3f Percent)\n", max_diff > max_tol ? "FAIL" : "PASS",
		stats1->max, stats2->max, max_tol, MAX_DIFF_TOL);
	fprintf(outputFP, "%s", msg);
	
	sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: %11f "
		"(%3f Percent)\n", mean_diff > mean_tol ? "FAIL" : "PASS",
		stats1->mean, stats2->mean, mean_tol, MEAN_DIFF_TOL);
	fprintf(outputFP, "%s", msg);
	
	sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: %11f "
		"(%3f Percent)\n", sdev_diff > sdev_tol ? "FAIL" : "PASS",
		stats1->sdev, stats2->sdev, sdev_tol/6.0, SDEV_DIFF_TOL);
	fprintf(outputFP, "%s", msg);
	
	if (psnr[band].psnr_good) {
	  sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                    PSNR "
		  "Minimum: %11f (higher == better)\n",
		  psnr->psnr < psnr_tol ? "FAIL" : "PASS",
		  psnr->psnr, psnr_tol);
	}
	else {
	  sprintf(msg, "[FAIL] [PSNR]   PSNR:      MISSING,                    "
		  "PSNR Minimum: %11f (higher == better)\n",
		  psnr_tol);
	}
	fprintf(outputFP, "%s", msg);
	
	fprintf(outputFP, "---------------------------------------------\n\n");
      }
      else if (!strict &&
	       (stats1->stats_good && stats2->stats_good) &&
	       (mean_diff > mean_tol ||
		sdev_diff > sdev_tol ||
		psnr->psnr < psnr_tol))
	{
	  // If not doing strict checking, skip comparing min and max values
	  fprintf(outputFP, "\n--------------------------------------------\n");
	  fprintf(outputFP, "Files contain COMPLEX values.\n");
	  
	  char msg[1024];
	  if (num_bands > 1) {
	    sprintf(band_str1, "Band %s in ", band_names1[band]);
	    sprintf(band_str2, "Band %s in ", band_names2[band]);
	  }
	  else {
	    strcpy(band_str1, "");
	    strcpy(band_str2, "");
	  }
	  sprintf(msg, "Comparing %s...\nFAIL: Comparing\n  %s%s\nto\n  %s%s"
		  "\n\n", val_name, band_str1, inFile1, band_str2, inFile2);
	  fprintf(outputFP, "%s", msg);
	  
	  sprintf(msg, "[%s] [mean]  File1: %12f,  File2: %12f, Tolerance: "
		  "%11f (%3f Percent)\n",
		  mean_diff > mean_tol ? "FAIL" : "PASS",
		  stats1->mean, stats2->mean, mean_tol, MEAN_DIFF_TOL);
	  fprintf(outputFP, "%s", msg);
	  
	  sprintf(msg, "[%s] [sdev]  File1: %12f,  File2: %12f, Tolerance: "
		  "%11f (%3f Percent)\n",
		  sdev_diff > sdev_tol ? "FAIL" : "PASS",
		  stats1->sdev, stats2->sdev, sdev_tol/6.0, SDEV_DIFF_TOL);
	  fprintf(outputFP, "%s", msg);
	  
	  if (psnr[band].psnr_good) {
	    sprintf(msg, "[%s] [PSNR]   PSNR: %12f,                    PSNR "
		    "Minimum: %11f (higher == better)\n",
		    psnr->psnr < psnr_tol ? "FAIL" : "PASS",
		    psnr->psnr, psnr_tol);
	  }
	  else {
	    sprintf(msg, "[FAIL] [PSNR]   PSNR:      MISSING,              "
		    "      PSNR Minimum: %11f (higher == better)\n", psnr_tol);
	  }
	  fprintf(outputFP, "%s", msg);
	  
	  fprintf(outputFP, "--------------------------------------------\n\n");
	}
      else if (stats1->stats_good && stats2->stats_good) {
	if (num_bands > 1) {
	  sprintf(band_str1, "Band %s in ", band_names1[band]);
	  sprintf(band_str2, "Band %s in ", band_names2[band]);
	}
	else {
	  strcpy(band_str1, "");
	  strcpy(band_str2, "");
	}
	asfPrintStatus("\nThe files contain COMPLEX data... Comparing %s\n", 
		       val_name);
	asfPrintStatus("PASS: No differences found in Image Statistics when "
		       "comparing\n  %s%s to\n  %s%s\n\n",
		       band_str1, inFile1, band_str2, inFile2);
	print_stats_results(inFile1, inFile2,
			    band_str1, band_str2,
			    stats1, stats2,
			    i ? cpsnr[band].i : cpsnr[band].q);
      }
      
      if (!stats1->stats_good || !stats2->stats_good) {
	char msg[1024];
	
	fprintf(outputFP, "\n---------------------------------------------\n");
	fprintf(outputFP, "Files contain COMPLEX values... Comparing %s\n", 
		val_name);
	
	if (num_bands > 1) {
	  sprintf(band_str1, "Band %s in ", band_names1[band]);
	  sprintf(band_str2, "Band %s in ", band_names2[band]);
	}
	else {
	  strcpy(band_str1, "");
	  strcpy(band_str2, "");
	}
	if (!stats1->stats_good) {
	  sprintf(msg, "FAIL: %s%s image statistics missing.\n", 
		  band_str1, inFile1);
	  fprintf(outputFP, "%s", msg);
	}
	if (!stats2->stats_good) {
	  sprintf(msg, "FAIL: %s%s image statistics missing.\n", 
		  band_str2, inFile2);
	  fprintf(outputFP, "%s", msg);
	}
	
	fprintf(outputFP, "---------------------------------------------\n\n");
      }
    } // For i value stats and q value stats
  } // For each band
  
  if (output_file_exists && outputFP) FCLOSE(outputFP);
  free_band_names(&band_names1, num_extracted_bands1);
  free_band_names(&band_names2, num_extracted_bands2);
}

void matrix2asf(char *inFile, char **outFile, char **outMeta)
{
  int ii, kk, tif = FALSE;
  char *dataFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+15));
  char *hdrFile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+15));
  char *outfile = (char *) MALLOC(sizeof(char)*(strlen(inFile)+15));
  char *outmeta = (char *) MALLOC(sizeof(char)*(strlen(inFile)+15));
  char *matrixType = (char *) MALLOC(sizeof(char)*5);
  char *error = (char *) MALLOC(sizeof(char)*255);
  int matrix = isPolsarproMatrix(inFile, &matrixType, &error);
  if (matrix) {
    if (strcmp_case(matrixType, "C2") == 0 ||
      strcmp_case(matrixType, "C3") == 0 ||
      strcmp_case(matrixType, "C4") == 0) {
      sprintf(dataFile, "%s%cC11.tif", inFile, DIR_SEPARATOR);
      if (fileExists(dataFile))
        tif = TRUE;
      else
        sprintf(dataFile, "%s%cC11.bin", inFile, DIR_SEPARATOR);
      sprintf(hdrFile, "%s%cC11.bin.hdr", inFile, DIR_SEPARATOR);
      sprintf(outfile, "%s%cC11.img", inFile, DIR_SEPARATOR);
      sprintf(outmeta, "%s%cC11.meta", inFile, DIR_SEPARATOR);
    }
    else if (strcmp_case(matrixType, "T3") == 0 ||
      strcmp_case(matrixType, "T4") == 0) {
      sprintf(dataFile, "%s%cT11.tif", inFile, DIR_SEPARATOR);
      if (fileExists(dataFile))
        tif = TRUE;
      else
        sprintf(dataFile, "%s%cT11.bin", inFile, DIR_SEPARATOR);
      sprintf(hdrFile, "%s%cT11.bin.hdr", inFile, DIR_SEPARATOR);
      sprintf(outfile, "%s%cT11.img", inFile, DIR_SEPARATOR);
      sprintf(outmeta, "%s%cT11.meta", inFile, DIR_SEPARATOR);
    }
    else if (strcmp_case(matrixType, "S2") == 0) {
      sprintf(dataFile, "%s%cS11.tif", inFile, DIR_SEPARATOR);
      if (fileExists(dataFile))
        tif = TRUE;
      else
        sprintf(dataFile, "%s%cS11.bin", inFile, DIR_SEPARATOR);
      sprintf(hdrFile, "%s%cS11.bin.hdr", inFile, DIR_SEPARATOR);
      sprintf(outfile, "%s%cS11.img", inFile, DIR_SEPARATOR);
      sprintf(outmeta, "%s%cS11.meta", inFile, DIR_SEPARATOR);
    }
  }
  else {
    strcpy(dataFile, inFile);
    char *f = STRDUP(inFile);
    char *ext = findExt(f);
    *ext = '\0';
    sprintf(outfile, "%s.img", f);
    sprintf(outmeta, "%s.meta", f);
    sprintf(hdrFile, "%s.hdr", inFile);
  }
  if (tif) {
    tiff_data_t t;
    get_tiff_info_from_file(dataFile, &t);
    export_tiff_to_asf_img(dataFile, NULL, outfile, outmeta,
                            t.height, t.width, REAL32, 0);
  }
  else {
    envi_header* envi = read_envi(hdrFile);
    meta_parameters *meta = envi2meta(envi);
    if (strcmp_case(matrixType, "C2") == 0)
      meta->general->image_data_type = POLARIMETRIC_C2_MATRIX;
    else if (strcmp_case(matrixType, "C3") == 0)
      meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
    else if (strcmp_case(matrixType, "C4") == 0)
      meta->general->image_data_type = POLARIMETRIC_C4_MATRIX;
    else if (strcmp_case(matrixType, "T3") == 0)
      meta->general->image_data_type = POLARIMETRIC_T3_MATRIX;
    else if (strcmp_case(matrixType, "T4") == 0)
      meta->general->image_data_type = POLARIMETRIC_T4_MATRIX;
    else if (strcmp_case(matrixType, "S2") == 0)
      meta->general->image_data_type = POLARIMETRIC_S2_MATRIX;
    else
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    float *floatBuf = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  
    FILE *fpIn = FOPEN(dataFile, "rb");
    FILE *fpOut = FOPEN(outfile, "wb");
    for (ii=0; ii<meta->general->line_count; ii++) {
      get_float_line(fpIn, meta, ii, floatBuf);
      for (kk=0; kk<meta->general->sample_count; kk++)
        ieee_big32(floatBuf[kk]);  
      put_float_line(fpOut, meta, ii, floatBuf);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
  
    meta_write(meta, outmeta);
    meta_free(meta);
    FREE(hdrFile);
    FREE(envi);
    FREE(floatBuf);
  }
  *outFile = outfile;
  *outMeta = outmeta;
}
