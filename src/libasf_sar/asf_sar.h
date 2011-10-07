#ifndef _ASF_SAR_H_
#define _ASF_SAR_H_

#define MAX_IMG_SIZE 100000

/* values for the layover/shadow mask*/
#define MASK_NORMAL 1
#define MASK_USER_MASK 2
#define MASK_SHADOW 3
#define MASK_LAYOVER 4
#define MASK_INVALID_DATA 5

/* At the moment, changing this to something other than 0 doesn't work.
   Needs to be investigated. */
#define BAD_DEM_HEIGHT 0.0

/* Choose a value not likely to occur in a DEM, and also won't screw up
   the range too much when viewing the DEM */
#define NO_DEM_DATA -3.333

/* Number of pixels added by create_dem_grid at the right edge of the
   image, to allow for height differences */
#define DEM_GRID_RHS_PADDING 400

/** The value of "fill_value" that means "leave masked out data as-is" */
#define LEAVE_MASK -1

/* Values for "which_gr_dem" in deskew_dem */
#define BACKCONVERTED_GR_DEM 1
#define ORIGINAL_GR_DEM 2

#include <stdio.h>
#include "poly.h"
#include "asf_meta.h"
#include "float_image.h"

/* For use by the "classifier" in the polarimetry calculations
   Used by: classify.c and polarimetry.c
   Not really for use outside of this library. */
typedef struct {
    int n_classes;
    double *entropy_min;
    double *entropy_max;
    double *anisotropy_min;
    double *anisotropy_max;
    double *alpha_min;
    double *alpha_max;
    int *greyscale_value;
} classifier_t;

/* Prototypes from gr2sr.c */
int gr2sr(const char *infile, const char *outfile);
int gr2sr_pixsiz(const char *infile, const char *outfile, float srPixSize);
int gr2sr_pixsiz_pp(const char *infile, const char *outfile,
                    float srPixSize);

/* Prototypes from sr2gr.c */
int sr2gr(const char *infile, const char *outfile);
int sr2gr_pixsiz(const char *infile, const char *outfile, float srPixSize);

/* Prototypes from reskew_dem.c */
int reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile,
               char *outAmpFile, char *inMaskFile, int add_speckle);
int reskew_dem_rad(char *inMetafile, char *inDEMfile, char *outDEMslant,
                   char *outDEMground, char *outAmpFile, char *inMaskFile,
                   radiometry_t rad, int add_speckle);

/* Prototypes from deskew_dem.c */
int deskew_dem(char *inDemSlant, char *inDemGround, char *outName,
               char *inSarName, int doRadiometric, char *inMaskName,
               char *outMaskName, int fill_holes, int fill_value,
               int which_gr_dem);

/* Prototypes from create_dem_grid.c */
int create_dem_grid(const char *demName, const char *sarName,
            const char *outName);
int create_dem_grid_ext(const char *demName, const char *sarName,
            const char *outName, int w, int h, int size,
            double *coverage_pct);

/* Prototypes from remap_poly.c */
int remap_poly(poly_2d *fwX, poly_2d *fwY, poly_2d *bwX, poly_2d *bwY,
           int outWidth, int outHeight, char *infile, char *outfile,
               float background_value);

/* Prototypes from fit_poly.c */
int fit_poly(char * gridFile, int degree, double *maxErr,
         poly_2d **fwX, poly_2d **fwY, poly_2d **bwX, poly_2d **bwY);

/* Prototypes from refine_offset.c */
void refine_offset(double x_off, double y_off, meta_parameters *meta,
                   double *out_t, double *out_x);

/* Prototypes from mfd.c */
int dem_to_mask(char *inDem, char *outMask, float cutoff);

int is_masked(double value);
float masked_value();
float unmasked_value();

/* Prototypes from to_sr.c */
int to_sr(const char *infile, const char *outfile);
int to_sr_pixsiz(const char *infile, const char *outfile, double pixel_size);

/* Prototypes from interp_dem_holes.c */
void interp_dem_holes_data(meta_parameters *meta, float *data, float cutoff,
                           int verbose);
void interp_dem_holes_file(const char *infile, const char *outfile,
                           float cutoff, int verbose);
void interp_dem_holes_float_image(FloatImage *img, float cutoff, int verbose);
void interp_dem_holes_float_image_rick(meta_parameters *meta,
                                       FloatImage *img,
                                       float cutoff, int verbose,
                                       int max_hole_width,
                                       int max_hole_height,
                                       double max_slope_angle,
                                       int do_diags);

/* Prototypes from deskew.c */
void deskew(const char *infile, const char *outfile);

/* Prototypes from c2p.c */
void c2p(const char *inDataName, const char *outfile,
         int multilook, int banded);
// "ext" version allows different metadata file name
void c2p_ext(const char *inDataName, const char *inMetaName,
             const char *outfile, int multilook, int banded);

/* Prototypes from classify.c (helper stuff for polarimetry) */
classifier_t *read_classifier(const char *classFile);
void free_classifier(classifier_t *classifier);
int classify(classifier_t *classifier, float entropy, float anisotropy,
             float alpha);

/* find_band.c */
const char *get_cal_band_name(meta_parameters *meta, char *base);
int find_band(meta_parameters *meta, char *name, int *ok);

/* Prototypes from polarimetry.c */
void polarimetric_decomp(const char *inFile, const char *outFile,
                         int amplitude_band,
                         int pauli_1_band,
                         int pauli_2_band,
                         int pauli_3_band,
                         int entropy_band,
                         int anisotropy_band,
                         int alpha_band,
                         int sinclair_1_band,
                         int sinclair_2_band,
                         int sinclair_3_band,
                         int freeman_1_band,
                         int freeman_2_band,
                         int freeman_3_band,
                         const char *classFile,
                         int class_band);
void cpx2sinclair(const char *inFile, const char *outFile, int tc_flag);
void cpx2pauli(const char *inFile, const char *outFile, int tc_flag);
void cpx2cloude_pottier(const char *inFile, const char *outFile, int tc_flag);
void cpx2entropy_anisotropy_alpha(const char *inFile, const char *outFile,
                                  int tc_flag);
void cpx2cloude_pottier8(const char *inFile, const char *outFile, int tc_flag);
void cpx2cloude_pottier16(const char *inFile, const char *outFile,
                          int tc_flag);
void cpx2debug(const char *inFile, const char *outFile);
void cpx2freeman_durden(const char *inFile, const char *outFile, int tc_flag);

void make_entropy_alpha_boundary(const char *fname, int size);


/* farcorr.c */
void faraday_correct(const char *inFile, const char *outFile, double threshold,
                     int save_intermediates, int use_single_rotation_value,
                     radiometry_t output_radiometry, int ksize);

// calibrate.c
int asf_calibrate(const char *inFile, const char *outFile, 
		  radiometry_t radiometry, int wh_scaleFlag);

#endif
