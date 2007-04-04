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

#include <stdio.h>
#include "poly.h"
#include "asf_meta.h"
#include "float_image.h"

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
	       char *outAmpFile, char *inMaskFile, int interp_holes);

/* Prototypes from deskew_dem.c */
int deskew_dem(char *inDemName, char *outName, char *inSarName,
	       int doRadiometric, char *inMaskName, char *outMaskName,
	       int fill_holes, int fill_value);

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

#endif
