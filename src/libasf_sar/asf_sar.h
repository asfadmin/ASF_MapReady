#ifndef _ASF_SAR_H_
#define _ASF_SAR_H_

#define MAX_IMG_SIZE 100000

/* values for the layover/shadow mask*/
#define MASK_NORMAL 1.0
#define MASK_LAYOVER 100.0
#define MASK_SHADOW 200.0
#define MASK_NO_DEM_DATA -1.0

/* Number of pixels added by create_dem_grid at the right edge of the 
   image, to allow for height differences */
#define DEM_GRID_RHS_PADDING 400

#include <stdio.h>
#include "poly.h"

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
	       char *outAmpFile, char *outMaskFile);

/* Prototypes from deskew_dem.c */
int deskew_dem(char *inDemName, char *outName, char *inSarName,
	       int doRadiometric, char *inMaskName, char *outMaskName,
               int fill_holes);

/* Prototypes from create_dem_grid.c */
int create_dem_grid(const char *demName, const char *sarName,
		    const char *outName);
int create_dem_grid_ext(const char *demName, const char *sarName,
			const char *outName, int w, int h, int size,
			float delta_y, double *coverage_pct);

/* Prototypes from remap_poly.c */
int remap_poly(poly_2d *fwX, poly_2d *fwY, poly_2d *bwX, poly_2d *bwY,
	       int outWidth, int outHeight, char *infile, char *outfile);

/* Prototypes from fit_poly.c */
int fit_poly(char * gridFile, int degree, double *maxErr,
	     poly_2d **fwX, poly_2d **fwY, poly_2d **bwX, poly_2d **bwY);

#endif
