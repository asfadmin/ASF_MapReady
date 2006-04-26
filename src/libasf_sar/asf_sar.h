#ifndef _ASF_SAR_H_
#define _ASF_SAR_H_

#define MAX_IMG_SIZE 100000

/* Prototypes from gr2sr.c */
int gr2sr(const char *infile, const char *outfile);
int gr2sr_pixsiz(const char *infile, const char *outfile, float srPixSize);

/* Prototypes from reskew_dem.c */
int reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile,
	       char *outAmpFile);

/* Prototypes from deskew_dem.c */
int deskew_dem(char *inDemName, char *outName, char *inSarName,
	       int doRadiometric);

/* Prototypes from create_dem_grid.c */
int create_dem_grid(const char *demName, const char *sarName,
		    const char *outName);
int create_dem_grid_ext(const char *demName, const char *sarName,
			const char *outName, int w, int h, int size);

#endif
