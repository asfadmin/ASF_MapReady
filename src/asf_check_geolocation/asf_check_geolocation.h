#ifndef __ASF_PROJ2SLANT_RANGE_H
#define __ASF_PROJ2SLANT_RANGE_H

#include "asf.h"
#include "asf_meta.h"

/* Prototypes for functions */
void create_dem_grid(char *demName, char *sarName, char *outName, double elev);
void fit_poly(char *inName, int degree, char *outName);
void fit_plane(char *inFile, char *outFile);
void reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile, 
                char *outAmpFile);
void shaded_relief(char *inFile, char *outFile, int addSpeckle);

#endif
