#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int exit_code;

/* Function definitions*/
int asf_import(char *inFile, char *outFile, char *format,
               char *prcOrbits, int prcFlag, 
	       double lat_begin, double lat_end);
int avg_in_dop(char *inFile1, char *inFile2, char *outFile1);
int doppler_per_patch(char *parFile1, char *parFile2, char *metaFile1, 
		      char *metaFile2, char *deltaFile, char *outFile1, 
		      char *outFile2);
int aisp(char *option, int startLineNum, int numPatches, char *inFile, 
	 char *outFile);
int c2p(char *inFile, char *outFile);
int cpx_autofilter(char *inFile1, char *inFile2, char *outFile1, char *outFile2);
int coregister_coarse(char *inFile1, char *inFile2, char *outFile, char *maskFile);
int coregister_fine(char *inFile1, char *inFile2, char *inCtrlFile, char *outFile, 
		    char *maskFile, int gridSize, int useFFT);
int fit_line(char *inFile, char *outFile);
int calc_deltas(char *inFile1, char *inFile2, int lineDiff, char *outFile);
int igram(char *inFile1, char *inFile2, char *outFile);	
int coh(char *inFile1, char *inFile2, char *outFile);
int multilook(char *inFile, char *outFile, char *metaFile);
int convert2byte(char *inFile, char *outFile, int nLooks, int nSmooth);
//int create_dem_grid(char *demFile, char *sarFile, char *ceosFile, char *gridFile);
int fit_plane(char *inFile, char *outFile, double fraction);
//int fit_poly(char *inFile, char *outFile, int degree);
int fit_warp(char *inFile1, char *inFile2, char *outFile);
int remap(char *inFile, char *outFile, char *options);
//int make_ddr(char *outFile, int nl, int ns, char *type);
//int reskew_dem(char *metaFile, char *demFile, char *outFile1, char *outFile2);
int trim(char *inFile, char *outFile, int sX, int sY, int nl, int ns);
//int fftMatch(char *inFile1, char *inFile2, char *outFile);
int asf_check_geolocation(char *sarName, char *mapDemName, char *offset, 
			  char *simAmpName, char *sarDemName);
int dem2phase(char *demFile, char *metaFile, char *baseFile, char *phaseFile);
int dem2seeds(char *demFile, char *ampFile, char *seedsFile, int fft);
int deramp(char *demFile, char *baseFile, char *outFile, int back);
int snaphu(char *snaphu_version, char *phaseFile, char *ampFile, char *pwrFile1, 
	   char *pwrFile2, char *config, char *outFile, int nAzimuth, int nRange, 
	   int nOverAzi, int nOverRng, int nProcs, int flattening);
int refine_base(char *phaseFile, char *seeds, char *oldBase, char *newBase);
int raster_calc(char *outFile,char *operation);
int convert2ppm(char *inFile, char *outFile);
int phase_filter(char *inFile, double strength, char *outFile);
int zeroify(char *phaseFile1, char *phaseFile2, char *outFile);
int escher(char *inFile, char *outFile);
int elev(char *phaseFile, char *baseFile, char *outFile, char *seeds);
int eleverr(char *cohFile, char *baseFile, char *maskFile, char *outFile);
int deskew_dem(char *inFile1, char *outFile, char *inFile2, int radiometric);
int asf_geocode(char *options, char *inFile, char *outFile);

#define AISP_VALID_PATCH_LENGTH 3300

#endif
