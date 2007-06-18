#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <asf_import.h>
#include <asf_terrcorr.h>
#include <asf_geocode.h>
#include <asf_export.h>

int exit_code;

/* Function definitions*/
int ardop(char *option, int startLineNum, int numPatches, char *inFile, 
	  char *outFile);
int c2p(char *inFile, char *outFile);
int coregister_coarse(char *inFile1, char *inFile2, char *outFile, char *maskFile);
int coregister_fine(char *inFile1, char *inFile2, char *inCtrlFile, char *outFile, 
		    char *maskFile, int gridSize, int useFFT);
int fit_line(char *inFile, char *outFile);
int calc_deltas(char *inFile1, char *inFile2, int lineDiff, char *outFile);
int convert2byte(char *inFile, char *outFile, int nLooks, int nSmooth);
int fit_plane(char *inFile, char *outFile, double fraction);
int fit_warp(char *inFile1, char *inFile2, char *outFile);
int remap(char *inFile, char *outFile, char *options);
int snaphu(char *snaphu_version, char *phaseFile, char *ampFile, char *pwrFile1, 
	   char *pwrFile2, char *config, char *outFile, int nAzimuth, int nRange, 
	   int nOverAzi, int nOverRng, int nProcs, int flattening);
int raster_calc(char *outFile,char *operation);

#define ARDOP_VALID_PATCH_LENGTH 3300

#endif
