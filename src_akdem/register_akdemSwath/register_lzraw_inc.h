#ifndef REGISTERLZRAWINC_H
#define REGISTERLZRAWINC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Function definitions*/
void lz2raw_flywheel(char *inFile1,char *outFile1);
void lz2raw_par(char *inFile1,char *outFile1);
void lz2raw(char *inFile1,char *outFile1);
void fix_in_fromraw(char *inFile1,char *outFile1);
void swath_offset( char *inMeta1, char *inMeta2, 
				char *inPar1, char *inPar2, char *latBounds);
void water_mask(char *demName,char *boundFileName, float rangePercent);

void avg_in_dop(char *inFile1,char * inFile2, char *outFile1);

void aisp(char *options, int startLineNum, int numPatches, char *inFile1, 
			char *outFile1);

void paisp(char *options, int startLineNum, int numPatches, 
		   int numProcessors, char *inFile1,char * outFile1);

void resolve(char *inFile1,char *inFile2,char *outFile1);

int fico(char *inFile1,char * inFile2,char * inCtrlFile,
		  char * outFile1,char * gridSize,int useFFT);

void fit_line(char *inFile1, char *outFile1);

void calc_deltas(char *inFile1,char * inFile2,int lineDiff,
				 char *outFile1);

void igram(char *inFile1,char *inFile2,char *outFile1);
	
void pigram(char *inFile1,char *inFile2,char *outFile,int numProcessors);

void coh(char *inFile1,char *inFile2);

void pcoh(char *inFile1,char *inFile2,int numProcessors);

void ml(char *outFile1,char *metaFile1);

void pml(char *outFile1,int numProcessors,char *metaFile1);

void las_op(char *outFile,char *operation);

#define AISP_VALID_PATCH_LENGTH 3300

#endif
