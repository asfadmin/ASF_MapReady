#ifndef REGISTERLZRAWINC_C
#define REGISTERLZRAWINC_C

#include <stdio.h>

#include "register_lzraw_inc.c"

void lz2raw_flywheel(char *inFile1,char *outFile1)
{
	char command[255];

	sprintf(command, "lz2raw_flywheel %s %s", inFile1, outFile1);

	printf("%s\n", command);
	system(command);
}

void fix_in_fromraw(char *inFile1,char *outFile1)
{
	char command[255];

	sprintf(command, "fix_in_fromraw %s.par %s.meta %s.in", inFile1, outFile1, outFile1);

	printf("%s\n", command);
	system(command);
}

void swath_offset( char *inMeta1, char *inMeta2, 
				char *inPar1, char *inPar2, char *latBounds)
{
	char command[255];

	sprintf(command, "swath_offset %s.meta %s.meta %s.par %s.par boundlines", inMeta1,
							inMeta2,
							inPar1,
							inPar2);
	if(strlen(latBounds) > 0)
		strcat(command, latBounds);

	printf("%s\n", command);
	system(command);
}

void avg_in_dop(char *inFile1,char * inFile2, char *outFile1)
{
	char command[255];
	sprintf(command, "avg_in_dop %s %s %s", inFile1, inFile2, outFile1);

	printf("%s\n", command);
	system(command);
}

void aisp(char *options, int startLineNum, int numPatches, char *inFile1, 
			char *outFile1)
{
	char command[255];
	sprintf(command, "aisp -p %i %s -l %i %s %s", numPatches, options, startLineNum,
						inFile1, outFile1);

	printf("%s\n", command);
	system(command);
}

void paisp(char *options, int startLineNum, int numPatches, 
		   int numProcessors, char *inFile1,char * outFile1)
{
	char command[255];

	sprintf(command, "mpirun -np %i paisp -p %i %s -l %i %s %s", numProcessors,
							   numPatches,
							   options,
							   startLineNum,
							   inFile1,
							   outFile1);
	printf("%s\n", command);
	system(command);
}

void resolve(char *inFile1,char *inFile2,char *outFile1)
{
	char command[255];

	sprintf(command, "resolve %s %s base.00 %s", inFile1, inFile2, outFile1);

	printf("%s\n", command);
	system(command);
}


int fico(char *inFile1,char * inFile2,char * inCtrlFile,
		  char * outFile1,char * gridSize,int useFFT)
{
	char command[255];

	sprintf(command, "fico %s %s %s %s %s", inFile1, inFile2, inCtrlFile,
					   outFile1, gridSize);
	if(useFFT == 1)
		strcat(command, " -f");

	printf("%s\n", command);	
	if( (system(command) >> 8) == 101)
	{
		printf("Error: Fico could not find many offsets with\n");
		printf("interferometric phase while processing this patch.\n");
		printf("This means the interferogram\n");
		printf("is either mis-registered or just bad.\n");
		printf("This can indicate any number of problems, please \n");
		printf("check your data set to ensure that these can be \n");
		printf("co-registered.\n");
		printf("Now exiting.\n");
		return 1;
	}
	return 0;
}

void fit_line(char *inFile1, char *outFile1)
{
	char command[255];

	sprintf(command, "fit_line %s %s", inFile1, outFile1);

	printf("%s\n", command);
	system(command);
}



void calc_deltas(char *inFile1,char * inFile2,int lineDiff,
				 char *outFile1)
{
	char command[255];

	sprintf(command, "calc_deltas %s %s %i %s", inFile1, inFile2,
						lineDiff, outFile1);
	printf("%s\n", command);
	system(command);
}

void igram(char *inFile1,char *inFile2,char *outFile1)
{
	char command[255];

	sprintf(command, "igram %s %s %s", inFile1, inFile2, outFile1);

	printf("%s\n", command);
	system(command);
}
	
void pigram(char *inFile1,char *inFile2,char *outFile,int numProcessors)
{
	char command[255];

	sprintf(command, "mpirun -np %i pigram %s %s %s", numProcessors, 
							  inFile1, inFile2,
							  outFile);
	printf("%s\n", command);
	system(command);
}


void coh(char *inFile1,char *inFile2)
{
	char command[255];

	sprintf(command, "coh %s %s out_coh", inFile1, inFile2);

	printf("%s\n", command);
	system(command);
}


void pcoh(char *inFile1,char *inFile2,int numProcessors)
{
	char command[255];

	sprintf(command, "mpirun -np %i pcoh %s %s out_coh", numProcessors,
							inFile1, inFile2);
	printf("%s\n", command);
	system(command);
}

void ml(char *outFile1,char *metaFile1)
{
	char command[255];

	sprintf(command, "ml %s %s_ml -n %s", outFile1, outFile1, metaFile1);

	printf("%s\n", command);
	system(command);
}

void pml(char *outFile1,int numProcessors,char *metaFile1)
{
	char command[255];

	sprintf(command, "mpirun -np %i pml -n %s %s %s_ml", numProcessors,
							metaFile1, outFile1, 
							outFile1);
	printf("%s\n", command);
	system(command);
}


void las_op(char *outFile,char *operation)
{
	char command[255];

	sprintf(command, "las_op %s %s", outFile, operation);

	printf("%s\n", command);
	system(command);
}

#endif
