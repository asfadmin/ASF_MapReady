#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include <stdio.h>
#include "functions.h"

int lz2raw_flywheel(char *inFile, char *outFile, char *prcOrbits, int prcFlag, double lat_begin, double lat_end)
{
        char command[255];
	int ret;

        sprintf(command, "lz2raw_flywheel %s %s", inFile, outFile);
	if (prcFlag == 1) sprintf (command, "%s -prc %s", command, prcOrbits); 
        if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");
/*	if (lat_begin!=-99.0 && lat_end!=99.0) sprintf(command, "%s -lat %lf %lf", command, lat_begin, lat_end);*/

        printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
        ret = system(command);

	return ret;
}

int ceos2raw(char *inFile, char *outFile)
{
        char command[255];
	int ret;

        sprintf(command, "ceos2raw %s %s", inFile, outFile);        
	/* if (logflag) sprintf(command, "%s -log %s", command, logFile); */
	/* if (quietflag) strcat(command, " -quiet"); */

        printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
        ret = system(command);

	return ret;
}

int trim_slc(char *inFile, char *outFile, int line, int sample, int length, int width)
{
        char command[255];
	int ret;

        sprintf(command, "trim_slc %s %s %d %d %d %d", inFile, outFile, line, sample, length, width);        
/*	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet"); */

        printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
        ret = system(command);

	return ret;
}

int fix_in_fromraw(char *inFile, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "fix_in_fromraw %s.par %s.meta %s.in", inFile, outFile, outFile);
	if (logflag) sprintf(command, "fix_in_fromraw %s.par %s.meta %s.in 100 -log %s", 
				inFile, outFile, outFile, logFile);
	if (quietflag) sprintf(command, "fix_in_fromraw %s.par %s.meta %s.in 100 -quiet", inFile, outFile, outFile);
	if (logflag && quietflag) sprintf(command, "fix_in_fromraw %s.par %s.meta %s.in 100 -log %s -quiet", inFile,
						outFile, outFile, logFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int swath_offset( char *inMeta1, char *inMeta2, char *inPar1, char *inPar2, char *latBounds)
{
	char command[255];
	int ret;

	sprintf(command, "swath_offset %s.meta %s.meta %s.par %s.par boundlines", inMeta1, inMeta2, inPar1, inPar2);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);
/*	if (quietflag) strcat(command, " -quiet");*/
	if(strlen(latBounds) > 0) strcat(command, latBounds);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int water_mask(char *demName, char *boundFileName, float rangePercent)
{
	char command[255];
	int ret;

	sprintf(command, "water_mask %s %s", demName, boundFileName);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");
/*	if(rangePercent != 100.0)
	{
		char tmp[100];

		sprintf(tmp, " -r %f", rangePercent);
		strcat(command, tmp);
	}*/

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int avg_in_dop(char *inFile1, char * inFile2, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "avg_in_dop %s %s %s", inFile1, 
		inFile2, outFile);
	if (logflag) sprintf(command, "avg_in_dop -log %s %s %s %s", logFile, 
			     inFile1, inFile2, outFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int aisp(char *options, int startLineNum, int numPatches, char *inFile, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "aisp -p %i -v %i %s -l %i %s %s", numPatches, AISP_VALID_PATCH_LENGTH,
		options, startLineNum, inFile, outFile);
	if (logflag) sprintf(command, "aisp -p %i -v %i %s -l %i -log %s %s %s", numPatches,
		 AISP_VALID_PATCH_LENGTH, options, startLineNum, logFile, inFile, outFile);
	if (quietflag) sprintf(command, "aisp -p %i -v %i %s -l %i -quiet %s %s", numPatches,
		 AISP_VALID_PATCH_LENGTH, options, startLineNum, inFile, outFile);
	if (logflag && quietflag) sprintf(command, "aisp -p %i -v %i %s -l %i %s %s",
		 numPatches,AISP_VALID_PATCH_LENGTH, options, startLineNum, inFile, outFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int paisp(char *options, int startLineNum, int numPatches, int numProcessors, char *inFile, char * outFile)
{
	char command[255];
	int ret;

	sprintf(command, "mpirun -np %i paisp -p %i -v %i %s -l %i %s %s", numProcessors, numPatches,
		 AISP_VALID_PATCH_LENGTH, options, startLineNum, inFile, outFile);
	if (logflag) sprintf(command, "mpirun -np %i paisp -p %i -v %i %s -l %i -x %s %s %s",
		 numProcessors, numPatches, AISP_VALID_PATCH_LENGTH, options, startLineNum, logFile, inFile, outFile);
	if (quietflag) sprintf(command, "mpirun -np %i paisp -p %i -v %i %s -l %i -q 1 %s %s",
		 numProcessors, numPatches, AISP_VALID_PATCH_LENGTH, options, startLineNum, inFile, outFile);
	if (logflag && quietflag) sprintf(command, "mpirun -np %i paisp -p %i -v %i %s -l %i -x %s -q 1 %s %s",
		 numProcessors, numPatches, AISP_VALID_PATCH_LENGTH, options, startLineNum, logFile, inFile, outFile);

	if (numPatches == -1) {
	  sprintf(command, "mpirun -np %i paisp -v %i %s %s %s", numProcessors,
		 AISP_VALID_PATCH_LENGTH, options, inFile, outFile);
	  if (logflag) sprintf(command, "mpirun -np %i paisp -v %i %s -x %s %s %s",
		 numProcessors, AISP_VALID_PATCH_LENGTH, options, logFile, inFile, outFile);
	  if (quietflag) sprintf(command, "mpirun -np %i paisp -v %i %s -q 1 %s %s",
		 numProcessors, AISP_VALID_PATCH_LENGTH, options, inFile, outFile);
	  if (logflag && quietflag) sprintf(command, "mpirun -np %i paisp -v %i %s -x %s -q 1 %s %s",
		 numProcessors, AISP_VALID_PATCH_LENGTH, options, logFile, inFile, outFile);
	}

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);
	
	return ret;
}

int c2p(char *inFile, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "c2p %s %s", inFile, outFile);
/*	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");*/

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int cpx_autofilter(char *inFile1, char *inFile2, char *outFile1, char *outFile2)
{
	char command[255];
	int ret;

	sprintf(command, "cpx_autofilter %s %s %s %s", inFile1, inFile2, outFile1, outFile2);
/*	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");*/

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int pcpx_autofilter(char *inFile1, char *inFile2, char *outFile1, char *outFile2, int procs)
{
	char command[255];
	int ret;

	sprintf(command, "pcpx_autofilter %s %s %s %s %d", inFile1, inFile2, outFile1, outFile2, procs);
/*	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");*/

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int resolve(char *inFile1, char *inFile2, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "resolve %s %s base.00 %s", inFile1, inFile2, outFile);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);
/*	if (quietflag) strcat(command, " -quiet");*/

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int fico(char *inFile1, char *inFile2, char *inCtrlFile, char *outFile, int gridSize, int useFFT)
{
	char command[255];
	int ret;

	sprintf(command, "fico %s %s %s %s %i", inFile1, inFile2, inCtrlFile, outFile, gridSize);
	if (useFFT == 1) strcat(command, " -f");
	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");	

	printf("\nCommand line: %s\nDate: ", command);	
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	if( (ret = system(command) >> 8) == 101)
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
	return ret;
}

int fit_line(char *inFile, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "fit_line %s %s", inFile, outFile);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);
	if (quietflag) strcat(command, " -quiet");

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);
	
	return ret;
}

int calc_deltas(char *inFile1, char *inFile2, int lineDiff, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "calc_deltas %s %s %i %s", inFile1, inFile2, lineDiff, outFile);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int igram(char *inFile1, char *inFile2, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "igram %s %s %s", inFile1, inFile2, outFile);

	printf("\nCommand line: %s\nDate: ", command);
	ret = system(command);

	return ret;
}
	
int pigram(char *inFile1, char *inFile2, char *outFile, int numProcessors)
{
	char command[255];
	int ret;

	sprintf(command, "mpirun -np %i pigram %s %s %s", numProcessors, inFile1, inFile2, outFile);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int coh(char *inFile1, char *inFile2, char *outFile)
{
	char command[255];
	int ret;

	sprintf(command, "coh %s %s %s", inFile1, inFile2, outFile);

	printf("\nCommand line: %s\nDate: ", command);
	ret = system(command);

	return ret;
}

int pcoh(char *inFile1, char *inFile2, char *outFile, int numProcessors)
{
	char command[255];
	int ret;

	sprintf(command, "mpirun -np %i pcoh %s %s %s", numProcessors, inFile1, inFile2, outFile);
	if (logflag) sprintf(command, "mpirun -np %i pcoh -x %s %s %s %s", numProcessors, logFile, inFile1, inFile2, outFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int ml(char *inFile, char *outFile, char *metaFile)
{
	char command[255];
	int ret;

	sprintf(command, "ml %s %s -n %s", inFile, outFile, metaFile);

	printf("\nCommand line: %s\nDate: ", command);
	ret = system(command);

	return ret;
}

int pml(char *inFile, char *outFile, int numProcessors, char *metaFile)
{
	char command[255];
	int ret;

	sprintf(command, "mpirun -np %i pml -a -n %s %s %s", numProcessors, metaFile, inFile, outFile);
	if (logflag) sprintf(command, "mpirun -np %i pml -a -n %s -x %s %s %s", 
						numProcessors, metaFile, logFile, inFile, outFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
	ret = system(command);

	return ret;
}

int amp2img(char *inFile, char *outFile, int nLooks, int nSmooth)
{
        char command[255];
        int ret;
        
        sprintf(command, "amp2img -look %dx%d -step %dx%d %s %s", nLooks, nSmooth, nLooks, nSmooth, inFile, outFile);
        if (logflag) sprintf(command, "amp2img -log %s -look %dx%d -step %dx%d %s %s", 
				logFile, nLooks, nSmooth, nLooks, nSmooth, inFile, outFile);
        if (quietflag) sprintf(command, "amp2img -quiet -look %dx%d -step %dx%d %s %s", 
				nLooks, nSmooth, nLooks, nSmooth, inFile, outFile);
        if (quietflag && logflag) 
	  sprintf(command, "amp2img -quiet -log %s -look %dx%d -step %dx%d %s %s", 
				logFile, nLooks, nSmooth, nLooks, nSmooth, inFile, outFile);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
         
        return ret;
}

int create_dem_grid(char *demFile, char *sarFile, char *ceosFile, char *gridFile)
{
        char command[255];
        int ret;

        sprintf(command, "create_dem_grid %s %s %s %s", demFile, sarFile, ceosFile, gridFile);
	if (logflag) sprintf(command, "create_dem_grid -log %s %s %s %s %s", logFile, demFile, sarFile, ceosFile, gridFile);

	printf("\nCommand line: %s\nDate: ", command);
	if (logflag) {
	  fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
	  printLog(logbuf);
	  FCLOSE(fLog);
	}
        ret = system(command);
          
        return ret;
}

int fit_plane(char *inFile, char *outFile, double fraction)
{
        char command[255];
        int ret;

        sprintf(command, "fit_plane %s %s k %.1lf", inFile, outFile, fraction);
	if (logflag) sprintf(command, "%s -log %s", command, logFile);

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
         
        return ret;
}       

int fit_warp(char *inFile1, char *inFile2, char *outFile)
{
        char command[255];
        int ret;

        sprintf(command, "fit_warp %s %s %s", inFile1, inFile2, outFile);
/*	if (logflag) sprintf(command, "%s -log %s", command, logFile);*/

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
         
        return ret;
}       

int remap(char *inFile, char *outFile, char *options)
{
        char command[255];
        int ret;
        
        sprintf(command, "remap %s %s %s", options, inFile, outFile);
        if (logflag) sprintf(command, "remap %s -log %s %s %s", options, logFile, inFile, outFile); 

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
 
        return ret;
}

int make_ddr(char *outFile, int nl, int ns, char *type)
{
        char command[255];
        int ret;
                                
        sprintf(command, "makeddr %s %d %d %s", outFile, nl, ns, type);
	if (logflag) sprintf(command, "makeddr -log %s %s %d %d %s", logFile, outFile, nl, ns, type); 
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}

int reskew_dem(char *demFile, char *metaFile, char *outFile1, char *outFile2)
{       
        char command[255];
        int ret;
                                
        sprintf(command, "reskew_dem %s %s %s %s", demFile, metaFile, outFile1, outFile2);
	if (logflag) sprintf(command, "%s -log %s", command, logFile); 
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}         

int trim(char *inFile, char *outFile, int sX, int sY, int nl, int ns)
{
        char command[255];
        int ret;
        
        sprintf(command, "trim -h %d -w %d %s %s %i %i", nl, ns, inFile, outFile, sX, sY);
        if (logflag) sprintf(command, "trim -log %s -h %d -w %d %s %s %i %i", logFile, nl, ns, inFile, outFile, sX, sY);
                                
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
         
        return ret;
}       

int fftMatch(char *inFile1, char *inFile2, char *outFile)
{       
        char command[255];
        int ret;
        
        sprintf(command, "fftMatch -m %s %s %s", outFile, inFile1, inFile2);
        if (logflag) sprintf(command, "fftMatch -m %s -log %s %s %s", outFile, logFile, inFile1, inFile2);
        if (quietflag) sprintf(command, "fftMatch -m %s -quiet %s %s", outFile, inFile1, inFile2);
	if (logflag && quietflag) sprintf(command, "fftMatch -m %s -log %s -quiet %s %s", outFile, logFile, inFile1, inFile2);
        
        printf("\nCommand line: %s\nDate: ", command);
	system("date");
	printf("Program: fftMatch\n\n");
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
	  StartWatchLog(fLog);
	  printLog("Program: fftMatch\n\n");
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
} 

int dem2phase(char *demFile, char *metaFile, char *baseFile, char *phaseFile)
{
        char command[255];
        int ret;
        
        sprintf(command, "dem2phase %s %s %s %s", demFile, metaFile, baseFile, phaseFile);
	if (logflag) sprintf(command, "dem2phase -log %s %s %s %s %s", logFile, demFile, metaFile, baseFile, phaseFile); 
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
          
        return ret;
}

int dem2seeds(char *demFile, char *ampFile, char *seedsFile, int fft)
{
        char command[255];
        int ret;
        
        if (fft) 
          sprintf(command, "dem2seeds_fft %s %s %s", demFile, ampFile, seedsFile);
        else 
          sprintf(command, "dem2seeds %s %s %s", demFile, ampFile, seedsFile);
        if (logflag) sprintf(command, "dem2seeds -log %s %s %s %s", logFile, demFile, ampFile, seedsFile);

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}

int deramp(char *demFile, char *metaFile, char *baseFile, char *outFile, int back)
{       
        char command[255];
        int ret;
 
        sprintf(command, "deramp %s %s %s %s", demFile, metaFile, baseFile, outFile);
	if (back) sprintf(command, "deramp -backward %s %s %s %s", demFile, metaFile, baseFile, outFile);
        if (logflag) sprintf(command, "deramp -log %s %s %s %s %s", logFile, demFile, metaFile, baseFile, outFile);
        if (logflag && back) sprintf(command, "deramp -backward -log %s %s %s %s %s", 
						logFile, demFile, metaFile, baseFile, outFile);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
         
        return ret;
}         


int snaphu_v2(char *phaseFile, char *ampFile, char *pwrFile1, char *pwrFile2, char *config, char *outFile, 
			int nAzimuth, int nRange, int nProcs)
{       
        char command[255];
        int ret;
        
        sprintf(command, "snaphu_v2 --tile %d %d 400 400 --nproc %d %s 4800 -m %s --AA %s %s -f %s -o %s", 
				nAzimuth, nRange, nProcs, phaseFile, ampFile, pwrFile1, pwrFile2, config, outFile);        
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}

int refine_base(char *phaseFile, char *seeds, char *metaFile, char *oldBase, char *newBase)
{
        char command[255];
        int ret;
        
        sprintf(command, "refine_base %s %s %s %s %s\n", phaseFile, seeds, metaFile, oldBase, newBase);
	if (logflag) 
	  sprintf(command, "refine_base -log %s %s %s %s %s %s\n", logFile, phaseFile, seeds, metaFile, oldBase, newBase);
/*	if (quietflag) 
	  sprintf(command, "refine_base -quiet %s %s %s %s %s\n", phaseFile, seeds, metaFile, oldBase, newBase);
	if (logflag && quietflag) 
	  sprintf(command, "refine_base -quiet -log %s %s %s %s %s %s\n", 
				logFile, phaseFile, seeds, metaFile, oldBase, newBase);*/

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\n", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}

int las_op(char *outFile, char *operation)
{
        char command[255];
	int ret;

        sprintf(command, "las_op %s %s", outFile, operation);
        if (logflag) sprintf(command, "las_op %s -log %s %s", outFile, logFile, operation);

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);

	return ret;
}

int las2ppm(char *inFile, char *outFile)
{
        char command[255];
	int ret;

        sprintf(command, "las2ppm -m %s %s", inFile, outFile);

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);

	return ret;
}

int phase_filter(char *inFile, double strength, char *outFile)
{
        char command[255];
	int ret;
          
        sprintf(command, "phase_filter %s %.1lf %s", inFile, strength, outFile);
        if (logflag) sprintf(command, "phase_filter -log %s %s %.1lf %s", logFile, inFile, strength, outFile);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
          
        return ret;
}

int zeroify(char *phaseFile1, char *phaseFile2, char *outFile)
{
        char command[255];
	int ret;

        sprintf(command, "zeroify %s %s %s", phaseFile1, phaseFile2, outFile);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
          
        return ret;
}

int escher(char *inFile, char *outFile)
{       
        char command[255];
        int ret;
        
        sprintf(command, "escher %s %s", inFile, outFile);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
          
        return ret;
}

int elev(char *phaseFile, char *baseFile, char *metaFile, char *outFile, char *seeds)
{       
        char command[255];
        int ret;
        
        sprintf(command, "elev %s %s %s %s %s", phaseFile, baseFile, metaFile, outFile, seeds);
        if (logflag) sprintf(command, "elev -log %s %s %s %s %s %s", logFile, phaseFile, baseFile, metaFile, outFile, seeds);
        if (quietflag) sprintf(command, "elev -quiet %s %s %s %s %s", phaseFile, baseFile, metaFile, outFile, seeds);
        if (logflag && quietflag) 
	  sprintf(command, "elev -log %s -quiet %s %s %s %s %s", logFile, phaseFile, baseFile, metaFile, outFile, seeds);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
          
        return ret;
}

int eleverr(char *cohFile, char *baseFile, char *metaFile, char *maskFile, char *outFile)
{                       
        char command[255];
	int ret;
 
	if (maskFile==NULL) {
          sprintf(command, "eleverr %s %s %s %s", cohFile, baseFile, metaFile, outFile);
	  if (logflag) sprintf(command, "eleverr -log %s %s %s %s %s", logFile, cohFile, baseFile, metaFile, outFile);
	}
	else {
          sprintf(command, "eleverr -mask %s %s %s %s %s", maskFile, cohFile, baseFile, metaFile, outFile);
	  if (logflag)
            sprintf(command, "eleverr -log %s -mask %s %s %s %s %s", logFile, maskFile, cohFile, baseFile, metaFile, outFile);
	}

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
          
        return ret;
}
          
int deskew_dem(char *inFile1, char *metaFile, char *outFile, char *inFile2)
{
        char command[255];
        int ret;

	if (strcmp(inFile2,"")==0)
		sprintf(command, "deskew_dem %s %s %s", inFile1, metaFile, outFile);
	else
	        sprintf(command, "deskew_dem -i %s 1 %s %s %s", inFile2, inFile1, metaFile, outFile);
        if (logflag) sprintf(command, "-log %s %s", logFile, command);
          
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
 
        return ret;
}

int projprm(char *projection, char *projkey, char *outFile, char *options)
{       
        char command[255];
        int ret;
            
        sprintf(command, "projprm %s %s %s %s", projection, projkey, outFile, options);
	if (logflag)
          sprintf(command, "projprm -log %s %s %s %s %s", logFile, projection, projkey, outFile, options);
        
        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}       

int geocode(char *metaFile, char *inFile, char *projFile, char *projkey, int pix_size, char *outFile)
{
        char command[255];
        int ret;
        
        sprintf(command, "geocode -p %d %s %s %s %s %s", pix_size, metaFile, inFile, projFile, projkey, outFile);
	if (logflag) sprintf(command, "geocode -p %d -x %s %s %s %s %s %s", 
					pix_size, logFile, metaFile, inFile, projFile, projkey, outFile);

        printf("\nCommand line: %s\nDate: ", command);
        if (logflag) {
          fLog = FOPEN(logFile, "a");
          sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
          printLog(logbuf);
          FCLOSE(fLog);
        }
        ret = system(command);
        
        return ret;
}

#endif
