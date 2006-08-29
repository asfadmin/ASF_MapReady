#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include <stdio.h>
#include "functions.h"

int asf_import(char *inFile, char *outFile, char *format,
               char *prcOrbits, int prcFlag,
               double lat_begin, double lat_end)
{
  char options[255]="", command[255];
  int ret;

  sprintf(options, "-log %s -quiet", logFile);
  if (prcFlag) sprintf (options, "%s -prc %s", options, prcOrbits);
  if (lat_begin!=-99.0 && lat_end!=99.0)
    sprintf(options, "%s -lat %lf %lf", options, lat_begin, lat_end);
  sprintf(command, "asf_import -format %s %s %s %s",
          format, options, inFile, outFile);

  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);

  return ret;
}

int avg_in_dop(char *inFile1, char * inFile2, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "avg_in_dop %s %s %s %s", options, inFile1, inFile2, 
	  outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int doppler_per_patch(char *parFile1, char * parFile2, char *metaFile1, 
		      char *metaFile2, char *deltaFile, char *outFile1, 
		      char *outFile2)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "doppler_per_patch %s -line 1000 %s %s %s %s %s %s %s", 
	  options, parFile1, parFile2, metaFile1, metaFile2, deltaFile, 
	  outFile1, outFile2);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int ardop(char *option, int startLineNum, int numPatches, char *inFile, 
	  char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet -p %i -v %i -l %i %s", 
	  logFile, numPatches, ARDOP_VALID_PATCH_LENGTH, startLineNum, option);
  sprintf(command, "ardop %s %s %s", options, inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int c2p(char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "c2p %s %s", inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int coregister_coarse(char *inFile1, char *inFile2, char *outFile, char *maskFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  if (maskFile != NULL) sprintf(options, "%s -mask %s", options, maskFile);
  sprintf(command, "coregister_coarse %s %s %s base.00 %s", 
	  options, inFile1, inFile2, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int coregister_fine(char *inFile1, char *inFile2, char *inCtrlFile, char *outFile, 
		    char *maskFile, int gridSize, int useFFT)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet -g %d", logFile, gridSize);
  if (useFFT == 1) strcat(options, " -f");
  if (maskFile != NULL) sprintf(options, "%s -mask %s", options, maskFile);
  sprintf(command, "coregister_fine %s %s %s %s %s", 
	  options, inFile1, inFile2, inCtrlFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);	
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  if( (ret = system(command) >> 8) == 101)
    {
      printf("Error: Register_fine could not find many offsets with\n");
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
  char options[255], command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  sprintf(command, "fit_line %s %s %s", options, inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int calc_deltas(char *inFile1, char *inFile2, int lineDiff, char *outFile)
{
  char options[255]="", command[255];
  int ret;

  sprintf(options, "-log %s", logFile);
  sprintf(command, "calc_deltas %s %s %i %s", 
	  inFile1, inFile2, lineDiff, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int igram(char *inFile1, char *inFile2, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "igram %s %s %s %s", options, inFile1, inFile2, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}
	
int coh(char *inFile1, char *inFile2, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "coh %s %s %s %s", options, inFile1, inFile2, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int multilook(char *inFile, char *outFile, char *metaFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -meta %s", logFile, metaFile);
  sprintf(command, "multilook %s %s %s", options, inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int convert2byte(char *inFile, char *outFile, int nLooks, int nSmooth)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  sprintf(command, "convert2byte %s -look %dx%d -step %dx%d %s %s", 
	  options, nLooks, nLooks, nSmooth, nSmooth, inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int fit_plane(char *inFile, char *outFile, double fraction)
{
        char options[255]="", command[255];
        int ret;

	sprintf(options, "-log %s -k %.1lf", logFile, fraction);
        sprintf(command, "fit_plane %s %s %s", options, inFile, outFile);

        printf("\nCommand line: %s\nDate: ", command);
	fLog = FOPEN(logFile, "a");
	sprintf(logbuf,"\nCommand line: %s\n", command);
	printLog(logbuf);
	FCLOSE(fLog);
        ret = system(command);
         
        return ret;
}       

int fit_warp(char *inFile1, char *inFile2, char *outFile)
{
        char command[255];
        int ret;

        sprintf(command, "fit_warp %s %s %s", inFile1, inFile2, outFile);

        printf("\nCommand line: %s\nDate: ", command);
	fLog = FOPEN(logFile, "a");
	sprintf(logbuf,"\nCommand line: %s\n", command);
	printLog(logbuf);
	FCLOSE(fLog);
        ret = system(command);
         
        return ret;
}       

int remap(char *inFile, char *outFile, char *options)
{
        char command[255];
        int ret;
        
	sprintf(options, "%s -log %s", options, logFile);
        sprintf(command, "remap %s %s %s", options, inFile, outFile);

        printf("\nCommand line: %s\nDate: ", command);
	fLog = FOPEN(logFile, "a");
	sprintf(logbuf,"\nCommand line: %s\n", command);
	printLog(logbuf);
	FCLOSE(fLog);
        ret = system(command);
 
        return ret;
}

int dem2phase(char *demFile, char *metaFile, char *baseFile, char *phaseFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "dem2phase %s %s %s %s", 
	  options, demFile, baseFile, phaseFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int dem2seeds(char *demFile, char *ampFile, char *seedsFile, int fft)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  if (fft) 
    sprintf(command, "dem2seeds_fft %s %s %s", demFile, ampFile, seedsFile);
  else 
    sprintf(command, "dem2seeds %s %s %s %s", 
	    options, demFile, ampFile, seedsFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int deramp(char *demFile, char *baseFile, char *outFile, int back)
{       
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  if (back) sprintf(options, "%s -backward", options);
  sprintf(command, "deramp %s %s %s %s", 
	  options, demFile, baseFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}         


int snaphu(char *snaphu_version, char *phaseFile, char *ampFile, char *pwrFile1, 
	   char *pwrFile2, char *config, char *outFile, int nAzimuth, int nRange, 
	   int nOverAzi, int nOverRng, int nProcs, int flattening)
{       
  char command[255];
  int ret;
  
  sprintf(command, "%s --tile %d %d %d %d --nproc %d %s 4800 -m %s --AA %s %s "
	  "-f %s -o %s", snaphu_version, nAzimuth, nRange, nOverAzi, nOverRng, 
	  nProcs, phaseFile, ampFile, pwrFile1, pwrFile2, config, outFile);
  if (flattening)
    sprintf(command, "%s --tile %d %d %d %d --nproc %d %s 4800 -m %s "
	    "--AA %s %s -f %s -o %s -e out_dem_phase.phase", 
	    snaphu_version, nAzimuth, nRange, nOverAzi, nOverRng, nProcs, 
	    phaseFile, ampFile, pwrFile1, pwrFile2, config, outFile);        
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int refine_base(char *phaseFile, char *seeds, char *oldBase, char *newBase)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  sprintf(command, "refine_base %s %s %s %s %s\n", 
	  options, phaseFile, seeds, oldBase, newBase);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int raster_calc(char *outFile, char *operation)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "raster_calc %s %s %s", options, outFile, operation);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int convert2ppm(char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "convert2ppm -mask %s %s", inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int phase_filter(char *inFile, double strength, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "phase_filter %s %s %.1lf %s", 
	  options, inFile, strength, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int zeroify(char *phaseFile1, char *phaseFile2, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "zeroify %s %s %s %s", 
	  options, phaseFile1, phaseFile2, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int escher(char *inFile, char *outFile)
{       
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "escher %s %s %s", options, inFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int elev(char *phaseFile, char *baseFile, char *outFile, char *seeds)
{       
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  sprintf(command, "elev %s %s %s %s %s", 
	  options, phaseFile, baseFile, outFile, seeds);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int eleverr(char *cohFile, char *baseFile, char *maskFile, char *outFile)
{                       
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  if (maskFile!=NULL) 
    sprintf(options, "-log %s -mask %s", logFile, maskFile); 
  sprintf(command, "eleverr %s %s %s %s", 
	  options, cohFile, baseFile, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

int deskew_dem(char *inFile1, char *outFile, char *inFile2, int radiometric)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  if (strcmp(inFile2,"")!=0) 
    sprintf(options, "-i %s %d", inFile2, radiometric); 
  sprintf(command, "deskew_dem %s %s %s", 
	  options, inFile1, outFile);
  
  printf("\nCommand line: %s\nDate: ", command);
  fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\nDate: ", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  
  return ret;
}

#endif
