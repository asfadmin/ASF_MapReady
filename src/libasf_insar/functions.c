#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include "functions.h"

int c2p_exec(char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "c2p %s %s", inFile, outFile);
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

int fit_line(char *inFile, char *outFile)
{
  char options[255], command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  sprintf(command, "fit_line %s %s %s", options, inFile, outFile);
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

int convert2byte(char *inFile, char *outFile, int nLooks, int nSmooth)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s -quiet", logFile);
  sprintf(command, "convert2byte %s -look %dx%d -step %dx%d %s %s", 
	  options, nLooks, nLooks, nSmooth, nSmooth, inFile, outFile);
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

int fit_plane(char *inFile, char *outFile, double fraction)
{
        char options[255]="", command[255];
        int ret;

	sprintf(options, "-log %s -k %.1lf", logFile, fraction);
        sprintf(command, "fit_plane %s %s %s", options, inFile, outFile);

        printf("\nCommand line: %s\n", command);
	//fLog = FOPEN(logFile, "a");
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

        printf("\nCommand line: %s\n", command);
	//fLog = FOPEN(logFile, "a");
	sprintf(logbuf,"\nCommand line: %s\n", command);
	printLog(logbuf);
	FCLOSE(fLog);
        ret = system(command);
        fLog = FOPEN(logFile, "a");
         
        return ret;
}       

int remap(char *inFile, char *outFile, char *options)
{
        char command[255];
        int ret;
        
	sprintf(options, "%s -log %s", options, logFile);
        sprintf(command, "remap %s %s %s", options, inFile, outFile);

        printf("\nCommand line: %s\n", command);
	//fLog = FOPEN(logFile, "a");
	sprintf(logbuf,"\nCommand line: %s\n", command);
	printLog(logbuf);
	FCLOSE(fLog);
        ret = system(command);
        fLog = FOPEN(logFile, "a");
 
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
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

int phase_filter(char *inFile, double strength, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "phase_filter %s %s %.1lf %s", 
	  options, inFile, strength, outFile);
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

int zeroify(char *phaseFile1, char *phaseFile2, char *outFile)
{
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "zeroify %s %s %s %s", 
	  options, phaseFile1, phaseFile2, outFile);
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

int escher(char *inFile, char *outFile)
{       
  char options[255]="", command[255];
  int ret;
  
  sprintf(options, "-log %s", logFile);
  sprintf(command, "escher %s %s %s", options, inFile, outFile);
  
  printf("\nCommand line: %s\n", command);
  //fLog = FOPEN(logFile, "a");
  sprintf(logbuf,"\nCommand line: %s\n", command);
  printLog(logbuf);
  FCLOSE(fLog);
  ret = system(command);
  fLog = FOPEN(logFile, "a");
  
  return ret;
}

#endif
