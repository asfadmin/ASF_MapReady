#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include <stdio.h>
#include "functions.h"

int import2asf(char *inFile, char *metaFile, char *type, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "import2asf -log %s %s CEOS %s %s %s", 
	  logFile, type, inFile, metaFile, outFile);
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

int projprm(char *projection, char *projkey, char *outFile, char *options)
{       
  char command[255];
  int ret;
  
  sprintf(command, "projprm -log %s %s %s %s %s", 
	  logFile, projection, projkey, outFile, options);
  
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

int geocode(char *inFile, char *projFile, char *projkey, float pix_size, 
	    float height, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "geocode -p %.2f -h %.3f -log %s %s %s %s %s", 
	  pix_size, height, logFile, inFile, projFile, projkey, outFile);
  
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

int corner_coords(char *inFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "corner_coords %s", inFile);
  printf("\nCommand line: %s\nDate: ", command);
  ret = system(command);
  
  return ret;
}


int filter(char *options, char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "filter %s -log %s %s %s", options, logFile, inFile, outFile);
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

int convert2byte(char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "convert2byte -log %s %s %s", logFile, inFile, outFile);
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

int convert2geotiff(char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "convert2geotiff -log %s -quiet %s %s", 
	  logFile, inFile, outFile);
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

int convert2jpeg(char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "convert2jpeg %s %s", inFile, outFile);
  printf("\nCommand line: %s\nDate: ", command);
  /*
    if (logflag) {
    fLog = FOPEN(logFile, "a");
    sprintf(logbuf,"\nCommand line: %s\n", command);
    printLog(logbuf);
    FCLOSE(fLog);
    }
  */
  ret = system(command);
  
  return ret;
}

int export_ceos(char *configFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "export_ceos %s", configFile);
  ret = system(command);
  
  return ret;
}

#endif
