#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include <stdio.h>
#include "asf.h"
#include "functions.h"

int asf_import(char *inFile, char *metaFile, char *type, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "asf_import -log %s %s CEOS %s %s %s", 
	  logFile, type, inFile, metaFile, outFile);
  printf("\nCommand line: %s\n", command);
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
  
  printf("\nCommand line: %s\n", command);
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
  
  printf("\nCommand line: %s\n", command);
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
  printf("\nCommand line: %s\n", command);
  ret = system(command);
  
  return ret;
}


int filter(char *options, char *inFile, char *outFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "filter %s -log %s %s %s", options, logFile, inFile, outFile);
  printf("\nCommand line: %s\n", command);
  if (logflag) {
    fLog = FOPEN(logFile, "a");
    sprintf(logbuf,"\nCommand line: %s\n", command);
    printLog(logbuf);
    FCLOSE(fLog);
  }
  ret = system(command);
  
  return ret;
}

int asf_export(int format, char *inFile, char *outFile)
{
  char command[255],format_str[10]="";
  int ret;
  
  switch (format)
    {
    case CEOS: break;
    case ASF: break;
    case GEOTIFF: strcpy(format_str,"geotiff "); break;
    case JPEG: strcpy(format_str,"jpeg "); break;
    case ENVI: strcpy(format_str,"envi "); break;
    case ESRI: strcpy(format_str,"esri "); break;
    case PPM: strcpy(format_str,"ppm "); break;
    case PNG: break;
    case LAS: break;
    }
  sprintf(command, "asf_export -f %s-o %s %s", format_str, outFile, inFile);
  printf("\nCommand line: %s\n", command);
  if (logflag) {
    fLog = FOPEN(logFile, "a");
    sprintf(logbuf,"\nCommand line: %s\n", command);
    printLog(logbuf);
    FCLOSE(fLog);
  }
  ret = system(command);
  
  return ret;
}

int asf_convert(char *configFile)
{
  char command[255];
  int ret;
  
  sprintf(command, "asf_convert %s", configFile);
  ret = system(command);
  
  return ret;
}

#endif
