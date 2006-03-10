#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include "functions.h"
#include "asf_reporting.h"

int asf_import(char *inFile, char *outFile, char *format, char *radiometry,
	       char *prcOrbits, double lat_begin, double lat_end)
{
  char options[255]="", command[1024];
  int ret;

  sprintf(options, "-log %s -quiet", logFile);
  if (prcOrbits) sprintf (options, "%s -prc %s", options, prcOrbits); 
  if (lat_begin!=-99.0 && lat_end!=99.0) 
    sprintf(options, "%s -lat %lf %lf", options, lat_begin, lat_end);
  sprintf(options, "%s %s", options, radiometry);
  sprintf(command, "asf_import -format %s %s %s %s", 
	  format, options, inFile, outFile);
  asfPrintStatus("\nCommand line: %s\nDate: ", command);
  ret = system(command);

  return ret;
}

int asf_geocode(char *options, char *inFile, char *outFile)
{
  char command[1024];
  int ret;

  sprintf(command, "asf_geocode %s -log %s %s %s", options, logFile, inFile, outFile);
  //asfPrintStatus("\nCommand line: %s\nDate: ", command);
  ret = system(command);

  return ret;
}

int asf_export(char *options, char *inFile, char *outFile)
{
  char command[1024];
  int ret;

  sprintf(command, "asf_export %s %s %s", options, inFile, outFile);
  asfPrintStatus("\nCommand line: %s\nDate: ", command);
  ret = system(command);

  return ret;
}

int asf_convert(char *configFile)
{
  char command[256];
  int ret;

  sprintf(command, "asf_convert %s\n", configFile);
  ret = system(command);

  return ret;
}

#endif
