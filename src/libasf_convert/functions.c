#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include "asf_reporting.h"
/*
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
  ret = asfSystem(command);

  return ret;
}
*/

/*
int ardop(char *options, char *inFile, char *outFile)
{
  char command[1024];
  int ret;

  sprintf(command, "ardop %s %s %s", options, inFile, outFile);
  ret = asfSystem(command);

  return ret;
}
*/

int image_stats(char *inFile, char *outFile, char *values, int bins, 
		     double interval)
{
  char options[255]="", command[1024];
  int ret;

  if (bins != -99)
    sprintf(options, "%i", bins);
  sprintf(command, "image_stats %s %s %s %s", values, options, inFile, outFile);
  ret = asfSystem(command);

  return ret;
}

int detect_cr(char *inFile, char *crFile, char *outFile, int chips, int text)
{
  char options[255]="", command[1024];
  int ret;

  if (chips)
    sprintf(options, "-chips ");
  if (text)
    sprintf(options, "-text ");
  if (chips && text)
    sprintf(options, "-chips -text ");
  sprintf(command, "detect_cr %s%s.img %s %s", options, inFile, crFile, outFile);
  ret = asfSystem(command);

  return ret;
}
/*
int asf_terrcorr(char *options, char *inFile, char *demFile, char *outFile) 
{
  char command[1024];
  int ret;

  sprintf(command, "asf_terrcorr %s %s %s %s", options, inFile, demFile, outFile);
  ret = asfSystem(command);

  return ret;
}
*/
/*
int asf_geocode(char *options, char *inFile, char *outFile)
{
  char command[1024];
  int ret;

  sprintf(command, "asf_geocode %s -log %s %s %s", options, logFile, inFile, outFile);
  //asfPrintStatus("\nCommand line: %s\nDate: ", command);
  ret = asfSystem(command);

  return ret;
}
*/
/*
int asf_export(char *options, char *inFile, char *outFile)
{
  char command[1024];
  int ret;

  sprintf(command, "asf_export %s %s %s", options, inFile, outFile);
  asfPrintStatus("\nCommand line: %s\nDate: ", command);
  ret = asfSystem(command);

  return ret;
}
*/

int call_asf_convert(char *configFile)
{
  char command[256];
  int ret;

  sprintf(command, "asf_convert %s\n", configFile);
  ret = asfSystem(command);

  return ret;
}

#endif
