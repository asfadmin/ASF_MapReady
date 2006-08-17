#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"

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

int call_asf_convert(char *configFile)
{
  char command[256];
  int ret;

  sprintf(command, "asf_convert %s\n", configFile);
  ret = asfSystem(command);

  return ret;
}

#endif
