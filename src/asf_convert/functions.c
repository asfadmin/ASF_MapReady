#ifndef FUNCTIONS_C
#define FUNCTIONS_C

#include "asf.h"
#include "functions.h"
#include "asf_reporting.h"

int asf_import(char *inFile, char *metaFile, char *type, char *outFile)
{
  char command[256];
  sprintf(command, "asf_import -log %s %s %s %s %s",
          logFile, type, inFile, metaFile, outFile);
  asfPrintStatus("\nCommand line:\n %s\n", command);
  return system(command);
}

int projprm(char *projection, char *projkey, char *outFile, char *options)
{
  char command[256];
  sprintf(command, "projprm -log %s %s %s %s %s",
          logFile, projection, projkey, outFile, options);
  asfPrintStatus("\nCommand line:\n %s\n", command);
  return system(command);
}

int geocode(char *inFile, char *projFile, char *projkey, float pix_size,
            float height, char *outFile)
{
  char command[256];
  sprintf(command, "geocode -p %.2f -h %.3f -log %s %s %s %s %s",
          pix_size, height, logFile, inFile, projFile, projkey, outFile);
  asfPrintStatus("\nCommand line:\n %s\n", command);
  return system(command);
}

int corner_coords(char *inFile)
{
  char command[256];
  sprintf(command, "corner_coords %s", inFile);
  asfPrintStatus("\nCommand line:\n %s\n", command);
  return system(command);
}

int filter(char *options, char *inFile, char *outFile)
{
  char command[256];
  sprintf(command, "filter %s -log %s %s %s",options,logFile,inFile,outFile);
  asfPrintStatus("\nCommand line:\n %s\n", command);
  return system(command);
}

int asf_export(int format, char *inFile, char *outFile)
{
  char command[256],format_str[10]="";
  switch (format) {
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
  asfPrintStatus("\nCommand line:\n %s\n", command);
  return system(command);
}

int asf_convert(char *configFile)
{
  char command[256];
  sprintf(command, "asf_convert %s", configFile);
  return system(command);
}

#endif
