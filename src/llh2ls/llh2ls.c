#include "asf.h"
#include "asf_meta.h"

#include <stdio.h>
#include <stdlib.h>

#define VERSION 0.2

static void usage(char *progname)
{
    asfPrintStatus("\n");
    asfPrintStatus("Usage: %s <ceos basename> <lat> <lon> [height]\n",progname);
    asfPrintStatus("   ceos basename  Name of a CEOS file\n");
    asfPrintStatus("   lat            Latitude (in degrees)\n");
    asfPrintStatus("   lon            Longitude (in degrees)\n");
    asfPrintStatus("   height         Height (in meters), optional with default 0\n");
    asfPrintStatus("\n");
    exit(EXIT_FAILURE);
}

int main(int argc,char *argv[])
{
  double lat, lon;  
  double height = 0;
  double line, samp;

  if (argc != 4 && argc != 5) {
    usage(argv[0]);
  }

  lat = atof(argv[2]);
  lon = atof(argv[3]);
  if (argc == 5) height = atof(argv[4]);

  meta_parameters *meta = meta_create(argv[1]);

  meta_get_lineSamp(meta, lat, lon, height, &line, &samp);

  printf("Line:   %f\n", line);
  printf("Sample: %f\n", samp);

  meta_free(meta);
  exit(EXIT_SUCCESS);
}

