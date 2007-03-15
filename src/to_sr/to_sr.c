#include "asf_sar.h"
#include "asf.h"

#include <stdio.h>
#include <stdlib.h>

#define VERSION 0.2

static void usage(char *progname)
{
    asfPrintStatus("\n");
    asfPrintStatus("Usage: %s [-p <pixsize>] <infile> <outfile>\n",progname);
    asfPrintStatus("   pixsize  Pixel size for output slant range image\n");
    asfPrintStatus("   infile   Input file base name.\n");
    asfPrintStatus("   outfile  Output slant range filebase name\n");
    asfPrintStatus("\n");
    asfPrintStatus(" If the pixel size is not specified, it is calculated\n");
    asfPrintStatus(" as follows: (speed of light)/(sample rate * 2*10^6)\n");
    asfPrintStatus("\n");
    exit(EXIT_FAILURE);
}

int main(int argc,char *argv[])
{
  float srPixSize= -1;   /* output pixel size             */

  char  infile[256];     /* Input file name               */
  char  outfile[256];    /* Output file name              */

  if ((argc != 5 && argc != 3) || (argc == 5 && strcmp(argv[1], "-p") != 0)) {
    usage(argv[0]);
  }

  if (strcmp(argv[1],"-p") == 0) {
    create_name(infile,argv[3],".img");
    create_name(outfile,argv[4],".img");
  } else {
    create_name(infile,argv[1],".img");
    create_name(outfile,argv[2],".img");
  }

  if (argc == 5) {
    srPixSize = atof(argv[2]);
    to_sr_pixsiz(infile, outfile, srPixSize);
  }
  else {
    to_sr(infile, outfile);
  }

  exit(EXIT_SUCCESS);
}

