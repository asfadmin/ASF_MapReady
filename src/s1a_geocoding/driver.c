/**************************************************************************
 
NAME: make_s1a_mapping

SYNOPSIS:  make_s1a_mapping <inxml file>

DESCRIPTION:  Creates 100 coefficients to map S1A data as follows:
	
  ll2samp[25] - map lat,lon to sample
  ll2line[25] - map lat,lon to line
  ls2lat[25]  - map line,sample to lat
  ls2lon[25]  - map line,sample to lon

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    s1a*.xml		Annotation file with GCP in it.

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0     9/15     T. Logan     Create geocoding mapping for s1a
    1.0     9/7      K. Hogenson  Made first set of mappings work

    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:   Can't figure out why the minimization is not working for samples.
	It seem to be working just fine for lines, lats, and lons.

**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <asf_meta.h>
#include <asf_license.h>

int main(int argc, char *argv[])
{
  double ll2samp[25], ll2line[25], ls2lat[25], ls2lon[25];
  int i;

  if (argc != 2) {
    printf("Usage: %s <inxmlfile>\n",argv[0]);
    exit(1);
  }

  make_s1_mapping(argv[1],ll2samp, ll2line, ls2lat, ls2lon);

  printf("LL2SAMP: ");
  for (i=0; i<25; i++) printf("%15.8f",ll2samp[i]);
  printf("\n");

  printf("LL2LINE: ");
  for (i=0; i<25; i++) printf("%15.8f",ll2line[i]);
  printf("\n");

  printf("LS2LAT: ");
  for (i=0; i<25; i++) printf("%g ",ls2lat[i]);
  printf("\n");

  printf("LS2LON: ");
  for (i=0; i<25; i++) printf("%g ",ls2lon[i]);
  printf("\n");




  exit(0);
}


