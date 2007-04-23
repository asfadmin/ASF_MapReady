/******************************************************************************
NAME:  deskew -- remove squint induced skew in imagery

SYNOPSIS:  deskew <infile> <outfile>

DESCRIPTION:

	Deskew uses the squint angle of an image along with the
        look angle to determine the amount of parallelogram shift
        skew that has been introduced in an image due to the doppler
        centroid choosen during image processing.  It then remaps
        the image (using bi-linear interpolation) to remove this
        skew.
 

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0		   Tom Logan

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/

/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/

#include "asf.h"
#include "asf_sar.h"

#define VERSION 1.5

int main(int argc,char *argv[])
{
  char infile[255], outfile[255];
  
  if (argc != 3)
    { 
      printf("\nUsage: %s <infile> <outfile>\n",argv[0]);
      printf("       <infile> 	input file name (.img,.meta)\n");
      printf("       <outfile>  output file name (.img,.meta)\n");
      printf("\n"
	     "Removes doppler skew from image.\n");
      printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
      exit(1);
    }

  create_name(infile, argv[1], ".img");
  create_name(outfile, argv[2], ".img");

  deskew(infile, outfile);
  return 0;
}
