/*****************************************************************************
NAME: fftMatchGrid

SYNOPSIS: fftMatchGrid [-log <file>] [-quiet] <img1.ext> <img2.ext> <grid.txt>

DESCRIPTION:

	fftMatch lines up two images, to slightly better the
	single-pixel precision.  It will work with images of any
	size, but is most efficient when the image dimensions are
	near a power of 2.  The images need not be square.

	The lining-up is performed using image correlation via
	the much-vaunted Fast Fourier Transform (FFT).  The working
	space size is rounded up to the nearest power of 2 (for the FFT).
	The first image is read in completely, and a chip of the second
	image is also read in.  The average brightness of the chip is
	calculated and subtracted from both images.  The images are then
	FFT'd to frequency space, the FFT'd chip is conjugated, and the
	two images are multiplied together.  The product is inverse-FFT'd,
	and the resulting correlation image shows a brightness peak
	where the two images line up best.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
	asf_fft.a: FFT Library

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/98   O. Lawlor    Needed better co-registration tool for
				 interferometry.
    1.5     2/02   P. Denny     Redo commandline parsing
    2.0    10/05   R. Gens      Converted tool into library function in
                                libasf_raster.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
	The images must have at least 75% overlap.
        fftMatch will occasionally fail to find the
        correct offset (especially with noisy images).

*****************************************************************************/
/****************************************************************************
*								            *
*   fftMatch attempts to estimate the offset between two		    *
*	     images using the Fast Fourier Transform.			    *
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
#include <math.h>
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "asf_raster.h"

#define VERSION 1.5

/**** PROTOTYPES ****/
void usage(char *name);


int main(int argc,char **argv)
{
  char *descFile=NULL,*inFile1=NULL,*inFile2=NULL;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */
  extern FILE *fLog;
  extern int logflag,quietflag;
  float bestLocX, bestLocY, certainty;

  fLog=NULL;
  logflag=quietflag=0;

  /* process command line */
  /* q has a : because we want to have it be -quiet (: = uiet) */
  while ((c=getopt(argc,argv,"m:c:l:q:")) != EOF)
    {
      switch (c) {
      case 'm':
	descFile=optarg;
	break;
      case 'l':/* -log <filename>, get logfile; this is sorta hacked */
	if (0==strncmp(optarg,"og",2))
	  {
	    sscanf(argv[optind++], "%s", logFile);
	    logflag=1;
	    fLog = FOPEN(logFile, "a");
	  }
	else usage(argv[0]);
	break;
      case 'q':/* -quiet flag; this is also hacked */
	if (0==strncmp(optarg,"uiet",4))
	  quietflag=1;
	else usage(argv[0]);
	break;
      default:
	if (fLog) FCLOSE(fLog);
	usage(argv[0]);
	break;
      }
    }

  if ((argc-optind) != 2) {
    if ((argc-optind) > 2) printf("\nToo many inputs.\n");
    if ((argc-optind) < 2) printf("\nToo few inputs.\n");
    if (fLog) FCLOSE(fLog);
    usage(argv[0]);
  }
  else {
    inFile1=argv[optind];
    inFile2=argv[optind+1];
  }

  fftMatch_gridded(inFile1, inFile2, descFile, &bestLocX, &bestLocY, &certainty);

  return (0);
}


void usage(char *name)
{
  printf("\nUSAGE:\n"
	 "   %s [-m <matchFile> [-log <file>] [-quiet]\n"
	 "            <master basename> <slave basename>\n"
	 "\nOPTIONS:\n"
	 "   -m <matchFile>:   an output file, listing how much image 2\n"
	 "                       should be shifted to line up with image 1:\n"
	 "                       <shift x (pixels)>   <shift y (pixels)>\n"
	 "                       <confidence (percent)>\n"
	 "   -c <corrImg.ext>: an image of the correlation between the two\n"
	 "                       source images.  This is useful for debugging.\n"
	 "   -log <file>:      allows the output to be written to a log file\n"
	 "   -quiet:           suppresses the output to the essentials\n"
	 "\nINPUTS:\n"
	 "   <master basename>: basename of any image in ASF Internal format (.img, .meta)\n"
	 "   <slave basename>:  an image to line up with the first.\n"
	 "                      The slave image should not be bigger than the master image\n"
	 "\nDESCRIPTION:\n"
	 "   fftMatch lines up the 'slave' image to the 'master' image to better\n"
	 "   than single-pixel precision.  It will work with images of any\n"
	 "   size, but is most efficient when the image dimensions are\n"
	 "   near a power of 2.  The images need not be square.  If the two images\n"
     "   have more than one band, only the first band from each image aligned.\n"
	 "\nVersion %.2f, ASF MAPREADY TOOLKIT\n\n",name,VERSION);
  exit(1);
}
