/****************************************************************
NAME:  reskew_dem

SYNOPSIS:  reskew_dem [-log <file>] <inGR_DEMfile> <outSR_DEM> <outSR_sim_amp>

DESCRIPTION:
	Reskew_dem maps an input, ground range DEM into slant range, and
	creates a simulated SAR image.  The input DEM must already be lined up
	with the image, but need not be precisely co-registered. In fact, the
	amplitude image is generated only so the images can be co-registered.

	This program is called by the dem2seeds script.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0      8/97        O. Lawlor   Reskew USGS DEMs for Interferometry/
    1.1      6/97        O. Lawlor   Made more consistent with deskew_dem.
    1.3     12/98        O. Lawlor   Allow ground and slant ranges to differ in
                                      length.
    1.31     7/01        R. Gens     Added logfile switch
    1.5     12/03        P. Denny    Update commandline parsing. Use meta 1.1
                                      instead of DDRs. This program is loaded
                                      with unnecessary globals. Yuk! Needs to
                                      be fixed sometime.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   reskew_dem -- this program remaps the input DEM to slant range,         *
*		  and creates a simulated slant-range amplitude image 	    *
*		  from it.						    *
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
#include "deskew.h"

#define VERSION 1.6
#define NUM_ARGS 4

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <inMeta> <inGR_DEM> <outSR_DEM> <outSR_simAmp>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   inMeta        Metadata slant range SAR image.\n"
	"   inGR_DEM      A lined-up ground-range DEM.\n"
	"   outSR_DEM     Output slant-range DEM.\n"
	"   outSR_simAmp  Output simulated amplitude image.\n");
 printf("\n"
	"OPTIONAL ARGUMENT:\n"
	"   -log   Allows output to be written to a log <file>.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program remaps the input DEM to slant range, and creates a simulated\n"
	"   slant-range amplitude image from it. This is useful for interferometry.\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}


int main(int argc, char *argv[])
{
	char inDEMfile[255],outDEMfile[255],outAmpFile[255],inMetafile[255];

	system("date");
	printf("Program: reskew_dem\n\n");
	
/* parse commandline arguments */
	logflag=FALSE;
/* FIXME: WTF? */
currArg=1;
	while (currArg < (argc-NUM_ARGS)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag = TRUE;
		}
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < NUM_ARGS) {
		printf("Insufficient arguments.\n");
		usage(argv[0]);
	}
	strcpy(inMetafile, argv[currArg]);
	strcpy(inDEMfile, argv[currArg+1]);
	strcpy(outDEMfile,argv[currArg+2]);
	strcpy(outAmpFile,argv[currArg+3]);

	reskew_dem(inMetafile, inDEMfile, outDEMfile, outAmpFile, NULL);
	exit(EXIT_SUCCESS);
}
