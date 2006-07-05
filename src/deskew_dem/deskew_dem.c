/****************************************************************
NAME:  deskew_dem

USAGE:  deskew_dem [-i inSARfile bit] [-log <file>]]
			<inDEMfile> <outfile>

SYNOPSIS:

    deskew_dem removes incidence angle skew from a slant-range
    DEM, and interpolates across areas that didn't phase unwrap.

    If <outfile> has the extension .dem or .ht, deskew_dem will
    remove the incidence angle skew from the input slant-range DEM.

    If the <outfile> has the extention of .img, or .amp, deskew_dem
    will output a terrain-corrected amplitude image, based on the input file
    <inDEMfile>.

    If the -g option is passed, the terrain correction is only
    geometric-- no radiometric incidence angle normalization will
    occur.

    The -log switch allows the output to be written to a log file.

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:     PURPOSE:
    ---------------------------------------------------------------
    1.0	     7/97  O. Lawlor   Deskew Interferometry DEMs.
    1.1	     6/98  O. Lawlor   Made more consistent with reskew_dem.
    1.2	     7/01  R. Gens     Added log file switch.
    1.35     4/02  P. Denny    Updated commandline parsing & usage()
    2.0      2/04  P. Denny    Removed use of DDR; upgraded to meta v1.1
                                Removed <ceos> command line argument
                                Fix sr2gr & gr2sr functions from leaving memory
                                Use newer io functions (eg: get_float_line)

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   deskew_dem -- this program removes incidence-angle skew and maps from   *
*		  slant range to ground range.				    *
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

#include "deskew_dem.h"

void usage(char *name);

#define VERSION 2.1
#define REQ_ARGS 2

int main(int argc, char *argv[])
{
	char inDemName[255],outName[255];
	char *inSarName;
	int doRadiometric=0;
	extern int currArg; /* in cla.h in asf.h in deskew_dem.h */

	logflag=FALSE;
	inSarName = NULL;
        int do_interp = TRUE;

/* parse commandline arguments */
	while (currArg < (argc-REQ_ARGS)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-i")) {
		        char *ext;
		        char tmp[1024];
			CHECK_ARG(2);
			strcpy(tmp,GET_ARG(2));
			inSarName = strdup(tmp);
			ext=findExt(inSarName);
			if ((0!=strcmp(ext,".amp")) && (0!=strcmp(ext,".img")))
			  {printf("**ERROR: <inSARfile> must have a \".amp\" or \".img\" extention.\n");usage(argv[0]);}
			doRadiometric = atoi(GET_ARG(1));
			if ((doRadiometric != 0) && (doRadiometric != 1))
			  {printf("**ERROR:  <bit> must be either 0 or 1\n"); usage(argv[0]);}
		}
		else if (strmatch(key,"-log")) {
			CHECK_ARG(1);
			strcpy(logFile,GET_ARG(1));
			fLog = FOPEN(logFile, "a");
			logflag = TRUE;
		}
                else if (strmatch(key,"-no-interp")) {
                        do_interp = FALSE;
                }
		else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < REQ_ARGS) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	strcpy(inDemName,argv[currArg]);
	strcpy(outName,  argv[currArg+1]);

	printf("Program: deskew_dem\n\n");
	if (logflag) {
	   printLog("Program: deskew_dem\n\n");
	}

	deskew_dem(inDemName, outName, inSarName, doRadiometric, NULL, NULL,
                   do_interp);
	exit(EXIT_SUCCESS);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-i <inSARfile> <bit>] [-no-interp] [-log <file>]\n"
	"              <inDEMfile> <outfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <inDEMfile>  IEEE float slant-range dem, with extension .ht or .dem.\n"
	"   <outfile>    Output image, with an extention.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -i	  The <inSARfile> is the image to be rectified (include the extention). If <bit>\n"
	"	     is 1 both radiometric and geometric rectification are performed; if <bit> is\n"
	"	     0 then only geometric rectification is performed.\n"
	"   -log  Allows the output to be written to a log <file>.\n"
        "   -no-interp  Layover regions are interpolated by default, this\n"
        "            will turn off the interpolation, leaving 0-filled holes.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program removes incidence-angle skew and maps from slant range to\n"
	"   ground range. It will do this to a DEM or SAR image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}
