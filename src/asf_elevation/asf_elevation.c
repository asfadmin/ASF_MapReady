/****************************************************************
NAME:  elev

SYNOPSIS:  

  elev [-log <file>] [-quiet] <phase> <base> <outfile> <seed_file>

DESCRIPTION:

       Elev calculates the elevation at each integrated pixel  in
       an unwrapped phase file.

       The  user also needs to specify a seed point file, identi-
       cal to the format used by tandem_ifm.

       The output file is a float file of the same dimensions  as
       the  input file.  Each value will be a height in meters or
       0 if the pixel is non-integrated.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:  DATE:   PROGRAMMER:   PURPOSE:
    ---------------------------------------------------------------
    0.1    10/96                 Original Creation
    0.2     4/97   T. Logan      Properly handles windowed images
    1.0     6/97   O. Lawlor     Gets image size from ddr
    1.2    10/97   O. Lawlor     Eliminated unwrapping mask parameter
    1.3    11/97   O. Lawlor     Changed CLA's to accept list of seed points
    2.0     6/98   O. Lawlor     Re-derived equation; updated for new ceos
    2.1     6/00   D. Koster     Modified to handle files larger than 2GB
    2.2     7/01   R. Gens       Added log file switch
    2.5     3/03   P. Denny      Obliterate use of DDR, and replace with
                                   updated meta structure. Standardized
                                   command line parsing
    2.6     7/05   R. Gens       Took care of endianess.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   elev  -  produce  an  elevation file from a deramped igram phase file.  *
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
#include "asf_meta.h"
#include "asf_endian.h"

#define VERSION 2.6

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] [-quiet] <unwrapped phase> <phase mask> <baseline>\n"
	"      <seeds> <slant amplitude> <slant coherence> <ground elevation>\n"
	"      <ground elevation error> <ground amplituide> <ground coherence>\n",
	name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"    phase:      Unwrapped phase file (.phase and .meta)\n"
	"    base:       File containing baseline params. used to unwrap\n"
	"                  format:  Bn_c   dBn   Bp_c   dBp \n"
	"    outfile:    Output file containing elevations.\n"
	"    seed_file:  Tandem_ifm style seed file.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log:    Option to have output written to a log <file>.\n"
	"   -quiet:  Option to have output surpressed to essential.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Generate a DEM from unwrapped phase\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  char *unwrapped_phase, *phase_mask, *baseline, *seeds, *slant_amplitude;
  char *slant_coherence, *ground_elevation, *ground_elevation_error;
  char *ground_amplitude, *ground_coherence;
  extern int currArg; /* pre-initialized to 1; like optind */
  
  logflag=quietflag=0;
  
  /* parse command line */
  while (currArg < (argc-10)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1)
	strcpy(logFile, GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=1;
    }
    else if (strmatch(key,"-quiet")) {
      quietflag=1;
    }
    else {
      printf( "\n**Invalid option:  %s\n",argv[currArg-1]); 
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < 10) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  unwrapped_phase = argv[currArg++];
  phase_mask = argv[currArg++];
  baseline = argv[currArg++];
  seeds  = argv[currArg++];
  slant_amplitude  = argv[currArg++];
  slant_coherence  = argv[currArg++];
  ground_elevation  = argv[currArg++];
  ground_elevation_error  = argv[currArg++];
  ground_amplitude  = argv[currArg++];
  ground_coherence = argv[currArg];
  
  asfSplashScreen (argc, argv);
  
  if (!logflag)
    sprintf(logFile, "tmp%d.log", (int)getpid());

  // Call library function that gets the work done
  asf_elevation(logFile, unwrapped_phase, phase_mask, baseline, seeds,
		slant_amplitude, slant_coherence, ground_elevation,
		ground_elevation_error, ground_amplitude, ground_coherence);

  return(0);
}

