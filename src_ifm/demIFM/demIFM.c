/****************************************************************
NAME:  demIFM

SYNOPSIS:  

demIFM [-offset] [-log <file>] <SAR.img> <metadata> <baseline file> <DEM.ext>

DESCRIPTION:

  This program co-registers a DEM to a SAR image.
  It then generates a simulated phase image from the DEM (out_dem_phase),
  and well-distributed seed points (out_dem_seeds)
  This is useful when running tandem_ifm(1).

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    fit_plane
    remap
    makeddr
    reskew_dem
    amp2img
    trim
    fftMatch
    
FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
  VERS:  DATE:	  AUTHOR:	PURPOSE:
  ---------------------------------------------------------------
  1.0	 9/97	O. Lawlor	Initial Creation
  1.1	 5/98	O. Lawlor	Added more reliable FFT Matching
  1.2	 6/98	O. Lawlor	Removed all matching, since geolocations
				 are so accurate.
  2.0	 7/01	R. Gens		Conversion from script to program
  2.5	10/01	P. Denny	Rewrote command line parsing
  2.52	 3/02   P. Denny        Updated fftMatch & trim calls
  2.7    4/02   P. Denny        Updated commandline parsing & usage()

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   demIFM -- co-registers a DEM to a SAR image. 			    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "ddr.h"

#define VERSION 2.7

/* local function declaration */
void usage(char *name);
void execCmd(char *cmd);

int main(int argc, char **argv)
{
  int offFlag=0;
  int shiftX, shiftY;
  char cmd[255];
  char sarFile[255], ceosFile[255], baseFile[255], demFile[255];
  struct DDR ddrIn;
  FILE *fCorr;

  logflag=0;

  currArg=1;
/* parse command line */
  while (currArg < (argc-4)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1)
      strcpy(logFile, GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=1;
    }
    else if (strmatch(key,"-offset")) {
      offFlag=1;
    }
    else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}
  sprintf(sarFile, "%s", argv[currArg]);
  sprintf(ceosFile,"%s", argv[currArg+1]);
  sprintf(baseFile,"%s", argv[currArg+2]);
  sprintf(demFile, "%s", argv[currArg+3]);

  StartWatch();
  system("date");
  printf("Program: demIFM\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: demIFM\n\n");
    FCLOSE(fLog);
  }

  c_getddr(sarFile,&ddrIn);

/* Find the chunk of the DEM which corresponds to this SAR image */
  sprintf(cmd, "create_dem_grid %s %s %s dem_grid", demFile, sarFile, ceosFile);
  execCmd(cmd);
  printf("\n");
  if (logflag) {
    fLog = FOPEN(logFile, "a");
    printLog("\n");
    FCLOSE(fLog);
  }

  sprintf(cmd, "fit_plane dem_grid dem_plane k 1.0");
  if (logflag) sprintf(cmd, "fit_plane dem_grid dem_plane k 1.0 -log %s", logFile);
  execCmd(cmd);

/* Extract this chunk of the DEM */
  sprintf(cmd, "remap -matrix dem_plane -translate 0 0 -width %d -height %d -bilinear -float %s dem_big.dem",
		ddrIn.ns+400, ddrIn.nl, demFile);
  if (logflag) sprintf(cmd, "remap -matrix dem_plane -translate 0 0 -width %d -height %d -bilinear -float -log %s"
				" %s dem_big.dem", ddrIn.ns+400, ddrIn.nl, logFile, demFile);
  execCmd(cmd);

  sprintf(cmd, "makeddr dem_big.dem %d %d float", ddrIn.nl, ddrIn.ns+400); 
  if (logflag) sprintf(cmd, "makeddr -log %s dem_big.dem %d %d float", logFile, ddrIn.nl, ddrIn.ns+400);
  execCmd(cmd); 

/* Create the slant range height and simulated SAR image */

  sprintf(cmd, "reskew_dem dem_big.dem %s dem_slant.ht dem_sim.amp", ceosFile);
  if (logflag) sprintf(cmd, "reskew_dem dem_big.dem %s dem_slant.ht dem_sim.amp -log %s", ceosFile, logFile);
  execCmd(cmd);

/* Perform pixel offset matching */

  if (!offFlag) {
    sprintf(cmd, "mv dem_slant.ht dem_lined.ht");
    system(cmd);
    sprintf(cmd, "mv dem_slant.ddr dem_lined.ddr");
    system(cmd);
  }
  else
  {
    sprintf(cmd, "amp2img -look 1x1 -step 1x1 -quiet dem_sim.amp dem_simbyte.img");
    if (logflag) sprintf(cmd, "amp2img -look 1x1 -step 1x1 -log %s -quiet dem_sim.amp dem_simbyte.img", logFile);
    execCmd(cmd);

    sprintf(cmd, "trim -h %d -w %d dem_simbyte.img dem_trimsim.img 0 0", ddrIn.nl, ddrIn.ns);
    if (logflag) sprintf(cmd, "trim -log %s -h %d -w %d dem_simbyte.img dem_trimsim.img 0 0", logFile, ddrIn.nl, ddrIn.ns);
    execCmd(cmd);

    sprintf(cmd, "fftMatch -quiet %s dem_trimsim.img dem_corr", sarFile); 
    if (logflag) sprintf(cmd, "fftMatch -quiet -log %s %s dem_trimsim.img dem_corr", logFile, sarFile); 
    printf("Command line: %s\nProgram: fftMatch\n\n", cmd);
    if (logflag) {
      fLog = FOPEN(logFile, "a");
      sprintf(logbuf, "Command line: %s\n\n", cmd);
      printLog(logbuf);
      FCLOSE(fLog);
    }
    system(cmd);
    printf("\n\n");
    if (logflag) printLog("\n\n");

{
    float xshift, yshift;

    fCorr = FOPEN("dem_corr", "r");
    fscanf(fCorr, "%f %f", &xshift, &yshift);
    FCLOSE(fCorr);

    shiftX = (int) (xshift+0.5);
    shiftY = (int) (yshift+0.5);
}

    shiftX *= -1;
    shiftY *= -1;

    sprintf(cmd, "trim -h %d -w %d dem_slant.ht dem_lined.ht %i %i", ddrIn.nl, ddrIn.ns, shiftY, shiftX);
    if (logflag) sprintf(cmd, "trim -log %s -h %d -w %d dem_slant.ht dem_lined.ht %i %i", 
			logFile, ddrIn.nl, ddrIn.ns, shiftY, shiftX);
    execCmd(cmd);

    sprintf(cmd, "trim -h %d -w %d dem_simbyte.img dem_lined_amp.img %i %i", ddrIn.nl, ddrIn.ns, shiftY, shiftX);
    if (logflag) sprintf(cmd, "trim -log %s -h %d -w %d dem_simbyte.img dem_lined_amp.img %i %i", 
			logFile, ddrIn.nl, ddrIn.ns, shiftY, shiftX);
    execCmd(cmd);
  }

/* Convert the slant range height image into phase and seed point files */

  sprintf(cmd, "dem2phase dem_lined.ht %s %s out_dem_phase.phase", ceosFile, baseFile);
  if (logflag) sprintf(cmd, "dem2phase -log %s dem_lined.ht %s %s out_dem_phase.phase", logFile, ceosFile, baseFile);
  execCmd(cmd);

  sprintf(cmd, "dem2seeds dem_lined.ht %s out_dem_seeds", sarFile);
  if (logflag) sprintf(cmd, "dem2seeds -log %s dem_lined.ht %s out_dem_seeds", logFile, sarFile);
  execCmd(cmd);

/* Exit gracefully */

  StopWatch();
  if (logflag) {
    fLog = FOPEN(logFile, "a");
    StopWatchLog(fLog);
    FCLOSE(fLog);
  }
  return (0);
}

void execCmd(char *cmd)
{
  printf("Command line: %s\nDate: ", cmd);
  if (logflag) {
    fLog = FOPEN(logFile, "a");
    sprintf(logbuf, "Command line: %s\n", cmd);
    printLog(logbuf);
    FCLOSE(fLog);
  }
  system(cmd);
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-offset] [-log <file>]\n"
	"          <SAR.img> <metadata> <baseline file> <DEM.ext>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <SAR.img>    A simulated Sar image\n" 
	"   <metadata>   The .meta file for the first SAR image of the pair.\n"
	"   <baseline>   Interferometric baseline (or an estimate),\n"
	"                  generated by resolve.\n"
	"   <DEM.ext>    The input DEM image. It must be a valid LAS image,\n"
	"                  and should cover almost all of the SAR image.\n"
	"                  But the DEM need not be aligned with the SAR image\n"
	"                  ...that's what demIFM is for.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -offset      Enables co-registration of simulated image with\n"
	"                  real image\n"
	"   -log <file>  Sends important output to log file\n\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Co-registers a DEM to a SAR image.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}
