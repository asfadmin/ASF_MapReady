/******************************************************************************
NAME:  creat_dem_grid

SYNOPSIS:

   create_dem_grid [-log <file>] <DEM> <SAR> <out_grid>

        -log <file> allows the output to be written to a log file
	<DEM>      A DEM to create a grid upon
        <SAR>      a SAR file for which to create the grid
        <out_grid> a mapping grid, for use with fit_plane-
        
DESCRIPTION:  
	
	Create a grid of points over the given LAS DEM
	that surrounds the given LAS SAR image/ceos.  This grid is
	used to project the DEM into the SAR image's (slant or ground
	range) coordinate space.

	During interferometry, it is necessary to clip out a piece of
        a map-registered DEM so the DEM is lined up with the SAR image.  This
        program is the first step in lining up the DEM.

        The program outputs a file containing pixel offsets for
        each input point.  This file can be read by the fit_plane(1) program,
        and that program's output used to move the image with remap(1).



EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:    DATE:   AUTHOR:
    -----------------------------------------------------------------
    1.0	     2/98    O. Lawlor	For demIFM. Initial development.
    1.01     7/01    R. Gens    Added logfile switch
    1.25     4/02    P. Denny   Standardized commandline parsing & usage()
    1.3      6/05    R. Gens    Implemented the changes that Joe and Orion
                                came up with.
    1.5      7/05    R. Gens    Removed DDR dependency.
    1.6      4/06    K.Hogenson Width & height & grid size arguments.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   create_dem_grid creates a grid which can be used to extract a portion   *
*		    of a DEM to fit a given SAR image. 			    *
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

#include "asf_sar.h"
#include "asf_meta.h"
#include "cproj.h"
#include "proj.h"

#define VERSION 1.7

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [-log <file>] [-w <grid_width>] [-h <grid_height>]\n"
         "      [-size <gridsize>] <DEM> <SAR> <out_grid>\n",name);
  printf("\n"
	 "ARGUMENTS:\n"
	 "   <DEM>        A DEM to create a grid upon.\n"
	 "   <SAR>        A SAR file for which to create the grid\n"
	 "   <out_grid>   A mapping grid, for use with fit_plane\n"
	 "   -log <file>  Allows the output to be written to a log file (optional)\n"
	 "   -w <width>   Desired width of the grid (optional)\n"
	 "   -h <height>  Desired height of the grid (optional)\n"
	 "   -size <size> Number of points on a grid side (optional)\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s creates a grid which can be used to extract a\n"
	 "   portion of a DEM to fit a given SAR image.\n",name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(1);
}

int main(int argc,char *argv[])
{
  char *demName,*sarName,*outName;
  double width = -1, height = -1;
  int gridResX = 20, gridResY = 20;

  logflag=0;
  
  /* parse command line */
  currArg=1; /*from cla.h in asf.h*/
  while (currArg < (argc-3)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile, GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      StartWatchLog(fLog);
      printLog("Program: create_dem_grid\n\n");
      logflag=1;
    }
    else if (strmatch(key,"-w")) {
      CHECK_ARG(1);
      width = atof(GET_ARG(1));
    }
    else if (strmatch(key,"-h")) {
      CHECK_ARG(1);
      height = atof(GET_ARG(1));
    }
    else if (strmatch(key,"-size")) {
      CHECK_ARG(1);
      gridResX = gridResY = atoi(GET_ARG(1));
    }
    else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}
  demName = argv[currArg];
  sarName = argv[currArg+1];
  outName = argv[currArg+2];

  system("date");
  printf("Program: create_dem_grid\n\n");

  create_dem_grid_ext(demName, sarName, outName, width, height, gridResX, 0.);
  return (0);
}
