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

#include "asf.h"
#include "asf_meta.h"
#include "cproj.h"
#include "proj.h"

#define VERSION 1.5
#define gridResX 30
#define gridResY 30


int getNextSarPt(meta_parameters *meta,int gridNo,int *x,int *y);

static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [-log <file>] <DEM> <SAR> <out_grid>\n",name);
  printf("\n"
	 "ARGUMENTS:\n"
	 "   <DEM>       A DEM to create a grid upon.\n"
	 "   <SAR>       A SAR file for which to create the grid\n"
	 "   <out_grid>  A mapping grid, for use with fit_plane\n"
	 "   -log <file> Allows the output to be written to a log file (optional)\n");
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
  int iflg=0;
  int gridCount,sar_x,sar_y,line_count,sample_count;
  char *demName,*sarName,*ceos,*outName;
  FILE *out;
  meta_parameters *metaSar, *metaDem;
  double elev = 0.0;
  
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
    else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}
  demName = argv[currArg];
  sarName = argv[currArg+1];
  outName = argv[currArg+2];
  
  system("date");
  printf("Program: create_dem_grid\n\n");
  
  out=FOPEN(outName,"w");
  metaSar = meta_read(sarName);
  metaDem = meta_read(demName);  

  metaSar->general->sample_count += 400;

  /* Convert all angles in projection part of metadata into radians -
     latlon_to_proj needs that lateron */
  switch (metaDem->projection->type) 
    {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      metaDem->projection->param.utm.lat0 *= D2R;
      metaDem->projection->param.utm.lon0 *= D2R;
      break;
    case POLAR_STEREOGRAPHIC:
      metaDem->projection->param.ps.slat *= D2R;
      metaDem->projection->param.ps.slon *= D2R;
      break;
    case ALBERS_EQUAL_AREA:
      metaDem->projection->param.albers.std_parallel1 *= D2R;
      metaDem->projection->param.albers.std_parallel2 *= D2R;
      metaDem->projection->param.albers.center_meridian *= D2R;
      metaDem->projection->param.albers.orig_latitude *= D2R;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      metaDem->projection->param.lamcc.plat1 *= D2R;
      metaDem->projection->param.lamcc.plat2 *= D2R;
      metaDem->projection->param.lamcc.lat0 *= D2R;
      metaDem->projection->param.lamcc.lon0 *= D2R;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      metaDem->projection->param.lamaz.center_lon *= D2R;
      metaDem->projection->param.lamaz.center_lat *= D2R;
      break;
    }
    
  /*Create a grid on the SAR image, and for each grid point:*/
  for (gridCount=0;getNextSarPt(metaSar,gridCount,&sar_x,&sar_y);gridCount++)
    {
      double dem_x,dem_y; /*This is what we're seeking-- 
			    the location on the DEM corresponding to the SAR point.*/
      double lat,lon; /*This is how we go between SAR and DEM images.*/
      double demProj_x,demProj_y; /*These are the projection coordinates for the DEM.*/
      int orig_x,orig_y;
      
      /*Compute the latitude and longitude of this location on the ground.*/
      meta_get_original_line_sample(metaSar, sar_y, sar_x, &orig_y, &orig_x);
      meta_get_latLon(metaSar,(float)orig_y,(float)orig_x,elev,&lat,&lon);
      
      /*Compute the projection coordinates of this location in the DEM.*/
      latlon_to_proj(metaDem->projection, metaSar->sar->look_direction, 
		     lat*D2R, lon*D2R, &demProj_x, &demProj_y);
      
      /*Compute the line,sample coordinates of this location in the DEM.*/
      dem_x = (demProj_x - metaDem->projection->startX) / metaDem->projection->perX;
      dem_y = (demProj_y - metaDem->projection->startY) / metaDem->projection->perY;

      /*Now output the points! 
      printf("Pt %4i :Sar( %i %i );Orig ( %i %i );Deg( %.2f %.2f );\n",
      gridCount,sar_x,sar_y,orig_x,orig_y,lat,lon);
      printf("\tproj( %.2f %.2f );dem( %.2f %.2f )\n",
      demProj_x,demProj_y,dem_x,dem_y);
      printf("\tstart( %.2f %.2f )\n", 
      metaDem->projection->startX, metaDem->projection->startY);*/
      fprintf(out,"%6d %6d %8.5f %8.5f %4.2f\n",sar_x,sar_y,dem_x,dem_y,1.0);
    }
  printf("   Created a grid of %ix%i points\n\n",gridResX,gridResY);
  if (logflag) {
    sprintf(logbuf,"   Created a grid of %ix%i points\n\n",gridResX,gridResY);
    printLog(logbuf);
    FCLOSE(fLog);
  }
  return (0);
}

/*Return a regular, 30x30 grid of points.*/
int getNextSarPt(meta_parameters *meta,int gridNo,int *x,int *y)
{
  int xtmp, ytmp;
  
  if (gridNo>=gridResX*gridResY)
    return 0;
  
  xtmp = gridNo % gridResX;
  ytmp = gridNo / gridResX;
  
  *x = 1 + (float) xtmp / (float) (gridResX-1) * (meta->general->sample_count);
  *y = 1 + (float) ytmp / (float) (gridResY-1) * (meta->general->line_count);
  
  return 1;
}
