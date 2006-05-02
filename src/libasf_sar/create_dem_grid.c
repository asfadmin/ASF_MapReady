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

#include "asf.h"
#include "asf_meta.h"
#include "cproj.h"
#include "proj.h"
#include "libasf_proj.h"
#include "asf_reporting.h"
#include "asf_sar.h"

/*Return a regular, 30x30 grid of points.*/
static int getNextSarPt(meta_parameters *meta,int gridNo,int *x,int *y,
			double width, double height,
			int gridResX, int gridResY)
{
  int xtmp, ytmp;
  
  if (gridNo>=gridResX*gridResY)
    return 0;
  
  xtmp = gridNo % gridResX;
  ytmp = gridNo / gridResX;

  *x = 1 + (float) xtmp / (float) (gridResX-1) * (width+400);
  *y = 1 + (float) ytmp / (float) (gridResY-1) * (height);
  
  return 1;
}

int create_dem_grid(const char *demName, const char *sarName,
		    const char *outName)
{
  return create_dem_grid_ext(demName, sarName, outName, -1, -1, -1);
}

int create_dem_grid_ext(const char *demName, const char *sarName,
			const char *outName, int w, int h, int size)
{
  int gridCount,sar_x,sar_y;
  FILE *out;
  meta_parameters *metaSar, *metaDem;
  double elev = 0.0;
  double width = w, height = h;
  int gridResX, gridResY;

  gridResX = size;
  gridResY = size;

  out=FOPEN(outName,"w");
  metaSar = meta_read(sarName);
  metaDem = meta_read(demName);  

  if (width < 0) width = metaSar->general->sample_count;
  if (height < 0) height = metaSar->general->line_count;
  if (gridResX < 0) gridResX = 20;
  if (gridResY < 0) gridResY = 20;

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
    default:
      break; /* do nothing, though perhaps we should error out? */
    }
    
  /*Create a grid on the SAR image, and for each grid point:*/
  for (gridCount=0;
       getNextSarPt(metaSar,gridCount,&sar_x,&sar_y,
		    width,height,gridResX,gridResY);
       gridCount++)
    {
      double dem_x,dem_y; /*This is what we're seeking-- 
			    the location on the DEM corresponding to the SAR point.*/
      double lat,lon; /*This is how we go between SAR and DEM images.*/
      double demProj_x,demProj_y; /*These are the projection coordinates for the DEM.*/
      int orig_x,orig_y;
      int grid_x, grid_y;

      getNextSarPt(metaSar,gridCount,&grid_x,&grid_y,
		   metaSar->general->sample_count,
		   metaSar->general->line_count,
		   gridResX, gridResY);

      /*Compute the latitude and longitude of this location on the ground.*/
      meta_get_original_line_sample(metaSar, sar_y, sar_x, &orig_y, &orig_x);
      meta_get_latLon(metaSar,(float)orig_y,(float)orig_x,elev,&lat,&lon);
      
      /*Compute the projection coordinates of this location in the DEM.*/
      latlon_to_proj(metaDem->projection, metaSar->sar->look_direction, 
		     lat*D2R, lon*D2R, &demProj_x, &demProj_y);
      
      /*Compute the line,sample coordinates of this location in the DEM.*/
      dem_x = (demProj_x - metaDem->projection->startX) / metaDem->projection->perX;
      dem_y = (demProj_y - metaDem->projection->startY) / metaDem->projection->perY;
      fprintf(out,"%6d %6d %8.5f %8.5f %4.2f\n",grid_x,grid_y,dem_x,dem_y,1.0);
    }
  //asfPrintStatus("   Created a grid of %ix%i points\n\n",gridResX,gridResY);
  FCLOSE(out);
  return TRUE;
}

