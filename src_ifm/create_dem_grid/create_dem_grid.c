/******************************************************************************
NAME:  creat_dem_grid

SYNOPSIS:

   create_dem_grid [-log <file>] <las DEM> <las SAR> <SAR Ceos> <out_grid>

        -log <file> allows the output to be written to a log file
	<las DEM> A LAS 6.0 DEM to create a grid upon
        <las SAR> a LAS 6.0 SAR file for which to create the grid
        <SAR Ceos> the ASF metadata file for the SAR file
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

#define VERSION 1.25
#define gridResX 2
#define gridResY 20


int getNextSarPt(struct DDR *ddr,int gridNo,int *x,int *y);

int main(int argc,char *argv[])
{
	int iflg=0;
	int gridCount,sar_x,sar_y;
	char *demName,*sarName,*ceos,*outName;
	FILE *out;
	meta_parameters *meta;
	struct DDR sar_ddr,dem_ddr;
	forward_transform latLon2proj[100];

	logflag=0;

	/* parse command line */
	currArg=1; /*from cla.h in asf.h*/
	while (currArg < (argc-4)) {
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
	if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	demName = argv[currArg];
	sarName = argv[currArg+1];
	ceos    = argv[currArg+2];
	outName = argv[currArg+3];

	system("date");
	printf("Program: create_dem_grid\n\n");

	out=FOPEN(outName,"w");
	
	c_getddr(sarName,&sar_ddr);
	meta=meta_init(ceos);
	
	c_getddr(demName,&dem_ddr);
	for_init(dem_ddr.proj_code,dem_ddr.zone_code,dem_ddr.proj_coef,dem_ddr.datum_code,
		NULL,NULL,&iflg,latLon2proj);
	
	if (iflg!=0)
	{
		sprintf(errbuf,"   ERROR: Problem in map projection initialization.  Exiting.\n");
		printErr(errbuf);
	}
	
	/*Create a grid on the SAR image, and for each grid point:*/
	for (gridCount=0;getNextSarPt(&sar_ddr,gridCount,&sar_x,&sar_y);gridCount++)
	{
		double dem_x,dem_y; /*This is what we're seeking-- the location on the DEM corresponding 
					to the SAR point.*/
		double lat,lon; /*This is how we go between SAR and DEM images.*/
		double demProj_x,demProj_y; /*These are the projection coordinates for the DEM.*/
		int orig_x,orig_y;
		
	/*Compute the latitude and longitude of this location on the ground.*/
		meta_get_orig((void *)&sar_ddr,sar_y,sar_x,&orig_y,&orig_x);
		meta_get_latLon(meta,(float)orig_y,(float)orig_x,0.0,&lat,&lon);
		
	/*Compute the projection coordinates of this location in the DEM.*/
		latLon2proj[dem_ddr.proj_code](lon*D2R,lat*D2R,&demProj_x,&demProj_y);
		
	/*Compute the line,sample coordinates of this location in the DEM.*/
		dem_x=(demProj_x-dem_ddr.upleft[1])/(dem_ddr.upright[1]-dem_ddr.upleft[1])*dem_ddr.ns;
		dem_y=(demProj_y-dem_ddr.upleft[0])/(dem_ddr.loleft [0]-dem_ddr.upleft[0])*dem_ddr.nl;
		
	/*Now output the points!

		printf("Pt %4i :Sar( %i %i );Deg( %.2f %.2f );\n",
			gridCount,sar_x,sar_y,lat,lon);
		printf("\tproj( %.2f %.2f );dem( %.2f %.2f )\n",
			demProj_x,demProj_y,dem_x,dem_y);*/
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

/*Return a regular, 10x10 grid of points.*/
int getNextSarPt(struct DDR *ddr,int gridNo,int *x,int *y)
{
	int xtmp, ytmp;
 
	if (gridNo>=gridResX*gridResY)
		return 0;

	xtmp = gridNo % gridResX;
	ytmp = gridNo / gridResX;

	*x = 1 + (float) xtmp / (float) (gridResX-1) * (ddr->ns-1);
	*y = 1 + (float) ytmp / (float) (gridResY-1) * (ddr->nl-1);

	return 1;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] <las DEM> <las SAR> <SAR Ceos> <out_grid>\n",name);
 printf("\n"
	"ARGUMENTS:\n"
	"   <las DEM>   A LAS 6.0 DEM to create a grid upon.\n"
	"   <las SAR>   A LAS 6.0 SAR file for which to create the grid\n"
	"   <SAR Ceos>  The ASF metadata file for the SAR file\n"
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
