/*******************************************************************************
 NAME: projectGeo

 SYNOPSIS: int projectGeo(double pixSize, char *metaName, char *in_proj,
                         char *in_key, char *in_tie, char *out_tie,
                         char *out_meta, int id, char win_type, char *win)

 DESCRIPTION:  This program converts the input metadata, projection file and
      key, and input tie points into the given output tie points and .meta file,
      by map-projecting the given input points.  It computes the output bounds
      of the newly geocoded image, and writes them to out_meta. 

    pixSize     output pixel size
    metaName     Input SAR metadata file (.meta)
    in_proj     Input Projection definition file name [ from projprm ]
    in_key      Input Projection key [as passed to projprm ]
    in_tie      Input ASCII tie point file name, from geoLatLon.
                  Has format:
                      <lat> <lon> <in X> <in Y>
    out_tie     Output ASCII tie point file name, for fit_quadratic.
                  Has format:
                       <out X> <out Y> <in X> <in Y>
    out_meta    Output metadata file name (.meta)
    id		process ID


 Windowing Modes: -o, -l, or -i set the size of the output image.
       The default is just big enough to contain the input image.

   -o    Means N W S E are projection coordinates:
    N    Y projection coordinate of top edge
    W    X projection coordinate of left edge
    S    Y projection coordinate of bottom edge
    E    X projection coordinate of right edge

   -l    Means N S are latitude, and E W are longitude:
    N    Latitude of north edge of output
    W    Longitude of west edge of output
    S    Latitude of south edge of output
    E    Longitude of east edge of output

   -i    Means write a nl x ns output image with:
    lat  Latitude of center of output
    lon  Longitude of center of output


 FUNCTION HISTORY:
     VERS:   DATE:  AUTHOR:	PURPOSE:
     ---------------------------------------------------------------
     1.0    03/98   O. Lawlor   Original Development
     2.0    ?       R. Gens     Convert from program to function
     2.5    08/03   P. Denny    Obliterate DDR and use new metadata
                                  Add this neat about & program history
     2.6    03/04   R. Gens	Added process ID for robustness

*******************************************************************************/

#include "projectGeo.h"

double aLat,aLon;/*First point in grid (for zone calculations)*/

int projectGeo(double pixSize, char *metaName, char *in_proj, char *in_key,
               char *in_tie, char *out_tie, char *out_meta, int id, char win_type,
               char *win)
{
	window *out_window=NULL;      /*Projection window*/
	proj_prm proj;                /*Projection parameters*/
	gridPoint grid[MAX_GRIDPTS];  /*Grid of converted points*/
	int nGrid;                    /*Number of points in grid*/
	int winIsLatLon=FALSE;        /*Is user-specified window in lat/lon instead of projection coords?*/
	int pointOfInterest=FALSE;    /*Is user-specified window (lat lon nl ns)?*/
	int userWin_flag=FALSE;       /*Did the user specify an output window?*/
	meta_parameters *meta;        /*Metadata structure*/
	char tmp[255];

	if (win_type=='l') {     /*User specified window as lat/lon*/
		winIsLatLon=TRUE;
		userWin_flag=TRUE;
	}
	else if (win_type=='o') {/*User specified window as projection coords.*/
		winIsLatLon=FALSE;
		userWin_flag=TRUE;
	}
	else if (win_type=='i') {/*User specified point of interest.*/
		pointOfInterest=TRUE;
		userWin_flag=TRUE;
	}
	if (userWin_flag) {
		out_window = getUserWindow(pointOfInterest,win);
	}

    /*Allocate and fill the meta structure*/
	meta = meta_read(metaName);

    /*Determine output pixel size*/
	if (pixSize==0) /*User did not specify a pixel size*/
		pixSize = meta->general->x_pixel_size;
	printf("   Setting output pixel size to %.2f meters\n",pixSize);
	if (logflag) {
	  sprintf(logbuf, "   Setting output pixel size to %.2f meters\n",pixSize);
	  printLog(logbuf);
	}

     /*Determine projection parameters*/
	if (0==get_proj_prm(in_proj,in_key,&proj))
		bail("   ProjectGeo: Error loading projection file.\n");
	
     /*Read points in grid*/
	read_grid(in_tie,grid,&nGrid);
	
     /*Map-project grid points*/
	project_grid(grid,nGrid,&proj);
	
     /*Extract the output window*/
	if (out_window==NULL)/*The user didn't specify an output window*/
	{/*So find the output window by finding the bounds of the grid*/
		out_window=window_grid(grid,nGrid);
		out_window->minY-=pixSize;/*Expand the window down 1 pixel*/
		out_window->maxX+=pixSize;/*Expand the window right 1 pixel*/
	} else if (winIsLatLon)
	/*User specified a window, but it must be map-projected*/
		projectWindow(out_window,init_proj(&proj));
	else if (pointOfInterest)
	/*User specified a point of interest*/
		projectPoint(out_window,pixSize,init_proj(&proj));

	/*Expand the output window to be an even multiple of pixSize pixels*/
	window_expand(pixSize,out_window);
	/*Convert projection coordinates to output pixels*/
	convert_grid(pixSize,out_window,grid,nGrid);
	
	/*Write out grid points*/
	write_grid(out_tie,grid,nGrid);
	
	if (write_proj_meta(pixSize, &proj, out_window, meta)) {
		meta_write(meta,out_meta);
		sprintf(tmp, "tmp%d", id);
                meta_write(meta,tmp);
	}
        meta_free(meta);
	
	printf("\n");
	if (logflag) {
	  printLog("\n");
	}

	return 0;
}
	
