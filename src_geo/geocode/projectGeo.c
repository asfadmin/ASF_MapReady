#include "projectGeo.h"

double aLat,aLon;/*First point in grid (for zone calculations)*/

int projectGeo(double pixSize, char *in_meta, char *in_proj, char *in_key, char *in_tie, \
		char *out_tie, char *out_meta, char *out_ddr, char win_type, char *win)
{
	window *out_window=NULL;/*Projection window*/
	proj_prm proj;/*Projection parameters*/
	gridPoint grid[MAX_GRIDPTS];/*Grid of converted points*/
	int nGrid;/*Number of points in grid*/

	int winIsLatLon=0;/*Is user-specified window in lat/lon instead of projection coords?*/
	int pointOfInterest=0;/*Is user-specified window (lat lon nl ns)?*/

	if (win_type=='l')
		winIsLatLon=1;/*User specified window as lat/lon*/
	else if (win_type=='o')
		winIsLatLon=0;/*User specified window as projection coords.*/
	else if (win_type=='i')
		pointOfInterest=1;/*User specified point of interest.*/
	if (strcmp(win,"")!=0) out_window=getUserWindow(pointOfInterest,win);
	
	/*Determine output pixel size*/
	if (pixSize==0)/*User did not specify a pixel size*/
		pixSize=ex_pix_size(in_meta);
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
	
	window_expand(pixSize,out_window);/*Expand the output window to be
		an even multiple of pixSize pixels*/
	/*Convert projection coordinates to output pixels*/
	convert_grid(pixSize,out_window,grid,nGrid);
	
	/*Write out grid points*/
	write_grid(out_tie,grid,nGrid);
	
	if (extExists(in_meta,".meta"))
	{/*Read & update .meta file*/
		meta_parameters *meta=meta_init(in_meta);
		if (write_proj_meta(pixSize,&proj,out_window,meta))
			meta_write(meta,out_meta);/*Only write out .meta if sucessful*/
	}
	
	{/*Fabricate output .ddr*/
		struct DDR ddr;
		c_intddr(&ddr);
		write_proj_ddr(pixSize,&proj,out_window,&ddr);
		c_putddr(out_ddr,&ddr);
	}
	
	/* Write the standard completion message and exit
	  ----------------------------------------------*/
/*	printf("projectGeo finished processing.\n\n");*/
	printf("\n");
	if (logflag) {
	  printLog("\n");
	}

	return 0;
}
	
