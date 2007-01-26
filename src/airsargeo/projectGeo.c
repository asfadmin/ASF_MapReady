/****************************************************************************
NAME:				projectGeo

SYNOPSIS:
	Reads in and map-projects a set of image latitude & longitude
values.

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  1.0    3/99  O. Lawlor     ASF      Total re-write of trancoord.

ALGORITHM REFERENCES:
        Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
        Survey Proffessional Paper 1395 (Supersedes USGS Bulletin 1532), United
        State Government Printing Office, Washington D.C., 1987. 

        "Software Documentation for GCTP General Cartographic Transformation
        Package", U.S. Geological Survey National Mapping Division, May 1982.

        Clarie, Charles N, "State Plane Coordinates by Automatic Data 
        Processing", U.S. Department of Commerce, Environmental Science 
        Services Admin., Coast and Geodetic Survey, Publication 62-4, 1973.
*****************************************************************************/
#include "projectGeo.h"

#define VERSION 1.0

/* function declarations */
void print_usage(void);

double aLat,aLon;/*First point in grid (for zone calculations)*/

int main(int argc,char **argv)
{
	double pixSize=0;/*Pixel size (m)*/
	window *out_window=NULL;/*Projection window*/
	proj_prm proj;/*Projection parameters*/
	gridPoint grid[MAX_GRIDPTS];/*Grid of converted points*/
	int nGrid;/*Number of points in grid*/
	
	char *in_meta,*in_proj,*in_key,*in_tie;/*Input parameter names*/
	char *out_tie,*out_meta,*out_ddr;/*Output parameter names*/
	int winIsLatLon=0;/*Is user-specified window in lat/lon instead of projection coords?*/
	int pointOfInterest=0;/*Is user-specified window (lat lon nl ns)?*/

	extern int optind;
	int c;
	
	/*Parse passed flags*/
	while ((c=getopt(argc,argv,"p")) != EOF)
	switch (c) {
	case 'p':
		sscanf(argv[optind++],"%lf",&pixSize);
		break;
	case '?':
		print_usage();
		break;
	}
	if (argc-optind<7) 
		print_usage();
	
	/*Copy over required parameters*/
	in_meta=argv[optind];
	in_proj=argv[optind+1];
	in_key=argv[optind+2];
	in_tie=argv[optind+3];
	out_tie=argv[optind+4];
	out_meta=argv[optind+5];
	out_ddr=argv[optind+6];
	optind+=7;/*Advance over required parameters*/
	if (argc-optind!=0)
	{/*More stuff!*/
		if (argc-optind==5)/*User specified an output window*/
		{
			if (0==strcmp(argv[optind],"-l"))
				winIsLatLon=1;/*User specified window as lat/lon*/
			else if (0==strcmp(argv[optind],"-o"))
				winIsLatLon=0;/*User specified window as projection coords.*/
			else if (0==strcmp(argv[optind],"-i"))
				pointOfInterest=1;/*User specified point of interest.*/
			else print_usage();/*Unrecognized option*/
			out_window=getUserWindow(pointOfInterest,argv[optind+1]);
		}
		else
			print_usage();
	}
	
/*Begin Processing:*/
	/*Determine output pixel size*/
	if (pixSize==0)/*User did not specify a pixel size*/
		pixSize=ex_pix_size(in_meta);
	printf("Setting output pixel size to %.2f meters\n",pixSize);

	/*Determine projection parameters*/
	if (0==get_proj_prm(in_proj,in_key,&proj))
		bail("ProjectGeo: Error loading projection file.\n");
	
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
	printf("projectGeo finished processing.\n\n");

	return 0;
}
	

void print_usage(void)
{
   printf("Usage:\n");
   printf("\tprojectGeo [-p pix] in_meta proj key in_tie out_tie out_meta out_ddr\n"
   	"       [[-o|-l] N S E W] [-i lat lon nl ns]\n");
   printf("\n");
   printf("    -p pix      set output pixel size to pix meters\n");
   printf("    in_meta     Input SAR metadata file\n");
   printf("    proj        Input Projection definition file\n");
   printf("    key         Input Projection key\n"
          "    in_tie      Input ASCII tie point file (lat lon inx iny)\n"
          "    out_tie     Output ASCII tie point file (outx outy inx iny)\n"
          "    out_meta    Output metadata file .meta\n"
          "    out_ddr     Output DDR file .ddr\n"
   	  "\n");
   
   printf("\n");
   printf(" Windowing Modes: -o, -l, or -i set the size of the output image.\n");
   printf("       The default is just big enough to contain the input image.\n");
   printf("\n");
   printf("   -o    Means N S E W are projection coordinates:\n");
   printf("    N    Y projection coordinate of top edge\n");   
   printf("    S    Y projection coordinate of bottom edge\n");
   printf("    E    X projection coordinate of right edge\n");
   printf("    W    X projection coordinate of left edge\n");
   printf("\n");
   printf("   -l    Means N S are latitude, and E W are longitude:\n");
   printf("    N    Latitude of north edge of output\n");   
   printf("    S    Latitude of south edge of output\n");
   printf("    E    Longitude of east edge of output\n");
   printf("    W    Longitude of west edge of output\n");
   printf("\n");
   printf("   -i    Means write a nl x ns output image with:\n");
   printf("    lat  Latitude of center of output\n");
   printf("    lon  Longitude of center of output\n");
   printf("\n"
   " This program converts the input metadata, projection file and key,\n"
   "and input tie points into the given output tie points, .meta file,\n"
   "and .ddr.  It map-projects the input points into the output points.\n"
   );
   printf("\nVersion %.2f, ASF STEP Tools\n",VERSION);
   printf("\n\n");
   exit(1);
}






