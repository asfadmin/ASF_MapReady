/******************************************************************************
NAME: fit_warp

SYNOPSIS: fit_warp <in> <image> <out>

DESCRIPTION:
    	
    	fit_warp finds a weighted combination of the point offsets
    from fico to generate two output translation LAS images--
    out.horiz and out.vert.  These can be fed to the "-warp" option
    in remap.
    
       The resulting warping images can represent a non-linear 
    image-image shift, which may result in much higher phase
    coherence for interferometry over moving and deforming surfaces,
    such as glaciers or ice sheets.
    

OPTIONS:
    	<in> an fico offset point file
    	<image> is a LAS image, used to set the size of the offset files.
    	     Otherwise, it is unused.
    	<out> the base name for the warp image files.
    	     out.horiz will the be the horizontal warping offset image;
    	     out.vert will be the vertical warping offset image.
    		
EXTERNAL ASSOCIATES:none

FILE REFERENCES:none

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     5/99   O. Lawlor	Modified from fit_plane, for mesh-based
    				antarctic ice sheet interferometry.
    1.1     7/05   R. Gens      Removed DDR dependency. Took care of endianess

HARDWARE/SOFTWARE LIMITATIONS: none


ALGORITHM REFERENCES:

BUGS: none known

******************************************************************************/
/****************************************************************************
*								            *
*   Fit_warp takes, as an input, correlation points from fico and 	    *
*   produces as output a horizontal and vertical warping offset image	    *
*   for use with remap.							    *
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

float version=1.1;

void usage(char *name);

#define REMAP_CHIP 16 /*Downsize factor of remap's warping image*/
#define MAX_POINTS 66000 /*Largest number of points to read*/

typedef struct {
	int x,y;/*Location of point*/
	double offX;/*Offset at point in X direction*/
	double offY;/*Offset at point in Y direction*/
	double weight;/*strength of point*/
} point;/*A single (fico-generated) offset point*/

point points[MAX_POINTS];
int nPoints=0;

/****************************************
findOffset:
Return the image-image offset at the given point, by
finding a weighted combination of the point offsets above.
	offset=sum(offset[i]*weight[i])/sum(weight[i]);

The weighting used is 
	weight[i]=point's weight/(dist(target,point)^4);
*/
void findOffset(int x,int y,float *offX,float *offY)
{
	double totalOffX=0,totalOffY=0;
	double totalWeight=0;
	int i;
	for (i=0;i<nPoints;i++)
	{
		int dx=points[i].x-x;
		int dy=points[i].y-y;
		/*Compute the square of the distance between the
		  target and this point plus some small constant
		  (30 pixels, squared).  The constant controls how much
		  one point's offset is smeared into the next (might want to be user-controllable)*/
		double distSq=900+dx*dx+dy*dy;
		
		/*A point's offset starts with the point's weight; but is
		  divided by the distance to the target to the fourth power.*/
		double weight=points[i].weight/(distSq*distSq);
		
		/*Add in this point's contribution to the overall offset and weight*/
		totalOffX+=points[i].offX*weight;
		totalOffY+=points[i].offY*weight;
		totalWeight+=weight;
		
	}
	*offX=totalOffX/totalWeight;
	*offY=totalOffY/totalWeight;
}

/*************************************
Main: fill point offset array, and call
findOffset for every possible offset.
*/
int main(int argc,char **argv)
{
	int x,y,line_count,sample_count;
	char line[255],*outName;
	FILE *in,*outH,*outV;
	float *bufH,*bufV;
	meta_parameters *meta;
	if (argc!=4) usage(argv[0]);
	
	in=FOPEN(argv[1],"r");
	meta = meta_read(argv[2]);
	outName=argv[3];
	
/*add mapping points */
	while (NULL!=(fgets(line,255,in)))
	{
		float e1x,e1y,e2x,e2y,weight;
		sscanf(line,"%f%f%f%f%f",&e1x,&e1y,&e2x,&e2y,&weight);
		points[nPoints].x=e1x/REMAP_CHIP;
		points[nPoints].y=e1y/REMAP_CHIP;
		points[nPoints].offX=e1x-e2x;
		points[nPoints].offY=e1y-e2y;
		points[nPoints].weight=weight;
		nPoints++;
	}
	printf("%d points read\n",nPoints);
	
/*Create offset images*/
	line_count = meta->general->line_count;
	sample_count = meta->general->sample_count;
	meta->general->line_count = (int) ceil(line_count/((float) REMAP_CHIP));
	meta->general->sample_count = (int) ceil(sample_count/((float) REMAP_CHIP));
	meta_write(meta, appendExt(outName,"_horiz"));
	meta_write(meta, appendExt(outName,"_vert"));
	line_count = meta->general->line_count;
	sample_count = meta->general->sample_count;
	
	printf("Output warping images will be %d lines by %d samples\n",
	       line_count, sample_count);
	outH=FOPEN(appendExt(outName,"_horiz.img"),"wb");
	outV=FOPEN(appendExt(outName,"_vert.img"),"wb");
	
	bufH=(float *)MALLOC(sizeof(float)*sample_count);
	bufV=(float *)MALLOC(sizeof(float)*sample_count);

/*Find & write offsets for each pixel*/
	/*Loop over each line in the (zoomed-out) output file*/
	for (y=0;y<line_count;y++)
	{
		for (x=0;x<sample_count;x++)
			findOffset(x,y,&bufH[x],&bufV[x]);
		printf(".");fflush(stdout);
		put_float_line(outH, meta, y, bufH);
		put_float_line(outV, meta, y, bufV);
	}
	printf("\n");
	
	FREE(bufH);
	FREE(bufV);
	FCLOSE(outH);
	FCLOSE(outV);

	return 0;
}

void usage(char *name)
{
		printf("\n\
Usage: %s <in> <image> <out>\n\
\t<in> input: a correlation point file (from coregister_fine)\n\
\t<image> input: an image (to set output size)\n\
\t<out> output: a remap-compatible set of warping files.\n\
\n\
Fit_warp takes, as an input, correlation points from coregister_fine and \n\
produces as output a horizontal and vertical warping offset image\n\
for use with remap.\n\
\nVersion %4.2f, ASF SAR Tools\n\n",name,version);
		exit(1);
}
