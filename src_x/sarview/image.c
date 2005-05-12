/*SARview image.c:
The main image I/O routines.  Contains initialization,
information, and update routines.
Currently, LAS and CEOS images are supported.
*/
#include "main.h"
#include "image.h"

#include <asf_raster.h> // for FLOAT_EQUIVALENT macro

#include <limits.h>

image_info image;/*Global image info structure*/
image_type type=image_none;
struct DDR *ddr=NULL;
FILE *ddr_file=NULL;
CEOS_FILE *ceos=NULL;
meta_parameters *meta=NULL;
histogram *data_hist=NULL;  /* used for histogram display */


/*****************************************************
 * image_delete
 * Nuke cached image info.*/
void image_delete(void)
{
	if (type==image_ddr) {
		free(ddr);
		fclose(ddr_file);
	}
	else if (type==image_ceos)
		closeCeos(ceos);
	if (meta!=NULL)
		meta_free(meta);

	type=image_none;
	ddr=NULL;
	ddr_file=NULL;
	ceos=NULL;
	meta=NULL;
	strcpy(image.fName,"<deleted>");
	image.width=image.height=-1;
}

/*****************************************************
 * image_loadLas:
 * Load routine number 1 -- read the metadata (if it's
 * there) and the ddr of the given image and set
 * link_imagewidth and height. Return 0 on failure.*/
int image_loadLas(char *fileName)
{
	type=image_ddr;

/* Get DDR structure (will created from meta if there is no DDR) */
	ddr=(struct DDR *)malloc(sizeof(struct DDR));
	c_getddr(fileName,ddr);

/* Get a meta structure (will be created from ddr if there's not a .meta */
	meta = meta_read(fileName);

	strcpy(image.fName,fileName);

/*Set the image size; based on the meta */
	image.width  = meta->general->sample_count;
	image.height = meta->general->line_count;

/*Open the image*/
	ddr_file = fopenImage(fileName,"rb");
	if (ddr_file==NULL)
		return 0;

	return 1;
}

/**********************************************************
 * image_loadCeos:
 * Load routine number 2 -- read the metadata (leader file)
 * of the given image and set link_imagewidth and height.
 * return 0 on failure.*/
int image_loadCeos(char *fileName)
{
	type=image_ceos;
	ceos=fopenCeos(fileName);
	strcpy(image.fName,fileName);

/*Get metadata*/
	ddr=&(ceos->ddr);
	meta= (ceos->meta);

/*Set the image size */
	image.width  = meta->general->sample_count;
	image.height = meta->general->line_count;

	return 1;
}

/*******************************************************
 * image_readLine:
 * Extract the entire given line from the current image.
 * Dest must be link_imagewidth floats long.*/
void image_readLine(float *dest,int lineY,int bandNo)
{
	int *iBuf,x;
        int bandLine;

	switch (type)
	{
	case image_ddr:
		bandLine = (bandNo * meta->general->line_count + lineY);
		get_float_line(ddr_file, meta, bandLine, dest);
		break;
	case image_ceos:
		iBuf=(int *)MALLOC(sizeof(int)*image.width);
		readCeosLine(iBuf,lineY,ceos);
		for (x=0;x<image.width;x++)
			dest[x]=(float)iBuf[x];
		FREE(iBuf);
		break;
	default:
		abort();/*Unknown image type*/
		break;
	}
}

/*********************************************************
 * image_slopeOffset:
 * Compute the pixel scaling slope and offset, and write
 * them to link_slope and link_offset
 * Actually this calculates the histogram too, so watch out! */
void image_slopeOffset(void)
{
	const int num_lines_to_examine = image.height;
	float *inBuf;
	histogram *h;
	int yCounter,lineY,x;
	double min=DBL_MAX, max=DBL_MIN;
	double width;
	int step_size = (image.height-1) / (num_lines_to_examine-1);
	const int hist_size = 1024;

/*Read in data to find the max and min*/
	inBuf=(float *)MALLOC(sizeof(float)*image.width);

/*Loop through data & compute max/min*/
	for (yCounter=0; yCounter<num_lines_to_examine; yCounter++)
	{
		lineY = yCounter * step_size;
		image_readLine(inBuf, lineY, 0);
		for (x=0; x<image.width; x++)
		{
			double val = inBuf[x];
			if ( FLOAT_EQUIVALENT(0.0, val) ) continue;
			if (min>val) min=val;
			if (max<val) max=val;
		}
	}
/*Stash the maximum and minimum in the image record*/
	image.min=min;
	image.max=max;

/*Loop through data *again* and compute histogram*/
	h = createHist(hist_size, min, max);
	for (yCounter=0; yCounter<num_lines_to_examine; yCounter++) {
		lineY = yCounter * step_size;
		image_readLine(inBuf, lineY, 0);
		addToHist(h, inBuf, image.width);
	}

/*Copy histogram of entire data set for display when requested*/
	data_hist = createHist(hist_size, min, max);
	copyHist(data_hist, h);

/*Re-set min and max based on the histogram-- this trimmed max & min
  work better because they ignore a few outlying pixels, and correctly
  map the vast majority.*/
	trimMaxMin(h,0.04,&min,&max);

/*Widen the range slightly (makes uniform-distribution
  images come out right by reversing the trimming)*/
	width=(max-min)*(1/(1.0-2*0.04)-1);
	min-=width/2;max+=width/2;

	if ((max-min)<0.01*(image.max-image.min))
		{max=image.max;min=image.min;}/*Don't mess with highly compressed images' min/max values*/
	if (image.min==0.0)
		min=0.0;/*Set minimum back to zero if it started there*/
	if (min==max)
		max=min+0.0000001;/*Put a *little* space between min and max*/

/*Now set slope and offset, to map pixels onto [0..255].  byte=slope*in+offset, so
	0=slope*min+offset and
	255=slope*max+offset.
	Thus */
	image.slope  = image.r_slope  = 255.0/(max-min);
	image.offset = image.r_offset = -image.r_slope*min;

	deleteHist(h);
	FREE(inBuf);
}

