/*SARview image.c:
The main image I/O routines.  Contains initialization,
information, and update routines.
Currently, LAS and CEOS images are supported.
*/
#include "main.h"
image_info image;/*Global image info structure*/

#include "image.h"

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
	ddr=(struct DDR *)malloc(sizeof(struct DDR));
	c_getddr(fileName,ddr);

	strcpy(image.fName,fileName);
/*Set the image size; based on the DDR*/
	image.width=ddr->ns;
	image.height=ddr->nl;

/*Open the image*/
	ddr_file=fopenImage(fileName,"rb");
	if (ddr_file==NULL)
		return 0;

/*Try to read in the metadata*/
	if (extExists(fileName,".meta"))
		meta=meta_read(fileName);

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
	ddr=&(ceos->ddr);

	strcpy(image.fName,fileName);
/*Set the image size; based on the DDR*/
	image.width=ddr->ns;
	image.height=ddr->nl;

/*Read in the metadata*/
	meta=meta_create(fileName);
	return 1;
}

/*******************************************************
 * image_readLine:
 * Extract the entire given line from the current image.
 * Dest must be link_imagewidth floats long.*/
void image_readLine(float *dest,int lineY,int bandNo)
{
	int *iBuf,x;
	switch (type)
	{
	case image_ddr:
		getFloatLine_mb(ddr_file,ddr,lineY,bandNo,dest);
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
 * them to link_slope and link_offset*/
void image_slopeOffset(void)
{
	const int nLines=100;/*Number of lines of data to examine*/

	float *inBuf;
	histogram *h;
	int yCounter,lineY,x;
	double min=200000000000000000.0,max=-200000000000000000.0;
	double width;

/*Make a quick check for byte data.*/
/* Commented out so histogram will always be calculated
	if (ddr->dtype==1)
	{ We have *byte* data-- hardcode slope & offset, and return.
		image.min=0;
		image.max=255;
		image.slope =image.r_slope=1.0;
		image.offset=image.r_offset=0.0;
		return;
	}
*/
		
/*If not byte data, we'll have to read in some data to find the max and min*/
	inBuf=(float *)MALLOC(sizeof(float)*image.width);

/*Loop through data & compute max/min*/
	for (yCounter=0;yCounter<nLines;yCounter++)
	{
		lineY=yCounter*(image.height-1)/(nLines-1);
		image_readLine(inBuf,lineY,0);
		for (x=0;x<image.width;x++)
		{
			double val=inBuf[x];
			if (min>val) min=val;
			if (max<val) max=val;
		}
	}
/*Stash the maximum and minimum in the image record*/
	image.min=min;
	image.max=max;

/*Loop through data *again* and compute histogram*/
	h=createHist(1024,min,max);

	for (yCounter=0;yCounter<nLines;yCounter++)
	{
		lineY=yCounter*(image.height-1)/(nLines-1);
		image_readLine(inBuf,lineY,0);
		addToHist(h,inBuf,image.width);
	}

/*Copy histogram of entire data set for display when requested*/
	data_hist=createHist(1024,min,max);
	copyHist(data_hist,h);

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
	image.slope =image.r_slope=255.0/(max-min);
	image.offset=image.r_offset=-image.r_slope*min;
	
	deleteHist(h);
	FREE(inBuf);	
}

