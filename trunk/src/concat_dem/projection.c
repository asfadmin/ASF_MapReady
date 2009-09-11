/*
Projection.c:
	Contains implementation of routines to create
output DDR and fetch line of DEM by projection coordinates.
*/

#include "asf.h"
#include "ddr.h"
#include "caplib.h"
#include "dem.h"

/*
Extract_proj:
	Extracts relevant projection parameters from DDR.
This is only necessary since in some projections, the 
projection y and line numbers increase in opposite directions. 
Thus we have to test to figure out whether the projection 
distance (per pixel) should be positive or negative.

	After calling this routine,
the projection coordinate of a point (x,y) is:
	px=pStartX+x*pDistX;
	py=pStartY+y*pDistY;
*/

void extract_proj(struct DDR *inDDR,
	double *pStartX,double *pStartY,
	double *pDistX,double *pDistY)
{
/*Figure out the projection coordinates:*/
	if (inDDR->upleft[1]<inDDR->upright[1])
		*pStartX=inDDR->upleft[1],*pDistX=inDDR->pdist_x;
	else
		*pStartX=inDDR->upleft[1],*pDistX=-inDDR->pdist_x;
	
	if (inDDR->upleft[0]<inDDR->loleft[0])
		*pStartY=inDDR->upleft[0],*pDistY=inDDR->pdist_y;
	else
		*pStartY=inDDR->upleft[0],*pDistY=-inDDR->pdist_y;
}


/*Create_outDDR:
	Uses the given extents and input DDR to construct
an output DDR and determine the output projection start and deltas.*/
void create_outDDR(extents ext,
	struct DDR *inDDR,
	double *pStartX,double *pStartY,
	double *pDistX,double *pDistY,struct DDR *outDDR)
{
	double pEndX,pEndY;
/*First, copy all the fields of the output DDR over from the input DDR.
We'll replace certain fields later.*/
	*outDDR=*inDDR;
	
/*Next, get the projection start and deltas.*/
	extract_proj(inDDR,pStartX,pStartY,pDistX,pDistY);
	if (*pDistX>0)
		*pStartX=ext.h.min;
	else
		*pStartX=ext.h.max;
	if (*pDistY>0)
		*pStartY=ext.v.min;
	else
		*pStartY=ext.v.max;
	
/*We use the total projection distance to figure out 
how many lines and samples there should be in output space.*/
	outDDR->ns=fabs((ext.h.max-ext.h.min)/ *pDistX)+1;
	outDDR->nl=fabs((ext.v.max-ext.v.min)/ *pDistY)+1;
	
/*Now we set the DDR corner projection coordinates.  
Note the annoying convention of using [0] for Y, [1] for X.*/
	pEndX=*pStartX+(outDDR->ns-1)* *pDistX;
	pEndY=*pStartY+(outDDR->nl-1)* *pDistY;
	outDDR->upleft[0]=*pStartY;
	outDDR->upleft[1]=*pStartX;
	outDDR->upright[0]=*pStartY;
	outDDR->upright[1]=pEndX;
	outDDR->loleft[0]=pEndY;
	outDDR->loleft[1]=*pStartX;
	outDDR->loright[0]=pEndY;
	outDDR->loright[1]=pEndX;
	
/*Print output size and pixel spacing.*/
	printf("Output DEM has %i lines, %i samples, \n"
		"  and a pixel size of %.2fx%.2f meters.\n",
		outDDR->nl,outDDR->ns,*pDistY,*pDistX);
}

/*GetImageLineByProj:
	PRIVATE routine called by getLineByProj.
Has same semantics as getLineByProj.  Returns
zero-padded float array in out.
*/
void getImageLineByProj(double px,double py,int ns,image *im,float *out)
{
#define MIN(a,b) ((a)<(b) ? (a) : (b))

	register int x;
	int reqLine,reqSample;
	reqSample=(px-im->pStartX)/im->pDistX;
	reqLine=(py-im->pStartY)/im->pDistY;
	
	/*printf("Projection %f,%f corresponds to line %i, sample %i\n",
		px,py,reqLine,reqSample);*/
	
	if ((reqLine>=0) && (reqLine < im->ddr.nl))
	/*Line is in bounds, so we read it in and zero-fill the rest of the line.*/
	{
		int inSample,outSample,copySamples;
		
		if (reqSample>0)
		{
			inSample=reqSample;
			outSample=0;
		} else {
			inSample=0;
			outSample=-reqSample;
		}
		copySamples=MIN(ns-outSample,im->ddr.ns-inSample);
		getFloatLine(im->f,&im->ddr,reqLine,im->readBuf);
		for (x=0;x<outSample;x++)
			out[x]=0.0;
		for (x=0;x<copySamples;x++)
			out[x+outSample]=im->readBuf[x+inSample];
		for (x=outSample+copySamples;x<ns;x++)
			out[x]=0.0;
	}
	else /*Fill blank line.*/
		for (x=0;x<ns;x++)
			out[x]=0.0;
}

/*GetLineByProj:
	Fetches a line of height (and error) of the given
DEM located at specified projection coordinates.
*/
void getLineByProj(double px,double py,int ns,
	dem *in,float *destHt,float *destErr)
{
	getImageLineByProj(px,py,ns,in->height,destHt);
	if (in->err!=NULL)
		getImageLineByProj(px,py,ns,in->err,destErr);
}

