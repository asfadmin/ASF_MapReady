/*warpMap.c:
	The private implementation of the mapping 
functions for the warping map.  This map specifies
the conversion between input and output coordinates 
via two image files, which give the horizontal and
vertical offsets. 
*/
#include "asf.h"
#include "Matrix2D.h"
#include "las.h"
#include "remap.h"
#include "quadratic.h"

/***************** Mapping Function: Warp ***************
A mapping function shows where to put the input pixels
in output space.  doMap is called for each output point
(so it needs to be fast), and it returns the location
in input space where the corresponding pixel can be
found.
*/
#define zoomOut 16 /*Number of pixels a single offset refers to*/
#define zoomInv (1.0/zoomOut) 
#define zoomShift 4 /*Log base 2 of zoomOut*/

typedef struct {
	mappingStruct m;
	int width,height;/*Size of offset images*/
	float *horiz,*vert;/*Offset images*/
} warpMapRec;

/* Prototypes */
void read_image(char *fName,struct DDR *ddr,float *dest);
mappingFunction warp_createMap(char *fName);
void warp_freeMap(mappingFunction map);
void warp_doMap(const warpMapRec *map,fPoint in,fPoint *out);


/*Read the given LAS image into the given pre-allocated buffer*/
void read_image(char *fName,struct DDR *ddr,float *dest)
{
	int lineNo;
	FILE *fp=FOPEN(fName,"rb");
	for (lineNo=0;lineNo<ddr->nl;lineNo++)
		getFloatLine_mb(fp,ddr,lineNo,0,&dest[ddr->ns*lineNo]);
	FCLOSE(fp);
}

mappingFunction warp_createMap(char *fName)
{
	/*Set offset file names*/
	char *hName=appendExt(fName,".horiz"),*vName=appendExt(fName,".vert");
	struct DDR ddr;/*Offset file DDR*/

	/*Allocate mapping structure to return*/
	warpMapRec *w=(warpMapRec *)MALLOC(sizeof(warpMapRec));
	mappingFunction ret=(mappingFunction)w;
	ret->type=warpMap;
	ret->doMap=(mappingFunc)warp_doMap;
	
	/*Set parameters*/
	c_getddr(hName,&ddr);
	w->width=ddr.ns;
	w->height=ddr.nl;
	
	/*Open & ingest offset images*/
	w->horiz=(float *)MALLOC(sizeof(float)*ddr.ns*ddr.nl);
	read_image(hName,&ddr,w->horiz);
	w->vert=(float *)MALLOC(sizeof(float)*ddr.ns*ddr.nl);
	read_image(vName,&ddr,w->vert);
	
	return ret;
}

void warp_freeMap(mappingFunction map)
{
	warpMapRec *w=(warpMapRec *)map;
	FREE(w->horiz);w->horiz=NULL;
	FREE(w->vert);w->vert=NULL;
	FREE(w);
}

/***************
doMap:
	Performs an inverse mapping: from the output space to the input space.
Implemented as function pointers for greater speed.
*/
void warp_doMap(const warpMapRec *map,fPoint in, fPoint *out)
{
	register warpMapRec *w=(warpMapRec *)map;
	int inX=(((unsigned int)(in.x))>>zoomShift);
	int inY=(((unsigned int)(in.y))>>zoomShift);
	register int index;/*Index of interested pixel in offset image*/
	register double dx,dy;
	register double A,B,C,D;
	if ((inX>w->width-2)||(inY>w->height-2))
	{/*Pixel is out of the bounds of the warp-- return no offset*/
		*out=in;
		return;
	}
	/*Find pixel index and interpolation coefficients dx and dy*/
	index=inY*w->width+inX;
	dx=(in.x-(inX<<zoomShift))*zoomInv;dy=(in.y-(inY<<zoomShift))*zoomInv;
	
	/*Use bilinear interpolation to find horizontal offset*/
	A=w->horiz[index];                B=w->horiz[index+1];
	C=w->horiz[index+w->width];       D=w->horiz[index+w->width+1];
	A+=dx*(B-A);C+=dx*(D-C);
	out->x=in.x-(A+dy*(C-A));
	
	/*Use bilinear interpolation to find vertical offset*/
	A=w->vert[index];                B=w->vert[index+1];
	C=w->vert[index+w->width];       D=w->vert[index+w->width+1];
	A+=dx*(B-A);C+=dx*(D-C);
	out->y=in.y-(A+dy*(C-A));
}
/*
void quadratic_doMap(matrixMapRec * map,fPoint  in, fPoint  *out)
{
	quadraticMapRec *q=(quadraticMapRec *)map;
	out->x=quadratic_eval(&q->outToInX,in.x,in.y);
	out->y=quadratic_eval(&q->outToInY,in.x,in.y);
}*/


