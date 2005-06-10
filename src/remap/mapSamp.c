/*MapSamp.c:
	The private implementation of the mapping and
sampling functions. 
*/
#include "asf.h"
#include "Matrix2D.h"
#include "las.h"
#include "remap.h"
#include "quadratic.h"
#include "poly.h"

/***************** Mapping Functions: ***************
A mapping function shows where to put the input pixels
in output space.  doMap is called for each output point
(so it needs to be fast), and it returns the location
in input space where the corresponding pixel can be
found.
*/

typedef struct {
	mappingStruct m;
	Matrix2D *outToIn;
	Matrix2D *inToOut;
} matrixMapRec;

#define mat ((matrixMapRec *)map)
#define coeff mat->inToOut->e

typedef struct {
	mappingStruct m;
	quadratic_2d inToOutX,inToOutY;
	quadratic_2d outToInX,outToInY;
} quadraticMapRec;

typedef struct {
	mappingStruct m;
	poly_2d *inToOutX,*inToOutY;
	poly_2d *outToInX,*outToInY;
} polyMapRec;


/*Prototypes:*/
void matrix_doMap(matrixMapRec * map,fPoint  in, fPoint  *out);
void quadratic_doMap(matrixMapRec * map,fPoint  in, fPoint  *out);
void poly_doMap(matrixMapRec * map,fPoint  in, fPoint  *out);
mappingFunction warp_createMap(char *fName);
void warp_freeMap(mappingFunction map);

/*************
CreateMap allocates storage for the given type of mapping,
and initializes the map.*/
mappingFunction createMap(mappingType m,void *param)
{
	if (m==matrixMap)
	{
		mappingFunction map=(mappingFunction)MALLOC(sizeof(matrixMapRec));
		map->type=m;
		map->doMap=(mappingFunc)matrix_doMap;
		mat->inToOut=identityMatrix2D();
		mat->outToIn=identityMatrix2D();
		return map;
	} else if (m==quadraticMap)
	{/*Quadratic map type expects a file name parameter*/
		FILE *f=FOPEN((char *)param,"r");
		quadraticMapRec *q=(quadraticMapRec *)MALLOC(sizeof(quadraticMapRec));
		q->m.type=m;
		q->m.doMap=(mappingFunc)quadratic_doMap;
		quadratic_read(&q->inToOutX,f);quadratic_read(&q->inToOutY,f);				quadratic_read(&q->outToInX,f);quadratic_read(&q->outToInY,f);
		FCLOSE(f);
		return (mappingFunction)q;
	} else if (m==polyMap)
	{/*Polynomial map type expects a file name parameter*/
		FILE *f=FOPEN((char *)param,"r");
		polyMapRec *q=(polyMapRec *)MALLOC(sizeof(polyMapRec));
		q->m.type=m;
		q->m.doMap=(mappingFunc)poly_doMap;
		q->inToOutX=poly_read(f);
		q->inToOutY=poly_read(f);				
		q->outToInX=poly_read(f);
		q->outToInY=poly_read(f);
		FCLOSE(f);
		return (mappingFunction)q;
	} else if (m==warpMap)
		return warp_createMap((char *)param);
	else {
	  sprintf(errbuf, "   ERROR: Unknown map type %d passed to createMap.\n",m);
	  printErr(errbuf);
	}
	return NULL;
}

/*************
AddMatrix concatenates the given Matrix with
the current mapping, and then deletes the Matrix.*/
void addMatrix(mappingFunction map,Matrix2D *thisMatrix)
{
	if (map->type==matrixMap)
	{
		Matrix2D *oldMat=mat->inToOut,*oldMat2=mat->outToIn;
		mat->inToOut=postMult2D(mat->inToOut,thisMatrix);
		mat->outToIn=invertMatrix2D(mat->inToOut);
		free(oldMat);free(oldMat2);free(thisMatrix);
	} 
	else {
	  sprintf(errbuf,"   ERROR: Unknown map function passed to addMatrix.\n");
	  printErr(errbuf);
	}
}
/**************
UpdateDDR updates various fields of the DDR structure for
the new map.*/
void updateDDR(mappingFunction map,struct DDR *inDDR, struct DDR *outDDR)
{
/*Move the origin*/
	fPoint inOrigin={0.0,0.0},outOrigin;
	map->doMap((void *)map,inOrigin,&outOrigin);
	outDDR->master_sample+=outOrigin.x;
	outDDR->master_line+=outOrigin.y;
	
	if (map->type==matrixMap)
	{/*Is a (linear) matrix map, so we can update the DDR
	scale values and still have them make sense.*/
		float xScale=1.0,yScale=1.0;
		if (map->type==matrixMap)
		{
			xScale=sqrt(coeff[0][0]*coeff[0][0]+coeff[1][0]*coeff[1][0]);
			yScale=sqrt(coeff[0][1]*coeff[0][1]+coeff[1][1]*coeff[1][1]);
		}
		outDDR->sample_inc/=xScale;
		outDDR->line_inc/=yScale;
		outDDR->pdist_x/=xScale;
		outDDR->pdist_y/=yScale;
	} else {/*Some other mapping type-- don't update the DDR*/
		
	}
}
/***************
forwardMap:
	Performs a forward mapping: from the input space to the output space.
*/
void forwardMap(mappingFunction map, fPoint  in, fPoint  *out)
{
	if (map->type==matrixMap)
		*out=transformPoint(mat->inToOut,in.x,in.y);
	else if (map->type==quadraticMap)
	{	
		quadraticMapRec *q=(quadraticMapRec *)map;
		out->x=quadratic_eval(&q->inToOutX,in.x,in.y);
		out->y=quadratic_eval(&q->inToOutY,in.x,in.y);
	}
	else if (map->type==polyMap)
	{	
		polyMapRec *q=(polyMapRec *)map;
		out->x=poly_eval(q->inToOutX,in.x,in.y);
		out->y=poly_eval(q->inToOutY,in.x,in.y);
	}
	else if (map->type==warpMap)
		*out=in;/* FIXME: Identity transformation *isn't* right, but it's close if displacements are small... */
	else
	{
	  sprintf(errbuf, "   ERROR: Unknown map function passed to forwardMap.\n");
	  printErr(errbuf);
	}
}
/***************
doMap:
	Performs an inverse mapping: from the output space to the input space.
Implemented as function pointers for greater speed.
*/
void matrix_doMap(matrixMapRec * map,fPoint  in, fPoint  *out)
{
	*out=transformPoint(map->outToIn,in.x,in.y);
}
void quadratic_doMap(matrixMapRec * map,fPoint  in, fPoint  *out)
{
	quadraticMapRec *q=(quadraticMapRec *)map;
	out->x=quadratic_eval(&q->outToInX,in.x,in.y);
	out->y=quadratic_eval(&q->outToInY,in.x,in.y);
}
void poly_doMap(matrixMapRec * map,fPoint  in, fPoint  *out)
{
	polyMapRec *q=(polyMapRec *)map;
	out->x=poly_eval(q->outToInX,in.x,in.y);
	out->y=poly_eval(q->outToInY,in.x,in.y);
}

/***************
KillMap completely destroys a map.*/
void killMap(mappingFunction map)
{
	if (map->type==matrixMap)
	{
		free(mat->inToOut);
		free(mat->outToIn);
	} else if (map->type==warpMap)
		warp_freeMap(map);
	FREE(map);
}

/****************** Sampling Functions: *****************
Once we know where the point in input space is, we get
a brightness value for that point by calling a sampling 
function.  Samples are nearest-neighbor, bilinear interpolation,
sinc interpolation (fourier synthesis), and (uniform- or
non-uniform weight) kernel.
*/
#define numSincs 512
#define sincPoints 8
typedef struct {
	sampleStruct s;
	float kern[sincPoints*numSincs];
} sincSampData;

typedef struct {
	sampleStruct s;
	int sx,sy,sxHalf,syHalf;
	float scale;
	float *k; /*a rows-are-sequential sx*sy array*/
} kernelSampData;

/*Prototypes:*/
float nearest_doSamp(sampleFunction samp,pixelFetcher *getRec,fPoint inPt);
float bilinear_doSamp(sampleFunction samp,pixelFetcher *getRec,fPoint inPt);
float kernel_doSamp(kernelSampData * k,pixelFetcher *getRec,fPoint inPt);
float sinc_doSamp(sincSampData * sinc,pixelFetcher *getRec,fPoint inPt);

/*CreateSamp creates a sampling function of the specified type.
It only works for sinc, nearest, and bilinear sampling--
kernel based sampling requires more parameters, and has its own creation
routine.*/
sampleFunction createSamp(sampleType s)
{
	sampleFunction samp;
	if (s==sincSamp)
	{
		int sincNo,pointNo;
		sincSampData *sinc=(sincSampData *)(samp=(sampleFunction)MALLOC(sizeof(sincSampData)));
		sinc->s.type=sincSamp;
		strcpy(sinc->s.description,"Sinc interpolation.\n");
		sinc->s.doSamp=(sampleFunc)sinc_doSamp;
		/*Initialize Sinc Kernel table*/
		for (sincNo=0;sincNo<numSincs;sincNo++)
		{
			float sincWeight=0.0;
			float xShift=((float)sincNo)/numSincs;
			float *sincKern=&sinc->kern[((int)(xShift*numSincs))*sincPoints];
			int cen=sincPoints/2;
			for (pointNo=0;pointNo<sincPoints;pointNo++)
			{
				float ex=pointNo-cen+1.0-xShift;
				if (ex==0.0)
					sincKern[pointNo]=1.0;
				else sincKern[pointNo]=sin(M_PI*ex)/(M_PI*ex);
				sincWeight+=sincKern[pointNo];
			}
			/*Now we must normalize the sinc weighting, by dividing by sincWeight*/
			sincWeight=1.0/sincWeight;
			for (pointNo=0;pointNo<sincPoints;pointNo++)
				sincKern[pointNo]*=sincWeight;
		}
	} else {
		samp=(sampleFunction)MALLOC(sizeof(sampleStruct));
		samp->type=s;
		if (s==nearestSamp)
		{
			strcpy(samp->description,"Nearest-neighbor.\n");
			samp->doSamp=(sampleFunc)nearest_doSamp;
		}
		else if (s==bilinearSamp)
		{
			strcpy(samp->description,"Bilinear.\n");
			samp->doSamp=(sampleFunc)bilinear_doSamp;
		}
		else 
		{
			sprintf(errbuf, "   ERROR: Bad sampling type %d passed to perform_mapping.\n",s);
			printErr(errbuf);
		}
	}
	return samp;
}
sampleFunction createKernelSamp(int x,int y)
{
	kernelSampData *k=(kernelSampData *)MALLOC(sizeof(kernelSampData));
	k->s.type=kernelSamp;
	sprintf(k->s.description,"Kernel-based filtering: kernel size is %ix%i.\n",x,y);
	k->s.doSamp=(sampleFunc)kernel_doSamp;
	k->sx=x;k->sy=y;k->sxHalf=k->sx/2;k->syHalf=k->sy/2;
	k->scale=1.0/((double)k->sx*k->sy);
	k->k=NULL;
	return (sampleFunction)k;
}
sampleFunction readKernelFile(char *filename)
{
	kernelSampData *k;
	int x,y;
	float inFloat;
	FILE *f=FOPEN(filename,"r");

	/*******FOPEN TAKES CARE OF THIS NOW********************
	if (f==NULL) {
	  sprintf(errbuf, "   ERROR: Cannot open kernel file '%s'.\n",filename);
	  printErr(errbuf);
	}
	*******************************************************/

	fscanf(f,"%i %i",&x,&y);
	k=(kernelSampData *)createKernelSamp(x,y);
	k->k=(float *)MALLOC(sizeof(float)*k->sx*k->sy);
	for (y=0;y<k->sy;y++)
		for (x=0;x<k->sx;x++)
		{
			fscanf(f,"%f",&inFloat);
			k->k[y*k->sx+x]=inFloat;
		}
	fclose(f);
	return (sampleFunction)k;
}


float nearest_doSamp(sampleFunction samp,pixelFetcher *getRec,fPoint inPt)
{
	return fetchPixelValue(getRec,(int)(inPt.x+10)-10,(int)(inPt.y+10)-10);
}
float bilinear_doSamp(sampleFunction samp,pixelFetcher *getRec,fPoint inPt)
{
	/*ix and iy used to be computed as floor(inPt); but this is 
	much faster and gives the same results for inPt>-9*/
	int ix=(int)(inPt.x+10)-10,iy=(int)(inPt.y+10)-10;
	register float dx=inPt.x-ix,dy=inPt.y-iy;
	float tl=fetchPixelValue(getRec,ix,iy),tr=fetchPixelValue(getRec,ix+1,iy);
	float bl=fetchPixelValue(getRec,ix,iy+1),br=fetchPixelValue(getRec,ix+1,iy+1);
	float tc=tl+(tr-tl)*dx;
	float bc=bl+(br-bl)*dx;
	return tc+(bc-tc)*dy;
}
float kernel_doSamp(kernelSampData * k,pixelFetcher *getRec,fPoint inPt)
{
	register float outValue=0.0;
	register int i,j;
	int starti=inPt.x-k->sxHalf,endi=starti+k->sx,
	    startj=inPt.y-k->syHalf,endj=startj+k->sy;
	if (k->k==NULL)
	{
		for (j=startj;j<endj;j++)
			for (i=starti;i<endi;i++)
				outValue+=fetchPixelValue(getRec,i,j);
		outValue*=k->scale;
	} else {
		register int kIndex=0;
		for (j=startj;j<endj;j++)
		   	for (i=starti;i<endi;i++)
				outValue+=k->k[kIndex++]*fetchPixelValue(getRec,i,j);
	}
	return outValue;
} 
float sinc_doSamp(sincSampData * sinc,pixelFetcher *getRec,fPoint inPt)
{
	register float outValue=0.0;
	register int i,j;
	int ix=floor(inPt.x),iy=floor(inPt.y),sx,sy;
	float dx=inPt.x-ix,dy=inPt.y-iy;
	register float *sincX=&sinc->kern[((int)(dx*numSincs))*sincPoints],
	     *sincY=&sinc->kern[((int)(dy*numSincs))*sincPoints];
	int ex=sincPoints+(sx=ix+1-sincPoints/2);
	int ey=sincPoints+(sy=iy+1-sincPoints/2);
	for (j=sy;j<ey;j++)
		for (i=sx;i<ex;i++)
			outValue+=sincX[i-sx]*sincY[j-sy]*fetchPixelValue(getRec,i,j);
	return outValue;
}

void killSamp(sampleFunction samp)
{
	if (samp->type==kernelSamp)
		if (((kernelSampData *)samp)->k!=NULL)
			FREE(((kernelSampData *)samp)->k);
	FREE(samp);
}


