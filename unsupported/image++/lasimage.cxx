#include <stdio.h>
#include "main.h"
#include "object.h"
#include "buffer2D.h"
#include "TWZimage.h"
#include "las.h"
class lasimage:public image { public:
	virtual void init2(const char *fileBaseName,const char *ext);
	virtual Bool mouseClick(int button,double x, double y);
	virtual char *getName(void);
	virtual Bool drawIntoBuffer(buffer2D *b,Bool needToDraw);
	virtual Bool drawSlowly(buffer2D *b,Bool needToDraw,Window w,int sizeX,int sizeY);
	virtual void die(void);
protected:
	FILE *f;
	struct DDR ddr;
	int dsize;
	virtual void initReading(const char *fileBaseName,const char *ext);
	virtual void readInLine(float *line,long y);
	virtual void writeOutLine(const float *line,long y,buffer2D *dest);
	virtual void computeMaxMin(Window w,int sizeX,int sizeY);
	virtual double getIntImageValue(int x,int y);
	float scale,shift,scaleX;
	Bool maxMinAreSet;
	float avg,max,min;
	char filename[255];
};
image *createLAS(const char *fileBaseName,const char *ext);
image *createLAS(const char *fileBaseName,const char *ext)
{
	lasimage *lasImg=new lasimage;
	lasImg->init2(fileBaseName,ext);
	return lasImg;
}

void lasimage::init2(const char *fileBaseName,const char *ext)
{
	image::init0();
	initReading(fileBaseName,ext);
}
char *lasimage::getName(void)
{
	return filename;
}
Bool lasimage::mouseClick(int button,double x, double y)
{
	if (button==1)
	{
		printf("Image - %s: \n\tx=%-12.3f y=%-12.3f value=%-15.6f\n",
			filename,x,y,getImageValue(x,y));
		return True;
	}
	return False;
}
void lasimage::initReading(const char *fileBaseName,const char *ext)
{
	char fileName[255],ddrName[255];
	strcpy(filename,fileBaseName);
	strcat(strcat(strcpy(fileName,fileBaseName),"."),ext);
	strcat(strcpy(ddrName,fileBaseName),".ddr");
	if (c_getddr(ddrName,&ddr)!=E_SUCC) 
	{
		printf("Fatal Error: DDR file '%s' could not be read in.\n",ddrName);
		exit(1);
	}
	if (NULL==(f=fopen(fileName,"rb"))) 
	{
		printf("Fatal Error: Image file '%s' could not be opened.\n",fileName);
		exit(1);
	}
	dsize=ddr.dtype;
	if (ddr.dtype==3) dsize=4;
	wid=ddr.ns;
	height=ddr.nl;
	maxMinAreSet=False;
}
void lasimage::computeMaxMin(Window w,int sizeX,int sizeY)
{
	float *buf;
	register int y,x;
	register double sum=0.0,lmin=20000000000000.0,lmax=-20000000000000.0;
	if (maxMinAreSet)
		return;
	buf=(float *)Malloc(sizeof(float)*wid);
	for (y=0;y<100;y++)
	{
		double lineSum=0.0;
		readInLine(buf,y/99.0*(height-1));
		if (w)
			tellUserToWait(w,sizeX,sizeY,y,200);
		for (x=0;x<wid;x++)
		{
			lineSum+=buf[x];
			if (lmax<buf[x])
				lmax=buf[x];
			if (lmin>buf[x])
				lmin=buf[x];
		}
		sum+=lineSum;
	}
	Free((void *)buf);
	min=lmin;
	max=lmax;
	avg=sum/((double)wid*(double)height);
	scale=(float)lenColor/(max-min);
	shift=startColor-min*scale;
	maxMinAreSet=True;
}
Bool lasimage::drawIntoBuffer(buffer2D *b,Bool needToDraw)
{
	double scaleY=(height-0.00001)/(b->frame.bottom-1);
	float *buf=(float *)Malloc(sizeof(float)*wid);
	int y;
	computeMaxMin(0,0,0);
	scaleX=(double)(wid-0.00001)/(b->frame.right-1);
	for (y=0;y<b->height;y++)
	{
		readInLine(buf,(int)((double)y*scaleY));
		writeOutLine(buf,y,b);
	}
	return True;
}
void resampleLine(float *in,int inWid,float *out,int outWid);
void resampleLine(float *in,int inWid,float *out,int outWid)
{
	register int outX,inX=-1,sumX;
	for (outX=0;outX<outWid;outX++)
	{
		register float outval=0.0;
		int oldInX=inX;
		inX=outX*(inWid-1)/(outWid-1);
		for (sumX=oldInX+1;sumX<=inX;sumX++)
			outval+=in[sumX];
		out[outX]=outval/(inX-oldInX);
	}
}
Bool lasimage::drawSlowly(buffer2D *b,Bool needToDraw,Window w,int sizeX,int sizeY)
{
	double scaleY=(height-0.00001)/(b->frame.bottom-1);
	float *in=(float *)Malloc(sizeof(float)*wid);
	float *out=(float *)Malloc(sizeof(float)*b->wid);
	int y;
	Bool wasMaxMin=maxMinAreSet;
	initUserWait(w);
	computeMaxMin(w,sizeX,sizeY);
	scaleX=(double)(wid-0.00001)/(b->frame.right-1);
	for (y=0;y<b->height;y++)
	{
		if (wasMaxMin)
			tellUserToWait(w,sizeX,sizeY,y,b->height);
		else
			tellUserToWait(w,sizeX,sizeY,y+b->height,2*b->height);
		readInLine(in,(int)((double)y*scaleY));
		resampleLine(in,wid,out,b->wid);
		writeOutLine(out,y,b);
	}
	Free((void *)in);
	Free((void *)out);
	return True;
}
void lasimage::readInLine(float *dest,long yLine)
{
	int x,maxX=wid;
	fseek(f,yLine*maxX*dsize,0);
	if (ddr.dtype==4) 
	{
		if (maxX!=fread(dest,dsize,maxX,f)) 
		{
			printf("Fatal LAS read error on image line %i!\n",yLine);
			exit(1);
		}
	} else {
		float *inputBuf=(float *)Malloc(dsize*maxX);
		if (maxX!=fread(inputBuf,dsize,maxX,f)) 
		{
			printf("Fatal LAS read error on image line %i!\n",yLine);
			exit(1);
		}
		if (ddr.dtype==1)
			for (x=0;x<maxX;x++)
				dest[x]=((unsigned char *)inputBuf)[x];
		else if (ddr.dtype==2)
			for (x=0;x<maxX;x++)
				dest[x]=((short *)inputBuf)[x];
		else if (ddr.dtype==3)
			for (x=0;x<maxX;x++)
				dest[x]=((long *)inputBuf)[x];
		Free((void *)inputBuf);
	}
}
void lasimage::writeOutLine(const float *line,long y,buffer2D *dest)
{
	register unsigned int x,outVal;
	register long offset=y*dest->rowBytes;
	register char *imageData=(char *)(dest->baseAddr);
	switch(dest->depthBytes)
	{
	case 1:
		for (x=0;x<dest->frame.right;x++)
			imageData[offset+x]=char(line[x]*scale+shift);
		break;
	case 2:
		for (x=0;x<dest->frame.right;x++)
		{
			outVal=0x1F&((int)(line[x]*scale+shift));
			*(unsigned short *)&(imageData[offset+2*x])=makeGrey16(outVal);
		}
		break;
	case 4:
		for (x=0;x<dest->frame.right;x++)
		{
			outVal=0xFF&((int)(line[x]*scale+shift));
			*(unsigned long *)&(imageData[offset+4*x])=makeGrey32(outVal);
		}
		break;
	}
}
void lasimage::die(void)
{
	fclose(f);
	image::die();
}

double lasimage::getIntImageValue(int x,int y)
{
	pixelType b;
	if ((x<0)||(y<0)||(x>=wid)||(y>=height))
		return 0.0;
	fseek(f,dsize*(wid*((int)y)+((int)x)),0);
	if (1!=fread(&b,dsize,1,f))
	{
		fprintf(stderr,"LAS Image Error: could not read pixel x=%i, y=%i.\n"
				"Does the file %s exist?\n",x,y,filename);
		return -999.0;
	}
	switch(ddr.dtype)
	{
		case 1:
			return b.c;
		case 2:
			return b.s;
		case 3:
			return b.l;
		case 4:
			return b.f;
		default:
			fprintf(stderr,"LAS Image error: unrecognized dtype %i\n",ddr.dtype);
			return -999.0;
	}
}
