/*
  LAS 6.0 Image I/O routines: these routines
  allow you to input and output LAS image lines of
  any type as though they were floating point arrays--
  these procedures take care of all neccessary conversions.
*/
#include <stdio.h>
#include <math.h>
#include "ddr.h"


void getFloatLine(FILE *f,const struct DDR *ddr,int yLine,float *dest); /*Read line from file.*/
void putFloatLine(FILE *f,const struct DDR *ddr,int yLine,const float *source); /*Write line to file.*/


/*Read a floating-point line from a LAS image file, converting if necessary.*/
void getFloatLine(FILE *f,const struct DDR *ddr,int yLine,float *dest)
{
	int x,maxX=ddr->ns;
	int dsize=ddr->dtype;
	if (ddr->dtype==3) dsize=4;
	fseek(f,yLine*maxX*dsize,0);
	if (ddr->dtype==4) 
	{
		if (maxX!=fread(dest,dsize,maxX,f)) 
		{
			printf("Fatal read error on image line %i!\n",yLine);
			exit(1);
		}
	} else {
		float *inputBuf=(float *)malloc(dsize*maxX);
		if (maxX!=fread(inputBuf,dsize,maxX,f)) 
		{
			printf("Fatal read error on image line %i!\n",yLine);
			exit(1);
		}
		if (ddr->dtype==1)
			for (x=0;x<maxX;x++)
				dest[x]=((unsigned char *)inputBuf)[x];
		else if (ddr->dtype==2)
			for (x=0;x<maxX;x++)
				dest[x]=((short *)inputBuf)[x];
		else if (ddr->dtype==3)
			for (x=0;x<maxX;x++)
				dest[x]=((long *)inputBuf)[x];
		free(inputBuf);
	}
}

/*Write a floating-point line to a LAS image file, converting if necessary.*/
void putFloatLine(FILE *f,const struct DDR *ddr,int yLine,const float *source)
{
	int x,maxX=ddr->ns;
	int dsize=ddr->dtype;
	if (ddr->dtype==3) dsize=4;
	fseek(f,yLine*maxX*dsize,0);
	if (ddr->dtype==4)
	{/*No conversion necessary for floats.*/
		if (maxX!=fwrite(source,dsize,maxX,f)) 
			printf("Write error on float image line %i!\n",yLine);
	} else {
		float *outputBuf=(float *)malloc(dsize*maxX);
		if (ddr->dtype==1)
			for (x=0;x<maxX;x++)
				((unsigned char *)outputBuf)[x]=source[x];
		else if (ddr->dtype==2)
			for (x=0;x<maxX;x++)
				((short *)outputBuf)[x]=source[x];
		else if (ddr->dtype==3)
			for (x=0;x<maxX;x++)
				((long *)outputBuf)[x]=source[x];
		if (maxX!=fwrite(outputBuf,dsize,maxX,f)) 
			printf("Write error on image line %i!\n",yLine);
		free(outputBuf);
	}
}
