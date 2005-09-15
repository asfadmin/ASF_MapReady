/*********************************************
Selection routines (sel.c)
These return the "current selection", which is composed of the 
full-resolution pixels of the current image included in the current
polygon, or all the image pixels if no polygon is selected.
*/
#include "main.h"
#include "image.h"

selection *selBounds(int *wid,int *ht)
{
	selection *s=(selection *)MALLOC(sizeof(selection));
	s->p=newPoly();
	if (s->p==NULL)
	{/*No polygon selected-- selection starts at image start*/
		s->startY=0;
		s->startX=0;
		*wid=s->wid=image.width;
		*ht=image.height;
	}
	else 
	{/*We have a polygon-- this forms the selection*/
		int minX,maxX,minY,maxY;
		rasterizePoly(s->p,image.height);
		polyBounds(s->p,&minX,&maxX,&minY,&maxY);
		s->startY=minY;
		s->startX=minX;
		*wid=s->wid=maxX-minX;
		*ht=maxY-minY;
	}
	s->inBuf=(float *)MALLOC(sizeof(float)*image.width);
	return (selection *)s;
}

/*Extract the given (selection-relative) line of full-resolution
selected pixels to the given array, filling in the non-selected parts
with backgroundFill*/
void selLine(selection *s,int colorBand,int yLine,float *dest,float backgroundFill)
{
	int scanNo,min,max;
	int x,y=yLine+s->startY;

/*Read line from appropriate band into input buffer*/
	image_readLine(s->inBuf,y,colorBand);

	if (s->p==NULL)
	{/*No currently selected polygon-- just copy over whole image line*/
		for (x=0;x<s->wid;x++)
			dest[x]=s->inBuf[x];
	} else {/*We have a polygon!*/
	/*Zero out destiation array*/
		for (x=0;x<s->wid;x++)
			dest[x]=backgroundFill;

	/*Copy each scan chunk from the input buffer to the destination*/
		scanNo=0;
		while (scanPoly(s->p,y,image.width,scanNo++,&min,&max))
			for (x=min;x<max;x++)
				dest[x-s->startX]=s->inBuf[x];/*Copy this part of data over*/
	}
}

void deleteSel(selection *s)
{
	if (s->p!=NULL)
		deletePoly(s->p);
	s->p=NULL;
	s->startY=s->startX=-1000;
	FREE(s->inBuf);
	FREE(s);
}
