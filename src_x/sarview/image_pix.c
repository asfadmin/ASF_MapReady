/*SARview image_pix.c:
The image pixel-oriented routines, used to extract
a 3-byte scaled window from the image.
Currently, LAS and CEOS images are supported.
*/
#include "main.h"
#include "image.h"

void zoomScaleLine(int *dest,/*Output pixel array*/
	float *zoomed,float *inBuf,/*Temporary storage arrays*/
	int preZoomed,int lineY,int startX,double zoom,int width);

/*Render the current image to the given Tcl/Tk photo name,
starting at (startX,startY) in the current image,
zooming it out by a factor of zoom, writing a total 
image which is width by height.*/
void image_drawToPhoto(char *imgName,
		int startX,int startY,double zoom, int width,int height)
{
	const int nLines=32;/*Number of lines of data to pass to Tk at once*/
	int blockNo;
	Tk_PhotoHandle hPhoto=Tk_FindPhoto(interp,imgName);

/*We set the "rgb plane offsets" differently depending on
whether the machine is big or little endian.  
Then we can always access the:
red component of a pixel x with (x>>16)&0xFF,
green component with (x>>8)&0xFF, and
blue component with (x)&0xFF.*/
#if defined(lil_endian)
	Tk_PhotoImageBlock block={NULL,-1,-1,-1,-1, {2,1,0}};
#elif defined(big_endian)
	Tk_PhotoImageBlock block={NULL,-1,-1,-1,-1, {sizeof(int)-3,sizeof(int)-2,sizeof(int)-1}};
#else
#   error "You must define either lil_endian or big_endian!"
#endif

	Tk_PhotoImageBlock *blockPtr=&block;
	int *pixelPtr=(int *)MALLOC(sizeof(int)*nLines*width);
	float *zoomed=(float *)MALLOC(sizeof(float)*width);
	float *inBuf=(float *)MALLOC(sizeof(float)*image.width);

	/*We cache the small brightness/contrast sample image,
	for better speed.*/
	int useCache=0;
	int readFromCache=0;
	static float *cached_zoom=NULL;

	if (0==strcmp(imgName,"img_bcpreview"))
	{/*This is a cachable image--fill or read from the cache*/
		useCache=1;
		if (cached_zoom==NULL)
			cached_zoom=(float *)MALLOC(sizeof(float)*width*height);
		else 
			readFromCache=1;
	} else /*This is not a cachable image--purge the cache*/
		if (cached_zoom!=NULL)
		{/*Blow away the old cached image*/
			FREE(cached_zoom);
			cached_zoom=NULL;
		}
	
	/*Tell TCL about the block of pixels we'll be feeding it*/
	blockPtr->pixelPtr=(unsigned char *)pixelPtr;
	blockPtr->width=width;
	blockPtr->height=nLines;
	blockPtr->pitch=sizeof(int)*width;
	blockPtr->pixelSize=sizeof(int);

	Tk_PhotoSetSize(hPhoto,width,height);

	/*Feed TCL the image in blocks of nLines*/
	for (blockNo=0;blockNo<=height/nLines;blockNo++)
	{
		int yCount;
		double y=startY+blockNo*nLines*zoom;

		int copyLines=nLines;/*Number of lines to actually copy (at most, nLines)*/
		if (blockNo*nLines+copyLines>height)
			blockPtr->height=copyLines=height-blockNo*nLines;/*Shorten last block as needed*/

		/*Read in and scale the lines*/
		for (yCount=0;yCount<copyLines;yCount++)
		{
			zoomScaleLine(&pixelPtr[yCount*width],
				useCache?(&cached_zoom[width*(blockNo*nLines+yCount)]):zoomed,
				inBuf,readFromCache,(int)(y+0.5),startX,zoom,width);
			y+=zoom;
		}
		/*Pass the block of lines to TCL*/
		Tk_PhotoPutBlock(hPhoto, blockPtr, 0,blockNo*nLines, width, copyLines);
	}

	FREE(inBuf);
	FREE(zoomed);
	FREE(pixelPtr);
}


/*Private routine, called by zoomScaleLine:
Downsize the given line by the given zoom factor.
The output will start at startX, and continue
for width zoom-ed out pixels.*/
void zoomLine(float *dest,const float *inBuf,
		int startX,double zoom,int width)
{
	if (zoom<1.5)/*Small zoom-out, so we can just subsample*/
	{
		register int outX;
		register double inX=startX+0.5;/*0.5 is sub-pixel correction*/
		for (outX=0;outX<width;outX++)
		{
			dest[outX]=inBuf[(int)(inX)];
			inX+=zoom;
		}
	} else /*zoom>=1.5, so we should *average* the line down*/
	{
		register int outX;
		register double inX=startX;
		register int last_inX=(int)(inX-zoom);
		if (last_inX<0) last_inX=0;/*Do a left-bounds check*/
		for (outX=0;outX<width;outX++)
		{
			register int next_inX=(int)inX,x;
			/*Sum across all the input pixels that 
			this output pixel represents, and average.*/
			register double sum=0;
			for (x=last_inX;x<next_inX;x++)
				sum+=inBuf[x];
			dest[outX]=(float)(sum/(next_inX-last_inX));
			last_inX=next_inX;
			inX+=zoom;
		}
	}
}

/*Draw floating-point values into destination buffer
as greyscale.
*/
void drawFloatsToGrey(float *zoomed,int *dest,int width)
{
	int x;
	static int *greyTable=NULL;
	if (greyTable==NULL)
	{/*Need to initialize byte-indexed table of RGB greyscale values*/
		int i;
		greyTable=(int *)MALLOC(sizeof(int)*256);
		for (i=0;i<256;i++)
		/*Set red, green, and blue components separately, but equally*/
			greyTable[i]=(i<<16)|(i<<8)|(i);
	}

/*Copy greyscale pixels into buffer, scaling if necessary*/
	if (image.slope==1.0&&image.offset==0.0)
		/*No scaling necessary*/
		for (x=0;x<width;x++)
			dest[x]=greyTable[(unsigned char)zoomed[x]];
	else /*Scaling (& possibly clipping) necessary*/
		for (x=0;x<width;x++)
		{
			int pix=scale_toByte(zoomed[x]);/*scale_toByte is a macro in tk_interface.h*/
			if (pix>255) pix=255;
			if (pix<0) pix=0;
			dest[x]=greyTable[pix];
		}
}
/*Draw floating-point values into destination buffer
as a single color.
*/
void drawFloatsToColor(float *zoomed,int *dest,int width,int color)
{
	int x;
	int bitShift=8*(2-color);
/*Copy pixels into buffer, scaling if necessary*/
	if (image.slope==1.0&&image.offset==0.0)
		/*No scaling necessary*/
		for (x=0;x<width;x++)
			dest[x]|=((int)zoomed[x])<<bitShift;
	else /*Scaling (& possibly clipping) necessary*/
		for (x=0;x<width;x++)
		{
			int pix=scale_toByte(zoomed[x]);/*scale_toByte is a macro in tk_interface.h*/
			if (pix>255) pix=255;
			if (pix<0) pix=0;
			dest[x]|=pix<<bitShift;
		}
}

/*Private routine: zoomScaleLine
Extract the given line from the current image, 
left edge at (startX,lineY), zoomed out by a factor of zoom,
writing a total of width pixels to dest.
If preZoomed is true, the data has previously been put into "zoomed".
*/
void zoomScaleLine(int *dest,/*Output pixel array*/
	float *zoomed,float *inBuf,/*Temporary storage arrays*/
	int preZoomed,int lineY,int startX,double zoom,int width)
{
	int x;

#define FALSIFY_IMAGE 0
#if FALSIFY_IMAGE /*For testing, generate a faked (mathematical) image*/
	for (x=0;x<width;x++)
		dest[x]=greyTable[(unsigned char)(lineY*x)];
#else/*Normally, copy actual image data*/
	if ((lineY<0)||(lineY>=image.height))
	/*Line is out-of-bounds-- return black (all zeros)*/
		for (x=0;x<width;x++)
			dest[x]=0;
	else 
	{/*Line is in-bounds-- read from file & zoom*/
		if (ddr->nbands==1)
		{/*Image is greyscale-- draw to buffer as greyscale*/
			if (!preZoomed)
			{
				image_readLine(inBuf,lineY,0);
				zoomLine(zoomed,inBuf,startX,zoom,width);
			}
			drawFloatsToGrey(zoomed,dest,width);
		} else {/*Image is color-- add each band to buffer*/
			int color;
			for (x=0;x<width;x++)/*Zero out pixels before we add bands into the buffer*/
				dest[x]=0;
			for (color=0;color<ddr->nbands;color++)
			{
				image_readLine(inBuf,lineY,color);
				zoomLine(zoomed,inBuf,startX,zoom,width);
				drawFloatsToColor(zoomed,dest,width,color);
			}
		}
	}
#endif
}
