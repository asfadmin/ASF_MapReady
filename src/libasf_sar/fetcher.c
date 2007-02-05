/*Fetcher.c:
	The way remap actually gets pixels out of its
input images.

This set of routines is based on the "blocking/reblocking"
idea from the Land Analysis System's geom1p image rectifier.
The idea is: we break the input image up into 256x256 blocks,
and each block is written to a contiguous chunk of a
temporary (blocking) file.

This way, if the image needs to be rotated by 90 degrees,
to generate a single (horizontal) output line, 
instead of having to read thousands of (non-contiguous)
*lines* from the input file, we only need to read a few
dozen (contiguous) *blocks* from our temporary file.
This scheme is even faster when we keep a block cache.

Example: *Without* using image blocks, 
>time remap big.img big_rot.img -rotate 45 -translate 3000 -near -width 8000 -height 8000
Remap input image: 'big.img'.  Output image: 'big_rot.img'.
        Input dimensions: 8192x7976 (LxS).
        Output dimensions: 8000x8000 (LxS).
        Input Pixel type/size: Unsigned Char pixels (1-byte unsigned).
        Output Pixel type/size: Unsigned Char pixels (1-byte unsigned).
        Sampling Type: Bilinear.

Took 23 minutes to remap 250 lines of data, when I got tired and killed it.
Extrapolating, it would probably take 736 minutes (11 hours) to remap all 8000 lines.

*Using* image blocks, the same task took
340.0u 44.0s 6:41 95% 0+0k 0+0io 0pf+0w
or just under 7 minutes, a 100x speed increase.
*/

#include "asf.h"
#include "asf_meta.h"
#include "matrix.h"
#include <stdio.h>
#include <unistd.h> /*For getpid()*/
#include "remap.h"

#define BLOCK_SHIFT 8 /*Log base 2 of size (in pixels) of one side of a block*/
#define PIX2BLOCK(pix) ((pix)>>BLOCK_SHIFT) /*Return the block number of the given pixel.*/
#define BLOCK_SIDE (1<<BLOCK_SHIFT) /*Size (in pixels) of one side of a block*/
#define BLOCK_MASK (BLOCK_SIDE-1) /*Bitwise mask for block indices*/
#define BLOCK_SIZE (sizeof(float)*BLOCK_SIDE*BLOCK_SIDE) /*Block size in bytes.*/

#define IMAGE_SLOP 4 /*# of input image widths to pad with zeros in the cache*/
#define CACHE_SLOP 20 /*# of extra cache blocks to allocate*/

static float backgroundFill=0;

typedef struct {
/*Original Image Info:*/
	meta_parameters *meta;/*Input image meta info*/
	FILE *inFile;/*File pointer to input image*/
	
/*Block Store (File containing image blocks).  Each block occupies
a contiguous BLOCK_SIZE space in this file.  The top-left image block
is written first, then to the blocks to its right, and so on down the image.
Note that the right- and bottom-edge blocks have to be zero-padded.
*/
	int blockWid;/*Width of blocking store file in blocks*/
	int blockHt;/*Height of blocking store file in blocks*/
	char blockName[255];/*Name of blocking store file*/
	FILE *blockIn;/*Input pointer to blocking store file*/

/*Block Cache:
The block cache is padded bigger (according to IMAGE_SLOP and CACHE_SLOP)
than the blocking store so it can cache a zero block for out-of-bounds
areas of the image.  This way, we almost never have to do output bounds
testing.
*/
	float **cache;/*left-right; then top-down list of block pointers.*/
	int startX,startY;/*Start of input image in block cache, in pixels*/
	int cacheWid,cacheHt;/*Width and height of block cache, in blocks*/
	int blocksInCache;/*Number of blocks currently stored in cache.*/
	int doomedBlock;/*Pointer to next block to be removed from the cache.*/
	float *zeroBlock;/*Pointer to a block containing all background pixels.*/

} fetchRec;/*Private pixel-fetching record, used below*/


void read_line(fetchRec *g,int y,float *destBuf,int destLen);
void createBlockingStore(fetchRec *g);
float *readBlock(fetchRec *g,int imgX,int imgY);
float *cacheMiss(fetchRec *g,int imgX,int imgY);

/************ Internal Utilities:*************/
/*Read_line: reads given line of the given input file,
padding with zeros out to destLen if needed.*/
void read_line(fetchRec *g,int y,float *destBuf,int destLen)
{
	int x;
	if (y>=g->meta->general->line_count)
	{/*We were asked for a line past the end of the image:*/
		for (x=0;x<destLen;x++)
			destBuf[x]=backgroundFill;
		return;/*Return a line of all background*/
	}
/*Otherwise, the line is in bounds.*/
	get_float_line(g->inFile, g->meta, y, destBuf);
	//getFloatLine_mb(g->inFile,g->inDDR,y,g->inBand,destBuf);
	
/*Pad to the end of the buffer with the background color*/
	for (x=g->meta->general->sample_count;x<destLen;x++)
		destBuf[x]=backgroundFill;
		
}
/* CreateBlockingStore: Decompose input image into blocks,
and write blocks to blocking store file.*/
void createBlockingStore(fetchRec *g)
{
	int blockX,blockY;/*Output block numbers, in X and Y*/
	float *inBuf;/*Image input buffer-- blockWid blocks*/
	FILE *outF;/*Blocking store file; output pointer*/
	
/*Create the blocking store file:*/
	sprintf(g->blockName,"remap_%d_%d.blk",(int)getpid(),0);
	//printf("   Creating image block file %s...\n",g->blockName);
	outF=fopen_tmp_file(g->blockName,"wb");
	
/*Allocate buffers for copy*/
	inBuf=(float *)MALLOC(g->blockWid*BLOCK_SIZE);
	
/*Copy data from input image to blocking store.  We do this by
reading BLOCK_SIDE lines into inBuf, then parsing out and writing
each block using outBuf.*/
	for (blockY=0;blockY<g->blockHt;blockY++)
	{
		/*Fetch corresponding lines of input file, padding with zeros if needed.*/
		int line;
		for (line=0;line<BLOCK_SIDE;line++)
			read_line(g,blockY*BLOCK_SIDE+line,
				&inBuf[line*(g->blockWid*BLOCK_SIDE)],g->blockWid*BLOCK_SIDE);
		/*Sort read-in lines into output blocks and write each out.*/
		for (blockX=0;blockX<g->blockWid;blockX++)
		{
			for (line=0;line<BLOCK_SIDE;line++)
				FWRITE(&inBuf[line*(g->blockWid*BLOCK_SIDE)+blockX*BLOCK_SIDE],
					1,sizeof(float)*BLOCK_SIDE,outF);
		}
		
	}
/*Close files and free up output buffers*/
	FCLOSE(outF);
	FREE(inBuf);

/*Open the freshly-created blocking store file for read.*/
	g->blockIn=fopen_tmp_file(g->blockName,"rb");
	//printf("   Done creating image block file\n");
}

/*Read in the block corresponding to to the given (in-bounds)
location (in image pixels), making room in memory if needed.*/
float *readBlock(fetchRec *g,int imgX,int imgY)
{
	float *retBlock=(float *)MALLOC(BLOCK_SIZE);
	int bx=PIX2BLOCK(imgX);
	int by=PIX2BLOCK(imgY);
	/*printf("Loading block at %d,%d: %d\n",imgX,imgY,by*g->blockWid+bx);*/
	FSEEK64(g->blockIn,(long long)BLOCK_SIZE*(by*g->blockWid+bx),0);
	FREAD(retBlock,1,BLOCK_SIZE,g->blockIn);
	
/*Now we make sure our pixel buffer isn't getting TOO big.*/
	g->blocksInCache++;
	while (g->blocksInCache*BLOCK_SIZE>30L*1024*1024) /*Keep no more than 30 MB of data in buffer.*/
	{
		/*start tossing blocks of data, starting from doomedBlock...*/
		float **doomed=&(g->cache[g->doomedBlock]);
		if ((*doomed!=NULL)&&(*doomed!=g->zeroBlock)) 
		{ /*...until we get under the limit.*/
			/*printf("...paging out block %i...\n",g->doomedBlock);*/
			FREE(*doomed);
			*doomed=NULL;
			g->blocksInCache--;
		}
		g->doomedBlock++;
		if (g->doomedBlock>=(g->cacheWid*g->cacheHt))
			g->doomedBlock=0;
	}
	return retBlock;
}

/*CacheMiss: return the block that should occupy the
location corresponding to this image point in the cache.
*/
float *cacheMiss(fetchRec *g,int imgX,int imgY)
{
	if ((imgX<0)||(imgY<0)||
		(imgX>=BLOCK_SIDE*g->blockWid)||(imgY>=BLOCK_SIDE*g->blockHt))
	/*Pixel is out of blocking store bounds-- add a zero block here*/
		return g->zeroBlock;
	else /*Pixel is in bounds-- read in the appropriate block*/
		return readBlock(g,imgX,imgY);
}


/************ External Entry Point:
set_background:
    Sets the value used when a remapped pixel falls outside the original image.*/
void set_background(float v)
{

    backgroundFill = v;
}

/************ External Entry Point:
CreateFetchRec:
	Creates &returns a "pixel fetch" record, to pass to fetchPixelValue.*/
pixelFetcher *createFetchRec(FILE *in,meta_parameters *meta)
{
	int i;
	fetchRec *g=(fetchRec *)MALLOC(sizeof(fetchRec));
/*Set input fields*/
	g->meta=meta;
	g->inFile=in;
	
/*Set Block Store fields*/
	/*Round *up* to nearest integral number of blocks*/
	g->blockWid=(g->meta->general->sample_count+BLOCK_SIDE-1)/BLOCK_SIDE;
	g->blockHt=(g->meta->general->line_count+BLOCK_SIDE-1)/BLOCK_SIDE;
	
/*Set Block Cache Fields*/
	g->cacheHt=2*(CACHE_SLOP+IMAGE_SLOP*g->blockHt);
	g->cacheWid=2*(CACHE_SLOP+IMAGE_SLOP*g->blockWid);
	g->startY=BLOCK_SIDE*(CACHE_SLOP+IMAGE_SLOP*g->blockHt);
	g->startX=BLOCK_SIDE*(CACHE_SLOP+IMAGE_SLOP*g->blockWid);
	g->cache=(float **)MALLOC(sizeof(float *)*g->cacheHt*g->cacheWid);
	for (i=0;i<g->cacheHt*g->cacheWid;i++)
		g->cache[i]=NULL;
	g->blocksInCache=0;
	g->doomedBlock=0;
	g->zeroBlock=(float *)MALLOC(BLOCK_SIZE);
	for (i=0;i<BLOCK_SIDE*BLOCK_SIDE;i++)
		g->zeroBlock[i]=backgroundFill;
	
/*Create and copy over the image data to the blocking store*/
	createBlockingStore(g);
	
	return (pixelFetcher *)g;
}

/************ External Entry Point:
KillFetchRec:
	De-allocates a pixel fetching record.*/
void killFetchRec(pixelFetcher *inGetRec)
{
	int i;
	fetchRec *g=(fetchRec *)inGetRec;
/*Free block cache*/
	for (i=0;i<g->cacheHt*g->cacheWid;i++)
		if ((g->cache[i]!=NULL)&&(g->cache[i]!=g->zeroBlock))
		{
			/*printf("Freeing cached block %d...\n",i);*/
			FREE(g->cache[i]);
		}
	FREE(g->cache);g->cache=NULL;
	FREE(g->zeroBlock);g->zeroBlock=NULL;
	
/*Free (& delete) block store*/
	FCLOSE(g->blockIn);
	unlink_tmp_file(g->blockName);
	
/*Free fetch rec*/
	FREE(g);g=NULL;
}

/************ External Entry Point:
Get a single pixel's value, interpreted as a floating point number.*/
float fetchPixelValue(pixelFetcher *inGetRec,int x, int y)
{
	register fetchRec *g=(fetchRec *)inGetRec;
	float **cache=g->cache;
	
	int bx=PIX2BLOCK(g->startX+x);
	int by=PIX2BLOCK(g->startY+y);
	int index=by*g->cacheWid+bx;
	if (cache[index]==NULL)
	/*The cache is empty for this location- refill it*/
		cache[index]=cacheMiss(g,x,y);
	return cache[index][((y&BLOCK_MASK)<<BLOCK_SHIFT)+(x&BLOCK_MASK)];
}
