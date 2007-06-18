#include "asf.h"
#include "fft.h"
#include "fft2d.h"

/* complex number def'n */
typedef struct {
        float r;
        float i;
} complex;

/************************
Filter sizes:
        These are expressed as powers of 2--
        dMx=3 -> filter 8-pixel chunk; write 4-pixel chunk;
        dMx=4 -> filter 16-pixel chunk; write 8-pixel chunk;
        dMx=5 -> filter 32-pixel chunk; write 16-pixel chunk;
        dMx=6 -> filter 64-pixel chunk; write 32-pixel chunk;
        dMx=7 -> filter 128-pixel chunk; write 64-pixel chunk;
*/

#define dMx 5
#define dMy 5

/*Size of filtered chunk.*/
#define dx (1<<dMx)
#define dy (1<<dMy)

/*Size of output chunk (requires 4 filtered chunks).*/
#define ox (1<<(dMx-1))
#define oy (1<<(dMy-1))

void blendData(complex **chunks,complex **last_chunks,
        float *weight,float *outBuf);

int nl,ns;

/***********************************************
blendHoriz, blendVert:
        These blend the complex numbers stored in
the given arrays either vertically or horizontally by
the given weighting array.

left,right,top, and bottom are [dx,dy] complex input arrays.
weight is [ox,oy] float input weighting array.
outBuf is [ns,oy] float output phase array.
*/

void blendHoriz(complex *left, complex *right, float *weight, float *outBuf)
{
  register int x,y;
  register float blend_r,blend_i;
  register float *out,*wL,*wR;
  register complex *cL,*cR;
  for (y=0; y<oy; y++) {
    cL = &left [y*dx];
    cR = &right[y*dx];
    
    wL = &weight[(oy-1)*ox+ox-1];
    wR = &weight[(oy-1)*ox+0   ];
    
    out = &outBuf[y*ns];
    for (x=0; x<ox; x++) {
      blend_r = (*wL  ) * cL->r + (*wR  ) * cR->r;
      blend_i = (*wL--) * cL->i + (*wR++) * cR->i;
      *out++ = atan2(blend_i,blend_r);
      cL++,cR++;
    }
  }
}

void blendVert(complex *top, complex *bottom, float *weight, float *outBuf)
{
  register int x,y;
  register float blend_r,blend_i;
  register float *out,*wT,*wB;
  register complex *cT,*cB;
  for (y=0; y<oy; y++) {
    cT = &top   [(y+oy)*dx];
    cB = &bottom[     y*dx];
    
    wT = &weight[(oy-1-y)*ox +ox-1];
    wB = &weight[       y*ox +ox-1];
    
    out = &outBuf[y*ns];
    for (x=0; x<ox; x++) {
      blend_r = (*wT) * cT->r + (*wB) * cB->r;
      blend_i = (*wT) * cT->i + (*wB) * cB->i;
      *out++ = atan2(blend_i,blend_r);
      cT++,cB++;
    }
  }
}

/*******************************************************
blendData:
        A routine internal to las_filter.  Given two rows
of chunks of data, blendData weights the two, converts
to polar form, and fills the given output buffer.

If last_chunks is NULL, this is the first line in the file.
If chunks is NULL, this is the last line in the file.

Chunks,last_chunks: a [ns/ox] array of [dx,dy] complex chunks.
weight: a [ox,oy] array of bilinear blending weights.  Strongest weight is at 0,0.
outBuf: a [ns,oy] array of output floats.
*/
void blendData(complex **chunks,complex **last_chunks,
	       float *weight,float *outBuf)
{
  int nChunkX=ns/ox-1;
  int chunkX,x,y;
  register float *wTL,*wTR,
    *wBL,*wBR;
  register complex *cTL,*cTR,
    *cBL,*cBR;
  register float *out,blend_r,blend_i;
  /*Figure out the possibility we're dealing with:*/
  if (chunks!=NULL && last_chunks!=NULL) {
    /*Left border:top-to-bottom 2-way blend*/
    chunkX=0;
    blendVert(&last_chunks[chunkX][0], &chunks[chunkX][0],
	      weight, &outBuf[chunkX*ox]);
    
    /*Interior lines: 4-way blend.*/
    /*Blending:
      last_chunks[chunkX-1] last_chunks[chunkX]
      chunks[chunkX-1] chunks[chunkX]
    */
    for (chunkX=1; chunkX<nChunkX; chunkX++)
      for (y=0; y<oy; y++) {
	cTL = &last_chunks[chunkX-1][(y+oy)*dx    +ox];
	cTR = &last_chunks[chunkX]  [(y+oy)*dx    +0];
	cBL = &chunks[chunkX-1][y*dx+ox];
	cBR = &chunks[chunkX]  [y*dx+0];
	
	wTL = &weight[(oy-1-y)*ox +ox-1];
	wTR = &weight[(oy-1-y)*ox +0];
	wBL = &weight[y*ox+ox-1];
	wBR = &weight[y*ox+0];
	
	out = &outBuf[y*ns+chunkX*ox];
	for (x=0; x<ox; x++) {
	  blend_r = (*wTL  ) * cTL->r + (*wTR  ) * cTR->r +
	    (*wBL  ) * cBL->r + (*wBR  ) * cBR->r;
	  blend_i = (*wTL--) * cTL->i + (*wTR++) * cTR->i +
	    (*wBL--) * cBL->i + (*wBR++) * cBR->i;
	  *out++ = atan2(blend_i,blend_r);
	  cTL++,cTR++,cBL++,cBR++;
	}
      }
    
    /*Right border: top-to-bottom 2-way blend*/
    blendVert(&last_chunks[chunkX-1][ox], &chunks[chunkX-1][ox],
	      weight, &outBuf[chunkX*ox]);
  } 
  else {
    
    if (chunks != NULL)
      /*Top Border: left-to-right 2-way blend*/
      for (chunkX=1; chunkX<nChunkX; chunkX++)
	blendHoriz(&chunks[chunkX-1][ox], &chunks[chunkX][0],
		   weight, &outBuf[chunkX*ox]);
    
    else if (last_chunks != NULL)
      /*Bottom Border: left-to-right 2-way blend*/
      for (chunkX=1; chunkX<nChunkX; chunkX++)
	blendHoriz(&last_chunks[chunkX-1][oy*dx+ox], &last_chunks[chunkX][oy*dx+0],
		   weight, &outBuf[chunkX*ox]);
    
    /*Now just copy over image corners.*/
    for (y=0; y<oy; y++) {
      if (chunks != NULL)/*Top line*/
	cBL = &chunks[chunkX-1][y*dx+ox],
	  cBR = &chunks[0]  [y*dx+0];
      else /*Bottom line.*/
	cBL = &last_chunks[chunkX-1][(y+oy)*dx    +ox],
	  cBR = &last_chunks[0]  [(y+oy)*dx    +0];
      out = &outBuf[y*ns+0*ox];
      for (x=0; x<ox; x++) {
	*out++ = atan2(cBR->i, cBR->r);
	cBR++;
      }
      out = &outBuf[y*ns+nChunkX*ox];
      for (x=0; x<ox; x++) {
	*out++ = atan2(cBL->i, cBL->r);
	cBL++;
      }
    }  
  }
}

void image_filter(FILE *in, meta_parameters *meta,
		  FILE *out, double strength);

int phase_filter(char *inFile, double strength, char *outFile)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta;

  /*Open input files.*/
  fpIn = fopenImage(inFile, "rb");
  fpOut = fopenImage(outFile, "wb");
  meta = meta_read(inFile);
  
  /*Round up to find image size which is an even number of output chunks..*/
  ns = (meta->general->sample_count+ox-1)/ox*ox;
  nl = (meta->general->line_count+oy-1)/oy*oy;
  asfPrintStatus("Output Size: %d samples by %d lines\n\n", ns, nl);
  
  /*Set up output file.*/
  meta_write(meta, outFile);
  
  /*Perform the filtering, write out.*/
  fft2dInit(dMy, dMx);
  image_filter(fpIn, meta, fpOut, strength);
  
  return (0);
}

/**************************************************
 read_image: reads the image file given by in & ddr
into the (delX x delY) float array dest.  Reads pixels 
into topleft corner of dest, starting
at (startY , startX) in the input file.
*/
void read_image(FILE *in,meta_parameters *meta, float *dest, 
		int startX,int startY,int delX,int delY)
{
  register int x,y,l;
  int stopY=delY, stopX=delX;
  if ((stopY+startY) > meta->general->line_count)
    stopY = meta->general->line_count - startY;
  if ((stopX+startX) > meta->general->sample_count)
    stopX = meta->general->sample_count - startX;
  /*Read portion of input image into topleft of dest array.*/
  for (y=0; y<stopY; y++) {
    l = ns*y;
    get_float_line(in, meta, startY+y, &dest[l]);
    for (x=stopX; x<delX; x++)
      dest[l+x] = 0.0; /*Fill rest of line with zeros.*/
  }
  /*Fill remainder of array (bottom portion) with zero lines.*/
  for (y=stopY; y<delY; y++) {
    l = ns*y;
    for (x=0; x<ns; x++)
      dest[l+x] = 0.0; /*Fill rest of in2 with zeros.*/
  }
}

/*******************************************
Phase_filter:
	Performs goldstein phase filtering on the given
dx x dy buffer of complex data.

Goldstein phase filtering consists of:
fft; exponentiate amplitude of fft'd data; ifft
Where the exponential applied (strength) is typically
between 1.2 and 1.7.  Larger strengths filter noise
better, but eliminate more good information, too.
Huge scalings, like 2.0 or 3.0, result in very geometric-
looking phase.
*/

void phase_filter_func(complex *buf,float strength)
{
  register int x,y;
  
  /*We must adjust the scaling for two reasons:
    -The complex buffer is not normalized (strength-1)
    -We operate on the square of the amplitude (/2)
    Hence,
    fft*=pow(fft.r^2+fft.i^2,adjScale);
    is equivalent to
    amp=sqrt(fft.r^2+fft.i^2);fft/=amp;fft*=pow(amp,strength);
  */
  float adjStrength=(strength-1)/2;
  
  /*fft buf*/
  fft2d((float *)buf,dMy,dMx);	
  
  /*Manipulate power spectrum.*/
  for (y=0; y<dy; y++) {
    register complex *fft = &buf[y*dx];
    for (x=0; x<dx; x++) {
      float mul = pow(fft->r*fft->r+fft->i*fft->i, adjStrength);
      fft->r *= mul;
      fft->i *= mul;
      fft++;
    }
  }
	
  /*ifft buf*/
  ifft2d((float *)buf, dMy, dMx);
}

/************************************************************
image_filter: 
	Applies the goldstein phase filter across an entire
image, and writes the result to another image.

	The goldstein phase filter works best when applied
to little pieces of the image.  But processing the image as
a bunch of little pieces results in a segmented phase image.
Hence we do a bilinear weighting of 4 overlapping filters 
to "feather" the edges.
*/

void image_filter(FILE *in, meta_parameters *meta,
		  FILE *out, double strength)
{
  int chunkX,chunkY,nChunkX,nChunkY;
  int i,x,y;
  float *inBuf, *outBuf, *weight;
  complex **chunks, **last_chunks=NULL;
  
  /*Allocate polar to complex conversion array*/
#define NUM_PHASE 512
#define phase2cpx(ph) p2c[(int)((ph)*polarCvrt)&(NUM_PHASE-1)]
  complex *p2c;
  float polarCvrt = NUM_PHASE/(2*PI);
  p2c = (complex *) MALLOC(sizeof(complex)*NUM_PHASE);
  for (i=0; i<NUM_PHASE; i++) {
      float phase = i*2*PI/NUM_PHASE;
      p2c[i].r = cos(phase);
      p2c[i].i = sin(phase);
  }
  
  /*Allocate bilinear weighting array.*/
  weight = (float *)MALLOC(sizeof(float)*ox*oy);
  for (y=0; y<oy; y++)
    for (x=0;x<ox;x++)
      weight[y*ox+x] = (float)x/(ox-1)*(float)y/(oy-1);
  
  /*Allocate storage arrays.*/
  nChunkX = ns/ox-1;
  nChunkY = nl/oy-1;
  outBuf = inBuf = (float *)MALLOC(sizeof(float)*ns*dy);
#define newChunkArray(name) name=(complex **)MALLOC(sizeof(complex **)*nChunkX); \
			for (chunkX=0;chunkX<nChunkX;chunkX++) \
				name[chunkX]=(complex *)MALLOC(sizeof(complex)*dx*dy);
  newChunkArray(chunks);
  
  /*Loop across each chunk in file.
    printf("Filtering phase in %d x %d blocks...\n",dx,dy);
    printf("Output phase in %d x %d blocks...\n",ox,oy);*/
  for (chunkY=0; chunkY<nChunkY; chunkY++) {

    asfLineMeter(chunkY, nChunkY);
    
    /*Read next chunk of input.*/
    read_image(in, meta, inBuf, 0, chunkY*ox, ns, dy);
    
    /*Convert polar image to complex chunk.*/
    for (chunkX=0; chunkX<nChunkX; chunkX++) {
	register float *in;
	register complex *out;
	for (y=0; y<dy; y++) {
	  in = &inBuf[y*ns+chunkX*ox];
	  out = &chunks[chunkX][y*dx];
	  for (x=0;x<dx;x++)
	    *out++=phase2cpx(*in++);
	}
    }
    
    /*Filter each newly-read chunk.*/
    for (chunkX=0; chunkX<nChunkX; chunkX++)
      phase_filter_func(chunks[chunkX], strength);
	
    /*Blend and write out filtered data.*/
    blendData(chunks, last_chunks, weight, outBuf);
    for (y=0; y<oy; y++) {
      if (chunkY*oy+y < meta->general->line_count)
	put_float_line(out, meta, chunkY*oy+y, &outBuf[y*ns]);
    }
		
    /*Swap chunks and last_chunks.*/
    {complex **tmp=last_chunks;last_chunks=chunks;chunks=tmp;}
    if (chunks == NULL) { newChunkArray(chunks); }
  }
  
  /*Write very last line of phase.*/
  blendData(NULL, last_chunks, weight, outBuf);
  for (y=0; y<oy; y++)
    if (chunkY*oy+y < meta->general->line_count)
      put_float_line(out, meta, chunkY*oy+y, &outBuf[y*ns]);
  
}

int zeroify(char *inFile, char *testFile, char *outFile)
{
  float *buf, *testbuf;
  FILE *fpOut, *fpIn, *fpTest;
  meta_parameters *metaIn, *metaTest;
  int x, y;
  int line_count, sample_count;
    
  /*Open output files.*/
  fpIn = fopenImage(inFile, "rb");
  fpTest = fopenImage(testFile, "rb");
  fpOut = fopenImage(outFile, "wb");
  
  /*Read and copy over DDR.*/
  metaIn = meta_read(inFile);
  metaTest = meta_read(testFile);
  meta_write(metaIn, outFile);
  
  /*Allocate buffers.*/
  buf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
  testbuf = (float *) MALLOC(sizeof(float)*metaTest->general->sample_count);
  
  /*Copy over each line of input to the output.*/
  line_count = metaIn->general->line_count;
  sample_count = metaIn->general->sample_count;
  for (y=0; y<line_count; y++) {
    get_float_line(fpIn, metaIn, y, buf);
    get_float_line(fpTest, metaTest, y, testbuf);
    for (x=0; x<sample_count; x++)
      if (testbuf[x] == 0)
	buf[x] = 0;
    put_float_line(fpOut, metaIn, y ,buf);
  }

  return 0;
}
