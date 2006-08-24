#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

#define VERSION 1.3
#define MINI(a,b) (((a)<(b))?(a):(b))
#define MAXI(a,b) (((a)>(b))?(a):(b))

void trim(char *infile, char *outfile, long long startX, long long startY,
	  long long sizeX, long long sizeY)
{
  meta_parameters *metaIn, *metaOut;
  int isComplex;
  long long pixelSize, offset;
  long long x,y,inMaxX,inMaxY,lastReadY,firstReadX,numInX;
  FILE *in,*out;
  char *buffer;

  /* Check for complex data and open files */
  isComplex = (findExt(infile)&&(0==strcmp(findExt(infile),".cpx")));
  in = fopenImage(infile,"rb");
  out = fopenImage(outfile,"wb");

  metaIn = meta_read(infile);
  pixelSize = metaIn->general->data_type;
  if (pixelSize==3) pixelSize=4;
  if (pixelSize==5) pixelSize=8;
  if (isComplex)    pixelSize*=2;

  inMaxX = metaIn->general->sample_count;
  inMaxY = metaIn->general->line_count;

  if (sizeX < 0) sizeX = inMaxX - startX;
  if (sizeY < 0) sizeY = inMaxY - startY;

  /* Write out metadata */
  metaOut = meta_read(infile);
  metaOut->general->line_count = sizeY;
  metaOut->general->sample_count = sizeX;
  metaOut->general->start_line += startY * metaOut->sar->line_increment;
  metaOut->general->start_sample += startX * metaOut->sar->sample_increment;

  /* Some sort of conditional on the validity of the corner coordinates would 
     be nice here.*/
  if (metaIn->projection) {
    double bX, mX, bY, mY;
    bY = metaIn->projection->startY;
    bX = metaIn->projection->startX;
    mY = metaIn->projection->perY;
    mX = metaIn->projection->perX;
    metaOut->projection->startY = bY + mY * startY;
    metaOut->projection->startX = bX + mX * startX;
  }

  meta_write(metaOut, outfile);

  /* If everything's OK, then allocate a buffer big enough for one line of 
     output data.*/
  buffer= (char *)MALLOC(pixelSize*(sizeX));

  /* If necessary, fill the top of the output with zeros, by loading up a 
     buffer and writing.*/
  for (x=0;x<sizeX*pixelSize;x++)
    buffer[x]=0;
  for (y=0;y<-startY && y<sizeY;y++) {
    FWRITE(buffer,pixelSize,sizeX,out);
    if (y==0)
      asfPrintStatus("   Filling zeros at beginning of output image\n");
  }

  /* Do some calculations on where we should read.*/
  firstReadX=MAXI(0,-startX);
  numInX=MINI(MINI(sizeX,inMaxX-(firstReadX+startX)),sizeX-firstReadX);
  lastReadY=MINI(sizeY,inMaxY-startY);
  offset=0;

  for (;y<lastReadY;y++) {
    int inputY=y+startY,
      inputX=firstReadX+startX,
      outputX=firstReadX;
    
    offset=pixelSize*(inputY*inMaxX+inputX);
    
    if (y==lastReadY) asfPrintStatus("   Writing output image\n");
    
    FSEEK64(in,offset,SEEK_SET);
    
    FREAD(buffer+outputX*pixelSize,pixelSize,numInX,in);
    FWRITE(buffer,pixelSize,sizeX,out);
  }

  /* Reset buffer to zeros and fill remaining pixels.*/
  for (x=0;x<sizeX*pixelSize;x++)
    buffer[x]=0;
  for (;y<sizeY;y++) {
    FWRITE(buffer,pixelSize,sizeX,out);
    if (y==sizeY)
      asfPrintStatus("   Filled zeros after writing output image\n");
  }

  /* Now close the files & free memory 'cause we're done.*/
  FREE(buffer);
  FCLOSE(in);
  FCLOSE(out);

  meta_free(metaIn);
  meta_free(metaOut);
}

void trim_zeros(char *infile, char *outfile, int * startX, int * endX)
{
  meta_parameters *metaIn;
  FILE *in;
  int i,nl,ns;
  float *buf;

  in = fopenImage(infile,"rb");
  metaIn = meta_read(infile);
  ns = metaIn->general->sample_count;
  nl = metaIn->general->line_count;
  
  *startX = ns-1;
  *endX = 0;

  buf = (float*)MALLOC(sizeof(float)*ns);
  for (i=0; i<nl; ++i) {
      int left = 0, right = ns-1;
      get_float_line(in, metaIn, i, buf);
      while (buf[left] == 0.0) ++left;
      while (buf[right] == 0.0) --right;
      if (left < *startX) *startX = left;
      if (right > *endX) *endX = right;
  }

  *endX -= *startX;

  free(buf);
  meta_free(metaIn);

  trim(infile, outfile, *startX, 0, *endX, nl);
}
