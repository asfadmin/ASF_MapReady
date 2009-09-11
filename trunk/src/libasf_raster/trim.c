#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

#define VERSION 1.3
#define MINI(a,b) (((a)<(b))?(a):(b))
#define MAXI(a,b) (((a)>(b))?(a):(b))

int trim(char *infile, char *outfile,
         long long startX, long long startY,
         long long sizeX, long long sizeY)
{
  meta_parameters *metaIn, *metaOut;
  long long pixelSize, offset;
  long long b,x,y,lastReadY,firstReadX,numInX;
  FILE *in,*out;
  char *buffer;

  // Check the pixel size
  metaIn = meta_read(infile);
  pixelSize = metaIn->general->data_type;
  if (pixelSize==3) pixelSize=4;         // INTEGER32
  else if (pixelSize==5) pixelSize=8;    // REAL64
  else if (pixelSize==6) pixelSize=2;    // COMPLEX_BYTE
  else if (pixelSize==7) pixelSize=4;    // COMPLEX_INTEGER16
  else if (pixelSize==8) pixelSize=8;    // COMPLEX_INTEGER32
  else if (pixelSize==9) pixelSize=8;    // COMPLEX_REAL32
  else if (pixelSize==10) pixelSize=16;  // COMPLEX_REAL64

  const int inMaxX = metaIn->general->sample_count;
  const int inMaxY = metaIn->general->line_count;

  if (sizeX < 0) sizeX = inMaxX - startX;
  if (sizeY < 0) sizeY = inMaxY - startY;

  /* Write out metadata */
  metaOut = meta_read(infile);
  metaOut->general->line_count = sizeY;
  metaOut->general->sample_count = sizeX;
  if (metaOut->sar) {
    if (!meta_is_valid_double(metaOut->sar->line_increment))
        metaOut->sar->line_increment = 1;
    if (!meta_is_valid_double(metaOut->sar->sample_increment))
        metaOut->sar->sample_increment = 1;
    metaOut->general->start_line += startY *
        metaOut->sar->line_increment * metaOut->general->line_scaling;
    metaOut->general->start_sample += startX *
         metaOut->sar->sample_increment * metaOut->general->sample_scaling;
  }
  else {
    metaOut->general->start_line +=
        startY * metaOut->general->line_scaling;
    metaOut->general->start_sample +=
        startX * metaOut->general->sample_scaling;
  }

  /* Some sort of conditional on the validity of the corner coordinates would 
     be nice here.*/
  if (metaIn->projection) {
    double bX, mX, bY, mY;
    bY = metaIn->projection->startY;
    bX = metaIn->projection->startX;
    mY = metaIn->projection->perY;
    mX = metaIn->projection->perX;
  }

  meta_write(metaOut, outfile);

  /* If everything's OK, then allocate a buffer big enough for one line of 
     output data.*/
  buffer= (char *)MALLOC(pixelSize*(sizeX));

  for (b=0; b<metaIn->general->band_count; ++b) {
    /* Open files */
    in = fopenImage(infile,"rb");
    out = fopenImage(outfile, b>0 ? "ab" : "wb");

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
      
      offset=pixelSize*(inputY*inMaxX+inputX) + b*inMaxX*inMaxY*pixelSize;
      
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

    FCLOSE(in);
    FCLOSE(out);
  }

  /* We're done.*/
  FREE(buffer);

  meta_free(metaIn);
  meta_free(metaOut);

  return 0;
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
      while (buf[left] == 0.0 && left<ns-1) ++left;
      while (buf[right] == 0.0 && right>0) --right;
      if (left < *startX) *startX = left;
      if (right > *endX) *endX = right;
  }

  *endX -= *startX;

  fclose(in);
  free(buf);
  meta_free(metaIn);

  trim(infile, outfile, *startX, 0, *endX, nl);
}

void trim_zeros_ext(char *infile, char *outfile, int update_meta,
                    int do_top, int do_left)
{
  int i,j;

  asfPrintStatus("trim zeros: %s -> %s\n", infile, outfile);

  FILE *in = fopenImage(infile,"rb");
  meta_parameters *metaIn = meta_read(infile);
  int ns = metaIn->general->sample_count;
  int nl = metaIn->general->line_count;
  
  int startX = ns-1;
  int endX = 0;
  int startY = -99;
  int endY = -99;

  asfPrintStatus("Scanning for zero fill...\n");
  float *buf = MALLOC(sizeof(float)*ns);
  for (i=0; i<nl; ++i) {
      get_float_line(in, metaIn, i, buf);

      // check left/right fill
      int left = 0, right = ns-1;
      while (buf[left] == 0.0 && left<ns-1) ++left;
      while (buf[right] == 0.0 && right>0) --right;
      if (left < startX) startX = left;
      if (right > endX) endX = right;

      // check top/bottom fill
      int all_zero=TRUE;
      for (j=0; j<ns; ++j) {
        if (buf[j] != 0.0) {
          all_zero=FALSE;
          break;
        }
      }
      if (!all_zero) {
        if (startY < 0) startY = i;
        endY = i;
      }
  }

  fclose(in);
  free(buf);

  int width, height, start_sample, start_line;

  if (do_top) {
    height = endY - startY;
    start_line = startY;
    asfPrintStatus("Removing %d pixels of top fill.\n", startY);
    asfPrintStatus("Removing %d pixels of bottom fill.\n", nl-endY);
  }
  else {
    height = nl;
    start_line = 0;
  }

  if (do_left) {
    width = endX - startX;
    start_sample = startX;
    asfPrintStatus("Removing %d pixels of left fill.\n", startX);
    asfPrintStatus("Removing %d pixels of right fill.\n", ns-endX);
  }
  else {
    width = ns;
    start_sample = 0;
  }

  if (!do_top && !do_left) {
    asfPrintStatus("No fill to be removed.\n");
  }
  else if (height == nl && width == ns && 
           start_line == 0 && start_sample == 0) {
    asfPrintStatus("No zero fill found.\n");
  }

  // now, actually trim the zerofill
  asfPrintStatus("Output start L/S: %d,%d\n"
                 "Output width/height: %5d,%5d\n"
                 "        (Input was): %5d,%5d\n"
                 "Trimming...\n",
                 start_line, start_sample, width, height, ns, nl);

  trim(infile, outfile, start_sample, start_line, width, height);

  if (!update_meta) {
    // write old metadata as new metadata, only updating line/sample counts
    // this overwrites what was created by trim()
    metaIn->general->line_count = height;
    metaIn->general->sample_count = width;
    meta_write(metaIn, outfile);

    asfPrintStatus("Did not update metadata "
                   "(aside from new line/sample counts).\n");
  }

  meta_free(metaIn);
}
