#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "meta_project.h"

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
     be nice here.

  if (metaIn->projection) {
    double bX, mX, bY, mY;
    bY = metaIn->projection->startY;
    bX = metaIn->projection->startX;
    mY = metaIn->projection->perY;
    mX = metaIn->projection->perX;
  }
  */
	meta_get_corner_coords(metaOut);
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

void subset_by_map(char *infile, char *outfile, double minX, double maxX,
  double minY, double maxY) 
{
  meta_parameters *meta = meta_read(infile);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  int nCoords = 4;
  long long startX, startY, sizeX, sizeY;
  double line, sample, height;
  double minSample = ns, maxSample = 0.0, minLine = nl, maxLine = 0.0;
  double minLat = 90.0, maxLat = -90.0, minLon = 180.0, maxLon = -180.0;
  double *lat = (double *) MALLOC(sizeof(double)*nCoords);
  double *lon = (double *) MALLOC(sizeof(double)*nCoords);
  proj_to_latlon(meta->projection, minX, minY, 0.0, &lat[0], &lon[0], &height);
  proj_to_latlon(meta->projection, minX, maxY, 0.0, &lat[1], &lon[1], &height);
  proj_to_latlon(meta->projection, maxX, minY, 0.0, &lat[2], &lon[2], &height);
  proj_to_latlon(meta->projection, maxX, maxY, 0.0, &lat[3], &lon[3], &height);
  int dateline = crosses_dateline(lon, 0, nCoords);
  int ii;
  for (ii=0; ii<nCoords; ii++) {
    if (dateline && lon[ii] < 0)
      lon[ii] += 360.0;
    if (lat[ii] > maxLat)
      maxLat = lat[ii];
    if (lat[ii] < minLat)
      minLat = lat[ii];
    if (lon[ii] > maxLon)
      maxLon = lon[ii];
    if (lon[ii] < minLon)
      minLon = lon[ii];
  }
  meta_get_lineSamp(meta, minLat, minLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  meta_get_lineSamp(meta, minLat, maxLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  meta_get_lineSamp(meta, maxLat, minLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  meta_get_lineSamp(meta, maxLat, maxLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  startX = (int) (minSample + 0.5);
  startY = (int) (minLine + 0.5);
  sizeX = (int) (maxSample - minSample);
  sizeY = (int) (maxLine - minLine);
  trim(infile, outfile, startX, startY, sizeX, sizeY);
  meta_free(meta); 
}

void subset_by_latlon(char *infile, char *outfile, double *lat, double *lon, 
  int nCoords) 
{
  meta_parameters *meta = meta_read(infile);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  long long startX, startY, sizeX, sizeY;
  double line, sample;
  double minSample = ns, maxSample = 0.0, minLine = nl, maxLine = 0.0;
  double minLat = 90.0, maxLat = -90.0, minLon = 180.0, maxLon = -180.0;
  int ii;

  int dateline = crosses_dateline(lon, 0, nCoords);
  asfPrintStatus("Subset does %scross the dateline\n", dateline ? "" : "not ");
  for (ii=0; ii<nCoords; ii++) {
    if (dateline && lon[ii] < 0)
      lon[ii] += 360.0;
    if (lat[ii] > maxLat)
      maxLat = lat[ii];
    if (lat[ii] < minLat)
      minLat = lat[ii];
    if (lon[ii] > maxLon)
      maxLon = lon[ii];
    if (lon[ii] < minLon)
      minLon = lon[ii];
  }
  meta_get_lineSamp(meta, minLat, minLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  meta_get_lineSamp(meta, minLat, maxLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  meta_get_lineSamp(meta, maxLat, minLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  meta_get_lineSamp(meta, maxLat, maxLon, 0.0, &line, &sample);
  if (line < minLine)
    minLine = line;
  if (line > maxLine)
    maxLine = line;
  if (sample < minSample)
    minSample = sample;
  if (sample > maxSample)
    maxSample = sample;
  startX = (int) (minSample + 0.5);
  startY = (int) (minLine + 0.5);
  sizeX = (int) (maxSample - minSample);
  sizeY = (int) (maxLine - minLine);
  trim(infile, outfile, startX, startY, sizeX, sizeY);
  meta_free(meta); 
}

Poly *polygon_new(double *x, double *y, int start, int end)
{
  Poly *self = MALLOC(sizeof(Poly));
  int n = end - start;
  self->n = n;
  self->x = MALLOC(sizeof(double)*n);
  self->y = MALLOC(sizeof(double)*n);
  self->dateline = crosses_dateline(x, 0, end);

  int i;
  for (i=start; i<end; ++i) {
    if (self->dateline && (x[i] < 0))
      self->x[i-start] = x[i] + 360.0;
    else
      self->x[i-start] = x[i];
    self->y[i-start] = y[i];
  }

  return self;
}

void polygon_free(Poly *self)
{
  if (self) {
    if (self->x)
      free(self->x);
    if (self->y)
      free(self->y);
    free(self);
  }
}

// this is from the comp.graphics.algorithms FAQ
// see http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
int point_in_polygon(Poly *self, double x, double y)
{
  int i, j, c = 0;
  for (i = 0, j = self->n-1; i < self->n; j = i++) {
    if ((((self->y[i]<=y) && (y<self->y[j])) ||
      ((self->y[j]<=y) && (y<self->y[i]))) &&
      (x < (self->x[j] - self->x[i]) * (y - self->y[i]) /
       (self->y[j] - self->y[i]) + self->x[i]))
      c = !c;
  }
  return c;
}

void clip_to_polygon(char *inFile, char *outFile, double *lat, double *lon, 
  int *start, int nParts, int nVertices)
{
  int ii, jj, kk;
  double pLat, pLon;
    
  // Grab some metadata
  meta_parameters *meta = meta_read(inFile);
  int nb = meta->general->band_count;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  char **bands = extract_band_names(meta->general->bands, nb);
  
  // Set things up for polygon tests
  int dateline = crosses_dateline(lon, 0, nVertices);
  int *factor = (int *) MALLOC(sizeof(int)*ns);
  float *values = (float *) MALLOC(sizeof(float)*ns);
  int begin, end;

  // Load polygon
  Poly **p = (Poly**) MALLOC(sizeof(Poly*)*nParts); 
  for (ii=0; ii<nParts; ii++) {
    begin = start[ii];
    end = start[ii+1];
    p[ii] = polygon_new(lon, lat, begin, end);
  }
  
  // Go through image and update values outside polygon
  FILE *fpIn = FOPEN(inFile, "rb");
  FILE *fpOut = FOPEN(outFile, "wb");
  for (kk=0; kk<nl; kk++) {
    for (ii=0; ii<ns; ii++) {
      factor[ii] = 0;
      meta_get_latLon(meta, (double) kk, (double) ii, 0.0, &pLat, &pLon);
      if (dateline && pLon<0)
        pLon += 360;
      for (jj=0; jj<nParts; jj++) {
        if (point_in_polygon(p[jj], pLon, pLat))
          factor[ii] = 1;
      } 
    }
    for (jj=0; jj<nb; jj++) {
      get_band_float_line(fpIn, meta, jj, kk, values);    
      if (strcmp_case(bands[jj], "lat") != 0 && 
        strcmp_case(bands[jj], "lon") != 0) {
        for (ii=0; ii<ns; ii++)
          values[ii] *= (float) factor[ii];
      }
      put_band_float_line(fpOut, meta, jj, kk, values);
    }
    asfLineMeter(kk, nl);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  meta_write(meta, outFile);
  meta_free(meta);
  for (ii=0; ii<nParts; ii++)
    polygon_free(p[ii]);
  FREE(p);
  FREE(factor);
  FREE(values);
  for (ii=0; ii<nb; ii++)
    FREE(bands[ii]);
  FREE(bands);
}