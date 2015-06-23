#include "asf.h"
#include "asf_meta.h"
#include <math.h>
#include "fft.h"
#include "fft2d.h"
#include "asf_raster.h"

#if defined(mingw) // MAXFLOAT not available on mingw
#define MAXFLOAT 3.4028234663852886e+38
#elif defined(darwin)
// Include nothing, MAXFLOAT is included in math.h on darwin
#elif !defined(cygwin) // cygwin has MAXFLOAT in math.h
#include <values.h> // unix systems have it in values.h
#endif

#define MINI(a,b) ( ((a)<(b)) ? (a) : (b) )

#define modX(x,ns) ((x+ns)%ns)  /*Return x, wrapped to [0..ns-1]*/
#define modY(y,nl) ((y+nl)%nl)  /*Return y, wrapped to [0..nl-1]*/

/* readImg: reads the image file given by in
   into the (nl x ns) float array dest.  Reads a total of
   (delY x delX) pixels into topleft corner of dest, starting
   at (startY , startX) in the input file.
*/
static void readImage(FILE *in,meta_parameters *meta,
              int startX,int startY,int delX,int delY,
              float add,float *sum, float *dest, int nl, int ns)
{
  float *inBuf=(float *)MALLOC(sizeof(float)*(meta->general->sample_count));
  register int x,y,l;
  double tempSum=0;

  // We've had some problems matching images with some extremely large
  // or NaN values.  If only some pixels in the image have these values,
  // we should still be able to match.
  // Hard to say what a "crazy big" value would be...
  // We'll make the maximum allowed value MAXFLOAT (the maximum single
  // precision floating point number), divided by the number of pixels.
  const double maxval = ((double)MAXFLOAT) / ((double)ns*nl);

  /*Read portion of input image into topleft of dest array.*/
  for (y=0;y<delY;y++) {
      l=ns*y;
      get_float_line(in,meta,startY+y,inBuf);
      if (sum==NULL) {
          for (x=0;x<delX;x++) {
              if (fabs(inBuf[startX+x]) < maxval && meta_is_valid_double(inBuf[startX+x])) {
                  dest[l+x]=inBuf[startX+x]+add;
              }
          }
      }
      else {
          for (x=0;x<delX;x++) {
              if (fabs(inBuf[startX+x]) < maxval && meta_is_valid_double(inBuf[startX+x]))
              {
                  tempSum+=inBuf[startX+x];
                  dest[l+x]=inBuf[startX+x]+add;
              }
          }
      }
      for (x=delX;x<ns;x++) {
          dest[l+x]=0.0; /*Fill rest of line with zeros.*/
      }
  }

  /*Fill remainder of array (bottom portion) with zero lines.*/
  for (y=delY;y<nl;y++) {
      l=ns*y;
      for (x=0;x<ns;x++) {
          dest[l+x]=0.0; /*Fill rest of in2 with zeros.*/
      }
  }
  if (sum!=NULL) {
      *sum=(float)tempSum;
  }
  FREE(inBuf);
}


/*Perform parabolic interpolation on peak.*/
static void topOffPeak(float *peaks,int i,int j,int maxI,float *di,float *dj,
                       int nl, int ns)
{
  float a,b,c,d;
  a=peaks[modY(j,nl)*maxI+modX(i-1,ns)];
  b=peaks[modY(j,nl)*maxI+modX(i,ns)];
  c=peaks[modY(j,nl)*maxI+modX(i+1,ns)];
  d=4*((a+c)/2-b);
  if (d!=0) {
    *di=i+(a-c)/d;
  }
  else {
    *di=i;
  }
  a=peaks[modY(j-1,nl)*maxI+modX(i,ns)];
  b=peaks[modY(j,nl)*maxI+modX(i,ns)];
  c=peaks[modY(j+1,nl)*maxI+modX(i,ns)];
  d=4*((a+c)/2-b);
  if (d!=0) {
    *dj=j+(a-c)/d;
  }
  else {
    *dj=j;
  }
}


/*FindPeak: search correlation image for coherence peak.*/
static void findPeak(float *corrImage,float *dx,float *dy,float *doubt,
             int nl, int ns, int chipX, int chipY,
             int searchX, int searchY)
{
  float biggestNearby=-100000000000.0;
  int delX=15+chipX/8,delY=15+chipY/8;
  int closeX=5+chipX/16,closeY=5+chipY/16;

  /*Search for the peak with the highest correlation strength*/
  int x,y,bestX=0,bestY=0;
  float bestMatch=-100000000000.0;
  float bestLocX=delX,bestLocY=delY;
  //asfPrintStatus("   Searching for Peak (largest offset=%d lines "
  //               "& %d samples)\n",
  //               searchY,searchX);
  for (y=chipY-searchY;y<chipY+searchY;y++) {
    for (x=chipX-searchX;x<chipX+searchX;x++) {
      int index=ns*modY(y,nl)+modX(x,ns);
      if (bestMatch<corrImage[index]) {
        bestMatch=corrImage[index];
        bestX=x;bestY=y;
      }
    }
  }
  topOffPeak(corrImage,bestX,bestY,ns,&bestLocX,&bestLocY,nl,ns);

  /*Compute the doubt in our offset guess, by
    finding the largest of several pixels around our guess.*/
  for (y=-delY;y<=delY;y++) {
    for (x=-delX;x<=delX;x++) {
      if ((abs(y)>closeY)||(abs(x)>closeX)) {
        float cor=corrImage[modY(bestY+y,nl)*ns+modX(bestX+x,ns)];
        if (biggestNearby<cor) {
          biggestNearby=cor;
        }
      }
    }
  }
  *doubt=biggestNearby/bestMatch;
  if (*doubt<0) *doubt=0;

  /*Output our guess:*/
  bestLocX-=chipX;
  bestLocY-=chipY;
  *dx=bestLocX;
  *dy=bestLocY;
}


/* las_fftProd: reads both given files, and correlates them into the
created outReal (nl x ns) float array.*/
static void fftProd(FILE *in1F,meta_parameters *metaMaster,
            FILE *in2F,meta_parameters *metaSlave,float *outReal[],
            int ns, int nl, int mX, int mY,
            int chipX, int chipY, int chipDX, int chipDY,
            int searchX, int searchY)
{
  float scaleFact=1.0/(chipDX*chipDY);
  register float *in1,*in2,*out;
  register int x,y,l;
  float aveChip;

  in1=(float *)MALLOC(sizeof(float)*ns*nl);
  in2=(float *)MALLOC(sizeof(float)*ns*nl);
  out=in2;
  *outReal=in2;

  /*Read image 2 (chip)*/
  //asfPrintStatus("Reading Image 2\n");
  readImage(in2F,metaSlave,
            chipX,chipY,chipDX,chipDY,
            0.0,&aveChip,in2,nl,ns);

  /*Compute average brightness of chip.*/
  aveChip/=-(float)chipDY*chipDX;

  /*Subtract this average off of image 2(chip):*/
  for (y=0;y<chipDY;y++) {
    l=ns*y;
    for (x=0;x<chipDX;x++) {
      in2[l+x]=(in2[l+x]+aveChip)*scaleFact;
    }
  }

  /*FFT image 2 */
  //asfPrintStatus("FFT Image 2\n");
  rfft2d(in2,mY,mX);

  /*Read image 1: Much easier, now that we know the average brightness. */
  //asfPrintStatus("Reading Image 1\n");
  readImage(in1F,metaMaster,
            0,0,MINI(metaMaster->general->sample_count,ns),
            MINI(metaMaster->general->line_count,nl),
            aveChip,NULL,in1,nl,ns);

  /*FFT Image 1 */
  //asfPrintStatus("FFT Image 1\n");
  rfft2d(in1,mY,mX);

  /*Conjugate in2.*/
  //asfPrintStatus("Conjugate Image 2\n");
  for (y=0;y<nl;y++) {
    l=ns*y;
    x = (y < 2) ? 1 : 0;
    //if (y<2) x=1; else x=0;
    for (;x<ns/2;x++) {
      in2[l+2*x+1]*=-1.0;
    }
  }

  /*Take complex product of in1 and in2 into out.*/
  //asfPrintStatus("Complex Product\n");
  rspect2dprod(in1,in2,out,nl,ns);

  /*Zero out the low frequencies of the correlation image.*/
  //asfPrintStatus("Zero low frequencies.\n");
  for (y=0;y<4;y++) {
    l=ns*y;
    for (x=0;x<8;x++) out[l+x]=0;
    l=ns*(nl-1-y);
    for (x=0;x<8;x++) out[l+x]=0;
  }

  /*Inverse-fft the product*/
  //asfPrintStatus("I-FFT\n");
  rifft2d(out,mY,mX);

  FREE(in1);/*Note: in2 shouldn't be freed, because we return it.*/
}

static int mini(int a, int b)
{
  return a<b ? a : b;
}

static int fftMatchBF(char *file1, char *file2, float *dx, float *dy, float *cert, double tol)
{
  int qf_saved = quietflag;
  quietflag = TRUE;
  int ok = FALSE;

  float dx1=0, dx2=0, dy1=0, dy2=0, cert1=0, cert2=0;
  fftMatch(file1, file2, NULL, &dx1, &dy1, &cert1);
  if (!meta_is_valid_double(dx1) || !meta_is_valid_double(dy1) || cert1<tol) {
    *dx = *dy = *cert = 0;
  }
  else {
    fftMatch(file2, file1, NULL, &dx2, &dy2, &cert2);
    if (!meta_is_valid_double(dx2) || !meta_is_valid_double(dy2) || cert2<tol) {
      *dx = *dy = *cert = 0;
    }
    else if (fabs(dx1 + dx2) > .25 || fabs(dy1 + dy2) > .25) {
      *dx = *dy = *cert = 0;
    }
    else {
      *dx = (dx1 - dx2) * 0.5;
      *dy = (dy1 - dy2) * 0.5;
      *cert = cert1 < cert2 ? cert1 : cert2; 
      ok = TRUE;
    }
  }

  quietflag = qf_saved;
  //asfPrintStatus("Result %s:\n"
  //               "dx1=%8.2f dy1=%8.2f cert=%f\n"
  //               "dx2=%8.2f dy2=%8.2f cert=%f\n", ok?"Yes":"No", dx1, dy1, cert1, dx2, dy2, cert2);

  return ok; 
}

typedef struct offset_point {
  int x_pos;
  int y_pos;
  float x_offset;
  float y_offset;
  float cert;
  int valid;
} offset_point_t;


static void comp_stats(offset_point_t *matches, int len,
                       double *avg_x, double *stddev_x,
                       double *avg_y, double *stddev_y)
{
  *avg_x=0;
  *stddev_x=0;
  *avg_y=0;
  *stddev_y=0;
  int ii,n=0;
  for (ii=0; ii<len; ++ii) {
    if (matches[ii].valid) {
      *avg_x += matches[ii].x_offset;
      *avg_y += matches[ii].y_offset;
      ++n;
    }
  }
  *avg_x /= (double)n;
  *avg_y /= (double)n;
  for (ii=0; ii<len; ++ii) {
    if (matches[ii].valid) {
      double o = matches[ii].x_offset - *avg_x;
      *stddev_x += o*o;
      o = matches[ii].y_offset - *avg_y;
      *stddev_y += o*o;
    }
  }
  *stddev_x /= (double)n;
  *stddev_y /= (double)n;
  *stddev_x = sqrt(*stddev_x);
  *stddev_y = sqrt(*stddev_y);
}

static int remove_outliers(offset_point_t *matches, int len)
{
  //printf("--- Iteration Start\n");
  double avg_x, stddev_x, avg_y, stddev_y;
  comp_stats(matches, len, &avg_x, &stddev_x, &avg_y, &stddev_y);
  double tol_x = 3*stddev_x;
  double tol_y = 3*stddev_y;
  int ii,rmx=0,rmy=0;

  //printf("Removing outliers in X.\n");
  //printf("X Average: %f, StdDev: %f\n", avg_x, stddev_x);
  //printf("X Allowed range: (%f %f)\n", avg_x-tol_x, avg_x+tol_x);
  for (ii=0; ii<len; ++ii) {
    if (matches[ii].valid && fabs(matches[ii].x_offset - avg_x) > tol_x) {
      //printf("X Outlier %d: %f\n", ii, matches[ii].x_offset);
      matches[ii].valid = FALSE;
      ++rmx;
    }
  }
  //printf("Removed %d points that were outliers in X.\n", rmx);

  //printf("Removing outliers in Y.\n");
  //printf("Y Average: %f, StdDev: %f\n", avg_y, stddev_y);
  //printf("Y Allowed range: (%f %f)\n", avg_y-tol_y, avg_y+tol_y);
  for (ii=0; ii<len; ++ii) {
    if (matches[ii].valid && fabs(matches[ii].y_offset - avg_y) > tol_y) {
      //printf("Y Outlier %d: %f\n", ii, matches[ii].y_offset);
      matches[ii].valid = FALSE;
      ++rmy;
    }
  }
  //printf("Removed %d points that were outliers in Y.\n", rmy);
  //asfPrintStatus("Removed %d outliers\n", rmx+rmy);
  return rmx+rmy;
}

static void print_matches(offset_point_t *matches, int num_x, int num_y, FILE *fp)
{
  int ii, jj,kk=0,n=0;
  double avg = 0, x_avg = 0, y_avg = 0, x_stddev = 0, y_stddev = 0, x_max = 0, y_max = 0;

  fprintf(fp, "=== X Offsets ===\n");
  for (ii=0; ii<num_y; ++ii) {
    for (jj=0; jj<num_x; ++jj) {
      if (matches[kk].valid) {
        fprintf(fp, "%5.2f ", matches[kk].x_offset);
        avg += hypot(matches[kk].x_offset, matches[kk].y_offset);
        x_avg += matches[kk].x_offset;
        if (fabs(matches[kk].x_offset) > x_max)
          x_max = fabs(matches[kk].x_offset);
        ++n;
      }
      else
        fprintf(fp, "  --  ");
      ++kk;
    }
    fprintf(fp, "\n");
  }
  avg /= (double)n;
  kk=0;
  fprintf(fp, "=== Y Offsets ===\n");
  for (ii=0; ii<num_y; ++ii) {
    for (jj=0; jj<num_x; ++jj) {
      if (matches[kk].valid) {
        fprintf(fp, "%5.2f ", matches[kk].y_offset); 
        y_avg += matches[kk].y_offset;
        if (fabs(matches[kk].y_offset) > y_max)
          y_max = fabs(matches[kk].y_offset);
      }
      else
        fprintf(fp, "  --  ");
      ++kk;
    }
    fprintf(fp, "\n");
  }
  kk=0;
  x_avg /= (double)n;
  y_avg /= (double)n;
  for (ii=0; ii<num_y; ++ii) {
    for (jj=0; jj<num_x; ++jj) {
      if (matches[kk].valid) {
        double tmp = matches[kk].x_offset - x_avg;
        x_stddev += tmp*tmp;
        tmp = matches[kk].y_offset - y_avg;
        y_stddev += tmp*tmp;
      }
      ++kk;
    }
  }
  x_stddev = sqrt(x_stddev) / (double)n;
  y_stddev = sqrt(y_stddev) / (double)n;
  fprintf(fp, "    X mean/stdev/max: %8.3f %8.3f %8.3f\n", x_avg, x_stddev, x_max);
  fprintf(fp, "    Y mean/stdev/max: %8.3f %8.3f %8.3f\n", y_avg, y_stddev, y_max);
  fprintf(fp, "Total Average Offset: %8.3f\n", avg);
}

int fftMatch_gridded(char *inFile1, char *inFile2, char *gridFile,
                     float *avgLocX, float *avgLocY, float *certainty,
                     int size, double tol, int overlap)
{
  meta_parameters *meta1 = meta_read(inFile1);
  meta_parameters *meta2 = meta_read(inFile2);

  int nl = mini(meta1->general->line_count, meta2->general->line_count);
  int ns = mini(meta1->general->sample_count, meta2->general->sample_count);

  if (size<0) size = 512;
  if (overlap<0) overlap = 256;
  if (tol<0) tol = .28;

  asfPrintStatus("Tile size is %dx%d pixels\n", size, size);
  asfPrintStatus("Tile overlap is %d pixels\n", overlap);
  asfPrintStatus("Match tolerance is %.2f\n", tol);

  long long lsz = (long long)size;

  int num_x = (ns - size) / (size - overlap);
  int num_y = (nl - size) / (size - overlap);
  int len = num_x*num_y;

  asfPrintStatus("Number of tiles is %dx%d\n", num_x, num_y);

  offset_point_t *matches = MALLOC(sizeof(offset_point_t)*len); 

  int ii, jj, kk=0, nvalid=0;
  for (ii=0; ii<num_y; ++ii) {
    int tile_y = ii*(size - overlap);
    if (tile_y + size > nl) {
      if (ii != num_y - 1)
        asfPrintError("Bad tile_y: %d %d %d %d %d\n", ii, num_y, tile_y, size, nl);
      tile_y = nl - size;
    }
    for (jj=0; jj<num_x; ++jj) {
      int tile_x = jj*(size - overlap);
      if (tile_x + size > ns) {
        if (jj != num_x - 1)
          asfPrintError("Bad tile_x: %d %d %d %d %d\n", jj, num_x, tile_x, size, ns);
        tile_x = ns - size;
      }
      //asfPrintStatus("Matching tile starting at (L,S) (%d,%d)\n", tile_y, tile_x);
      char trim_append[64];
      sprintf(trim_append, "_chip_%05d_%05d", tile_y, tile_x);
      char *trim_chip1 = appendToBasename(inFile1, trim_append);
      char *trim_chip2 = appendToBasename(inFile2, trim_append);
      trim(inFile1, trim_chip1, (long long)tile_x, (long long)tile_y, lsz, lsz);
      trim(inFile2, trim_chip2, (long long)tile_x, (long long)tile_y, lsz, lsz);
      //char smooth_append[64];
      //sprintf(smooth_append, "_smooth_chip_%05d_%05d", tile_x, tile_y);
      //char *smooth_chip1 = appendToBasename(inFile1, smooth_append);
      //smooth(trim_chip1, smooth_chip1, 3, EDGE_TRUNCATE);
      float dx, dy, cert;
      int ok = fftMatchBF(trim_chip1, trim_chip2, &dx, &dy, &cert, tol);
      matches[kk].x_pos = tile_x;
      matches[kk].y_pos = tile_y;
      matches[kk].cert = cert;
      matches[kk].x_offset = dx;
      matches[kk].y_offset = dy;
      matches[kk].valid = ok && cert>tol;
      asfPrintStatus("%s: %5d %5d dx=%7.3f, dy=%7.3f, cert=%5.3f\n",
                     matches[kk].valid?"GOOD":"BAD ", tile_y, tile_x, dx, dy, cert);
      if (matches[kk].valid) ++nvalid;
      ++kk;
      //printf("%4.1f ", dx);
      removeImgAndMeta(trim_chip1);
      FREE(trim_chip1);
      removeImgAndMeta(trim_chip2);
      FREE(trim_chip2);
      //unlink(smooth_chip1);
      //FREE(smooth_chip1);
    }
    //printf("\n");
  }

  //print_matches(matches, num_x, num_y, stdout);

  asfPrintStatus("Removing grid offset outliers.\n");
  asfPrintStatus("Starting with %d offsets.\n", nvalid);

  int removed, iter=0;
  do {
    removed = remove_outliers(matches, len);
    if (removed > 0)
      asfPrintStatus("Iteration %d: Removed %d outliers\n", ++iter, removed);
  }
  while (removed > 0);
  asfPrintStatus("Finished removing outliers.\n");

  if (gridFile) {
    FILE *offset_fp = FOPEN(gridFile,"w");
    print_matches(matches, num_x, num_y, offset_fp);
    FCLOSE(offset_fp);
  }

  char *name = appendExt(inFile1, ".offsets.txt");
  FILE *fp = FOPEN(name, "w");

  int valid_points = 0;
  for (ii=0; ii<len; ++ii) {
    if (matches[ii].valid) {
      ++valid_points;
      if (fp) {
        fprintf(fp, "%5d %5d %14.5f %14.5f %14.5f\n",
                matches[ii].x_pos, matches[ii].y_pos,
                matches[ii].x_pos + matches[ii].x_offset,
                matches[ii].y_pos + matches[ii].y_offset,
                matches[ii].cert);
      }
    }
  }

  if (valid_points < 1) {
     asfPrintStatus("Too few points for a good match.\n");
  
     *avgLocX = 0;
     *avgLocY = 0;
     *certainty = 0;
  }
  else {
    *avgLocX = 0;
    *avgLocY = 0;
    *certainty = 0;
    int n = 0;
    for (ii=0; ii<len; ++ii) {
      if (matches[ii].valid) {
        *avgLocX += matches[ii].x_offset;
        *avgLocY += matches[ii].y_offset;
        *certainty += matches[ii].cert;
        ++n;
      }
    }

    *avgLocX /= (float)n;
    *avgLocY /= (float)n;
    //*certainty = (float)n / (float)len;
    *certainty /= (float)n;
  }

  asfPrintStatus("Found %d offsets.\n", valid_points);
  asfPrintStatus("Average tile offset: dx=%f, dy=%f, cert=%f\n",
                 *avgLocX, *avgLocY, *certainty);

  meta_free(meta1);
  meta_free(meta2);

  FCLOSE(fp);
  asfPrintStatus("Generated grid match file: %s\n", name);

  FREE(matches);
  FREE(name);

  return (0);
}

int fftMatch_proj(char *inFile1, char *inFile2, float *offsetX, float *offsetY, float *certainty)
{
  // Determine the offsets based on metadata
  meta_parameters *refMeta = meta_read(inFile1);
  if (!refMeta->projection)
    asfPrintError("File (%s) is not map projected!\n", inFile1);
  meta_parameters *testMeta = meta_read(inFile2);
  if (!testMeta->projection)
    asfPrintError("File (%s) is not map projected!\n", inFile2);
  double testStartX = testMeta->projection->startX + 
    testMeta->general->start_sample*testMeta->projection->perX;
  double testStartY = testMeta->projection->startY +
    testMeta->general->start_line*testMeta->projection->perY;
  double refStartX = refMeta->projection->startX +
    refMeta->general->start_sample*refMeta->projection->perX;
  double refStartY = refMeta->projection->startY +
    refMeta->general->start_line*refMeta->projection->perY;
  float diffX = (testStartX - refStartX) / testMeta->projection->perX;
  float diffY = (testStartY - refStartY) / testMeta->projection->perY;
  meta_free(refMeta);
  meta_free(testMeta);

  // Figure out what FFT parameters are going to be
  int size = 512;
  double tol = -1;
  float diff; 
  if (fabs(diffX) > fabs(diffY))
    diff = fabs(diffX);
  else
    diff = fabs(diffY);
  while ((size/4) < diff) {
    size *= 2; 
  }
  int overlap = size / 2;

  // Determine the offsets based on mapping
  float offX, offY, cert;
  asfPrintStatus("Determing offsets by FFT matching\n\n");
  fftMatch_gridded(inFile1, inFile2, NULL, &offX, &offY, &cert, size, tol, 
    overlap);
  *certainty = cert;

  // Compare both offsets
  *offsetX = diffX - offX;
  *offsetY = diffY - offY;

  return (0);
}

/*
 * Call fftMatch_proj on projected data, and fftMatch on data that is not
 * projected.
 */
int fftMatch_either(char *inFile1, char *inFile2, float *offsetX,
                    float *offsetY, float *certainty)
{
        meta_parameters *refMeta = meta_read(inFile1);
        meta_parameters *testMeta = meta_read(inFile2);
        if (!refMeta->projection || !testMeta->projection) {
                asfPrintStatus("Data are not map projected.\n");
                return fftMatch(inFile1, inFile2, NULL, offsetX, offsetY,
                                certainty);
        } else {
                asfPrintStatus("Data are map projected.\n");
                return fftMatch_proj(inFile1, inFile2, offsetX, offsetY,
                                certainty);
        }
}

int fftMatch_opt(char *inFile1, char *inFile2, float *offsetX, float *offsetY)
{ 
  // Generate temporary directory
  char tmpDir[1024];
  char metaFile[1024], outFile[1024], sarFile[1024], opticalFile[1024];
  char *baseName = get_basename(inFile1);
  strcpy(tmpDir, baseName);
  strcat(tmpDir, "-");
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);
  
  // Cutting optical to SAR extent
  asfPrintStatus("Cutting optical to SAR extent ...\n");
  sprintf(metaFile, "%s.meta", inFile1);
  sprintf(outFile, "%s%c%s_sub.img", tmpDir, DIR_SEPARATOR, inFile2);
  trim_to(inFile2, outFile, metaFile);
  
  // Clipping optical image including blackfill
  asfPrintStatus("\nClipping optical image including blackfill ...\n");
  meta_parameters *metaOpt = meta_read(outFile);
  meta_parameters *metaSAR = meta_read(metaFile);
  int line_count = metaSAR->general->line_count;
  int sample_count = metaSAR->general->sample_count;
  float *floatLine = (float *) MALLOC(sizeof(float)*sample_count);
  unsigned char *byteLine = (unsigned char *) MALLOC(sizeof(char)*sample_count);
  FILE *fpOptical = FOPEN(outFile, "rb");
  sprintf(sarFile, "%s.img", inFile1);
  FILE *fpSAR = FOPEN(sarFile, "rb");
  sprintf(outFile, "%s%c%s_mask.img", tmpDir, DIR_SEPARATOR, inFile2);
  sprintf(metaFile, "%s%c%s_mask.meta", tmpDir, DIR_SEPARATOR, inFile2);
  FILE *fpOut = FOPEN(outFile, "wb");
  int ii, kk;
  for (ii=0; ii<line_count; ii++) {
    get_float_line(fpSAR, metaSAR, ii, floatLine);
    get_byte_line(fpOptical, metaOpt, ii, byteLine);
    for (kk=0; kk<sample_count; kk++) {
      if (!FLOAT_EQUIVALENT(floatLine[kk], 0.0))
        floatLine[kk] = (float) byteLine[kk];
    }
    put_float_line(fpOut, metaSAR, ii, floatLine);
  }
  FCLOSE(fpOptical);
  FCLOSE(fpSAR);
  FCLOSE(fpOut);
  meta_write(metaSAR, metaFile);

  // Edge filtering optical image
  asfPrintStatus("\nEdge filtering optical image ...\n");
  sprintf(opticalFile, "%s%c%s_sobel.img", tmpDir, DIR_SEPARATOR, inFile2);
  kernel_filter(outFile, opticalFile, SOBEL, 3, 0, 0);

  // Edge filtering SAR image
  asfPrintStatus("\nEdge filtering SAR image ...\n");
  sprintf(sarFile, "%s%c%s_sobel.img", tmpDir, DIR_SEPARATOR, inFile1);
  kernel_filter(inFile1, sarFile, SOBEL, 3, 0, 0);

  // FFT matching on a grid
  asfPrintStatus("\nFFT matching on a grid ...\n");
  float certainty;
  fftMatch_proj(sarFile, opticalFile, offsetX, offsetY, &certainty);

  // Clean up
  remove_dir(tmpDir);

  return (0);
}

double distance_to(int index, float *x, float *y, int num)
{
  int i;
  double d=0;

  float xref = index >= 0 ? x[index] : 0;
  float yref = index >= 0 ? y[index] : 0;

  for (i=0; i<num; ++i)
    d += hypot((double)(x[i] - xref), (double)(y[i] - yref));

  return d + hypot(xref,yref);
}

int fftMatch_projList(char *inFile, char *descFile)
{
  FILE *fp = FOPEN(inFile, "r");
  if (!fp) asfPrintError("Failed to open %s\n", inFile);

  char line[255], master[255];
  float x_offs[255], y_offs[255];
  int n=0;

  while (NULL != fgets(line, 255, fp)) {
    if (line[strlen(line)-1]=='\n')
      line[strlen(line)-1] = '\0';
    if (line[0] == '#' || line[0] == '\0')
      continue;

    if (n==0) {
      strcpy(master, line);
    }
    else {
      float certainty;
      fftMatch_proj(master, line, &x_offs[n-1], &y_offs[n-1], &certainty);
    }

    ++n;
    if (n>=255)
      asfPrintError("Too many granules: max 255");
  }

  FCLOSE(fp);

  FILE *fpd=NULL;
  if (descFile) {
    fpd = FOPEN(descFile, "w");
    fprintf(fpd, "master,slave,offsetX,offsetY,total offsets\n");
  }

  int num=n-1;
  n=0;

  char best[255];
  double min_dist;

  fp = FOPEN(inFile, "r");
  while (NULL != fgets(line, 255, fp)) {
    if (line[strlen(line)-1]=='\n')
      line[strlen(line)-1] = '\0';
    if (line[0] == '#' || line[0] == '\0')
      continue;

    if (n==0) {
      min_dist = distance_to(-1, x_offs, y_offs, num);
      fprintf(fpd ? fpd : stdout, "%s,%s,%.5f,%.5f,%.5f\n",
              master, master, 0., 0., min_dist);
      strcpy(best, master);
    }
    else {
      double d = distance_to(n-1, x_offs, y_offs, num);
      fprintf(fpd ? fpd : stdout, "%s,%s,%.5f,%.5f,%.5f\n",
              master, line, x_offs[n-1], y_offs[n-1], d);
      if (d < min_dist) {
        min_dist = d;
        strcpy(best, line);
      }
    }

    ++n;
  }

  asfPrintStatus("Best is %s\n", best);

  if (descFile) {
    asfPrintStatus("Generated match file (%s)!\n", descFile);
    FCLOSE(fpd);
  }

  FCLOSE(fp);

  if (strcmp(master, best) == 0) {
    asfPrintStatus("Reference granule is already the best: %s\n", master);
  }
  else {
    char *new = appendExt(inFile, ".new");
    fpd = FOPEN(new, "w");
    fprintf(fpd,"%s\n", best);

    fp = FOPEN(inFile, "r");
    while (NULL != fgets(line, 255, fp)) {
      if (line[strlen(line)-1]=='\n')
        line[strlen(line)-1] = '\0';
      if (line[0] == '#' || line[0] == '\0')
        continue;

      if (strcmp(line, best) != 0) {
        fprintf(fpd,"%s\n", line);
      } 
    }
    FCLOSE(fpd);
    FCLOSE(fp);
  }
 
  return 0;
}

int fftMatch(char *inFile1, char *inFile2, char *corrFile,
          float *bestLocX, float *bestLocY, float *certainty)
{
  int nl,ns;
  int mX,mY;               /*Invariant: 2^mX=ns; 2^mY=nl.*/
  int chipX, chipY;        /*Chip location (top left corner) in second image*/
  int chipDX,chipDY;       /*Chip size in second image.*/
  int searchX,searchY;     /*Maximum distance to search for peak*/

  int x,y;
  float doubt;
  float *corrImage=NULL;
  FILE *corrF=NULL,*in1F,*in2F;
  meta_parameters *metaMaster, *metaSlave, *metaOut;

  in1F = fopenImage(inFile1,"rb");
  in2F = fopenImage(inFile2,"rb");
  metaMaster = meta_read(inFile1);
  metaSlave = meta_read(inFile2);

  /*Round to find nearest power of 2 for FFT size.*/
  mX = (int)(log((float)(metaMaster->general->sample_count))/log(2.0)+0.5);
  mY = (int)(log((float)(metaMaster->general->line_count))/log(2.0)+0.5);

  /* Keep size of fft's reasonable */
  if (mX > 13) mX = 13;
  if (mY > 15) mY = 15;
  ns = 1<<mX;
  nl = 1<<mY;

  /* Test chip size to see if we have enough memory for it */
  /* Reduce it if necessary, but not below 1024x1024 (which needs 4 Mb of memory) */
  float *test_mem = (float *)malloc(sizeof(float)*ns*nl*2);
  if (!test_mem && !quietflag) asfPrintStatus("\n");
  while (!test_mem) {
      mX--;
      mY--;
      ns = 1<<mX;
      nl = 1<<mY;
      if (ns < 1024 || nl < 1024) {
          asfPrintError("FFT Size too small (%dx%d)...\n", ns, nl);
      }
      if (!quietflag) asfPrintStatus("   Not enough memory... reducing FFT Size to %dx%d\n", ns, nl);
      test_mem = (float *)malloc(sizeof(float)*ns*nl*2);
  }
  FREE(test_mem);
  if (!quietflag) asfPrintStatus("\n");

  /*Set up search chip size.*/
  chipDX=MINI(metaSlave->general->sample_count,ns)*3/4;
  chipDY=MINI(metaSlave->general->line_count,nl)*3/4;
  chipX=MINI(metaSlave->general->sample_count,ns)/8;
  chipY=MINI(metaSlave->general->line_count,nl)/8;
  searchX=MINI(metaSlave->general->sample_count,ns)*3/8;
  searchY=MINI(metaSlave->general->line_count,nl)*3/8;

  fft2dInit(mY, mX);

  if (!quietflag && ns*nl*2*sizeof(float)>20*1024*1024) {
    asfPrintStatus(
            "   These images will take %d megabytes of memory to match.\n\n",
            ns*nl*2*sizeof(float)/(1024*1024));
  }

  /*Optionally open the correlation image file.*/
  if (corrFile) {
    metaOut = meta_read(inFile1);
    metaOut->general->data_type= REAL32;
    metaOut->general->line_count = 2*searchY;
    metaOut->general->sample_count = 2*searchX;
    corrF=fopenImage(corrFile,"w");
  }

  /*Perform the correlation.*/
  fftProd(in1F,metaMaster,in2F,metaSlave,&corrImage,ns,nl,mX,mY,
          chipX,chipY,chipDX,chipDY,searchX,searchY);

  /*Optionally write out correlation image.*/
  if (corrFile) {
    int outY=0;
    float *outBuf=(float*)MALLOC(sizeof(float)*metaOut->general->sample_count);
    for (y=chipY-searchY;y<chipY+searchY;y++) {
      int index=ns*modY(y,nl);
      int outX=0;
      for (x=chipX-searchX;x<chipX+searchX;x++) {
        outBuf[outX++]=corrImage[index+modX(x,ns)];
      }
      put_float_line(corrF,metaOut,outY++,outBuf);
    }
    meta_write(metaOut, corrFile);
    meta_free(metaOut);
    FREE(outBuf);
    FCLOSE(corrF);
  }

  /*Search correlation image for a peak.*/
  findPeak(corrImage,bestLocX,bestLocY,&doubt,nl,ns,
           chipX,chipY,searchX,searchY);
           *certainty = 1-doubt;

  FREE(corrImage);
  if (!quietflag) {
    asfPrintStatus("   Offset slave image: dx = %f, dy = %f\n"
                   "   Certainty: %f%%\n",*bestLocX,*bestLocY,100*(1-doubt));
  }

  meta_free(metaSlave);
  meta_free(metaMaster);
  FCLOSE(in1F);
  FCLOSE(in2F);

  return (0);
}

/* This method is here to match the old interface of fftMatch.  Old code
   that we don't want to mess with, but still want to work, should just call
   this method instead of the redone fftMatch */
void fftMatch_withOffsetFile(char *inFile1, char *inFile2, char *corrFile,
                 char *offsetFileName)
{
  float dx, dy, cert;
  fftMatch(inFile1, inFile2, corrFile, &dx, &dy, &cert);
  if (offsetFileName) {
    FILE *descF=FOPEN(offsetFileName,"w");
    fprintf(descF,"%f\t%f\t%f\n",dx,dy,100*cert);
    FCLOSE(descF);
  }
}
