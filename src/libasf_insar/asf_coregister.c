#include "asf.h"
#include "ardop_params.h"
#include "asf_insar.h"
#include "functions.h"
#include "asf_meta.h"
#include "fft.h"
#include "ifm.h"
#include "asf_endian.h"

/*
// Coarse coregistration

void CreateFicoControl(char *ctrl,char *img1,char *img2,int multiLook);
void WriteFicoControl(char *,int,int);

int coregister_coarse(char *masterFile, char *slaveFile, char *ctrlFile,
                      char *maskFile)
{
  int multiLook;
  char baseFile[512], metaFile[512], meta_master[512], meta_slave[512];
  meta_parameters *meta;

  // Determine whether multilooking is required
  create_name(metaFile, masterFile, "_amp.meta");
  meta = meta_read(metaFile);
  multiLook = meta->sar->look_count;
  meta_free(meta);

  // Determine baseline
  create_name(meta_master, masterFile, "_amp");
  create_name(meta_slave, slaveFile, "_amp");
  strcpy(baseFile, "base.00");
  write_baseline( baseFile, find_baseline(meta_master, meta_slave) );

  // Write control file
  CreateFicoControl(ctrlFile, masterFile, slaveFile, multiLook);

  return TRUE;
}

void execute(char *cmd)
{
  char report[1024];

  // Report (& log) the command line
  sprintf(report,"\nExecuting: %s\n",cmd);
  printf(report);
  fflush(NULL);
  if (logflag) {
    printLog(report);
  }

  // Make the call & report any errors
  if (0!=system(cmd))
    {
      sprintf(errbuf,"   ERROR: Command '%s' returned in error!\n",cmd);
      printErr(errbuf);
    }
}

void cpx_2_amp_byte(char *img, int multiLook)
{
  char tmp[256];
  sprintf(tmp,"%s_amp.img",img);

  if (!fileExists(tmp)) {
    char cmd[512];
    char tmp_cmd[512];
    char c2p_out[256];
    char convert2byte_out[256];

    // Convert complex to amp & phase
    sprintf(c2p_out,"%s_c2p",img);
    sprintf(cmd,"c2p %s %s",img,c2p_out);
    execute(cmd);

    // Convert amp image to byte
    sprintf(convert2byte_out,"%s_amp",img);
    sprintf(cmd," -look %dx1 -step %dx1 %s.amp %s.img",
            multiLook,multiLook,c2p_out,convert2byte_out);
    if (logflag) {
      sprintf(cmd," -log %s %s",
              logFile, strcpy(tmp_cmd,cmd));
    }
    if (quietflag) {
      sprintf(cmd," -quiet %s", strcpy(tmp_cmd,cmd));
    }
    sprintf(cmd,"convert2byte %s", strcpy(tmp_cmd,cmd));
    execute(cmd);
  }
}

void CreateFicoControl(char *ctrl,char *img1,char *img2, int multiLook)
{
  float offX,offY;
  char cmd[512];
  char tmp_cmd[512];
  char *offsetF="res_offsets";
  FILE *f;

  cpx_2_amp_byte(img1, multiLook);
  cpx_2_amp_byte(img2, multiLook);

  // Line up our two images to about the pixel level
  sprintf(cmd," -m %s %s_amp.img %s_amp.img ",offsetF,img1,img2);
  if (logflag) {
    sprintf(cmd," -log %s %s ", logFile,strcpy(tmp_cmd,cmd));
  }
  if (quietflag) {
    sprintf(cmd," -quiet %s ", strcpy(tmp_cmd,cmd));
  }
  sprintf(cmd,"fftMatch %s", strcpy(tmp_cmd,cmd));
  execute(cmd);

  // Read fftMatch output & use it
  f=fopen(offsetF,"r");
  if (f==NULL || 2!=fscanf(f,"%f%f",&offX,&offY))
    asfPrintError("   Couldn't extract offset parameters from file %s!\n",
                  offsetF);
  fclose(f);
  unlink(offsetF);
  offY*=multiLook;
  asfPrintStatus("   Complex image offset is %d rows, %d columns\n\n",
                 (int)offY,(int)offX);
  WriteFicoControl(ctrl,(int)offX,(int)offY);
}

void WriteFicoControl(char *fnm, int xoff, int yoff)
{
  int chip_size = 32;  // this value must be a power of 2, usually 16
  int os = 1;          // this value is also a power of 2, usually 4
  float xmep = 4.1;    // x maximum error pixel value that is accepted
  float ymep = 6.1;    // y maximum error pixel value that is accepted

  FILE *fp = FOPEN(fnm,"w");
  fprintf(fp,"%d\n%d\n%d\n%d\n%f\n%f\n",
          xoff, yoff, chip_size, os,xmep,ymep);
  FCLOSE(fp);
  return;
}


// Fine coregistration

#define borderX 80    // Distances from edge of image to start correlating
#define borderY 80
#define minSNR 0.30   // SNR's below this will be deleted
#define maxDisp 1.8   // Forward and reverse correlations which differ by more
                      // than this will be deleted

// Read-only, informational globals
int ns, nl;                  // Width and length of source images
int intOffsetX, intOffsetY;  // Image offset estimates from coregister_coarse
int srcSize=32, trgSize;
float xMEP=4.1,yMEP=6.1;     // Maximum Error Pixel values
complexFloat cZero;

void readControlFile(char *filename);
void initSourcePts(char *gridRes);

bool getNextPoint(int *x1, int *y1, int *x2, int *y2);
bool outOfBoundary(int x1, int y1, int x2, int y2, int srcSize, int trgSize);

void getPeak(int x1, int y1, char *szImg1, int x2, int y2, char *szImg2,
	     float *dx, float *dy, float *snr, int fft_flag);
void topOffPeak(float *peaks, int i, int j, int maxI, int maxJ,
		float *dx, float *dy);

#ifndef TWOPI
# define TWOPI (2*PI)
#endif

double topi(double x)
{
  x=fmod(x,TWOPI);
  if (x>PI)
    return x-TWOPI;
  else if (x<=-PI)
    return x+TWOPI;
  return x;
}

float getPhaseCoherence(complexFloat *igram, int sizeX, int sizeY)
{
  register double phaseErr=0;
  register int npixels=0;
#define ml 8
#define boxX 1
#define boxY 8
  int targX, targY, mlSizeY=sizeY/ml;
  // Multilook the interferogram in-place
  for (targY=0; targY<mlSizeY; targY++) {
    int targetIndex = targY*sizeX;
    int sourceIndex = targY*ml*sizeX;
    for (targX=0; targX<sizeX; targX++) {
      register int y;
      register double sum_imag,sum_real;
      register complexFloat *src = &igram[sourceIndex];
      sum_imag = sum_real=0;
      for (y=0; y<boxY; y++) {
	sum_imag += src->imag;
	sum_real += src->real;
	src += sizeX;
      }
      sourceIndex++;
      igram[targetIndex++].real = atan2(sum_real,sum_imag);
    }
  }
  // Now find the deviation from f1 continuity in this multilooked image
  for (targY=0; targY<mlSizeY; targY++) {
#define winSize 10
#define halfSize (winSize/2)
#define invWinSize (1/(float)winSize)
    float deltas[500], sumDiff=0;
    int index=targY*sizeX;
    // Compute the phase difference at each pixel
    for (targX=0; targX<(sizeX-1); targX++)
      deltas[targX]=topi(igram[index+targX+1].real-igram[index+targX].real);
    // Compute a moving average of the phase difference, and compare this to 
    // the middle value
    for (targX=0; targX<winSize; targX++)
      sumDiff += deltas[targX];
    for (targX=halfSize; targX<(sizeX-1-halfSize); targX++) {
      phaseErr += fabs(sumDiff*invWinSize-deltas[targX]);
      sumDiff -= deltas[targX-halfSize];
      sumDiff += deltas[targX+halfSize];
      npixels++;
    }
  }
  return 1.0/(phaseErr/npixels);
}

float getFFTCorrelation(complexFloat *igram,int sizeX,int sizeY)
{
  int line, samp;
  int fftpowr;
  float ampTmp=0;
  float maxAmp=0;
  complexFloat *fftBuf;
  complexFloat *fftTemp;
  complexFloat *fft;
  
  fft = (complexFloat *)MALLOC(sizeof(complexFloat)*sizeX*sizeX);
  fftTemp = (complexFloat *)MALLOC(sizeof(complexFloat)*sizeX*sizeX);
  fftpowr = (log(sizeX)/log(2));
  fftInit(fftpowr);
  
  // First do the FFT of each line
  for(line=0; line<sizeX; line++) {
    fftBuf = &igram[line*sizeX];
    ffts((float *)fftBuf, fftpowr, 1);
    for(samp=0; samp<sizeX; samp++) {
      fftTemp[line*sizeX+samp].real = fftBuf[samp].real;
      fftTemp[line*sizeX+samp].imag = fftBuf[samp].imag;
    }
  }
  
  // Now do the FFT of the columns of the FFT'd lines
  for(samp=0; samp<sizeX; samp++) {
      
    // Fill up the FFT buffer
    for(line=0;line<sizeX;line++) {
      fftBuf[line].real = fftTemp[line*sizeX+samp].real;
      fftBuf[line].imag = fftTemp[line*sizeX+samp].imag;
    }
      
    // Do the FFT
    ffts((float *)fftBuf, fftpowr, 1);
    for(line=0;line<sizeX;line++) {
      fft[line*sizeX+samp].real = fftBuf[line].real;
      fft[line*sizeX+samp].imag = fftBuf[line].imag;
    }
  }
  free(fftTemp);
  fftFree();
  
  // Now we have a two dimension FFT that we can search to find the max value
  for(line=0; line<sizeX; line++) {
    for(samp=0; samp<sizeX; samp++) {
      ampTmp=sqrt(fft[line*sizeX+samp].real*fft[line*sizeX+samp].real +
		  fft[line*sizeX+samp].imag*fft[line*sizeX+samp].imag);
      if(ampTmp>maxAmp)
	maxAmp=ampTmp;
    }
  }
  free(fft);
  return maxAmp;
}

int coregister_fine(char *masterFile, char *slaveFile, char *ctrlFile,
                    char *ficoFile, char *maskFile, int gridSize, int fft_flag)
{
  int x1, x2, y1, y2;
  int goodPoints, attemptedPoints;
  char gridRes[256];

  if (fft_flag)
    asfPrintStatus("   Using Complex FFT instead of coherence for matching\n");

  readControlFile(ctrlFile);

  // calculate parameters
  trgSize = 2*srcSize;

  // determine size of input files
  meta_parameters *meta = meta_read(masterFile);
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  meta_free(meta);

  // initialize params before looping
  cZero = Czero();

  // create output file
  FILE *fp = FOPEN(ficoFile, "w");

  initSourcePts(gridRes);

  // Loop over grid, performing forward and backward correlations
  goodPoints=attemptedPoints=0;
  while (getNextPoint(&x1,&y1,&x2,&y2)) {
    float dx,dy,snr,dxFW,dyFW,snrFW,dxBW,dyBW,snrBW;
    attemptedPoints++;
    // Check bounds...
    if (!(outOfBoundary(x1, y1, x2, y2, srcSize, trgSize) ||
          outOfBoundary(x2, y2, x1, y1, srcSize, trgSize))) {
      // ...check forward correlation...
      getPeak(x1, y1, masterFile, x2, y2,slaveFile, &dxFW, &dyFW, &snrFW,
              fft_flag);
      if (snrFW > minSNR) {
        // ...check backward correlation...
        getPeak(x2, y2, slaveFile, x1, y1, masterFile, &dxBW, &dyBW, &snrBW,
                fft_flag);
        dxBW *= -1.0;
        dyBW *= -1.0;
        if ((snrBW > minSNR)&&
            (fabs(dxFW-dxBW) < maxDisp)&&
            (fabs(dyFW-dyBW) < maxDisp)) {
          goodPoints++;
          dx = (dxFW+dxBW)/2;
          dy = (dyFW+dyBW)/2;
          snr = snrFW*snrBW;
          fprintf(fp,"%6d %6d %8.5f %8.5f %4.2f\n",
                  x1, y1, x2+dx, y2+dy, snr);
          fflush(fp);
          if (!quietflag && (goodPoints <= 10 || !(goodPoints%100)))
            printf("\t%6d %6d %8.5f %8.5f %4.2f/%4.2f\n",
                   x1, y1, dx, dy, snrFW, snrBW);
        }
      }
    }
  }

  if (goodPoints<20)
    asfPrintError("   *************** ERROR! ************\n"
                  "   **    coregister_fine was only able to find %i points which\n"
                  "   ** correlated the same backwards and forwards.  This\n"
                  "   ** is not enough for a planar map!\n"
                  "   **    Problems with coregister_fine can usually be traced back "
                  "to coregister_fine's\n"
                  "   ** control file, which MUST have a good estimate of the \n"
                  "   ** single-pixel offset between the two images.\n"
                  "   **      Exiting with error!\n", goodPoints);
  else
    asfPrintStatus("   coregister_fine attempted %d correlations, %d succeeded"
                   ".\n\n", attemptedPoints, goodPoints);

  return TRUE;
}

bool outOfBoundary(int x1, int y1, int x2, int y2, int srcSize, int trgSize)
{
  if (x1 - srcSize/2 + 1 < 0) return TRUE;
  if (y1 - srcSize/2 + 1 < 0) return TRUE;
  if (x2 - trgSize/2 + 1 < 0) return TRUE;
  if (y2 - trgSize/2 + 1 < 0) return TRUE;
  if (x1 + srcSize/2  >= ns) return TRUE;
  if (y1 + srcSize/2  >= nl) return TRUE;
  if (x2 + trgSize/2  >= ns) return TRUE;
  if (y2 + trgSize/2  >= nl) return TRUE;
  return FALSE;
}

void readControlFile(char *filename)
{
  float ignored;
  // read control file parameters
  FILE *fp = FOPEN(filename,"r"); // control file:

  // range offset (loc in img 2 = loc in img 1 - x offset)
  fscanf(fp, "%d", &intOffsetX);
  // azimuth offset (loc in img 2 = loc in img 1 - y offset)
  fscanf(fp, "%d", &intOffsetY);
  fscanf(fp, "%d", &srcSize);
  if (srcSize<8) srcSize=32;
  fscanf(fp, "%f", &ignored);//IGNORED oversample. This is now always 1
  fscanf(fp, "%f", &xMEP);
  fscanf(fp, "%f", &yMEP);

  FCLOSE(fp);
}

int pointNo=0;
int gridResolution=20;
void initSourcePts(char *gridRes)
{
  // Check to see if the last parameter contains a number, the grid resolution
  if (gridRes)
    gridResolution=atoi(gridRes);
  if (gridResolution<2)
    gridResolution=20;
  if (!quietflag)
    printf("   Sampling rectangular grid, %ix%i resolution.\n",
           gridResolution,gridResolution);
}
bool getNextPoint(int *x1,int *y1,int *x2,int *y2)
{
  int unscaledX, unscaledY;
  unscaledX=pointNo%gridResolution;
  unscaledY=pointNo/gridResolution;
  *x1=unscaledX*(ns-2*borderX)/(gridResolution-1)+borderX;
  *y1=unscaledY*(nl-2*borderY)/(gridResolution-1)+borderY;
  *x2=*x1-intOffsetX;
  *y2=*y1-intOffsetY;
  if (pointNo>=(gridResolution*gridResolution))
    return FALSE;
  pointNo++;
  return TRUE;
}
// getPeak:
// This function computes a correlation peak, with SNR, between
// the two given images at the given points.

void getPeak(int x1,int y1,char *szImg1,int x2,int y2,char *szImg2,
             float *peakX,float *peakY, float *snr,int fft_flag)
{
  FILE *fpSource, *fpTarget;
  meta_parameters *metaSource, *metaTarget;
  static float *peaks;
  static complexFloat *bufSource, *bufTarget;
  static complexFloat *s=NULL, *t, *product; // Keep working arrays around
  int srcSamples, trgSamples;
  int peakMaxX, peakMaxY, x,y,xOffset,yOffset,count;
  int xOffsetStart, yOffsetStart, xOffsetEnd, yOffsetEnd;
  float dx,dy,accel1 = (float)(trgSize/2 - srcSize/2);
  float peakMax, thisMax, peakSum;

  // Calculate the limits of the time domain correlations...
  //   A coordinate in the target may be set to:
  //   (  (trgSize/2 - srcSize/2), (trgSize/2 - srcSize/2)  ).
  //   If this is the ulh element of the source chip, then
  //     the src chip coincides with the trg precisely, with no offset.

  xOffsetStart = (trgSize/2 - srcSize/2) - (int)(xMEP);
  xOffsetEnd = (trgSize/2 - srcSize/2) + (int)(xMEP);
  yOffsetStart = (trgSize/2 - srcSize/2) - (int)(yMEP);
  yOffsetEnd = (trgSize/2 - srcSize/2) + (int)(yMEP);

  // Read metadata
  metaSource = meta_read(szImg1);
  metaTarget = meta_read(szImg2);
  srcSamples = metaSource->general->sample_count;
  trgSamples = metaTarget->general->sample_count;

  // Allocate working arrays if we haven't already done so
  if (s==NULL)
    {
      bufSource = (complexFloat *) MALLOC(sizeof(complexFloat)*srcSize*srcSamples);
      bufTarget = (complexFloat *) MALLOC(sizeof(complexFloat)*trgSize*trgSamples);
      s = (complexFloat *)(MALLOC(srcSize*srcSize*sizeof(complexFloat)));
      t = (complexFloat *)(MALLOC(trgSize*trgSize*sizeof(complexFloat)));
      product = (complexFloat *)(MALLOC(srcSize*srcSize*sizeof(complexFloat)));
      peaks=(float *)MALLOC(sizeof(float)*trgSize*trgSize);
    }

  // Open files, read lines and create subset
  fpSource = FOPEN(szImg1, "rb");
  fpTarget = FOPEN(szImg2, "rb");
  get_complexFloat_lines(fpSource, metaSource, y1-srcSize/2+1, srcSize, bufSource);
  get_complexFloat_lines(fpTarget, metaTarget, y2-trgSize/2+1, trgSize, bufTarget);
  FCLOSE(fpSource);
  FCLOSE(fpTarget);
  for (y=0; y<srcSize; y++) {
    int srcIndex = y*srcSize;
    for (x=0; x<srcSize; x++)
      s[srcIndex++] = bufSource[x1-srcSize/2+1+x+y*srcSamples];
  }
  for (y=0; y<trgSize; y++) {
    int trgIndex = y*trgSize;
    for (x=0; x<trgSize; x++)
      t[trgIndex++] = bufTarget[x2-trgSize/2+1+x+y*trgSamples];
  }

  // Take the complex conjugate of the source chunk (so we only have to do 
  // so once)
  for(y=0;y<srcSize;y++) {
    int srcIndex=y*srcSize;
    for(x=0;x<srcSize;x++)
      s[srcIndex++].imag*=-1;
  }

  // Now compute the best possible offset between these two images,
  // by checking the phase coherence at each possible offset
  peakMax = peakSum = 0.0;
  peakMaxX=peakMaxY=count=0;
  for(yOffset=yOffsetStart;yOffset<=yOffsetEnd;yOffset++) {
    for(xOffset=xOffsetStart;xOffset<=xOffsetEnd;xOffset++) {
      // Form an interferogram
      // (multiply by complex conjugate at this offset between the images)
      for(y=0;y<srcSize;y++) {
        int srcIndex=y*srcSize;
        int trgIndex=xOffset+(yOffset+y)*trgSize;
        for(x=0;x<srcSize;x++) {
          product[srcIndex] = Cmul(s[srcIndex], t[trgIndex]);
          srcIndex++,trgIndex++;
        }
      }

      // Find the phase coherence for this interferogram
      if(fft_flag)
        thisMax=getFFTCorrelation(product,srcSize,srcSize);
      else
        thisMax=getPhaseCoherence(product,srcSize,srcSize);

      // Possibly save this coherence value
      if (thisMax>peakMax) {
        peakMax=thisMax;
        peakMaxX=xOffset;
        peakMaxY=yOffset;
      }
      peaks[yOffset*trgSize+xOffset]=thisMax;
      peakSum += thisMax;
      count++;
    }
  }

  // Calculate the SNR, with a much faster (but weaker) SNR calculation
  *snr = peakMax / ((peakSum - peakMax) / (float)(count-1))-1.0;

  //printf("peakMaxX: %i, peakMaxY: %i\n", peakMaxX, peakMaxY);
  if ((peakMaxX>xOffsetStart)&&(peakMaxY>yOffsetStart)&&
      (peakMaxX<xOffsetEnd)&&(peakMaxY<yOffsetEnd))
    topOffPeak(peaks,peakMaxX,peakMaxY,trgSize,trgSize,&dx,&dy);
  else
    dx=dy=0.0;

  *peakX=((float)(peakMaxX) + dx - accel1 );
  *peakY=((float)(peakMaxY) + dy - accel1 );
}

// TopOffPeak:
// Given an array of peak values, use trilinear interpolation to determine 
// the exact (i.e. float) top.
// This works by finding the peak of a parabola which goes though the highest 
// point, and the three points surrounding it.
void topOffPeak(float *peaks,int i, int j, int maxI, int maxJ, 
		float *dx, float *dy)
{
  int offset=j*maxI+i;
  float a,b,c,d;
  a=peaks[offset-1];
  b=peaks[offset];
  c=peaks[offset+1];
  d=4*((a+c)/2-b);
  if (d!=0)
    *dx=(a-c)/d;
  else *dx=0;
  a=peaks[offset-maxI];
  b=peaks[offset];
  c=peaks[offset+maxI];
  d=4*((a+c)/2-b);
  if (d!=0)
    *dy=(a-c)/d;
  else *dy=0;
}
*/

// Doppler function

int average_in_doppler(char *inFileMaster, char *inFileSlave, char *outFile)
{  
  FILE *fp;
  struct ARDOP_PARAMS ardop_master, ardop_slave;
  float avg_t1, avg_t2, avg_t3;

  // Read .in file into ardop structures 
  read_params(inFileMaster, &ardop_master);
  read_params(inFileSlave, &ardop_slave);
  
  avg_t1 = (ardop_master.fd + ardop_slave.fd) / 2;
  avg_t2 = (ardop_master.fdd + ardop_slave.fdd) / 2;
  avg_t3 = (ardop_master.fddd + ardop_slave.fddd) / 2;
  
  asfPrintStatus("\nAverage Doppler: %e %e %e \n\n", 
		 avg_t1, avg_t2, avg_t3);
  
  // Store result, in the order fd fdd fddd
  fp = FOPEN(outFile, "w");
  fprintf(fp, "%e %e %e", avg_t1, avg_t2, avg_t3);
  FCLOSE(fp); 
  
  return(0);
}

/* need to be finished
struct INPUT_ARDOP_PARAMS *read_ardop_params(char *img_file, char *doppler,
					     int nPatches, int firstLine);
{
  struct INPUT_ARDOP_PARAMS *params_in;
  float fd, fdd, fddd;
  FILE *fp;

  params_in = get_input_ardop_params_struct(img_file, "");
  if (doppler) {
    fp = FOPEN(doppler, "r");
    fscanf(fp, "%f %f %r", &fd, &fdd, &fddd);
    FCLOSE(fp);
    params_in->fd = fd;
    params_in->fdd = fdd;
    params_in->fddd = fddd;
  }
  params_in->iflag = 1;
  params_in->npatches = nPatches;
  params_in->ifirstline = firstLine;
  return params_in;
}
*/

// Wrapper function for the coregistration

int asf_coregister(int datatype, char *coregType, char *baseName, int deskew,
		   long *p1_master_start, long *p1_slave_start, int p1_patches,
		   long *pL_master_start, long *pL_slave_start, int pL_patches,
		   long master_offset, long slave_offset, int maximum_offset,
		   int *master_patches, int *slave_patches, int *p1_range_offset, 
		   int *p1_azimuth_offset, int *pL_range_offset, 
		   int *pL_azimuth_offset, int *grid, int *fft, int power,
		   char *masterFile, char *slaveFile)
{
  FILE *fp;
  char options[255], tmp[255], masterPatch[255], slavePatch[255];
  char coregSlave[255];
  int delta;

  // level zero data - requires processing first
  if (datatype<2) {
    
    // Calculate average Doppler
    check_return(average_in_doppler(masterFile, slaveFile, "reg/avedop"),
		 "calculating the average Doppler (avg_in_dop)");
    sprintf(tmp, "%s.dop", masterFile);
    fileCopy("reg/avedop", tmp);
    sprintf(tmp, "%s.dop", slaveFile);
    fileCopy("reg/avedop", tmp);
    
    // Coregister with patch method
    if (strncmp(coregType, "PATCH", 5)==0) {

      // Process first master and slave patch
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      sprintf(masterPatch, "reg/%s_p1", masterFile);
      check_return(ardop(options, *p1_master_start, p1_patches, 
			 masterFile, masterPatch),
		   "processing first patch of master image (ardop)");
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", slaveFile);
      sprintf(slavePatch, "reg/%s_p1", slaveFile);
      check_return(ardop(options, *p1_slave_start, p1_patches, 
			 slaveFile, slavePatch),
		   "processing first patch of slave image (ardop)");
      
      // Determine offset first patch - pixel level
      check_return(coregister_coarse(masterPatch, slavePatch, 
				     "reg/ctrl1", NULL),
		   "offset estimation first patch (coregister_coarse)");
      
      // Determine default start of last patch
      if (*pL_master_start == 0 && *pL_slave_start == 0) {
	*pL_master_start = master_offset - pL_patches*4096;
	*pL_slave_start = slave_offset - pL_patches*4096;
      }
      
      // Process last master and slave patch
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      sprintf(masterPatch, "reg/%s_pL", masterFile);
      check_return(ardop(options, *pL_master_start, pL_patches, 
			 masterFile, masterPatch),
		   "processing last patch of master image (ardop)");
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", slaveFile);
      sprintf(slavePatch, "reg/%s_pL", slaveFile);
      check_return(ardop(options, *pL_slave_start, pL_patches, 
			 slaveFile, slavePatch),
		   "processing last patch of slave image (ardop)");
      
      // Determine offset last patch - pixel level
      check_return(coregister_coarse(masterPatch, slavePatch, 
				     "reg/ctrlL", NULL),
		   "offset estimation last patch (coregister_coarse)");
      
      // Read offsets determined by coarse coregistration
      fp = FOPEN("reg/ctrl1", "r");
      fscanf(fp, "%d%d", p1_range_offset, p1_azimuth_offset);
      FCLOSE(fp);
      fp = FOPEN("reg/ctrlL", "r");
      fscanf(fp, "%d%d", pL_range_offset, pL_azimuth_offset);
      FCLOSE(fp);

      // Determine whether the measured offset are beyond the given limit
      if ((fabs(*p1_range_offset - *pL_range_offset) > maximum_offset) ||
	  (fabs(*p1_azimuth_offset - *pL_azimuth_offset) > maximum_offset)) {
	asfPrintError("Estimated offset for first and last patch differs more "
		      "than %d pixels\nProcessing terminated to allow manual "
		      "offset estimation\n", maximum_offset);
      }

      // Check whether grid size and FFT flag are set to reasonable values
      if (*grid < 20 || *grid > 200) {
	asfPrintWarning("grid size out of range - set to default value of 20\n");
	*grid = 20;
      }
      if (*fft < 0 || *fft > 1) {
	asfPrintWarning("FFT flag set to invalid value - set to value of 1\n");
	*fft = 1;
      }
      sprintf(tmp, "%s.base.00", baseName);
      fileCopy("base.00", tmp);
      
      // Perform fine coregistration - subpixel level
      sprintf(masterPatch, "reg/%s_p1_cpx.img", masterFile);
      sprintf(slavePatch, "reg/%s_p1_cpx.img", slaveFile);
      check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrl1",
				   "reg/fico1", NULL, *grid, *fft),
		   "fine coregistration first patch (coregister_fine)");
      sprintf(masterPatch, "reg/%s_pL_cpx", masterFile);
      sprintf(slavePatch, "reg/%s_pL_cpx", slaveFile);
      check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrlL",
				   "reg/ficoL", NULL, *grid, *fft),
		   "fine coregistration last patch (coregister_fine)");

      // Determine parameters for SAR processing
      check_return(fit_line("reg/fico1", "reg/line1"),
		   "fit regression line first patch (fit_line)");
      check_return(fit_line("reg/ficoL", "reg/lineL"),
		   "fit regression line last patch (fit_line)");
      check_return(calc_deltas("reg/line1", "reg/lineL",
			       *pL_master_start - *p1_master_start, 
			       "reg/deltas"),
		   "conversion of regression coefficients (calc_deltas)");

      // Process master image
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      check_return(ardop(options, *p1_master_start, *master_patches, 
			 masterFile, masterFile),
		   "processing master image (ardop)");
      if (power == 1) {
	sprintf(tmp, "%s_a_pwr.img", baseName);;
	fileRename("a_pwr.img", tmp);
	sprintf(tmp, "%s_a_pwr.meta", baseName);
	fileRename("mv a_pwr.meta", tmp);
      }
      sprintf(tmp, "%s.meta", baseName);
      fileCopy("a.meta", tmp);

      // Process slave image
      delta = *p1_master_start - master_offset;
      if(delta != 0) {
	double a, b, c, d;
	double e, f, g, h;
	
	fp = FOPEN("reg/deltas", "r");
	fscanf(fp, "%lf%lf%lf%lf", &a, &b, &c, &d);
	fscanf(fp, "%lf%lf%lf%lf", &e, &f, &g, &h);
	FCLOSE(fp);
	fp = FOPEN("reg/deltas", "w");
	fprintf(fp, "%e %e %e %e\n",
		(a-(delta*e)),(b-(delta*f)),(c-(delta*g)),(d-(delta*h)));
	fprintf(fp, "%e %e %e %e\n", e, f, g, h);
	FCLOSE(fp);
      }
      if (deskew == 0) sprintf(options, "-o reg/deltas -debug 1 -c %s.dop",
			       masterFile);
      if (deskew == 1) sprintf(options, "-o reg/deltas -debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      sprintf(coregSlave, "%s_corr", slaveFile);
      check_return(ardop(options, *p1_slave_start, *slave_patches, 
			 slaveFile, coregSlave),
		   "processing slave image (ardop)");
      if (power == 1) {
	sprintf(tmp, "%s_b_pwr.img", baseName);
	fileRename("b_corr_pwr.img", tmp);
	sprintf(tmp, "%s_b_pwr.meta", baseName);
	fileRename("b_corr_pwr.meta", tmp);
      }
    }

    // Coregister with FRAME method
    else if (strncmp(coregType, "FRAME", 5)==0) {

      // Process master image
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      check_return(ardop(options, *p1_master_start, *master_patches, 
			 masterFile, masterFile),
		   "processing slave image (ardop)");

      // Process slave image
      if (deskew == 0) sprintf(options, "-debug 1 -c %s.dop", masterFile);
      if (deskew == 1) sprintf(options, "-debug 1 -e 1");
      if (power == 1) strcat(options, " -power");
      check_return(ardop(options, *p1_slave_start, *slave_patches, 
			 slaveFile, slaveFile),
		   "processing slave image (ardop)");

      sprintf(masterPatch, "%s_cpx", masterFile);
      sprintf(slavePatch, "%s_cpx", slaveFile);
      check_return(coregister_coarse(masterPatch, slavePatch, "reg/ctrl", NULL),
		   "offset estimation slave image (coregister_coarse)");
      sprintf(tmp, "%s.base.00", baseName);
      fileCopy("base.00", tmp);
      
      // Adjusting to optimize the overlap 
      fp = FOPEN("reg/ctrl", "r");
      fscanf(fp, "%d", &delta);
      fscanf(fp, "%d", &delta);
      FCLOSE(fp);
      if (delta > 0) {
	*master_patches = 
	  (int) ((master_offset-4096-delta)/ARDOP_VALID_PATCH_LENGTH) + 1;
	*slave_patches = *master_patches;
	*p1_master_start = delta;
      }
      else {
	*slave_patches = 
	  (int) ((slave_offset-4096+delta)/ARDOP_VALID_PATCH_LENGTH) + 1;
	*master_patches = *slave_patches;
	*p1_slave_start = -delta;
      }
      check_return(ardop(options, *p1_master_start, *master_patches, 
			 masterFile, masterFile),
		   "processing master image (ardop)");
      check_return(ardop(options, *p1_slave_start, *slave_patches, 
			 slaveFile, slaveFile),
		   "processing slave image (ardop)");
      
      if (power == 1) {
	sprintf(tmp, "%s_a_pwr.img", baseName);
	fileRename("a_pwr.img", tmp);
	sprintf(tmp, "%s_a_pwr.meta", baseName);
	fileRename("a_pwr.meta", tmp);
	sprintf(tmp, "%s_b_pwr.img", baseName);
	fileRename("b_pwr.img", tmp);
	sprintf(tmp, "%s_b_pwr.meta", baseName);
	fileRename("b_pwr.meta", tmp);
      }
      
      sprintf(masterPatch, "%s_cpx", masterFile);
      sprintf(slavePatch, "%s_cpx", slaveFile);
      check_return(coregister_coarse(masterPatch, slavePatch, "reg/ctrl", NULL),
		   "offset estimation slave image (coregister_coarse)");
      check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrl", 
				   "reg/fico", NULL, *grid, *fft),
		   "fine coregistration slave image (coregister_fine)");
      check_return(fit_plane("reg/fico", "reg/matrix", 0.8),
		   "calculate transformation parameters (fit_plane)");
      sprintf(tmp, "-matrix reg/matrix -sameSize");
      sprintf(slavePatch, "%s_cpx.img", slaveFile);
      sprintf(coregSlave, "%s_corr_cpx.img", slaveFile);
      check_return(remap(slavePatch, coregSlave, tmp),
		   "resampling of slave image (remap)");
    }
    else {
      asfPrintError("Coregistration type (%s) not supported!\n", coregType);
    }
  }

  // level one data - can be coregistered right away
  else if (datatype == 2) {

    // Coarse coregistration
    sprintf(masterPatch, "%s_cpx", masterFile);
    sprintf(slavePatch, "%s_cpx", slaveFile);
    check_return(coregister_coarse(masterPatch, slavePatch, "reg/ctrl", NULL),
		 "offset estimation (coregister_coarse)");
    sprintf(tmp, "%s.base.00", baseName);
    fileCopy("base.00", tmp);

    // Fine coregistration
    check_return(coregister_fine(masterPatch, slavePatch, "reg/ctrl", 
				 "reg/fico", NULL, *grid, *fft),
		 "fine coregistration slave image (coregister_fine)");
    
    // Remapping slave image on top of master image
    check_return(fit_plane("reg/fico", "reg/matrix", 0.8),
		 "calculating transformation parameters (fit_plane)");
    sprintf(tmp, "-matrix reg/matrix -sameSize");
    sprintf(slavePatch, "%s_cpx.img", slaveFile);
    sprintf(coregSlave, "%s_corr_cpx.img", slaveFile);
    check_return(remap(slavePatch, coregSlave, tmp),
		 "resampling of slave image (remap)");

    sprintf(tmp, "%s_a_amp.img", baseName);
    fileRename("a_amp.img", tmp);
    sprintf(tmp, "%s_a_amp.meta", baseName);
    fileRename("a_amp.meta", tmp);
    remove("b_amp.img");
    remove(" b_amp.meta");
    
    // Generate an amplitude image for the slave
    sprintf(coregSlave, "%s_corr_cpx", slaveFile);
    sprintf(tmp, "%s_corr", slaveFile);
    check_return(c2p_exec(coregSlave, tmp),
		 "converting complex slave image into phase and amplitude (c2p)");
  }

  else 
    asfPrintError("Deta type (%d) not supported\n");

  return(0);
}
