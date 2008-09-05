#include "asf.h"
#include "asf_meta.h"
#include "ardop_defs.h"
#include "asf_insar.h"
#include "functions.h"
#include "fft.h"
#include "ifm.h"
#include "asf_endian.h"

// Coarse coregistration

int coregister_coarse(char *masterFile, char *slaveFile, 
		      int *nOffX, int *nOffY, char *maskFile)
{
  int multiLook;
  float offX, offY, certainty;
  char metaFile[512], meta_master[512], meta_slave[512];
  meta_parameters *meta;

  // Determine whether multilooking is required
  create_name(metaFile, masterFile, "_amp.meta");
  meta = meta_read(metaFile);
  multiLook = meta->sar->look_count;
  meta_free(meta);

  // Determine offset
  create_name(meta_master, masterFile, "_amp");
  create_name(meta_slave, slaveFile, "_amp");
  check_return(fftMatch(meta_master, meta_slave, NULL, 
			&offX, &offY, &certainty),
	       "determining offset during coarse coregistration (fftMatch)");
  offY *= multiLook;
  *nOffX = (int) offX;
  *nOffY = (int) offY;
  asfPrintStatus("   Complex image offset is %d rows, %d columns\n\n",
                 *nOffY, *nOffX);

  return (0);
}


// Fine coregistration

float getFFTCorrelation(complexFloat *igram, int sizeX, int sizeY)
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

// getPeak:
// This function computes a correlation peak, with SNR, between
// the two given images at the given points.
void getPeak(int x1,int y1,char *szImg1,int x2,int y2,char *szImg2,
	     int srcSize, int trgSize,
             float *peakX,float *peakY, float *snr)
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
  float xmep = 4.1;    // x maximum error pixel value that is accepted
  float ymep = 6.1;    // y maximum error pixel value that is accepted

  // Calculate the limits of the time domain correlations...
  //   A coordinate in the target may be set to:
  //   (  (trgSize/2 - srcSize/2), (trgSize/2 - srcSize/2)  ).
  //   If this is the ulh element of the source chip, then
  //     the src chip coincides with the trg precisely, with no offset.

  xOffsetStart = (trgSize/2 - srcSize/2) - (int)(xmep);
  xOffsetEnd = (trgSize/2 - srcSize/2) + (int)(xmep);
  yOffsetStart = (trgSize/2 - srcSize/2) - (int)(ymep);
  yOffsetEnd = (trgSize/2 - srcSize/2) + (int)(ymep);

  // Read metadata
  metaSource = meta_read(szImg1);
  metaTarget = meta_read(szImg2);
  srcSamples = metaSource->general->sample_count;
  trgSamples = metaTarget->general->sample_count;

  // Allocate working arrays if we haven't already done so
  if (s==NULL)
    {
      bufSource = (complexFloat *) 
	MALLOC(sizeof(complexFloat)*srcSize*srcSamples);
      bufTarget = (complexFloat *) 
	MALLOC(sizeof(complexFloat)*trgSize*trgSamples);
      s = (complexFloat *)(MALLOC(srcSize*srcSize*sizeof(complexFloat)));
      t = (complexFloat *)(MALLOC(trgSize*trgSize*sizeof(complexFloat)));
      product = (complexFloat *)(MALLOC(srcSize*srcSize*sizeof(complexFloat)));
      peaks=(float *)MALLOC(sizeof(float)*trgSize*trgSize);
    }

  // Open files, read lines and create subset
  fpSource = FOPEN(szImg1, "rb");
  fpTarget = FOPEN(szImg2, "rb");
  get_complexFloat_lines(fpSource, metaSource, y1-srcSize/2+1, srcSize, 
			 bufSource);
  get_complexFloat_lines(fpTarget, metaTarget, y2-trgSize/2+1, trgSize, 
			 bufTarget);
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

      thisMax = getFFTCorrelation(product, srcSize, srcSize);

      // Possibly save this coherence value
      if (thisMax > peakMax) {
        peakMax = thisMax;
        peakMaxX = xOffset;
        peakMaxY = yOffset;
      }
      peaks[yOffset*trgSize+xOffset] = thisMax;
      peakSum += thisMax;
      count++;
    }
  }

  // Calculate the SNR, with a much faster (but weaker) SNR calculation
  *snr = peakMax / ((peakSum - peakMax) / (float)(count-1))-1.0;

  //printf("peakMaxX: %i, peakMaxY: %i\n", peakMaxX, peakMaxY);
  if ((peakMaxX>xOffsetStart) && (peakMaxY>yOffsetStart) &&
      (peakMaxX<xOffsetEnd) && (peakMaxY<yOffsetEnd))
    topOffPeak(peaks, peakMaxX, peakMaxY, trgSize, trgSize, &dx, &dy);
  else
    dx=dy=0.0;

  *peakX=((float)(peakMaxX) + dx - accel1 );
  *peakY=((float)(peakMaxY) + dy - accel1 );
}

bool outOfBoundary(int x1, int y1, int x2, int y2, int srcSize, int trgSize,
		   int nl, int ns)
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

int coregister_fine(char *masterFile, char *slaveFile, int nOffX, int nOffY,
                    char *ficoFile, char *maskFile, int gridSize)
{
  int x1, x2, y1, y2, srcSize=32, trgSize, borderX=80, borderY=80;
  int gridResolution=20, pointNo, goodPoints, attemptedPoints;
  char gridRes[256];
  float minSNR = 0.3;  // Threshold for deleting points
  float maxDisp = 1.8; // Forward and reverse correlations which differ by more
                       // than this will be deleted

  // calculate parameters
  trgSize = 2*srcSize;

  // determine size of input files
  meta_parameters *meta = meta_read(masterFile);
  int ns = meta->general->sample_count;
  int nl = meta->general->line_count;
  meta_free(meta);

  // create output file
  FILE *fp = FOPEN(ficoFile, "w");

  // Check to see if the last parameter contains a number, the grid resolution
  if (gridRes)
    gridResolution=atoi(gridRes);
  if (gridResolution<2)
    gridResolution=20;
  if (!quietflag)
    printf("   Sampling rectangular grid, %ix%i resolution.\n",
           gridResolution,gridResolution);

  // Loop over grid, performing forward and backward correlations
  pointNo = goodPoints = attemptedPoints = 0;
  while (pointNo < (gridResolution*gridResolution)) {
    float dx, dy, snr, dxFW, dyFW, snrFW, dxBW, dyBW, snrBW;
    int unscaledX = pointNo % gridResolution;
    int unscaledY = pointNo / gridResolution;
    x1 = unscaledX*(ns-2*borderX)/(gridResolution-1) + borderX;
    y1 = unscaledY*(nl-2*borderY)/(gridResolution-1) + borderY;
    x2 = x1 - nOffX;
    y2 = y1 - nOffY;
    pointNo++;
    attemptedPoints++;

    // Check bounds...
    if (!(outOfBoundary(x1, y1, x2, y2, srcSize, trgSize, nl, ns) ||
          outOfBoundary(x2, y2, x1, y1, srcSize, trgSize, nl, ns))) {
      // ...check forward correlation...
      getPeak(x1, y1, masterFile, x2, y2, slaveFile, srcSize, trgSize,
	      &dxFW, &dyFW, &snrFW);
      if (snrFW > minSNR) {
        // ...check backward correlation...
        getPeak(x2, y2, slaveFile, x1, y1, masterFile, srcSize, trgSize,
		&dxBW, &dyBW, &snrBW);
        dxBW *= -1.0;
        dyBW *= -1.0;
        if ((snrBW > minSNR) &&
            (fabs(dxFW-dxBW) < maxDisp) &&
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
    asfPrintError("   coregister_fine was only able to find %i points which\n"
                  "   correlated the same backwards and forwards. This\n"
                  "   is not enough for a planar map!\n", goodPoints);
  else
    asfPrintStatus("   coregister_fine attempted %d correlations, %d succeeded"
                   ".\n\n", attemptedPoints, goodPoints);

  return (0);
}


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
  
  asfPrintStatus("\n   Average Doppler: %e %e %e \n\n", 
		 avg_t1, avg_t2, avg_t3);
  
  // Store result, in the order fd fdd fddd
  fp = FOPEN(outFile, "w");
  fprintf(fp, "%e %e %e", avg_t1, avg_t2, avg_t3);
  FCLOSE(fp); 
  
  return(0);
}

// Read ardop parameters before calling it
struct INPUT_ARDOP_PARAMS 
   *read_ardop_params(char *inFile, char *outFile, char *doppler, char *offset,
		      int power, int deskew, int nPatches, int firstLine)
{
  struct INPUT_ARDOP_PARAMS *params_in;
  float fd, fdd, fddd;
  float sloper, interr, slopea, intera;
  float dsloper, dinterr, dslopea, dintera;
  FILE *fp;

  params_in = get_input_ardop_params_struct(inFile, outFile);
  if (doppler) {
    fp = FOPEN(doppler, "r");
    fscanf(fp, "%f %f %f", &fd, &fdd, &fddd);
    FCLOSE(fp);
    params_in->fd = &fd;
    params_in->fdd = &fdd;
    params_in->fddd = &fddd;
  }
  if (offset) {
    fp = FOPEN(offset, "r");
    fscanf(fp,"%f %f %f %f", &sloper, &interr, &slopea, &intera);
    params_in->sloper = &sloper;
    params_in->interr = &interr;
    params_in->slopea = &slopea;
    params_in->intera = &intera;
    fscanf(fp,"%f %f %f %f", &dsloper, &dinterr, &dslopea, &dintera);
    params_in->dsloper = &dsloper;
    params_in->dinterr = &dinterr;
    params_in->dslopea = &dslopea;
    params_in->dintera = &dintera;
    FCLOSE(fp);
  }
  int iflag = 1;
  params_in->iflag = &iflag;
  params_in->pwrFlag = &power;
  params_in->deskew = &deskew;
  params_in->npatches = &nPatches;
  params_in->ifirstline = &firstLine;
  return params_in;
}

// Wrapper function for the coregistration

int asf_coregister(int datatype, char *coregType, char *baseName, int deskew,
		   long *p1_master_start, long *p1_slave_start, int p1_patches,
		   long *pL_master_start, long *pL_slave_start, int pL_patches,
		   long master_offset, long slave_offset, int maximum_offset,
		   int *master_patches, int *slave_patches, 
		   int *p1_range_offset, int *p1_azimuth_offset, 
		   int *pL_range_offset, int *pL_azimuth_offset, 
		   int *grid, int power,
		   char *masterFile, char *slaveFile)
{
  FILE *fp;
  char tmp[255], masterPatch[255], slavePatch[255], coregSlave[255];
  char baseFile[255], *dopFile;
  int delta, nOffX, nOffY;
  struct INPUT_ARDOP_PARAMS *ardop_params;

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

      // Process first master patch
      sprintf(masterPatch, "reg/%s_p1", masterFile);
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", masterFile);
      }
      ardop_params = 
	read_ardop_params(masterFile, masterPatch, dopFile, NULL,
			  0, deskew, p1_patches, *p1_master_start);
      asfPrintStatus("   Processing first patch of master image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params),
		   "processing first patch of master image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      // Process first slave patch
      sprintf(slavePatch, "reg/%s_p1", slaveFile);
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      ardop_params = 
	read_ardop_params(slaveFile, slavePatch, dopFile, NULL,
			  0, deskew, p1_patches, *p1_slave_start);
      asfPrintStatus("   Processing first patch of slave image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params),
		   "processing first patch of slave image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);
      
      // Determine offset first patch - pixel level
      asfPrintStatus("\n   Coregistering first patch to pixel level ...\n");
      check_return(coregister_coarse(masterPatch, slavePatch, &p1_range_offset,
				     &p1_azimuth_offset, NULL),
		   "offset estimation first patch (coregister_coarse)");
      
      // Determine default start of last patch
      if (*pL_master_start == 0 && *pL_slave_start == 0) {
	*pL_master_start = master_offset - pL_patches*4096;
	*pL_slave_start = slave_offset - pL_patches*4096;
      }
      
      // Process last master patch
      sprintf(masterPatch, "reg/%s_pL", masterFile);
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", masterFile);
      }
      ardop_params = 
	read_ardop_params(masterFile, masterPatch, dopFile, NULL,
			  0, deskew, pL_patches, *pL_master_start);
      asfPrintStatus("   Processing last patch of master image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params),
		   "processing last patch of master image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      // Process last slave patch
      sprintf(slavePatch, "reg/%s_pL", slaveFile);
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      ardop_params = 
	read_ardop_params(slaveFile, slavePatch, dopFile, NULL,
			  0, deskew, pL_patches, *pL_slave_start);
      asfPrintStatus("   Processing last patch of slave image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params),
		   "processing last patch of slave image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);
      
      // Determine offset last patch - pixel level
      asfPrintStatus("\n   Coregistering last patch to pixel level ...\n");
      check_return(coregister_coarse(masterPatch, slavePatch, &pL_range_offset,
				     &pL_azimuth_offset, NULL),
		   "offset estimation last patch (coregister_coarse)");

      // Determine whether the measured offset are beyond the given limit
      if ((fabs(*p1_range_offset - *pL_range_offset) > maximum_offset) ||
	  (fabs(*p1_azimuth_offset - *pL_azimuth_offset) > maximum_offset)) {
	asfPrintError("Estimated offset for first and last patch differs more "
		      "than %d pixels\nProcessing terminated to allow manual "
		      "offset estimation\n", maximum_offset);
      }

      // Check whether grid size and FFT flag are set to reasonable values
      if (*grid < 20 || *grid > 200) {
	asfPrintWarning("grid size out of range - "
			"set to default value of 20\n");
	*grid = 20;
      }
      sprintf(tmp, "%s.base.00", baseName);
      fileCopy("base.00", tmp);
      
      // Perform fine coregistration - subpixel level
      sprintf(masterPatch, "reg/%s_p1_cpx.img", masterFile);
      sprintf(slavePatch, "reg/%s_p1_cpx.img", slaveFile);
      check_return(coregister_fine(masterPatch, slavePatch, nOffX, nOffY,
				   "reg/fico1", NULL, *grid),
		   "fine coregistration first patch (coregister_fine)");
      sprintf(masterPatch, "reg/%s_pL_cpx", masterFile);
      sprintf(slavePatch, "reg/%s_pL_cpx", slaveFile);
      check_return(coregister_fine(masterPatch, slavePatch, nOffX, nOffY,
				   "reg/ficoL", NULL, *grid),
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
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", masterFile);
      }
      ardop_params = 
	read_ardop_params(masterFile, masterPatch, dopFile, NULL,
			  power, deskew, *master_patches, master_offset);
      asfPrintStatus("   Processing master image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params), "processing master image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      if (power) {
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
      
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      sprintf(coregSlave, "%s_corr", slaveFile);
      ardop_params = 
	read_ardop_params(slaveFile, slavePatch, dopFile, "reg/deltas",
			  power, deskew, *slave_patches, slave_offset);
      asfPrintStatus("   Processing slave image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params), "processing slave image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      if (power) {
	sprintf(tmp, "%s_b_pwr.img", baseName);
	fileRename("b_corr_pwr.img", tmp);
	sprintf(tmp, "%s_b_pwr.meta", baseName);
	fileRename("b_corr_pwr.meta", tmp);
      }
    }

    // Coregister with FRAME method
    else if (strncmp(coregType, "FRAME", 5)==0) {

      // Process master image
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      ardop_params = 
	read_ardop_params(masterFile, masterPatch, dopFile, NULL,
			  power, deskew, *master_patches, master_offset);
      asfPrintStatus("   Processing slave image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params), "processing slave image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      // Process slave image
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      ardop_params = 
	read_ardop_params(slaveFile, slavePatch, dopFile, NULL,
			  power, deskew, *slave_patches, slave_offset);
      check_return(ardop(ardop_params), "processing slave image (ardop)");
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      sprintf(masterPatch, "%s_cpx", masterFile);
      sprintf(slavePatch, "%s_cpx", slaveFile);
      asfPrintStatus("   Coregistering images to pixel level ...\n");
      check_return(coregister_coarse(masterPatch, slavePatch, 
				     &nOffX, &delta, NULL),
		   "offset estimation slave image (coregister_coarse)");

      // Determine the baseline
      sprintf(masterPatch, "%s_amp", masterFile);
      sprintf(slavePatch, "%s_amp", slaveFile);
      strcpy(baseFile, "base.00");
      write_baseline( baseFile, find_baseline(masterFile, slaveFile) );

      sprintf(tmp, "%s.base.00", baseName);
      fileCopy("base.00", tmp);
      
      // Adjusting to optimize the overlap 
      /*
      fp = FOPEN("reg/ctrl", "r");
      fscanf(fp, "%d", &delta);
      fscanf(fp, "%d", &delta);
      FCLOSE(fp);
      */
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

      // Processing master image
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      ardop_params = 
	read_ardop_params(masterFile, masterPatch, dopFile, NULL,
			  power, deskew, *master_patches, master_offset);
      asfPrintStatus("\n   Processing master image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params), "processing master image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);

      // Processing slave image
      if (deskew)
	dopFile = NULL;
      else {
	dopFile = (char *) MALLOC(sizeof(char)*255);
	sprintf(dopFile, "%s.dop", slaveFile);
      }
      ardop_params = 
	read_ardop_params(slaveFile, slavePatch, dopFile, NULL,
			  power, deskew, *slave_patches, slave_offset);
      asfPrintStatus("   Processing slave image ...\n");
      quietflag = TRUE;
      check_return(ardop(ardop_params), "processing slave image (ardop)");
      quietflag = FALSE;
      FREE(ardop_params);
      if (dopFile)
	FREE(dopFile);
      
      if (power) {
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
      check_return(coregister_coarse(masterPatch, slavePatch, 
				     &nOffX, &nOffY, NULL),
		   "offset estimation slave image (coregister_coarse)");
      check_return(coregister_fine(masterPatch, slavePatch, nOffX, nOffY, 
				   "reg/fico", NULL, *grid),
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
    check_return(coregister_coarse(masterPatch, slavePatch,
				   &nOffX, &nOffY, NULL),
		 "offset estimation (coregister_coarse)");

    // Determine the baseline
    sprintf(masterPatch, "%s_amp", masterFile);
    sprintf(slavePatch, "%s_amp", slaveFile);
    strcpy(baseFile, "base.00");
    write_baseline( baseFile, find_baseline(masterFile, slaveFile) );


    // Fine coregistration
    check_return(coregister_fine(masterPatch, slavePatch, nOffX, nOffY, 
				 "reg/fico", NULL, *grid),
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

  // Determine the baseline
  sprintf(masterPatch, "%s_amp", masterFile);
  sprintf(slavePatch, "%s_corr_amp", slaveFile);
  strcpy(baseFile, "base.00");
  write_baseline( baseFile, find_baseline(masterFile, slaveFile) );
  sprintf(tmp, "%s.base.00", baseName);
  fileCopy("base.00", tmp);

  return(0);
}
