#include "image_stats.h"
#include "least_squares.h"
#include "matrix.h"

/* Global variables */
extern int lines, samples, bins;
extern double min, max, interval;

void calculate_plot(char *axis, char *gridFile, char *dataFile, char *maskFile, 
		    char *outFile, meta_parameters *meta, float xConstant)
{
  FILE *fpIn=NULL, *fpImg=NULL, *fpOut=NULL, *fpMask=NULL, *fpComp=NULL;
  quadratic_2d q;
  plot_t *plot;
  int maskFlag=FALSE, ii, kk, ll, size=BUFSIZE, points=0, x, y, index;
  char cmd[255], inLine[255], *mask=NULL; 
  float *bufImage=NULL, *bufRef=NULL, *bufComp=NULL, temp;
  double xValue, slope, offset, *l, *s, *value;
  
  /* Prepare mask image for reading */
  if (maskFile != NULL) maskFlag = TRUE;
  if (maskFlag) {
    fpMask = fopenImage(maskFile, "rb");
    mask = (unsigned char *) MALLOC(lines * samples * sizeof(char));
    FREAD(mask, sizeof(char), lines*samples, fpMask);
    FCLOSE(fpMask);
  }
  
  /* Set things up for least square calculation */
  value=(double *)MALLOC(sizeof(double)*MAX_PTS);
  l=(double *)MALLOC(sizeof(double)*MAX_PTS);
  s=(double *)MALLOC(sizeof(double)*MAX_PTS);
  
  /* Read xValue grid points and estimate parameter for xValue calculation 
     using least squares approach */
  points = 0;
  fpIn = FOPEN(gridFile, "r");
  while (NULL!=(fgets(inLine, 255, fpIn))) {
    sscanf(inLine,"%lf%lf%lf", &value[points], &l[points], &s[points]); 
    points++;
  }
  FCLOSE(fpIn);
  
  q = find_quadratic(value, l, s, points);
  q.A = xConstant; 
  
  /* Determine minimum and maximum xValue for binning */
  min = 100000000;
  max = -100000000;
  for (ii=0; ii<lines; ii++) {
    for (kk=0; kk<samples; kk++) {
      x = ii;
      y = kk;
      xValue = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y;
      xValue += q.G*x*x*y + q.H*x*y*y + q.I*x*x*y*y;
      xValue += q.J*x*x*x + q.K*y*y*y;
      if (maskFlag) {
	if ((xValue > max) && mask[ii*samples+kk]) max = xValue;
	if ((xValue < min) && mask[ii*samples+kk]) min = xValue;
      }
      else {
	if (xValue > max) max = xValue;
	if (xValue < min) min = xValue;
      }
    }
  }

  /* Determine parameters for binning */
  if (interval > 0.0) 
    bins = ((max-min) / interval) + 0.5; /* interval supersedes number of bins */
  if (bins < 2 || bins > 1024) bins = 256;
  slope = (bins-1) / (max-min);
  offset = -slope * min;
  interval = (max-min) / bins;
  
  /* Initialize plot */
  plot = (plot_t *) MALLOC(bins * sizeof(plot_t));
  for (ii=0; ii<bins; ii++) {
    plot[ii].count = 0;
    plot[ii].min = 100000000;
    plot[ii].max = -100000000;
    plot[ii].mean = 0.0;
    plot[ii].stdDev = 0.0;
  }
  
  /* Prepare input image for reading */
  fpImg = fopenImage(dataFile, "rb");
  bufImage = (float *) MALLOC(samples * sizeof(float) * BUFSIZE);
  
  /* First data sweep: Calculate mean values and get the counts */
  printf("   First data sweep: Calculate mean values and get the counts ...\n");
  for (ii=0; ii<lines; ii+=size) {
    if ((lines-ii)<BUFSIZE) size = lines-ii;
    get_float_lines(fpImg, meta, ii, size, bufImage);
    for (ll=0; ll<size; ll++) 
      for (kk=0; kk<samples; kk++) {
	x = ii + ll;
	y = kk;
	xValue = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y;
	xValue += q.G*x*x*y + q.H*x*y*y + q.I*x*x*y*y;
	xValue += q.J*x*x*x + q.K*y*y*y;
	index = (int) (slope*xValue + offset);

	if (maskFlag) {
	  if (mask[x*samples+y]) {
	    if (bufImage[ll*samples+kk] < plot[index].min)
	      plot[index].min = bufImage[ll*samples+kk];
	    if (bufImage[ll*samples+kk] > plot[index].max)
	      plot[index].max = bufImage[ll*samples+kk];
	    plot[index].mean += bufImage[ll*samples+kk];
	    plot[index].count++;
	  }
	}
	else {
	  if (bufImage[ll*samples+kk] < plot[index].min)
	    plot[index].min = bufImage[ll*samples+kk];
	  if (bufImage[ll*samples+kk] > plot[index].max)
	    plot[index].max = bufImage[ll*samples+kk];
	  plot[index].mean += bufImage[ll*samples+kk];
	  plot[index].count++;
	}
      }
  }
  for (ii=0; ii<bins; ii++)
    plot[ii].mean /= plot[ii].count;
  
  FCLOSE(fpImg);
  
  /* Second data sweep: Calculate standard deviations */
  printf("   Second data sweep: Calculating standard deviations ...\n\n");
  fpImg = fopenImage(dataFile, "rb");
  for (ii=0; ii<lines; ii+=size) {
    if ((lines-ii)<BUFSIZE) size = lines-ii;
    get_float_lines(fpImg, meta, ii, size, bufImage);
    for (ll=0; ll<size; ll++) 
      for (kk=0; kk<samples; kk++) {
	x = ii + ll;
	y = kk;
	xValue = q.A + q.B*x + q.C*y + q.D*x*x + q.E*x*y+ q.F*y*y;
	xValue += q.G*x*x*y + q.H*x*y*y + q.I*x*x*y*y;
	xValue += q.J*x*x*x + q.K*y*y*y;
	index = (int) (slope*xValue + offset);

	if (maskFlag) {
	  if (mask[x*samples+y]) 
	    plot[index].stdDev +=
	      SQR(plot[index].mean - bufImage[ll*samples+kk]);
	}
	else {
	  plot[index].stdDev +=
	    SQR(plot[index].mean - bufImage[ll*samples+kk]);
	}
      }
  }
  for (ii=0; ii<bins; ii++) 
    plot[ii].stdDev = sqrt(plot[ii].stdDev / (plot[ii].count-1));
  
  FCLOSE(fpImg);
  
  /* Prepare output file for writing */
  fpOut = FOPEN(outFile, "w");
  fprintf(fpOut, "%s", axis);

  if (meta->general->image_data_type == SIGMA_IMAGE ||
      meta->general->image_data_type == GAMMA_IMAGE ||
      meta->general->image_data_type == BETA_IMAGE) {
    fprintf(fpOut, "\tMin value\tMax value\tMean value\tdB value\tCount"
	    "\tStandard deviation\n");
    for (ii=0; ii<bins; ii++)
      fprintf(fpOut, "%10.4lf\t%.8lf\t%10.4lf\t%10.4lf\t%10.4lf\t%10li\t%10.4lf\n", 
	      (min+ii*interval), plot[ii].min, plot[ii].max, plot[ii].mean, 
	      10*log10(plot[ii].mean), plot[ii].count, plot[ii].stdDev);
  }
  else {
    fprintf(fpOut, "\tMin value\tMax value\tMean value\tCount\tStandard deviation\n");
    for (ii=0; ii<bins; ii++)
      fprintf(fpOut, "%10.4lf\t%.8lf\t%10.4lf\t%10.4lf\t%10li\t%10.4lf\n", 
	      (min+ii*interval), plot[ii].min, plot[ii].max,
	      plot[ii].mean, plot[ii].count, plot[ii].stdDev);
  }

  FCLOSE(fpOut);
  
}
