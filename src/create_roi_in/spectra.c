/******************************************************************************
NAME: Range spectra calculator for SEASAT offset video 

SYNOPSIS:

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    9/27/12  T. Logan     ASF Day of innovation
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fftw3.h>

typedef struct {
   float real;
   float imag;
} complexFloat;

#define LINE_LEN 13680
#define FFT_LEN  16384
#define SUM_LINES 10000
#define DEVS     1.5
#define MAX_CALTONES 20

void spectra(FILE *fp,int sl, int nl,double iqmean,int *ocnt,double *ocal)
{
  unsigned char in[FFT_LEN];
  complexFloat **sum;
  int   k,i,kk,j;
  FILE  *fpout;
  complexFloat *a;
  fftwf_plan plonga;
  double mag[FFT_LEN];
  double shift[FFT_LEN];
  char infile[256];
  double mean = 0;
  double diff, sqdiff, sumsq, stddev;
  int    notch_loc[10000];
  double notch_val[10000];
  double notch_pow[10000];
  int    max_loc;
  
  printf("\nCalculating the Range Spectra\n");
    
  sum = (complexFloat **) malloc (sizeof(complexFloat *)*SUM_LINES);
  for (i=0; i<SUM_LINES; i++) {
    sum[i] = (complexFloat *) malloc (sizeof(complexFloat)*FFT_LEN);
    if (sum[i]==NULL) {printf("ERROR IN MALLOC!!!\n"); exit(1);}
  }

  a = (complexFloat*) fftwf_malloc(sizeof(fftwf_complex)*FFT_LEN);
  plonga = fftwf_plan_dft_1d(FFT_LEN,(fftwf_complex*) a,(fftwf_complex*) a, FFTW_FORWARD, FFTW_MEASURE);
  for(i=0;i<SUM_LINES;i++) for(k=0;k<FFT_LEN;k++) { sum[i][k].real = 0.0; sum[i][k].imag = 0.0;}
  
  /* seek to middle of file */
  long int where = ((sl+nl/2) - SUM_LINES/2);
  where = where * LINE_LEN;
  if (where < 0) { 
     printf("ERROR: Bad seek in spectra!!!  May need to decrease SUM_LINES\n"); 
     printf(" sl = %i, nl/2 = %i, SUM_LINES/2 = %i, LINE_LEN = %i, WHERE = %li\n",sl,nl/2,SUM_LINES/2,LINE_LEN,where);
     exit(1);}
  fseek(fp,where,SEEK_SET);
  
  for (i=0; i<SUM_LINES; i++) {
    if (i%1000==0) {printf("\tCalculating Spectra...  line %i\n",i+(sl+nl/2)-SUM_LINES/2);}
    fread(in,1,LINE_LEN,fp);
    for (k=0; k<LINE_LEN; k++) {
      kk = in[k];
      if (kk < 0) kk = kk+32;
      a[k].real = kk-iqmean;
      a[k].imag = 0.0;
    }
    for (k=LINE_LEN; k<FFT_LEN; k++) { a[k].real = 0.0; a[k].imag = 0.0; }
    fftwf_execute(plonga);
    for (k=0; k<FFT_LEN; k++)  { sum[i][k].real = a[k].real; sum[i][k].imag = a[k].imag;}
  }

  for (k=0; k<FFT_LEN; k++)  {mag[k]=0.0;}

  for (i=0; i<SUM_LINES; i++) {  
    for (k=0; k<FFT_LEN; k++)  {
       mag[k] = mag[k]+ sqrt(sum[i][k].real*sum[i][k].real+sum[i][k].imag*sum[i][k].imag);
    }
  }

  /* rotate the spectra */
  for (k=0; k < FFT_LEN/2; k++) shift[k] = mag[k+FFT_LEN/2];
  for (k=FFT_LEN/2; k<FFT_LEN; k++) shift[k] = mag[k-FFT_LEN/2];

  /* write out unchanged spectra */
  fpout = fopen("spectra.out","w");
  for (k=0; k<FFT_LEN; k++) {
    shift[k] = shift[k] / SUM_LINES;
    fprintf(fpout,"%f\n",shift[k]);
  }
  fclose(fpout);
  
  /* get the mean and standard deviation of the spectra */
  for (k=FFT_LEN/2+4; k<FFT_LEN; k++) { mean = mean + shift[k]; }
  mean = mean / (FFT_LEN/2-4);
  for (k=FFT_LEN/2+4; k<FFT_LEN; k++) {
     diff = shift[k]-mean;	/* deviation from mean */
     sqdiff = diff*diff;	/* square of deviation from mean */
     sumsq = sumsq + sqdiff;	/* sum of squared deviations */
  }
  stddev = sqrt(sumsq/(FFT_LEN/2-5));

  printf("Mean is %lf\n",mean);
  printf("Standard Deviation is %lf\n",stddev);
  printf("Values that are more than %f standard deviations from the mean (loc,val,%%):\n",DEVS);
  printf("\tBin\tPower\tCaltone\n");

  /* find out where all of the notch filters need to be */
  int notch_cnt = 0;
  for (k=FFT_LEN/2+4; k<FFT_LEN; k++) {
    if ((shift[k]-mean)> DEVS*stddev) {
       notch_loc[notch_cnt] = k;
       notch_val[notch_cnt] = (k-(double)(FFT_LEN/2))/(double)FFT_LEN;
       notch_pow[notch_cnt] = shift[k];
       printf("\t%i\t%lf\t%lf\n",notch_loc[notch_cnt],notch_pow[notch_cnt],notch_val[notch_cnt]);
       notch_cnt++;
    }
  }

  /* dump out the spectra with notches removed - for a visual test only */
  for (k=0;k<notch_cnt;k++) { shift[notch_loc[k]] = mean; }
  fpout = fopen("spectra.fixed","w");
  for (k=0; k<FFT_LEN; k++) {
    fprintf(fpout,"%f\n",shift[k]);
  }
  fclose(fpout);
  
  /* sort the notch locations and remove any that fall within +/-6 of each other */
  for (k=0;k<notch_cnt;k++) {
    double max_pow = 0;
    for (j=k; j<notch_cnt; j++) {
      if (notch_pow[j] > max_pow) { max_pow = notch_pow[j]; max_loc = j; }
    }
    if (max_pow>0.0) {
      double tmp_pow = notch_pow[k];
      double tmp_val = notch_val[k];
      int    tmp_loc = notch_loc[k];
      
      notch_pow[k] = notch_pow[max_loc];
      notch_val[k] = notch_val[max_loc];
      notch_loc[k] = notch_loc[max_loc];
      
      notch_pow[max_loc] = tmp_pow;
      notch_val[max_loc] = tmp_val;
      notch_loc[max_loc] = tmp_loc;
    }
    else break;
    
    for (i=k+1; i<notch_cnt; i++) {
      if (abs(notch_loc[i]-notch_loc[k])<6) {
        notch_pow[i] = 0;
	notch_val[i] = 0;
	notch_loc[i] = 0;
      }
    }
  }

  int tcnt = notch_cnt;
  for (k=0;k<tcnt;k++) {
    if (notch_val[k]==0) break;
  }
  
  printf("After culling neighbors, only %i caltones remain:\n",k);
  for (i=0; i<k; i++) printf("\t%i\t%lf\t%.14lf\n",notch_loc[i],notch_pow[i],notch_val[i]);
  
  if (k > MAX_CALTONES) {
    printf("Too many caltones remain, only notching top %i values\n",MAX_CALTONES);
    *ocnt = MAX_CALTONES;
    for (i=0; i<MAX_CALTONES; i++) { ocal[i] = notch_val[i]; }
  } else {
    *ocnt = k;
    for (i=0;i<k;i++) { ocal[i] = notch_val[i]; }
  }
  
  printf("\n");
}
