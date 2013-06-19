/******************************************************************************
NAME: Doppler estimator for SEASAT offset video 

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
    1.0	    9/21/12  T. Logan     Seasat Proof of Concept Project - ASF
    				  
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:  Algorithm converted from ROI PAC fortran
			Added calculation of i/q mean 
			Added culling points for linear regression and modulo math...

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>

typedef struct {
   float real;
   float imag;
} complexFloat;

void cfft1d(int n, complexFloat *c, int dir);
void yax2bxc(double x_vec[],double y_vec[],int n,double *a,double *b,double *c);

int line_len = 13680;
int fft_len  = 16384;
int half_fft = 8192;
int sum_lines = 10000;

void estdop(FILE *fp, int sl, int nl, double *t1, double *t2, double *t3, double *iqmean)
{
  unsigned char in[fft_len];
  complexFloat prod[fft_len];
  double acc[fft_len];
  double line[fft_len];
  double sumi[sum_lines];
  double tsum=0.0;
  double cnt = 0.0;
  int   fftlen;
  int   len = 13680;
  int   k,i,kk, start, end;
  FILE  *fpout;
  complexFloat *a, *b;
  fftwf_plan plonga, phalfa, plongb, phalfb;
  
  a = (complexFloat*) fftwf_malloc(sizeof(fftwf_complex)*fft_len);
  b = (complexFloat*) fftwf_malloc(sizeof(fftwf_complex)*fft_len);
  
  plonga = fftwf_plan_dft_1d(fft_len,(fftwf_complex*) a,(fftwf_complex*) a, FFTW_FORWARD, FFTW_MEASURE);
  phalfa = fftwf_plan_dft_1d(half_fft, (fftwf_complex*)&a[8192],(fftwf_complex*) &a[8192],  FFTW_BACKWARD, FFTW_MEASURE);
  plongb = fftwf_plan_dft_1d(fft_len, (fftwf_complex*)b, (fftwf_complex*)b, FFTW_FORWARD, FFTW_MEASURE);
  phalfb = fftwf_plan_dft_1d(half_fft, (fftwf_complex*)&b[8192], (fftwf_complex*)&b[8192],  FFTW_BACKWARD, FFTW_MEASURE);
  
  for(k=0;k<fft_len;k++) { prod[k].real = 0.0; prod[k].imag = 0.0;}
  for(i=0;i<sum_lines;i++) sumi[i] = 0.0; 

  /* seek to middle of file */
  long int where = (long int)((sl+nl/2) - sum_lines/2) * (long int) line_len;
  fseek(fp,where,SEEK_SET);

  /* Pre-read the first line of the file */
  fread(in,1,line_len,fp);
  
  for (i=0; i<sum_lines; i++) {
    if (i%1000==0) {printf("\tEstimating Doppler...  line %i\n",i+(sl+nl/2)-sum_lines/2);}
  
    /* convert 'in' to complex, store in a, transform */
    for (k=0; k<line_len; k++) {
      kk = in[k];
      if (kk < 0) kk = kk+32;
      a[k].real = kk-15.5;
      a[k].imag = 0.0;
    }
    for (k=line_len; k<fft_len; k++) { a[k].real = 0.0; a[k].imag = 0.0; }
    
    fftwf_execute(plonga);
    fftwf_execute(phalfa);

    /* read in next line, convert to complex, transform */
    fread(in,1,line_len,fp);
    for (k=0; k<line_len; k++) {
      kk = in[k];
      if (kk < 0) kk = kk+32;
      b[k].real = kk-15.5;
      b[k].imag = 0.0;
      sumi[i] = sumi[i] + in[k];
    }
    for (k=line_len; k<fft_len; k++) { b[k].real = 0.0; b[k].imag = 0.0; }
    fftwf_execute(plongb);
    fftwf_execute(phalfb);
        
    /* Calculate the product */
    for (k=0; k<half_fft; k++) {
      // a[k+half_fft].imag = -1.0 *a[k+half_fft].imag;  /* conjugate of a */
      prod[k].real = prod[k].real + (a[k+half_fft].real*b[k+half_fft].real + a[k+half_fft].imag*b[k+half_fft].imag);
      prod[k].imag = prod[k].imag + (a[k+half_fft].real*b[k+half_fft].imag - a[k+half_fft].imag*b[k+half_fft].real);
    }

  }
  
  for (k=0; k<len/2; k++) {
    acc[k] = atan2(prod[k].imag,prod[k].real);
    acc[k] = -1.0*acc[k]/2.0/3.14159265;
    line[k] = k;
  }

  fpout = fopen("dop.pre","w");
  for (k=0; k<len/2; k++) {
    fprintf(fpout,"%i %lf\n",k,acc[k]);
  }
  fclose(fpout);

  /* fix wrap-around in doppler estimation as it confuses the quadratic fit */
  for (i=1;i<len/2;i++) {
    if (fabs(acc[i]-acc[i-1]) > 0.7) {
      if ((acc[i]-acc[i-1])>0.0) acc[i] = acc[i]-1.0;
      else acc[i] = acc[i]+1.0;
    }
  }

  /* 2nd try at fixing doppler wrap around */
  tsum = 0.0;
  for (i=0; i<3000; i++) { tsum = tsum + acc[i]; }
  tsum = tsum / 3000;
  
  for (i=0; i<len/2; i++) { 
    if (fabs(acc[i]-tsum) > 0.5) {
      if (acc[i]-tsum>0.0) acc[i] = acc[i]-1.0;
      else acc[i] = acc[i]+1.0;
    }
  }      

  fpout = fopen("dop.out","w");
  for (k=0; k<len/2; k++) {
    fprintf(fpout,"%i %lf\n",k,acc[k]);
  }
  fclose(fpout);

  yax2bxc(line,acc,len/2,t3,t2,t1);

  tsum = 0.0;
  for (i=0; i<sum_lines; i++) {
    sumi[i] = sumi[i] / line_len;
    tsum = tsum + sumi[i];
  }
  tsum = tsum / sum_lines;
  printf("Calculated i mean %lf\n\n",tsum);
  *iqmean = tsum;  
  
}

/******************************************************************************
NAME:       yax2bxc.c

DESCRIPTION:    Computes a, b, and c for y = ax^2 + bx + c using quadratic
        	regression (least squares fit) given double vectors y and x
        	of length n.

PARAMETERS: 	x_vec   double[]     Input vector of X values
        	y_vec   double[]     Input vector of Y values
        	n   	int         Length of input vectors
        	a   	double*      Return x^2 coefficient
        	b   	double*      Return x coefficient
        	c   	double*      Return offset factor

HISTORY:        Ver 1.0   2/97   T. Logan    Initial Creation
                Ver 1.1   5/98   O. Lawlor   Changed to doubles
		Ver 1.2   9/12   T. Logan    Uses all doubles now
	`	
ALGORITHM REF:  Cheney, Ward & D. Kincaid, Numerical Mathematics and Computing,
            2nd Editn. pp 360-362. Brooks/Cole Pub. Co., Pacific Grove, Ca.
******************************************************************************/

void yax2bxc(double x_vec[],double y_vec[],int n,double *a,double *b,double *c)
{
  double x1, x2, x3, x4,     	/* Sum of x, x^2, x^3, x^4 */
         y1, yx, yx2;     	/* Sum of y, y*x, y*x^2    */
  double d1, d2, d3, d4, d5;     /* Intermediate Values     */
  double t1, t2, t3;     	/* Equation Solutions      */
  double res;
  double max_val, tmp, diff;
  int    max_loc;
  int   i;
  int   cnt;

  res = 1.0;
 
  while (res >= 0.01) {
 
    /* Calculate all of the first order sums */
    x1 = x2 = x3 = x4 = y1 = yx = yx2 = 0.0;
    cnt = 0;
    
    for (i=0; i<n; i++) {
      /* skip 3180 - 3980 since this is where the nadir reflecitons occur */
      if ( (i<3180 || i>3980) && x_vec[i] != 0.0) {
        x1  += x_vec[i];
        x2  += x_vec[i] * x_vec[i];
        x3  += x_vec[i] * x_vec[i] * x_vec[i];
        x4  += x_vec[i] * x_vec[i] * x_vec[i] * x_vec[i];
        y1  += y_vec[i];
        yx  += y_vec[i] * x_vec[i];
        yx2 += y_vec[i] * x_vec[i] * x_vec[i];
	cnt++;
      }
   }

   d1 = cnt*x2  - x1*x1;
   d2 = cnt*x3  - x1*x2;
   d3 = cnt*x4  - x2*x2;
   d4 = cnt*yx  - x1*y1;
   d5 = cnt*yx2 - x2*y1;

   t1 = (d1*d5 - d2*d4) / (d1*d3 - d2*d2);
   t2 = (d4 - d2*t1) / d1;
   t3 = (y1 - x2*t1 - x1*t2) / cnt;

   max_val = 0.0;
   max_loc = -1;

   /* check the regression, throwing out the one input that has the highest error */
   for (i=0; i<n; i++) {
     if (x_vec[i] != 0.0) {
       tmp = t1 * x_vec[i] * x_vec[i] + t2 * x_vec[i] + t3; 
       diff = fabs(y_vec[i]-tmp);
       if (diff > max_val) { max_val = diff; max_loc = i; }
     }
   }
   
   if (max_loc != -1) {
//     printf("Culling point %i (%lf) res=%lf\n",max_loc,y_vec[max_loc],max_val);
     x_vec[max_loc]=0.0;
     y_vec[max_loc]=0.0;
     res = max_val;
   }
   else  res = 0.0;

 }

 *a = t1;
 *b = t2;
 *c = t3;

 return;
}
