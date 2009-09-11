/*******************************************************************
FUNCTION NAME:   kernel - performs the kernel operation of the
                 filter

INPUT: inbuf       - input image buffer
       nLines      - number of lines (image buffer)
       nSamples    - number of samples per line (image buffer)
       xSample     - sample position within line
       kernel_size - number of samples in kernel
*******************************************************************/
#include "asf.h"
#include "kernel.h"

#define SQR(X) ((X)*(X))

double calc_mean(float *inbuf, int nSamples, int xSample, int kernel_size)
{
  register int i,j;
  double sum=0.0, mean;
  int half = (kernel_size-1)/2, base;

  for (i=0; i<kernel_size; i++) {
    base = (xSample-half);
    for (j=0; j<kernel_size; j++) {
      sum += inbuf[base+j*nSamples];
      base++;
    }
  }
  mean = sum / (SQR(kernel_size));

  return mean;
}

double calc_std_dev(float *inbuf, int nSamples, int xSample, int kernel_size,
		    double mean)
{
  register int i,j;
  double sum_vv=0.0, standard_deviation;
  int half = (kernel_size-1)/2, base;

  for (i=0; i<kernel_size; i++) {
    base = (xSample-half);
    for (j=0; j<kernel_size; j++) {
      sum_vv += SQR(inbuf[base+j+nSamples] - mean);
      base++;
    }
  }
  standard_deviation = sqrt(sum_vv / (SQR(kernel_size)-1));

  return standard_deviation;
}

float kernel(filter_type_t filter_type, float *inbuf, int nLines, int nSamples, 
	     int xSample, int kernel_size)
{                    
  float sum = 0.0, mean, standard_deviation, value;         
  int half = (kernel_size-1)/2, base = (xSample-half), total = 0;
  double ci, cu, cmax, center, a, b, d, rf;
  register int i, j;
  
  switch(filter_type)
    {
    case (AVERAGE):
      for (i=0; i<nLines; i++) {
	for (j=xSample-half; j<=xSample+half; j++) {
	    if (inbuf[base]!=0 && j<nSamples)
	      {
		sum += inbuf[base];
		total += 1;
	      }
	    base++;
	  }
	base += nSamples;
	base -= kernel_size;
      }
      value = sum/total;
      break;
    case(GAUSSIAN):
    case(LAPLACE1):
    case(LAPLACE2):
    case(LAPLACE3):
    case(SOBEL):
    case(PREWITT):
    case(EDGE):
    case(MEDIAN):
    case(LEE):
    case(ENHANCED_LEE):
    case(FROST):
    case(ENHANCED_FROST):
      /* not implemented yet; here to keep a complaining compiler shut up */
      break;
    case(GAMMA_MAP):
      center = inbuf[base+ half + half*nSamples];
      mean = calc_mean(inbuf, nSamples, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, xSample, kernel_size, mean);
      ci = standard_deviation/mean;
      cu = sqrt(1/kernel_size);
      cmax = sqrt(2)*cu;
      a = (1+SQR(cu)) / (SQR(ci)-SQR(cu));
      b = a - kernel_size - 1;
      d = SQR(mean)*SQR(b) + 4*a*kernel_size*mean*center;
      rf = (b*mean + sqrt(d)) / (2*a);

      if (ci <= cu) value = mean;
      else if ((cu < ci) && (ci < cmax)) value = rf;
      else if (ci >= cmax) value = center;
      break;
    case(KUAN):
      /* not implemented yet; here to keep a complaining compiler shut up */
      break;
    }
  
  return value;
}
