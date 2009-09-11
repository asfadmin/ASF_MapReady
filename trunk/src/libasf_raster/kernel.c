/*******************************************************************
FUNCTION NAME:   kernel - performs the kernel operation of the
                 filter

INPUT: inbuf          - input image buffer
       nLines         - number of lines (image buffer)
       nSamples       - number of samples per line (image buffer)
       yLine          - line of interest
       xSample        - sample position within line
       kernel_size    - number of samples in kernel
       damping_factor - exponential damping factor
       nLooks         - number of looks in radar image
*******************************************************************/
#include <assert.h>

#include "asf.h"
#include "asf_raster.h"

#define SQR(X) ((X)*(X))

int compare_values(const float *valueA, const float *valueB)
{
  if (*valueA <  *valueB) return -1;
  if (*valueA == *valueB) return  0;
  if (*valueA >  *valueB) return  1;
  
  assert (FALSE);		/* Shouldn't be here.  */
  return 0;
}

double calc_sum(float *inbuf, int nSamples, int yLine, int xSample, int kernel_size)
{
  register int i,j;
  double sum=0.0;
  int half = (kernel_size-1)/2;
  int base = (xSample-half+(yLine-half)*nSamples);

  for (i=0; i<kernel_size; i++) 
    for (j=0; j<kernel_size; j++) 
      sum += inbuf[base+j+i*nSamples];

  return sum;
}

double calc_mean(float *inbuf, int nSamples, int yLine, int xSample, 
		 int kernel_size)
{
  register int i,j;
  double sum=0.0, mean;
  int half = (kernel_size-1)/2;
  int base = (xSample-half+(yLine-half)*nSamples);

  for (i=0; i<kernel_size; i++) 
    for (j=0; j<kernel_size; j++) 
      sum += inbuf[base+j+i*nSamples];

  mean = sum / SQR(kernel_size);

  return mean;
}

double calc_std_dev(float *inbuf, int nSamples, int yLine, int xSample, 
		    int kernel_size, double mean)
{
  register int i,j;
  double sum_vv=0.0, standard_deviation;
  int half = (kernel_size-1)/2;
  int base = (xSample-half+(yLine-half)*nSamples);

  for (i=0; i<kernel_size; i++)
    for (j=0; j<kernel_size; j++)
      sum_vv += SQR(inbuf[base+j+i*nSamples]-mean);

  standard_deviation = sqrt(sum_vv / (SQR(kernel_size)-1));

  return standard_deviation;
}

float kernel(filter_type_t filter_type, float *inbuf, int nLines, int nSamples, 
	     int yLine, int xSample, int kernel_size, float damping_factor, 
	     int nLooks)
{                    
  double sum = 0.0, mean, standard_deviation, weight, value = 0.0;         
  int half = (kernel_size-1)/2;
  int base = xSample-half+(yLine-half)*nSamples;
  int total = 0, sigmsq=4;
  double ci, cu, cmax, center, a, b, d, rf = 0.0, x, y, m;
  float *pix;
  register int i, j;
  
  switch(filter_type)
    {
    case AVERAGE:
      for (i=yLine-half; i<=yLine-half; i++)
	for (j=xSample-half; j<=xSample+half; j++)
	  sum += inbuf[i*nSamples+j];
      total = kernel_size * kernel_size;
      value = sum/total;
      break;

    case GAUSSIAN:
      sum = calc_sum(inbuf, nSamples, yLine, xSample, kernel_size);
      for (i=yLine-half; i<=yLine-half; i++) {
        for (j=xSample-half; j<=xSample+half; j++) {
          value += exp(- (SQR(i-half)+SQR(j-xSample)) / (2*sigmsq)) 
                   * inbuf[base] / sum;
          base++;
        }
        base += nSamples;
        base -= kernel_size;
      }
      break;

    case LAPLACE1:
      /* Kernel:  0  1  0
                  1 -4  1
                  0  1  0 */      

      value = inbuf[base+1] + inbuf[base+nSamples] - 4*inbuf[base+1+nSamples] 
        + inbuf[base+2+nSamples] + inbuf[base+1+2*nSamples];
      break;

    case LAPLACE2:
      /* Kernel: -1 -1 -1
                 -1  8 -1
                 -1 -1 -1 */

      value = -inbuf[base] - inbuf[base+1] - inbuf[base+2]
        - inbuf[base+nSamples] + 8*inbuf[base+1+nSamples] 
        - inbuf[base+2+nSamples] - inbuf[base+2*nSamples]
        - inbuf[base+1+2*nSamples] - inbuf[base+2+2*nSamples];
      break;

    case LAPLACE3:
      /* Kernel:  1 -2  1
                 -2  4 -2
                  1 -2  1 */

      value = inbuf[base] - 2*inbuf[base+1] + inbuf[base+2]
        - 2*inbuf[base+nSamples] + 4*inbuf[base+1+nSamples] 
        - 2*inbuf[base+2+nSamples] + inbuf[base+2*nSamples]
        - 2*inbuf[base+1+2*nSamples] + inbuf[base+2+2*nSamples];
      break;

    case SOBEL:
      /* Kernels: -1  0  1      1  2  1
                  -2  0  2      0  0  0
                  -1  0  1     -1 -2 -1

                      x            y      */

      x = -inbuf[base] + inbuf[base+2] - 2*inbuf[base+nSamples] 
        + 2*inbuf[base+2+nSamples] - inbuf[base+2*nSamples]
        + inbuf[base+2+2*nSamples];
      y = inbuf[base] + 2*inbuf[base+1] + inbuf[base+2]
        - inbuf[base+2*nSamples] - 2*inbuf[base+1+2*nSamples] 
        - inbuf[base+2+2*nSamples];
      value = sqrt(SQR(x) + SQR(y));
      break;
    case PREWITT:
      /* Kernels: -1  0  1      1  1  1
                  -1  0  1      0  0  0
                  -1  0  1     -1 -1 -1

                      x            y      */

      x = -inbuf[base] + inbuf[base+2] - inbuf[base+nSamples] 
        + inbuf[base+2+nSamples] - inbuf[base+2*nSamples]
        + inbuf[base+2+2*nSamples];
      y = inbuf[base] + inbuf[base+1] + inbuf[base+2]
        - inbuf[base+2*nSamples] - inbuf[base+1+2*nSamples] 
        - inbuf[base+2+2*nSamples];
      value = sqrt(SQR(x) + SQR(y));
      break;

    case EDGE:
      value = inbuf[base + half + half*nSamples] - kernel(AVERAGE,inbuf,nLines,
        nSamples,yLine,xSample,kernel_size,damping_factor,nLooks);
      break;

    case MEDIAN:
      pix = (float*) MALLOC(kernel_size*kernel_size*sizeof(float));
      for (i=yLine-half; i<=yLine+half; i++) {
        for (j=xSample-half; j<=xSample+half; j++) 
          pix[total++] = inbuf[base++];
        base += nSamples;
        base -= kernel_size;
      }
      qsort(pix, kernel_size*kernel_size, sizeof(float), (void*)compare_values);
      value = pix[kernel_size*kernel_size/2 + 1];
      FREE(pix);
      break;

    case LEE:
      center = inbuf[base + half + half*nSamples];
      mean = calc_mean(inbuf, nSamples, yLine, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, yLine, xSample, kernel_size, mean);
      ci = standard_deviation/mean;
      cu = sqrt(1/(double)nLooks);
      weight = 1 - SQR(cu)/SQR(ci);
      value = center*weight + mean*(1-weight);
      break;

    case ENHANCED_LEE:
      center = inbuf[base + half + half*nSamples];
      mean = calc_mean(inbuf, nSamples, yLine, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, yLine, xSample, kernel_size, mean);
      ci = standard_deviation/mean;
      cu = sqrt(1/(double)nLooks);
      cmax = sqrt(1+2.0/(double)nLooks);
      weight = exp(-damping_factor*(ci-cu)/(cmax-ci));
      rf = center*weight + center*(1-weight);
      if (ci <= cu) value = mean;
      else if ((cu < ci) && (ci < cmax)) value = rf;
      else if (ci >= cmax) value = center;
      break;

    case FROST:
      mean = calc_mean(inbuf, nSamples, yLine, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, yLine, xSample, kernel_size, mean);
      ci = standard_deviation/mean;
      a = damping_factor * SQR(ci);
      for (i=yLine-half; i<=yLine+half; i++) {
	for (j=xSample-half; j<=xSample+half; j++) {
          m = exp(-a * abs(j-half));
          rf += m * inbuf[base];
          sum += m;
          base++;
        }
	base += nSamples;
	base -= kernel_size;
      }
      value = rf / sum;
      break;

    case ENHANCED_FROST:
      for (i=yLine-half; i<=yLine+half; i++) {
        for (j=xSample-half; j<=xSample+half; j++) {
          inbuf[base] = SQR(inbuf[base]);
          base++;
        }
        base += nSamples;
        base -= kernel_size;
      }
      base = xSample- half;
      center = inbuf[base + half + half*nSamples];
      mean = calc_mean(inbuf, nSamples, yLine, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, yLine, xSample, kernel_size, mean);
      cu = sqrt(1/(double)nLooks);
      cmax = sqrt(1+2.0/(double)nLooks);
      for (i=0; i<nLines; i++) {
	for (j=xSample-half; j<=xSample+half; j++) {
          ci = sqrt(SQR(inbuf[base]-mean))/mean;
          m = exp(-damping_factor * (ci-cu) / (cmax-ci) * abs(j-half));
          rf += m * inbuf[base];
          sum += m;
          base++;
        }
	base += nSamples;
	base -= kernel_size;
      }
      rf /= sum;
      if (ci < cu) value = sqrt(mean);
      else if ((cu <= ci) && (ci <= cmax)) value = sqrt(rf);
      else if (ci > cmax) value = sqrt(center);
      break;

    case GAMMA_MAP:
      center = inbuf[base + half + half*nSamples];
      mean = calc_mean(inbuf, nSamples, yLine, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, yLine, xSample, kernel_size, mean);
      ci = standard_deviation/mean;
      cu = sqrt(1/(double)nLooks);
      cmax = sqrt(2.0)*cu;
      a = (1+SQR(cu)) / (SQR(ci)-SQR(cu));
      b = a - nLooks - 1;
      d = SQR(mean)*SQR(b) + 4*a*nLooks*mean*center;
      rf = (b*mean + sqrt(d)) / (2*a);
      if (ci <= cu) value = mean;
      else if ((cu < ci) && (ci < cmax)) value = rf;
      else if (ci >= cmax) value = center;
      break;

    case KUAN:
      center = inbuf[base + half + half*nSamples];
      mean = calc_mean(inbuf, nSamples, yLine, xSample, kernel_size);
      standard_deviation = 
	calc_std_dev(inbuf, nSamples, yLine, xSample, kernel_size, mean);
      ci = standard_deviation/mean;
      cu = sqrt(1/(double)nLooks);
      weight = (1 - SQR(cu)/SQR(ci))/(1 + SQR(cu));
      value = center*weight + mean*(1-weight);
      break;
      
    }

  return value;
}
