/*******************************************************************
FUNCTION NAME:   interpolate - performs the interpolation to calculate
                 the pixel value e.g. during resampling

INPUT: interpolation  - interpolation method (nearest neighbor, bilinear, sinc)
       inbuf          - input image buffer
       nLines         - number of lines (image buffer)
       nSamples       - number of samples per line (image buffer)
       xLine          - line of interest
       xSample        - sample position within line
       weighting      - weighting function to be applied (Kaiser, Hamming)
       nKernel        - kernel size for sinc function
*******************************************************************/
#include "asf.h"
#include "asf_raster.h"

#define SQR(X) ((X)*(X))

float interpolate(interpolate_type_t interpolation, float *inbuf, int nLines, 
		  int nSamples, float xLine, float xSample, 
		  weighting_type_t weighting, int nKernel)
{
  int ix, iy, base;
  int a00, a10, a01, a11;
             
  /* Get closed pixel position to start from */
  ix = (int)(xSample+10) - 10;
  iy = (int)(xLine+10) - 10;
  base = ix + iy*nSamples;

  switch(interpolation)
    {
    case NEAREST:
      value = inbuf[base];
      break;

    case BILINEAR:
      a00 = inbuf[base];
      a10 = inbuf[base+1] - inbuf[base];
      a01 = inbuf[base+nLines] - inbuf[base];
      a11 = inbuf[base] - inbuf[base+1] - inbuf[base+nLines] + inbuf[base+nLines+1];
      value = a00 + a10*xSample + a01*xLine + a11*xSample*xLine;
      printf("value: %f\n", value);
      exit(0);
      break;

    case BICUBIC:
      break;

    case SINC:
      break;
    }

  return value;
}
