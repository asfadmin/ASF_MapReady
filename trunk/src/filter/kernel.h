typedef enum {
  AVERAGE=1,
  GAUSSIAN,
  LAPLACE1,
  LAPLACE2,
  LAPLACE3,
  SOBEL,
  PREWITT,
  EDGE,
  MEDIAN,
  LEE,
  ENHANCED_LEE,
  FROST,
  ENHANCED_FROST,
  GAMMA_MAP,
  KUAN
} filter_type_t;

float kernel(filter_type_t filter_type, float *inbuf, int nLines, int nSamples, 
	     int xSample, int kernel_size);
