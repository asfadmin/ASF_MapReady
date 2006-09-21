#include "pta.h"
#include "asf_complex.h"
#include "asf.h"

fcpx *forward_fft(complexFloat *image, int line_count, int sample_count)
{
  int i;

  // Allocate memory
  int size = line_count * sample_count;
  fcpx *in_cpx = (fcpx *) fftwf_malloc(sizeof(fcpx)*size);
  fcpx *fft_cpx = (fcpx *) fftwf_malloc(sizeof(fcpx)*size);

  // Fill in the values from internal complexFloat format
  for (i=0; i<size; i++) {
    in_cpx[i][0] = image[i].real;
    in_cpx[i][1] = image[i].imag;
  } 

  // Do the forward FFT
  int flags = FFTW_DESTROY_INPUT + FFTW_ESTIMATE;
  fftwf_plan fw_plan = fftwf_plan_dft_2d(line_count, sample_count, in_cpx, fft_cpx, 
					 FFTW_FORWARD, flags);
  fftwf_execute(fw_plan);
  fftwf_destroy_plan(fw_plan);
  fftwf_free(in_cpx);

  return fft_cpx;
}

complexFloat *inverse_fft(fftwf_complex *fft_cpx, int line_count, int sample_count)
{
  int i;
  
  // Allocate memory
  int size = line_count * sample_count;
  fcpx *out_cpx = (fcpx *) fftwf_malloc(sizeof(fcpx)*size);
  complexFloat *image = (complexFloat *) MALLOC(sizeof(complexFloat)*size);

  // Do the inverse FFT
  int flags = FFTW_DESTROY_INPUT + FFTW_ESTIMATE;
  fftwf_plan bw_plan = fftwf_plan_dft_2d(line_count, sample_count, fft_cpx, out_cpx,
					 FFTW_BACKWARD, flags);
  fftwf_execute(bw_plan);
  fftwf_destroy_plan(bw_plan);
  fftwf_free(fft_cpx);

  // Fill in the values into internal complexFloat format
  for (i=0; i<size; i++) {
    image[i].real = out_cpx[i][0] / size;
    image[i].imag = out_cpx[i][1] / size;
  }

  return image; 
}

fcpx *oversample(fcpx *in, int srcSize, int oversampling_factor)
{
  int i, k;

  // Allocate memory for oversampled image
  int bigSize = srcSize * oversampling_factor;
  fcpx *out = (fcpx *) fftwf_malloc(sizeof(fcpx)*bigSize*bigSize);

  // Oversample 
  // uppler left corner
  for (i=0; i<srcSize/2; i++) {
    for (k=0; k<srcSize/2; k++)
      out[i*bigSize+k][0] = in[i*srcSize+k][0];
      out[i*bigSize+k][1] = in[i*srcSize+k][1];
    }
  // upper right corner
  for (i=0; i<srcSize/2; i++)
    for (k=srcSize/2; k<srcSize; k++) {
      out[i*bigSize+bigSize-srcSize+k][0] = in[i*srcSize+k][0];
      out[i*bigSize+bigSize-srcSize+k][1] = in[i*srcSize+k][1];
    }
  // lower left corner
  for (i=srcSize/2; i<srcSize; i++)
    for (k=0; k<srcSize/2; k++) {
      out[(bigSize-srcSize+i)*bigSize+k][0] = in[i*srcSize+k][0];
      out[(bigSize-srcSize+i)*bigSize+k][1] = in[i*srcSize+k][1];
    }
  // lower right corner
  for (i=srcSize/2; i<srcSize; i++) {
    for (k=srcSize/2; k<srcSize; k++)
      out[(bigSize-srcSize+i)*bigSize+bigSize-srcSize+k][0] = in[i*srcSize+k][0];
      out[(bigSize-srcSize+i)*bigSize+bigSize-srcSize+k][1] = in[i*srcSize+k][1];
    }

  return(out);
}

void complex2polar(complexFloat *in, int line_count, int sample_count,
		   float *amplitude, float *phase)
{
  int i, k, index;

  for (i=0; i<line_count; i++)
    for (k=0; k<sample_count; k++) {
      index = i*sample_count+k;
      if (in[index].real!=0.0 || in[index].imag!=0.0) {
	amplitude[index] = sqrt(in[index].real*in[index].real + 
				in[index].imag*in[index].imag);
	phase[index] = atan2(in[index].imag, in[index].real);
      }
      else {
	amplitude[index] = 0.0;
	phase[index] = 0.0;
      }
    }
}
